#load "window.p";
#load "graphics_engine/graphics_engine.p";
#load "files.p";
#load "hash_table.p";
#load "file_watcher.p";

// @Incomplete: Maybe scale fonts with output size? Where the chosen font size is relative to 1080p?

Viewport :: struct {
    // Screen positions in pixels
    tl: [2]f32;
    br: [2]f32;
}

Element :: struct {
    // General

    virtual_x: f32;
    virtual_y: f32;
    virtual_w: f32;
    virtual_h: f32;
    alignment: Text_Alignment;

    shadowed: bool; // Outline the text or image
    shadow_color: GE_Color;
    underlined: bool; // Underline the text    
    bullet: bool; // Draw a bullet point left to the text
    interact: s64; // Only show this element after the user has interacted x times with this slide
    
    place_below_element: *Element;
    
    // Text

    font: *Font;
    color: GE_Color;
    text: string;

    // Image

    image: *GE_Texture;
}

Slide :: struct {
    background: GE_Color;
    elements: [..]Element;
    interacts: s64;
}

Slide_Set :: struct {
    arena: Memory_Arena;
    allocator: Allocator;
    font_table: Chained_Hash_Table(string, Font);
    slides: [..]Slide;
    index: s64;
}

Slides :: struct {
    window: Window;
    ge: Graphics_Engine;

    active_set: Slide_Set;
    watcher: File_Watcher;
    display_custom_cursor: bool;
}



/* ------------------------------------------------ Slide Set ------------------------------------------------ */

create_slide_set :: (set: *Slide_Set) {
    create_memory_arena(*set.arena, 1000000, 0, false);
    set.allocator = allocator_from_memory_arena(*set.arena);
    chained_hash_table_create(*set.font_table, 8, string_hash, strings_equal_case_insensitive);
    set.slides.allocator = *set.allocator;
    set.index = 0;
}

destroy_slide_set :: (set: *Slide_Set) {
    destroy_memory_arena(*set.arena);
    ~set = .{};
}



/* ------------------------------------------------- Parsing ------------------------------------------------- */

parse_slide_set :: (slides: *Slides, file_path: string) {
    /* ----------------------------------------------- Helpers ----------------------------------------------- */

    DEFAULT_SHADOW_COLOR: GE_Color : .{ 125, 130, 118, 255 };
    
    Options :: struct {
        // Meta
        previous_line_was_element: bool; // Volatile options are reset when after completing an element (meaning: multiple contiguous lines of text or an element). This is implemented as: Reset when encountering the first directive after an element.
        place_next_below_element: *Element;
        
        // Persistent
        x: f32;
        y: f32;
        w: f32;
        h: f32;
        color: GE_Color;
        font_display_name: string;
        alignment: Text_Alignment;
        
        // Volatile
        shadowed: bool;
        shadow_color: GE_Color;
        underlined: bool;
        bullet: bool;
        interact: s64;
    }

    reset_volatile :: (options: *Options) {
        options.shadowed     = false;
        options.shadow_color = DEFAULT_SHADOW_COLOR;
        options.underlined   = false;
        options.bullet       = false;
        options.interact     = 0;
    }

    reset_all :: (options: *Options) {
        options.x = 0.5;
        options.y = 0.5;
        options.w = -1;
        options.h = 0.25;
        options.color = .{ 255, 255, 255, 255 };
        options.font_display_name = "Barlow-45";
        options.alignment = .Centered | .Median;
        reset_volatile(options);
    }

    ensure_existing_slide :: (file_path: string, line_index: s64, slide: *Slide) -> bool {
        if !slide print("%:%: This directive is not allowed before the first slide.\n", file_path, line_index);
        return slide != null;
    }

    ensure_n_args :: (file_path: string, line_index: s64, args: *[..]string, n: s64) -> bool {
        if args.count != n print("%:%: This directive expects % operands, but % were given.\n", file_path, line_index, n, args.count);
        return args.count == n;
    }
    
    parse_int :: (file_path: string, line_index: s64, arg: string) -> s64 {
        result, valid := string_to_integer(arg);

        if !valid print("%:%: An integer value was expected here.\n", file_path, line_index);

        return result;
    }

    parse_float :: (file_path: string, line_index: s64, arg: string) -> f32 {
        result, valid := string_to_float(arg);

        if !valid print("%:%: An floating point value was expected here.\n", file_path, line_index);

        return result;
    }
    
    parse_color :: (file_path: string, line_index: s64, args: *[..]string) -> GE_Color {
        if !ensure_n_args(file_path, line_index, args, 3) return .{ 255, 255, 255, 255 };

        result: GE_Color = ---;
        result.r = parse_int(file_path, line_index, array_get(args, 0));
        result.g = parse_int(file_path, line_index, array_get(args, 1));
        result.b = parse_int(file_path, line_index, array_get(args, 2));
        result.a = 255;
        return result;
    }
    
    

    /* ------------------------------------------------- Loop ------------------------------------------------- */

    destroy_slide_set(*slides.active_set);
    create_slide_set(*slides.active_set);
    
    file_content, success := read_entire_file(*temp, file_path);

    if !success {
        print("Failed to parse '%': The file does not exist!\n", file_path);
        return;
    }

    line_index := 0;
    
    options: Options = ---;
    reset_all(*options);
    
    while file_content.count {
        line: string = read_next_line(*file_content);
        ++line_index;
        
        {
            pound_index, pound_found := search_string(line, #char "#");
            if pound_found line = substring_view(line, 0, pound_index);
        }
        
        line = trim_string_view(line);
        if line.count == 0 continue;

        current_slide: *Slide = ifx slides.active_set.slides.count then array_get_pointer(*slides.active_set.slides, slides.active_set.slides.count - 1) else null;

        if line[0] == #char ":" {
            first_space, space_found := search_string(line, #char " ");
            if !space_found first_space = line.count;

            ident := substring_view(line, 1, first_space);
            args  := ifx space_found then string_split(substring_view(line, first_space + 1, line.count), #char " ", *temp) else ([..]string).{};

            if options.previous_line_was_element {
                reset_volatile(*options);
            }

            options.previous_line_was_element = false;
            
            if strings_equal(ident, "slide", .Case_Insensitive) {
                slide := array_push(*slides.active_set.slides);
                slide.elements.allocator = *slides.active_set.allocator;
                options.place_next_below_element = null;
            } else if strings_equal(ident, "align", .Case_Insensitive) {
                for i := 0; i < args.count; ++i {
                    name := array_get(*args, i);

                    if strings_equal(name, "left", .Case_Insensitive) {
                        options.alignment &= ^(.Left | .Centered | .Right);
                        options.alignment |= .Left;
                    } else if strings_equal(name, "center", .Case_Insensitive) {
                        options.alignment &= ^(.Left | .Centered | .Right);
                        options.alignment |= .Centered;
                    } else if strings_equal(name, "right", .Case_Insensitive) {
                        options.alignment &= ^(.Left | .Centered | .Right);
                        options.alignment |= .Right;
                    } else if strings_equal(name, "top", .Case_Insensitive) {
                        options.alignment &= ^(.Top | .Median | .Bottom);
                        options.alignment |= .Top;
                    } else if strings_equal(name, "median", .Case_Insensitive) {
                        options.alignment &= ^(.Top | .Median | .Bottom);
                        options.alignment |= .Median;
                    } else if strings_equal(name, "bottom", .Case_Insensitive) {
                        options.alignment &= ^(.Top | .Median | .Bottom);
                        options.alignment |= .Bottom;
                    } else {
                        print("%:%: Unknown alignment '%'.\n", file_path, line_index, name);
                    }
                }
            } else if strings_equal(ident, "image", .Case_Insensitive) {
                if !ensure_existing_slide(file_path, line_index, current_slide) || !ensure_n_args(file_path, line_index, *args, 1) continue;
                image := array_push(*current_slide.elements);
                image.image     = get_image(slides, array_get(*args, 0));
                image.virtual_x = options.x;
                image.virtual_y = options.y;
                image.virtual_w = options.w;
                image.virtual_h = options.h;
                image.alignment = options.alignment;
                image.shadowed  = options.shadowed;
                image.shadow_color = options.shadow_color;
                image.interact     = options.interact;
                image.place_below_element = options.place_next_below_element;
                options.previous_line_was_element = true;
                options.place_next_below_element = image;
            } else if strings_equal(ident, "background", .Case_Insensitive) {
                if !ensure_existing_slide(file_path, line_index, current_slide) continue;
                current_slide.background = parse_color(file_path, line_index, *args);
            } else if strings_equal(ident, "shadow", .Case_Insensitive) {
                if args.count == 0 {
                    options.shadowed = true;
                    options.shadow_color = DEFAULT_SHADOW_COLOR;
                } else if args.count == 1 && strings_equal(array_get(*args, 0), "on", .Case_Insensitive) {
                    options.shadowed = true;
                    options.shadow_color = DEFAULT_SHADOW_COLOR;
                } else if args.count == 1 && strings_equal(array_get(*args, 0), "off", .Case_Insensitive) {
                    options.shadowed = false;
                } else {
                    options.shadowed = true;
                    options.shadow_color = parse_color(file_path, line_index, *args);
                }
            } else if strings_equal(ident, "underline", .Case_Insensitive) {
                if args.count == 0 {
                    options.underlined = true;
                } else if args.count == 1 && strings_equal(array_get(*args, 0), "on", .Case_Insensitive) {
                    options.underlined = true;
                } else if args.count == 1 && strings_equal(array_get(*args, 0), "off", .Case_Insensitive) {
                    options.underlined = false;
                } else {
                    ensure_n_args(file_path, line_index, *args, 0);
                    continue;
                }
            } else if strings_equal(ident, "bullet", .Case_Insensitive) {
                if !ensure_n_args(file_path, line_index, *args, 0) continue;
                options.bullet = true;
            } else if strings_equal(ident, "interact", .Case_Insensitive) {
                if !ensure_n_args(file_path, line_index, *args, 1) continue;
                options.interact = parse_int(file_path, line_index, array_get(*args, 0));
            } else if strings_equal(ident, "font", .Case_Insensitive) {
                if !ensure_n_args(file_path, line_index, *args, 1) continue;
                options.font_display_name = array_get(*args, 0);
            } else if strings_equal(ident, "x", .Case_Insensitive) {
                if !ensure_n_args(file_path, line_index, *args, 1) continue;                
                options.x = parse_float(file_path, line_index, array_get(*args, 0));                
            } else if strings_equal(ident, "y", .Case_Insensitive) {
                if !ensure_n_args(file_path, line_index, *args, 1) continue;                
                options.y = parse_float(file_path, line_index, array_get(*args, 0));
                options.place_next_below_element = null;
            } else if strings_equal(ident, "w", .Case_Insensitive) {
                if !ensure_n_args(file_path, line_index, *args, 1) continue;
                if strings_equal(array_get(*args, 0), "auto", .Case_Insensitive) {
                    options.w = -1;
                } else {
                    options.w = parse_float(file_path, line_index, array_get(*args, 0));
                }
            } else if strings_equal(ident, "h", .Case_Insensitive) {
                if !ensure_n_args(file_path, line_index, *args, 1) continue;
                if strings_equal(array_get(*args, 0), "auto", .Case_Insensitive) {
                    options.h = -1;
                } else {
                    options.h = parse_float(file_path, line_index, array_get(*args, 0));
                }
            } else {
                print("%:%: Unknown directive '%'.\n", file_path, line_index, ident);
            }
        } else {
            if !ensure_existing_slide(file_path, line_index, current_slide) {
                continue;
            }

            text := array_push(*current_slide.elements);
            text.font         = get_font(slides, options.font_display_name);
            text.color        = options.color;
            text.text         = copy_string(*slides.active_set.allocator, line);
            text.virtual_x    = options.x;
            text.virtual_y    = options.y;
            text.alignment    = options.alignment;
            text.shadowed     = options.shadowed;
            text.shadow_color = options.shadow_color;
            text.underlined   = options.underlined;
            text.bullet       = options.bullet;
            text.interact     = options.interact;
            text.place_below_element = options.place_next_below_element;

            options.previous_line_was_element = true;
            options.place_next_below_element = text;
        }        
    }    

    slides.watcher.check_interval_in_seconds = 0.5;
    slides.watcher.allocator = *slides.active_set.allocator;
    create_file_watcher(*slides.watcher);
    add_file_to_watch(*slides.watcher, file_path);
}



/* ------------------------------------------------ User Input ------------------------------------------------ */

get_max_interacts :: (slide: *Slide) -> s64 {
    result := 0;

    for i := 0; i < slide.elements.count; ++i {
        element := array_get_pointer(*slide.elements, i);
        result = max(result, element.interact);
    }

    return result;
}

forward :: (slides: *Slides) {
    if slides.active_set.index >= slides.active_set.slides.count {
        slides.active_set.index = slides.active_set.slides.count;
        return;
    }
    
    slide := array_get_pointer(*slides.active_set.slides, slides.active_set.index);
    
    if slide.interacts < get_max_interacts(slide) {
        ++slide.interacts;
    } else if slides.active_set.index < slides.active_set.slides.count {
        slide.interacts = 0;
        ++slides.active_set.index;
    }
}

backward :: (slides: *Slides) {
    if slides.active_set.index >= slides.active_set.slides.count {
        slides.active_set.index = slides.active_set.slides.count - 1;
        return;
    }

    slide := array_get_pointer(*slides.active_set.slides, slides.active_set.index);
    
    if slide.interacts > 0 {
        --slide.interacts;
    } else if slides.active_set.index > 0 {
        slide.interacts = 0;
        --slides.active_set.index;
    }
}

simulate_one_frame :: (slides: *Slides) {
    reloaded_files := update_file_watcher(*slides.watcher, *temp);
    if reloaded_files.count {
        file_path := array_get(*reloaded_files, 0);
        print("Reloading slides '%'...\n", file_path);
        
        previous_index := slides.active_set.index;
        parse_slide_set(slides, file_path);
        if previous_index < slides.active_set.slides.count slides.active_set.index = previous_index;
    }

    if slides.window.keys[.F11] & .Pressed {
        if !slides.window.fullscreen {
            set_window_style(*slides.window, .Fullscreen);
        } else {
            set_window_style(*slides.window, .Maximized);
        }
    }

    if slides.window.keys[.Escape] & .Pressed {
        slides.display_custom_cursor = !slides.display_custom_cursor;
    }
    
    if slides.window.keys[.Arrow_Left] & .Repeated {
        backward(slides);
    } else if slides.window.keys[.Arrow_Right] & .Repeated || slides.window.keys[.Enter] & .Repeated || slides.window.buttons[.Left] & .Pressed {
        forward(slides);
    }
}



/* ------------------------------------------------- Graphics ------------------------------------------------- */

get_font :: (slides: *Slides, display_name: string) -> *Font {
    font := chained_hash_table_query(*slides.active_set.font_table, display_name);
    if font return font;

    name: string = ---;
    size: s16 = ---;
    
    delim, delim_found := search_string(display_name, #char "-");
    if delim_found {
        success := false;
        name = substring_view(display_name, 0, delim);
        size, success = string_to_integer(substring_view(display_name, delim + 1, display_name.count));
        if !success size = 32; // Eh we should probably report an error here?
    } else {
        name = display_name;
        size = 32;
    }
    
    // First look into the local data directory. If no such file exists, try to
    // look into the OS's default font directory.
    file_path := mprint(*temp, "data/%.ttf", name);
    if !file_exists(file_path) {
#if TARGET_PLATFORM == .Windows {
        file_path := mprint(*temp, "C:/Windows/fonts/%.ttf", name);
} #else #if TARGET_PLATFORM == .Linux {
        file_path := mprint(*temp, "/usr/share/fonts/truetype/%.ttf", name);
}
    }
        
    display_name = copy_string(Default_Allocator, display_name);
    font = chained_hash_table_push(*slides.active_set.font_table, display_name);
    success := ge_create_font_from_file(*slides.ge, font, file_path, size, .Extended_Ascii);

    if !success print("Failed to load the font '%'.\n", display_name);
    
    return font;
}

get_image :: (slides: *Slides, file_path: string) -> *GE_Texture {
    relative_file_path := mprint(*temp, "data/%", file_path);
    image, success := ge_create_texture_from_file(*slides.ge, relative_file_path);
    if !success print("Failed to load the image '%'.\n", file_path);
    return image;
}

screen_position_from_virtual :: (viewport: *Viewport, vx, vy: f32) -> f32, f32 {
    return vx * (viewport.br[0] - viewport.tl[0]) + viewport.tl[0],
    vy * (viewport.br[1] - viewport.tl[1]) + viewport.tl[1];
}

screen_size_from_virtual :: (viewport: *Viewport, vw, vh: f32, ratio: f32) -> f32, f32 {
    sx, sy := vw * (viewport.br[0] - viewport.tl[0]), vh * (viewport.br[1] - viewport.tl[1]);

    if vw == -1 && vh == -1 {
        sx = 0.25 * (viewport.br[0] - viewport.tl[0]);
        sy = 0.25 * (viewport.br[1] - viewport.tl[1]);
    } else if vw == -1 {
        sx = sy * ratio;
    } else if vh == -1 {
        sy = sx / ratio;
    }
    
    return sx, sy;
}

calculate_screen_space_height :: (viewport: *Viewport, element: *Element) -> f32 {
    image_height, text_height: f32 = 0;

    if element.image {
        ignored: f32 = ---;
        ignored, image_height = screen_size_from_virtual(viewport, element.virtual_w, element.virtual_h, cast(f32) element.image.w / cast(f32) element.image.h);
    }

    if element.text {
        text_height = cast(f32) element.font.line_height;
    }
    
    return max(image_height, text_height);
}

calculate_screen_space_offset :: (viewport: *Viewport, element: *Element) -> f32 {
    if element.place_below_element == null return 0;

    prev_height := calculate_screen_space_height(viewport, element.place_below_element);
    my_height   := calculate_screen_space_height(viewport, element);

    offset := calculate_screen_space_offset(viewport, element.place_below_element);

    if element.place_below_element.alignment & .Top {
        offset += prev_height;
    } else if element.place_below_element.alignment & .Median {
        offset += prev_height / 2;
    }

    if element.alignment & .Median {
        offset += my_height / 2;
    } else if element.alignment & .Bottom {
        offset += my_height;
    }

    return offset;
}

draw_slide_set :: (slides: *Slides, set: *Slide_Set, viewport: *Viewport) {
    slide := array_get_pointer(*slides.active_set.slides, slides.active_set.index);
    
    ge_imm2d_colored_rect(*slides.ge, viewport.tl[0], viewport.tl[1], viewport.br[0], viewport.br[1], slide.background);

    for i := 0; i < slide.elements.count; ++i {
        SHADOW_OFFSET: f32 : 2;
        SHADOW_SIZE: f32 : 2;

        element := array_get_pointer(*slide.elements, i);

        if slide.interacts < element.interact continue;
        
        sx, sy := screen_position_from_virtual(viewport, element.virtual_x, element.virtual_y);
        sy += calculate_screen_space_offset(viewport, element);
        
        //
        // Draw an image
        //
        if element.image {
            sw, sh: f32 = screen_size_from_virtual(viewport, element.virtual_w, element.virtual_h, cast(f32) element.image.w / cast(f32) element.image.h);

            image_x0, image_y0: f32 = ---;
            image_x1, image_y1: f32 = ---;

            if element.alignment & .Left {
                image_x0 = sx;
                image_x1 = sx + sw;
            } else if element.alignment & .Centered {
                image_x0 = sx - sw / 2;
                image_x1 = sx + sw / 2;
            } else if element.alignment & .Right {
                image_x0 = sx - sw;
                image_x1 = sx;
            }

            if element.alignment & .Top {
                image_y0 = sy;
                image_y1 = sy + sh;
            } else if element.alignment & .Median {
                image_y0 = sy - sh / 2;
                image_y1 = sy + sh / 2;
            } else if element.alignment & .Bottom {
                image_y0 = sy - sh;
                image_y1 = sy;
            }

            if element.shadowed {
                ge_imm2d_colored_rect(*slides.ge, image_x0 - SHADOW_SIZE, image_y0 - SHADOW_SIZE, image_x1 + SHADOW_SIZE, image_y0, element.shadow_color); // Top
                ge_imm2d_colored_rect(*slides.ge, image_x0 - SHADOW_SIZE, image_y1, image_x1 + SHADOW_SIZE, image_y1 + SHADOW_SIZE, element.shadow_color); // Bottom
                ge_imm2d_colored_rect(*slides.ge, image_x0 - SHADOW_SIZE, image_y0, image_x0, image_y1, element.shadow_color); // Left
                ge_imm2d_colored_rect(*slides.ge, image_x1, image_y0, image_x1 + SHADOW_SIZE, image_y1, element.shadow_color); // Right
            }

            ge_imm2d_textured_rect(*slides.ge, image_x0, image_y0, image_x1, image_y1, element.image, .{ 255, 255, 255, 255 });            
        }
        
        //
        // Draw a bullet point
        //
        if element.bullet {
            radius := cast(f32) element.font.line_height * 0.15;

            local_offset_x, local_offset_y: f32 = 0;

            if element.alignment & .Centered {
                local_offset_x -= cast(f32) get_string_width_in_pixels(element.font, element.text) / 2 + radius * 5;
            }

            if element.alignment & .Top {
                local_offset_y += cast(f32) element.font.average_character_height / 2;
            } else if element.alignment & .Bottom {
                local_offset_y -= cast(f32) element.font.average_character_height / 2;
            }

            if element.shadowed ge_imm2d_circle(*slides.ge, sx + local_offset_x + SHADOW_OFFSET, sy + local_offset_y + SHADOW_OFFSET, radius, element.shadow_color);
            
            ge_imm2d_circle(*slides.ge, sx + local_offset_x, sy + local_offset_y, radius, element.color);
            
            if element.alignment & .Left {
                sx += radius * 5;
            } else if element.alignment & .Right {
                sx -= radius * 5;
            }
        }

        //
        // Draw a text underline
        //
        if element.underlined && element.text {
            width:  f32 = cast(f32) get_string_width_in_pixels(element.font, element.text);
            height: f32 = cast(f32) element.font.line_height / 17;
            
            local_offset_x, local_offset_y: f32 = 0;

            if element.alignment & .Right {
                local_offset_x -= width;
            } else if element.alignment & .Centered {
                local_offset_x -= width / 2;
            }

            if element.alignment & .Top {
                local_offset_y += cast(f32) element.font.average_character_height;
            } else if element.alignment & .Median {
                local_offset_y += cast(f32) element.font.average_character_height / 2;
            }
            
            local_offset_x += cast(f32) get_character_cursor_offset_in_pixels(element.font, element.text[0]);
            local_offset_y += cast(f32) element.font.average_character_height * 0.15;
            
            if element.shadowed ge_imm2d_colored_rect(*slides.ge, sx + local_offset_x + SHADOW_OFFSET, sy + local_offset_y + SHADOW_OFFSET, sx + local_offset_x + width + SHADOW_OFFSET, sy + local_offset_y + height + SHADOW_OFFSET, element.shadow_color);
            ge_imm2d_colored_rect(*slides.ge, sx + local_offset_x, sy + local_offset_y, sx + local_offset_x + width, sy + local_offset_y + height, element.color);
        }

        //
        // Draw the text
        //
        if element.text {
            if element.shadowed ge_draw_text(*slides.ge, element.font, element.text, sx + SHADOW_OFFSET, sy + SHADOW_OFFSET, element.alignment, element.shadow_color);
            ge_draw_text(*slides.ge, element.font, element.text, sx, sy, element.alignment, element.color);
        }
    }
}

draw_one_frame :: (slides: *Slides) {
    ge_clear_screen(*slides.ge, .{ 0, 0, 0, 255 });

    if slides.active_set.index < slides.active_set.slides.count {
        viewport := Viewport.{ .[ 0, 0 ], .[ xx slides.window.w, xx slides.window.h ] };
        draw_slide_set(slides, *slides.active_set, *viewport);
    }

    if slides.display_custom_cursor {
        ge_imm2d_circle(*slides.ge, xx slides.window.mouse_x, xx slides.window.mouse_y, 12, .{ 101, 114, 117, 255 });
        ge_imm2d_circle(*slides.ge, xx slides.window.mouse_x, xx slides.window.mouse_y, 10, .{ 255, 255, 255, 255 });
    }
    
    ge_imm2d_flush(*slides.ge);
    ge_swap_buffers(*slides.ge);
}



/* ----------------------------------------------- Entry Point ----------------------------------------------- */

main :: () -> s32 {
    slides: Slides;

    START_IN_FULLSCREEN :: true;
    
    create_temp_allocator(1000000);
    if !create_window(*slides.window, "Slides", WINDOW_DONT_CARE, WINDOW_DONT_CARE, WINDOW_DONT_CARE, WINDOW_DONT_CARE, ifx START_IN_FULLSCREEN then .Fullscreen else .Default) {
        print("Failed to create window.\n");
        return -1;
    }

    if !ge_create(*slides.ge, *slides.window, Default_Allocator) {
        print("Failed to create graphics engine.\n");
        return -1;
    }

    hide_cursor(*slides.window);
    parse_slide_set(*slides, "default.slides");

    while !slides.window.should_close {
        frame_start := window_get_hardware_time();
        
        // Update the window
        update_window(*slides.window);
        simulate_one_frame(*slides);
        
        // Draw one frame
        draw_one_frame(*slides);

        release_temp_allocator(0);
        
        frame_end := window_get_hardware_time();
        window_ensure_frame_time(frame_start, frame_end, 60);
    }

    destroy_slide_set(*slides.active_set);
    ge_destroy(*slides.ge);
    destroy_window(*slides.window);
    destroy_temp_allocator();
    return 0;
}

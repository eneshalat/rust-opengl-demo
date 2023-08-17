use std::io::Cursor;

use glow::{HasContext, NativeBuffer, NativeVertexArray, NativeTexture, NativeProgram, NativeUniformLocation};
use image::io::Reader as ImageReader;
use nalgebra_glm as glm;
use sdl2::video::SwapInterval;

fn load_resource_as_string(path: &str) -> std::io::Result<String> {
    std::fs::read_to_string(path)
}

fn load_resource(path: &str) -> std::io::Result<Vec<u8>> {
    std::fs::read(path)
}

unsafe fn create_objects(gl: &glow::Context, vertices: &[f32], elements: &[u32]) -> std::io::Result<(NativeBuffer, NativeVertexArray, NativeBuffer)> {
    let vertices_u8: &[u8] = core::slice::from_raw_parts(
        vertices.as_ptr() as *const u8,
        vertices.len() * core::mem::size_of::<f32>(),
    );

    let elements_u8: &[u8] = core::slice::from_raw_parts(
        elements.as_ptr() as *const u8,
        elements.len() * core::mem::size_of::<u32>(),
    );

    let vbo = gl.create_buffer().unwrap();
    gl.bind_buffer(glow::ARRAY_BUFFER, Some(vbo));
    gl.buffer_data_u8_slice(glow::ARRAY_BUFFER, vertices_u8, glow::STATIC_DRAW);

    let vao = gl.create_vertex_array().unwrap();
    gl.bind_vertex_array(Some(vao));
    gl.vertex_attrib_pointer_f32(0, 3, glow::FLOAT, false, 8 * std::mem::size_of::<f32>() as i32, 0);
    gl.enable_vertex_attrib_array(0);
    gl.vertex_attrib_pointer_f32(1, 3, glow::FLOAT, false, 8 * std::mem::size_of::<f32>() as i32, 3 * std::mem::size_of::<f32>() as i32);
    gl.enable_vertex_attrib_array(1);
    gl.vertex_attrib_pointer_f32(2, 2, glow::FLOAT, false, 8 * std::mem::size_of::<f32>() as i32, 6 * std::mem::size_of::<f32>() as i32);
    gl.enable_vertex_attrib_array(2);

    let ebo = gl.create_buffer().unwrap();
    gl.bind_buffer(glow::ELEMENT_ARRAY_BUFFER, Some(ebo));
    gl.buffer_data_u8_slice(glow::ELEMENT_ARRAY_BUFFER, elements_u8, glow::STATIC_DRAW);

    Ok((vbo, vao, ebo))
}

unsafe fn load_textures(gl: &glow::Context, texture_files: &[&str]) -> std::io::Result<Vec<NativeTexture>> {
    let mut textures = Vec::new();
    
    for file in texture_files {
        let file_content = load_resource(*file)?;
        let mut img_reader = ImageReader::new(
            Cursor::new(file_content)
        ).with_guessed_format()?.decode().expect("Image read failed.");

        // Because the texture is readed by OpenGL as flipped, we need to preflip beforehand
        // to eliminate the problem.

        img_reader = img_reader.flipv();
        
        let (width, height, _channels) = (
            img_reader.width() as i32,
            img_reader.height() as i32,
            img_reader.color().channel_count() as i32,
        );

        let file_bytes = img_reader.as_bytes();
        let file_texture = gl.create_texture().expect("Cannot create texture.");
        gl.bind_texture(glow::TEXTURE_2D, Some(file_texture));

        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_WRAP_S, glow::MIRRORED_REPEAT as i32);
        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_WRAP_T, glow::MIRRORED_REPEAT as i32);
        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_MIN_FILTER, glow::NEAREST_MIPMAP_LINEAR as i32);
        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_MAG_FILTER, glow::LINEAR as i32);

        gl.tex_image_2d(glow::TEXTURE_2D, 0, glow::RGB as i32, width, height, 
            0, glow::RGB, glow::UNSIGNED_BYTE, Some(file_bytes));
        gl.generate_mipmap(glow::TEXTURE_2D);

        textures.push(file_texture);
    }

    Ok(textures)
}

unsafe fn setup_camera(gl: &glow::Context, program: &NativeProgram) -> (Option<NativeUniformLocation>, Option<NativeUniformLocation>, Option<NativeUniformLocation>) {
    let uni_model = gl.get_uniform_location(*program, "model");
    let uni_view = gl.get_uniform_location(*program, "view");
    let uni_projection = gl.get_uniform_location(*program, "projection");

    (uni_model, uni_view, uni_projection)
}

unsafe fn update_camera(gl: &glow::Context, timer: &sdl2::TimerSubsystem, is_ortho: bool, mvp_u_loc:(Option<NativeUniformLocation>, Option<NativeUniformLocation>, Option<NativeUniformLocation>),
    w_h: (i32, i32)) {

    let ticks = timer.ticks();
    // println!("{}", ticks);

    let mut model = glm::Mat4::identity();
    // model = glm::rotate(&model, (ticks as f32 / 500.0) * f32::to_radians(10.0), &glm::vec3(1.0, 1.0, 0.0));
    model = glm::rotate(&model,  f32::to_radians(0.0), &glm::vec3(1.0, 1.0, 0.0));
    
    let cam_circ_r: f32 = f32::to_radians((ticks as f32 / 50.0) % 360.0).sin() + 2.0;
    let cam_angle: f32 = f32::to_radians((ticks as f32 / 50.0) % 360.0);
    let cam_pos = glm::vec3(cam_circ_r * cam_angle.cos(), 1.0, cam_circ_r * cam_angle.sin());
    // println!("{:?}", (ticks as f32 / 500.0) % (2.0*glm::pi::<f32>()));

    let mut view = glm::Mat4::identity();
    // view = glm::translate(&view, &glm::vec3(0.0, 0.0, -3.0));
    view = glm::look_at(&cam_pos, &glm::vec3(0.0f32, 0.0f32, 0.0f32), &glm::vec3(0.0f32, 1.0f32, 0.0f32));

    let projection = if is_ortho {
        glm::ortho(0.0, w_h.0 as f32, 0.0, w_h.1 as f32, 0.1, 100.0)
    } else {
        glm::perspective(f32::to_radians(45.0), w_h.0 as f32 / w_h.1 as f32, 0.1, 100.0)
    };

    if let Some(u_m) = mvp_u_loc.0 {
        gl.uniform_matrix_4_f32_slice(Some(&u_m), false, glm::value_ptr(&model));
    }

    if let Some(u_v) = mvp_u_loc.1 {
        gl.uniform_matrix_4_f32_slice(Some(&u_v), false, glm::value_ptr(&view));
    }

    if let Some(u_p) = mvp_u_loc.2 {
        gl.uniform_matrix_4_f32_slice(Some(&u_p), false, glm::value_ptr(&projection));
    }
}

unsafe fn setup_shader(gl: &glow::Context, shader_version: &str) -> std::io::Result<NativeProgram> {
    let program = gl.create_program().expect("Cannot create program");   

    let (vertex_shader_source, fragment_shader_source) = (
        load_resource_as_string("res/shader/default.vert")?,
        load_resource_as_string("res/shader/default.frag")?,
    );

    let shader_sources = [
        (glow::VERTEX_SHADER, vertex_shader_source),
        (glow::FRAGMENT_SHADER, fragment_shader_source),
    ];

    let mut shaders = Vec::with_capacity(shader_sources.len());

    for (shader_type, shader_source) in shader_sources.iter() {
        let shader = gl
            .create_shader(*shader_type)
            .expect("Cannot create shader");

        gl.shader_source(shader, &format!("{}\n{}", shader_version, shader_source));
        gl.compile_shader(shader);

        if !gl.get_shader_compile_status(shader) {
            panic!("{}", gl.get_shader_info_log(shader));
        }
        gl.attach_shader(program, shader);
        shaders.push(shader);
    }
    
    gl.link_program(program);
    if !gl.get_program_link_status(program) {
        panic!("{}", gl.get_program_info_log(program));
    }

    for shader in shaders {
        gl.detach_shader(program, shader);
        gl.delete_shader(shader);
    }

    Ok(program)
}

unsafe fn after_resize(gl: &glow::Context, width: i32, height: i32) {
    gl.viewport(0, 0, width, height);
}

unsafe fn toggle_wireframe(gl: &glow::Context, val: bool) {
    gl.polygon_mode(glow::FRONT_AND_BACK, if val { glow::LINE } else { glow::FILL });
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    
    let (mut width, mut height) = (900, 900) as (i32, i32); 

    let (gl, sdl, shader_version, window, mut events_loop, _context) = {
        let sdl = sdl2::init()?;
        let video = sdl.video()?;
        let gl_attr = video.gl_attr();
        gl_attr.set_context_profile(sdl2::video::GLProfile::Core);
        gl_attr.set_context_version(4, 6);

        let window = video
            .window("Rust OpenGL Demo by eneshalat", width as u32, height as u32)
            .opengl()
            .resizable()
            .build()?;

        let gl_context = window.gl_create_context()?;
        let gl = unsafe {
            glow::Context::from_loader_function(|s| video.gl_get_proc_address(s) as *const _)
        };
        unsafe {
            gl.viewport(0, 0, width, height);
        }

        video.gl_set_swap_interval(SwapInterval::VSync)?;

        let event_loop = sdl.event_pump()?;
        (gl, sdl, "#version 460", window, event_loop, gl_context)
    };

    unsafe {
        
        let f_program = setup_shader(&gl, shader_version)?;

        /* let vertices = [
            //    pos     //   color   //  texcoord
            0.5,  0.5, 0.0, 1.0, 0.5, 0.0, 1.0, 1.0,// top right
            0.5, -0.5, 0.0, 0.75, 0.38, 0.0, 1.0, 0.0,// bottom right
            -0.5, -0.5, 0.0, 0.5, 0.25, 0.0, 0.0, 0.0,// bottom left
            -0.5,  0.5, 0.0, 0.75, 0.38, 0.0, 0.0, 1.0, // top left 
        ]; */

        let vertices = [
            -0.5, -0.5, -0.5, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.5, -0.5, -0.5,  0.0, 0.0, 0.0, 1.0, 0.0,
            0.5,  0.5, -0.5,  0.0, 0.0, 0.0, 1.0, 1.0,
            0.5,  0.5, -0.5,  0.0, 0.0, 0.0, 1.0, 1.0,
           -0.5,  0.5, -0.5,  0.0, 0.0, 0.0, 0.0, 1.0,
           -0.5, -0.5, -0.5,  0.0, 0.0, 0.0, 0.0, 0.0,
       
           -0.5, -0.5,  0.5, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.5, -0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
            0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 1.0,
            0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 1.0,
           -0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 0.0, 1.0,
           -0.5, -0.5,  0.5, 0.0, 0.0, 0.0, 0.0, 0.0,
       
           -0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
           -0.5,  0.5, -0.5, 0.0, 0.0, 0.0, 1.0, 1.0,
           -0.5, -0.5, -0.5, 0.0, 0.0, 0.0, 0.0, 1.0,
           -0.5, -0.5, -0.5, 0.0, 0.0, 0.0, 0.0, 1.0,
           -0.5, -0.5,  0.5, 0.0, 0.0, 0.0, 0.0, 0.0,
           -0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
       
            0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
            0.5,  0.5, -0.5, 0.0, 0.0, 0.0, 1.0, 1.0,
            0.5, -0.5, -0.5, 0.0, 0.0, 0.0, 0.0, 1.0,
            0.5, -0.5, -0.5, 0.0, 0.0, 0.0, 0.0, 1.0,
            0.5, -0.5,  0.5, 0.0, 0.0, 0.0, 0.0, 0.0,
            0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
       
           -0.5, -0.5, -0.5, 0.0, 0.0, 0.0, 0.0, 1.0,
            0.5, -0.5, -0.5, 0.0, 0.0, 0.0, 1.0, 1.0,
            0.5, -0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
            0.5, -0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
           -0.5, -0.5,  0.5, 0.0, 0.0, 0.0, 0.0, 0.0,
           -0.5, -0.5, -0.5, 0.0, 0.0, 0.0, 0.0, 1.0,
       
           -0.5,  0.5, -0.5, 0.0, 0.0, 0.0, 0.0, 1.0,
            0.5,  0.5, -0.5, 0.0, 0.0, 0.0, 1.0, 1.0,
            0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
            0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 1.0, 0.0,
           -0.5,  0.5,  0.5, 0.0, 0.0, 0.0, 0.0, 0.0,
           -0.5,  0.5, -0.5, 0.0, 0.0, 0.0, 0.0, 1.0   
        ];

        let elements = [
            0, 1, 3,
            1, 2, 3
        ];

        let (vbo, vao, _ebo) = create_objects(&gl, &vertices, &elements)?;

        let texture_list = load_textures(&gl, &[
            "res/texture/gradient.png"
        ])?;

        // Binding textures...
        for texture in texture_list {
            gl.bind_texture(glow::TEXTURE_2D, Some(texture));
        }

        let timer = sdl.timer().unwrap();

        gl.enable(glow::DEPTH_TEST);

        // Camera initialization...

        gl.use_program(Some(f_program));

        let mvp_loc = setup_camera(&gl, &f_program);

        gl.clear_color(0.1, 0.1, 0.1, 1.0);

        {
            let mut running = true;
            let mut ortho = false;

            let mut wireframe = false;
            
            while running {
                {
                    for event in events_loop.poll_iter() {
                        match event {
                            sdl2::event::Event::Quit { .. } => running = false,
                            sdl2::event::Event::KeyUp { keycode, .. } => {
                                if let Some(k) = keycode {

                                    match k.name().as_str() {
                                        "O" => ortho = !ortho,
                                        "P" => {
                                            wireframe = !wireframe;
                                            toggle_wireframe(&gl, wireframe);
                                        },
                                        _ => {}
                                    };
                                }
                            },
                            sdl2::event::Event::Window { win_event, .. } => match win_event {
                                sdl2::event::WindowEvent::Resized(w, h) => {
                                    after_resize(&gl, w, h);
                                    (width, height) = (w, h);
                                },
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                }

                gl.clear(glow::COLOR_BUFFER_BIT | glow::DEPTH_BUFFER_BIT);

                update_camera(&gl, &timer, ortho, mvp_loc, (width, height));

                gl.draw_arrays(glow::TRIANGLES, 0, 36);
                // gl.draw_elements(glow::TRIANGLES, 6, glow::UNSIGNED_INT, 0);
                window.gl_swap_window();
            }

            gl.delete_program(f_program);
            gl.delete_vertex_array(vao);
            gl.delete_buffer(vbo);
        }
    }

    Ok(())
}

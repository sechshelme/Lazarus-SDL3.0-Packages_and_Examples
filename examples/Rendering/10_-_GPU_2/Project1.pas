program Project1;

// https://github.com/libsdl-org/SDL/issues/9876

uses
  ctypes,
  SDL3,
  test_gpu_spirv;

const
  widht = 320;
  Height = 240;

const
  TESTGPU_SUPPORTED_FORMATS = SDL_GPU_SHADERFORMAT_SPIRV or SDL_GPU_SHADERFORMAT_DXBC or SDL_GPU_SHADERFORMAT_DXIL or SDL_GPU_SHADERFORMAT_METALLIB;

type
  TRenderState = record
    buf_vertex: PSDL_GPUBuffer;
    pipeline: PSDL_GPUGraphicsPipeline;
    sample_count: TSDL_GPUSampleCount;
  end;

  { TWindowState }

  TWindowState = record
    angle_x, angle_y, angle_z: cint;
    tex_depth, tex_msaa, tex_resolve: PSDL_GPUTexture;
    prev_drawablew, prev_drawableh: uint32;
  end;
  PWindowState = ^TWindowState;

var
  render_state: TRenderState;
  window_states: PWindowState;

  window: PSDL_Window;
  gpu_device: PSDL_GPUDevice;

type
  TVertexData = record
    x, y, z, r, g, b: single;
  end;

const
  vertex_data: array[0..35] of TVertexData = (
    { Front face. }
    { Bottom left }
    (x: -0.5; y: 0.5; z: -0.5; r: 1.0; g: 0.0; b: 0.0), { red }
    (x: 0.5; y: -0.5; z: -0.5; r: 0.0; g: 0.0; b: 1.0), { blue }
    (x: -0.5; y: -0.5; z: -0.5; r: 0.0; g: 1.0; b: 0.0), { green }

    { Top right }
    (x: -0.5; y: 0.5; z: -0.5; r: 1.0; g: 0.0; b: 0.0), { red }
    (x: 0.5; y: 0.5; z: -0.5; r: 1.0; g: 1.0; b: 0.0), { yellow }
    (x: 0.5; y: -0.5; z: -0.5; r: 0.0; g: 0.0; b: 1.0), { blue }

    { Left face }
    { Bottom left }
    (x: -0.5; y: 0.5; z: 0.5; r: 1.0; g: 1.0; b: 1.0), { white }
    (x: -0.5; y: -0.5; z: -0.5; r: 0.0; g: 1.0; b: 0.0), { green }
    (x: -0.5; y: -0.5; z: 0.5; r: 0.0; g: 1.0; b: 1.0), { cyan }

    { Top right }
    (x: -0.5; y: 0.5; z: 0.5; r: 1.0; g: 1.0; b: 1.0), { white }
    (x: -0.5; y: 0.5; z: -0.5; r: 1.0; g: 0.0; b: 0.0), { red }
    (x: -0.5; y: -0.5; z: -0.5; r: 0.0; g: 1.0; b: 0.0), { green }

    { Top face }
    { Bottom left }
    (x: -0.5; y: 0.5; z: 0.5; r: 1.0; g: 1.0; b: 1.0), { white }
    (x: 0.5; y: 0.5; z: -0.5; r: 1.0; g: 1.0; b: 0.0), { yellow }
    (x: -0.5; y: 0.5; z: -0.5; r: 1.0; g: 0.0; b: 0.0), { red }

    { Top right }
    (x: -0.5; y: 0.5; z: 0.5; r: 1.0; g: 1.0; b: 1.0), { white }
    (x: 0.5; y: 0.5; z: 0.5; r: 0.0; g: 0.0; b: 0.0), { black }
    (x: 0.5; y: 0.5; z: -0.5; r: 1.0; g: 1.0; b: 0.0), { yellow }

    { Right face }
    { Bottom left }
    (x: 0.5; y: 0.5; z: -0.5; r: 1.0; g: 1.0; b: 0.0), { yellow }
    (x: 0.5; y: -0.5; z: 0.5; r: 1.0; g: 0.0; b: 1.0), { magenta }
    (x: 0.5; y: -0.5; z: -0.5; r: 0.0; g: 0.0; b: 1.0), { blue }

    { Top right }
    (x: 0.5; y: 0.5; z: -0.5; r: 1.0; g: 1.0; b: 0.0), { yellow }
    (x: 0.5; y: 0.5; z: 0.5; r: 0.0; g: 0.0; b: 0.0), { black }
    (x: 0.5; y: -0.5; z: 0.5; r: 1.0; g: 0.0; b: 1.0), { magenta }

    { Back face }
    { Bottom left }
    (x: 0.5; y: 0.5; z: 0.5; r: 0.0; g: 0.0; b: 0.0), { black }
    (x: -0.5; y: -0.5; z: 0.5; r: 0.0; g: 1.0; b: 1.0), { cyan }
    (x: 0.5; y: -0.5; z: 0.5; r: 1.0; g: 0.0; b: 1.0), { magenta }

    { Top right }
    (x: 0.5; y: 0.5; z: 0.5; r: 0.0; g: 0.0; b: 0.0), { black }
    (x: -0.5; y: 0.5; z: 0.5; r: 1.0; g: 1.0; b: 1.0), { white }
    (x: -0.5; y: -0.5; z: 0.5; r: 0.0; g: 1.0; b: 1.0), { cyan }

    { Bottom face }
    { Bottom left }
    (x: -0.5; y: -0.5; z: -0.5; r: 0.0; g: 1.0; b: 0.0), { green }
    (x: 0.5; y: -0.5; z: 0.5; r: 1.0; g: 0.0; b: 1.0), { magenta }
    (x: -0.5; y: -0.5; z: 0.5; r: 0.0; g: 1.0; b: 1.0), { cyan }

    { Top right }
    (x: -0.5; y: -0.5; z: -0.5; r: 0.0; g: 1.0; b: 0.0), { green }
    (x: 0.5; y: -0.5; z: -0.5; r: 0.0; g: 0.0; b: 1.0), { blue }
    (x: 0.5; y: -0.5; z: 0.5; r: 1.0; g: 0.0; b: 1.0)  { magenta }
    );

  function load_shader(is_vertex: boolean): PSDL_GPUShader;
  var
    createinfo: TSDL_GPUShaderCreateInfo;
    format: TSDL_GPUShaderFormat;
  begin
    createinfo.num_samplers := 0;
    createinfo.num_storage_buffers := 0;
    createinfo.num_storage_textures := 0;
    if is_vertex then  begin
      createinfo.num_uniform_buffers := 1;
    end else begin
      createinfo.num_uniform_buffers := 0;
    end;
    createinfo.props := 0;

    format := SDL_GetGPUShaderFormats(gpu_device);

    createinfo.format := SDL_GPU_SHADERFORMAT_SPIRV;
    if is_vertex then  begin
      createinfo.code := PUint8(cube_vert_spv);
      createinfo.code_size := cube_vert_spv_len;
      createinfo.stage := SDL_GPU_SHADERSTAGE_VERTEX;
    end else begin
      createinfo.code := PUint8(cube_frag_spv);
      createinfo.code_size := cube_frag_spv_len;
      createinfo.stage := SDL_GPU_SHADERSTAGE_FRAGMENT;
    end;

    Result := SDL_CreateGPUShader(gpu_device, @createinfo);
  end;

  function CreateDepthTexture(drawablew: uint32; drawableh: uint32): PSDL_GPUTexture;
  var
    createinfo: TSDL_GPUTextureCreateInfo;
  begin
    createinfo._type := SDL_GPU_TEXTURETYPE_2D;
    createinfo.format := SDL_GPU_TEXTUREFORMAT_D16_UNORM;
    createinfo.Width := drawablew;
    createinfo.Height := drawableh;
    createinfo.layer_count_or_depth := 1;
    createinfo.num_levels := 1;
    createinfo.sample_count := render_state.sample_count;
    createinfo.usage := SDL_GPU_TEXTUREUSAGE_DEPTH_STENCIL_TARGET;
    createinfo.props := 0;

    Result := SDL_CreateGPUTexture(gpu_device, @createinfo);
    if Result = nil then begin
      WriteLn('CreateDepthTexture  error.');
    end;
  end;

  function CreateMSAATexture(drawablew: uint32; drawableh: uint32): PSDL_GPUTexture;
  var
    createinfo: TSDL_GPUTextureCreateInfo;
  begin
    if render_state.sample_count = SDL_GPU_SAMPLECOUNT_1 then begin
      Exit(nil);
    end;

    createinfo._type := SDL_GPU_TEXTURETYPE_2D;
    createinfo.format := SDL_GetGPUSwapchainTextureFormat(gpu_device, window);
    createinfo.Width := drawablew;
    createinfo.Height := drawableh;
    createinfo.layer_count_or_depth := 1;
    createinfo.num_levels := 1;
    createinfo.sample_count := render_state.sample_count;
    createinfo.usage := SDL_GPU_TEXTUREUSAGE_COLOR_TARGET;
    createinfo.props := 0;

    Result := SDL_CreateGPUTexture(gpu_device, @createinfo);
    if Result = nil then begin
      WriteLn('CreateMSAATexture()  error.');
    end;
  end;

  function CreateResolveTexture(drawablew: uint32; drawableh: uint32): PSDL_GPUTexture;
  var
    createinfo: TSDL_GPUTextureCreateInfo;
  begin
    if render_state.sample_count = SDL_GPU_SAMPLECOUNT_1 then begin
      Exit(nil);
    end;

    createinfo._type := SDL_GPU_TEXTURETYPE_2D;
    createinfo.format := SDL_GetGPUSwapchainTextureFormat(gpu_device, window);
    createinfo.Width := drawablew;
    createinfo.Height := drawableh;
    createinfo.layer_count_or_depth := 1;
    createinfo.num_levels := 1;
    createinfo.sample_count := SDL_GPU_SAMPLECOUNT_1;
    createinfo.usage := SDL_GPU_TEXTUREUSAGE_COLOR_TARGET or SDL_GPU_TEXTUREUSAGE_SAMPLER;
    createinfo.props := 0;

    Result := SDL_CreateGPUTexture(gpu_device, @createinfo);
    if Result = nil then begin
      WriteLn('CreateResolveTexture()  error.');
    end;
  end;

  procedure init_renderer_state(msaa: longint);
  var
    vertex_shader, fragment_shader: PSDL_GPUShader;
    buffer_desc: TSDL_GPUBufferCreateInfo;
    buf_transfer: PSDL_GPUTransferBuffer;
    transfer_buffer_desc: TSDL_GPUTransferBufferCreateInfo;
    map: Pointer;
    cmd: PSDL_GPUCommandBuffer;
    copy_pass: PSDL_GPUCopyPass;
    buf_location: TSDL_GPUTransferBufferLocation;
    dst_region: TSDL_GPUBufferRegion;
    pipelinedesc: TSDL_GPUGraphicsPipelineCreateInfo;
    color_target_desc: TSDL_GPUColorTargetDescription;
    drawablew, drawableh: uint32;

    vertex_buffer_desc: TSDL_GPUVertexBufferDescription;
    vertex_attributes: array[0..1] of TSDL_GPUVertexAttribute;
    winstate: PWindowState;
  begin
    gpu_device := SDL_CreateGPUDevice(TESTGPU_SUPPORTED_FORMATS, True, nil);
//        gpu_device := SDL_CreateGPUDevice(TESTGPU_SUPPORTED_FORMATS, True, 'vulkan');
    if gpu_device = nil then begin
      WriteLn('gpu_device  error.');
    end;

    if not SDL_ClaimWindowForGPUDevice(gpu_device, window) then begin
      SDL_Log('Claim error!:  %s', SDL_GetError);
    end;

    vertex_shader := load_shader(True);
    if vertex_shader = nil then begin
      WriteLn('vertex_shader  error.');
    end;

    fragment_shader := load_shader(True);
    if fragment_shader = nil then begin
      WriteLn('fragment_shader  error.');
    end;

    buffer_desc.usage := SDL_GPU_BUFFERUSAGE_VERTEX;
    buffer_desc.size := SizeOf(vertex_data);
    buffer_desc.props := 0;

    render_state.buf_vertex := SDL_CreateGPUBuffer(gpu_device, @buffer_desc);
    if render_state.buf_vertex = nil then begin
      WriteLn('render_state.buf_vertex  error.');
    end;

    SDL_SetGPUBufferName(gpu_device, render_state.buf_vertex, 'космонавт');

    transfer_buffer_desc.usage := SDL_GPU_TRANSFERBUFFERUSAGE_UPLOAD;
    transfer_buffer_desc.size := SizeOf(vertex_data);
    transfer_buffer_desc.props := 0;
    buf_transfer := SDL_CreateGPUTransferBuffer(gpu_device, @transfer_buffer_desc);
    if buf_transfer = nil then begin
      WriteLn('bus_transfer  error.');
    end;

    map := SDL_MapGPUTransferBuffer(gpu_device, buf_transfer, False);
    SDL_memcpy(map, @vertex_data[0], SizeOf(vertex_data));
    SDL_UnmapGPUTransferBuffer(gpu_device, buf_transfer);

    cmd := SDL_AcquireGPUCommandBuffer(gpu_device);
    copy_pass := SDL_BeginGPUCopyPass(cmd);
    buf_location.transfer_buffer := buf_transfer;
    buf_location.offset := 0;
    dst_region.buffer := render_state.buf_vertex;
    dst_region.offset := 0;
    dst_region.size := SizeOf(vertex_data);
    SDL_UploadToGPUBuffer(copy_pass, @buf_location, @dst_region, False);
    SDL_EndGPUCopyPass(copy_pass);
    SDL_SubmitGPUCommandBuffer(cmd);

    SDL_ReleaseGPUTransferBuffer(gpu_device, buf_transfer);

    render_state.sample_count := SDL_GPU_SAMPLECOUNT_1;
    if (msaa <> 0) and SDL_GPUTextureSupportsSampleCount(gpu_device, SDL_GetGPUSwapchainTextureFormat(gpu_device, window), SDL_GPU_SAMPLECOUNT_4) then begin
      render_state.sample_count := SDL_GPU_SAMPLECOUNT_4;
    end;

    pipelinedesc := Default(TSDL_GPUGraphicsPipelineCreateInfo);
    color_target_desc := Default(TSDL_GPUColorTargetDescription);

    color_target_desc.format := SDL_GetGPUSwapchainTextureFormat(gpu_device, window);

    pipelinedesc.target_info.num_color_targets := 1;
    pipelinedesc.target_info.color_target_descriptions := @color_target_desc;
    pipelinedesc.target_info.depth_stencil_format := SDL_GPU_TEXTUREFORMAT_D16_UNORM;
    pipelinedesc.target_info.has_depth_stencil_target := True;

    pipelinedesc.depth_stencil_state.enable_depth_test := True;
    pipelinedesc.depth_stencil_state.enable_depth_write := True;
    pipelinedesc.depth_stencil_state.compare_op := SDL_GPU_COMPAREOP_LESS_OR_EQUAL;

    pipelinedesc.multisample_state.sample_count := render_state.sample_count;

    pipelinedesc.primitive_type := SDL_GPU_PRIMITIVETYPE_TRIANGLELIST;

    pipelinedesc.vertex_shader := vertex_shader;
    pipelinedesc.fragment_shader := fragment_shader;

    vertex_buffer_desc.slot := 0;
    vertex_buffer_desc.input_rate := SDL_GPU_VERTEXINPUTRATE_VERTEX;
    vertex_buffer_desc.instance_step_rate := 0;
    vertex_buffer_desc.pitch := SizeOf(TVertexData);

    vertex_attributes[0].buffer_slot := 0;
    vertex_attributes[0].format := SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3;
    vertex_attributes[0].location := 0;
    vertex_attributes[0].offset := 0;

    vertex_attributes[1].buffer_slot := 0;
    vertex_attributes[1].format := SDL_GPU_VERTEXELEMENTFORMAT_FLOAT3;
    vertex_attributes[1].location := 1;
    vertex_attributes[1].offset := SizeOf(cfloat) * 3;

    pipelinedesc.vertex_input_state.num_vertex_buffers := 1;
    pipelinedesc.vertex_input_state.vertex_buffer_descriptions := @vertex_buffer_desc;
    pipelinedesc.vertex_input_state.num_vertex_attributes := 2;
    pipelinedesc.vertex_input_state.vertex_attributes := PSDL_GPUVertexAttribute(@vertex_attributes);

    pipelinedesc.props := 0;

    WriteLn(11111);
//    render_state.pipeline := SDL_CreateGPUGraphicsPipeline(gpu_device, @pipelinedesc);
    render_state.pipeline := SDL_CreateGPUGraphicsPipeline(gpu_device, @pipelinedesc);
    if render_state.pipeline = nil then begin
      WriteLn('render_state.pipeline  error.');
    end;
    WriteLn(11111);

    SDL_ReleaseGPUShader(gpu_device, vertex_shader);
    SDL_ReleaseGPUShader(gpu_device, fragment_shader);

    window_states := PWindowState(SDL_calloc(1, SizeOf(TWindowState)));
    if window_states = nil then begin
      WriteLn('window_states  error.');
    end;

    winstate := @window_states[0];

    SDL_GetWindowSizeInPixels(window, pcint(@drawablew), pcint(@drawableh));
    WriteLn(drawablew, 'x', drawableh);
    winstate^.tex_depth := CreateDepthTexture(drawablew, drawableh);
    winstate^.tex_msaa := CreateMSAATexture(drawablew, drawableh);
    winstate^.tex_resolve := CreateResolveTexture(drawablew, drawableh);

    winstate^.angle_x := 0;
    winstate^.angle_y := 0;
    winstate^.angle_z := 0;
  end;

  procedure Render(window: PSDL_Window);
  var
    winstate: PWindowState;
    swapchainTexture: PSDL_GPUTexture;
    color_target: TSDL_GPUColorTargetInfo;
    depth_target: TSDL_GPUDepthStencilTargetInfo;
    matrix_rotate: array[0..15] of single = (1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
    matrix_final: array[0..15] of single = (1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);
    cmd: PSDL_GPUCommandBuffer;
    pass: PSDL_GPURenderPass;
    vertex_binding: TSDL_GPUBufferBinding;
    blit_info: TSDL_GPUBlitInfo;
    drawablew, drawableh: DWord;
  begin
    winstate := @window_states[0];
    cmd := SDL_AcquireGPUCommandBuffer(gpu_device);
    if cmd = nil then begin
      WriteLn('Render cmd  error.');
    end;
    if not SDL_AcquireGPUSwapchainTexture(cmd, window, @swapchainTexture, @drawablew, @drawableh) then begin
      WriteLn('swapchain error');
    end;

    if swapchainTexture = nil then begin
      SDL_SubmitGPUCommandBuffer(cmd);
    end;

    // matrix zeugs --------

    if (winstate^.prev_drawablew <> drawablew) or (winstate^.prev_drawableh <> drawableh) then begin
      SDL_ReleaseGPUTexture(gpu_device, winstate^.tex_depth);
      SDL_ReleaseGPUTexture(gpu_device, winstate^.tex_msaa);
      SDL_ReleaseGPUTexture(gpu_device, winstate^.tex_resolve);
      winstate^.tex_depth := CreateDepthTexture(drawablew, drawableh);
      winstate^.tex_msaa := CreateMSAATexture(drawablew, drawableh);
      winstate^.tex_resolve := CreateResolveTexture(drawablew, drawableh);
    end;
    winstate^.prev_drawablew := drawablew;
    winstate^.prev_drawableh := drawableh;

    color_target := Default(TSDL_GPUColorTargetInfo);
    color_target.clear_color.a := 1.0;
    if winstate^.tex_msaa <> nil then begin
      color_target.load_op := SDL_GPU_LOADOP_CLEAR;
      color_target.store_op := SDL_GPU_STOREOP_RESOLVE;
      color_target.texture := winstate^.tex_msaa;
      color_target.resolve_texture := winstate^.tex_resolve;
      color_target.cycle := True;
      color_target.cycle_resolve_texture := True;
    end else begin
      color_target.load_op := SDL_GPU_LOADOP_CLEAR;
      color_target.store_op := SDL_GPU_STOREOP_STORE;
      color_target.texture := swapchainTexture;
    end;

    depth_target := Default(TSDL_GPUDepthStencilTargetInfo);
    depth_target.clear_depth := 1.0;
    depth_target.load_op := SDL_GPU_LOADOP_CLEAR;
    depth_target.store_op := SDL_GPU_STOREOP_DONT_CARE;
    depth_target.stencil_load_op := SDL_GPU_LOADOP_DONT_CARE;
    depth_target.stencil_store_op := SDL_GPU_STOREOP_DONT_CARE;
    depth_target.texture := winstate^.tex_depth;
    depth_target.cycle := True;

    vertex_binding.buffer := render_state.buf_vertex;
    vertex_binding.offset := 0;

    SDL_PushGPUVertexUniformData(cmd, 0, @matrix_final, SizeOf(matrix_final));

    pass := SDL_BeginGPURenderPass(cmd, @color_target, 1, @depth_target);
    SDL_BindGPUGraphicsPipeline(pass, render_state.pipeline);
    SDL_BindGPUVertexBuffers(pass, 0, @vertex_binding, 1);
    SDL_DrawGPUPrimitives(pass, 36, 1, 0, 0);
    SDL_EndGPURenderPass(pass);

    if render_state.sample_count = SDL_GPU_SAMPLECOUNT_1 then begin
      blit_info := Default(TSDL_GPUBlitInfo);
      blit_info.Source.texture := winstate^.tex_resolve;
      blit_info.Source.w := drawablew;
      blit_info.Source.h := drawableh;

      blit_info.destination.texture := swapchainTexture;
      blit_info.destination.w := drawablew;
      blit_info.destination.h := drawableh;

      blit_info.load_op := SDL_GPU_LOADOP_DONT_CARE;
      blit_info.filter := SDL_GPU_FILTER_LINEAR;

      SDL_BlitGPUTexture(cmd, @blit_info);
    end;

    SDL_SubmitGPUCommandBuffer(cmd);
  end;

  procedure loop;
  begin
    Render(window);
  end;

  procedure main;
  var
    msaa: cint = 0;

    event: TSDL_Event;
    quit: boolean = False;
    mode: PSDL_DisplayMode;
    dw, dh: longint;
    cmdbuf: PSDL_GPUCommandBuffer;
    swapchainTexture: PSDL_GPUTexture = nil;
    renderPass: PSDL_GPURenderPass;
    color_target_info: TSDL_GPUColorTargetInfo;
    now: TUint64;
    thn: TUint64 = 0;
    frames: TUint64 = 0;
    currentTime: double;
  begin
    if not SDL_init(SDL_INIT_VIDEO) then begin
      SDL_Log('Konnte SDL-VIDEO nicht laden!:  %s', SDL_GetError);
    end;

    window := SDL_CreateWindow('Palette', widht * 2, Height * 2, 0);
    if window = nil then begin
      SDL_Log('Konnte kein Windows erzeugen!:  %s', SDL_GetError);
    end;

    //gpu_device := SDL_CreateGPUDevice(TESTGPU_SUPPORTED_FORMATS, True, nil);
    //if gpu_device = nil then begin
    //  SDL_Log('Konnte keine GPU-Device erzeugen!:  %s', SDL_GetError);
    //end;
    //
    //if not SDL_ClaimWindowForGPUDevice(gpu_device, window) then begin
    //  SDL_Log('Claim error!:  %s', SDL_GetError);
    //end;

    mode := SDL_GetCurrentDisplayMode(SDL_GetPrimaryDisplay);
    if mode <> nil then begin
      SDL_Log('Screen BPP     : %d', SDL_BITSPERPIXEL(mode^.format));
    end;

    init_renderer_state(msaa);

    ////////////


    thn := SDL_GetTicks();

    while not quit do begin
      loop();
      while SDL_PollEvent(@event) do begin
        case event._type of
          SDL_EVENT_KEY_DOWN: begin
            case event.key.key of
              SDLK_ESCAPE: begin
                quit := True;
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;
    end;

    now := SDL_GetTicks;
    if now > thn then begin
      SDL_log('%2.2f frames per secound'#10, frames * 1000 / (now - thn));
    end;

    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.

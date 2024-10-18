program Project1;

// https://github.com/libsdl-org/SDL/issues/9876

uses
  SDL3;

const
  widht = 320;
  Height = 240;

const
  TESTGPU_SUPPORTED_FORMATS = SDL_GPU_SHADERFORMAT_SPIRV or SDL_GPU_SHADERFORMAT_DXBC or SDL_GPU_SHADERFORMAT_DXIL or SDL_GPU_SHADERFORMAT_METALLIB;


  procedure main;
  var
    window: PSDL_Window;
    event: TSDL_Event;
    quit: boolean = False;
    gpu_device: PSDL_GPUDevice;
    mode: PSDL_DisplayMode;
    dw, dh: longint;
    cmdbuf: PSDL_GPUCommandBuffer;
    swapchainTexture: PSDL_GPUTexture = nil;
    w, h: DWord;
    renderPass: PSDL_GPURenderPass;
    color_target_info: TSDL_GPUColorTargetInfo;
  begin
    if not SDL_init(SDL_INIT_VIDEO) then begin
      SDL_Log('Konnte SDL-VIDEO nicht laden!:  %s', SDL_GetError);
    end;

    window := SDL_CreateWindow('Palette', widht * 2, Height * 2, 0);
    if window = nil then begin
      SDL_Log('Konnte kein Windows erzeugen!:  %s', SDL_GetError);
    end;

    gpu_device := SDL_CreateGPUDevice(TESTGPU_SUPPORTED_FORMATS, True, nil);
    if gpu_device = nil then begin
      SDL_Log('Konnte keine GPU-Device erzeugen!:  %s', SDL_GetError);
    end;

    if not SDL_ClaimWindowForGPUDevice(gpu_device, window) then begin
      SDL_Log('Claim error!:  %s', SDL_GetError);
    end;

    mode := SDL_GetCurrentDisplayMode(SDL_GetPrimaryDisplay);
    if mode <> nil then begin
      SDL_Log('Screen BPP     : %d', SDL_BITSPERPIXEL(mode^.format));
    end;

    SDL_GetWindowSize(window, @dw, @dh);
    SDL_Log('Windows Size   : %dx%d', dw, dh);
    SDL_GetWindowSizeInPixels(window, @dw, @dh);
    SDL_Log('Draw Size      : %dx%d', dw, dh);


    while not quit do begin
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

      cmdbuf := SDL_AcquireGPUCommandBuffer(gpu_device);
      if cmdbuf = nil then begin
        SDL_Log('Konnte kein cmdbuf erzeugen!:  %s', SDL_GetError);
      end;

      w := 0;
      h := 0;
      if not SDL_AcquireGPUSwapchainTexture(cmdbuf, window, @swapchainTexture, @w, @h) then begin
        SDL_Log('Konnte keine swap chain Textur erzeugen!:  %s', SDL_GetError);
      end;
      WriteLn(w, ' x ', h);


      if swapchainTexture = nil then begin
        WriteLn('error');
        //     exit;
      end else begin
        FillChar(color_target_info, SizeOf(color_target_info), $00);

        color_target_info.texture := swapchainTexture;
        color_target_info.clear_color.r := random;
        color_target_info.clear_color.g := random;
        color_target_info.clear_color.b := random;
        color_target_info.clear_color.a := 1.0;
        color_target_info.load_op := SDL_GPU_LOADOP_CLEAR;
        color_target_info.store_op := SDL_GPU_STOREOP_STORE;

        renderPass := SDL_BeginGPURenderPass(cmdbuf, @color_target_info, 1, nil);
        if renderPass = nil then begin
          WriteLn('nil');
        end else begin
          WriteLn('io.');
        end;
        SDL_EndGPURenderPass(renderPass);

        SDL_SubmitGPUCommandBuffer(cmdbuf);

      end;

    end;

    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.

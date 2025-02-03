program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3,
  SDL3_ttf,
  SDL3_rtf,
  RTF_Tools;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  ctx: PRTF_Context;

  procedure SDLMain;
  const
    step = 0.00001;
  var
    e: TSDL_Event;
    quit: boolean = False;
    rDest: TSDL_FRect;
    keyStat: PBoolean;
    time: extended;
    red, green, blue: int64;
  begin
    rDest.x := 0;
    rDest.y := 0;
    rDest.w := 100;
    rDest.h := 100;
    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      if keyStat[SDL_SCANCODE_SPACE] then begin
      end;

      if keyStat[SDL_SCANCODE_RIGHT] then begin
        if keyStat[SDL_SCANCODE_LSHIFT] then begin
          rDest.x -= step;
          rDest.w += step * 2;
        end else begin
          rDest.x += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_LEFT] then begin
        if keyStat[SDL_SCANCODE_LSHIFT] then begin
          if rDest.w > 1 then begin
            rDest.x += step;
            rDest.w -= step * 2;
          end;
        end else begin
          rDest.x -= step;
        end;
      end;
      if keyStat[SDL_SCANCODE_DOWN] then begin
        if keyStat[SDL_SCANCODE_LSHIFT] then begin
          rDest.y -= step;
          rDest.h += step * 2;
        end else begin
          rDest.y += step;
        end;
      end;
      if keyStat[SDL_SCANCODE_UP] then begin
        if keyStat[SDL_SCANCODE_LSHIFT] then begin
          if rDest.h > 1 then begin
            rDest.y += step;
            rDest.h -= step * 2;
          end;
        end else begin
          rDest.y -= step;
        end;
      end;

      if rDest.h < 1 then begin
        rDest.h := 1;
      end;

      while SDL_PollEvent(@e) do begin
        case e._type of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.key of

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

      time := SDL_GetTicks / 1000;
      red := Trunc((SDL_sinf(time) + 1) / 2.0 * 255);
      green := Trunc((SDL_sinf(time / 2) + 1) / 2.0 * 255);
      blue := Trunc((SDL_sinf(time / 3) + 1) / 2.0 * 255);

      SDL_SetRenderDrawColor(renderer, red, green, blue, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);

      RTF_Render(ctx, nil, 0);

      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);
  TTF_Init;

  window := SDL_CreateWindow('SDL3 Window', 800, 600, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !');
  end;

  renderer := SDL_CreateRenderer(window, nil);
  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  ctx := GetRTF_ctx(renderer);

  SDL_Log('io');
  if RTF_Load(ctx, 'text.rtf') <> 0 then  begin
    SDL_Log('Kann kein RTF-Datei nicht laden !    %s', RTF_GetError);
  end;
  SDL_Log('io');

  SDLMain;

  RTF_FreeContext(ctx);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  TTF_Quit;
  SDL_Quit;
end.

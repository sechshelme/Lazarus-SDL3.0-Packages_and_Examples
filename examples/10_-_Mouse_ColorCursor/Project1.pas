program Project1;

// https://wiki.libsdl.org/SDL3/SDL_CreateColorCursor
//https://wiki.libsdl.org/SDL3/SDL_CreateCursor

uses
  SDL3,
  ctypes;

var
  win: PSDL_Window;
  renderer: PSDL_Renderer;
  surface: PSDL_Surface;

   customColorCursor: PSDL_Cursor;

  procedure Init;
  begin
    SDL_Init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('Cursor', 640, 480, SDL_WINDOW_RESIZABLE);
    if win = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Konnte Fenster nicht erzeugen !');
      Halt(-1);
    end;

    renderer := SDL_CreateRenderer(win, nil, SDL_RENDERER_ACCELERATED);
    if renderer = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Kann kein SDL-Renderer erzeugen !');
      Halt(-1);
    end;

    surface:=SDL_LoadBMP('mauer.bmp');
    if surface = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Kann kein BMP nicht laden !');
      Halt(-1);
    end;

    customColorCursor:=SDL_CreateColorCursor(surface,0,0);
    if customColorCursor = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Kann kein Cursor laden !');
      Halt(-1);
    end;
    SDL_SetCursor(customColorCursor);
  end;

  procedure Render;
  begin
    SDL_SetRenderDrawColor(renderer, $FF, $80, $80, 255);
    SDL_RenderClear(renderer);
    SDL_RenderPresent(renderer);
  end;

  procedure Event;
  var
    quit: boolean = False;
    e: TSDL_Event;
    sym: TSDL_KeyCode;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) <> 0 do begin
        case e.type_ of
          SDL_EVENT_MOUSE_BUTTON_DOWN: begin
            case e.button.button of
              SDL_BUTTON_LEFT: begin
              end;
              SDL_BUTTON_RIGHT: begin
              end;
            end;
            WriteLn('dows');
          end;
          SDL_EVENT_KEY_DOWN: begin
            sym := e.key.keysym.sym;
            case sym of
              SDLK_ESCAPE, SDLK_AC_BACK: begin
                quit := True;
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

      Render;
    end;
  end;

  procedure Destroy;
  begin
    SDL_DestroyCursor(customColorCursor);
    SDL_DestroySurface(surface);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);
  end;

begin
  Init;
  Event;
  Destroy;
  SDL_Log('Ende');
end.

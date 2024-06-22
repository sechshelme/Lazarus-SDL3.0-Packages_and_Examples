program Project1;

// https://wiki.libsdl.org/SDL3/SDL_CreateColorCursor

uses
  SDL3,
  ctypes;

var
  win: PSDL_Window;
  renderer: PSDL_Renderer;
  surface: PSDL_Surface;
  customColorCursor: PSDL_Cursor;

  function CreateSurface: PSDL_Surface;
  const
    size = 256;
  var
    r: TSDL_Rect;
    i: Integer;
  begin
    Result := SDL_CreateSurface(size, size, SDL_PIXELFORMAT_RGBA8888);
    for i := 0 to size div 2 do begin
      r.x := i;
      r.y := i;
      r.w := i*2;
      r.h := i*2;

      SDL_FillSurfaceRect(Result, @r, (i * 223) shl 8 + $FF);
    end;
  end;

  procedure Init;
  begin
    SDL_Init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('Cursor', 640, 480, SDL_WINDOW_RESIZABLE);
    if win = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Konnte Fenster nicht erzeugen !');
      Halt(-1);
    end;

    renderer := SDL_CreateRenderer(win, nil);
    if renderer = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Kann kein SDL-Renderer erzeugen !');
      Halt(-1);
    end;

    surface := CreateSurface;

    customColorCursor := SDL_CreateColorCursor(surface, 0, 0);
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
      while SDL_PollEvent(@e) do begin
        case e._type of
          SDL_EVENT_MOUSE_BUTTON_DOWN: begin
            case e.button.button of
              SDL_BUTTON_LEFT: begin
              end;
              SDL_BUTTON_RIGHT: begin
              end;
            end;
          end;
          SDL_EVENT_KEY_DOWN: begin
            sym := e.key.key;
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
    SDL_Quit;
  end;

begin
  Init;
  Event;
  Destroy;
end.

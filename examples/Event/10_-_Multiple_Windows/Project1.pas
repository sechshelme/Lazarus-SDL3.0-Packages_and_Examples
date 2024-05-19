program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3;

var
  window: array[0..3] of PSDL_Window;

  procedure Run;
  var
    event: TSDL_Event;
    quit: boolean = False;
    win: PSDL_Window;
    title: PChar;

  begin
    while not quit do begin
      while SDL_PollEvent(@event) do begin
        case event.type_ of
          SDL_EVENT_KEY_DOWN: begin
            SDL_Log('key: %i', event.key.keysym.sym); // neu

            case event.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_t: begin
                SDL_SetWindowTitle(window[1], 'Hallo');
              end;
            end;
          end;
          SDL_EVENT_WINDOW_RESIZED: begin
            win := SDL_GetWindowFromID(event.window.windowID);
            title := SDL_GetWindowTitle(win);
            WriteLn(title);
          end;
        end;
      end;

    end;
  end;

  procedure main;
  var
    i: integer;
    s: string;
  begin
    SDL_init(SDL_INIT_VIDEO);

    for i := 0 to Length(window) - 1 do begin
      str(i, s);
      window[i] := SDL_CreateWindow(PChar('SDL3 Window (' + s + ')'), 320, 200, SDL_WINDOW_RESIZABLE or SDL_WINDOW_OPENGL);
      if window[i] = nil then begin
        SDL_Log('Kann kein SDL-Fenster erzeugen !');
      end;
      SDL_SetWindowPosition(window[i], i * 100 + 500, i * 75);
      SDL_ShowWindow(window[i]);
    end;

    Run;

    for i := 0 to Length(window) - 1 do begin
      SDL_DestroyWindow(window[i]);
    end;

    SDL_Quit;
    SDL_Log('Application quit successfully!');
  end;

begin
  main;
end.

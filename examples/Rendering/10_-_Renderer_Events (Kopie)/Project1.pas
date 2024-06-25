program Project1;

uses
  SDL3;
var
  window, window2: PSDL_Window;


  procedure SDLMain;
  var
    e: TSDL_Event;
    quit: boolean = False;

  begin
    while not quit do begin
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
            WriteLn('quit');
          end;
        end;
      end;

    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 800, 600, 0);
    window2 := SDL_CreateWindow('SDL3 Window', 800, 600, 0);

  SDLMain;

  SDL_DestroyWindow(window);
  SDL_DestroyWindow(window2);

  SDL_Quit;
end.

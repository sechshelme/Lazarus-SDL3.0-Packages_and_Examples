program Project1;

uses
  sdl3,
  GL;

var
  window: PSDL_Window;
  e: TSDL_Event;
  quit: boolean = False;
  glcontext: TSDL_GLContext;

begin
  SDL_init(SDL_INIT_VIDEO);
  window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE);

  glcontext := SDL_GL_CreateContext(window);

  glClearColor(0, 1, 0, 0);

  while not quit do begin
    while SDL_PollEvent(@e) do begin
      case e._type of
        SDL_EVENT_KEY_DOWN: begin
          case e.key.key of

            SDLK_ESCAPE: begin
              WriteLn('down');
              quit := True;
            end;
            SDLK_r: begin
              glClearColor(1, 0, 0, 0);
            end;
            SDLK_g: begin
              glClearColor(0, 1, 0, 0);
            end;
            SDLK_b: begin
              glClearColor(0, 0, 1, 0);
            end;
          end;
        end;
        SDL_EVENT_QUIT: begin
          WriteLn('quit');
          quit := True;
        end;
      end;
    end;
    glClear(GL_COLOR_BUFFER_BIT);

    SDL_GL_SwapWindow(window);
  end;

  SDL_DestroyWindow(window);
  SDL_GL_DeleteContext(glcontext);
  SDL_Quit;
end.

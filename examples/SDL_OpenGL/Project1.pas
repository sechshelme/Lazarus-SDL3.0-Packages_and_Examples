program Project1;

uses
  sdl3,
  SDL3_opengl;

var
  ver: TSDL_Version;
  window: PSDL_Window;
  e: TSDL_Event;
  quit: boolean = False;
  image, screen: PSDL_Surface;
  dstrect: TSDL_Rect = (x: 100; y: 100; w: 200; h: 200);
  glcontext: TSDL_GLContext;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE);

  glcontext := SDL_GL_CreateContext(window);

  glClearColor(0, 1, 0, 0);

  while not quit do begin
    while SDL_PollEvent(@e) <> 0 do begin
      case e.type_ of
        //        SDL_KEYDOWN: begin
        SDL_EVENT_KEY_DOWN: begin
          case e.key.keysym.sym of

            SDLK_ESCAPE: begin
              //            SDLK_ESCAPE: begin
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
          //          SDL_QUITEV: begin
          quit := True;
        end;
      end;
    end;
    glClear(GL_COLOR_BUFFER_BIT);

    SDL_GL_SwapWindow(window);
  end;


  //  SDL_Delay(3000);
  SDL_DestroyWindow(window);

  SDL_VERSION(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);
  SDL_GetVersion(@ver);
  WriteLn(ver.major, '.', ver.minor, '.', ver.patch);

  SDL_GL_DeleteContext(glcontext);
  SDL_Quit;

end.

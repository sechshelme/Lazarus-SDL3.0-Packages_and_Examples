program Project1;

// https://github.com/libsdl-org/SDL/issues/9631

uses
  SDL3;

var
  win: PSDL_Window;

  procedure loadFile(userdata: pointer; filelist: PPchar; filter: longint); cdecl;
  begin
    WriteLn('loadfile');
  end;

  procedure FileOpen;
  var
    filters: array of TSDL_DialogFileFilter = (
      (Name: 'Alle Dateien'; pattern: '*'),
      (Name: 'JPG Bilder'; pattern: 'jpg;jpeg'),
      (Name: 'PNG Bilder'; pattern: 'png'),
      (Name: nil; pattern: nil));

  begin
    WriteLn('open');
    SDL_ShowOpenFileDialog(@loadFile, nil, win, PSDL_DialogFileFilter(filters), 'test.txt', SDL_FALSE);
  end;


  procedure main;
  var
    renderer: PSDL_Renderer;
    quit: TSDL_bool = SDL_FALSE;
    e: TSDL_Event;
  begin
    SDL_init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('SDL3 Window', 640, 480, SDL_WINDOW_RESIZABLE);
    if win = nil then begin
      WriteLn('Could not create window: ', SDL_GetError);
      Halt(1);
    end;
    renderer := SDL_CreateRenderer(win, nil, SDL_RENDERER_PRESENTVSYNC);
    if renderer = nil then begin
      WriteLn('Could not create renderer: ', SDL_GetError);
      Halt(1);
    end;

    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e.type_ of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_o: begin
                FileOpen;
              end;
              SDLK_SPACE: begin
                SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_INFORMATION, 'Info', 'Es wurde [SPACE] gedrückt !', nil);
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

      SDL_SetRenderDrawColor(renderer, 20, 120, 20, 255);
      SDL_RenderClear(renderer);

      SDL_RenderPresent(renderer);
    end;


    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(win);
    SDL_Quit;
  end;

begin
  main;
end.

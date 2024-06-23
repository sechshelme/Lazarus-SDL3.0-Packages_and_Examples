program Project1;

// https://wiki.libsdl.org/SDL3/SDL_ShowOpenFileDialog
// https://wiki.libsdl.org/SDL3/SDL_DialogFileCallback
// https://wiki.libsdl.org/SDL3/SDL_ShowMessageBox

uses
  SDL3;

var
  win: PSDL_Window;

  procedure loadFile(userdata: pointer; filelist: PPchar; filter: longint); cdecl;
  begin
    if filelist <> nil then begin
      while filelist^ <> nil do begin
        SDL_Log('Datei: %s', filelist^);
        Inc(filelist);
      end;
    end;
  end;

  procedure FileOpen;
  var
    filters: array of TSDL_DialogFileFilter = (
      (Name: 'Alle Dateien'; pattern: '*'),
      (Name: 'JPG Bilder'; pattern: 'jpg;jpeg'),
      (Name: 'PNG Bilder'; pattern: 'png'));
  begin
    SDL_ShowOpenFileDialog(@loadFile, nil, win, PSDL_DialogFileFilter(filters), Length(filters), 'test.txt', SDL_TRUE);
  end;

  procedure ShowMessageBox;
  const
    Buttons: array [0..3] of TSDL_MessageBoxButtonData = (
      (flags: 0; buttonID: 0; Text: 'Nein'),
      (flags: SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT; buttonID: 1; Text: 'Ja'),
      (flags: SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT; buttonID: 2; Text: 'Abbruch'),
      (flags: 0; buttonID: 3; Text: 'Hilfe'));

    colorSchema: TSDL_MessageBoxColorScheme = (colors: (
      (r: 255; g: 0; b: 0),
      (r: 0; g: 255; b: 0),
      (r: 255; g: 255; b: 0),
      (r: 0; g: 0; b: 255),
      (r: 255; g: 0; b: 255)));

    messageboxdata: TSDL_MessageBoxData = (
      flags: SDL_MESSAGEBOX_INFORMATION;
      window: nil;
      title: 'Beispiel MessageBox';
      message: 'Drücke einen Knopf';
      numbuttons: length(Buttons);
      Buttons: @Buttons;
      colorScheme: @colorSchema);
  var
    buttonid: longint;
  begin
    SDL_ShowMessageBox(@messageboxdata, @buttonid);
    SDL_Log('Es wurde Button %i gedrückt !', buttonid);
  end;


  procedure main;
  var
    renderer: PSDL_Renderer;
    quit: TSDL_bool = SDL_FALSE;
    e: TSDL_Event;
  begin
    SDL_init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('SDL3 Dialog Example', 640, 480, SDL_WINDOW_RESIZABLE);
    if win = nil then begin
      WriteLn('Could not create window: ', SDL_GetError);
      Halt(1);
    end;
    renderer := SDL_CreateRenderer(win, nil);
    if renderer = nil then begin
      WriteLn('Could not create renderer: ', SDL_GetError);
      Halt(1);
    end;

    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e._type of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.key of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_o: begin
                FileOpen;
              end;
              SDLK_s: begin
                SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_INFORMATION, 'Info', 'Es wurde [SPACE] gedrückt !', nil);
              end;
              SDLK_m: begin
                ShowMessageBox;
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

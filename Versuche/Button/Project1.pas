program Project1;

uses
  heaptrc,
  ctypes,
  SDL3,
  Button,
  AudioBar;

  // https://gist.github.com/chrplr/cd76ec6d3c0140a1786a5b083620ea3d
  // https://stackoverflow.com/questions/62105714/sdl-loadwav-not-loading-file

type
  TMyApp = class(TObject)
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    window: PSDL_Window;
    renderer: PSDL_Renderer;

    SoundBar: array[0..5] of TSoundBar;
    WindowSize: TSDL_Point;
  end;

  { TMyApp }

  constructor TMyApp.Create;
  var
    i: integer;
    s: string;
  begin
    WindowSize.x := 320;
    WindowSize.y := 200;

    if SDL_init(SDL_INIT_VIDEO or SDL_INIT_AUDIO or SDL_INIT_EVENTS) <> 0 then  begin
      SDL_Log('Kann kein SDL-Fenster erzeugen !   %s', SDL_GetError);
    end;

    window := SDL_CreateWindow('SDL3 Window', WindowSize.x, WindowSize.y, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Kann kein Window erzeugen !   %s', SDL_GetError);
    end;

    renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_PRESENTVSYNC);
    if renderer = nil then begin
      SDL_Log('Kann kein SDL-Renderer erzeugen !   %s', SDL_GetError);
    end;

    for i := 0 to Length(SoundBar) - 1 do begin
      WriteStr(s, 'Boing_', i + 1, '.wav');
      SoundBar[i] := TSoundBar.Create(renderer, s, 0, i);
    end;
  end;

  destructor TMyApp.Destroy;
  var
    i: integer;
  begin
    for i := 0 to Length(SoundBar) - 1 do begin
      SoundBar[i].Free;
    end;

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    SDL_Quit;
    SDL_Log('Application quit successfully!');

    inherited Destroy;
  end;

  procedure TMyApp.Run;
  var
    event: TSDL_Event;
    quit: boolean = False;
    time: extended;
    red, green, blue: single;
    i: integer;

  begin
    for i := 0 to Length(SoundBar) - 1 do begin
      SoundBar[i].Resize(WindowSize);
    end;

    while not quit do begin
      while SDL_PollEvent(@event) do begin
        case event.type_ of
          SDL_EVENT_WINDOW_RESIZED: begin
            WindowSize.x := event.window.data1;
            WindowSize.y := event.window.data2;
            for i := 0 to Length(SoundBar) - 1 do begin
              SoundBar[i].Resize(WindowSize);
            end;
          end;
          SDL_EVENT_KEY_DOWN: begin
            case event.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_m: begin
                SDL_ShowSimpleMessageBox(0, 'box', 'box', window);
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;

        for i := 0 to Length(SoundBar) - 1 do begin
          SoundBar[i].EventHandle(event);
        end;
      end;

      //      SDL_Log('size: %i', SDL_GetAudioStreamQueued(sound.stream));

      time := SDL_GetTicks / 1000;
      red := (SDL_sinf(time) + 1) / 2.0;
      green := (SDL_sinf(time / 2) + 1) / 2.0;
      blue := (SDL_sinf(time / 3) + 1) / 2.0;
      SDL_SetRenderDrawColorFloat(renderer, red, green, blue, SDL_ALPHA_OPAQUE);

      SDL_RenderClear(renderer);
      for i := 0 to Length(SoundBar) - 1 do begin
        SoundBar[i].Paint;
      end;
      SDL_RenderPresent(renderer);
    end;
  end;

var
  MyApp: TMyApp;

begin
  MyApp := TMyApp.Create;
  MyApp.Run;
  MyApp.Free;
end.

program Project1;

uses
  sdl3,
  SDL3_mixer;

var
  music: PMix_Music;
  window: PSDL_Window;
  renderer: PSDL_Renderer;

  procedure EventHandle;
  var
    quit: TSDL_bool = SDL_FALSE;
    event: TSDL_Event;
  begin
    while not quit do begin
      while SDL_PollEvent(@event) do begin
        case event._type of
          SDL_EVENT_KEY_DOWN: begin
            SDL_Log('key: %i', event.key.key); // neu

            case event.key.key of
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

      SDL_SetRenderDrawColorFloat(renderer, 1.0, 1.0, 1.0, SDL_ALPHA_OPAQUE_FLOAT);
      SDL_RenderDebugText(renderer, 110, 90, 'press <ESC>');
      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO);
  if Mix_Init(MIX_INIT_WAVPACK) <> 0 then begin
    SDL_Log('Fehler: MixInit: %s', SDL_GetError());
    halt;
  end;

  window := SDL_CreateWindow('SDL3 Window', 320, 200, 0);
  if window = nil then begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !   %s', SDL_GetError);
  end;

  renderer:=SDL_CreateRenderer(window,nil);
  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Rendere erzeugen !   %s', SDL_GetError);
  end;

  if Mix_OpenAudio(0, nil) < 0 then  begin
    SDL_Log('Fehler: Kann Audio nicht öffnen: %s', SDL_GetError());
  end;

  music := Mix_LoadMUS('/home/tux/Schreibtisch/sound/test.mp3');
  if music = nil then begin
    WriteLn('WAV nicht gefunden !  ', Mix_GetError);
  end;

  Mix_PlayMusic(music, -10);

  EventHandle;

  SDL_DestroyWindow(window);

  Mix_FreeMusic(music);
  Mix_CloseAudio;
  SDL_Quit;
end.

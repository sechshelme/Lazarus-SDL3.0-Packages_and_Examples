program Project1;

uses
  ctypes,
  SDL3;

type
  TWave = record
    spec: TSDL_AudioSpec;
    sound: PUint8;
    soundlen: DWord;
  end;

  TSound = record
    wave: TWave;
    stream: PSDL_AudioStream;
    stream_ID: TSDL_AudioDeviceID;
  end;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  sound: TSound;

  WindowSize: TSDL_Point = (x: 320; y: 200);

  function Rect(x, y, w, h: single): TSDL_FRect; inline;
  begin
    Result.x := x;
    Result.y := y;
    Result.w := w;
    Result.h := h;
  end;

  procedure AudioStreamCallback(userdata: pointer; stream: PSDL_AudioStream; additional_amount: longint; total_amount: longint); cdecl;
  begin
    if additional_amount = total_amount then begin
      SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
    end;
    WriteLn('Callback');
    WriteLn('additional_amount: ', additional_amount);
    WriteLn('total_amount: ', total_amount);
  end;

  function LoadWave: TSound;
  const
    SoundFile = '../Boing_1.wav';
  var
    i: integer;
  begin
    if not SDL_LoadWAV(SoundFile, @Result.wave.spec, @Result.wave.sound, @Result.wave.soundlen) then begin
      SDL_LogError(0, 'Konnte WAV nicht öffnen !   %s', SDL_GetError);
      Exit;
    end;

    for i := 0 to SDL_GetNumAudioDrivers - 1 do begin
      SDL_Log('%i: %s', i, SDL_GetAudioDriver(i));
    end;

    SDL_Log('Using audio driver: %s', SDL_GetCurrentAudioDriver());

    Result.stream := SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, @Result.wave.spec, @AudioStreamCallback, nil);
    if Result.stream = nil then  begin
      SDL_LogError(0, 'Konnte Stream nicht öffnen !   %s', SDL_GetError);
    end;

    Result.stream_ID := SDL_GetAudioStreamDevice(Result.stream);

    WriteLn('Time: ', (Result.wave.soundlen div Result.wave.spec.freq div Result.wave.spec.channels div SDL_AUDIO_BYTESIZE(Result.wave.spec.format)) / 60: 4: 2);
    WriteLn('len: ', Result.wave.soundlen);
    WriteLn('spec.freq: ', Result.wave.spec.freq);
    WriteLn('spec.channels: ', Result.wave.spec.channels);
    WriteLn('spec.format: ', Result.wave.spec.format);
  end;


  procedure Run;
  var
    event: TSDL_Event;
    quit: boolean = False;
    RectStart, RectStack, RectPause, RectStop: TSDL_FRect;
    mp: TSDL_FPoint;

    procedure SetRect(x, y: Tint32);
    var
      w: single;
    begin
      w := x / 4;
      RectStart := Rect(0 * w + w / 10, y / 10, w * 8 / 10, y * 8 / 10);
      RectStack := Rect(1 * w + w / 10, y / 10, w * 8 / 10, y * 8 / 10);
      RectPause := Rect(2 * w + w / 10, y / 10, w * 8 / 10, y * 8 / 10);
      RectStop := Rect(3 * w + w / 10, y / 10, w * 8 / 10, y * 8 / 10);
    end;

    procedure Start;
    begin
      SDL_ClearAudioStream(sound.stream);
      SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
      SDL_ResumeAudioDevice(sound.stream_ID);
    end;

    procedure Stack;
    begin
      SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
      SDL_ResumeAudioDevice(sound.stream_ID);
    end;

    procedure Pause;
    begin
      if SDL_AudioDevicePaused(sound.stream_ID) then begin
        SDL_ResumeAudioDevice(sound.stream_ID);
      end else begin
        SDL_PauseAudioDevice(sound.stream_ID);
      end;
    end;

    procedure Stop;
    begin
      SDL_PauseAudioDevice(sound.stream_ID);
      SDL_ClearAudioStream(sound.stream);
    end;

  begin
    SetRect(WindowSize.x, WindowSize.y);

    while not quit do begin

      while SDL_PollEvent(@event) do begin
        case event._type of
          SDL_EVENT_WINDOW_RESIZED: begin
            SetRect(event.window.data1, event.window.data2);
          end;
          SDL_EVENT_KEY_DOWN: begin
            case event.key.key of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_h: begin
                Stop;
              end;
              SDLK_s: begin
                Start;
              end;
              SDLK_p: begin
                Pause;
              end;
              SDLK_m: begin
                SDL_ShowSimpleMessageBox(0,'box','box',window);
              end;
            end;
          end;
          SDL_EVENT_MOUSE_BUTTON_DOWN: begin
            mp.x := event.button.x;
            mp.y := event.button.y;
            if SDL_PointInRectFloat(@mp, @RectStart) then  begin
              Start;
            end;
            if SDL_PointInRectFloat(@mp, @RectStack) then  begin
              Stack;
            end;
            if SDL_PointInRectFloat(@mp, @RectPause) then  begin
              Pause;
            end;
            if SDL_PointInRectFloat(@mp, @RectStop) then  begin
              Stop;
            end;
          end;
          SDL_EVENT_MOUSE_BUTTON_UP: begin
            SDL_Log('Mouse up');
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

//      SDL_Log('size: %i', SDL_GetAudioStreamQueued(sound.stream));

      SDL_SetRenderDrawColorFloat(renderer, Random, Random, Random, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);

      SDL_SetRenderDrawColorFloat(renderer, 0.0, 1.0, 0.0, SDL_ALPHA_OPAQUE);
      SDL_RenderFillRect(renderer, @RectStart);

      SDL_SetRenderDrawColorFloat(renderer, 0.0, 0.0, 1.0, SDL_ALPHA_OPAQUE);
      SDL_RenderFillRect(renderer, @RectStack);

      SDL_SetRenderDrawColorFloat(renderer, 1.0, 1.0, 0.0, SDL_ALPHA_OPAQUE);
      SDL_RenderFillRect(renderer, @RectPause);

      SDL_SetRenderDrawColorFloat(renderer, 1.0, 0.0, 0.0, SDL_ALPHA_OPAQUE);
      SDL_RenderFillRect(renderer, @RectStop);

      SDL_RenderPresent(renderer);
    end;
  end;

  procedure main;
  begin
    if not SDL_init(SDL_INIT_VIDEO or SDL_INIT_AUDIO or SDL_INIT_EVENTS)  then  begin
      SDL_Log('Kann kein SDL-Fenster erzeugen !   %s', SDL_GetError);
    end;

    window := SDL_CreateWindow('SDL3 Window', WindowSize.x, WindowSize.y, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Kann kein Window erzeugen !   %s', SDL_GetError);
    end;

    renderer := SDL_CreateRenderer(window, nil);
    if renderer = nil then begin
      SDL_Log('Kann kein SDL-Renderer erzeugen !   %s', SDL_GetError);
    end;

    sound := LoadWave;
    Run;

    SDL_DestroyAudioStream(sound.stream);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    SDL_Quit;
    SDL_Log('Application quit successfully!');
  end;

begin
  main;
end.

program Project1;

uses
  ctypes,
  SDL3;

  // https://gist.github.com/chrplr/cd76ec6d3c0140a1786a5b083620ea3d

  // /home/tux/Schreibtisch/von_Git/SDL/SDL3/SDL/test/loopwave.c
  // https://stackoverflow.com/questions/62105714/sdl-loadwav-not-loading-file

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

  function LoadWave: TSound;
  var
    //    wave: TWave;
    i, minimum: integer;
    device: TSDL_AudioDeviceID;
  begin
    //    if SDL_LoadWAV('/home/tux/Schreibtisch/sound/test2.wav', @wave.spec, @wave.sound, @wave.soundlen) <> 0 then begin
    //    if SDL_LoadWAV('tataa.wav', @Result.wave.spec, @Result.wave.sound, @Result.wave.soundlen) <> 0 then begin
    if SDL_LoadWAV('/home/tux/Schreibtisch/sound/test.wav', @Result.wave.spec, @Result.wave.sound, @Result.wave.soundlen) <> 0 then begin
      SDL_LogError(0, 'Konnte WAV nicht öffnen !   %s', SDL_GetError);
      Exit;
    end;

    SDL_Log('Available audio drivers:');

    for i := 0 to SDL_GetNumAudioDrivers - 1 do begin
      SDL_Log('%i: %s', i, SDL_GetAudioDriver(i));
    end;

    SDL_Log('Using audio driver: %s', SDL_GetCurrentAudioDriver());

    Result.stream := SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_OUTPUT, @Result.wave.spec, nil, nil);
    if Result.stream = nil then  begin
      SDL_LogError(0, 'Konnte Stream nicht öffnen !   %s', SDL_GetError);
    end;

    Result.stream_ID := SDL_GetAudioStreamDevice(Result.stream);

    //    SDL_PutAudioStreamData(Result.stream, Result.wave.sound, Result.wave.soundlen);
    //device := SDL_GetAudioStreamDevice(Result);
    //SDL_ResumeAudioDevice(device);
    //
    //SDL_Delay(200);
    //device := SDL_GetAudioStreamDevice(Result);
    //SDL_PauseAudioDevice(device);
    //
    //SDL_Delay(200);
    //device := SDL_GetAudioStreamDevice(Result);
    //SDL_ResumeAudioDevice(device);

    //    WriteLn('time: ', wave.soundlen div (wave.spec.freq * wave.spec.channels * SDL_AUDIO_BITSIZE(wave.spec.format)));
    //    WriteLn('time: ', wave.soundlen div (wave.spec.freq * wave.spec.channels * SDL_AUDIO_BITSIZE(wave.spec.format)));

    //    WriteLn('device: ', device);
    WriteLn('len: ', Result.wave.soundlen);
    WriteLn('spec.freq: ', Result.wave.spec.freq);
    WriteLn('spec.channels: ', Result.wave.spec.channels);
    WriteLn('spec.format: ', Result.wave.spec.format);
  end;


  procedure Run;
  var
    event: TSDL_Event;
    quit: boolean = False;
  begin

    while not quit do begin

      while SDL_PollEvent(@event) do begin
        case event.type_ of
          SDL_EVENT_KEY_DOWN: begin
            case event.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_s: begin
                SDL_ClearAudioStream(sound.stream);
                SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
                SDL_ResumeAudioDevice(sound.stream_ID);
              end;
              SDLK_p: begin
                if SDL_AudioDevicePaused(sound.stream_ID) then begin
                  SDL_ResumeAudioDevice(sound.stream_ID);
                end else begin
                  SDL_PauseAudioDevice(sound.stream_ID);
                end;
              end;
            end;
          end;
          SDL_EVENT_MOUSE_BUTTON_DOWN: begin
            SDL_Log('Mouse down');
          end;
          SDL_EVENT_MOUSE_BUTTON_UP: begin
            SDL_Log('Mouse up');
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

      SDL_SetRenderDrawColorFloat(renderer, Random, Random, Random, SDL_ALPHA_OPAQUE);

      SDL_RenderClear(renderer);
      SDL_RenderPresent(renderer);
    end;
  end;

  procedure main;
  begin
    if SDL_init(SDL_INIT_VIDEO or SDL_INIT_AUDIO or SDL_INIT_EVENTS) <> 0 then  begin
      SDL_Log('Kann kein SDL-Fenster erzeugen !   %s', SDL_GetError);
    end;

    window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Kann kein Window erzeugen !   %s', SDL_GetError);
    end;

    renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_PRESENTVSYNC);
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

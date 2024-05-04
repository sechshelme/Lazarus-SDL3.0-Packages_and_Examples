program Project1;
{$mode ObjFPC}{$H+}


uses
  heaptrc,
  ctypes,
  SDL3,
  Button;

  // https://gist.github.com/chrplr/cd76ec6d3c0140a1786a5b083620ea3d

  // /home/tux/Schreibtisch/von_Git/SDL/SDL3/SDL/test/loopwave.c
  // https://stackoverflow.com/questions/62105714/sdl-loadwav-not-loading-file

type

  { TMyApp }

  TMyApp = class(TObject)
    procedure main;
  private
    procedure PauseClick(Sender: TObject);
    procedure SartClick(Sender: TObject);
    procedure StackClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
  end;

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


var
  ButtonStart, ButtonStack, ButtonPause, ButtonStop: TButton;


  function Rect(x, y, w, h: single): TSDL_FRect; inline;
  begin
    Result.x := x;
    Result.y := y;
    Result.w := w;
    Result.h := h;
  end;

  procedure Start(Sender: TObject);
  begin
    SDL_ClearAudioStream(sound.stream);
    SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
    SDL_ResumeAudioDevice(sound.stream_ID);
  end;

  procedure Stack(Sender: TObject);
  begin
    SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
    SDL_ResumeAudioDevice(sound.stream_ID);
  end;

  procedure Pause(Sender: TObject);
  begin
    if SDL_AudioDevicePaused(sound.stream_ID) then begin
      SDL_ResumeAudioDevice(sound.stream_ID);
    end else begin
      SDL_PauseAudioDevice(sound.stream_ID);
    end;
  end;

  procedure Stop(Sender: TObject);
  begin
    SDL_PauseAudioDevice(sound.stream_ID);
    SDL_ClearAudioStream(sound.stream);
  end;



  procedure AdioStreamCallback(userdata: pointer; stream: PSDL_AudioStream; additional_amount: longint; total_amount: longint); cdecl;
  begin
    if additional_amount = total_amount then begin
      SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);

      //      IsEnd := SDL_TRUE;
    end;
    //    exit;
    WriteLn('Callback');
    WriteLn('additional_amount: ', additional_amount);
    WriteLn('total_amount: ', total_amount);
  end;

  function LoadWave: TSound;
  const
    SoundFile = 'Boing_1.wav';
    //        SoundFile='/home/tux/Schreibtisch/sound/test.wav';
    //    SoundFile = 'tataa.wav';
    //    SoundFile='/home/tux/Schreibtisch/sound/test2.wav';
  var
    i: integer;
  begin
    if SDL_LoadWAV(SoundFile, @Result.wave.spec, @Result.wave.sound, @Result.wave.soundlen) <> 0 then begin
      SDL_LogError(0, 'Konnte WAV nicht öffnen !   %s', SDL_GetError);
      Exit;
    end;

    for i := 0 to SDL_GetNumAudioDrivers - 1 do begin
      SDL_Log('%i: %s', i, SDL_GetAudioDriver(i));
    end;

    SDL_Log('Using audio driver: %s', SDL_GetCurrentAudioDriver());

    Result.stream := SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_OUTPUT, @Result.wave.spec, @AdioStreamCallback, nil);
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

  begin
    SetRect(WindowSize.x, WindowSize.y);

    while not quit do begin

      while SDL_PollEvent(@event) do begin
        case event.type_ of
          SDL_EVENT_WINDOW_RESIZED: begin
            SetRect(event.window.data1, event.window.data2);
          end;
          SDL_EVENT_KEY_DOWN: begin
            case event.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_h: begin
                Stop(nil);
              end;
              SDLK_s: begin
                Start(nil);
              end;
              SDLK_p: begin
                Pause(nil);
              end;
              SDLK_m: begin
                SDL_ShowSimpleMessageBox(0, 'box', 'box', window);
              end;
            end;
          end;
          SDL_EVENT_MOUSE_BUTTON_DOWN: begin
            //mp.x := event.button.x;
            //mp.y := event.button.y;
            //if SDL_PointInRectFloat(@mp, @RectStart) then  begin
            //  Start(nil);
            //end;
            //if SDL_PointInRectFloat(@mp, @RectStack) then  begin
            //  Stack(nil);
            //end;
            //if SDL_PointInRectFloat(@mp, @RectPause) then  begin
            //  Pause(nil);
            //end;
            //if SDL_PointInRectFloat(@mp, @RectStop) then  begin
            //  Stop(nil);
            //end;
          end;
          SDL_EVENT_MOUSE_BUTTON_UP: begin
            SDL_Log('Mouse up');
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;

        ButtonStart.EventHandle(event);
        ButtonStack.EventHandle(event);
        ButtonPause.EventHandle(event);
        ButtonStop.EventHandle(event);
      end;

      SDL_Log('size: %i', SDL_GetAudioStreamQueued(sound.stream));

      SDL_SetRenderDrawColorFloat(renderer, Random, Random, Random, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);

      //SDL_SetRenderDrawColorFloat(renderer, 0.0, 1.0, 0.0, SDL_ALPHA_OPAQUE);
      //SDL_RenderFillRect(renderer, @RectStart);
      //
      //SDL_SetRenderDrawColorFloat(renderer, 0.0, 0.0, 1.0, SDL_ALPHA_OPAQUE);
      //SDL_RenderFillRect(renderer, @RectStack);
      //
      //SDL_SetRenderDrawColorFloat(renderer, 1.0, 1.0, 0.0, SDL_ALPHA_OPAQUE);
      //SDL_RenderFillRect(renderer, @RectPause);
      //
      //SDL_SetRenderDrawColorFloat(renderer, 1.0, 0.0, 0.0, SDL_ALPHA_OPAQUE);
      //SDL_RenderFillRect(renderer, @RectStop);

      ButtonStart.Paint;
      ButtonStack.Paint;
      ButtonPause.Paint;
      ButtonStop.Paint;

      SDL_RenderPresent(renderer);
    end;
  end;

  { TMyApp }

  procedure TMyApp.main;
  begin
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

    ButtonStart := TButton.Create(renderer);
    ButtonStart.Left := 10;
    ButtonStart.Top := 10;
    ButtonStart.OnClick := @SartClick;

    ButtonStack := TButton.Create(renderer);
    ButtonStack.Left := 90;
    ButtonStack.Top := 10;
    //    ButtonStart.OnClick:=@StackClick;

    ButtonPause := TButton.Create(renderer);
    ButtonPause.Left := 170;
    ButtonPause.Top := 10;
    //    ButtonStart.OnClick:=@PauseClick;

    ButtonStop := TButton.Create(renderer);
    ButtonStop.Left := 250;
    ButtonStop.Top := 10;
    //    ButtonStart.OnClick:=@StopClick;




    sound := LoadWave;
    Run;

    ButtonStart.Free;
    ButtonStack.Free;
    ButtonPause.Free;
    ButtonStop.Free;

    SDL_DestroyAudioStream(sound.stream);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    SDL_Quit;
    SDL_Log('Application quit successfully!');

  end;

  procedure TMyApp.PauseClick(Sender: TObject);
  begin
    if SDL_AudioDevicePaused(sound.stream_ID) then begin
      SDL_ResumeAudioDevice(sound.stream_ID);
    end else begin
      SDL_PauseAudioDevice(sound.stream_ID);
    end;
  end;

  procedure TMyApp.SartClick(Sender: TObject);
  begin
    SDL_ClearAudioStream(sound.stream);
    SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
    SDL_ResumeAudioDevice(sound.stream_ID);
  end;

  procedure TMyApp.StackClick(Sender: TObject);
  begin
    SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
    SDL_ResumeAudioDevice(sound.stream_ID);
  end;

  procedure TMyApp.StopClick(Sender: TObject);
  begin
    SDL_PauseAudioDevice(sound.stream_ID);
    SDL_ClearAudioStream(sound.stream);
  end;

var
  MyApp: TMyApp;

begin
  MyApp := TMyApp.Create;

  MyApp.main;
  MyApp.Free;
end.

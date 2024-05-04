program Project1;

uses
  heaptrc,
  ctypes,
  SDL3,
  Button;

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

type
  TMyApp = class(TObject)
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  private
    ButtonStart, ButtonStack, ButtonPause, ButtonStop: TButton;
    WindowSize: TSDL_Point;
    function LoadWave: TSound;

    procedure PauseClick(Sender: TObject);
    procedure SartClick(Sender: TObject);
    procedure StackClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
  end;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  sound: TSound;


procedure AudioStreamCallback(userdata: pointer; stream: PSDL_AudioStream; additional_amount: longint; total_amount: longint); cdecl;
begin
  if additional_amount = total_amount then begin
    SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
  end;
  //    WriteLn('Callback');
  //    WriteLn('additional_amount: ', additional_amount);
  //    WriteLn('total_amount: ', total_amount);
end;

  procedure TMyApp.SartClick(Sender: TObject);
  begin
    WriteLn('Start');
    SDL_ClearAudioStream(sound.stream);
    SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
    SDL_ResumeAudioDevice(sound.stream_ID);
  end;

  procedure TMyApp.StackClick(Sender: TObject);
  begin
    WriteLn('Stack');
    SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
    SDL_ResumeAudioDevice(sound.stream_ID);
  end;

  procedure TMyApp.PauseClick(Sender: TObject);
  begin
    WriteLn('Pause');
    if SDL_AudioDevicePaused(sound.stream_ID) then begin
      SDL_ResumeAudioDevice(sound.stream_ID);
    end else begin
      SDL_PauseAudioDevice(sound.stream_ID);
    end;
  end;

  procedure TMyApp.StopClick(Sender: TObject);
  begin
    WriteLn('Stop');
    SDL_PauseAudioDevice(sound.stream_ID);
    SDL_ClearAudioStream(sound.stream);
  end;



  function TMyApp.LoadWave: TSound;
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

    Result.stream := SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_OUTPUT, @Result.wave.spec, @AudioStreamCallback, nil);
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


  { TMyApp }

  constructor TMyApp.Create;
  begin
    WindowSize.x:= 320;
    WindowSize.y:= 200;

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
    ButtonStart.Caption := 'Start';
    ButtonStart.OnClick := @SartClick;
    ButtonStart.Color := [0.0, 1.0, 0.0, SDL_ALPHA_OPAQUE];

    ButtonStack := TButton.Create(renderer);
    ButtonStart.Caption := 'Stack';
    ButtonStack.OnClick := @StackClick;
    ButtonStack.Color := [0.0, 0.0, 1.0, SDL_ALPHA_OPAQUE];

    ButtonPause := TButton.Create(renderer);
    ButtonPause.Caption := 'Pause';
    ButtonPause.OnClick := @PauseClick;
    ButtonPause.Color := [1.0, 1.0, 0.0, SDL_ALPHA_OPAQUE];

    ButtonStop := TButton.Create(renderer);
    ButtonStop.Caption := 'Stop';
    ButtonStop.OnClick := @StopClick;
    ButtonStop.Color := [1.0, 0.0, 0.0, SDL_ALPHA_OPAQUE];

    sound := LoadWave;
  end;

  destructor TMyApp.Destroy;
begin
  ButtonStart.Free;
  ButtonStack.Free;
  ButtonPause.Free;
  ButtonStop.Free;

  SDL_DestroyAudioStream(sound.stream);
  SDL_free(sound.wave.sound);
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

    procedure SetRect(x, y: Tint32);
    var
      w: single;
    begin
      w := x / 4;
      ButtonStart.Rect := Rect(0 * w + w / 10, y / 10, w * 8 / 10, y * 8 / 10);
      ButtonStack.Rect := Rect(1 * w + w / 10, y / 10, w * 8 / 10, y * 8 / 10);
      ButtonPause.Rect := Rect(2 * w + w / 10, y / 10, w * 8 / 10, y * 8 / 10);
      ButtonStop.Rect := Rect(3 * w + w / 10, y / 10, w * 8 / 10, y * 8 / 10);
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
                //                Stop(nil);
              end;
              SDLK_s: begin
                //                Start(nil);
              end;
              SDLK_p: begin
                //                Pause(nil);
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

        ButtonStart.EventHandle(event);
        ButtonStack.EventHandle(event);
        ButtonPause.EventHandle(event);
        ButtonStop.EventHandle(event);
      end;

      //      SDL_Log('size: %i', SDL_GetAudioStreamQueued(sound.stream));

      SDL_SetRenderDrawColorFloat(renderer, Random, Random, Random, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);

      ButtonStart.Paint;
      ButtonStack.Paint;
      ButtonPause.Paint;
      ButtonStop.Paint;

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

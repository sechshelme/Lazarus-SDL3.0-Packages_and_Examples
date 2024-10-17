unit AudioBar;

interface

uses
  SDL3,
  Button;

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
  PSound = ^TSound;

  { TSoundBar }

  TSoundBar = class(TObject)
  public
    constructor Create(renderer: PSDL_Renderer; const Titel: string; x, y: integer);
    destructor Destroy; override;
    procedure EventHandle(var event: TSDL_Event);
    procedure Paint;
    procedure Resize(size: TSDL_Point);
  private
    BarPos, WindowSize: TSDL_Point;
    ButtonStart, ButtonStack, ButtonPause, ButtonStop: TButton;
    FTitle: string;
    sound: TSound;
    function LoadWave: TSound;

    procedure StartClick(Sender: TObject);
    procedure StackClick(Sender: TObject);
    procedure PauseClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
  end;



implementation

procedure AudioStreamCallback(userdata: pointer; {%H-}stream: PSDL_AudioStream; additional_amount: longint; total_amount: longint); cdecl;
var
  sound: TSound;
begin
  sound := PSound(userdata)^;

  if additional_amount = total_amount then begin
    SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
  end;
end;


{ TSoundBar }

constructor TSoundBar.Create(renderer: PSDL_Renderer; const Titel: string; x, y: integer);
begin
  FTitle := Titel;
  BarPos.x := x;
  BarPos.y := y;

  ButtonStart := TButton.Create(renderer);
  ButtonStart.Caption := 'Start';
  ButtonStart.OnClick := @StartClick;
  ButtonStart.Color := $00FF00FF;

  ButtonStack := TButton.Create(renderer);
  ButtonStart.Caption := 'Stack';
  ButtonStack.OnClick := @StackClick;
  ButtonStack.Color := $0000FFFF;

  ButtonPause := TButton.Create(renderer);
  ButtonPause.Caption := 'Pause';
  ButtonPause.OnClick := @PauseClick;
  ButtonPause.Color := $FFFF00FF;

  ButtonStop := TButton.Create(renderer);
  ButtonStop.Caption := 'Stop';
  ButtonStop.OnClick := @StopClick;
  ButtonStop.Color := $FF0000FF;

  sound := LoadWave;
end;

destructor TSoundBar.Destroy;
begin
  ButtonStart.Free;
  ButtonStack.Free;
  ButtonPause.Free;
  ButtonStop.Free;

  SDL_DestroyAudioStream(sound.stream);
  SDL_free(sound.wave.sound);

  inherited Destroy;
end;

procedure TSoundBar.EventHandle(var event: TSDL_Event);
begin
  ButtonStart.EventHandle(event);
  ButtonStack.EventHandle(event);
  ButtonPause.EventHandle(event);
  ButtonStop.EventHandle(event);
end;

procedure TSoundBar.Paint;
begin
  ButtonStart.Paint;
  ButtonStack.Paint;
  ButtonPause.Paint;
  ButtonStop.Paint;
end;

procedure TSoundBar.Resize(size: TSDL_Point);

  procedure SetRect(l, r: single);
  var
    x, y, w, h: single;
  begin
    x := BarPos.x;
    y :=  BarPos.y * WindowSize.y / 6 ;
    w := l / 4;
    h := r / 6;
    ButtonStart.CanvasRect := RectF(x + 0 * w + w / 10, y + 0 * h + h / 10, w * 8 / 10, h * 8 / 10);
    ButtonStack.CanvasRect := RectF(x + 1 * w + w / 10, y + 0 * h + h / 10, w * 8 / 10, h * 8 / 10);
    ButtonPause.CanvasRect := RectF(x + 2 * w + w / 10, y + 0 * h + h / 10, w * 8 / 10, h * 8 / 10);
    ButtonStop.CanvasRect := RectF(x + 3 * w + w / 10, y + 0 * h + h / 10, w * 8 / 10, h * 8 / 10);
  end;

begin
  WindowSize := size;
  SetRect(WindowSize.x, WindowSize.y);
end;

function TSoundBar.LoadWave: TSound;
var
  i: integer;
begin
  if not SDL_LoadWAV(PChar(FTitle), @Result.wave.spec, @Result.wave.sound, @Result.wave.soundlen) then begin
    SDL_LogError(0, 'Konnte WAV nicht öffnen !   %s', SDL_GetError);
    Exit;
  end;

  for i := 0 to SDL_GetNumAudioDrivers - 1 do begin
    SDL_Log('%i: %s', i, SDL_GetAudioDriver(i));
  end;

  SDL_Log('Using audio driver: %s', SDL_GetCurrentAudioDriver());

  Result.stream := SDL_OpenAudioDeviceStream(SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK, @Result.wave.spec, @AudioStreamCallback, @sound);
  if Result.stream = nil then  begin
    SDL_LogError(0, 'Konnte Stream nicht öffnen !   %s', SDL_GetError);
  end;

  Result.stream_ID := SDL_GetAudioStreamDevice(Result.stream);

  SDL_Log('Time: %i', (Result.wave.soundlen div Result.wave.spec.freq div Result.wave.spec.channels div SDL_AUDIO_BYTESIZE(Result.wave.spec.format)) / 60);
  SDL_Log('len: %i', Result.wave.soundlen);
  SDL_Log('spec.freq: %i', Result.wave.spec.freq);
  SDL_Log('spec.channels: %i', Result.wave.spec.channels);
  SDL_Log('spec.format: %i', Result.wave.spec.format);
end;

procedure TSoundBar.StartClick(Sender: TObject);
begin
  SDL_ClearAudioStream(sound.stream);
  SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
  SDL_ResumeAudioDevice(sound.stream_ID);
end;

procedure TSoundBar.StackClick(Sender: TObject);
begin
  SDL_PutAudioStreamData(sound.stream, sound.wave.sound, sound.wave.soundlen);
  SDL_ResumeAudioDevice(sound.stream_ID);
end;

procedure TSoundBar.PauseClick(Sender: TObject);
begin
  if SDL_AudioDevicePaused(sound.stream_ID) then begin
    SDL_ResumeAudioDevice(sound.stream_ID);
  end else begin
    SDL_PauseAudioDevice(sound.stream_ID);
  end;
end;

procedure TSoundBar.StopClick(Sender: TObject);
begin
  SDL_PauseAudioDevice(sound.stream_ID);
  SDL_ClearAudioStream(sound.stream);
end;

end.

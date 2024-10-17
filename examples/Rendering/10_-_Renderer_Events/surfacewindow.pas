unit SurfaceWindow;

{$modeswitch ArrayOperators}

interface

uses
  SDL3;

type

  { TSurfaceWindow }

  TSurfaceWindow = class(TObject)
    constructor Create;
    destructor Destroy; override;
    procedure EventHandle(var ev: TSDL_Event);
    procedure LoopHandle;
    procedure paint;
  private
    win: PSDL_Window;
    winSurface: PSDL_Surface;
    keyStat: PBoolean;
    Surface: array of PSDL_Surface;
    procedure WinTransform(const ofs: TSDL_Rect);
    procedure ShowWinPos(const ev: TSDL_Event);

    procedure printSurface(sur: PSDL_Surface);
    function CreateBMPSurface(path: PChar): PSDL_Surface;
    function CreateSurfaceFromTriBuffer: PSDL_Surface;
    function CreateSurfaceFromQuadBuffer: PSDL_Surface;
    function CreateSurfaceFromClassicTriBuffer: PSDL_Surface;
  end;

implementation

constructor TSurfaceWindow.Create;
const
  pixels_1: array [0..3] of byte = ($FF, $FF, $00, $FF);
  pixels_2: array [0..0] of DWord = ($FFFF00FF);
begin
  inherited Create;
  win := SDL_CreateWindow('Surface Window', 320, 200, SDL_WINDOW_RESIZABLE);
  if win = nil then begin
    SDL_Log('Kann Surface Window nicht erzeugen !');
  end;
  SDL_SetWindowPosition(win, 50, 50);
  keyStat := SDL_GetKeyboardState(nil);

  // --- Surfaces inizialisieren
  // io.
  Surface += [SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA32)];
  SDL_memcpy(Surface[0]^.pixels, @pixels_1, sizeof(pixels_1));

  // warped
  Surface += [SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA32)];
  SDL_memcpy(Surface[1]^.pixels, @pixels_2, sizeof(pixels_2));

  // io.
  Surface += [SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA8888)];
  SDL_memcpy(Surface[2]^.pixels, @pixels_2, sizeof(pixels_2));

  Surface += [CreateBMPSurface('mauer.bmp')];
  Surface += [CreateBMPSurface('autos4bit.bmp')];
  Surface += [CreateBMPSurface('autos8bit.bmp')];

//  Surface += [SDL_ConvertSurfaceFormat(Surface[4], SDL_PIXELFORMAT_RGBA32)];

  Surface += [CreateSurfaceFromQuadBuffer];
  Surface += [CreateSurfaceFromTriBuffer];
  Surface += [CreateSurfaceFromClassicTriBuffer];
end;

destructor TSurfaceWindow.Destroy;
begin
  SDL_DestroyWindow(win);
  inherited Destroy;
end;

procedure TSurfaceWindow.paint;
var
  skyblue: uint32;
  w, h: longint;
  i: integer;
  r: TSDL_Rect;
begin
  if winSurface <> nil then begin
    SDL_DestroySurface(winSurface);
  end;
  winSurface := SDL_GetWindowSurface(win);

  skyblue := SDL_MapRGB(@winSurface^.format,nil, 65, 193, 193);
  SDL_FillSurfaceRect(winSurface, nil, skyblue);

  SDL_GetWindowSize(win, @w, @h);
  for i := 0 to Length(Surface) - 1 do begin
    r.items := [10 + (i mod 3) * (w div 5), 10 + (i div 3) * (h div 5), w div 6, h div 6];
    SDL_BlitSurfaceScaled(Surface[i], nil, winSurface, @r, SDL_SCALEMODE_NEAREST);
  end;

  SDL_UpdateWindowSurface(win);
end;

procedure TSurfaceWindow.EventHandle(var ev: TSDL_Event);
var
  i: integer;
begin
  case ev._type of
    SDL_EVENT_KEY_DOWN: begin
      case ev.key.key of
        SDLK_p: begin
          for i := 0 to Length(Surface) - 1 do begin
            printSurface(Surface[i]);
          end;
        end;
      end;
    end;
    SDL_EVENT_WINDOW_MOVED, SDL_EVENT_WINDOW_RESIZED: begin
      ShowWinPos(ev);
    end;
  end;
end;

procedure TSurfaceWindow.LoopHandle;
var
  IsShift, IsCtrl: boolean;
  step: integer;
  trans: TSDL_Rect = (x: 0; y: 0; w: 0; h: 0);
  IsMotif: boolean = False;
begin
  if (SDL_GetWindowFlags(win) and SDL_WINDOW_INPUT_FOCUS) = SDL_WINDOW_INPUT_FOCUS then begin
    IsShift := (keyStat[SDL_SCANCODE_LSHIFT] ) or (keyStat[SDL_SCANCODE_RSHIFT] );
    IsCtrl := (keyStat[SDL_SCANCODE_LCTRL] ) or (keyStat[SDL_SCANCODE_RCTRL] );

    //  if IsCtrl then WriteLn('Ctrl')else WriteLn('no Ctrl');

    if IsShift then begin
      step := 2;
    end else begin
      step := 1;
    end;

    if keyStat[SDL_SCANCODE_RIGHT]  then begin
      if IsCtrl then begin
        Inc(trans.w, step);
      end else begin
        Inc(trans.x, step);
      end;
      IsMotif := True;
    end;

    if keyStat[SDL_SCANCODE_LEFT]  then begin
      if IsCtrl then begin
        Dec(trans.w, step);
      end else begin
        Dec(trans.x, step);
      end;
      IsMotif := True;
    end;

    if keyStat[SDL_SCANCODE_DOWN]  then begin
      if IsCtrl then begin
        Inc(trans.h, step);
      end else begin
        Inc(trans.y, step);
      end;
      IsMotif := True;
    end;

    if keyStat[SDL_SCANCODE_UP]  then begin
      if IsCtrl then begin
        Dec(trans.h, step);
      end else begin
        Dec(trans.y, step);
      end;
      IsMotif := True;
    end;

    if IsMotif then begin
      WinTransform(trans);
    end;
  end;
end;

procedure TSurfaceWindow.WinTransform(const ofs: TSDL_Rect);
var
  x, y, w, h: longint;
begin
  SDL_GetWindowPosition(win, @x, @y);
  SDL_GetWindowSize(win, @w, @h);
  Inc(x, ofs.x);
  Inc(y, ofs.y);
  Inc(w, ofs.w);
  Inc(h, ofs.h);
  SDL_SetWindowPosition(win, x, y);
  if w < 10 then begin
    w := 10;
  end;
  if h < 10 then begin
    h := 10;
  end;
  SDL_SetWindowSize(win, w, h);
end;

procedure TSurfaceWindow.ShowWinPos(const ev: TSDL_Event);
var
  x, y, w, h: longint;
  w2: PSDL_Window;
begin
  w2 := SDL_GetWindowFromID(ev.window.windowID);
  if w2 = win then begin
    SDL_GetWindowPosition(win, @x, @y);
    SDL_GetWindowSize(win, @w, @h);
    SDL_Log('Left: %i  Right: %i,  Width: %i  Height: %i', x, y, w, h);
  end;
end;

procedure TSurfaceWindow.printSurface(sur: PSDL_Surface);
var
  ch: pbyte;
begin
  ch := sur^.pixels;
  SDL_Log('Pixel: %02X %02X %02X %02X ', ch[0], ch[1], ch[2], ch[3]);
  //SDL_Log('format: %u', sur^.format^.format);
  //SDL_Log('bit per pixel: %u    bytes per Pixel: %u', sur^.format^.bits_per_pixel, sur^.format^.bytes_per_pixel);
  //SDL_Log('Rmask:  %08X   Gmask:  %08X   Bmask:  %08X   Amask:  %08X   ', sur^.format^.Rmask, sur^.format^.Gmask, sur^.format^.Bmask, sur^.format^.Amask);
  //SDL_Log('Rshift: %u   Gshift: %u   Bshift: %u   Ashift: %u   ', sur^.format^.Rshift, sur^.format^.Gshift, sur^.format^.Bshift, sur^.format^.Ashift);
  //SDL_Log('Rloss: %u   Gloss: %u   Bloss: %u   Aloss: %u'#10#10, sur^.format^.Rloss, sur^.format^.Gloss, sur^.format^.Bloss, sur^.format^.Aloss);
end;

function TSurfaceWindow.CreateBMPSurface(path: PChar): PSDL_Surface;
begin
  Result := SDL_LoadBMP(path);
  if Result = nil then begin
    SDL_Log('Konnte BMP nicht laden!: %s   (%s)', path, SDL_GetError);
  end;
end;

// === TriBuffer

type
  TTriByte = bitpacked record
    rgb: 0..$FFFFFF;
  end;
  PTriByte = ^TTriByte;

operator := (const AValue: integer): TTriByte; inline;
begin
  Result.rgb := AValue;
end;

operator := (const AValue: TTriByte): integer; inline;
begin
  Result := AValue.rgb;
end;

function TSurfaceWindow.CreateSurfaceFromTriBuffer: PSDL_Surface;
const
  Data: array of TTriByte = nil;
begin
  Data := [
    $000000, $FF0000, $00FF00, $0000FF,
    $444444, $FF4444, $44FF44, $4444FF,
    $888888, $FF8888, $88FF88, $8888FF,
    $AAAAAA, $FFAAAA, $AAFFAA, $AAAAAA];

//  function SDL_CreateSurfaceFrom(Width: longint; Height: longint; format: TSDL_PixelFormat; pixels: pointer; pitch: longint): PSDL_Surface; cdecl; external libSDL3;

  Result := SDL_CreateSurfaceFrom(4,4,SDL_PIXELFORMAT_RGB24, PTriByte(Data), 12);
  if Result = nil then begin
    SDL_Log('Konnte Surface nicht laden!:  %s', SDL_GetError);
  end;
end;

// === QuadBuffer

function TSurfaceWindow.CreateSurfaceFromQuadBuffer: PSDL_Surface;
const
  Data: array of DWord = (
    $000000FF, $FF0000FF, $00FF00FF, $0000FFFF,
    $444444FF, $FF4444FF, $44FF44FF, $4444FFFF,
    $888888FF, $FF8888FF, $88FF88FF, $8888FFFF,
    $AAAAAAFF, $FFAAAAFF, $AAFFAAFF, $AAAAAAFF);
begin
//  Result := SDL_CreateSurfaceFrom(PDWord(Data), 4, 4, 16, SDL_PIXELFORMAT_RGBA8888);
  Result := SDL_CreateSurfaceFrom(4,4,SDL_PIXELFORMAT_RGBA8888, PDWord(Data), 16);
  if Result = nil then begin
    SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
  end;
end;

function TSurfaceWindow.CreateSurfaceFromClassicTriBuffer: PSDL_Surface;
const
  Data: array of TTriByte = (
    (rgb: $000000), (rgb: $FF0000), (rgb: $00FF00), (rgb: $0000FF),
    (rgb: $444444), (rgb: $FF4444), (rgb: $44FF44), (rgb: $4444FF),
    (rgb: $888888), (rgb: $FF8888), (rgb: $88FF88), (rgb: $8888FF),
    (rgb: $AAAAAA), (rgb: $FFAAAA), (rgb: $AAFFAA), (rgb: $AAAAAA));
begin
//  Result := SDL_CreateSurfaceFrom(PDWord(Data), 4, 4, 12, SDL_PIXELFORMAT_RGB24);
  Result := SDL_CreateSurfaceFrom(4,4,SDL_PIXELFORMAT_RGB24, PDWord(Data), 12);
  if Result = nil then begin
    SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
  end;
end;

end.

unit RenderWindow;

interface

uses
  SDL3;

type

  { TRenderWindow }

  TRenderWindow = class(TObject)
    constructor Create;
    destructor Destroy; override;
    procedure EventHandle(var ev: TSDL_Event);
    procedure LoopHandle;
    procedure paint;
  private
    win: PSDL_Window;
    FWidth, FHeight: longint;
    renderer: PSDL_Renderer;
    bitmapTex: PSDL_Texture;
    keyStat: PUInt8;
    rDest: TSDL_FRect;
    function CreateTexture: PSDL_Texture;
  end;

implementation

function TRenderWindow.CreateTexture: PSDL_Texture;
var
  bitmapSurface: PSDL_Surface;
begin
  bitmapSurface := SDL_LoadBMP('mauer.bmp');
  if bitmapSurface = nil then  begin
    SDL_Log('Kann keine Textur erzeugen !');
  end;

  Result := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
  if Result = nil then begin
    SDL_Log('Kann Textur nicht laden ezeugen !');
  end;
  SDL_DestroySurface(bitmapSurface);
end;

constructor TRenderWindow.Create;
begin
  inherited Create;
  FWidth := 640;
  FHeight := 480;
  win := SDL_CreateWindow('Surface Window', FWidth, FHeight, SDL_WINDOW_RESIZABLE);
  if win = nil then begin
    SDL_Log('Kann Surface Window nicht erzeugen !');
  end;
  SDL_SetWindowPosition(win, 500, 150);

  renderer := SDL_CreateRenderer(win, nil);
  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  keyStat := SDL_GetKeyboardState(nil);
  bitmapTex := CreateTexture;

  rDest.items := [0, 0, 100, 100];
end;

destructor TRenderWindow.Destroy;
begin
  SDL_DestroyTexture(bitmapTex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(win);
  inherited Destroy;
end;

procedure TRenderWindow.paint;
var
  time: extended;
  red, green, blue: single;
  rSrc, r: TSDL_FRect;
begin
  time := SDL_GetTicks / 1000;
  red := (SDL_sinf(time) + 1) / 2.0;
  green := (SDL_sinf(time / 2) + 1) / 2.0;
  blue := (SDL_sinf(time / 3) + 1) / 2.0;
  SDL_SetRenderDrawColorFloat(renderer, red, green, blue, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(renderer);

  rSrc.items := [0, 0, 400, 400];
  SDL_RenderTexture(renderer, bitmapTex, @rSrc, @rDest);

  r.items := [200, 200, 64, 64];
  SDL_RenderTexture(renderer, bitmapTex, @rSrc, @r);

  SDL_RenderPresent(renderer);
end;

procedure TRenderWindow.EventHandle(var ev: TSDL_Event);
begin
  case ev._type of
    SDL_EVENT_WINDOW_RESIZED: begin
      SDL_GetWindowSize(win, @FWidth, @FHeight);
    end;
  end;
end;

procedure TRenderWindow.LoopHandle;
var
  IsShift, IsCtrl: boolean;
  step: single;
begin
  if (SDL_GetWindowFlags(win) and SDL_WINDOW_INPUT_FOCUS) = SDL_WINDOW_INPUT_FOCUS then begin
    IsShift := (keyStat[SDL_SCANCODE_LSHIFT] <> 0) or (keyStat[SDL_SCANCODE_RSHIFT] <> 0);
    IsCtrl := (keyStat[SDL_SCANCODE_LCTRL] <> 0) or (keyStat[SDL_SCANCODE_RCTRL] <> 0);

    if IsShift then begin
      step := 0.5;
    end else begin
      step := 0.05;
    end;

    if keyStat[SDL_SCANCODE_RIGHT] <> 0 then begin
      if IsCtrl then begin
        rDest.x -= step;
        rDest.w += step * 2;
      end else begin
        rDest.x += step;
      end;
    end;
    if keyStat[SDL_SCANCODE_LEFT] <> 0 then begin
      if IsCtrl then begin
        if rDest.w > 1 then begin
          rDest.x += step;
          rDest.w -= step * 2;
        end;
      end else begin
        rDest.x -= step;
      end;
    end;
    if keyStat[SDL_SCANCODE_DOWN] <> 0 then begin
      if IsCtrl then begin
        rDest.y -= step;
        rDest.h += step * 2;
      end else begin
        rDest.y += step;
      end;
    end;
    if keyStat[SDL_SCANCODE_UP] <> 0 then begin
      if IsCtrl then begin
        if rDest.h > 1 then begin
          rDest.y += step;
          rDest.h -= step * 2;
        end;
      end else begin
        rDest.y -= step;
      end;
    end;

    rDest.w := SDL_clamp(rDest.w, 1, FWidth);
    rDest.h := SDL_clamp(rDest.h, 1, FHeight);

    rDest.x := SDL_clamp(rDest.x, 0, FWidth - rDest.w);
    rDest.y := SDL_clamp(rDest.y, 0, FHeight - rDest.h);
  end;
end;

end.

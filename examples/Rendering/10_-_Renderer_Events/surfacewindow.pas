unit SurfaceWindow;

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
    keyStat: PUInt8;
    //    procedure WinPos(ofsx, ofsy: integer);
    //    procedure WinResize(ofsx, ofsy: integer);
    procedure WinTransform(const ofs: TSDL_Rect);
    procedure ShowWinPos(const ev: TSDL_Event);
  end;

implementation

constructor TSurfaceWindow.Create;
begin
  inherited Create;
  win := SDL_CreateWindow('Surface Window', 320, 200, SDL_WINDOW_RESIZABLE);
  if win = nil then begin
    SDL_Log('Kann Surface Window nicht erzeugen !');
  end;
  SDL_SetWindowPosition(win, 50, 50);
  keyStat := SDL_GetKeyboardState(nil);
end;

destructor TSurfaceWindow.Destroy;
begin
  SDL_DestroyWindow(win);
  inherited Destroy;
end;

procedure TSurfaceWindow.paint;
var
  skyblue: uint32;
begin
  if winSurface <> nil then begin
    SDL_DestroySurface(winSurface);
  end;
  winSurface := SDL_GetWindowSurface(win);

  skyblue := SDL_MapRGB(winSurface^.format, 65, 193, 193);
  SDL_FillSurfaceRect(winSurface, nil, skyblue);
  SDL_UpdateWindowSurface(win);
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


procedure TSurfaceWindow.EventHandle(var ev: TSDL_Event);
begin
  case ev._type of
    SDL_EVENT_WINDOW_MOVED,
    SDL_EVENT_WINDOW_RESIZED: begin
      ShowWinPos(ev);
    end;
  end;
end;

procedure TSurfaceWindow.LoopHandle;
var
  IsShift, IsCtrl: boolean;
  step: integer;
  trans: TSDL_Rect = (x: 0; y: 0; w: 0; h: 0);
begin
  if (SDL_GetWindowFlags(win) and SDL_WINDOW_INPUT_FOCUS) = SDL_WINDOW_INPUT_FOCUS then begin
    IsShift := (keyStat[SDL_SCANCODE_LSHIFT] <> 0) or (keyStat[SDL_SCANCODE_RSHIFT] <> 0);
    IsCtrl := (keyStat[SDL_SCANCODE_LCTRL] <> 0) or (keyStat[SDL_SCANCODE_RCTRL] <> 0);

  if IsCtrl then WriteLn('Ctrl')else WriteLn('no Ctrl');

    if IsShift then begin
      step := 2;
    end else begin
      step := 1;
    end;

    if keyStat[SDL_SCANCODE_RIGHT] <> 0 then begin
      if IsCtrl then begin
        Inc(trans.w, step);
      end else begin
        Inc(trans.x, step);
      end;
    end;

    if keyStat[SDL_SCANCODE_LEFT] <> 0 then begin
      if IsCtrl then begin
        Dec(trans.w, step);
      end else begin
        Dec(trans.x, step);
      end;
    end;

    if keyStat[SDL_SCANCODE_DOWN] <> 0 then begin
      if IsCtrl then begin
        Inc(trans.h, step);
      end else begin
        Inc(trans.y, step);
      end;
    end;

    if keyStat[SDL_SCANCODE_UP] <> 0 then begin
      if IsCtrl then begin
        Dec(trans.h, step);
      end else begin
        Dec(trans.y, step);
      end;
    end;

    WinTransform(trans);
  end;
end;

end.

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
  private
    win: PSDL_Window;
    winSurface: PSDL_Surface;
    procedure paint;
  end;

implementation

{ TSurfaceWindow }

constructor TSurfaceWindow.Create;
begin
  inherited Create;
  win := SDL_CreateWindow('Surface Window', 320, 200, SDL_WINDOW_RESIZABLE);
  if win = nil then begin
    SDL_Log('Kann Surface Window nicht erzeugen !');
  end;
  SDL_SetWindowPosition(win, 50, 50);
  paint;
end;

destructor TSurfaceWindow.Destroy;
begin
  SDL_DestroySurface(winSurface);
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

procedure TSurfaceWindow.EventHandle(var ev: TSDL_Event);
begin
  case ev._type of
    SDL_EVENT_WINDOW_RESIZED: begin
      paint;
    end;
  end;
end;

end.

unit TargetTextureWindow;

interface

uses
  SDL3;

type

  { TTargetTextureWindow }

  TTargetTextureWindow = class(TObject)
    constructor Create;
    destructor Destroy; override;
    procedure EventHandle(var ev: TSDL_Event);
    procedure LoopHandle;
    procedure paint;
  private
    win: PSDL_Window;
    WinSize: TSDL_Point;
    renderer: PSDL_Renderer;
    texture: PSDL_Texture;
    keyStat: PUInt8;
    procedure Triangle(Arenderer:PSDL_Renderer);
  end;


implementation

constructor TTargetTextureWindow.Create;
var
  r: TSDL_FRect;
begin
  WinSize.x := 640;
  WinSize.y := 480;
  win := SDL_CreateWindow('Surface Window', WinSize.x, WinSize.y, SDL_WINDOW_RESIZABLE);
  if win = nil then begin
    SDL_Log('Kann Surface Window nicht erzeugen !');
  end;
  SDL_SetWindowPosition(win, 500, 550);

  renderer := SDL_CreateRenderer(win, nil);
  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  texture := SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, 512, 512);
  SDL_SetRenderTarget(renderer, texture);

  SDL_SetRenderDrawColorFloat(renderer, 0.5, 0.5, 1.0, SDL_ALPHA_OPAQUE);
  r.items := [20, 20, 512- 40, 512- 40];
  SDL_RenderFillRect(renderer, @r);

  Triangle(renderer);

  keyStat := SDL_GetKeyboardState(nil);
end;

destructor TTargetTextureWindow.Destroy;
begin
  SDL_DestroyTexture(texture);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(win);
  inherited Destroy;
end;

procedure TTargetTextureWindow.EventHandle(var ev: TSDL_Event);
begin

end;

procedure TTargetTextureWindow.LoopHandle;
begin

end;

procedure TTargetTextureWindow.paint;
const
  angele: double = 0.0;
var
  destR: TSDL_FRect;
begin

  SDL_SetRenderTarget(renderer, nil);
  SDL_SetRenderDrawColorFloat(renderer, 0.5, 1.0, 0.5, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(renderer);

  destR.items := [50, 50, 200, 200];
  angele += 0.1;

  SDL_RenderTextureRotated(renderer, texture, nil, @destR, angele, nil, SDL_FLIP_NONE);

  SDL_RenderPresent(renderer);
end;

procedure TTargetTextureWindow.Triangle(Arenderer: PSDL_Renderer);
const
  vert: array of TSDL_Vertex = (
    (position: (x: 256; y: 150); color: (r: 1.0; g: 0.0; b: 0.0; a: 1.0); tex_coord: (x: 0.5; y: 1.0)),
    (position: (x: 106; y: 450); color: (r: 0.0; g: 0.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 0.0)),
    (position: (x: 406; y: 450); color: (r: 0.0; g: 1.0; b: 0.0; a: 1.0); tex_coord: (x: 1.0; y: 0.0)));
begin
  SDL_RenderGeometry(Arenderer, nil, PSDL_Vertex(vert), Length(vert), nil, 0);
end;

end.

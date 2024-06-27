unit RenderGeometrieWindow;

interface

uses
  SDL3;
type
TRenderGeometrieWindow = class(TObject)
  constructor Create;
  destructor Destroy; override;
  procedure EventHandle(var ev: TSDL_Event);
  procedure LoopHandle;
  procedure paint;
private
  win: PSDL_Window;
  WinSize: TSDL_Point;
  renderer: PSDL_Renderer;
  bitmapTex: PSDL_Texture;
  keyStat: PUInt8;
  rDest: TSDL_FRect;
  function CreateTexture: PSDL_Texture;
end;
implementation

procedure Triangle(renderer: PSDL_Renderer);
const
  vert: array of TSDL_Vertex = (
    (position: (x: 200; y: 150); color: (r: 1.0; g: 0.0; b: 0.0; a: 1.0); tex_coord: (x: 0.5; y: 1.0)),
    (position: (x: 100; y: 450); color: (r: 0.0; g: 0.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 0.0)),
    (position: (x: 300; y: 450); color: (r: 0.0; g: 1.0; b: 0.0; a: 1.0); tex_coord: (x: 1.0; y: 0.0)));
begin
  SDL_RenderGeometry(renderer, nil, PSDL_Vertex(vert), Length(vert), nil, 0);
end;

procedure Quad(renderer: PSDL_Renderer);
const
  vert: array of TSDL_Vertex = (
    (position: (x: 500; y: 150); color: (r: 1.0; g: 0.0; b: 0.0; a: 1.0); tex_coord: (x: 0.5; y: 1.0)),
    (position: (x: 700; y: 150); color: (r: 0.0; g: 0.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 0.0)),
    (position: (x: 700; y: 250); color: (r: 0.0; g: 0.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 0.0)),
    (position: (x: 500; y: 250); color: (r: 1.0; g: 1.0; b: 0.0; a: 1.0); tex_coord: (x: 1.0; y: 0.0)));
  indices: array of integer = (0, 1, 2, 2, 3, 0);
begin
  SDL_RenderGeometry(renderer, nil, PSDL_Vertex(vert), Length(vert), PInteger(indices), Length(indices));
end;

function CreateTexture(renderer: PSDL_Renderer): PSDL_Texture;
var
  bitmapSurface: PSDL_Surface;
begin
  bitmapSurface := SDL_LoadBMP('mauer.bmp');
  if bitmapSurface = nil then  begin
    SDL_Log('Kann keine textur erzeugen !');
  end;

  Result := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
  if Result = nil then begin
    SDL_Log('Kann Textur nicht laden ezeugen !');
  end;
  SDL_DestroySurface(bitmapSurface);
end;

procedure TexturQuad(renderer: PSDL_Renderer);
const
  vert: array of TSDL_Vertex = (
    (position: (x: 500; y: 350); color: (r: 1.0; g: 1.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 0.0)),
    (position: (x: 700; y: 350); color: (r: 1.0; g: 1.0; b: 1.0; a: 1.0); tex_coord: (x: 1.0; y: 0.0)),
    (position: (x: 700; y: 450); color: (r: 1.0; g: 1.0; b: 1.0; a: 1.0); tex_coord: (x: 1.0; y: 1.0)),
    (position: (x: 500; y: 450); color: (r: 1.0; g: 1.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 1.0)));
  indices: array of integer = (0, 1, 2, 2, 3, 0);
var
  texture: PSDL_Texture;
begin
  texture := CreateTexture(renderer);
  SDL_RenderGeometry(renderer, texture, PSDL_Vertex(vert), Length(vert), PInteger(indices), Length(indices));
  SDL_DestroyTexture(texture);
end;

constructor TRenderGeometrieWindow.Create;
begin
  inherited Create;
  WinSize.x := 640;
  WinSize.y := 480;
  win := SDL_CreateWindow('Surface Window', WinSize.x, WinSize.y, SDL_WINDOW_RESIZABLE);
  if win = nil then begin
    SDL_Log('Kann Surface Window nicht erzeugen !');
  end;
  SDL_SetWindowPosition(win, 500, 250);

  renderer := SDL_CreateRenderer(win, nil);
  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  keyStat := SDL_GetKeyboardState(nil);
  bitmapTex := CreateTexture;

  rDest.items := [0, 0, 100, 100];
end;

destructor TRenderGeometrieWindow.Destroy;
begin
  SDL_DestroyTexture(bitmapTex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(win);
  inherited Destroy;
end;

procedure TRenderGeometrieWindow.EventHandle(var ev: TSDL_Event);
begin
  case ev._type of
    SDL_EVENT_WINDOW_RESIZED: begin
      SDL_GetWindowSize(win, @WinSize.x, @WinSize.y);
    end;
  end;
end;

procedure TRenderGeometrieWindow.LoopHandle;
begin

end;

procedure TRenderGeometrieWindow.paint;
var
  rSrc, r: TSDL_FRect;
begin
  SDL_SetRenderDrawColorFloat(renderer, 1.0, 0.5, 0.5, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(renderer);

  Triangle(renderer);
  Quad(renderer);
  TexturQuad(renderer);


  //rSrc.items := [0, 0, 400, 400];
  //SDL_RenderTexture(renderer, bitmapTex, @rSrc, @rDest);
  //
  //r.items := [200, 200, 64, 64];
  //SDL_RenderTexture(renderer, bitmapTex, @rSrc, @r);

  SDL_RenderPresent(renderer);
end;

function TRenderGeometrieWindow.CreateTexture: PSDL_Texture;
begin

end;

end.


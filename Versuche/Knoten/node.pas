unit Node;

interface

uses
  SDL3, FillCol;

type
  PNode = ^TNode;

  TNode = class(TObject)
    renderer: PSDL_Renderer;
    image: PSDL_Texture;
    Fpos: TSDL_FRect;
    link: PNode;
    backLink: PNode;
    constructor Create(screen: PSDL_Renderer);
    function isHis(var pt: TSDL_FPoint): TSDL_bool;
    procedure Clear;
    procedure SetPos(x, y: single);
    procedure unlink;
    procedure linkTo(other: PNode);
    procedure draw;
  end;



implementation

{ TNode }

constructor TNode.Create(screen: PSDL_Renderer);
var
  fill: TSDL_Color;
begin
  renderer := screen;
  Fpos.x := 0;
  Fpos.y := 0;
  Fpos.w := 15;
  Fpos.h := 15;
  image := SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, Trunc(Fpos.w), Trunc(Fpos.x));
  fill.r := 100;
  fill.g := 200;
  fill.b := 100;
  fill.a := 255;
  fillColor(renderer, image, fill);
end;

function TNode.isHis(var pt: TSDL_FPoint): TSDL_bool;
begin
  Result := SDL_PointInRectFloat(@pt, @Fpos);
end;

procedure TNode.Clear;
begin
  SDL_DestroyTexture(image);
  if backLink^.link = @Self then begin
    backLink^.unlink;
  end;
end;

procedure TNode.SetPos(x, y: single);
begin
  Fpos.x := x;
  Fpos.y := y;
end;

procedure TNode.unlink;
begin
  link := nil;
  backLink := nil;
end;

procedure TNode.linkTo(other: PNode);
begin
  link := other;
end;

procedure TNode.draw;
begin
  SDL_RenderTexture(renderer, image, nil, @Fpos);
  if link <> nil then begin
    SDL_SetRenderDrawColor(renderer, $FF, $FF, $FF, $FF);
  end;
  SDL_RenderLine(renderer, Fpos.x + Fpos.w / 2, Fpos.y + Fpos.h / 2, link^.Fpos.x + link^.Fpos.w / 2, link^.Fpos.y + link^.Fpos.h / 2);
end;

end.

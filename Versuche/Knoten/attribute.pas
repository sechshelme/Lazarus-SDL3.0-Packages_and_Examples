unit Attribute;

{$modeswitch arrayoperators on}

interface

uses
  SDL3, FillCol, Node;

type
  TAttribute = class(TObject)
    renderer: PSDL_Renderer;
    image: TSDL_Texture;
    Fpos: TSDL_FRect;
    mouse: TSDL_FPoint;
    input, output, dragLink: TNode;
    dragging: TSDL_bool;
    Value: integer;
    constructor Create(screen: PSDL_Renderer);
    procedure Clear;
    procedure SetPos(x, y: single);
    procedure handleEvent(event: PSDL_Event);
    procedure draw;
  end;


implementation

var
  inputs: array of TNode = nil;

constructor TAttribute.Create(screen: PSDL_Renderer);
var
  fill: TSDL_Color;
begin
  dragging := SDL_FALSE;
  Value := 0;
  renderer := screen;
  Fpos.x := 0;
  Fpos.y := 0;
  Fpos.w := 80;
  Fpos.h := 50;
  image := SDL_CreateTexture(renderer, SDL_PIXELFORMAT_RGBA8888, SDL_TEXTUREACCESS_TARGET, Trunc(Fpos.w), Trunc(Fpos.h));
  fill.r := 200;
  fill.g := 200;
  fill.b := 120;
  fill.a := 255;
  fillColor(renderer, image, fill);
  input := TNode.Create(renderer);
  inputs += [input];
  output := TNode.Create(renderer);
  dragLink := nil;
  SetPos(Random(900), Random(900));
end;

procedure TAttribute.Clear;
begin
  SDL_DestroyTexture(image);
  image := nil;
  input.Free;
  output.Free;
end;

procedure TAttribute.SetPos(x, y: single);
begin
  input.SetPos(x - 5, y);
  output.SetPos(x + Fpos.w - 10, y);
  Fpos.x := x;
  Fpos.y := y;
end;

procedure TAttribute.handleEvent(event: PSDL_Event);
var
  i: integer;
begin
  case event^.type_ of
    SDL_EVENT_MOUSE_MOTION: begin
      mouse.x := event^.button.x;
      mouse.y := event^.button.y;
      if dragLink <> nil then begin
        dragLink.SetPos(mouse.x - 8, mouse.y - 8);
      end else if dragging then begin
        SetPos(mouse.x, mouse.y);
      end;
    end;
    SDL_EVENT_MOUSE_BUTTON_DOWN: begin
      if output.isHis(mouse) then begin
        if output.link <> nil then begin
          output.link.unlink;
        end;
        output.unlink;
        dragLink := TNode.Create(renderer);
        dragLink.SetPos(mouse.x, mouse.y);
        output.linkTo(dragLink);
      end else if input.isHis(mouse) then begin
        input.backLink.unlink;
        input.backLink := nil;
      end else if SDL_PointInRectFloat(@mouse, @Fpos) then begin
        dragging := SDL_TRUE;
      end;
    end;
    SDL_EVENT_MOUSE_BUTTON_UP: begin
      if dragLink <> nil then begin
        if output.link <> nil then begin
          output.unlink;
        end;
        for i := 0 to Length(inputs) - 1 do begin
          if inputs[i].isHis(mouse) then begin
            if inputs[i].backLink = nil then begin
              output.linkTo(inputs[i]);
            end;
          end;
        end;
        dragLink.Clear;
        dragLink.Free;
        dragLink := nil;
      end;
      dragging := SDL_FALSE;
    end;
  end;
end;

procedure TAttribute.draw;
begin
  if image <> nil then begin
    SDL_RenderTexture(renderer, image, nil, @Fpos);
    input.draw;
    output.draw;
  end;
end;

end.

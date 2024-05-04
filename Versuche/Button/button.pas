unit Button;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  SDL3;

type
  TNotifyEvent = procedure(Sender: TObject) of object;

  { TButton }

  TButton = class(TObject)
  private
    FOnClick: TNotifyEvent;
    FRenderer: PSDL_Renderer;
    FRect:TSDL_FRect;
    procedure SetHeight(AValue: single);
    procedure SetLeft(AValue: single);
    procedure SetTop(AValue: single);
    procedure SetWidth(AValue: single);
  public
    constructor Create(renderer: PSDL_Renderer);
    destructor Destroy; override;
    property Left: single read FRect.x write SetLeft;
    property Top: single read FRect.y write SetTop;
    property Width: single read FRect.w write SetWidth;
    property Height: single read FRect.h write SetHeight;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    procedure Paint;
    procedure EventHandle(var event: TSDL_Event);
  end;



implementation


{ TButton }

procedure TButton.SetHeight(AValue: Single);
begin
  FRect.h := AValue;
end;

procedure TButton.SetLeft(AValue: Single);
begin
   FRect.x := AValue;
end;

procedure TButton.SetTop(AValue: Single);
begin
   FRect.y := AValue;
end;

procedure TButton.SetWidth(AValue: Single);
begin
  FRect.w := AValue;
end;

constructor TButton.Create(renderer: PSDL_Renderer);
begin
  inherited Create;
  FRenderer := renderer;
  FRect.x:=0;
  FRect.y:=0;
  FRect.w:=75;
  FRect.h:=25;
end;

destructor TButton.Destroy;
begin
  inherited Destroy;
end;

procedure TButton.Paint;
var
  r: TSDL_FRect;
begin
  SDL_SetRenderDrawColorFloat(FRenderer, 0.5, 0.5, 0.5, SDL_ALPHA_OPAQUE);
  r.x := Left;
  r.y := Top;
  r.w := Width;
  r.h := Height;
  SDL_RenderFillRect(FRenderer, @r);
end;

procedure TButton.EventHandle(var event: TSDL_Event);
var
  mp: TSDL_FPoint;
begin
  case event.type_ of
    SDL_EVENT_MOUSE_BUTTON_DOWN: begin
      mp.x := event.button.x;
      mp.y := event.button.y;
      if SDL_PointInRectFloat(@mp, @FRect) then  begin
       WriteLn('------------------------------------------');
       if FOnClick<>nil then OnClick(nil);
      end;
    end;
  end;
end;

end.

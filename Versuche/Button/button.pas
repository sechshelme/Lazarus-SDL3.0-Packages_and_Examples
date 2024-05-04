unit Button;

interface

uses
  SDL3;

type
  TVector4f = array [0..3] of single;
  TNotifyEvent = procedure(Sender: TObject) of object;

  { TButton }

  TButton = class(TObject)
  private
    FCaption: string;
    FColor: TVector4f;
    FOnClick: TNotifyEvent;
    FRenderer: PSDL_Renderer;
    FRect: TSDL_FRect;
    procedure SetCaption(AValue: string);
    procedure SetColor(AValue: TVector4f);
    procedure SetHeight(AValue: single);
    procedure SetLeft(AValue: single);
    procedure SetRect(AValue: TSDL_FRect);
    procedure SetTop(AValue: single);
    procedure SetWidth(AValue: single);
  public
    constructor Create(renderer: PSDL_Renderer);
    destructor Destroy; override;
    property Left: single read FRect.x write SetLeft;
    property Top: single read FRect.y write SetTop;
    property Width: single read FRect.w write SetWidth;
    property Height: single read FRect.h write SetHeight;
    property Rect: TSDL_FRect read FRect write SetRect;
    property Caption: string read FCaption write SetCaption;
    property Color: TVector4f read FColor write SetColor;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    procedure Paint;
    procedure EventHandle(var event: TSDL_Event);
  end;

function Rect(x, y, w, h: single): TSDL_FRect; inline;

implementation

function Rect(x, y, w, h: single): TSDL_FRect; inline;
begin
  Result.x := x;
  Result.y := y;
  Result.w := w;
  Result.h := h;
end;

procedure TButton.SetHeight(AValue: single);
begin
  FRect.h := AValue;
end;

procedure TButton.SetCaption(AValue: string);
begin
  if FCaption = AValue then begin
    Exit;
  end;
  FCaption := AValue;
end;

procedure TButton.SetColor(AValue: TVector4f);
begin
  FColor := AValue;
end;

procedure TButton.SetLeft(AValue: single);
begin
  FRect.x := AValue;
end;

procedure TButton.SetRect(AValue: TSDL_FRect);
begin
  FRect := AValue;
end;

procedure TButton.SetTop(AValue: single);
begin
  FRect.y := AValue;
end;

procedure TButton.SetWidth(AValue: single);
begin
  FRect.w := AValue;
end;

constructor TButton.Create(renderer: PSDL_Renderer);
begin
  inherited Create;
  FRenderer := renderer;
  FRect.x := 0;
  FRect.y := 0;
  FRect.w := 75;
  FRect.h := 25;
  FCaption := 'Button';
  FColor := [0.5, 0.5, 0.5, SDL_ALPHA_OPAQUE];
end;

destructor TButton.Destroy;
begin
  inherited Destroy;
end;

procedure TButton.Paint;
var
  r: TSDL_FRect;
begin
  SDL_SetRenderDrawColorFloat(FRenderer, FColor[0], FColor[1], FColor[2], FColor[3]);
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
        if FOnClick <> nil then begin
          OnClick(nil);
        end;
      end;
    end;
  end;
end;

end.

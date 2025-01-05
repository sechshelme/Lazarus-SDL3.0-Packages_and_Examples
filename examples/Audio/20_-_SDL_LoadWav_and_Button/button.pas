unit Button;

interface

uses
  SDL3;

type
  TNotifyEvent = procedure(Sender: TObject) of object;

  { TButton }

  TButton = class(TObject)
  private
    FCaption: string;
    FColor: TUint32;
    FOnClick: TNotifyEvent;
    FOnDown: TNotifyEvent;
    FRect: TSDL_FRect;
    tex: PSDL_Texture;
    IsMouseDown, IsButtonDown: boolean;
    FRenderer: PSDL_Renderer;
    procedure CreateTexture;
    procedure SetCaption(AValue: string);
    procedure SetColor(AValue: TUint32);
    procedure SetCanvasRect(AValue: TSDL_FRect);
  public
    property CanvasRect: TSDL_FRect read FRect write SetCanvasRect;
    property Caption: string read FCaption write SetCaption;
    property Color: TUint32 read FColor write SetColor;
    property OnDown: TNotifyEvent read FOnDown write FOnDown;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    constructor Create(renderer: PSDL_Renderer);
    destructor Destroy; override;
    procedure Paint;
    procedure EventHandle(var event: TSDL_Event);
  end;

  function Rect(x, y, w, h: integer): TSDL_Rect; inline;
  function RectF(x, y, w, h: Single): TSDL_FRect; inline;

implementation

function Rect(x, y, w, h: integer): TSDL_Rect; inline;
begin
  Result.x := x;
  Result.y := y;
  Result.w := w;
  Result.h := h;
end;

function RectF(x, y, w, h: Single): TSDL_FRect;
begin
  Result.x := x;
  Result.y := y;
  Result.w := w;
  Result.h := h;
end;

{ TButton }

constructor TButton.Create(renderer: PSDL_Renderer);
begin
  inherited Create;
  FRenderer := renderer;
  IsMouseDown := False;
  IsButtonDown := False;
  FRect.x := 0;
  FRect.y := 0;
  FRect.w := 75;
  FRect.h := 25;
  FCaption := 'Button';
  FColor := $888888FF;
  CreateTexture;
end;

destructor TButton.Destroy;
begin
  SDL_DestroyTexture(tex);
  inherited Destroy;
end;

procedure TButton.CreateTexture;
const
  col1 = $FFFFFFFF;
  col2 = $000000FF;
  w = 32;
  h = 32;
var
  sur: PSDL_Surface;
  r: TSDL_Rect;
begin
  sur := SDL_CreateSurface(w, h * 2, SDL_PIXELFORMAT_RGBA8888);
  r := Rect(0, 0, w, h * 2);

  SDL_FillSurfaceRect(sur, @r, $000000FF);

  // Rahmen Up
  r := Rect(1, h + 1, w - 2, h - 2);
  SDL_FillSurfaceRect(sur, @r, col1);

  r := Rect(3, h + 3, w - 4, h - 4);
  SDL_FillSurfaceRect(sur, @r, col2);

  r := Rect(3, h + 3, w - 6, h - 6);
  SDL_FillSurfaceRect(sur, @r, FColor);

  // Rahmen Down
  r := Rect(1, 1, w - 2, h - 2);
  SDL_FillSurfaceRect(sur, @r, col2);

  r := Rect(3, 3, w - 4, h - 4);
  SDL_FillSurfaceRect(sur, @r, col1);

  r := Rect(3, 3, w - 6, h - 6);
  SDL_FillSurfaceRect(sur, @r, FColor);

  if tex <> nil then  begin
    SDL_DestroyTexture(tex);
  end;
  tex := SDL_CreateTextureFromSurface(FRenderer, sur);
  SDL_DestroySurface(sur);
end;

procedure TButton.Paint;
var
  srcrect: TSDL_FRect;
  w, h: Single;
begin
  SDL_GetTextureSize(tex, @w, @h);

  srcrect.x := 0;
  srcrect.w := w;
  srcrect.h := h / 2;
  if IsButtonDown then begin
    srcrect.y := 0;
  end else begin
    srcrect.y := h / 2;
  end;
  SDL_RenderTexture(FRenderer, tex, @srcrect, @FRect);
end;

procedure TButton.SetCaption(AValue: string);
begin
  if FCaption = AValue then begin
    Exit;
  end;
  FCaption := AValue;
end;

procedure TButton.SetColor(AValue: TUint32);
begin
  if FColor = AValue then begin
    Exit;
  end;
  FColor := AValue;
  CreateTexture;
end;

procedure TButton.SetCanvasRect(AValue: TSDL_FRect);
begin
  FRect := AValue;
  CreateTexture;
end;

procedure TButton.EventHandle(var event: TSDL_Event);
var
  mp: TSDL_FPoint;
  IsInRegion: TSDL_bool;
begin
  case Event._type of
    SDL_EVENT_MOUSE_BUTTON_DOWN: begin
      mp.x := event.button.x;
      mp.y := event.button.y;
      IsInRegion := SDL_PointInRectFloat(@mp, @FRect);
      if IsInRegion then begin
        IsMouseDown := True;
        IsButtonDown := True;
      end else begin
        IsMouseDown := False;
        IsButtonDown := False;
      end;
    end;
    SDL_EVENT_MOUSE_MOTION: begin
      mp.x := Trunc(event.motion.x);
      mp.y := Trunc(event.motion.y);
      IsInRegion := SDL_PointInRectFloat(@mp, @FRect);
      if IsMouseDown then begin
        if IsInRegion then begin
          IsButtonDown := True;
        end else begin
          IsButtonDown := False;
        end;
      end;
    end;
    SDL_EVENT_MOUSE_BUTTON_UP: begin
      mp.x := Trunc(event.button.x);
      mp.y := Trunc(event.button.y);
      IsInRegion := SDL_PointInRect(@mp, @FRect);
      if IsMouseDown and IsInRegion then begin
        if OnClick <> nil then begin
          OnClick(self);
        end;
      end;
      IsMouseDown := False;
      IsButtonDown := False;
    end;
  end;
end;

end.

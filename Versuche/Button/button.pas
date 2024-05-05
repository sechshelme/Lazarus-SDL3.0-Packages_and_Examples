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
    FRect: TSDL_Rect;
    IsMouseDown, IsButtonDown: boolean;
    FRenderer: PSDL_Renderer;
    procedure SetCaption(AValue: string);
    procedure SetColor(AValue: TUint32);
    procedure SetCanvasRect(AValue: TSDL_Rect);
  public
    property CanvasRect: TSDL_Rect read FRect write SetCanvasRect;
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

implementation

function Rect(x, y, w, h: integer): TSDL_Rect; inline;
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
end;

destructor TButton.Destroy;
begin
  inherited Destroy;
end;

procedure TButton.Paint;
var
  tex: PSDL_Texture;
  sur: PSDL_Surface;
  dstrect: TSDL_FRect;
  r: TSDL_Rect;
  col1, col2: TUint32;
begin
  if IsButtonDown then begin
    col1 := $000000FF;
    col2 := $FFFFFFFF;
  end else begin
    col1 := $FFFFFFFF;
    col2 := $000000FF;
  end;

  sur := SDL_CreateSurface(FRect.w, FRect.h, SDL_PIXELFORMAT_RGBA8888);
  r := Rect(0, 0, FRect.w, FRect.h);
  SDL_FillSurfaceRect(sur, @r, $000000FF);

  r := Rect(1, 1, FRect.w - 2, FRect.h - 2);
  SDL_FillSurfaceRect(sur, @r, col1);

  r := Rect(3, 3, FRect.w - 4, FRect.h - 4);
  SDL_FillSurfaceRect(sur, @r, col2);

  r := Rect(3, 3, FRect.w - 6, FRect.h - 6);
  SDL_FillSurfaceRect(sur, @r, FColor);

  tex := SDL_CreateTextureFromSurface(FRenderer, sur);

  dstrect.x := FRect.x;
  dstrect.y := FRect.y;
  dstrect.w := FRect.w;
  dstrect.h := FRect.h;

  SDL_RenderTexture(FRenderer, tex, nil, @dstrect);

  SDL_DestroyTexture(tex);
  SDL_DestroySurface(sur);
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
  FColor := AValue;
end;

procedure TButton.SetCanvasRect(AValue: TSDL_Rect);
begin
  FRect := AValue;
end;

procedure TButton.EventHandle(var event: TSDL_Event);
var
  mp: TSDL_Point;
  IsInRegion: TSDL_bool;
begin
  case Event.type_ of
    SDL_EVENT_MOUSE_BUTTON_DOWN: begin
      mp.x := Trunc(event.button.x);
      mp.y := Trunc(event.button.y);
      IsInRegion := SDL_PointInRect(@mp, @FRect);
      if IsInRegion then begin
        IsMouseDown := True;
        IsButtonDown := True;
      end else begin
        IsMouseDown := False;
        IsButtonDown := False;
      end;
      Paint;
    end;
    SDL_EVENT_MOUSE_MOTION: begin
      mp.x := Trunc(event.motion.x);
      mp.y := Trunc(event.motion.y);
      IsInRegion := SDL_PointInRect(@mp, @FRect);
      if IsMouseDown then begin
        if IsInRegion then begin
          IsButtonDown := True;
        end else begin
          IsButtonDown := False;
        end;
        Paint;
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
      Paint;
    end;
  end;
end;

end.

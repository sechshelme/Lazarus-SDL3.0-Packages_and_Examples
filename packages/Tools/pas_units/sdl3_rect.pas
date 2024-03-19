unit SDL3_rect;

interface

uses
  ctypes,
  SDL3_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_Point = ^TSDL_Point;

  TSDL_Point = record
    x: longint;
    y: longint;
  end;

  PSDL_FPoint = ^TSDL_FPoint;

  TSDL_FPoint = record
    x: single;
    y: single;
  end;

  PSDL_Rect = ^TSDL_Rect;

  TSDL_Rect = record
    x: longint;
    y: longint;
    w: longint;
    h: longint;
  end;

  PSDL_FRect = ^TSDL_FRect;

  TSDL_FRect = record
    x: single;
    y: single;
    w: single;
    h: single;
  end;

function SDL_PointInRect(p: PSDL_Point; r: PSDL_Rect): TSDL_bool;
function SDL_RectEmpty(r: PSDL_Rect): TSDL_bool;
function SDL_RectsEqual(a: PSDL_Rect; b: PSDL_Rect): TSDL_bool;

function SDL_HasRectIntersection(A: PSDL_Rect; B: PSDL_Rect): TSDL_bool; cdecl; external;
function SDL_GetRectIntersection(A: PSDL_Rect; B: PSDL_Rect; Result: PSDL_Rect): TSDL_bool; cdecl; external;
function SDL_GetRectUnion(A: PSDL_Rect; B: PSDL_Rect; Result: PSDL_Rect): longint; cdecl; external;
function SDL_GetRectEnclosingPoints(points: PSDL_Point; Count: longint; clip: PSDL_Rect; Result: PSDL_Rect): TSDL_bool; cdecl; external;
function SDL_GetRectAndLineIntersection(rect: PSDL_Rect; X1: Plongint; Y1: Plongint; X2: Plongint; Y2: Plongint): TSDL_bool; cdecl; external;


{ SDL_FRect versions...  }
function SDL_PointInRectFloat(p: PSDL_FPoint; r: PSDL_FRect): TSDL_bool;
function SDL_RectEmptyFloat(r: PSDL_FRect): TSDL_bool;
function SDL_RectsEqualEpsilon(a: PSDL_FRect; b: PSDL_FRect; epsilon: cfloat): TSDL_bool;
function SDL_RectsEqualFloat(a:PSDL_FRect; b: PSDL_FRect):TSDL_bool;

function SDL_HasRectIntersectionFloat(A: PSDL_FRect; B: PSDL_FRect): TSDL_bool; cdecl; external;
function SDL_GetRectIntersectionFloat(A: PSDL_FRect; B: PSDL_FRect; Result: PSDL_FRect): TSDL_bool; cdecl; external;
function SDL_GetRectUnionFloat(A: PSDL_FRect; B: PSDL_FRect; Result: PSDL_FRect): longint; cdecl; external;
function SDL_GetRectEnclosingPointsFloat(points: PSDL_FPoint; Count: longint; clip: PSDL_FRect; Result: PSDL_FRect): TSDL_bool; cdecl; external;
function SDL_GetRectAndLineIntersectionFloat(rect: PSDL_FRect; X1: Psingle; Y1: Psingle; X2: Psingle; Y2: Psingle): TSDL_bool; cdecl; external;

implementation

function SDL_PointInRect(p: PSDL_Point; r: PSDL_Rect): TSDL_bool;
begin
  if (p^.x >= r^.x) and (p^.x < (r^.x + r^.w)) and (p^.y >= r^.y) and (p^.y < (r^.y + r^.h)) then begin
    Result := SDL_TRUE;
  end else begin
    Result := SDL_FALSE;
  end;
  {SDL_FORCE_INLINE SDL_bool SDL_PointInRect(const SDL_Point *p, const SDL_Rect *r) }
  {    return ( (p->x >= r->x) && (p->x < (r->x + r->w)) && }
  {             (p->y >= r->y) && (p->y < (r->y + r->h)) ) ? SDL_TRUE : SDL_FALSE; }
end;

function SDL_RectEmpty(r: PSDL_Rect): TSDL_bool;
begin
  if (r = nil) or (r^.w <= 0) or (r^.h <= 0) then begin
    Result := SDL_TRUE;
  end else begin
    Result := SDL_FALSE;
  end;
  {SDL_FORCE_INLINE SDL_bool SDL_RectEmpty(const SDL_Rect *r) }
  {    return ((!r) || (r->w <= 0) || (r->h <= 0)) ? SDL_TRUE : SDL_FALSE; }
end;

function SDL_RectsEqual(a: PSDL_Rect; b: PSDL_Rect): TSDL_bool;
begin
  if ((a <> nil) and (b <> nil) and (a^.x = b^.x) and (a^.y = b^.y) and (a^.w = b^.w) and (a^.h = b^.h)) then begin
    Result := SDL_TRUE;
  end else begin
    Result := SDL_FALSE;
  end;
  {SDL_FORCE_INLINE SDL_bool SDL_RectsEqual(const SDL_Rect *a, const SDL_Rect *b) }
  {    return (a && b && (a->x == b->x) && (a->y == b->y) && }
  {            (a->w == b->w) && (a->h == b->h)) ? SDL_TRUE : SDL_FALSE; }
end;

function SDL_PointInRectFloat(p: PSDL_FPoint; r: PSDL_FRect): TSDL_bool;
begin
  if (p^.x >= r^.x) and (p^.x < (r^.x + r^.w)) and (p^.y >= r^.y) and (p^.y < (r^.y + r^.h)) then begin
    Result := SDL_TRUE;
  end else begin
    Result := SDL_FALSE;
  end;
  {SDL_FORCE_INLINE SDL_bool SDL_PointInRectFloat(const SDL_FPoint *p, const SDL_FRect *r) }
  {    return ( (p->x >= r->x) && (p->x < (r->x + r->w)) && }
  {             (p->y >= r->y) && (p->y < (r->y + r->h)) ) ? SDL_TRUE : SDL_FALSE; }
end;

function SDL_RectEmptyFloat(r: PSDL_FRect): TSDL_bool;
begin
  if (r = nil) or (r^.w <= 0.0) or (r^.h <= 0.0) then begin
    Result := SDL_TRUE;
  end else begin
    Result := SDL_FALSE;
  end;
  {SDL_FORCE_INLINE SDL_bool SDL_RectEmptyFloat(const SDL_FRect *r) }
  {    return ((!r) || (r->w <= 0.0f) || (r->h <= 0.0f)) ? SDL_TRUE : SDL_FALSE; }
end;

function SDL_RectsEqualEpsilon(a: PSDL_FRect; b: PSDL_FRect; epsilon: cfloat): TSDL_bool;
begin
  if (a <> nil) and (b <> nil) and ((a = b) or
    ((SDL_fabsf(a^.x - b^.x) <= epsilon) and
    (SDL_fabsf(a^.y - b^.y) <= epsilon) and
    (SDL_fabsf(a^.w - b^.w) <= epsilon) and
    (SDL_fabsf(a^.h - b^.h) <= epsilon))) then begin
    Result := SDL_TRUE;
  end else begin
    Result := SDL_FALSE;
  end;
  {SDL_FORCE_INLINE SDL_bool SDL_RectsEqualEpsilon(const SDL_FRect *a, const SDL_FRect *b, const float epsilon) }
  {    return (a && b && ((a == b) || }
  {            ((SDL_fabsf(a->x - b->x) <= epsilon) && }
  {            (SDL_fabsf(a->y - b->y) <= epsilon) && }
  {            (SDL_fabsf(a->w - b->w) <= epsilon) && }
  {            (SDL_fabsf(a->h - b->h) <= epsilon)))) }
  {            ? SDL_TRUE : SDL_FALSE; }
end;

function SDL_RectsEqualFloat(a: PSDL_FRect; b: PSDL_FRect): TSDL_bool;
begin
  Result:=  SDL_RectsEqualEpsilon(a, b, SDL_FLT_EPSILON);
  {SDL_FORCE_INLINE SDL_bool SDL_RectsEqualFloat(const SDL_FRect *a, const SDL_FRect *b) }
  {    return SDL_RectsEqualEpsilon(a, b, SDL_FLT_EPSILON); }
end;

end.

unit SDL_rect;

interface

uses
  ctypes, SDL_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


type
  PSDL_Point = ^TSDL_Point;
  TSDL_Point = record
      x : longint;
      y : longint;
    end;

  PSDL_FPoint = ^TSDL_FPoint;
  TSDL_FPoint = record
      x : single;
      y : single;
    end;

  PSDL_Rect = ^TSDL_Rect;
  TSDL_Rect = record
      x : longint;
      y : longint;
      w : longint;
      h : longint;
    end;

  PSDL_FRect = ^TSDL_FRect;
  TSDL_FRect = record
      x : single;
      y : single;
      w : single;
      h : single;
    end;
{ xxxxxxxxxxxxxx }
{SDL_FORCE_INLINE bool SDL_PointInRect(const SDL_Point *p, const SDL_Rect *r) }
{ }
{    return ( p && r && (p->x >= r->x) && (p->x < (r->x + r->w)) && }
{             (p->y >= r->y) && (p->y < (r->y + r->h)) ) ? true : false; }
{ }
{ }
{*
 * Determine whether a rectangle has no area.
 *
 * A rectangle is considered "empty" for this function if `r` is NULL, or if
 * `r`'s width and/or height are <= 0.
 *
 * Note that this is a forced-inline function in a header, and not a public
 * API function available in the SDL library (which is to say, the code is
 * embedded in the calling program and the linker and dynamic loader will not
 * be able to find this function inside SDL itself).
 *
 * \param r the rectangle to test.
 * \returns true if the rectangle is "empty", false otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
{ xxxxxxxxxxxxxx }
{SDL_FORCE_INLINE bool SDL_RectEmpty(const SDL_Rect *r) }
{ }
{    return ((!r) || (r->w <= 0) || (r->h <= 0)) ? true : false; }
{ }
{*
 * Determine whether two rectangles are equal.
 *
 * Rectangles are considered equal if both are not NULL and each of their x,
 * y, width and height match.
 *
 * Note that this is a forced-inline function in a header, and not a public
 * API function available in the SDL library (which is to say, the code is
 * embedded in the calling program and the linker and dynamic loader will not
 * be able to find this function inside SDL itself).
 *
 * \param a the first rectangle to test.
 * \param b the second rectangle to test.
 * \returns true if the rectangles are equal, false otherwise.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
  }
{ // xxxxxxxxxxxxxx }
{SDL_FORCE_INLINE bool SDL_RectsEqual(const SDL_Rect *a, const SDL_Rect *b) }
{ }
{    return (a && b && (a->x == b->x) && (a->y == b->y) && }
{            (a->w == b->w) && (a->h == b->h)) ? true : false; }
{ }
function SDL_HasRectIntersection(A:PSDL_Rect; B:PSDL_Rect):Tbool;cdecl;external libSDL3;
function SDL_GetRectIntersection(A:PSDL_Rect; B:PSDL_Rect; result:PSDL_Rect):Tbool;cdecl;external libSDL3;
function SDL_GetRectUnion(A:PSDL_Rect; B:PSDL_Rect; result:PSDL_Rect):Tbool;cdecl;external libSDL3;
function SDL_GetRectEnclosingPoints(points:PSDL_Point; count:longint; clip:PSDL_Rect; result:PSDL_Rect):Tbool;cdecl;external libSDL3;
function SDL_GetRectAndLineIntersection(rect:PSDL_Rect; X1:Plongint; Y1:Plongint; X2:Plongint; Y2:Plongint):Tbool;cdecl;external libSDL3;
{ xxxxxxxxxxxxxx }
{SDL_FORCE_INLINE bool SDL_PointInRectFloat(const SDL_FPoint *p, const SDL_FRect *r) }
{ }
{    return ( p && r && (p->x >= r->x) && (p->x <= (r->x + r->w)) && }
{             (p->y >= r->y) && (p->y <= (r->y + r->h)) ) ? true : false; }
{ }
{ xxxxxxxxxxxxxx }
{SDL_FORCE_INLINE bool SDL_RectEmptyFloat(const SDL_FRect *r) }
{ }
{    return ((!r) || (r->w < 0.0f) || (r->h < 0.0f)) ? true : false; }
{ }
{ xxxxxxxxxxxxxx }
{SDL_FORCE_INLINE bool SDL_RectsEqualEpsilon(const SDL_FRect *a, const SDL_FRect *b, const float epsilon) }
{ }
{    return (a && b && ((a == b) || }
{            ((SDL_fabsf(a->x - b->x) <= epsilon) && }
{            (SDL_fabsf(a->y - b->y) <= epsilon) && }
{            (SDL_fabsf(a->w - b->w) <= epsilon) && }
{            (SDL_fabsf(a->h - b->h) <= epsilon)))) }
{            ? true : false; }
{ }
{ xxxxxxxxxxxxxx }
{SDL_FORCE_INLINE bool SDL_RectsEqualFloat(const SDL_FRect *a, const SDL_FRect *b) }
{ }
{  return SDL_RectsEqualEpsilon(a, b, SDL_FLT_EPSILON); }
{ }
function SDL_HasRectIntersectionFloat(A:PSDL_FRect; B:PSDL_FRect):Tbool;cdecl;external libSDL3;
function SDL_GetRectIntersectionFloat(A:PSDL_FRect; B:PSDL_FRect; result:PSDL_FRect):Tbool;cdecl;external libSDL3;
function SDL_GetRectUnionFloat(A:PSDL_FRect; B:PSDL_FRect; result:PSDL_FRect):Tbool;cdecl;external libSDL3;
function SDL_GetRectEnclosingPointsFloat(points:PSDL_FPoint; count:longint; clip:PSDL_FRect; result:PSDL_FRect):Tbool;cdecl;external libSDL3;
function SDL_GetRectAndLineIntersectionFloat(rect:PSDL_FRect; X1:Psingle; Y1:Psingle; X2:Psingle; Y2:Psingle):Tbool;cdecl;external libSDL3;

implementation


end.

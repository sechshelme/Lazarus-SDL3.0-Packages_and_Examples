unit SDL3_mouse;

interface

uses
  SDL3_stdinc, SDL3_video, SDL3_surface;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_MouseID = ^TSDL_MouseID;
  TSDL_MouseID = uint32;

  PSDL_Cursor = Pointer;

  PSDL_SystemCursor = ^TSDL_SystemCursor;
  TSDL_SystemCursor = longint;

const
  SDL_SYSTEM_CURSOR_ARROW = 0;
  SDL_SYSTEM_CURSOR_IBEAM = 1;
  SDL_SYSTEM_CURSOR_WAIT = 2;
  SDL_SYSTEM_CURSOR_CROSSHAIR = 3;
  SDL_SYSTEM_CURSOR_WAITARROW = 4;
  SDL_SYSTEM_CURSOR_SIZENWSE = 5;
  SDL_SYSTEM_CURSOR_SIZENESW = 6;
  SDL_SYSTEM_CURSOR_SIZEWE = 7;
  SDL_SYSTEM_CURSOR_SIZENS = 8;
  SDL_SYSTEM_CURSOR_SIZEALL = 9;
  SDL_SYSTEM_CURSOR_NO = 10;
  SDL_SYSTEM_CURSOR_HAND = 11;
  SDL_SYSTEM_CURSOR_WINDOW_TOPLEFT = 12;
  SDL_SYSTEM_CURSOR_WINDOW_TOP = 13;
  SDL_SYSTEM_CURSOR_WINDOW_TOPRIGHT = 14;
  SDL_SYSTEM_CURSOR_WINDOW_RIGHT = 15;
  SDL_SYSTEM_CURSOR_WINDOW_BOTTOMRIGHT = 16;
  SDL_SYSTEM_CURSOR_WINDOW_BOTTOM = 17;
  SDL_SYSTEM_CURSOR_WINDOW_BOTTOMLEFT = 18;
  SDL_SYSTEM_CURSOR_WINDOW_LEFT = 19;
  SDL_NUM_SYSTEM_CURSORS = 20;

type
  PSDL_MouseWheelDirection = ^TSDL_MouseWheelDirection;
  TSDL_MouseWheelDirection = longint;

const
  SDL_MOUSEWHEEL_NORMAL = 0;
  SDL_MOUSEWHEEL_FLIPPED = 1;

function SDL_GetMouseFocus: PSDL_Window; cdecl; external;
function SDL_GetMouseState(x: Psingle; y: Psingle): uint32; cdecl; external;
function SDL_GetGlobalMouseState(x: Psingle; y: Psingle): uint32; cdecl; external;
function SDL_GetRelativeMouseState(x: Psingle; y: Psingle): uint32; cdecl; external;
procedure SDL_WarpMouseInWindow(window: PSDL_Window; x: single; y: single); cdecl; external;
function SDL_WarpMouseGlobal(x: single; y: single): longint; cdecl; external;
function SDL_SetRelativeMouseMode(Enabled: TSDL_bool): longint; cdecl; external;
function SDL_CaptureMouse(Enabled: TSDL_bool): longint; cdecl; external;
function SDL_GetRelativeMouseMode: TSDL_bool; cdecl; external;
function SDL_CreateCursor(Data: PUint8; mask: PUint8; w: longint; h: longint; hot_x: longint; hot_y: longint): PSDL_Cursor; cdecl; external;
function SDL_CreateColorCursor(surface: PSDL_Surface; hot_x: longint; hot_y: longint): PSDL_Cursor; cdecl; external;
function SDL_CreateSystemCursor(id: TSDL_SystemCursor): PSDL_Cursor; cdecl; external;
function SDL_SetCursor(cursor: PSDL_Cursor): longint; cdecl; external;
function SDL_GetCursor: PSDL_Cursor; cdecl; external;
function SDL_GetDefaultCursor: PSDL_Cursor; cdecl; external;
procedure SDL_DestroyCursor(cursor: PSDL_Cursor); cdecl; external;
function SDL_ShowCursor: longint; cdecl; external;
function SDL_HideCursor: longint; cdecl; external;
function SDL_CursorVisible: TSDL_bool; cdecl; external;

function SDL_BUTTON(X: int32): int32;

const
  SDL_BUTTON_LEFT = 1;
  SDL_BUTTON_MIDDLE = 2;
  SDL_BUTTON_RIGHT = 3;
  SDL_BUTTON_X1 = 4;
  SDL_BUTTON_X2 = 5;

function SDL_BUTTON_LMASK: int32;
function SDL_BUTTON_MMASK: int32;
function SDL_BUTTON_RMASK: int32;
function SDL_BUTTON_X1MASK: int32;
function SDL_BUTTON_X2MASK: int32;

implementation

function SDL_BUTTON(X: int32): int32;
begin
  SDL_BUTTON := 1 shl (X - 1);
end;

function SDL_BUTTON_LMASK: int32;
begin
  SDL_BUTTON_LMASK := SDL_BUTTON(SDL_BUTTON_LEFT);
end;

function SDL_BUTTON_MMASK: int32;
begin
  SDL_BUTTON_MMASK := SDL_BUTTON(SDL_BUTTON_MIDDLE);
end;

function SDL_BUTTON_RMASK: int32;
begin
  SDL_BUTTON_RMASK := SDL_BUTTON(SDL_BUTTON_RIGHT);
end;

function SDL_BUTTON_X1MASK: int32;
begin
  SDL_BUTTON_X1MASK := SDL_BUTTON(SDL_BUTTON_X1);
end;

function SDL_BUTTON_X2MASK: int32;
begin
  SDL_BUTTON_X2MASK := SDL_BUTTON(SDL_BUTTON_X2);
end;

end.

unit SDL_mouse;

interface

uses
  SDL_stdinc, SDL_video, SDL_surface;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_MouseID = ^TSDL_MouseID;
  TSDL_MouseID = TUint32;

  PSDL_SystemCursor = ^TSDL_SystemCursor;
  TSDL_SystemCursor = longint;

const
  SDL_SYSTEM_CURSOR_DEFAULT = 0;
  SDL_SYSTEM_CURSOR_TEXT = 1;
  SDL_SYSTEM_CURSOR_WAIT = 2;
  SDL_SYSTEM_CURSOR_CROSSHAIR = 3;
  SDL_SYSTEM_CURSOR_PROGRESS = 4;
  SDL_SYSTEM_CURSOR_NWSE_RESIZE = 5;
  SDL_SYSTEM_CURSOR_NESW_RESIZE = 6;
  SDL_SYSTEM_CURSOR_EW_RESIZE = 7;
  SDL_SYSTEM_CURSOR_NS_RESIZE = 8;
  SDL_SYSTEM_CURSOR_MOVE = 9;
  SDL_SYSTEM_CURSOR_NOT_ALLOWED = 10;
  SDL_SYSTEM_CURSOR_POINTER = 11;
  SDL_SYSTEM_CURSOR_NW_RESIZE = 12;
  SDL_SYSTEM_CURSOR_N_RESIZE = 13;
  SDL_SYSTEM_CURSOR_NE_RESIZE = 14;
  SDL_SYSTEM_CURSOR_E_RESIZE = 15;
  SDL_SYSTEM_CURSOR_SE_RESIZE = 16;
  SDL_SYSTEM_CURSOR_S_RESIZE = 17;
  SDL_SYSTEM_CURSOR_SW_RESIZE = 18;
  SDL_SYSTEM_CURSOR_W_RESIZE = 19;
  SDL_SYSTEM_CURSOR_COUNT = 20;

type
  PSDL_MouseWheelDirection = ^TSDL_MouseWheelDirection;
  TSDL_MouseWheelDirection = longint;

const
  SDL_MOUSEWHEEL_NORMAL = 0;
  SDL_MOUSEWHEEL_FLIPPED = 1;

type
  PSDL_MouseButtonFlags = ^TSDL_MouseButtonFlags;
  TSDL_MouseButtonFlags = TUint32;

const
  SDL_BUTTON_LEFT = 1;
  SDL_BUTTON_MIDDLE = 2;
  SDL_BUTTON_RIGHT = 3;
  SDL_BUTTON_X1 = 4;
  SDL_BUTTON_X2 = 5;

function SDL_BUTTON_MASK(X: longint): longint;
function SDL_BUTTON_LMASK: longint;
function SDL_BUTTON_MMASK: longint;
function SDL_BUTTON_RMASK: longint;
function SDL_BUTTON_X1MASK: longint;
function SDL_BUTTON_X2MASK: longint;

type
  TSDL_Cursor = record
  end;
  PSDL_Cursor = ^TSDL_Cursor;

function SDL_HasMouse: Tbool; cdecl; external libSDL3;
function SDL_GetMice(Count: Plongint): PSDL_MouseID; cdecl; external libSDL3;
function SDL_GetMouseNameForID(instance_id: TSDL_MouseID): pansichar; cdecl; external libSDL3;
function SDL_GetMouseFocus: PSDL_Window; cdecl; external libSDL3;
function SDL_GetMouseState(x: Psingle; y: Psingle): TSDL_MouseButtonFlags; cdecl; external libSDL3;
function SDL_GetGlobalMouseState(x: Psingle; y: Psingle): TSDL_MouseButtonFlags; cdecl; external libSDL3;
function SDL_GetRelativeMouseState(x: Psingle; y: Psingle): TSDL_MouseButtonFlags; cdecl; external libSDL3;
procedure SDL_WarpMouseInWindow(window: PSDL_Window; x: single; y: single); cdecl; external libSDL3;
function SDL_WarpMouseGlobal(x: single; y: single): Tbool; cdecl; external libSDL3;
function SDL_SetWindowRelativeMouseMode(window: PSDL_Window; Enabled: Tbool): Tbool; cdecl; external libSDL3;
function SDL_GetWindowRelativeMouseMode(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_CaptureMouse(Enabled: Tbool): Tbool; cdecl; external libSDL3;
function SDL_CreateCursor(Data: PUint8; mask: PUint8; w: longint; h: longint; hot_x: longint;
  hot_y: longint): PSDL_Cursor; cdecl; external libSDL3;
function SDL_CreateColorCursor(surface: PSDL_Surface; hot_x: longint; hot_y: longint): PSDL_Cursor; cdecl; external libSDL3;
function SDL_CreateSystemCursor(id: TSDL_SystemCursor): PSDL_Cursor; cdecl; external libSDL3;
function SDL_SetCursor(cursor: PSDL_Cursor): Tbool; cdecl; external libSDL3;
function SDL_GetCursor: PSDL_Cursor; cdecl; external libSDL3;
function SDL_GetDefaultCursor: PSDL_Cursor; cdecl; external libSDL3;
procedure SDL_DestroyCursor(cursor: PSDL_Cursor); cdecl; external libSDL3;
function SDL_ShowCursor: Tbool; cdecl; external libSDL3;
function SDL_HideCursor: Tbool; cdecl; external libSDL3;
function SDL_CursorVisible: Tbool; cdecl; external libSDL3;

implementation

function SDL_BUTTON_MASK(X: longint): longint;
begin
  SDL_BUTTON_MASK := 1 shl (X - 1);
end;

function SDL_BUTTON_LMASK: longint;
begin
  SDL_BUTTON_LMASK := SDL_BUTTON_MASK(SDL_BUTTON_LEFT);
end;

function SDL_BUTTON_MMASK: longint;
begin
  SDL_BUTTON_MMASK := SDL_BUTTON_MASK(SDL_BUTTON_MIDDLE);
end;

function SDL_BUTTON_RMASK: longint;
begin
  SDL_BUTTON_RMASK := SDL_BUTTON_MASK(SDL_BUTTON_RIGHT);
end;

function SDL_BUTTON_X1MASK: longint;
begin
  SDL_BUTTON_X1MASK := SDL_BUTTON_MASK(SDL_BUTTON_X1);
end;

function SDL_BUTTON_X2MASK: longint;
begin
  SDL_BUTTON_X2MASK := SDL_BUTTON_MASK(SDL_BUTTON_X2);
end;


end.

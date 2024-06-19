/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 19-06-2024   19:57                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_MouseID = ^TSDL_MouseID;
  TSDL_MouseID = TUint32;

  PSDL_Cursor = ^TSDL_Cursor;
  TSDL_Cursor = Pointer;

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
  SDL_NUM_SYSTEM_CURSORS = 20;

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

function SDL_BUTTON_LMASK: int32;
function SDL_BUTTON_MMASK: int32;
function SDL_BUTTON_RMASK: int32;
function SDL_BUTTON_X1MASK: int32;
function SDL_BUTTON_X2MASK: int32;
function SDL_HasMouse: TSDL_bool; cdecl; external sdl3_lib;
function SDL_GetMice(Count: Plongint): PSDL_MouseID; cdecl; external sdl3_lib;
function SDL_GetMouseInstanceName(instance_id: TSDL_MouseID): PChar; cdecl; external sdl3_lib;
function SDL_GetMouseFocus: PSDL_Window; cdecl; external sdl3_lib;
function SDL_GetMouseState(x: Psingle; y: Psingle): TSDL_MouseButtonFlags; cdecl; external sdl3_lib;
function SDL_GetGlobalMouseState(x: Psingle; y: Psingle): TSDL_MouseButtonFlags; cdecl; external sdl3_lib;
function SDL_GetRelativeMouseState(x: Psingle; y: Psingle): TSDL_MouseButtonFlags; cdecl; external sdl3_lib;
procedure SDL_WarpMouseInWindow(window: PSDL_Window; x: single; y: single); cdecl; external sdl3_lib;
function SDL_WarpMouseGlobal(x: single; y: single): longint; cdecl; external sdl3_lib;
function SDL_SetRelativeMouseMode(Enabled: TSDL_bool): longint; cdecl; external sdl3_lib;
function SDL_CaptureMouse(Enabled: TSDL_bool): longint; cdecl; external sdl3_lib;
function SDL_GetRelativeMouseMode: TSDL_bool; cdecl; external sdl3_lib;
function SDL_CreateCursor(Data: PUint8; mask: PUint8; w: longint; h: longint; hot_x: longint; hot_y: longint): PSDL_Cursor; cdecl; external sdl3_lib;
function SDL_CreateColorCursor(surface: PSDL_Surface; hot_x: longint; hot_y: longint): PSDL_Cursor; cdecl; external sdl3_lib;
function SDL_CreateSystemCursor(id: TSDL_SystemCursor): PSDL_Cursor; cdecl; external sdl3_lib;
function SDL_SetCursor(cursor: PSDL_Cursor): longint; cdecl; external sdl3_lib;
function SDL_GetCursor: PSDL_Cursor; cdecl; external sdl3_lib;
function SDL_GetDefaultCursor: PSDL_Cursor; cdecl; external sdl3_lib;
procedure SDL_DestroyCursor(cursor: PSDL_Cursor); cdecl; external sdl3_lib;
function SDL_ShowCursor: longint; cdecl; external sdl3_lib;
function SDL_HideCursor: longint; cdecl; external sdl3_lib;
function SDL_CursorVisible: TSDL_bool; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

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

{$ENDIF read_implementation}

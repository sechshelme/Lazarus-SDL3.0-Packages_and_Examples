/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 06-07-2024   17:47                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  TSDL_KeyboardID = uint32;
  PSDL_KeyboardID = ^TSDL_KeyboardID;

  PSDL_Keysym = ^TSDL_Keysym;

  TSDL_Keysym = record
    scancode: TSDL_Scancode;
    sym: TSDL_Keycode;
    mod_: uint16;
    unused: uint32;
  end;

function SDL_GetKeyboardFocus: PSDL_Window; cdecl; external sdl3_lib;
function SDL_GetKeyboardState(numkeys: Plongint): PUint8; cdecl; external sdl3_lib;
procedure SDL_ResetKeyboard; cdecl; external sdl3_lib;
function SDL_GetModState: TSDL_Keymod; cdecl; external sdl3_lib;
procedure SDL_SetModState(modstate: TSDL_Keymod); cdecl; external sdl3_lib;
function SDL_GetKeyFromScancode(scancode: TSDL_Scancode): TSDL_Keycode; cdecl; external sdl3_lib;
function SDL_GetScancodeFromKey(key: TSDL_Keycode): TSDL_Scancode; cdecl; external sdl3_lib;
function SDL_GetScancodeName(scancode: TSDL_Scancode): PChar; cdecl; external sdl3_lib;
function SDL_GetScancodeFromName(Name: PChar): TSDL_Scancode; cdecl; external sdl3_lib;
function SDL_GetKeyName(key: TSDL_Keycode): PChar; cdecl; external sdl3_lib;
function SDL_GetKeyFromName(Name: PChar): TSDL_Keycode; cdecl; external sdl3_lib;
procedure SDL_StartTextInput; cdecl; external sdl3_lib;
function SDL_TextInputActive: TSDL_bool; cdecl; external sdl3_lib;
procedure SDL_StopTextInput; cdecl; external sdl3_lib;
procedure SDL_ClearComposition; cdecl; external sdl3_lib;
//function SDL_TextInputShown: TSDL_bool; cdecl; external sdl3_lib;
//function SDL_SetTextInputRect(rect: PSDL_Rect): longint; cdecl; external sdl3_lib;

function SDL_SetTextInputArea(window : PSDL_Window; rect: PSDL_Rect; cursor :longint): longint; cdecl; external sdl3_lib;
function SDL_GetTextInputArea(window : PSDL_Window; rect: PSDL_Rect; cursor :Plongint): longint; cdecl; external sdl3_lib;

//extern SDL_DECLSPEC int SDLCALL SDL_SetTextInputArea(SDL_Window *window, const SDL_Rect *rect, int cursor);
//extern SDL_DECLSPEC int SDLCALL SDL_GetTextInputArea(SDL_Window *window, SDL_Rect *rect, int *cursor);



function SDL_HasScreenKeyboardSupport: TSDL_bool; cdecl; external sdl3_lib;
function SDL_ScreenKeyboardShown(window: PSDL_Window): TSDL_bool; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

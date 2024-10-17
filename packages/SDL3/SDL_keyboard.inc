/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_KeyboardID = ^TSDL_KeyboardID;
  TSDL_KeyboardID = TUint32;

function SDL_HasKeyboard: Tbool; cdecl; external libSDL3;
function SDL_GetKeyboards(Count: Plongint): PSDL_KeyboardID; cdecl; external libSDL3;
function SDL_GetKeyboardNameForID(instance_id: TSDL_KeyboardID): pansichar; cdecl; external libSDL3;
function SDL_GetKeyboardFocus: PSDL_Window; cdecl; external libSDL3;
function SDL_GetKeyboardState(numkeys: Plongint): PBoolean; cdecl; external libSDL3;
procedure SDL_ResetKeyboard; cdecl; external libSDL3;
function SDL_GetModState: TSDL_Keymod; cdecl; external libSDL3;
procedure SDL_SetModState(modstate: TSDL_Keymod); cdecl; external libSDL3;
function SDL_GetKeyFromScancode(scancode: TSDL_Scancode; modstate: TSDL_Keymod; key_event: Tbool): TSDL_Keycode; cdecl; external libSDL3;
function SDL_GetScancodeFromKey(key: TSDL_Keycode; modstate: PSDL_Keymod): TSDL_Scancode; cdecl; external libSDL3;
function SDL_SetScancodeName(scancode: TSDL_Scancode; Name: pansichar): Tbool; cdecl; external libSDL3;
function SDL_GetScancodeName(scancode: TSDL_Scancode): pansichar; cdecl; external libSDL3;
function SDL_GetScancodeFromName(Name: pansichar): TSDL_Scancode; cdecl; external libSDL3;
function SDL_GetKeyName(key: TSDL_Keycode): pansichar; cdecl; external libSDL3;
function SDL_GetKeyFromName(Name: pansichar): TSDL_Keycode; cdecl; external libSDL3;
function SDL_StartTextInput(window: PSDL_Window): Tbool; cdecl; external libSDL3;

type
  PSDL_TextInputType = ^TSDL_TextInputType;
  TSDL_TextInputType = longint;

const
  SDL_TEXTINPUT_TYPE_TEXT = 0;
  SDL_TEXTINPUT_TYPE_TEXT_NAME = 1;
  SDL_TEXTINPUT_TYPE_TEXT_EMAIL = 2;
  SDL_TEXTINPUT_TYPE_TEXT_USERNAME = 3;
  SDL_TEXTINPUT_TYPE_TEXT_PASSWORD_HIDDEN = 4;
  SDL_TEXTINPUT_TYPE_TEXT_PASSWORD_VISIBLE = 5;
  SDL_TEXTINPUT_TYPE_NUMBER = 6;
  SDL_TEXTINPUT_TYPE_NUMBER_PASSWORD_HIDDEN = 7;
  SDL_TEXTINPUT_TYPE_NUMBER_PASSWORD_VISIBLE = 8;

type
  PSDL_Capitalization = ^TSDL_Capitalization;
  TSDL_Capitalization = longint;

const
  SDL_CAPITALIZE_NONE = 0;
  SDL_CAPITALIZE_SENTENCES = 1;
  SDL_CAPITALIZE_WORDS = 2;
  SDL_CAPITALIZE_LETTERS = 3;

function SDL_StartTextInputWithProperties(window: PSDL_Window; props: TSDL_PropertiesID): Tbool; cdecl; external libSDL3;

const
  SDL_PROP_TEXTINPUT_TYPE_NUMBER = 'SDL.textinput.type';
  SDL_PROP_TEXTINPUT_CAPITALIZATION_NUMBER = 'SDL.textinput.capitalization';
  SDL_PROP_TEXTINPUT_AUTOCORRECT_BOOLEAN = 'SDL.textinput.autocorrect';
  SDL_PROP_TEXTINPUT_MULTILINE_BOOLEAN = 'SDL.textinput.multiline';
  SDL_PROP_TEXTINPUT_ANDROID_INPUTTYPE_NUMBER = 'SDL.textinput.android.inputtype';

function SDL_TextInputActive(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_StopTextInput(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_ClearComposition(window: PSDL_Window): Tbool; cdecl; external libSDL3;
function SDL_SetTextInputArea(window: PSDL_Window; rect: PSDL_Rect; cursor: longint): Tbool; cdecl; external libSDL3;
function SDL_GetTextInputArea(window: PSDL_Window; rect: PSDL_Rect; cursor: Plongint): Tbool; cdecl; external libSDL3;
function SDL_HasScreenKeyboardSupport: Tbool; cdecl; external libSDL3;
function SDL_ScreenKeyboardShown(window: PSDL_Window): Tbool; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}

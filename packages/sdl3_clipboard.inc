/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 06-07-2024   17:47                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_SetClipboardText(Text: PChar): longint; cdecl; external sdl3_lib;
function SDL_GetClipboardText: PChar; cdecl; external sdl3_lib;
function SDL_HasClipboardText: TSDL_bool; cdecl; external sdl3_lib;
function SDL_SetPrimarySelectionText(Text: PChar): longint; cdecl; external sdl3_lib;
function SDL_GetPrimarySelectionText: PChar; cdecl; external sdl3_lib;
function SDL_HasPrimarySelectionText: TSDL_bool; cdecl; external sdl3_lib;

type
  PSDL_ClipboardDataCallback = ^TSDL_ClipboardDataCallback;
  TSDL_ClipboardDataCallback = function(userdata: pointer; mime_type: PChar; size: Psize_t): pointer; cdecl;

  TSDL_ClipboardCleanupCallback = procedure(userdata: pointer); cdecl;

function SDL_SetClipboardData(callback: TSDL_ClipboardDataCallback; cleanup: TSDL_ClipboardCleanupCallback; userdata: pointer; mime_types: PPchar; num_mime_types: Tsize_t): longint; cdecl; external sdl3_lib;
function SDL_ClearClipboardData: longint; cdecl; external sdl3_lib;
function SDL_GetClipboardData(mime_type: PChar; size: Psize_t): pointer; cdecl; external sdl3_lib;
function SDL_HasClipboardData(mime_type: PChar): TSDL_bool; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

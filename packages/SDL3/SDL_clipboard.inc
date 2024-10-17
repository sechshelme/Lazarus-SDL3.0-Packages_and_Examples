/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_SetClipboardText(Text: pansichar): Tbool; cdecl; external libSDL3;
function SDL_GetClipboardText: pansichar; cdecl; external libSDL3;
function SDL_HasClipboardText: Tbool; cdecl; external libSDL3;
function SDL_SetPrimarySelectionText(Text: pansichar): Tbool; cdecl; external libSDL3;
function SDL_GetPrimarySelectionText: pansichar; cdecl; external libSDL3;
function SDL_HasPrimarySelectionText: Tbool; cdecl; external libSDL3;

type
  PSDL_ClipboardDataCallback = ^TSDL_ClipboardDataCallback;
  TSDL_ClipboardDataCallback = function(userdata: pointer; mime_type: pansichar; size: Psize_t): pointer; cdecl;

  TSDL_ClipboardCleanupCallback = procedure(userdata: pointer); cdecl;

function SDL_SetClipboardData(callback: TSDL_ClipboardDataCallback; cleanup: TSDL_ClipboardCleanupCallback; userdata: pointer; mime_types: PPansichar; num_mime_types: Tsize_t): Tbool; cdecl; external libSDL3;
function SDL_GetClipboardData(mime_type: pansichar; size: Psize_t): pointer; cdecl; external libSDL3;
function SDL_GetClipboardMimeTypes(num_mime_types: Psize_t): PPAnsiChar; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}

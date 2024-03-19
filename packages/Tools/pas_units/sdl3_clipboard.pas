unit SDL3_clipboard;

interface

uses
  SDL3_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

function SDL_SetClipboardText(Text: PChar): longint; cdecl; external;
function SDL_GetClipboardText: PChar; cdecl; external;
function SDL_HasClipboardText: TSDL_bool; cdecl; external;
function SDL_SetPrimarySelectionText(Text: PChar): longint; cdecl; external;
function SDL_GetPrimarySelectionText: PChar; cdecl; external;
function SDL_HasPrimarySelectionText: TSDL_bool; cdecl; external;

type
  PSDL_ClipboardDataCallback = ^TSDL_ClipboardDataCallback;
  TSDL_ClipboardDataCallback = function(userdata: pointer; mime_type: PChar; size: Psize_t): pointer; cdecl;

  TSDL_ClipboardCleanupCallback = procedure(userdata: pointer); cdecl;

function SDL_SetClipboardData(callback: TSDL_ClipboardDataCallback; cleanup: TSDL_ClipboardCleanupCallback; userdata: pointer; mime_types: PPchar; num_mime_types: Tsize_t): longint; cdecl; external;
function SDL_ClearClipboardData: longint; cdecl; external;
function SDL_GetClipboardData(mime_type: PChar; size: Psize_t): pointer; cdecl; external;
function SDL_HasClipboardData(mime_type: PChar): TSDL_bool; cdecl; external;

implementation

end.

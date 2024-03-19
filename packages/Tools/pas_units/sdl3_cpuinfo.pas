unit SDL3_cpuinfo;

interface

uses
  SDL3_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  SDL_CACHELINE_SIZE = 128;

function SDL_GetCPUCount: longint; cdecl; external;
function SDL_GetCPUCacheLineSize: longint; cdecl; external;
function SDL_HasAltiVec: TSDL_bool; cdecl; external;
function SDL_HasMMX: TSDL_bool; cdecl; external;
function SDL_HasSSE: TSDL_bool; cdecl; external;
function SDL_HasSSE2: TSDL_bool; cdecl; external;
function SDL_HasSSE3: TSDL_bool; cdecl; external;
function SDL_HasSSE41: TSDL_bool; cdecl; external;
function SDL_HasSSE42: TSDL_bool; cdecl; external;
function SDL_HasAVX: TSDL_bool; cdecl; external;
function SDL_HasAVX2: TSDL_bool; cdecl; external;
function SDL_HasAVX512F: TSDL_bool; cdecl; external;
function SDL_HasARMSIMD: TSDL_bool; cdecl; external;
function SDL_HasNEON: TSDL_bool; cdecl; external;
function SDL_HasLSX: TSDL_bool; cdecl; external;
function SDL_HasLASX: TSDL_bool; cdecl; external;
function SDL_GetSystemRAM: longint; cdecl; external;
function SDL_SIMDGetAlignment: Tsize_t; cdecl; external;

implementation

end.

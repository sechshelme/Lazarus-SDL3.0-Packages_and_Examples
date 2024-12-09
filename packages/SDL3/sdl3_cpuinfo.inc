
{$IFDEF read_interface}

const
  SDL_CACHELINE_SIZE = 128;

function SDL_GetCPUCount: longint; cdecl; external sdl3_lib;
function SDL_GetCPUCacheLineSize: longint; cdecl; external sdl3_lib;
function SDL_HasAltiVec: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasMMX: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasSSE: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasSSE2: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasSSE3: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasSSE41: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasSSE42: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasAVX: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasAVX2: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasAVX512F: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasARMSIMD: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasNEON: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasLSX: TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasLASX: TSDL_bool; cdecl; external sdl3_lib;
function SDL_GetSystemRAM: longint; cdecl; external sdl3_lib;
function SDL_SIMDGetAlignment: Tsize_t; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

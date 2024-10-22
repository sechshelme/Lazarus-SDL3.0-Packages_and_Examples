unit SDL_cpuinfo;

interface

uses
  SDL_stdinc;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}


const
  SDL_CACHELINE_SIZE = 128;

function SDL_GetNumLogicalCPUCores: longint; cdecl; external libSDL3;
function SDL_GetCPUCacheLineSize: longint; cdecl; external libSDL3;
function SDL_HasAltiVec: Tbool; cdecl; external libSDL3;
function SDL_HasMMX: Tbool; cdecl; external libSDL3;
function SDL_HasSSE: Tbool; cdecl; external libSDL3;
function SDL_HasSSE2: Tbool; cdecl; external libSDL3;
function SDL_HasSSE3: Tbool; cdecl; external libSDL3;
function SDL_HasSSE41: Tbool; cdecl; external libSDL3;
function SDL_HasSSE42: Tbool; cdecl; external libSDL3;
function SDL_HasAVX: Tbool; cdecl; external libSDL3;
function SDL_HasAVX2: Tbool; cdecl; external libSDL3;
function SDL_HasAVX512F: Tbool; cdecl; external libSDL3;
function SDL_HasARMSIMD: Tbool; cdecl; external libSDL3;
function SDL_HasNEON: Tbool; cdecl; external libSDL3;
function SDL_HasLSX: Tbool; cdecl; external libSDL3;
function SDL_HasLASX: Tbool; cdecl; external libSDL3;
function SDL_GetSystemRAM: longint; cdecl; external libSDL3;
function SDL_GetSIMDAlignment: Tsize_t; cdecl; external libSDL3;

implementation


end.

unit SDL_metal;

interface

uses
  SDL_stdinc, SDL_video;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  TSDL_MetalView = pointer;
  PSDL_MetalView = ^TSDL_MetalView;

function SDL_Metal_CreateView(window: PSDL_Window): TSDL_MetalView; cdecl; external libSDL3;
procedure SDL_Metal_DestroyView(view: TSDL_MetalView); cdecl; external libSDL3;
function SDL_Metal_GetLayer(view: TSDL_MetalView): pointer; cdecl; external libSDL3;

implementation


end.

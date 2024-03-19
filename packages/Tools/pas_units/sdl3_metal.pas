unit SDL3_metal;

interface

uses
  SDL3_video;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  PSDL_MetalView = ^TSDL_MetalView;
  TSDL_MetalView = pointer;

function SDL_Metal_CreateView(window: PSDL_Window): TSDL_MetalView; cdecl; external;
procedure SDL_Metal_DestroyView(view: TSDL_MetalView); cdecl; external;
function SDL_Metal_GetLayer(view: TSDL_MetalView): pointer; cdecl; external;

implementation

end.

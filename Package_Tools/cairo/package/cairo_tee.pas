unit cairo_tee;

interface

uses
  cairo;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

function cairo_tee_surface_create(primary: Pcairo_surface_t): Pcairo_surface_t; cdecl; external cairo_lib;
procedure cairo_tee_surface_add(abstract_surface: Pcairo_surface_t; target: Pcairo_surface_t); cdecl; external cairo_lib;
procedure cairo_tee_surface_remove(abstract_surface: Pcairo_surface_t; target: Pcairo_surface_t); cdecl; external cairo_lib;
function cairo_tee_surface_index(abstract_surface: Pcairo_surface_t; index: dword): Pcairo_surface_t; cdecl; external cairo_lib;

implementation

end.

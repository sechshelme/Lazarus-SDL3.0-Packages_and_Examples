unit cairo_xlib_xrender;

interface

uses
  xrender,
  x, xlib,
  cairo;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

function cairo_xlib_surface_create_with_xrender_format(dpy: PDisplay; drawable: TDrawable; screen: PScreen; format: PXRenderPictFormat; Width: longint; Height: longint): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_xlib_surface_get_xrender_format(surface: Pcairo_surface_t): PXRenderPictFormat; cdecl; external cairo_lib;

implementation

end.

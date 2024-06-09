unit cairo_xlib;

interface

uses
  x,
  xlib,
  cairo;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

function cairo_xlib_surface_create(dpy: PDisplay; drawable: TDrawable; visual: PVisual; Width: longint; Height: longint): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_xlib_surface_create_for_bitmap(dpy: PDisplay; bitmap: TPixmap; screen: PScreen; Width: longint; Height: longint): Pcairo_surface_t; cdecl; external cairo_lib;
procedure cairo_xlib_surface_set_size(surface: Pcairo_surface_t; Width: longint; Height: longint); cdecl; external cairo_lib;
procedure cairo_xlib_surface_set_drawable(surface: Pcairo_surface_t; drawable: TDrawable; Width: longint; Height: longint); cdecl; external cairo_lib;
function cairo_xlib_surface_get_display(surface: Pcairo_surface_t): PDisplay; cdecl; external cairo_lib;
function cairo_xlib_surface_get_drawable(surface: Pcairo_surface_t): TDrawable; cdecl; external cairo_lib;
function cairo_xlib_surface_get_screen(surface: Pcairo_surface_t): PScreen; cdecl; external cairo_lib;
function cairo_xlib_surface_get_visual(surface: Pcairo_surface_t): PVisual; cdecl; external cairo_lib;
function cairo_xlib_surface_get_depth(surface: Pcairo_surface_t): longint; cdecl; external cairo_lib;
function cairo_xlib_surface_get_width(surface: Pcairo_surface_t): longint; cdecl; external cairo_lib;
function cairo_xlib_surface_get_height(surface: Pcairo_surface_t): longint; cdecl; external cairo_lib;
procedure cairo_xlib_device_debug_cap_xrender_version(device: Pcairo_device_t; major_version: longint; minor_version: longint); cdecl; external cairo_lib;
procedure cairo_xlib_device_debug_set_precision(device: Pcairo_device_t; precision: longint); cdecl; external cairo_lib;
function cairo_xlib_device_debug_get_precision(device: Pcairo_device_t): longint; cdecl; external cairo_lib;

implementation

end.

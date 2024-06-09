unit cairo_xcb;

interface

uses
  cairo;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


//{$include <xcb/xcb.h>}
//{$include <xcb/render.h>}

//function cairo_xcb_surface_create(connection:Pxcb_connection_t; drawable:Txcb_drawable_t; visual:Pxcb_visualtype_t; width:longint; height:longint):Pcairo_surface_t;cdecl; external cairo_lib;
//function cairo_xcb_surface_create_for_bitmap(connection:Pxcb_connection_t; screen:Pxcb_screen_t; bitmap:Txcb_pixmap_t; width:longint; height:longint):Pcairo_surface_t;cdecl; external cairo_lib;
//function cairo_xcb_surface_create_with_xrender_format(connection:Pxcb_connection_t; screen:Pxcb_screen_t; drawable:Txcb_drawable_t; format:Pxcb_render_pictforminfo_t; width:longint; 
//           height:longint):Pcairo_surface_t;cdecl; external cairo_lib;
//procedure cairo_xcb_surface_set_size(surface:Pcairo_surface_t; width:longint; height:longint);cdecl; external cairo_lib;
//procedure cairo_xcb_surface_set_drawable(surface:Pcairo_surface_t; drawable:Txcb_drawable_t; width:longint; height:longint);cdecl; external cairo_lib;
//function cairo_xcb_device_get_connection(device:Pcairo_device_t):Pxcb_connection_t;cdecl; external cairo_lib;
//{ debug interface  }
//procedure cairo_xcb_device_debug_cap_xshm_version(device:Pcairo_device_t; major_version:longint; minor_version:longint);cdecl; external cairo_lib;
//procedure cairo_xcb_device_debug_cap_xrender_version(device:Pcairo_device_t; major_version:longint; minor_version:longint);cdecl; external cairo_lib;
//{
// * @precision: -1 implies automatically choose based on antialiasing mode,
// *            any other value overrides and sets the corresponding PolyMode.
//  }
//procedure cairo_xcb_device_debug_set_precision(device:Pcairo_device_t; precision:longint);cdecl; external cairo_lib;
//function cairo_xcb_device_debug_get_precision(device:Pcairo_device_t):longint;cdecl; external cairo_lib;

implementation


end.

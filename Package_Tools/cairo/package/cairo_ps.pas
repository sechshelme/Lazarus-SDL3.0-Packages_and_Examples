unit cairo_ps;

interface

uses
  cairo;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  Pcairo_ps_level = ^Tcairo_ps_level;
  Tcairo_ps_level =  Longint;
  Const
    CAIRO_PS_LEVEL_2 = 0;
    CAIRO_PS_LEVEL_3 = 1;
type
  Tcairo_ps_level_t = Tcairo_ps_level;
  Pcairo_ps_level_t = ^Tcairo_ps_level_t;
  PPcairo_ps_level_t = ^Pcairo_ps_level_t;

function cairo_ps_surface_create(filename:Pchar; width_in_points:Tdouble; height_in_points:Tdouble):Pcairo_surface_t;cdecl; external cairo_lib;
function cairo_ps_surface_create_for_stream(write_func:Tcairo_write_func_t; closure:pointer; width_in_points:Tdouble; height_in_points:Tdouble):Pcairo_surface_t;cdecl; external cairo_lib;
procedure cairo_ps_surface_restrict_to_level(surface:Pcairo_surface_t; level:Tcairo_ps_level_t);cdecl; external cairo_lib;
procedure cairo_ps_get_levels(levels:PPcairo_ps_level_t; num_levels:Plongint);cdecl; external cairo_lib;
function cairo_ps_level_to_string(level:Tcairo_ps_level_t):Pchar;cdecl; external cairo_lib;
procedure cairo_ps_surface_set_eps(surface:Pcairo_surface_t; eps:Tcairo_bool_t);cdecl; external cairo_lib;
function cairo_ps_surface_get_eps(surface:Pcairo_surface_t):Tcairo_bool_t;cdecl; external cairo_lib;
procedure cairo_ps_surface_set_size(surface:Pcairo_surface_t; width_in_points:Tdouble; height_in_points:Tdouble);cdecl; external cairo_lib;
procedure cairo_ps_surface_dsc_comment(surface:Pcairo_surface_t; comment:Pchar);cdecl; external cairo_lib;
procedure cairo_ps_surface_dsc_begin_setup(surface:Pcairo_surface_t);cdecl; external cairo_lib;
procedure cairo_ps_surface_dsc_begin_page_setup(surface:Pcairo_surface_t);cdecl; external cairo_lib;

implementation

end.

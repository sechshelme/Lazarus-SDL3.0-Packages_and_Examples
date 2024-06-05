unit cairo_svg;

interface

uses
  cairo;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  Pcairo_svg_version = ^Tcairo_svg_version;
  Tcairo_svg_version = longint;

const
  CAIRO_SVG_VERSION_1_1 = 0;
  CAIRO_SVG_VERSION_1_2 = 1;

type
  Tcairo_svg_version_t = Tcairo_svg_version;
  Pcairo_svg_version_t = ^Tcairo_svg_version_t;
  PPcairo_svg_version_t = ^Pcairo_svg_version_t;

type
  Pcairo_svg_unit = ^Tcairo_svg_unit;
  Tcairo_svg_unit = longint;

const
  CAIRO_SVG_UNIT_USER = 0;
  CAIRO_SVG_UNIT_EM = 1;
  CAIRO_SVG_UNIT_EX = 2;
  CAIRO_SVG_UNIT_PX = 3;
  CAIRO_SVG_UNIT_IN = 4;
  CAIRO_SVG_UNIT_CM = 5;
  CAIRO_SVG_UNIT_MM = 6;
  CAIRO_SVG_UNIT_PT = 7;
  CAIRO_SVG_UNIT_PC = 8;
  CAIRO_SVG_UNIT_PERCENT = 9;

type
  Tcairo_svg_unit_t = Tcairo_svg_unit;
  Pcairo_svg_unit_t = ^Tcairo_svg_unit_t;

function cairo_svg_surface_create(filename: PChar; width_in_points: Tdouble; height_in_points: Tdouble): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_svg_surface_create_for_stream(write_func: Tcairo_write_func_t; closure: pointer; width_in_points: Tdouble; height_in_points: Tdouble): Pcairo_surface_t; cdecl; external cairo_lib;
procedure cairo_svg_surface_restrict_to_version(surface: Pcairo_surface_t; version: Tcairo_svg_version_t); cdecl; external cairo_lib;
procedure cairo_svg_get_versions(versions: PPcairo_svg_version_t; num_versions: Plongint); cdecl; external cairo_lib;
function cairo_svg_version_to_string(version: Tcairo_svg_version_t): PChar; cdecl; external cairo_lib;
procedure cairo_svg_surface_set_document_unit(surface: Pcairo_surface_t; unit_: Tcairo_svg_unit_t); cdecl; external cairo_lib;
function cairo_svg_surface_get_document_unit(surface: Pcairo_surface_t): Tcairo_svg_unit_t; cdecl; external cairo_lib;

implementation

end.

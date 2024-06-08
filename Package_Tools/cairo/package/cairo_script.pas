unit cairo_script;

interface

uses
  cairo;

type
  Pcairo_script_mode_t = ^Tcairo_script_mode_t;
  Tcairo_script_mode_t = longint;

const
  CAIRO_SCRIPT_MODE_ASCII = 0;
  CAIRO_SCRIPT_MODE_BINARY = 1;

function cairo_script_create(filename: PChar): Pcairo_device_t; cdecl; external cairo_lib;
function cairo_script_create_for_stream(write_func: Tcairo_write_func_t; closure: pointer): Pcairo_device_t; cdecl; external cairo_lib;
procedure cairo_script_write_comment(script: Pcairo_device_t; comment: PChar; len: longint); cdecl; external cairo_lib;
procedure cairo_script_set_mode(script: Pcairo_device_t; mode: Tcairo_script_mode_t); cdecl; external cairo_lib;
function cairo_script_get_mode(script: Pcairo_device_t): Tcairo_script_mode_t; cdecl; external cairo_lib;
function cairo_script_surface_create(script: Pcairo_device_t; content: Tcairo_content_t; Width: Tdouble; Height: Tdouble): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_script_surface_create_for_target(script: Pcairo_device_t; target: Pcairo_surface_t): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_script_from_recording_surface(script: Pcairo_device_t; recording_surface: Pcairo_surface_t): Tcairo_status_t; cdecl; external cairo_lib;

implementation

end.

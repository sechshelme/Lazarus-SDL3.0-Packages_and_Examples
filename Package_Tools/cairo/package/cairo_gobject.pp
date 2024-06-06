unit cairo_gobject;

interface

uses
  ctypes, cairo;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  gulong = culong;
  GType = gulong;
  TGType = GType;

// {$include <glib-object.h>}
function CAIRO_GOBJECT_TYPE_CONTEXT: TGType;

function cairo_gobject_context_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_DEVICE: TGType;

function cairo_gobject_device_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_MATRIX: TGType;

function cairo_gobject_matrix_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_PATTERN: TGType;

function cairo_gobject_pattern_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_SURFACE: TGType;

function cairo_gobject_surface_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_RECTANGLE: TGType;

function cairo_gobject_rectangle_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_SCALED_FONT: TGType;

function cairo_gobject_scaled_font_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_FONT_FACE: TGType;

function cairo_gobject_font_face_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_FONT_OPTIONS: TGType;

function cairo_gobject_font_options_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_RECTANGLE_INT: TGType;

function cairo_gobject_rectangle_int_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_REGION: TGType;

function cairo_gobject_region_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_GLYPH: TGType;

function cairo_gobject_glyph_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TEXT_CLUSTER_GLYPH: TGType;

function cairo_gobject_text_cluster_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_STATUS: TGType;

function cairo_gobject_status_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_CONTENT: TGType;

function cairo_gobject_content_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_OPERATOR: TGType;

function cairo_gobject_operator_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_ANTIALIAS: TGType;

function cairo_gobject_antialias_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_FILL_RULE: TGType;

function cairo_gobject_fill_rule_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_LINE_CAP: TGType;

function cairo_gobject_line_cap_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_LINE_JOIN: TGType;

function cairo_gobject_line_join_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_TEXT_CLUSTER_FLAGS: TGType;

function cairo_gobject_text_cluster_flags_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_FONT_SLANT: TGType;

function cairo_gobject_font_slant_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_FONT_WEIGHT: TGType;

function cairo_gobject_font_weight_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_SUBPIXEL_ORDER: TGType;

function cairo_gobject_subpixel_order_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_HINT_STYLE: TGType;

function cairo_gobject_hint_style_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_HNT_METRICS: TGType;
function CAIRO_GOBJECT_TYPE_HINT_METRICS: TGType;

function cairo_gobject_hint_metrics_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_FONT_TYPE: TGType;

function cairo_gobject_font_type_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_PATH_DATA_TYPE: TGType;

function cairo_gobject_path_data_type_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_DEVICE_TYPE: TGType;

function cairo_gobject_device_type_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_SURFACE_TYPE: TGType;

function cairo_gobject_surface_type_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_FORMAT: TGType;

function cairo_gobject_format_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_PATTERN_TYPE: TGType;

function cairo_gobject_pattern_type_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_EXTEND: TGType;

function cairo_gobject_extend_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_FILTER: TGType;

function cairo_gobject_filter_get_type: TGType; cdecl; external cairo_lib;
function CAIRO_GOBJECT_TYPE_REGION_OVERLAP: TGType;

function cairo_gobject_region_overlap_get_type: TGType; cdecl; external cairo_lib;

implementation


function CAIRO_GOBJECT_TYPE_CONTEXT: TGType;
begin
  CAIRO_GOBJECT_TYPE_CONTEXT := cairo_gobject_context_get_type;
end;


function CAIRO_GOBJECT_TYPE_DEVICE: TGType;
begin
  CAIRO_GOBJECT_TYPE_DEVICE := cairo_gobject_device_get_type;
end;


function CAIRO_GOBJECT_TYPE_MATRIX: TGType;
begin
  CAIRO_GOBJECT_TYPE_MATRIX := cairo_gobject_matrix_get_type;
end;


function CAIRO_GOBJECT_TYPE_PATTERN: TGType;
begin
  CAIRO_GOBJECT_TYPE_PATTERN := cairo_gobject_pattern_get_type;
end;


function CAIRO_GOBJECT_TYPE_SURFACE: TGType;
begin
  CAIRO_GOBJECT_TYPE_SURFACE := cairo_gobject_surface_get_type;
end;


function CAIRO_GOBJECT_TYPE_RECTANGLE: TGType;
begin
  CAIRO_GOBJECT_TYPE_RECTANGLE := cairo_gobject_rectangle_get_type;
end;


function CAIRO_GOBJECT_TYPE_SCALED_FONT: TGType;
begin
  CAIRO_GOBJECT_TYPE_SCALED_FONT := cairo_gobject_scaled_font_get_type;
end;


function CAIRO_GOBJECT_TYPE_FONT_FACE: TGType;
begin
  CAIRO_GOBJECT_TYPE_FONT_FACE := cairo_gobject_font_face_get_type;
end;


function CAIRO_GOBJECT_TYPE_FONT_OPTIONS: TGType;
begin
  CAIRO_GOBJECT_TYPE_FONT_OPTIONS := cairo_gobject_font_options_get_type;
end;


function CAIRO_GOBJECT_TYPE_RECTANGLE_INT: TGType;
begin
  CAIRO_GOBJECT_TYPE_RECTANGLE_INT := cairo_gobject_rectangle_int_get_type;
end;


function CAIRO_GOBJECT_TYPE_REGION: TGType;
begin
  CAIRO_GOBJECT_TYPE_REGION := cairo_gobject_region_get_type;
end;


function CAIRO_GOBJECT_TYPE_GLYPH: TGType;
begin
  CAIRO_GOBJECT_TYPE_GLYPH := cairo_gobject_glyph_get_type;
end;


function CAIRO_GOBJECT_TEXT_CLUSTER_GLYPH: TGType;
begin
  CAIRO_GOBJECT_TEXT_CLUSTER_GLYPH := cairo_gobject_text_cluster_get_type;
end;


function CAIRO_GOBJECT_TYPE_STATUS: TGType;
begin
  CAIRO_GOBJECT_TYPE_STATUS := cairo_gobject_status_get_type;
end;


function CAIRO_GOBJECT_TYPE_CONTENT: TGType;
begin
  CAIRO_GOBJECT_TYPE_CONTENT := cairo_gobject_content_get_type;
end;


function CAIRO_GOBJECT_TYPE_OPERATOR: TGType;
begin
  CAIRO_GOBJECT_TYPE_OPERATOR := cairo_gobject_operator_get_type;
end;


function CAIRO_GOBJECT_TYPE_ANTIALIAS: TGType;
begin
  CAIRO_GOBJECT_TYPE_ANTIALIAS := cairo_gobject_antialias_get_type;
end;


function CAIRO_GOBJECT_TYPE_FILL_RULE: TGType;
begin
  CAIRO_GOBJECT_TYPE_FILL_RULE := cairo_gobject_fill_rule_get_type;
end;


function CAIRO_GOBJECT_TYPE_LINE_CAP: TGType;
begin
  CAIRO_GOBJECT_TYPE_LINE_CAP := cairo_gobject_line_cap_get_type;
end;


function CAIRO_GOBJECT_TYPE_LINE_JOIN: TGType;
begin
  CAIRO_GOBJECT_TYPE_LINE_JOIN := cairo_gobject_line_join_get_type;
end;


function CAIRO_GOBJECT_TYPE_TEXT_CLUSTER_FLAGS: TGType;
begin
  CAIRO_GOBJECT_TYPE_TEXT_CLUSTER_FLAGS := cairo_gobject_text_cluster_flags_get_type;
end;


function CAIRO_GOBJECT_TYPE_FONT_SLANT: TGType;
begin
  CAIRO_GOBJECT_TYPE_FONT_SLANT := cairo_gobject_font_slant_get_type;
end;


function CAIRO_GOBJECT_TYPE_FONT_WEIGHT: TGType;
begin
  CAIRO_GOBJECT_TYPE_FONT_WEIGHT := cairo_gobject_font_weight_get_type;
end;


function CAIRO_GOBJECT_TYPE_SUBPIXEL_ORDER: TGType;
begin
  CAIRO_GOBJECT_TYPE_SUBPIXEL_ORDER := cairo_gobject_subpixel_order_get_type;
end;


function CAIRO_GOBJECT_TYPE_HINT_STYLE: TGType;
begin
  CAIRO_GOBJECT_TYPE_HINT_STYLE := cairo_gobject_hint_style_get_type;
end;


function CAIRO_GOBJECT_TYPE_HNT_METRICS: TGType;
begin
  CAIRO_GOBJECT_TYPE_HNT_METRICS := cairo_gobject_hint_metrics_get_type;
end;


function CAIRO_GOBJECT_TYPE_HINT_METRICS: TGType;
begin
  CAIRO_GOBJECT_TYPE_HINT_METRICS := cairo_gobject_hint_metrics_get_type;
end;


function CAIRO_GOBJECT_TYPE_FONT_TYPE: TGType;
begin
  CAIRO_GOBJECT_TYPE_FONT_TYPE := cairo_gobject_font_type_get_type;
end;


function CAIRO_GOBJECT_TYPE_PATH_DATA_TYPE: TGType;
begin
  CAIRO_GOBJECT_TYPE_PATH_DATA_TYPE := cairo_gobject_path_data_type_get_type;
end;


function CAIRO_GOBJECT_TYPE_DEVICE_TYPE: TGType;
begin
  CAIRO_GOBJECT_TYPE_DEVICE_TYPE := cairo_gobject_device_type_get_type;
end;


function CAIRO_GOBJECT_TYPE_SURFACE_TYPE: TGType;
begin
  CAIRO_GOBJECT_TYPE_SURFACE_TYPE := cairo_gobject_surface_type_get_type;
end;


function CAIRO_GOBJECT_TYPE_FORMAT: TGType;
begin
  CAIRO_GOBJECT_TYPE_FORMAT := cairo_gobject_format_get_type;
end;


function CAIRO_GOBJECT_TYPE_PATTERN_TYPE: TGType;
begin
  CAIRO_GOBJECT_TYPE_PATTERN_TYPE := cairo_gobject_pattern_type_get_type;
end;


function CAIRO_GOBJECT_TYPE_EXTEND: TGType;
begin
  CAIRO_GOBJECT_TYPE_EXTEND := cairo_gobject_extend_get_type;
end;


function CAIRO_GOBJECT_TYPE_FILTER: TGType;
begin
  CAIRO_GOBJECT_TYPE_FILTER := cairo_gobject_filter_get_type;
end;


function CAIRO_GOBJECT_TYPE_REGION_OVERLAP: TGType;
begin
  CAIRO_GOBJECT_TYPE_REGION_OVERLAP := cairo_gobject_region_overlap_get_type;
end;


end.

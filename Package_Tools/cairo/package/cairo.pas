unit cairo;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
  {$IFDEF Linux}
  cairo_lib = 'libcairo.so.2';
  {$ENDIF}

  {$IFDEF Windows}
  cairo_lib = 'libcairo-2.dll';
  {$ENDIF}

  // === Eigenes
type
  Tdouble = double;
  Tcairo_t = Pointer;
  Pcairo_t = ^Tcairo_t;
  Tcairo_surface_t = Pointer;
  Pcairo_surface_t = ^Tcairo_surface_t;
  PPcairo_surface_t = ^Pcairo_surface_t;
  Tcairo_device_t = Pointer;
  Pcairo_device_t = ^Tcairo_device_t;
  Tcairo_pattern_t = Pointer;
  Pcairo_pattern_t = ^Tcairo_pattern_t;
  Tcairo_scaled_font_t = Pointer;
  Pcairo_scaled_font_t = ^Tcairo_scaled_font_t;
  Tcairo_font_face_t = Pointer;
  Pcairo_font_face_t = ^Tcairo_font_face_t;
  Tcairo_font_options_t = Pointer;
  Pcairo_font_options_t = ^Tcairo_font_options_t;
  Tcairo_region_t = Pointer;
  Pcairo_region_t = ^Tcairo_region_t;

  // === cairo-version.h
const
  CAIRO_VERSION_MAJOR = 1;
  CAIRO_VERSION_MINOR = 18;
  CAIRO_VERSION_MICRO = 1;

  // === cairo-features.h
const
  CAIRO_HAS_FC_FONT = 1;
  CAIRO_HAS_FT_FONT = 1;
  CAIRO_HAS_GOBJECT_FUNCTIONS = 1;
  CAIRO_HAS_IMAGE_SURFACE = 1;
  CAIRO_HAS_MIME_SURFACE = 1;
  CAIRO_HAS_OBSERVER_SURFACE = 1;
  CAIRO_HAS_PDF_SURFACE = 1;
  CAIRO_HAS_PNG_FUNCTIONS = 1;
  CAIRO_HAS_PS_SURFACE = 1;
  CAIRO_HAS_RECORDING_SURFACE = 1;
  CAIRO_HAS_SCRIPT_SURFACE = 1;
  CAIRO_HAS_SVG_SURFACE = 1;
  CAIRO_HAS_TEE_SURFACE = 1;
  CAIRO_HAS_USER_FONT = 1;
  CAIRO_HAS_XCB_SHM_FUNCTIONS = 1;
  CAIRO_HAS_XCB_SURFACE = 1;
  CAIRO_HAS_XLIB_SURFACE = 1;
  CAIRO_HAS_XLIB_XRENDER_SURFACE = 1;

  // ===

function CAIRO_VERSION_ENCODE(major, minor, micro: longint): longint;

function cairo_version: longint; cdecl; external cairo_lib;
function cairo_version_string: PChar; cdecl; external cairo_lib;

type
  Pcairo_bool_t = ^Tcairo_bool_t;
  Tcairo_bool_t = longint;
  Tcairo = Tcairo_t;
  Tcairo_surface = Tcairo_surface_t;
  Tcairo_device = Tcairo_device_t;

  Pcairo_matrix = ^Tcairo_matrix;

  Tcairo_matrix = record
    xx: Tdouble;
    yx: Tdouble;
    xy: Tdouble;
    yy: Tdouble;
    x0: Tdouble;
    y0: Tdouble;
  end;
  Tcairo_matrix_t = Tcairo_matrix;
  Pcairo_matrix_t = ^Tcairo_matrix_t;
  Tcairo_pattern = Tcairo_pattern_t;
  Tcairo_destroy_func_t = procedure(Data: pointer); cdecl;
  Pcairo_user_data_key = ^Tcairo_user_data_key;

  Tcairo_user_data_key = record
    unused: longint;
  end;
  Tcairo_user_data_key_t = Tcairo_user_data_key;
  Pcairo_user_data_key_t = ^Tcairo_user_data_key_t;
  Pcairo_status = ^Tcairo_status;
  Tcairo_status = longint;

const
  CAIRO_STATUS_SUCCESS = 0;
  CAIRO_STATUS_NO_MEMORY = 1;
  CAIRO_STATUS_INVALID_RESTORE = 2;
  CAIRO_STATUS_INVALID_POP_GROUP = 3;
  CAIRO_STATUS_NO_CURRENT_POINT = 4;
  CAIRO_STATUS_INVALID_MATRIX = 5;
  CAIRO_STATUS_INVALID_STATUS = 6;
  CAIRO_STATUS_NULL_POINTER = 7;
  CAIRO_STATUS_INVALID_STRING = 8;
  CAIRO_STATUS_INVALID_PATH_DATA = 9;
  CAIRO_STATUS_READ_ERROR = 10;
  CAIRO_STATUS_WRITE_ERROR = 11;
  CAIRO_STATUS_SURFACE_FINISHED = 12;
  CAIRO_STATUS_SURFACE_TYPE_MISMATCH = 13;
  CAIRO_STATUS_PATTERN_TYPE_MISMATCH = 14;
  CAIRO_STATUS_INVALID_CONTENT = 15;
  CAIRO_STATUS_INVALID_FORMAT = 16;
  CAIRO_STATUS_INVALID_VISUAL = 17;
  CAIRO_STATUS_FILE_NOT_FOUND = 18;
  CAIRO_STATUS_INVALID_DASH = 19;
  CAIRO_STATUS_INVALID_DSC_COMMENT = 20;
  CAIRO_STATUS_INVALID_INDEX = 21;
  CAIRO_STATUS_CLIP_NOT_REPRESENTABLE = 22;
  CAIRO_STATUS_TEMP_FILE_ERROR = 23;
  CAIRO_STATUS_INVALID_STRIDE = 24;
  CAIRO_STATUS_FONT_TYPE_MISMATCH = 25;
  CAIRO_STATUS_USER_FONT_IMMUTABLE = 26;
  CAIRO_STATUS_USER_FONT_ERROR = 27;
  CAIRO_STATUS_NEGATIVE_COUNT = 28;
  CAIRO_STATUS_INVALID_CLUSTERS = 29;
  CAIRO_STATUS_INVALID_SLANT = 30;
  CAIRO_STATUS_INVALID_WEIGHT = 31;
  CAIRO_STATUS_INVALID_SIZE = 32;
  CAIRO_STATUS_USER_FONT_NOT_IMPLEMENTED = 33;
  CAIRO_STATUS_DEVICE_TYPE_MISMATCH = 34;
  CAIRO_STATUS_DEVICE_ERROR = 35;
  CAIRO_STATUS_INVALID_MESH_CONSTRUCTION = 36;
  CAIRO_STATUS_DEVICE_FINISHED = 37;
  CAIRO_STATUS_JBIG2_GLOBAL_MISSING = 38;
  CAIRO_STATUS_PNG_ERROR = 39;
  CAIRO_STATUS_FREETYPE_ERROR = 40;
  CAIRO_STATUS_WIN32_GDI_ERROR = 41;
  CAIRO_STATUS_TAG_ERROR = 42;
  CAIRO_STATUS_DWRITE_ERROR = 43;
  CAIRO_STATUS_SVG_FONT_ERROR = 44;
  CAIRO_STATUS_LAST_STATUS = 45;

type
  Tcairo_status_t = Tcairo_status;
  Pcairo_status_t = ^Tcairo_status_t;

type
  Pcairo_content = ^Tcairo_content;
  Tcairo_content = longint;

const
  CAIRO_CONTENT_COLOR = $1000;
  CAIRO_CONTENT_ALPHA = $2000;
  CAIRO_CONTENT_COLOR_ALPHA = $3000;

type
  Tcairo_content_t = Tcairo_content;
  Pcairo_content_t = ^Tcairo_content_t;

type
  Pcairo_format = ^Tcairo_format;
  Tcairo_format = longint;

const
  CAIRO_FORMAT_INVALID = -(1);
  CAIRO_FORMAT_ARGB32 = 0;
  CAIRO_FORMAT_RGB24 = 1;
  CAIRO_FORMAT_A8 = 2;
  CAIRO_FORMAT_A1 = 3;
  CAIRO_FORMAT_RGB16_565 = 4;
  CAIRO_FORMAT_RGB30 = 5;
  CAIRO_FORMAT_RGB96F = 6;
  CAIRO_FORMAT_RGBA128F = 7;

type
  Tcairo_format_t = Tcairo_format;
  Pcairo_format_t = ^Tcairo_format_t;

type
  Pcairo_dither = ^Tcairo_dither;
  Tcairo_dither = longint;

const
  CAIRO_DITHER_NONE = 0;
  CAIRO_DITHER_DEFAULT = 1;
  CAIRO_DITHER_FAST = 2;
  CAIRO_DITHER_GOOD = 3;
  CAIRO_DITHER_BEST = 4;

type
  Tcairo_dither_t = Tcairo_dither;
  Pcairo_dither_t = ^Tcairo_dither_t;

procedure cairo_pattern_set_dither(pattern: Pcairo_pattern_t; dither: Tcairo_dither_t); cdecl; external cairo_lib;
function cairo_pattern_get_dither(pattern: Pcairo_pattern_t): Tcairo_dither_t; cdecl; external cairo_lib;

type
  Tcairo_write_func_t = function(closure: pointer; Data: pbyte; length: dword): Tcairo_status_t; cdecl;
  Tcairo_read_func_t = function(closure: pointer; Data: pbyte; length: dword): Tcairo_status_t; cdecl;
  Pcairo_rectangle_int = ^Tcairo_rectangle_int;

  Tcairo_rectangle_int = record
    x: longint;
    y: longint;
    Width: longint;
    Height: longint;
  end;
  Tcairo_rectangle_int_t = Tcairo_rectangle_int;
  Pcairo_rectangle_int_t = ^Tcairo_rectangle_int_t;

function cairo_create(target: Pcairo_surface_t): Pcairo_t; cdecl; external cairo_lib;
function cairo_reference(cr: Pcairo_t): Pcairo_t; cdecl; external cairo_lib;
procedure cairo_destroy(cr: Pcairo_t); cdecl; external cairo_lib;
function cairo_get_reference_count(cr: Pcairo_t): dword; cdecl; external cairo_lib;
function cairo_get_user_data(cr: Pcairo_t; key: Pcairo_user_data_key_t): pointer; cdecl; external cairo_lib;
function cairo_set_user_data(cr: Pcairo_t; key: Pcairo_user_data_key_t; user_data: pointer; Destroy: Tcairo_destroy_func_t): Tcairo_status_t; cdecl; external cairo_lib;
procedure cairo_save(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_restore(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_push_group(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_push_group_with_content(cr: Pcairo_t; content: Tcairo_content_t); cdecl; external cairo_lib;
function cairo_pop_group(cr: Pcairo_t): Pcairo_pattern_t; cdecl; external cairo_lib;
procedure cairo_pop_group_to_source(cr: Pcairo_t); cdecl; external cairo_lib;

type
  Pcairo_operator = ^Tcairo_operator;
  Tcairo_operator = longint;

const
  CAIRO_OPERATOR_CLEAR = 0;
  CAIRO_OPERATOR_SOURCE = 1;
  CAIRO_OPERATOR_OVER = 2;
  CAIRO_OPERATOR_IN = 3;
  CAIRO_OPERATOR_OUT = 4;
  CAIRO_OPERATOR_ATOP = 5;
  CAIRO_OPERATOR_DEST = 6;
  CAIRO_OPERATOR_DEST_OVER = 7;
  CAIRO_OPERATOR_DEST_IN = 8;
  CAIRO_OPERATOR_DEST_OUT = 9;
  CAIRO_OPERATOR_DEST_ATOP = 10;
  CAIRO_OPERATOR_XOR = 11;
  CAIRO_OPERATOR_ADD = 12;
  CAIRO_OPERATOR_SATURATE = 13;
  CAIRO_OPERATOR_MULTIPLY = 14;
  CAIRO_OPERATOR_SCREEN = 15;
  CAIRO_OPERATOR_OVERLAY = 16;
  CAIRO_OPERATOR_DARKEN = 17;
  CAIRO_OPERATOR_LIGHTEN = 18;
  CAIRO_OPERATOR_COLOR_DODGE = 19;
  CAIRO_OPERATOR_COLOR_BURN = 20;
  CAIRO_OPERATOR_HARD_LIGHT = 21;
  CAIRO_OPERATOR_SOFT_LIGHT = 22;
  CAIRO_OPERATOR_DIFFERENCE = 23;
  CAIRO_OPERATOR_EXCLUSION = 24;
  CAIRO_OPERATOR_HSL_HUE = 25;
  CAIRO_OPERATOR_HSL_SATURATION = 26;
  CAIRO_OPERATOR_HSL_COLOR = 27;
  CAIRO_OPERATOR_HSL_LUMINOSITY = 28;

type
  Tcairo_operator_t = Tcairo_operator;
  Pcairo_operator_t = ^Tcairo_operator_t;

procedure cairo_set_operator(cr: Pcairo_t; op: Tcairo_operator_t); cdecl; external cairo_lib;
procedure cairo_set_source(cr: Pcairo_t; Source: Pcairo_pattern_t); cdecl; external cairo_lib;
procedure cairo_set_source_rgb(cr: Pcairo_t; red: Tdouble; green: Tdouble; blue: Tdouble); cdecl; external cairo_lib;
procedure cairo_set_source_rgba(cr: Pcairo_t; red: Tdouble; green: Tdouble; blue: Tdouble; alpha: Tdouble); cdecl; external cairo_lib;
procedure cairo_set_source_surface(cr: Pcairo_t; surface: Pcairo_surface_t; x: Tdouble; y: Tdouble); cdecl; external cairo_lib;
procedure cairo_set_tolerance(cr: Pcairo_t; tolerance: Tdouble); cdecl; external cairo_lib;

type
  Pcairo_antialias = ^Tcairo_antialias;
  Tcairo_antialias = longint;

const
  CAIRO_ANTIALIAS_DEFAULT = 0;
  CAIRO_ANTIALIAS_NONE = 1;
  CAIRO_ANTIALIAS_GRAY = 2;
  CAIRO_ANTIALIAS_SUBPIXEL = 3;
  CAIRO_ANTIALIAS_FAST = 4;
  CAIRO_ANTIALIAS_GOOD = 5;
  CAIRO_ANTIALIAS_BEST = 6;

type
  Tcairo_antialias_t = Tcairo_antialias;
  Pcairo_antialias_t = ^Tcairo_antialias_t;

procedure cairo_set_antialias(cr: Pcairo_t; antialias: Tcairo_antialias_t); cdecl; external cairo_lib;

type
  Pcairo_fill_rule = ^Tcairo_fill_rule;
  Tcairo_fill_rule = longint;

const
  CAIRO_FILL_RULE_WINDING = 0;
  CAIRO_FILL_RULE_EVEN_ODD = 1;

type
  Tcairo_fill_rule_t = Tcairo_fill_rule;
  Pcairo_fill_rule_t = ^Tcairo_fill_rule_t;

procedure cairo_set_fill_rule(cr: Pcairo_t; fill_rule: Tcairo_fill_rule_t); cdecl; external cairo_lib;
procedure cairo_set_line_width(cr: Pcairo_t; Width: Tdouble); cdecl; external cairo_lib;
procedure cairo_set_hairline(cr: Pcairo_t; set_hairline: Tcairo_bool_t); cdecl; external cairo_lib;

type
  Pcairo_line_cap = ^Tcairo_line_cap;
  Tcairo_line_cap = longint;

const
  CAIRO_LINE_CAP_BUTT = 0;
  CAIRO_LINE_CAP_ROUND = 1;
  CAIRO_LINE_CAP_SQUARE = 2;

type
  Tcairo_line_cap_t = Tcairo_line_cap;
  Pcairo_line_cap_t = ^Tcairo_line_cap_t;

procedure cairo_set_line_cap(cr: Pcairo_t; line_cap: Tcairo_line_cap_t); cdecl; external cairo_lib;

type
  Pcairo_line_join = ^Tcairo_line_join;
  Tcairo_line_join = longint;

const
  CAIRO_LINE_JOIN_MITER = 0;
  CAIRO_LINE_JOIN_ROUND = 1;
  CAIRO_LINE_JOIN_BEVEL = 2;

type
  Tcairo_line_join_t = Tcairo_line_join;
  Pcairo_line_join_t = ^Tcairo_line_join_t;

procedure cairo_set_line_join(cr: Pcairo_t; line_join: Tcairo_line_join_t); cdecl; external cairo_lib;
procedure cairo_set_dash(cr: Pcairo_t; dashes: Pdouble; num_dashes: longint; offset: Tdouble); cdecl; external cairo_lib;
procedure cairo_set_miter_limit(cr: Pcairo_t; limit: Tdouble); cdecl; external cairo_lib;
procedure cairo_translate(cr: Pcairo_t; tx: Tdouble; ty: Tdouble); cdecl; external cairo_lib;
procedure cairo_scale(cr: Pcairo_t; sx: Tdouble; sy: Tdouble); cdecl; external cairo_lib;
procedure cairo_rotate(cr: Pcairo_t; angle: Tdouble); cdecl; external cairo_lib;
procedure cairo_transform(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_set_matrix(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_identity_matrix(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_user_to_device(cr: Pcairo_t; x: Pdouble; y: Pdouble); cdecl; external cairo_lib;
procedure cairo_user_to_device_distance(cr: Pcairo_t; dx: Pdouble; dy: Pdouble); cdecl; external cairo_lib;
procedure cairo_device_to_user(cr: Pcairo_t; x: Pdouble; y: Pdouble); cdecl; external cairo_lib;
procedure cairo_device_to_user_distance(cr: Pcairo_t; dx: Pdouble; dy: Pdouble); cdecl; external cairo_lib;
procedure cairo_new_path(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_move_to(cr: Pcairo_t; x: Tdouble; y: Tdouble); cdecl; external cairo_lib;
procedure cairo_new_sub_path(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_line_to(cr: Pcairo_t; x: Tdouble; y: Tdouble); cdecl; external cairo_lib;
procedure cairo_curve_to(cr: Pcairo_t; x1: Tdouble; y1: Tdouble; x2: Tdouble; y2: Tdouble; x3: Tdouble; y3: Tdouble); cdecl; external cairo_lib;
procedure cairo_arc(cr: Pcairo_t; xc: Tdouble; yc: Tdouble; radius: Tdouble; angle1: Tdouble; angle2: Tdouble); cdecl; external cairo_lib;
procedure cairo_arc_negative(cr: Pcairo_t; xc: Tdouble; yc: Tdouble; radius: Tdouble; angle1: Tdouble; angle2: Tdouble); cdecl; external cairo_lib;
procedure cairo_rel_move_to(cr: Pcairo_t; dx: Tdouble; dy: Tdouble); cdecl; external cairo_lib;
procedure cairo_rel_line_to(cr: Pcairo_t; dx: Tdouble; dy: Tdouble); cdecl; external cairo_lib;
procedure cairo_rel_curve_to(cr: Pcairo_t; dx1: Tdouble; dy1: Tdouble; dx2: Tdouble; dy2: Tdouble;
  dx3: Tdouble; dy3: Tdouble); cdecl; external cairo_lib;
procedure cairo_rectangle(cr: Pcairo_t; x: Tdouble; y: Tdouble; Width: Tdouble; Height: Tdouble); cdecl; external cairo_lib;
procedure cairo_close_path(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_path_extents(cr: Pcairo_t; x1: Pdouble; y1: Pdouble; x2: Pdouble; y2: Pdouble); cdecl; external cairo_lib;
procedure cairo_paint(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_paint_with_alpha(cr: Pcairo_t; alpha: Tdouble); cdecl; external cairo_lib;
procedure cairo_mask(cr: Pcairo_t; pattern: Pcairo_pattern_t); cdecl; external cairo_lib;
procedure cairo_mask_surface(cr: Pcairo_t; surface: Pcairo_surface_t; surface_x: Tdouble; surface_y: Tdouble); cdecl; external cairo_lib;
procedure cairo_stroke(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_stroke_preserve(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_fill(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_fill_preserve(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_copy_page(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_show_page(cr: Pcairo_t); cdecl; external cairo_lib;
function cairo_in_stroke(cr: Pcairo_t; x: Tdouble; y: Tdouble): Tcairo_bool_t; cdecl; external cairo_lib;
function cairo_in_fill(cr: Pcairo_t; x: Tdouble; y: Tdouble): Tcairo_bool_t; cdecl; external cairo_lib;
function cairo_in_clip(cr: Pcairo_t; x: Tdouble; y: Tdouble): Tcairo_bool_t; cdecl; external cairo_lib;
procedure cairo_stroke_extents(cr: Pcairo_t; x1: Pdouble; y1: Pdouble; x2: Pdouble; y2: Pdouble); cdecl; external cairo_lib;
procedure cairo_fill_extents(cr: Pcairo_t; x1: Pdouble; y1: Pdouble; x2: Pdouble; y2: Pdouble); cdecl; external cairo_lib;
procedure cairo_reset_clip(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_clip(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_clip_preserve(cr: Pcairo_t); cdecl; external cairo_lib;
procedure cairo_clip_extents(cr: Pcairo_t; x1: Pdouble; y1: Pdouble; x2: Pdouble; y2: Pdouble); cdecl; external cairo_lib;

type
  Pcairo_rectangle = ^Tcairo_rectangle;

  Tcairo_rectangle = record
    x: Tdouble;
    y: Tdouble;
    Width: Tdouble;
    Height: Tdouble;
  end;
  Tcairo_rectangle_t = Tcairo_rectangle;
  Pcairo_rectangle_t = ^Tcairo_rectangle_t;
  Pcairo_rectangle_list = ^Tcairo_rectangle_list;

  Tcairo_rectangle_list = record
    status: Tcairo_status_t;
    rectangles: Pcairo_rectangle_t;
    num_rectangles: longint;
  end;
  Tcairo_rectangle_list_t = Tcairo_rectangle_list;
  Pcairo_rectangle_list_t = ^Tcairo_rectangle_list_t;

function cairo_copy_clip_rectangle_list(cr: Pcairo_t): Pcairo_rectangle_list_t; cdecl; external cairo_lib;
procedure cairo_rectangle_list_destroy(rectangle_list: Pcairo_rectangle_list_t); cdecl; external cairo_lib;

const
  CAIRO_TAG_DEST = 'cairo.dest';
  CAIRO_TAG_LINK = 'Link';
  CAIRO_TAG_CONTENT = 'cairo.content';
  CAIRO_TAG_CONTENT_REF = 'cairo.content_ref';

procedure cairo_tag_begin(cr: Pcairo_t; tag_name: PChar; attributes: PChar); cdecl; external cairo_lib;
procedure cairo_tag_end(cr: Pcairo_t; tag_name: PChar); cdecl; external cairo_lib;

type
  Tcairo_scaled_font = Tcairo_scaled_font_t;
  Tcairo_font_face = Tcairo_font_face_t;

  PPcairo_glyph_t = ^Pcairo_glyph_t;
  Pcairo_glyph_t = ^Tcairo_glyph_t;

  Tcairo_glyph_t = record
    index: dword;
    x: Tdouble;
    y: Tdouble;
  end;

function cairo_glyph_allocate(num_glyphs: longint): Pcairo_glyph_t; cdecl; external cairo_lib;
procedure cairo_glyph_free(glyphs: Pcairo_glyph_t); cdecl; external cairo_lib;

type
  PPcairo_text_cluster_t = ^Pcairo_text_cluster_t;
  Pcairo_text_cluster_t = ^Tcairo_text_cluster_t;

  Tcairo_text_cluster_t = record
    num_bytes: longint;
    num_glyphs: longint;
  end;

function cairo_text_cluster_allocate(num_clusters: longint): Pcairo_text_cluster_t; cdecl; external cairo_lib;
procedure cairo_text_cluster_free(clusters: Pcairo_text_cluster_t); cdecl; external cairo_lib;

type
  Pcairo_text_cluster_flags = ^Tcairo_text_cluster_flags;
  Tcairo_text_cluster_flags = longint;

const
  CAIRO_TEXT_CLUSTER_FLAG_BACKWARD = $00000001;

type
  Tcairo_text_cluster_flags_t = Tcairo_text_cluster_flags;
  Pcairo_text_cluster_flags_t = ^Tcairo_text_cluster_flags_t;

type
  Pcairo_text_extents_t = ^Tcairo_text_extents_t;

  Tcairo_text_extents_t = record
    x_bearing: Tdouble;
    y_bearing: Tdouble;
    Width: Tdouble;
    Height: Tdouble;
    x_advance: Tdouble;
    y_advance: Tdouble;
  end;

  Pcairo_font_extents_t = ^Tcairo_font_extents_t;

  Tcairo_font_extents_t = record
    ascent: Tdouble;
    descent: Tdouble;
    Height: Tdouble;
    max_x_advance: Tdouble;
    max_y_advance: Tdouble;
  end;

  Pcairo_font_slant = ^Tcairo_font_slant;
  Tcairo_font_slant = longint;

const
  CAIRO_FONT_SLANT_NORMAL = 0;
  CAIRO_FONT_SLANT_ITALIC = 1;
  CAIRO_FONT_SLANT_OBLIQUE = 2;

type
  Tcairo_font_slant_t = Tcairo_font_slant;
  Pcairo_font_slant_t = ^Tcairo_font_slant_t;

type
  Pcairo_font_weight = ^Tcairo_font_weight;
  Tcairo_font_weight = longint;

const
  CAIRO_FONT_WEIGHT_NORMAL = 0;
  CAIRO_FONT_WEIGHT_BOLD = 1;

type
  Tcairo_font_weight_t = Tcairo_font_weight;
  Pcairo_font_weight_t = ^Tcairo_font_weight_t;

type
  Pcairo_subpixel_order = ^Tcairo_subpixel_order;
  Tcairo_subpixel_order = longint;

const
  CAIRO_SUBPIXEL_ORDER_DEFAULT = 0;
  CAIRO_SUBPIXEL_ORDER_RGB = 1;
  CAIRO_SUBPIXEL_ORDER_BGR = 2;
  CAIRO_SUBPIXEL_ORDER_VRGB = 3;
  CAIRO_SUBPIXEL_ORDER_VBGR = 4;

type
  Tcairo_subpixel_order_t = Tcairo_subpixel_order;
  Pcairo_subpixel_order_t = ^Tcairo_subpixel_order_t;

type
  Pcairo_hint_style = ^Tcairo_hint_style;
  Tcairo_hint_style = longint;

const
  CAIRO_HINT_STYLE_DEFAULT = 0;
  CAIRO_HINT_STYLE_NONE = 1;
  CAIRO_HINT_STYLE_SLIGHT = 2;
  CAIRO_HINT_STYLE_MEDIUM = 3;
  CAIRO_HINT_STYLE_FULL = 4;

type
  Tcairo_hint_style_t = Tcairo_hint_style;
  Pcairo_hint_style_t = ^Tcairo_hint_style_t;

type
  Pcairo_hint_metrics = ^Tcairo_hint_metrics;
  Tcairo_hint_metrics = longint;

const
  CAIRO_HINT_METRICS_DEFAULT = 0;
  CAIRO_HINT_METRICS_OFF = 1;
  CAIRO_HINT_METRICS_ON = 2;

type
  Tcairo_hint_metrics_t = Tcairo_hint_metrics;
  Pcairo_hint_metrics_t = ^Tcairo_hint_metrics_t;

type
  Pcairo_color_mode = ^Tcairo_color_mode;
  Tcairo_color_mode = longint;

const
  CAIRO_COLOR_MODE_DEFAULT = 0;
  CAIRO_COLOR_MODE_NO_COLOR = 1;
  CAIRO_COLOR_MODE_COLOR = 2;

type
  Tcairo_color_mode_t = Tcairo_color_mode;
  Pcairo_color_mode_t = ^Tcairo_color_mode_t;

type
  Tcairo_font_options = Tcairo_font_options_t;

function cairo_font_options_create: Pcairo_font_options_t; cdecl; external cairo_lib;
function cairo_font_options_copy(original: Pcairo_font_options_t): Pcairo_font_options_t; cdecl; external cairo_lib;
procedure cairo_font_options_destroy(options: Pcairo_font_options_t); cdecl; external cairo_lib;
function cairo_font_options_status(options: Pcairo_font_options_t): Tcairo_status_t; cdecl; external cairo_lib;
procedure cairo_font_options_merge(options: Pcairo_font_options_t; other: Pcairo_font_options_t); cdecl; external cairo_lib;
function cairo_font_options_equal(options: Pcairo_font_options_t; other: Pcairo_font_options_t): Tcairo_bool_t; cdecl; external cairo_lib;
function cairo_font_options_hash(options: Pcairo_font_options_t): dword; cdecl; external cairo_lib;
procedure cairo_font_options_set_antialias(options: Pcairo_font_options_t; antialias: Tcairo_antialias_t); cdecl; external cairo_lib;
function cairo_font_options_get_antialias(options: Pcairo_font_options_t): Tcairo_antialias_t; cdecl; external cairo_lib;
procedure cairo_font_options_set_subpixel_order(options: Pcairo_font_options_t; subpixel_order: Tcairo_subpixel_order_t); cdecl; external cairo_lib;
function cairo_font_options_get_subpixel_order(options: Pcairo_font_options_t): Tcairo_subpixel_order_t; cdecl; external cairo_lib;
procedure cairo_font_options_set_hint_style(options: Pcairo_font_options_t; hint_style: Tcairo_hint_style_t); cdecl; external cairo_lib;
function cairo_font_options_get_hint_style(options: Pcairo_font_options_t): Tcairo_hint_style_t; cdecl; external cairo_lib;
procedure cairo_font_options_set_hint_metrics(options: Pcairo_font_options_t; hint_metrics: Tcairo_hint_metrics_t); cdecl; external cairo_lib;
function cairo_font_options_get_hint_metrics(options: Pcairo_font_options_t): Tcairo_hint_metrics_t; cdecl; external cairo_lib;
function cairo_font_options_get_variations(options: Pcairo_font_options_t): PChar; cdecl; external cairo_lib;
procedure cairo_font_options_set_variations(options: Pcairo_font_options_t; variations: PChar); cdecl; external cairo_lib;

const
  CAIRO_COLOR_PALETTE_DEFAULT = 0;

procedure cairo_font_options_set_color_mode(options: Pcairo_font_options_t; color_mode: Tcairo_color_mode_t); cdecl; external cairo_lib;
function cairo_font_options_get_color_mode(options: Pcairo_font_options_t): Tcairo_color_mode_t; cdecl; external cairo_lib;
function cairo_font_options_get_color_palette(options: Pcairo_font_options_t): dword; cdecl; external cairo_lib;
procedure cairo_font_options_set_color_palette(options: Pcairo_font_options_t; palette_index: dword); cdecl; external cairo_lib;
procedure cairo_font_options_set_custom_palette_color(options: Pcairo_font_options_t; index: dword; red: Tdouble; green: Tdouble; blue: Tdouble;
  alpha: Tdouble); cdecl; external cairo_lib;
function cairo_font_options_get_custom_palette_color(options: Pcairo_font_options_t; index: dword; red: Pdouble; green: Pdouble; blue: Pdouble;
  alpha: Pdouble): Tcairo_status_t; cdecl; external cairo_lib;
procedure cairo_select_font_face(cr: Pcairo_t; family: PChar; slant: Tcairo_font_slant_t; weight: Tcairo_font_weight_t); cdecl; external cairo_lib;
procedure cairo_set_font_size(cr: Pcairo_t; size: Tdouble); cdecl; external cairo_lib;
procedure cairo_set_font_matrix(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_get_font_matrix(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_set_font_options(cr: Pcairo_t; options: Pcairo_font_options_t); cdecl; external cairo_lib;
procedure cairo_get_font_options(cr: Pcairo_t; options: Pcairo_font_options_t); cdecl; external cairo_lib;
procedure cairo_set_font_face(cr: Pcairo_t; font_face: Pcairo_font_face_t); cdecl; external cairo_lib;
function cairo_get_font_face(cr: Pcairo_t): Pcairo_font_face_t; cdecl; external cairo_lib;
procedure cairo_set_scaled_font(cr: Pcairo_t; scaled_font: Pcairo_scaled_font_t); cdecl; external cairo_lib;
function cairo_get_scaled_font(cr: Pcairo_t): Pcairo_scaled_font_t; cdecl; external cairo_lib;
procedure cairo_show_text(cr: Pcairo_t; utf8: PChar); cdecl; external cairo_lib;
procedure cairo_show_glyphs(cr: Pcairo_t; glyphs: Pcairo_glyph_t; num_glyphs: longint); cdecl; external cairo_lib;
procedure cairo_show_text_glyphs(cr: Pcairo_t; utf8: PChar; utf8_len: longint; glyphs: Pcairo_glyph_t; num_glyphs: longint;
  clusters: Pcairo_text_cluster_t; num_clusters: longint; cluster_flags: Tcairo_text_cluster_flags_t); cdecl; external cairo_lib;
procedure cairo_text_path(cr: Pcairo_t; utf8: PChar); cdecl; external cairo_lib;
procedure cairo_glyph_path(cr: Pcairo_t; glyphs: Pcairo_glyph_t; num_glyphs: longint); cdecl; external cairo_lib;
procedure cairo_text_extents(cr: Pcairo_t; utf8: PChar; extents: Pcairo_text_extents_t); cdecl; external cairo_lib;
procedure cairo_glyph_extents(cr: Pcairo_t; glyphs: Pcairo_glyph_t; num_glyphs: longint; extents: Pcairo_text_extents_t); cdecl; external cairo_lib;
procedure cairo_font_extents(cr: Pcairo_t; extents: Pcairo_font_extents_t); cdecl; external cairo_lib;
function cairo_font_face_reference(font_face: Pcairo_font_face_t): Pcairo_font_face_t; cdecl; external cairo_lib;
procedure cairo_font_face_destroy(font_face: Pcairo_font_face_t); cdecl; external cairo_lib;
function cairo_font_face_get_reference_count(font_face: Pcairo_font_face_t): dword; cdecl; external cairo_lib;
function cairo_font_face_status(font_face: Pcairo_font_face_t): Tcairo_status_t; cdecl; external cairo_lib;

type
  Pcairo_font_type = ^Tcairo_font_type;
  Tcairo_font_type = longint;

const
  CAIRO_FONT_TYPE_TOY = 0;
  CAIRO_FONT_TYPE_FT = 1;
  CAIRO_FONT_TYPE_WIN32 = 2;
  CAIRO_FONT_TYPE_QUARTZ = 3;
  CAIRO_FONT_TYPE_USER = 4;
  CAIRO_FONT_TYPE_DWRITE = 5;

type
  Tcairo_font_type_t = Tcairo_font_type;
  Pcairo_font_type_t = ^Tcairo_font_type_t;

function cairo_font_face_get_type(font_face: Pcairo_font_face_t): Tcairo_font_type_t; cdecl; external cairo_lib;
function cairo_font_face_get_user_data(font_face: Pcairo_font_face_t; key: Pcairo_user_data_key_t): pointer; cdecl; external cairo_lib;
function cairo_font_face_set_user_data(font_face: Pcairo_font_face_t; key: Pcairo_user_data_key_t; user_data: pointer; Destroy: Tcairo_destroy_func_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_scaled_font_create(font_face: Pcairo_font_face_t; font_matrix: Pcairo_matrix_t; ctm: Pcairo_matrix_t; options: Pcairo_font_options_t): Pcairo_scaled_font_t; cdecl; external cairo_lib;
function cairo_scaled_font_reference(scaled_font: Pcairo_scaled_font_t): Pcairo_scaled_font_t; cdecl; external cairo_lib;
procedure cairo_scaled_font_destroy(scaled_font: Pcairo_scaled_font_t); cdecl; external cairo_lib;
function cairo_scaled_font_get_reference_count(scaled_font: Pcairo_scaled_font_t): dword; cdecl; external cairo_lib;
function cairo_scaled_font_status(scaled_font: Pcairo_scaled_font_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_scaled_font_get_type(scaled_font: Pcairo_scaled_font_t): Tcairo_font_type_t; cdecl; external cairo_lib;
function cairo_scaled_font_get_user_data(scaled_font: Pcairo_scaled_font_t; key: Pcairo_user_data_key_t): pointer; cdecl; external cairo_lib;
function cairo_scaled_font_set_user_data(scaled_font: Pcairo_scaled_font_t; key: Pcairo_user_data_key_t; user_data: pointer; Destroy: Tcairo_destroy_func_t): Tcairo_status_t; cdecl; external cairo_lib;
procedure cairo_scaled_font_extents(scaled_font: Pcairo_scaled_font_t; extents: Pcairo_font_extents_t); cdecl; external cairo_lib;
procedure cairo_scaled_font_text_extents(scaled_font: Pcairo_scaled_font_t; utf8: PChar; extents: Pcairo_text_extents_t); cdecl; external cairo_lib;
procedure cairo_scaled_font_glyph_extents(scaled_font: Pcairo_scaled_font_t; glyphs: Pcairo_glyph_t; num_glyphs: longint; extents: Pcairo_text_extents_t); cdecl; external cairo_lib;
function cairo_scaled_font_text_to_glyphs(scaled_font: Pcairo_scaled_font_t; x: Tdouble; y: Tdouble; utf8: PChar; utf8_len: longint;
  glyphs: PPcairo_glyph_t; num_glyphs: Plongint; clusters: PPcairo_text_cluster_t; num_clusters: Plongint; cluster_flags: Pcairo_text_cluster_flags_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_scaled_font_get_font_face(scaled_font: Pcairo_scaled_font_t): Pcairo_font_face_t; cdecl; external cairo_lib;
procedure cairo_scaled_font_get_font_matrix(scaled_font: Pcairo_scaled_font_t; font_matrix: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_scaled_font_get_ctm(scaled_font: Pcairo_scaled_font_t; ctm: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_scaled_font_get_scale_matrix(scaled_font: Pcairo_scaled_font_t; scale_matrix: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_scaled_font_get_font_options(scaled_font: Pcairo_scaled_font_t; options: Pcairo_font_options_t); cdecl; external cairo_lib;
function cairo_toy_font_face_create(family: PChar; slant: Tcairo_font_slant_t; weight: Tcairo_font_weight_t): Pcairo_font_face_t; cdecl; external cairo_lib;
function cairo_toy_font_face_get_family(font_face: Pcairo_font_face_t): PChar; cdecl; external cairo_lib;
function cairo_toy_font_face_get_slant(font_face: Pcairo_font_face_t): Tcairo_font_slant_t; cdecl; external cairo_lib;
function cairo_toy_font_face_get_weight(font_face: Pcairo_font_face_t): Tcairo_font_weight_t; cdecl; external cairo_lib;
function cairo_user_font_face_create: Pcairo_font_face_t; cdecl; external cairo_lib;

type

  Tcairo_user_scaled_font_init_func_t = function(scaled_font: Pcairo_scaled_font_t; cr: Pcairo_t; extents: Pcairo_font_extents_t): Tcairo_status_t; cdecl;
  Tcairo_user_scaled_font_render_glyph_func_t = function(scaled_font: Pcairo_scaled_font_t; glyph: dword; cr: Pcairo_t; extents: Pcairo_text_extents_t): Tcairo_status_t; cdecl;
  Tcairo_user_scaled_font_text_to_glyphs_func_t = function(scaled_font: Pcairo_scaled_font_t; utf8: PChar; utf8_len: longint; glyphs: PPcairo_glyph_t; num_glyphs: Plongint;
    clusters: PPcairo_text_cluster_t; num_clusters: Plongint; cluster_flags: Pcairo_text_cluster_flags_t): Tcairo_status_t; cdecl;
  Tcairo_user_scaled_font_unicode_to_glyph_func_t = function(scaled_font: Pcairo_scaled_font_t; unicode: dword; glyph_index: Pdword): Tcairo_status_t; cdecl;

procedure cairo_user_font_face_set_init_func(font_face: Pcairo_font_face_t; init_func: Tcairo_user_scaled_font_init_func_t); cdecl; external cairo_lib;
procedure cairo_user_font_face_set_render_glyph_func(font_face: Pcairo_font_face_t; render_glyph_func: Tcairo_user_scaled_font_render_glyph_func_t); cdecl; external cairo_lib;
procedure cairo_user_font_face_set_render_color_glyph_func(font_face: Pcairo_font_face_t; render_glyph_func: Tcairo_user_scaled_font_render_glyph_func_t); cdecl; external cairo_lib;
procedure cairo_user_font_face_set_text_to_glyphs_func(font_face: Pcairo_font_face_t; text_to_glyphs_func: Tcairo_user_scaled_font_text_to_glyphs_func_t); cdecl; external cairo_lib;
procedure cairo_user_font_face_set_unicode_to_glyph_func(font_face: Pcairo_font_face_t; unicode_to_glyph_func: Tcairo_user_scaled_font_unicode_to_glyph_func_t); cdecl; external cairo_lib;
function cairo_user_font_face_get_init_func(font_face: Pcairo_font_face_t): Tcairo_user_scaled_font_init_func_t; cdecl; external cairo_lib;
function cairo_user_font_face_get_render_glyph_func(font_face: Pcairo_font_face_t): Tcairo_user_scaled_font_render_glyph_func_t; cdecl; external cairo_lib;
function cairo_user_font_face_get_render_color_glyph_func(font_face: Pcairo_font_face_t): Tcairo_user_scaled_font_render_glyph_func_t; cdecl; external cairo_lib;
function cairo_user_font_face_get_text_to_glyphs_func(font_face: Pcairo_font_face_t): Tcairo_user_scaled_font_text_to_glyphs_func_t; cdecl; external cairo_lib;
function cairo_user_font_face_get_unicode_to_glyph_func(font_face: Pcairo_font_face_t): Tcairo_user_scaled_font_unicode_to_glyph_func_t; cdecl; external cairo_lib;
function cairo_user_scaled_font_get_foreground_marker(scaled_font: Pcairo_scaled_font_t): Pcairo_pattern_t; cdecl; external cairo_lib;
function cairo_user_scaled_font_get_foreground_source(scaled_font: Pcairo_scaled_font_t): Pcairo_pattern_t; cdecl; external cairo_lib;
function cairo_get_operator(cr: Pcairo_t): Tcairo_operator_t; cdecl; external cairo_lib;
function cairo_get_source(cr: Pcairo_t): Pcairo_pattern_t; cdecl; external cairo_lib;
function cairo_get_tolerance(cr: Pcairo_t): Tdouble; cdecl; external cairo_lib;
function cairo_get_antialias(cr: Pcairo_t): Tcairo_antialias_t; cdecl; external cairo_lib;
function cairo_has_current_point(cr: Pcairo_t): Tcairo_bool_t; cdecl; external cairo_lib;
procedure cairo_get_current_point(cr: Pcairo_t; x: Pdouble; y: Pdouble); cdecl; external cairo_lib;
function cairo_get_fill_rule(cr: Pcairo_t): Tcairo_fill_rule_t; cdecl; external cairo_lib;
function cairo_get_line_width(cr: Pcairo_t): Tdouble; cdecl; external cairo_lib;
function cairo_get_hairline(cr: Pcairo_t): Tcairo_bool_t; cdecl; external cairo_lib;
function cairo_get_line_cap(cr: Pcairo_t): Tcairo_line_cap_t; cdecl; external cairo_lib;
function cairo_get_line_join(cr: Pcairo_t): Tcairo_line_join_t; cdecl; external cairo_lib;
function cairo_get_miter_limit(cr: Pcairo_t): Tdouble; cdecl; external cairo_lib;
function cairo_get_dash_count(cr: Pcairo_t): longint; cdecl; external cairo_lib;
procedure cairo_get_dash(cr: Pcairo_t; dashes: Pdouble; offset: Pdouble); cdecl; external cairo_lib;
procedure cairo_get_matrix(cr: Pcairo_t; matrix: Pcairo_matrix_t); cdecl; external cairo_lib;
function cairo_get_target(cr: Pcairo_t): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_get_group_target(cr: Pcairo_t): Pcairo_surface_t; cdecl; external cairo_lib;

type
  Pcairo_path_data_type = ^Tcairo_path_data_type;
  Tcairo_path_data_type = longint;

const
  CAIRO_PATH_MOVE_TO = 0;
  CAIRO_PATH_LINE_TO = 1;
  CAIRO_PATH_CURVE_TO = 2;
  CAIRO_PATH_CLOSE_PATH = 3;

type
  Tcairo_path_data_type_t = Tcairo_path_data_type;
  Pcairo_path_data_type_t = ^Tcairo_path_data_type_t;

type
  Tcairo_path_data_t = record
    case longint of
      0: (header: record
          _type: Tcairo_path_data_type_t;
          length: longint;
          end);
      1: (point: record
          x: Tdouble;
          y: Tdouble;
          end);
  end;
  Pcairo_path_data_t = ^Tcairo_path_data_t;
  Pcairo_path = ^Tcairo_path;

  Tcairo_path = record
    status: Tcairo_status_t;
    Data: Pcairo_path_data_t;
    num_data: longint;
  end;
  Tcairo_path_t = Tcairo_path;
  Pcairo_path_t = ^Tcairo_path_t;

function cairo_copy_path(cr: Pcairo_t): Pcairo_path_t; cdecl; external cairo_lib;
function cairo_copy_path_flat(cr: Pcairo_t): Pcairo_path_t; cdecl; external cairo_lib;
procedure cairo_append_path(cr: Pcairo_t; path: Pcairo_path_t); cdecl; external cairo_lib;
procedure cairo_path_destroy(path: Pcairo_path_t); cdecl; external cairo_lib;
function cairo_status(cr: Pcairo_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_status_to_string(status: Tcairo_status_t): PChar; cdecl; external cairo_lib;
function cairo_device_reference(device: Pcairo_device_t): Pcairo_device_t; cdecl; external cairo_lib;

type
  Pcairo_device_type = ^Tcairo_device_type;
  Tcairo_device_type = longint;

const
  CAIRO_DEVICE_TYPE_DRM = 0;
  CAIRO_DEVICE_TYPE_GL = 1;
  CAIRO_DEVICE_TYPE_SCRIPT = 2;
  CAIRO_DEVICE_TYPE_XCB = 3;
  CAIRO_DEVICE_TYPE_XLIB = 4;
  CAIRO_DEVICE_TYPE_XML = 5;
  CAIRO_DEVICE_TYPE_COGL = 6;
  CAIRO_DEVICE_TYPE_WIN32 = 7;
  CAIRO_DEVICE_TYPE_INVALID = -(1);

type
  Tcairo_device_type_t = Tcairo_device_type;
  Pcairo_device_type_t = ^Tcairo_device_type_t;

function cairo_device_get_type(device: Pcairo_device_t): Tcairo_device_type_t; cdecl; external cairo_lib;
function cairo_device_status(device: Pcairo_device_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_device_acquire(device: Pcairo_device_t): Tcairo_status_t; cdecl; external cairo_lib;
procedure cairo_device_release(device: Pcairo_device_t); cdecl; external cairo_lib;
procedure cairo_device_flush(device: Pcairo_device_t); cdecl; external cairo_lib;
procedure cairo_device_finish(device: Pcairo_device_t); cdecl; external cairo_lib;
procedure cairo_device_destroy(device: Pcairo_device_t); cdecl; external cairo_lib;
function cairo_device_get_reference_count(device: Pcairo_device_t): dword; cdecl; external cairo_lib;
function cairo_device_get_user_data(device: Pcairo_device_t; key: Pcairo_user_data_key_t): pointer; cdecl; external cairo_lib;
function cairo_device_set_user_data(device: Pcairo_device_t; key: Pcairo_user_data_key_t; user_data: pointer; Destroy: Tcairo_destroy_func_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_create_similar(other: Pcairo_surface_t; content: Tcairo_content_t; Width: longint; Height: longint): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_surface_create_similar_image(other: Pcairo_surface_t; format: Tcairo_format_t; Width: longint; Height: longint): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_surface_map_to_image(surface: Pcairo_surface_t; extents: Pcairo_rectangle_int_t): Pcairo_surface_t; cdecl; external cairo_lib;
procedure cairo_surface_unmap_image(surface: Pcairo_surface_t; image: Pcairo_surface_t); cdecl; external cairo_lib;
function cairo_surface_create_for_rectangle(target: Pcairo_surface_t; x: Tdouble; y: Tdouble; Width: Tdouble; Height: Tdouble): Pcairo_surface_t; cdecl; external cairo_lib;

type
  Pcairo_surface_observer_mode_t = ^Tcairo_surface_observer_mode_t;
  Tcairo_surface_observer_mode_t = longint;

const
  CAIRO_SURFACE_OBSERVER_NORMAL = 0;
  CAIRO_SURFACE_OBSERVER_RECORD_OPERATIONS = $1;

function cairo_surface_create_observer(target: Pcairo_surface_t; mode: Tcairo_surface_observer_mode_t): Pcairo_surface_t; cdecl; external cairo_lib;

type
  Tcairo_surface_observer_callback_t = procedure(observer: Pcairo_surface_t; target: Pcairo_surface_t; Data: pointer); cdecl;

function cairo_surface_observer_add_paint_callback(abstract_surface: Pcairo_surface_t; func: Tcairo_surface_observer_callback_t; Data: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_observer_add_mask_callback(abstract_surface: Pcairo_surface_t; func: Tcairo_surface_observer_callback_t; Data: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_observer_add_fill_callback(abstract_surface: Pcairo_surface_t; func: Tcairo_surface_observer_callback_t; Data: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_observer_add_stroke_callback(abstract_surface: Pcairo_surface_t; func: Tcairo_surface_observer_callback_t; Data: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_observer_add_glyphs_callback(abstract_surface: Pcairo_surface_t; func: Tcairo_surface_observer_callback_t; Data: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_observer_add_flush_callback(abstract_surface: Pcairo_surface_t; func: Tcairo_surface_observer_callback_t; Data: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_observer_add_finish_callback(abstract_surface: Pcairo_surface_t; func: Tcairo_surface_observer_callback_t; Data: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_observer_print(abstract_surface: Pcairo_surface_t; write_func: Tcairo_write_func_t; closure: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_observer_elapsed(abstract_surface: Pcairo_surface_t): Tdouble; cdecl; external cairo_lib;
function cairo_device_observer_print(abstract_device: Pcairo_device_t; write_func: Tcairo_write_func_t; closure: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_device_observer_elapsed(abstract_device: Pcairo_device_t): Tdouble; cdecl; external cairo_lib;
function cairo_device_observer_paint_elapsed(abstract_device: Pcairo_device_t): Tdouble; cdecl; external cairo_lib;
function cairo_device_observer_mask_elapsed(abstract_device: Pcairo_device_t): Tdouble; cdecl; external cairo_lib;
function cairo_device_observer_fill_elapsed(abstract_device: Pcairo_device_t): Tdouble; cdecl; external cairo_lib;
function cairo_device_observer_stroke_elapsed(abstract_device: Pcairo_device_t): Tdouble; cdecl; external cairo_lib;
function cairo_device_observer_glyphs_elapsed(abstract_device: Pcairo_device_t): Tdouble; cdecl; external cairo_lib;
function cairo_surface_reference(surface: Pcairo_surface_t): Pcairo_surface_t; cdecl; external cairo_lib;
procedure cairo_surface_finish(surface: Pcairo_surface_t); cdecl; external cairo_lib;
procedure cairo_surface_destroy(surface: Pcairo_surface_t); cdecl; external cairo_lib;
function cairo_surface_get_device(surface: Pcairo_surface_t): Pcairo_device_t; cdecl; external cairo_lib;
function cairo_surface_get_reference_count(surface: Pcairo_surface_t): dword; cdecl; external cairo_lib;
function cairo_surface_status(surface: Pcairo_surface_t): Tcairo_status_t; cdecl; external cairo_lib;

type
  Pcairo_surface_type = ^Tcairo_surface_type;
  Tcairo_surface_type = longint;

const
  CAIRO_SURFACE_TYPE_IMAGE = 0;
  CAIRO_SURFACE_TYPE_PDF = 1;
  CAIRO_SURFACE_TYPE_PS = 2;
  CAIRO_SURFACE_TYPE_XLIB = 3;
  CAIRO_SURFACE_TYPE_XCB = 4;
  CAIRO_SURFACE_TYPE_GLITZ = 5;
  CAIRO_SURFACE_TYPE_QUARTZ = 6;
  CAIRO_SURFACE_TYPE_WIN32 = 7;
  CAIRO_SURFACE_TYPE_BEOS = 8;
  CAIRO_SURFACE_TYPE_DIRECTFB = 9;
  CAIRO_SURFACE_TYPE_SVG = 10;
  CAIRO_SURFACE_TYPE_OS2 = 11;
  CAIRO_SURFACE_TYPE_WIN32_PRINTING = 12;
  CAIRO_SURFACE_TYPE_QUARTZ_IMAGE = 13;
  CAIRO_SURFACE_TYPE_SCRIPT = 14;
  CAIRO_SURFACE_TYPE_QT = 15;
  CAIRO_SURFACE_TYPE_RECORDING = 16;
  CAIRO_SURFACE_TYPE_VG = 17;
  CAIRO_SURFACE_TYPE_GL = 18;
  CAIRO_SURFACE_TYPE_DRM = 19;
  CAIRO_SURFACE_TYPE_TEE = 20;
  CAIRO_SURFACE_TYPE_XML = 21;
  CAIRO_SURFACE_TYPE_SKIA = 22;
  CAIRO_SURFACE_TYPE_SUBSURFACE = 23;
  CAIRO_SURFACE_TYPE_COGL = 24;

type
  Tcairo_surface_type_t = Tcairo_surface_type;
  Pcairo_surface_type_t = ^Tcairo_surface_type_t;

function cairo_surface_get_type(surface: Pcairo_surface_t): Tcairo_surface_type_t; cdecl; external cairo_lib;
function cairo_surface_get_content(surface: Pcairo_surface_t): Tcairo_content_t; cdecl; external cairo_lib;
function cairo_surface_write_to_png(surface: Pcairo_surface_t; filename: PChar): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_write_to_png_stream(surface: Pcairo_surface_t; write_func: Tcairo_write_func_t; closure: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_get_user_data(surface: Pcairo_surface_t; key: Pcairo_user_data_key_t): pointer; cdecl; external cairo_lib;
function cairo_surface_set_user_data(surface: Pcairo_surface_t; key: Pcairo_user_data_key_t; user_data: pointer; Destroy: Tcairo_destroy_func_t): Tcairo_status_t; cdecl; external cairo_lib;

const
  CAIRO_MIME_TYPE_JPEG = 'image/jpeg';
  CAIRO_MIME_TYPE_PNG = 'image/png';
  CAIRO_MIME_TYPE_JP2 = 'image/jp2';
  CAIRO_MIME_TYPE_URI = 'text/x-uri';
  CAIRO_MIME_TYPE_UNIQUE_ID = 'application/x-cairo.uuid';
  CAIRO_MIME_TYPE_JBIG2 = 'application/x-cairo.jbig2';
  CAIRO_MIME_TYPE_JBIG2_GLOBAL = 'application/x-cairo.jbig2-global';
  CAIRO_MIME_TYPE_JBIG2_GLOBAL_ID = 'application/x-cairo.jbig2-global-id';
  CAIRO_MIME_TYPE_CCITT_FAX = 'image/g3fax';
  CAIRO_MIME_TYPE_CCITT_FAX_PARAMS = 'application/x-cairo.ccitt.params';
  CAIRO_MIME_TYPE_EPS = 'application/postscript';
  CAIRO_MIME_TYPE_EPS_PARAMS = 'application/x-cairo.eps.params';

procedure cairo_surface_get_mime_data(surface: Pcairo_surface_t; mime_type: PChar; Data: PPbyte; length: Pdword); cdecl; external cairo_lib;
function cairo_surface_set_mime_data(surface: Pcairo_surface_t; mime_type: PChar; Data: pbyte; length: dword; Destroy: Tcairo_destroy_func_t; closure: pointer): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_surface_supports_mime_type(surface: Pcairo_surface_t; mime_type: PChar): Tcairo_bool_t; cdecl; external cairo_lib;
procedure cairo_surface_get_font_options(surface: Pcairo_surface_t; options: Pcairo_font_options_t); cdecl; external cairo_lib;
procedure cairo_surface_flush(surface: Pcairo_surface_t); cdecl; external cairo_lib;
procedure cairo_surface_mark_dirty(surface: Pcairo_surface_t); cdecl; external cairo_lib;
procedure cairo_surface_mark_dirty_rectangle(surface: Pcairo_surface_t; x: longint; y: longint; Width: longint; Height: longint); cdecl; external cairo_lib;
procedure cairo_surface_set_device_scale(surface: Pcairo_surface_t; x_scale: Tdouble; y_scale: Tdouble); cdecl; external cairo_lib;
procedure cairo_surface_get_device_scale(surface: Pcairo_surface_t; x_scale: Pdouble; y_scale: Pdouble); cdecl; external cairo_lib;
procedure cairo_surface_set_device_offset(surface: Pcairo_surface_t; x_offset: Tdouble; y_offset: Tdouble); cdecl; external cairo_lib;
procedure cairo_surface_get_device_offset(surface: Pcairo_surface_t; x_offset: Pdouble; y_offset: Pdouble); cdecl; external cairo_lib;
procedure cairo_surface_set_fallback_resolution(surface: Pcairo_surface_t; x_pixels_per_inch: Tdouble; y_pixels_per_inch: Tdouble); cdecl; external cairo_lib;
procedure cairo_surface_get_fallback_resolution(surface: Pcairo_surface_t; x_pixels_per_inch: Pdouble; y_pixels_per_inch: Pdouble); cdecl; external cairo_lib;
procedure cairo_surface_copy_page(surface: Pcairo_surface_t); cdecl; external cairo_lib;
procedure cairo_surface_show_page(surface: Pcairo_surface_t); cdecl; external cairo_lib;
function cairo_surface_has_show_text_glyphs(surface: Pcairo_surface_t): Tcairo_bool_t; cdecl; external cairo_lib;
function cairo_image_surface_create(format: Tcairo_format_t; Width: longint; Height: longint): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_format_stride_for_width(format: Tcairo_format_t; Width: longint): longint; cdecl; external cairo_lib;
function cairo_image_surface_create_for_data(Data: pbyte; format: Tcairo_format_t; Width: longint; Height: longint; stride: longint): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_image_surface_get_data(surface: Pcairo_surface_t): pbyte; cdecl; external cairo_lib;
function cairo_image_surface_get_format(surface: Pcairo_surface_t): Tcairo_format_t; cdecl; external cairo_lib;
function cairo_image_surface_get_width(surface: Pcairo_surface_t): longint; cdecl; external cairo_lib;
function cairo_image_surface_get_height(surface: Pcairo_surface_t): longint; cdecl; external cairo_lib;
function cairo_image_surface_get_stride(surface: Pcairo_surface_t): longint; cdecl; external cairo_lib;
function cairo_image_surface_create_from_png(filename: PChar): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_image_surface_create_from_png_stream(read_func: Tcairo_read_func_t; closure: pointer): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_recording_surface_create(content: Tcairo_content_t; extents: Pcairo_rectangle_t): Pcairo_surface_t; cdecl; external cairo_lib;
procedure cairo_recording_surface_ink_extents(surface: Pcairo_surface_t; x0: Pdouble; y0: Pdouble; Width: Pdouble; Height: Pdouble); cdecl; external cairo_lib;
function cairo_recording_surface_get_extents(surface: Pcairo_surface_t; extents: Pcairo_rectangle_t): Tcairo_bool_t; cdecl; external cairo_lib;

type
  Pcairo_raster_source_acquire_func_t = ^Tcairo_raster_source_acquire_func_t;
  Tcairo_raster_source_acquire_func_t = function(pattern: Pcairo_pattern_t; callback_data: pointer; target: Pcairo_surface_t; extents: Pcairo_rectangle_int_t): Pcairo_surface_t; cdecl;
  Tcairo_raster_source_release_func_t = procedure(pattern: Pcairo_pattern_t; callback_data: pointer; surface: Pcairo_surface_t); cdecl;
  Pcairo_raster_source_release_func_t = ^Tcairo_raster_source_release_func_t;
  Tcairo_raster_source_snapshot_func_t = function(pattern: Pcairo_pattern_t; callback_data: pointer): Tcairo_status_t; cdecl;
  Tcairo_raster_source_copy_func_t = function(pattern: Pcairo_pattern_t; callback_data: pointer; other: Pcairo_pattern_t): Tcairo_status_t; cdecl;
  Tcairo_raster_source_finish_func_t = procedure(pattern: Pcairo_pattern_t; callback_data: pointer); cdecl;

function cairo_pattern_create_raster_source(user_data: pointer; content: Tcairo_content_t; Width: longint; Height: longint): Pcairo_pattern_t; cdecl; external cairo_lib;
procedure cairo_raster_source_pattern_set_callback_data(pattern: Pcairo_pattern_t; Data: pointer); cdecl; external cairo_lib;
function cairo_raster_source_pattern_get_callback_data(pattern: Pcairo_pattern_t): pointer; cdecl; external cairo_lib;
procedure cairo_raster_source_pattern_set_acquire(pattern: Pcairo_pattern_t; acquire: Tcairo_raster_source_acquire_func_t; Release: Tcairo_raster_source_release_func_t); cdecl; external cairo_lib;
procedure cairo_raster_source_pattern_get_acquire(pattern: Pcairo_pattern_t; acquire: Pcairo_raster_source_acquire_func_t; Release: Pcairo_raster_source_release_func_t); cdecl; external cairo_lib;
procedure cairo_raster_source_pattern_set_snapshot(pattern: Pcairo_pattern_t; snapshot: Tcairo_raster_source_snapshot_func_t); cdecl; external cairo_lib;
function cairo_raster_source_pattern_get_snapshot(pattern: Pcairo_pattern_t): Tcairo_raster_source_snapshot_func_t; cdecl; external cairo_lib;
procedure cairo_raster_source_pattern_set_copy(pattern: Pcairo_pattern_t; copy: Tcairo_raster_source_copy_func_t); cdecl; external cairo_lib;
function cairo_raster_source_pattern_get_copy(pattern: Pcairo_pattern_t): Tcairo_raster_source_copy_func_t; cdecl; external cairo_lib;
procedure cairo_raster_source_pattern_set_finish(pattern: Pcairo_pattern_t; finish: Tcairo_raster_source_finish_func_t); cdecl; external cairo_lib;
function cairo_raster_source_pattern_get_finish(pattern: Pcairo_pattern_t): Tcairo_raster_source_finish_func_t; cdecl; external cairo_lib;
function cairo_pattern_create_rgb(red: Tdouble; green: Tdouble; blue: Tdouble): Pcairo_pattern_t; cdecl; external cairo_lib;
function cairo_pattern_create_rgba(red: Tdouble; green: Tdouble; blue: Tdouble; alpha: Tdouble): Pcairo_pattern_t; cdecl; external cairo_lib;
function cairo_pattern_create_for_surface(surface: Pcairo_surface_t): Pcairo_pattern_t; cdecl; external cairo_lib;
function cairo_pattern_create_linear(x0: Tdouble; y0: Tdouble; x1: Tdouble; y1: Tdouble): Pcairo_pattern_t; cdecl; external cairo_lib;
function cairo_pattern_create_radial(cx0: Tdouble; cy0: Tdouble; radius0: Tdouble; cx1: Tdouble; cy1: Tdouble;
  radius1: Tdouble): Pcairo_pattern_t; cdecl; external cairo_lib;
function cairo_pattern_create_mesh: Pcairo_pattern_t; cdecl; external cairo_lib;
function cairo_pattern_reference(pattern: Pcairo_pattern_t): Pcairo_pattern_t; cdecl; external cairo_lib;
procedure cairo_pattern_destroy(pattern: Pcairo_pattern_t); cdecl; external cairo_lib;
function cairo_pattern_get_reference_count(pattern: Pcairo_pattern_t): dword; cdecl; external cairo_lib;
function cairo_pattern_status(pattern: Pcairo_pattern_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_pattern_get_user_data(pattern: Pcairo_pattern_t; key: Pcairo_user_data_key_t): pointer; cdecl; external cairo_lib;
function cairo_pattern_set_user_data(pattern: Pcairo_pattern_t; key: Pcairo_user_data_key_t; user_data: pointer; Destroy: Tcairo_destroy_func_t): Tcairo_status_t; cdecl; external cairo_lib;

type
  Pcairo_pattern_type = ^Tcairo_pattern_type;
  Tcairo_pattern_type = longint;

const
  CAIRO_PATTERN_TYPE_SOLID = 0;
  CAIRO_PATTERN_TYPE_SURFACE = 1;
  CAIRO_PATTERN_TYPE_LINEAR = 2;
  CAIRO_PATTERN_TYPE_RADIAL = 3;
  CAIRO_PATTERN_TYPE_MESH = 4;
  CAIRO_PATTERN_TYPE_RASTER_SOURCE = 5;

type
  Tcairo_pattern_type_t = Tcairo_pattern_type;
  Pcairo_pattern_type_t = ^Tcairo_pattern_type_t;

function cairo_pattern_get_type(pattern: Pcairo_pattern_t): Tcairo_pattern_type_t; cdecl; external cairo_lib;
procedure cairo_pattern_add_color_stop_rgb(pattern: Pcairo_pattern_t; offset: Tdouble; red: Tdouble; green: Tdouble; blue: Tdouble); cdecl; external cairo_lib;
procedure cairo_pattern_add_color_stop_rgba(pattern: Pcairo_pattern_t; offset: Tdouble; red: Tdouble; green: Tdouble; blue: Tdouble;
  alpha: Tdouble); cdecl; external cairo_lib;
procedure cairo_mesh_pattern_begin_patch(pattern: Pcairo_pattern_t); cdecl; external cairo_lib;
procedure cairo_mesh_pattern_end_patch(pattern: Pcairo_pattern_t); cdecl; external cairo_lib;
procedure cairo_mesh_pattern_curve_to(pattern: Pcairo_pattern_t; x1: Tdouble; y1: Tdouble; x2: Tdouble; y2: Tdouble;
  x3: Tdouble; y3: Tdouble); cdecl; external cairo_lib;
procedure cairo_mesh_pattern_line_to(pattern: Pcairo_pattern_t; x: Tdouble; y: Tdouble); cdecl; external cairo_lib;
procedure cairo_mesh_pattern_move_to(pattern: Pcairo_pattern_t; x: Tdouble; y: Tdouble); cdecl; external cairo_lib;
procedure cairo_mesh_pattern_set_control_point(pattern: Pcairo_pattern_t; point_num: dword; x: Tdouble; y: Tdouble); cdecl; external cairo_lib;
procedure cairo_mesh_pattern_set_corner_color_rgb(pattern: Pcairo_pattern_t; corner_num: dword; red: Tdouble; green: Tdouble; blue: Tdouble); cdecl; external cairo_lib;
procedure cairo_mesh_pattern_set_corner_color_rgba(pattern: Pcairo_pattern_t; corner_num: dword; red: Tdouble; green: Tdouble; blue: Tdouble;
  alpha: Tdouble); cdecl; external cairo_lib;
procedure cairo_pattern_set_matrix(pattern: Pcairo_pattern_t; matrix: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_pattern_get_matrix(pattern: Pcairo_pattern_t; matrix: Pcairo_matrix_t); cdecl; external cairo_lib;

type
  Pcairo_extend = ^Tcairo_extend;
  Tcairo_extend = longint;

const
  CAIRO_EXTEND_NONE = 0;
  CAIRO_EXTEND_REPEAT = 1;
  CAIRO_EXTEND_REFLECT = 2;
  CAIRO_EXTEND_PAD = 3;

type
  Tcairo_extend_t = Tcairo_extend;
  Pcairo_extend_t = ^Tcairo_extend_t;

procedure cairo_pattern_set_extend(pattern: Pcairo_pattern_t; extend: Tcairo_extend_t); cdecl; external cairo_lib;
function cairo_pattern_get_extend(pattern: Pcairo_pattern_t): Tcairo_extend_t; cdecl; external cairo_lib;

type
  Pcairo_filter = ^Tcairo_filter;
  Tcairo_filter = longint;

const
  CAIRO_FILTER_FAST = 0;
  CAIRO_FILTER_GOOD = 1;
  CAIRO_FILTER_BEST = 2;
  CAIRO_FILTER_NEAREST = 3;
  CAIRO_FILTER_BILINEAR = 4;
  CAIRO_FILTER_GAUSSIAN = 5;

type
  Tcairo_filter_t = Tcairo_filter;
  Pcairo_filter_t = ^Tcairo_filter_t;

procedure cairo_pattern_set_filter(pattern: Pcairo_pattern_t; filter: Tcairo_filter_t); cdecl; external cairo_lib;
function cairo_pattern_get_filter(pattern: Pcairo_pattern_t): Tcairo_filter_t; cdecl; external cairo_lib;
function cairo_pattern_get_rgba(pattern: Pcairo_pattern_t; red: Pdouble; green: Pdouble; blue: Pdouble; alpha: Pdouble): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_pattern_get_surface(pattern: Pcairo_pattern_t; surface: PPcairo_surface_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_pattern_get_color_stop_rgba(pattern: Pcairo_pattern_t; index: longint; offset: Pdouble; red: Pdouble; green: Pdouble; blue: Pdouble; alpha: Pdouble): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_pattern_get_color_stop_count(pattern: Pcairo_pattern_t; Count: Plongint): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_pattern_get_linear_points(pattern: Pcairo_pattern_t; x0: Pdouble; y0: Pdouble; x1: Pdouble; y1: Pdouble): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_pattern_get_radial_circles(pattern: Pcairo_pattern_t; x0: Pdouble; y0: Pdouble; r0: Pdouble; x1: Pdouble; y1: Pdouble; r1: Pdouble): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_mesh_pattern_get_patch_count(pattern: Pcairo_pattern_t; Count: Pdword): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_mesh_pattern_get_path(pattern: Pcairo_pattern_t; patch_num: dword): Pcairo_path_t; cdecl; external cairo_lib;
function cairo_mesh_pattern_get_corner_color_rgba(pattern: Pcairo_pattern_t; patch_num: dword; corner_num: dword; red: Pdouble; green: Pdouble; blue: Pdouble; alpha: Pdouble): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_mesh_pattern_get_control_point(pattern: Pcairo_pattern_t; patch_num: dword; point_num: dword; x: Pdouble; y: Pdouble): Tcairo_status_t; cdecl; external cairo_lib;
procedure cairo_matrix_init(matrix: Pcairo_matrix_t; xx: Tdouble; yx: Tdouble; xy: Tdouble; yy: Tdouble; x0: Tdouble; y0: Tdouble); cdecl; external cairo_lib;
procedure cairo_matrix_init_identity(matrix: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_matrix_init_translate(matrix: Pcairo_matrix_t; tx: Tdouble; ty: Tdouble); cdecl; external cairo_lib;
procedure cairo_matrix_init_scale(matrix: Pcairo_matrix_t; sx: Tdouble; sy: Tdouble); cdecl; external cairo_lib;
procedure cairo_matrix_init_rotate(matrix: Pcairo_matrix_t; radians: Tdouble); cdecl; external cairo_lib;
procedure cairo_matrix_translate(matrix: Pcairo_matrix_t; tx: Tdouble; ty: Tdouble); cdecl; external cairo_lib;
procedure cairo_matrix_scale(matrix: Pcairo_matrix_t; sx: Tdouble; sy: Tdouble); cdecl; external cairo_lib;
procedure cairo_matrix_rotate(matrix: Pcairo_matrix_t; radians: Tdouble); cdecl; external cairo_lib;
function cairo_matrix_invert(matrix: Pcairo_matrix_t): Tcairo_status_t; cdecl; external cairo_lib;
procedure cairo_matrix_multiply(Result: Pcairo_matrix_t; a: Pcairo_matrix_t; b: Pcairo_matrix_t); cdecl; external cairo_lib;
procedure cairo_matrix_transform_distance(matrix: Pcairo_matrix_t; dx: Pdouble; dy: Pdouble); cdecl; external cairo_lib;
procedure cairo_matrix_transform_point(matrix: Pcairo_matrix_t; x: Pdouble; y: Pdouble); cdecl; external cairo_lib;

type
  Tcairo_region = Tcairo_region_t;
  Pcairo_region_overlap = ^Tcairo_region_overlap;
  Tcairo_region_overlap = longint;

const
  CAIRO_REGION_OVERLAP_IN = 0;
  CAIRO_REGION_OVERLAP_OUT = 1;
  CAIRO_REGION_OVERLAP_PART = 2;

type
  Tcairo_region_overlap_t = Tcairo_region_overlap;
  Pcairo_region_overlap_t = ^Tcairo_region_overlap_t;

function cairo_region_create: Pcairo_region_t; cdecl; external cairo_lib;
function cairo_region_create_rectangle(rectangle: Pcairo_rectangle_int_t): Pcairo_region_t; cdecl; external cairo_lib;
function cairo_region_create_rectangles(rects: Pcairo_rectangle_int_t; Count: longint): Pcairo_region_t; cdecl; external cairo_lib;
function cairo_region_copy(original: Pcairo_region_t): Pcairo_region_t; cdecl; external cairo_lib;
function cairo_region_reference(region: Pcairo_region_t): Pcairo_region_t; cdecl; external cairo_lib;
procedure cairo_region_destroy(region: Pcairo_region_t); cdecl; external cairo_lib;
function cairo_region_equal(a: Pcairo_region_t; b: Pcairo_region_t): Tcairo_bool_t; cdecl; external cairo_lib;
function cairo_region_status(region: Pcairo_region_t): Tcairo_status_t; cdecl; external cairo_lib;
procedure cairo_region_get_extents(region: Pcairo_region_t; extents: Pcairo_rectangle_int_t); cdecl; external cairo_lib;
function cairo_region_num_rectangles(region: Pcairo_region_t): longint; cdecl; external cairo_lib;
procedure cairo_region_get_rectangle(region: Pcairo_region_t; nth: longint; rectangle: Pcairo_rectangle_int_t); cdecl; external cairo_lib;
function cairo_region_is_empty(region: Pcairo_region_t): Tcairo_bool_t; cdecl; external cairo_lib;
function cairo_region_contains_rectangle(region: Pcairo_region_t; rectangle: Pcairo_rectangle_int_t): Tcairo_region_overlap_t; cdecl; external cairo_lib;
function cairo_region_contains_point(region: Pcairo_region_t; x: longint; y: longint): Tcairo_bool_t; cdecl; external cairo_lib;
procedure cairo_region_translate(region: Pcairo_region_t; dx: longint; dy: longint); cdecl; external cairo_lib;
function cairo_region_subtract(dst: Pcairo_region_t; other: Pcairo_region_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_region_subtract_rectangle(dst: Pcairo_region_t; rectangle: Pcairo_rectangle_int_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_region_intersect(dst: Pcairo_region_t; other: Pcairo_region_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_region_intersect_rectangle(dst: Pcairo_region_t; rectangle: Pcairo_rectangle_int_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_region_union(dst: Pcairo_region_t; other: Pcairo_region_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_region_union_rectangle(dst: Pcairo_region_t; rectangle: Pcairo_rectangle_int_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_region_xor(dst: Pcairo_region_t; other: Pcairo_region_t): Tcairo_status_t; cdecl; external cairo_lib;
function cairo_region_xor_rectangle(dst: Pcairo_region_t; rectangle: Pcairo_rectangle_int_t): Tcairo_status_t; cdecl; external cairo_lib;
procedure cairo_debug_reset_static_data; cdecl; external cairo_lib;

implementation

function CAIRO_VERSION_ENCODE(major, minor, micro: longint): longint;
begin
  CAIRO_VERSION_ENCODE := ((major * 10000) + (minor * 100)) + (micro * 1);
end;
//
//{ was #define dname def_expr }
//function CAIRO_VERSION : longint; { return type might be wrong }
//  begin
//    CAIRO_VERSION:=CAIRO_VERSION_ENCODE(CAIRO_VERSION_MAJOR,CAIRO_VERSION_MINOR,CAIRO_VERSION_MICRO);
//  end;
//
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }
//function CAIRO_VERSION_STRINGIZE(major,minor,micro : longint) : longint;
//begin
//  CAIRO_VERSION_STRINGIZE:=CAIRO_VERSION_STRINGIZE_(major,minor,micro);
//end;

//{ was #define dname def_expr }
//function CAIRO_VERSION_STRING : longint; { return type might be wrong }
//  begin
//    CAIRO_VERSION_STRING:=CAIRO_VERSION_STRINGIZE(CAIRO_VERSION_MAJOR,CAIRO_VERSION_MINOR,CAIRO_VERSION_MICRO);
//  end;


end.

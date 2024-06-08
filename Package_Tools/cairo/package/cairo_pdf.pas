unit cairo_pdf;

interface

uses
  cairo;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  Pcairo_pdf_version = ^Tcairo_pdf_version;
  Tcairo_pdf_version = longint;

const
  CAIRO_PDF_VERSION_1_4 = 0;
  CAIRO_PDF_VERSION_1_5 = 1;
  CAIRO_PDF_VERSION_1_6 = 2;
  CAIRO_PDF_VERSION_1_7 = 3;

type
  Tcairo_pdf_version_t = Tcairo_pdf_version;
  Pcairo_pdf_version_t = ^Tcairo_pdf_version_t;
  PPcairo_pdf_version_t = ^Pcairo_pdf_version_t;

function cairo_pdf_surface_create(filename: PChar; width_in_points: Tdouble; height_in_points: Tdouble): Pcairo_surface_t; cdecl; external cairo_lib;
function cairo_pdf_surface_create_for_stream(write_func: Tcairo_write_func_t; closure: pointer; width_in_points: Tdouble; height_in_points: Tdouble): Pcairo_surface_t; cdecl; external cairo_lib;
procedure cairo_pdf_surface_restrict_to_version(surface: Pcairo_surface_t; version: Tcairo_pdf_version_t); cdecl; external cairo_lib;
procedure cairo_pdf_get_versions(versions: PPcairo_pdf_version_t; num_versions: Plongint); cdecl; external cairo_lib;
function cairo_pdf_version_to_string(version: Tcairo_pdf_version_t): PChar; cdecl; external cairo_lib;
procedure cairo_pdf_surface_set_size(surface: Pcairo_surface_t; width_in_points: Tdouble; height_in_points: Tdouble); cdecl; external cairo_lib;

type
  Pcairo_pdf_outline_flags = ^Tcairo_pdf_outline_flags;
  Tcairo_pdf_outline_flags = longint;

const
  CAIRO_PDF_OUTLINE_FLAG_OPEN = $1;
  CAIRO_PDF_OUTLINE_FLAG_BOLD = $2;
  CAIRO_PDF_OUTLINE_FLAG_ITALIC = $4;

type
  Tcairo_pdf_outline_flags_t = Tcairo_pdf_outline_flags;
  Pcairo_pdf_outline_flags_t = ^Tcairo_pdf_outline_flags_t;

const
  CAIRO_PDF_OUTLINE_ROOT = 0;

function cairo_pdf_surface_add_outline(surface: Pcairo_surface_t; parent_id: longint; utf8: PChar; link_attribs: PChar; flags: Tcairo_pdf_outline_flags_t): longint; cdecl; external cairo_lib;

type
  Pcairo_pdf_metadata = ^Tcairo_pdf_metadata;
  Tcairo_pdf_metadata = longint;

const
  CAIRO_PDF_METADATA_TITLE = 0;
  CAIRO_PDF_METADATA_AUTHOR = 1;
  CAIRO_PDF_METADATA_SUBJECT = 2;
  CAIRO_PDF_METADATA_KEYWORDS = 3;
  CAIRO_PDF_METADATA_CREATOR = 4;
  CAIRO_PDF_METADATA_CREATE_DATE = 5;
  CAIRO_PDF_METADATA_MOD_DATE = 6;

type
  Tcairo_pdf_metadata_t = Tcairo_pdf_metadata;
  Pcairo_pdf_metadata_t = ^Tcairo_pdf_metadata_t;

procedure cairo_pdf_surface_set_metadata(surface: Pcairo_surface_t; metadata: Tcairo_pdf_metadata_t; utf8: PChar); cdecl; external cairo_lib;
procedure cairo_pdf_surface_set_custom_metadata(surface: Pcairo_surface_t; Name: PChar; Value: PChar); cdecl; external cairo_lib;
procedure cairo_pdf_surface_set_page_label(surface: Pcairo_surface_t; utf8: PChar); cdecl; external cairo_lib;
procedure cairo_pdf_surface_set_thumbnail_size(surface: Pcairo_surface_t; Width: longint; Height: longint); cdecl; external cairo_lib;

implementation

end.

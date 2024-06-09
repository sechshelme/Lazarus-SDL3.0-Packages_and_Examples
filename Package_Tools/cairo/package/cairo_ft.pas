unit cairo_ft;

interface

uses
  cairo;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

// {$include <ft2build.h>}

//function cairo_ft_font_face_create_for_ft_face(face:TFT_Face; load_flags:longint):Pcairo_font_face_t;cdecl; external cairo_lib;
//type
//  Pcairo_ft_synthesize_t = ^Tcairo_ft_synthesize_t;
//  Tcairo_ft_synthesize_t =  Longint;
//  Const
//    CAIRO_FT_SYNTHESIZE_BOLD = 1 shl 0;
//    CAIRO_FT_SYNTHESIZE_OBLIQUE = 1 shl 1;
//
//procedure cairo_ft_font_face_set_synthesize(font_face:Pcairo_font_face_t; synth_flags:dword);cdecl; external cairo_lib;
//procedure cairo_ft_font_face_unset_synthesize(font_face:Pcairo_font_face_t; synth_flags:dword);cdecl; external cairo_lib;
//function cairo_ft_font_face_get_synthesize(font_face:Pcairo_font_face_t):dword;cdecl; external cairo_lib;
//function cairo_ft_scaled_font_lock_face(scaled_font:Pcairo_scaled_font_t):TFT_Face;cdecl; external cairo_lib;
//procedure cairo_ft_scaled_font_unlock_face(scaled_font:Pcairo_scaled_font_t);cdecl; external cairo_lib;
//function cairo_ft_font_face_create_for_pattern(pattern:PFcPattern):Pcairo_font_face_t;cdecl; external cairo_lib;
//procedure cairo_ft_font_options_substitute(options:Pcairo_font_options_t; pattern:PFcPattern);cdecl; external cairo_lib;

implementation


end.

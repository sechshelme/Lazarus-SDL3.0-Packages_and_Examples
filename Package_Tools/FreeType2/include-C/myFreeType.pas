
unit myFreeType;
interface


{ }
{ ===========================  ftcache.h  =========================== }
{ }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function FTC_IMAGE_TYPE_COMPARE(d1,d2 : longint) : longint;



{$endif}
{ OTSVG_H_  }
{ END  }
{ }
{ ===========================  ftparams.h  =========================== }
{ }

{ was #define dname def_expr }
function FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_FAMILY : longint; { return type might be wrong }

{ this constant is deprecated  }
const
  FT_PARAM_TAG_IGNORE_PREFERRED_FAMILY = FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_FAMILY;  
function FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_SUBFAMILY : longint; { return type might be wrong }

{ this constant is deprecated  }
const
  FT_PARAM_TAG_IGNORE_PREFERRED_SUBFAMILY = FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_SUBFAMILY;  
{ was #define dname def_expr }
function FT_PARAM_TAG_INCREMENTAL : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_PARAM_TAG_IGNORE_SBIX : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_PARAM_TAG_LCD_FILTER_WEIGHTS : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_PARAM_TAG_RANDOM_SEED : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_PARAM_TAG_STEM_DARKENING : longint; { return type might be wrong }

function FT_PARAM_TAG_UNPATENTED_HINTING : longint; { return type might be wrong }

{  }
{  }


{ }
{ ===========================  ftgxval.h  =========================== }
{ }



{ was #define dname def_expr }
function FT_VALIDATE_feat : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_VALIDATE_mort : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_VALIDATE_morx : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_VALIDATE_bsln : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_VALIDATE_just : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_VALIDATE_kern : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_VALIDATE_opbd : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_VALIDATE_trak : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_VALIDATE_prop : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_VALIDATE_lcar : longint; { return type might be wrong }

const
  FT_VALIDATE_GX = ((((((((FT_VALIDATE_feat or FT_VALIDATE_mort) or FT_VALIDATE_morx) or FT_VALIDATE_bsln) or FT_VALIDATE_just) or FT_VALIDATE_kern) or FT_VALIDATE_opbd) or FT_VALIDATE_trak) or FT_VALIDATE_prop) or FT_VALIDATE_lcar;  


{ }
{ ===========================  tttags.h  =========================== }
{ }

{ was #define dname def_expr }
function TTAG_avar : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_BASE : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_bdat : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_BDF : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_bhed : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_bloc : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_bsln : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_CBDT : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_CBLC : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_CFF : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_CFF2 : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_CID : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_cmap : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_COLR : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_CPAL : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_cvar : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_cvt : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_DSIG : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_EBDT : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_EBLC : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_EBSC : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_feat : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_FOND : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_fpgm : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_fvar : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_gasp : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_GDEF : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_glyf : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_GPOS : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_GSUB : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_gvar : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_HVAR : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_hdmx : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_head : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_hhea : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_hmtx : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_JSTF : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_just : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_kern : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_lcar : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_loca : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_LTSH : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_LWFN : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_MATH : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_maxp : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_META : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_MMFX : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_MMSD : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_mort : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_morx : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_MVAR : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_name : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_opbd : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_OS2 : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_OTTO : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_PCLT : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_POST : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_post : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_prep : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_prop : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_sbix : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_sfnt : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_SING : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_SVG : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_trak : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_true : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_ttc : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_ttcf : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_TYP1 : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_typ1 : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_VDMX : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_vhea : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_vmtx : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_VVAR : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_wOFF : longint; { return type might be wrong }

{ was #define dname def_expr }
function TTAG_wOF2 : longint; { return type might be wrong }

{ used by "Keyboard.dfont" on legacy Mac OS X  }
{ was #define dname def_expr }
function TTAG_0xA5kbd : longint; { return type might be wrong }

{ used by "LastResort.dfont" on legacy Mac OS X  }
{ was #define dname def_expr }
function TTAG_0xA5lst : longint; { return type might be wrong }

{ }
{ ===========================  ftmm.h  =========================== }
{ }


{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function FT_MAKE_TAG(_x1,_x2,_x3,_x4 : longint) : longint;

{*********************************************************************** }
{*********************************************************************** }
{                                                                        }
{                    L I S T   M A N A G E M E N T                       }
{                                                                        }
{*********************************************************************** }
{*********************************************************************** }


{  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function FT_IS_EMPTY(list : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_BOOL(x : longint) : longint;

{ concatenate C tokens  }
{#define FT_ERR_XCAT( x, y )  x ## y }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_ERR_CAT(x,y : longint) : longint;

{ see `ftmoderr.h` for descriptions of the following macros  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_ERR(e : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function FT_ERROR_BASE(x : longint) : Tx;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function FT_ERROR_MODULE(x : longint) : Tx;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_ERR_EQ(x,e : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_ERR_NEQ(x,e : longint) : longint;

{ }
{ ===========================  ftsizes.h  =========================== }
{ }


{#ifdef FT_CONFIG_OPTION_USE_MODULE_ERRORS }
{#define FT_MODERRDEF( e, v, s )  FT_Mod_Err_ ## e = v, }
{#else }
{#define FT_MODERRDEF( e, v, s )  FT_Mod_Err_ ## e = 0, }
{#endif }
{#define FT_MODERR_START_LIST  enum  }
{#define FT_MODERR_END_LIST    FT_Mod_Err_Max ; }
{***************************************************************** }
{***************************************************************** }
{****                                                         **** }
{****               LIST MODULE ERROR BASES                   **** }
{****                                                         **** }
{***************************************************************** }
{***************************************************************** }
{
  FT_MODERRDEF( Base,      0x000, "base module" )
  FT_MODERRDEF( Autofit,   0x100, "autofitter module" )
  FT_MODERRDEF( BDF,       0x200, "BDF module" )
  FT_MODERRDEF( Bzip2,     0x300, "Bzip2 module" )
  FT_MODERRDEF( Cache,     0x400, "cache module" )
  FT_MODERRDEF( CFF,       0x500, "CFF module" )
  FT_MODERRDEF( CID,       0x600, "CID module" )
  FT_MODERRDEF( Gzip,      0x700, "Gzip module" )
  FT_MODERRDEF( LZW,       0x800, "LZW module" )
  FT_MODERRDEF( OTvalid,   0x900, "OpenType validation module" )
  FT_MODERRDEF( PCF,       0xA00, "PCF module" )
  FT_MODERRDEF( PFR,       0xB00, "PFR module" )
  FT_MODERRDEF( PSaux,     0xC00, "PS auxiliary module" )
  FT_MODERRDEF( PShinter,  0xD00, "PS hinter module" )
  FT_MODERRDEF( PSnames,   0xE00, "PS names module" )
  FT_MODERRDEF( Raster,    0xF00, "raster module" )
  FT_MODERRDEF( SFNT,     0x1000, "SFNT module" )
  FT_MODERRDEF( Smooth,   0x1100, "smooth raster module" )
  FT_MODERRDEF( TrueType, 0x1200, "TrueType module" )
  FT_MODERRDEF( Type1,    0x1300, "Type 1 module" )
  FT_MODERRDEF( Type42,   0x1400, "Type 42 module" )
  FT_MODERRDEF( Winfonts, 0x1500, "Windows FON/FNT module" )
  FT_MODERRDEF( GXvalid,  0x1600, "GX validation module" )
  FT_MODERRDEF( Sdf,      0x1700, "Signed distance field raster module" )

 }
{ }
{ ===========================  fterrdef.h  =========================== }
{ }
{*************************************************************************
   *
   * @enum:
   *   FT_Err_XXX
   *
    }
{ generic errors  }
{
  FT_NOERRORDEF_( Ok,                                        0x00,
                  "no error" )

  FT_ERRORDEF_( Cannot_Open_Resource,                        0x01,
                "cannot open resource" )
  FT_ERRORDEF_( Unknown_File_Format,                         0x02,
                "unknown file format" )
  FT_ERRORDEF_( Invalid_File_Format,                         0x03,
                "broken file" )
  FT_ERRORDEF_( Invalid_Version,                             0x04,
                "invalid FreeType version" )
  FT_ERRORDEF_( Lower_Module_Version,                        0x05,
                "module version is too low" )
  FT_ERRORDEF_( Invalid_Argument,                            0x06,
                "invalid argument" )
  FT_ERRORDEF_( Unimplemented_Feature,                       0x07,
                "unimplemented feature" )
  FT_ERRORDEF_( Invalid_Table,                               0x08,
                "broken table" )
  FT_ERRORDEF_( Invalid_Offset,                              0x09,
                "broken offset within table" )
  FT_ERRORDEF_( Array_Too_Large,                             0x0A,
                "array allocation size too large" )
  FT_ERRORDEF_( Missing_Module,                              0x0B,
                "missing module" )
  FT_ERRORDEF_( Missing_Property,                            0x0C,
                "missing property" )

//   glyph/character errors 

  FT_ERRORDEF_( Invalid_Glyph_Index,                         0x10,
                "invalid glyph index" )
  FT_ERRORDEF_( Invalid_Character_Code,                      0x11,
                "invalid character code" )
  FT_ERRORDEF_( Invalid_Glyph_Format,                        0x12,
                "unsupported glyph image format" )
  FT_ERRORDEF_( Cannot_Render_Glyph,                         0x13,
                "cannot render this glyph format" )
  FT_ERRORDEF_( Invalid_Outline,                             0x14,
                "invalid outline" )
  FT_ERRORDEF_( Invalid_Composite,                           0x15,
                "invalid composite glyph" )
  FT_ERRORDEF_( Too_Many_Hints,                              0x16,
                "too many hints" )
  FT_ERRORDEF_( Invalid_Pixel_Size,                          0x17,
                "invalid pixel size" )
  FT_ERRORDEF_( Invalid_SVG_Document,                        0x18,
                "invalid SVG document" )

  // handle errors 

  FT_ERRORDEF_( Invalid_Handle,                              0x20,
                "invalid object handle" )
  FT_ERRORDEF_( Invalid_Library_Handle,                      0x21,
                "invalid library handle" )
  FT_ERRORDEF_( Invalid_Driver_Handle,                       0x22,
                "invalid module handle" )
  FT_ERRORDEF_( Invalid_Face_Handle,                         0x23,
                "invalid face handle" )
  FT_ERRORDEF_( Invalid_Size_Handle,                         0x24,
                "invalid size handle" )
  FT_ERRORDEF_( Invalid_Slot_Handle,                         0x25,
                "invalid glyph slot handle" )
  FT_ERRORDEF_( Invalid_CharMap_Handle,                      0x26,
                "invalid charmap handle" )
  FT_ERRORDEF_( Invalid_Cache_Handle,                        0x27,
                "invalid cache manager handle" )
  FT_ERRORDEF_( Invalid_Stream_Handle,                       0x28,
                "invalid stream handle" )

  // driver errors 

  FT_ERRORDEF_( Too_Many_Drivers,                            0x30,
                "too many modules" )
  FT_ERRORDEF_( Too_Many_Extensions,                         0x31,
                "too many extensions" )

 // memory errors 

  FT_ERRORDEF_( Out_Of_Memory,                               0x40,
                "out of memory" )
  FT_ERRORDEF_( Unlisted_Object,                             0x41,
                "unlisted object" )

  // stream errors 

  FT_ERRORDEF_( Cannot_Open_Stream,                          0x51,
                "cannot open stream" )
  FT_ERRORDEF_( Invalid_Stream_Seek,                         0x52,
                "invalid stream seek" )
  FT_ERRORDEF_( Invalid_Stream_Skip,                         0x53,
                "invalid stream skip" )
  FT_ERRORDEF_( Invalid_Stream_Read,                         0x54,
                "invalid stream read" )
  FT_ERRORDEF_( Invalid_Stream_Operation,                    0x55,
                "invalid stream operation" )
  FT_ERRORDEF_( Invalid_Frame_Operation,                     0x56,
                "invalid frame operation" )
  FT_ERRORDEF_( Nested_Frame_Access,                         0x57,
                "nested frame access" )
  FT_ERRORDEF_( Invalid_Frame_Read,                          0x58,
                "invalid frame read" )

  // raster errors 

  FT_ERRORDEF_( Raster_Uninitialized,                        0x60,
                "raster uninitialized" )
  FT_ERRORDEF_( Raster_Corrupted,                            0x61,
                "raster corrupted" )
  FT_ERRORDEF_( Raster_Overflow,                             0x62,
                "raster overflow" )
  FT_ERRORDEF_( Raster_Negative_Height,                      0x63,
                "negative height while rastering" )

  // cache errors 

  FT_ERRORDEF_( Too_Many_Caches,                             0x70,
                "too many registered caches" )

  //TrueType and SFNT errors 

  FT_ERRORDEF_( Invalid_Opcode,                              0x80,
                "invalid opcode" )
  FT_ERRORDEF_( Too_Few_Arguments,                           0x81,
                "too few arguments" )
  FT_ERRORDEF_( Stack_Overflow,                              0x82,
                "stack overflow" )
  FT_ERRORDEF_( Code_Overflow,                               0x83,
                "code overflow" )
  FT_ERRORDEF_( Bad_Argument,                                0x84,
                "bad argument" )
  FT_ERRORDEF_( Divide_By_Zero,                              0x85,
                "division by zero" )
  FT_ERRORDEF_( Invalid_Reference,                           0x86,
                "invalid reference" )
  FT_ERRORDEF_( Debug_OpCode,                                0x87,
                "found debug opcode" )
  FT_ERRORDEF_( ENDF_In_Exec_Stream,                         0x88,
                "found ENDF opcode in execution stream" )
  FT_ERRORDEF_( Nested_DEFS,                                 0x89,
                "nested DEFS" )
  FT_ERRORDEF_( Invalid_CodeRange,                           0x8A,
                "invalid code range" )
  FT_ERRORDEF_( Execution_Too_Long,                          0x8B,
                "execution context too long" )
  FT_ERRORDEF_( Too_Many_Function_Defs,                      0x8C,
                "too many function definitions" )
  FT_ERRORDEF_( Too_Many_Instruction_Defs,                   0x8D,
                "too many instruction definitions" )
  FT_ERRORDEF_( Table_Missing,                               0x8E,
                "SFNT font table missing" )
  FT_ERRORDEF_( Horiz_Header_Missing,                        0x8F,
                "horizontal header (hhea) table missing" )
  FT_ERRORDEF_( Locations_Missing,                           0x90,
                "locations (loca) table missing" )
  FT_ERRORDEF_( Name_Table_Missing,                          0x91,
                "name table missing" )
  FT_ERRORDEF_( CMap_Table_Missing,                          0x92,
                "character map (cmap) table missing" )
  FT_ERRORDEF_( Hmtx_Table_Missing,                          0x93,
                "horizontal metrics (hmtx) table missing" )
  FT_ERRORDEF_( Post_Table_Missing,                          0x94,
                "PostScript (post) table missing" )
  FT_ERRORDEF_( Invalid_Horiz_Metrics,                       0x95,
                "invalid horizontal metrics" )
  FT_ERRORDEF_( Invalid_CharMap_Format,                      0x96,
                "invalid character map (cmap) format" )
  FT_ERRORDEF_( Invalid_PPem,                                0x97,
                "invalid ppem value" )
  FT_ERRORDEF_( Invalid_Vert_Metrics,                        0x98,
                "invalid vertical metrics" )
  FT_ERRORDEF_( Could_Not_Find_Context,                      0x99,
                "could not find context" )
  FT_ERRORDEF_( Invalid_Post_Table_Format,                   0x9A,
                "invalid PostScript (post) table format" )
  FT_ERRORDEF_( Invalid_Post_Table,                          0x9B,
                "invalid PostScript (post) table" )
  FT_ERRORDEF_( DEF_In_Glyf_Bytecode,                        0x9C,
                "found FDEF or IDEF opcode in glyf bytecode" )
  FT_ERRORDEF_( Missing_Bitmap,                              0x9D,
                "missing bitmap in strike" )
  FT_ERRORDEF_( Missing_SVG_Hooks,                           0x9E,
                "SVG hooks have not been set" )

  // CFF, CID, and Type 1 errors 

  FT_ERRORDEF_( Syntax_Error,                                0xA0,
                "opcode syntax error" )
  FT_ERRORDEF_( Stack_Underflow,                             0xA1,
                "argument stack underflow" )
  FT_ERRORDEF_( Ignore,                                      0xA2,
                "ignore" )
  FT_ERRORDEF_( No_Unicode_Glyph_Name,                       0xA3,
                "no Unicode glyph name found" )
  FT_ERRORDEF_( Glyph_Too_Big,                               0xA4,
                "glyph too big for hinting" )

  // BDF errors 

  FT_ERRORDEF_( Missing_Startfont_Field,                     0xB0,
                "`STARTFONT' field missing" )
  FT_ERRORDEF_( Missing_Font_Field,                          0xB1,
                "`FONT' field missing" )
  FT_ERRORDEF_( Missing_Size_Field,                          0xB2,
                "`SIZE' field missing" )
  FT_ERRORDEF_( Missing_Fontboundingbox_Field,               0xB3,
                "`FONTBOUNDINGBOX' field missing" )
  FT_ERRORDEF_( Missing_Chars_Field,                         0xB4,
                "`CHARS' field missing" )
  FT_ERRORDEF_( Missing_Startchar_Field,                     0xB5,
                "`STARTCHAR' field missing" )
  FT_ERRORDEF_( Missing_Encoding_Field,                      0xB6,
                "`ENCODING' field missing" )
  FT_ERRORDEF_( Missing_Bbx_Field,                           0xB7,
                "`BBX' field missing" )
  FT_ERRORDEF_( Bbx_Too_Big,                                 0xB8,
                "`BBX' too big" )
  FT_ERRORDEF_( Corrupted_Font_Header,                       0xB9,
                "Font header corrupted or missing fields" )
  FT_ERRORDEF_( Corrupted_Font_Glyphs,                       0xBA,
                "Font glyphs corrupted or missing fields" )

 }

{ }
{ ===========================  freetype.h  =========================== }
{ }




#define FT_ENC_TAG( value, a, b, c, d )                             \
          value = ( ( FT_STATIC_BYTE_CAST( FT_UInt32, a ) << 24 ) | \
                    ( FT_STATIC_BYTE_CAST( FT_UInt32, b ) << 16 ) | \
                    ( FT_STATIC_BYTE_CAST( FT_UInt32, c ) <<  8 ) | \
                      FT_STATIC_BYTE_CAST( FT_UInt32, d )         )
 }
{#endif /* FT_ENC_TAG */ }
{
  typedef enum  FT_Encoding_
  
    FT_ENC_TAG( FT_ENCODING_NONE, 0, 0, 0, 0 ),

    FT_ENC_TAG( FT_ENCODING_MS_SYMBOL, 's', 'y', 'm', 'b' ),
    FT_ENC_TAG( FT_ENCODING_UNICODE,   'u', 'n', 'i', 'c' ),

    FT_ENC_TAG( FT_ENCODING_SJIS,    's', 'j', 'i', 's' ),
    FT_ENC_TAG( FT_ENCODING_PRC,     'g', 'b', ' ', ' ' ),
    FT_ENC_TAG( FT_ENCODING_BIG5,    'b', 'i', 'g', '5' ),
    FT_ENC_TAG( FT_ENCODING_WANSUNG, 'w', 'a', 'n', 's' ),
    FT_ENC_TAG( FT_ENCODING_JOHAB,   'j', 'o', 'h', 'a' ),

    // for backward compatibility 
    FT_ENCODING_GB2312     = FT_ENCODING_PRC,
    FT_ENCODING_MS_SJIS    = FT_ENCODING_SJIS,
    FT_ENCODING_MS_GB2312  = FT_ENCODING_PRC,
    FT_ENCODING_MS_BIG5    = FT_ENCODING_BIG5,
    FT_ENCODING_MS_WANSUNG = FT_ENCODING_WANSUNG,
    FT_ENCODING_MS_JOHAB   = FT_ENCODING_JOHAB,

    FT_ENC_TAG( FT_ENCODING_ADOBE_STANDARD, 'A', 'D', 'O', 'B' ),
    FT_ENC_TAG( FT_ENCODING_ADOBE_EXPERT,   'A', 'D', 'B', 'E' ),
    FT_ENC_TAG( FT_ENCODING_ADOBE_CUSTOM,   'A', 'D', 'B', 'C' ),
    FT_ENC_TAG( FT_ENCODING_ADOBE_LATIN_1,  'l', 'a', 't', '1' ),

    FT_ENC_TAG( FT_ENCODING_OLD_LATIN_2, 'l', 'a', 't', '2' ),

    FT_ENC_TAG( FT_ENCODING_APPLE_ROMAN, 'a', 'r', 'm', 'n' )

   FT_Encoding;
 }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function FT_HAS_HORIZONTAL(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_VERTICAL(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_KERNING(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_SCALABLE(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_SFNT(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_FIXED_WIDTH(face : longint) : longint;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_FIXED_SIZES(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_FAST_GLYPHS(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_GLYPH_NAMES(face : longint) : longint;

{ return type might be wrong }   
function FT_HAS_MULTIPLE_MASTERS(face : longint) : longint;

{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_NAMED_INSTANCE(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_VARIATION(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_CID_KEYED(face : longint) : longint;
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_TRICKY(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_COLOR(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_SVG(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_SBIX(face : longint) : longint;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_SBIX_OVERLAY(face : longint) : longint;


{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function FT_LOAD_TARGET_(x : longint) : longint;

{ was #define dname def_expr }
function FT_LOAD_TARGET_NORMAL : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_LOAD_TARGET_LIGHT : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_LOAD_TARGET_MONO : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_LOAD_TARGET_LCD : longint; { return type might be wrong }

{ was #define dname def_expr }
function FT_LOAD_TARGET_LCD_V : longint; { return type might be wrong }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_LOAD_TARGET_MODE(x : longint) : longint;




{ }
{ ===========================  fterrors.h  =========================== }
{ }
{$endif}
{ FT_CONFIG_OPTION_USE_MODULE_ERRORS  }
{ If FT_ERRORDEF is not defined, we need to define a simple  }
{ enumeration type.                                          }
{                                                            }
{$ifndef FT_ERRORDEF}
{$define FT_INCLUDE_ERR_PROTOS}
{#define FT_ERRORDEF( e, v, s )  e = v, }
{#define FT_ERROR_START_LIST     enum  }
{#define FT_ERROR_END_LIST       FT_ERR_CAT( FT_ERR_PREFIX, Max ) ; }
{$endif}
{ !FT_ERRORDEF  }
{ this macro is used to define an error  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function FT_ERRORDEF_(e,v,s : longint) : longint;

{ this is only used for <module>_Err_Ok, which must be 0!  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_NOERRORDEF_(e,v,s : longint) : longint;


{ }
{ ===========================  ftstdlib.h  =========================== }
{ }
{$ifndef FTSTDLIB_H_}
{$define FTSTDLIB_H_}
{$include <stddef.h>}

const
  ft_ptrdiff_t = ptrdiff_t;  
{$include <limits.h>}

const
  FT_CHAR_BIT = CHAR_BIT;  
  FT_USHORT_MAX = USHRT_MAX;  
  FT_INT_MAX = INT_MAX;  
  FT_INT_MIN = INT_MIN;  
  FT_UINT_MAX = UINT_MAX;  
  FT_LONG_MIN = LONG_MIN;  
  FT_LONG_MAX = LONG_MAX;  
  FT_ULONG_MAX = ULONG_MAX;  
{$ifdef LLONG_MAX}
  FT_LLONG_MAX = LLONG_MAX;  
{$endif}
{$ifdef LLONG_MIN}

const
  FT_LLONG_MIN = LLONG_MIN;  
{$endif}
{$ifdef ULLONG_MAX}

const
  FT_ULLONG_MAX = ULLONG_MAX;  
{$endif}
{$include <string.h>}

const
  ft_memchr = memchr;  
  ft_memcmp = memcmp;  
  ft_memcpy = memcpy;  
  ft_memmove = memmove;  
  ft_memset = memset;  
  ft_strcat = strcat;  
  ft_strcmp = strcmp;  
  ft_strcpy = strcpy;  
  ft_strlen = strlen;  
  ft_strncmp = strncmp;  
  ft_strncpy = strncpy;  
  ft_strrchr = strrchr;  
  ft_strstr = strstr;  
{$include <stdio.h>}

const
  FT_FILE = FILE;  
  ft_fclose = fclose;  
  ft_fopen = fopen;  
  ft_fread = fread;  
  ft_fseek = fseek;  
  ft_ftell = ftell;  
  ft_snprintf = snprintf;  
{$include <stdlib.h>}

const
  ft_qsort = qsort;  
{*************************************************************************
   *
   *                       memory allocation
   *
    }
  ft_scalloc = calloc;  
  ft_sfree = free;  
  ft_smalloc = malloc;  
  ft_srealloc = realloc;  
{*************************************************************************
   *
   *                         miscellaneous
   *
    }
  ft_strtol = strtol;  
  ft_getenv = getenv;  
{*************************************************************************
   *
   *                        execution control
   *
    }
{$include <setjmp.h>}
{ note: this cannot be a typedef since   }

const
  ft_jmp_buf = jmp_buf;  
{       `jmp_buf` is defined as a macro  }
{       on certain platforms             }
  ft_longjmp = longjmp;  
{#define ft_setjmp( b ) setjmp( *(ft_jmp_buf*) &(b) ) /* same thing here */ }
{ The following is only used for debugging purposes, i.e., if    }
{ `FT_DEBUG_LEVEL_ERROR` or `FT_DEBUG_LEVEL_TRACE` are defined.  }



implementation

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FTC_IMAGE_TYPE_COMPARE(d1,d2 : longint) : longint;
begin
  FTC_IMAGE_TYPE_COMPARE:=(((d1^.face_id)=((d2^.face_id) and (@(d1^.width))))=((d2^.width) and (@(d1^.flags))))=(d2^.flags);
end;

{ was #define dname def_expr }
function FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_FAMILY : longint; { return type might be wrong }
  begin
    FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_FAMILY:=FT_MAKE_TAG('i','g','p','f');
  end;

{ was #define dname def_expr }
function FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_SUBFAMILY : longint; { return type might be wrong }
  begin
    FT_PARAM_TAG_IGNORE_TYPOGRAPHIC_SUBFAMILY:=FT_MAKE_TAG('i','g','p','s');
  end;

{ was #define dname def_expr }
function FT_PARAM_TAG_INCREMENTAL : longint; { return type might be wrong }
  begin
    FT_PARAM_TAG_INCREMENTAL:=FT_MAKE_TAG('i','n','c','r');
  end;

{ was #define dname def_expr }
function FT_PARAM_TAG_IGNORE_SBIX : longint; { return type might be wrong }
  begin
    FT_PARAM_TAG_IGNORE_SBIX:=FT_MAKE_TAG('i','s','b','x');
  end;

{ was #define dname def_expr }
function FT_PARAM_TAG_LCD_FILTER_WEIGHTS : longint; { return type might be wrong }
  begin
    FT_PARAM_TAG_LCD_FILTER_WEIGHTS:=FT_MAKE_TAG('l','c','d','f');
  end;

{ was #define dname def_expr }
function FT_PARAM_TAG_RANDOM_SEED : longint; { return type might be wrong }
  begin
    FT_PARAM_TAG_RANDOM_SEED:=FT_MAKE_TAG('s','e','e','d');
  end;

{ was #define dname def_expr }
function FT_PARAM_TAG_STEM_DARKENING : longint; { return type might be wrong }
  begin
    FT_PARAM_TAG_STEM_DARKENING:=FT_MAKE_TAG('d','a','r','k');
  end;

{ was #define dname def_expr }
function FT_PARAM_TAG_UNPATENTED_HINTING : longint; { return type might be wrong }
  begin
    FT_PARAM_TAG_UNPATENTED_HINTING:=FT_MAKE_TAG('u','n','p','a');
  end;

{ was #define dname def_expr }
function FT_VALIDATE_feat : longint; { return type might be wrong }
  begin
    FT_VALIDATE_feat:=FT_VALIDATE_GX_BITFIELD(feat);
  end;

{ was #define dname def_expr }
function FT_VALIDATE_mort : longint; { return type might be wrong }
  begin
    FT_VALIDATE_mort:=FT_VALIDATE_GX_BITFIELD(mort);
  end;

{ was #define dname def_expr }
function FT_VALIDATE_morx : longint; { return type might be wrong }
  begin
    FT_VALIDATE_morx:=FT_VALIDATE_GX_BITFIELD(morx);
  end;

{ was #define dname def_expr }
function FT_VALIDATE_bsln : longint; { return type might be wrong }
  begin
    FT_VALIDATE_bsln:=FT_VALIDATE_GX_BITFIELD(bsln);
  end;

{ was #define dname def_expr }
function FT_VALIDATE_just : longint; { return type might be wrong }
  begin
    FT_VALIDATE_just:=FT_VALIDATE_GX_BITFIELD(just);
  end;

{ was #define dname def_expr }
function FT_VALIDATE_kern : longint; { return type might be wrong }
  begin
    FT_VALIDATE_kern:=FT_VALIDATE_GX_BITFIELD(kern);
  end;

{ was #define dname def_expr }
function FT_VALIDATE_opbd : longint; { return type might be wrong }
  begin
    FT_VALIDATE_opbd:=FT_VALIDATE_GX_BITFIELD(opbd);
  end;

{ was #define dname def_expr }
function FT_VALIDATE_trak : longint; { return type might be wrong }
  begin
    FT_VALIDATE_trak:=FT_VALIDATE_GX_BITFIELD(trak);
  end;

{ was #define dname def_expr }
function FT_VALIDATE_prop : longint; { return type might be wrong }
  begin
    FT_VALIDATE_prop:=FT_VALIDATE_GX_BITFIELD(prop);
  end;

{ was #define dname def_expr }
function FT_VALIDATE_lcar : longint; { return type might be wrong }
  begin
    FT_VALIDATE_lcar:=FT_VALIDATE_GX_BITFIELD(lcar);
  end;

{ was #define dname def_expr }
function TTAG_avar : longint; { return type might be wrong }
  begin
    TTAG_avar:=FT_MAKE_TAG('a','v','a','r');
  end;

{ was #define dname def_expr }
function TTAG_BASE : longint; { return type might be wrong }
  begin
    TTAG_BASE:=FT_MAKE_TAG('B','A','S','E');
  end;

{ was #define dname def_expr }
function TTAG_bdat : longint; { return type might be wrong }
  begin
    TTAG_bdat:=FT_MAKE_TAG('b','d','a','t');
  end;

{ was #define dname def_expr }
function TTAG_BDF : longint; { return type might be wrong }
  begin
    TTAG_BDF:=FT_MAKE_TAG('B','D','F',' ');
  end;

{ was #define dname def_expr }
function TTAG_bhed : longint; { return type might be wrong }
  begin
    TTAG_bhed:=FT_MAKE_TAG('b','h','e','d');
  end;

{ was #define dname def_expr }
function TTAG_bloc : longint; { return type might be wrong }
  begin
    TTAG_bloc:=FT_MAKE_TAG('b','l','o','c');
  end;

{ was #define dname def_expr }
function TTAG_bsln : longint; { return type might be wrong }
  begin
    TTAG_bsln:=FT_MAKE_TAG('b','s','l','n');
  end;

{ was #define dname def_expr }
function TTAG_CBDT : longint; { return type might be wrong }
  begin
    TTAG_CBDT:=FT_MAKE_TAG('C','B','D','T');
  end;

{ was #define dname def_expr }
function TTAG_CBLC : longint; { return type might be wrong }
  begin
    TTAG_CBLC:=FT_MAKE_TAG('C','B','L','C');
  end;

{ was #define dname def_expr }
function TTAG_CFF : longint; { return type might be wrong }
  begin
    TTAG_CFF:=FT_MAKE_TAG('C','F','F',' ');
  end;

{ was #define dname def_expr }
function TTAG_CFF2 : longint; { return type might be wrong }
  begin
    TTAG_CFF2:=FT_MAKE_TAG('C','F','F','2');
  end;

{ was #define dname def_expr }
function TTAG_CID : longint; { return type might be wrong }
  begin
    TTAG_CID:=FT_MAKE_TAG('C','I','D',' ');
  end;

{ was #define dname def_expr }
function TTAG_cmap : longint; { return type might be wrong }
  begin
    TTAG_cmap:=FT_MAKE_TAG('c','m','a','p');
  end;

{ was #define dname def_expr }
function TTAG_COLR : longint; { return type might be wrong }
  begin
    TTAG_COLR:=FT_MAKE_TAG('C','O','L','R');
  end;

{ was #define dname def_expr }
function TTAG_CPAL : longint; { return type might be wrong }
  begin
    TTAG_CPAL:=FT_MAKE_TAG('C','P','A','L');
  end;

{ was #define dname def_expr }
function TTAG_cvar : longint; { return type might be wrong }
  begin
    TTAG_cvar:=FT_MAKE_TAG('c','v','a','r');
  end;

{ was #define dname def_expr }
function TTAG_cvt : longint; { return type might be wrong }
  begin
    TTAG_cvt:=FT_MAKE_TAG('c','v','t',' ');
  end;

{ was #define dname def_expr }
function TTAG_DSIG : longint; { return type might be wrong }
  begin
    TTAG_DSIG:=FT_MAKE_TAG('D','S','I','G');
  end;

{ was #define dname def_expr }
function TTAG_EBDT : longint; { return type might be wrong }
  begin
    TTAG_EBDT:=FT_MAKE_TAG('E','B','D','T');
  end;

{ was #define dname def_expr }
function TTAG_EBLC : longint; { return type might be wrong }
  begin
    TTAG_EBLC:=FT_MAKE_TAG('E','B','L','C');
  end;

{ was #define dname def_expr }
function TTAG_EBSC : longint; { return type might be wrong }
  begin
    TTAG_EBSC:=FT_MAKE_TAG('E','B','S','C');
  end;

{ was #define dname def_expr }
function TTAG_feat : longint; { return type might be wrong }
  begin
    TTAG_feat:=FT_MAKE_TAG('f','e','a','t');
  end;

{ was #define dname def_expr }
function TTAG_FOND : longint; { return type might be wrong }
  begin
    TTAG_FOND:=FT_MAKE_TAG('F','O','N','D');
  end;

{ was #define dname def_expr }
function TTAG_fpgm : longint; { return type might be wrong }
  begin
    TTAG_fpgm:=FT_MAKE_TAG('f','p','g','m');
  end;

{ was #define dname def_expr }
function TTAG_fvar : longint; { return type might be wrong }
  begin
    TTAG_fvar:=FT_MAKE_TAG('f','v','a','r');
  end;

{ was #define dname def_expr }
function TTAG_gasp : longint; { return type might be wrong }
  begin
    TTAG_gasp:=FT_MAKE_TAG('g','a','s','p');
  end;

{ was #define dname def_expr }
function TTAG_GDEF : longint; { return type might be wrong }
  begin
    TTAG_GDEF:=FT_MAKE_TAG('G','D','E','F');
  end;

{ was #define dname def_expr }
function TTAG_glyf : longint; { return type might be wrong }
  begin
    TTAG_glyf:=FT_MAKE_TAG('g','l','y','f');
  end;

{ was #define dname def_expr }
function TTAG_GPOS : longint; { return type might be wrong }
  begin
    TTAG_GPOS:=FT_MAKE_TAG('G','P','O','S');
  end;

{ was #define dname def_expr }
function TTAG_GSUB : longint; { return type might be wrong }
  begin
    TTAG_GSUB:=FT_MAKE_TAG('G','S','U','B');
  end;

{ was #define dname def_expr }
function TTAG_gvar : longint; { return type might be wrong }
  begin
    TTAG_gvar:=FT_MAKE_TAG('g','v','a','r');
  end;

{ was #define dname def_expr }
function TTAG_HVAR : longint; { return type might be wrong }
  begin
    TTAG_HVAR:=FT_MAKE_TAG('H','V','A','R');
  end;

{ was #define dname def_expr }
function TTAG_hdmx : longint; { return type might be wrong }
  begin
    TTAG_hdmx:=FT_MAKE_TAG('h','d','m','x');
  end;

{ was #define dname def_expr }
function TTAG_head : longint; { return type might be wrong }
  begin
    TTAG_head:=FT_MAKE_TAG('h','e','a','d');
  end;

{ was #define dname def_expr }
function TTAG_hhea : longint; { return type might be wrong }
  begin
    TTAG_hhea:=FT_MAKE_TAG('h','h','e','a');
  end;

{ was #define dname def_expr }
function TTAG_hmtx : longint; { return type might be wrong }
  begin
    TTAG_hmtx:=FT_MAKE_TAG('h','m','t','x');
  end;

{ was #define dname def_expr }
function TTAG_JSTF : longint; { return type might be wrong }
  begin
    TTAG_JSTF:=FT_MAKE_TAG('J','S','T','F');
  end;

{ was #define dname def_expr }
function TTAG_just : longint; { return type might be wrong }
  begin
    TTAG_just:=FT_MAKE_TAG('j','u','s','t');
  end;

{ was #define dname def_expr }
function TTAG_kern : longint; { return type might be wrong }
  begin
    TTAG_kern:=FT_MAKE_TAG('k','e','r','n');
  end;

{ was #define dname def_expr }
function TTAG_lcar : longint; { return type might be wrong }
  begin
    TTAG_lcar:=FT_MAKE_TAG('l','c','a','r');
  end;

{ was #define dname def_expr }
function TTAG_loca : longint; { return type might be wrong }
  begin
    TTAG_loca:=FT_MAKE_TAG('l','o','c','a');
  end;

{ was #define dname def_expr }
function TTAG_LTSH : longint; { return type might be wrong }
  begin
    TTAG_LTSH:=FT_MAKE_TAG('L','T','S','H');
  end;

{ was #define dname def_expr }
function TTAG_LWFN : longint; { return type might be wrong }
  begin
    TTAG_LWFN:=FT_MAKE_TAG('L','W','F','N');
  end;

{ was #define dname def_expr }
function TTAG_MATH : longint; { return type might be wrong }
  begin
    TTAG_MATH:=FT_MAKE_TAG('M','A','T','H');
  end;

{ was #define dname def_expr }
function TTAG_maxp : longint; { return type might be wrong }
  begin
    TTAG_maxp:=FT_MAKE_TAG('m','a','x','p');
  end;

{ was #define dname def_expr }
function TTAG_META : longint; { return type might be wrong }
  begin
    TTAG_META:=FT_MAKE_TAG('M','E','T','A');
  end;

{ was #define dname def_expr }
function TTAG_MMFX : longint; { return type might be wrong }
  begin
    TTAG_MMFX:=FT_MAKE_TAG('M','M','F','X');
  end;

{ was #define dname def_expr }
function TTAG_MMSD : longint; { return type might be wrong }
  begin
    TTAG_MMSD:=FT_MAKE_TAG('M','M','S','D');
  end;

{ was #define dname def_expr }
function TTAG_mort : longint; { return type might be wrong }
  begin
    TTAG_mort:=FT_MAKE_TAG('m','o','r','t');
  end;

{ was #define dname def_expr }
function TTAG_morx : longint; { return type might be wrong }
  begin
    TTAG_morx:=FT_MAKE_TAG('m','o','r','x');
  end;

{ was #define dname def_expr }
function TTAG_MVAR : longint; { return type might be wrong }
  begin
    TTAG_MVAR:=FT_MAKE_TAG('M','V','A','R');
  end;

{ was #define dname def_expr }
function TTAG_name : longint; { return type might be wrong }
  begin
    TTAG_name:=FT_MAKE_TAG('n','a','m','e');
  end;

{ was #define dname def_expr }
function TTAG_opbd : longint; { return type might be wrong }
  begin
    TTAG_opbd:=FT_MAKE_TAG('o','p','b','d');
  end;

{ was #define dname def_expr }
function TTAG_OS2 : longint; { return type might be wrong }
  begin
    TTAG_OS2:=FT_MAKE_TAG('O','S','/','2');
  end;

{ was #define dname def_expr }
function TTAG_OTTO : longint; { return type might be wrong }
  begin
    TTAG_OTTO:=FT_MAKE_TAG('O','T','T','O');
  end;

{ was #define dname def_expr }
function TTAG_PCLT : longint; { return type might be wrong }
  begin
    TTAG_PCLT:=FT_MAKE_TAG('P','C','L','T');
  end;

{ was #define dname def_expr }
function TTAG_POST : longint; { return type might be wrong }
  begin
    TTAG_POST:=FT_MAKE_TAG('P','O','S','T');
  end;

{ was #define dname def_expr }
function TTAG_post : longint; { return type might be wrong }
  begin
    TTAG_post:=FT_MAKE_TAG('p','o','s','t');
  end;

{ was #define dname def_expr }
function TTAG_prep : longint; { return type might be wrong }
  begin
    TTAG_prep:=FT_MAKE_TAG('p','r','e','p');
  end;

{ was #define dname def_expr }
function TTAG_prop : longint; { return type might be wrong }
  begin
    TTAG_prop:=FT_MAKE_TAG('p','r','o','p');
  end;

{ was #define dname def_expr }
function TTAG_sbix : longint; { return type might be wrong }
  begin
    TTAG_sbix:=FT_MAKE_TAG('s','b','i','x');
  end;

{ was #define dname def_expr }
function TTAG_sfnt : longint; { return type might be wrong }
  begin
    TTAG_sfnt:=FT_MAKE_TAG('s','f','n','t');
  end;

{ was #define dname def_expr }
function TTAG_SING : longint; { return type might be wrong }
  begin
    TTAG_SING:=FT_MAKE_TAG('S','I','N','G');
  end;

{ was #define dname def_expr }
function TTAG_SVG : longint; { return type might be wrong }
  begin
    TTAG_SVG:=FT_MAKE_TAG('S','V','G',' ');
  end;

{ was #define dname def_expr }
function TTAG_trak : longint; { return type might be wrong }
  begin
    TTAG_trak:=FT_MAKE_TAG('t','r','a','k');
  end;

{ was #define dname def_expr }
function TTAG_true : longint; { return type might be wrong }
  begin
    TTAG_true:=FT_MAKE_TAG('t','r','u','e');
  end;

{ was #define dname def_expr }
function TTAG_ttc : longint; { return type might be wrong }
  begin
    TTAG_ttc:=FT_MAKE_TAG('t','t','c',' ');
  end;

{ was #define dname def_expr }
function TTAG_ttcf : longint; { return type might be wrong }
  begin
    TTAG_ttcf:=FT_MAKE_TAG('t','t','c','f');
  end;

{ was #define dname def_expr }
function TTAG_TYP1 : longint; { return type might be wrong }
  begin
    TTAG_TYP1:=FT_MAKE_TAG('T','Y','P','1');
  end;

{ was #define dname def_expr }
function TTAG_typ1 : longint; { return type might be wrong }
  begin
    TTAG_typ1:=FT_MAKE_TAG('t','y','p','1');
  end;

{ was #define dname def_expr }
function TTAG_VDMX : longint; { return type might be wrong }
  begin
    TTAG_VDMX:=FT_MAKE_TAG('V','D','M','X');
  end;

{ was #define dname def_expr }
function TTAG_vhea : longint; { return type might be wrong }
  begin
    TTAG_vhea:=FT_MAKE_TAG('v','h','e','a');
  end;

{ was #define dname def_expr }
function TTAG_vmtx : longint; { return type might be wrong }
  begin
    TTAG_vmtx:=FT_MAKE_TAG('v','m','t','x');
  end;

{ was #define dname def_expr }
function TTAG_VVAR : longint; { return type might be wrong }
  begin
    TTAG_VVAR:=FT_MAKE_TAG('V','V','A','R');
  end;

{ was #define dname def_expr }
function TTAG_wOFF : longint; { return type might be wrong }
  begin
    TTAG_wOFF:=FT_MAKE_TAG('w','O','F','F');
  end;

{ was #define dname def_expr }
function TTAG_wOF2 : longint; { return type might be wrong }
  begin
    TTAG_wOF2:=FT_MAKE_TAG('w','O','F','2');
  end;

{ was #define dname def_expr }
function TTAG_0xA5kbd : longint; { return type might be wrong }
  begin
    TTAG_0xA5kbd:=FT_MAKE_TAG($A5,'k','b','d');
  end;

{ was #define dname def_expr }
function TTAG_0xA5lst : longint; { return type might be wrong }
  begin
    TTAG_0xA5lst:=FT_MAKE_TAG($A5,'l','s','t');
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_MAKE_TAG(_x1,_x2,_x3,_x4 : longint) : longint;
begin
  FT_MAKE_TAG:=((((FT_STATIC_BYTE_CAST(FT_Tag,_x1)) shl 24) or ((FT_STATIC_BYTE_CAST(FT_Tag,_x2)) shl 16)) or ((FT_STATIC_BYTE_CAST(FT_Tag,_x3)) shl 8)) or (FT_STATIC_BYTE_CAST(FT_Tag,_x4));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_EMPTY(list : longint) : longint;
begin
  FT_IS_EMPTY:=(list.head)=0;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_BOOL(x : longint) : longint;
begin
  FT_BOOL:=FT_STATIC_CAST(FT_Bool,x<>0);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_ERR_CAT(x,y : longint) : longint;
begin
  FT_ERR_CAT:=FT_ERR_XCAT(x,y);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_ERR(e : longint) : longint;
begin
  FT_ERR:=FT_ERR_CAT(FT_ERR_PREFIX,e);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function FT_ERROR_BASE(x : longint) : Tx;
begin
  FT_ERROR_BASE:=Tx(@($FF));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function FT_ERROR_MODULE(x : longint) : Tx;
begin
  FT_ERROR_MODULE:=Tx(@($FF00));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_ERR_EQ(x,e : longint) : longint;
begin
  FT_ERR_EQ:=(FT_ERROR_BASE(x))=(FT_ERROR_BASE(FT_ERR(e)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_ERR_NEQ(x,e : longint) : longint;
begin
  FT_ERR_NEQ:=(FT_ERROR_BASE(x))<>(FT_ERROR_BASE(FT_ERR(e)));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_CURVE_TAG(flag : longint) : longint;
begin
  FT_CURVE_TAG:=flag and $03;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_HORIZONTAL(face : longint) : longint;
begin
  FT_HAS_HORIZONTAL:= not ( not ((face^.face_flags) and FT_FACE_FLAG_HORIZONTAL));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_VERTICAL(face : longint) : longint;
begin
  FT_HAS_VERTICAL:= not ( not ((face^.face_flags) and FT_FACE_FLAG_VERTICAL));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_KERNING(face : longint) : longint;
begin
  FT_HAS_KERNING:= not ( not ((face^.face_flags) and FT_FACE_FLAG_KERNING));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_SCALABLE(face : longint) : longint;
begin
  FT_IS_SCALABLE:= not ( not ((face^.face_flags) and FT_FACE_FLAG_SCALABLE));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_SFNT(face : longint) : longint;
begin
  FT_IS_SFNT:= not ( not ((face^.face_flags) and FT_FACE_FLAG_SFNT));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_FIXED_WIDTH(face : longint) : longint;
begin
  FT_IS_FIXED_WIDTH:= not ( not ((face^.face_flags) and FT_FACE_FLAG_FIXED_WIDTH));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_FIXED_SIZES(face : longint) : longint;
begin
  FT_HAS_FIXED_SIZES:= not ( not ((face^.face_flags) and FT_FACE_FLAG_FIXED_SIZES));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_FAST_GLYPHS(face : longint) : longint;
begin
  FT_HAS_FAST_GLYPHS:=0;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_GLYPH_NAMES(face : longint) : longint;
begin
  FT_HAS_GLYPH_NAMES:= not ( not ((face^.face_flags) and FT_FACE_FLAG_GLYPH_NAMES));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_MULTIPLE_MASTERS(face : longint) : longint;
begin
  FT_HAS_MULTIPLE_MASTERS:= not ( not ((face^.face_flags) and FT_FACE_FLAG_MULTIPLE_MASTERS));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_NAMED_INSTANCE(face : longint) : longint;
begin
  FT_IS_NAMED_INSTANCE:= not ( not ((face^.face_index) and $7FFF0000));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_VARIATION(face : longint) : longint;
begin
  FT_IS_VARIATION:= not ( not ((face^.face_flags) and FT_FACE_FLAG_VARIATION));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_CID_KEYED(face : longint) : longint;
begin
  FT_IS_CID_KEYED:= not ( not ((face^.face_flags) and FT_FACE_FLAG_CID_KEYED));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_IS_TRICKY(face : longint) : longint;
begin
  FT_IS_TRICKY:= not ( not ((face^.face_flags) and FT_FACE_FLAG_TRICKY));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_COLOR(face : longint) : longint;
begin
  FT_HAS_COLOR:= not ( not ((face^.face_flags) and FT_FACE_FLAG_COLOR));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_SVG(face : longint) : longint;
begin
  FT_HAS_SVG:= not ( not ((face^.face_flags) and FT_FACE_FLAG_SVG));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_SBIX(face : longint) : longint;
begin
  FT_HAS_SBIX:= not ( not ((face^.face_flags) and FT_FACE_FLAG_SBIX));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_HAS_SBIX_OVERLAY(face : longint) : longint;
begin
  FT_HAS_SBIX_OVERLAY:= not ( not ((face^.face_flags) and FT_FACE_FLAG_SBIX_OVERLAY));
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_LOAD_TARGET_(x : longint) : longint;
begin
  FT_LOAD_TARGET_:=(FT_STATIC_CAST(FT_Int32,Tx(@(15)))) shl 16;
end;

{ was #define dname def_expr }
function FT_LOAD_TARGET_NORMAL : longint; { return type might be wrong }
  begin
    FT_LOAD_TARGET_NORMAL:=FT_LOAD_TARGET_(FT_RENDER_MODE_NORMAL);
  end;

{ was #define dname def_expr }
function FT_LOAD_TARGET_LIGHT : longint; { return type might be wrong }
  begin
    FT_LOAD_TARGET_LIGHT:=FT_LOAD_TARGET_(FT_RENDER_MODE_LIGHT);
  end;

{ was #define dname def_expr }
function FT_LOAD_TARGET_MONO : longint; { return type might be wrong }
  begin
    FT_LOAD_TARGET_MONO:=FT_LOAD_TARGET_(FT_RENDER_MODE_MONO);
  end;

{ was #define dname def_expr }
function FT_LOAD_TARGET_LCD : longint; { return type might be wrong }
  begin
    FT_LOAD_TARGET_LCD:=FT_LOAD_TARGET_(FT_RENDER_MODE_LCD);
  end;

{ was #define dname def_expr }
function FT_LOAD_TARGET_LCD_V : longint; { return type might be wrong }
  begin
    FT_LOAD_TARGET_LCD_V:=FT_LOAD_TARGET_(FT_RENDER_MODE_LCD_V);
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_LOAD_TARGET_MODE(x : longint) : longint;
begin
  FT_LOAD_TARGET_MODE:=FT_STATIC_CAST(FT_Render_Mode,(x shr 16) and 15);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_ERRORDEF_(e,v,s : longint) : longint;
begin
  FT_ERRORDEF_:=FT_ERRORDEF(FT_ERR_CAT(FT_ERR_PREFIX,e),v+FT_ERR_BASE,s);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_NOERRORDEF_(e,v,s : longint) : longint;
begin
  FT_NOERRORDEF_:=FT_ERRORDEF(FT_ERR_CAT(FT_ERR_PREFIX,e),v,s);
end;

{ was #define dname def_expr }
function FT_PUBLIC_FUNCTION_ATTRIBUTE : longint; { return type might be wrong }
  begin
    FT_PUBLIC_FUNCTION_ATTRIBUTE:=__declspec(dllexport);
  end;

{ was #define dname def_expr }
function FT_PUBLIC_FUNCTION_ATTRIBUTE : longint; { return type might be wrong }
  begin
    FT_PUBLIC_FUNCTION_ATTRIBUTE:=__declspec(dllimport);
  end;

{ was #define dname def_expr }
function FT_PUBLIC_FUNCTION_ATTRIBUTE : longint; { return type might be wrong }
  begin
    FT_PUBLIC_FUNCTION_ATTRIBUTE:=__attribute__(visibility('default'));
  end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   
function FT_UNUSED(arg : longint) : longint;
begin
  FT_UNUSED:=arg:=arg;
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function FT_STATIC_CAST(_type,var : longint) : Ttype;
begin
  FT_STATIC_CAST:=Ttype(var);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function FT_REINTERPRET_CAST(_type,var : longint) : Ttype;
begin
  FT_REINTERPRET_CAST:=Ttype(var);
end;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function FT_STATIC_BYTE_CAST(_type,var : longint) : Ttype;
begin
  FT_STATIC_BYTE_CAST:=Ttype(byte(var));
end;


end.

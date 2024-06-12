
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
{ ===========================  ftimage.h  =========================== }
{ }

{  }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function FT_CURVE_TAG(flag : longint) : longint;

{#ifndef FT_IMAGE_TAG }
{#define FT_IMAGE_TAG( value, _x1, _x2, _x3, _x4 )                         \ }
{value = ( ( FT_STATIC_BYTE_CAST( unsigned long, _x1 ) << 24 ) | \ }
{                    ( FT_STATIC_BYTE_CAST( unsigned long, _x2 ) << 16 ) | \ }
{                    ( FT_STATIC_BYTE_CAST( unsigned long, _x3 ) << 8  ) | \ }
{                      FT_STATIC_BYTE_CAST( unsigned long, _x4 )         ) }
{#endif /* FT_IMAGE_TAG */ }
{
  typedef enum  FT_Glyph_Format_
  
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_NONE, 0, 0, 0, 0 ),

    FT_IMAGE_TAG( FT_GLYPH_FORMAT_COMPOSITE, 'c', 'o', 'm', 'p' ),
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_BITMAP,    'b', 'i', 't', 's' ),
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_OUTLINE,   'o', 'u', 't', 'l' ),
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_PLOTTER,   'p', 'l', 'o', 't' ),
    FT_IMAGE_TAG( FT_GLYPH_FORMAT_SVG,       'S', 'V', 'G', ' ' )

   FT_Glyph_Format;
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
{***************************************************************** }
{***************************************************************** }
{****                                                         **** }
{****                       SETUP MACROS                      **** }
{****                                                         **** }
{***************************************************************** }
{***************************************************************** }

const
  FT_ERR_PREFIX = FT_Err_;  
{$endif}
{ FT_ERR_BASE is used as the base for module-specific errors.  }
{                                                              }
{$ifdef FT_CONFIG_OPTION_USE_MODULE_ERRORS}
{$ifndef FT_ERR_BASE}

const
  FT_ERR_BASE = FT_Mod_Err_Base;  
{$endif}
{$else}
{$undef FT_ERR_BASE}

const
  FT_ERR_BASE = 0;  
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

{#ifdef FT_ERROR_START_LIST }
{  FT_ERROR_START_LIST }
{#endif }
{ now include the error codes  }
{$include <freetype/fterrdef.h>}
{#ifdef FT_ERROR_END_LIST }
{  FT_ERROR_END_LIST }
{#endif }
{***************************************************************** }
{***************************************************************** }
{****                                                         **** }
{****                      SIMPLE CLEANUP                     **** }
{****                                                         **** }
{***************************************************************** }
{***************************************************************** }

function FT_Error_String(error_code:TFT_Error):Pchar;cdecl; external freetype_lib;
{  }
{$endif}
{ FT_ERR_PROTOS_DEFINED  }
{$endif}
{ FT_INCLUDE_ERR_PROTOS  }
{$endif}
{ !(FTERRORS_H_ && __FTERRORS_H__)  }
{ END  }
{ }
{ ===========================  ftgzip.h  =========================== }
{ }
{***************************************************************************
 *
 * ftgzip.h
 *
 *   Gzip-compressed stream support.
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{$ifndef FTGZIP_H_}
{$define FTGZIP_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *   gzip
   *
   * @title:
   *   GZIP Streams
   *
   * @abstract:
   *   Using gzip-compressed font files.
   *
   * @description:
   *   In certain builds of the library, gzip compression recognition is
   *   automatically handled when calling @FT_New_Face or @FT_Open_Face.
   *   This means that if no font driver is capable of handling the raw
   *   compressed file, the library will try to open a gzipped stream from it
   *   and re-open the face with it.
   *
   *   The stream implementation is very basic and resets the decompression
   *   process each time seeking backwards is needed within the stream,
   *   which significantly undermines the performance.
   *
   *   This section contains the declaration of Gzip-specific functions.
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_Stream_OpenGzip
   *
   * @description:
   *   Open a new stream to parse gzip-compressed font files.  This is mainly
   *   used to support the compressed `*.pcf.gz` fonts that come with
   *   XFree86.
   *
   * @input:
   *   stream ::
   *     The target embedding stream.
   *
   *   source ::
   *     The source stream.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The source stream must be opened _before_ calling this function.
   *
   *   Calling the internal function `FT_Stream_Close` on the new stream will
   *   **not** call `FT_Stream_Close` on the source stream.  None of the
   *   stream objects will be released to the heap.
   *
   *   This function may return `FT_Err_Unimplemented_Feature` if your build
   *   of FreeType was not compiled with zlib support.
    }

function FT_Stream_OpenGzip(stream:TFT_Stream; source:TFT_Stream):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Gzip_Uncompress
   *
   * @description:
   *   Decompress a zipped input buffer into an output buffer.  This function
   *   is modeled after zlib's `uncompress` function.
   *
   * @input:
   *   memory ::
   *     A FreeType memory handle.
   *
   *   input ::
   *     The input buffer.
   *
   *   input_len ::
   *     The length of the input buffer.
   *
   * @output:
   *   output ::
   *     The output buffer.
   *
   * @inout:
   *   output_len ::
   *     Before calling the function, this is the total size of the output
   *     buffer, which must be large enough to hold the entire uncompressed
   *     data (so the size of the uncompressed data must be known in
   *     advance).  After calling the function, `output_len` is the size of
   *     the used data in `output`.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function may return `FT_Err_Unimplemented_Feature` if your build
   *   of FreeType was not compiled with zlib support.
   *
   * @since:
   *   2.5.1
    }
(* Const before type ignored *)
function FT_Gzip_Uncompress(memory:TFT_Memory; output:PFT_Byte; output_len:PFT_ULong; input:PFT_Byte; input_len:TFT_ULong):TFT_Error;cdecl; external freetype_lib;
{  }
{$endif}
{ FTGZIP_H_  }
{ END  }
{ }
{ ===========================  ftstdlib.h  =========================== }
{ }
{***************************************************************************
 *
 * ftstdlib.h
 *
 *   ANSI-specific library and header configuration file (specification
 *   only).
 *
 * Copyright (C) 2002-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{*************************************************************************
   *
   * This file is used to group all `#includes` to the ANSI~C library that
   * FreeType normally requires.  It also defines macros to rename the
   * standard functions within the FreeType source code.
   *
   * Load a file which defines `FTSTDLIB_H_` before this one to override it.
   *
    }
{$ifndef FTSTDLIB_H_}
{$define FTSTDLIB_H_}
{$include <stddef.h>}

const
  ft_ptrdiff_t = ptrdiff_t;  
{*************************************************************************
   *
   *                          integer limits
   *
   * `UINT_MAX` and `ULONG_MAX` are used to automatically compute the size of
   * `int` and `long` in bytes at compile-time.  So far, this works for all
   * platforms the library has been tested on.  We also check `ULLONG_MAX`
   * to see whether we can use 64-bit `long long` later on.
   *
   * Note that on the extremely rare platforms that do not provide integer
   * types that are _exactly_ 16 and 32~bits wide (e.g., some old Crays where
   * `int` is 36~bits), we do not make any guarantee about the correct
   * behaviour of FreeType~2 with all fonts.
   *
   * In these cases, `ftconfig.h` will refuse to compile anyway with a
   * message like 'couldn't find 32-bit type' or something similar.
   *
    }
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
{*************************************************************************
   *
   *                character and string processing
   *
    }
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
{*************************************************************************
   *
   *                          file handling
   *
    }
{$include <stdio.h>}

const
  FT_FILE = FILE;  
  ft_fclose = fclose;  
  ft_fopen = fopen;  
  ft_fread = fread;  
  ft_fseek = fseek;  
  ft_ftell = ftell;  
  ft_snprintf = snprintf;  
{*************************************************************************
   *
   *                            sorting
   *
    }
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
{$include <stdarg.h>}
{$endif}
{ FTSTDLIB_H_  }
{ END  }
{ }
{ ===========================  ftheader.h  =========================== }
{ }
{***************************************************************************
 *
 * ftheader.h
 *
 *   Build macros of the FreeType 2 library.
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{$ifndef FTHEADER_H_}
{$define FTHEADER_H_}
{@********************************************************************** }
{                                                                        }
{ <Macro>                                                                }
{                                                         }
{                                                                        }
{ <Description>                                                          }
{    This macro is used in association with @ in header     }
{    files to ensure that the declarations within are properly           }
{    encapsulated in an `extern "C"  .. ` block when included from a   }
{    C++ compiler.                                                       }
{                                                                        }
{#ifndef  }
{#  ifdef __cplusplus }
{# //   define   extern "C"  }
{#  else }
{/#  define   /* nothing */ }
{#  endif }
{#endif }
{@********************************************************************** }
{                                                                        }
{ <Macro>                                                                }
{                                                           }
{                                                                        }
{ <Description>                                                          }
{    This macro is used in association with @ in header   }
{    files to ensure that the declarations within are properly           }
{    encapsulated in an `extern "C"  .. ` block when included from a   }
{    C++ compiler.                                                       }
{                                                                        }
{#ifndef  }
{#  ifdef __cplusplus }
{#    define    }
{#  else }
{#   define   /* nothing */ }
{#  endif }
{#endif }
{*************************************************************************
   *
   * Aliases for the FreeType 2 public and configuration files.
   *
    }
{*************************************************************************
   *
   * @section:
   *   header_file_macros
   *
   * @title:
   *   Header File Macros
   *
   * @abstract:
   *   Macro definitions used to `#include` specific header files.
   *
   * @description:
   *   In addition to the normal scheme of including header files like
   *
   *   ```
   *     #include <freetype/freetype.h>
   *     #include <freetype/ftmm.h>
   *     #include <freetype/ftglyph.h>
   *   ```
   *
   *   it is possible to used named macros instead.  They can be used
   *   directly in `#include` statements as in
   *
   *   ```
   *     #include FT_FREETYPE_H
   *     #include FT_MULTIPLE_MASTERS_H
   *     #include FT_GLYPH_H
   *   ```
   *
   *   These macros were introduced to overcome the infamous 8.3~naming rule
   *   required by DOS (and `FT_MULTIPLE_MASTERS_H` is a lot more meaningful
   *   than `ftmm.h`).
   *
    }
{*************************************************************************
   *
   * @macro:
   *   FT_AUTOHINTER_H
   *
   * @description:
   *   A macro used in `#include` statements to name the file containing
   *   structures and macros related to the auto-hinting module.
   *
   *   Deprecated since version~2.9; use @FT_DRIVER_H instead.
   *
    }

const
  FT_AUTOHINTER_H = FT_DRIVER_H;  
{*************************************************************************
   *
   * @macro:
   *   FT_CFF_DRIVER_H
   *
   * @description:
   *   A macro used in `#include` statements to name the file containing
   *   structures and macros related to the CFF driver module.
   *
   *   Deprecated since version~2.9; use @FT_DRIVER_H instead.
   *
    }
  FT_CFF_DRIVER_H = FT_DRIVER_H;  
{*************************************************************************
   *
   * @macro:
   *   FT_TRUETYPE_DRIVER_H
   *
   * @description:
   *   A macro used in `#include` statements to name the file containing
   *   structures and macros related to the TrueType driver module.
   *
   *   Deprecated since version~2.9; use @FT_DRIVER_H instead.
   *
    }
  FT_TRUETYPE_DRIVER_H = FT_DRIVER_H;  
{*************************************************************************
   *
   * @macro:
   *   FT_PCF_DRIVER_H
   *
   * @description:
   *   A macro used in `#include` statements to name the file containing
   *   structures and macros related to the PCF driver module.
   *
   *   Deprecated since version~2.9; use @FT_DRIVER_H instead.
   *
    }
  FT_PCF_DRIVER_H = FT_DRIVER_H;  
{  }
{ These header files don't need to be included by the user.  }
{#define FT_ERROR_DEFINITIONS_H  <freetype/fterrdef.h> }
{#define FT_PARAMETER_TAGS_H     <freetype/ftparams.h> }
{ Deprecated macros.  }
{#define FT_UNPATENTED_HINTING_H   <freetype/ftparams.h> }
{#define FT_TRUETYPE_UNPATENTED_H  <freetype/ftparams.h> }
{ `FT_CACHE_H` is the only header file needed for the cache subsystem.  }
  FT_CACHE_IMAGE_H = FT_CACHE_H;  
  FT_CACHE_SMALL_BITMAPS_H = FT_CACHE_H;  
  FT_CACHE_CHARMAP_H = FT_CACHE_H;  
{ The internals of the cache sub-system are no longer exposed.  We  }
{ default to `FT_CACHE_H` at the moment just in case, but we know   }
{ of no rogue client that uses them.                                }
{                                                                   }
  FT_CACHE_MANAGER_H = FT_CACHE_H;  
  FT_CACHE_INTERNAL_MRU_H = FT_CACHE_H;  
  FT_CACHE_INTERNAL_MANAGER_H = FT_CACHE_H;  
  FT_CACHE_INTERNAL_CACHE_H = FT_CACHE_H;  
  FT_CACHE_INTERNAL_GLYPH_H = FT_CACHE_H;  
  FT_CACHE_INTERNAL_IMAGE_H = FT_CACHE_H;  
  FT_CACHE_INTERNAL_SBITS_H = FT_CACHE_H;  
{ TODO(david): Move this section below to a different header  }
{$ifdef FT2_BUILD_LIBRARY}
{$if defined( _MSC_VER )      /* Visual C++ (and Intel C++) */}
{ We disable the warning `conditional expression is constant' here  }
{ in order to compile cleanly with the maximum level of warnings.   }
{ In particular, the warning complains about stuff like `while(0)'  }
{ which is very useful in macro definitions.  There is no benefit   }
{ in having it enabled.                                             }
(** unsupported pragma#pragma warning( disable : 4127 )*)
{$endif}
{ _MSC_VER  }
{$endif}
{ FT2_BUILD_LIBRARY  }
{$endif}
{ FTHEADER_H_  }
{ END  }
{ }
{ ===========================  ftconfig.h  =========================== }
{ }
{***************************************************************************
 *
 * ftconfig.h
 *
 *   ANSI-specific configuration file (specification only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{*************************************************************************
   *
   * This header file contains a number of macro definitions that are used by
   * the rest of the engine.  Most of the macros here are automatically
   * determined at compile time, and you should not need to change it to port
   * FreeType, except to compile the library with a non-ANSI compiler.
   *
   * Note however that if some specific modifications are needed, we advise
   * you to place a modified copy in your build directory.
   *
   * The build directory is usually `builds/<system>`, and contains
   * system-specific files that are always included first when building the
   * library.
   *
   * This ANSI version should stay in `include/config/`.
   *
    }
{$ifndef FTCONFIG_H_}
{$define FTCONFIG_H_}
{$include <ft2build.h>}
{$include FT_CONFIG_OPTIONS_H}
{$include FT_CONFIG_STANDARD_LIBRARY_H}
{$include <freetype/config/integer-types.h>}
{$include <freetype/config/public-macros.h>}
{$include <freetype/config/mac-support.h>}
{$endif}
{ FTCONFIG_H_  }
{ END  }
{ }
{ ===========================  mac-support.h  =========================== }
{ }
{***************************************************************************
 *
 * config/mac-support.h
 *
 *   Mac/OS X support configuration header.
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{$ifndef FREETYPE_CONFIG_MAC_SUPPORT_H_}
{$define FREETYPE_CONFIG_MAC_SUPPORT_H_}
{*************************************************************************
   *
   * Mac support
   *
   *   This is the only necessary change, so it is defined here instead
   *   providing a new configuration file.
    }
{$if defined( __APPLE__ ) || ( defined( __MWERKS__ ) && defined( macintosh ) )}
{ No Carbon frameworks for 64bit 10.4.x.                          }
{ `AvailabilityMacros.h` is available since Mac OS X 10.2,        }
{ so guess the system version by maximum errno before inclusion.  }
{$include <errno.h>}
{$ifdef ECANCELED /* defined since 10.2 */}
{$include "AvailabilityMacros.h"}
{$endif}
{#if defined( __LP64__ ) && \ }
{    ( MAC_OS_X_VERSION_MIN_REQUIRED <= MAC_OS_X_VERSION_10_4 ) }
{#undef FT_MACINTOSH }
{$endif}
(*** was #elif ****){$else defined( __SC__ ) || defined( __MRC__ )}
{ Classic MacOS compilers  }
{$include "ConditionalMacros.h"}
{$if TARGET_OS_MAC}

const
  FT_MACINTOSH = 1;  
{$endif}
{$endif}
{ Mac support  }
{$endif}
{ FREETYPE_CONFIG_MAC_SUPPORT_H_  }
{ }
{ ===========================  public-macros.h  =========================== }
{ }
{***************************************************************************
 *
 * config/public-macros.h
 *
 *   Define a set of compiler macros used in public FreeType headers.
 *
 * Copyright (C) 2020-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{
   * The definitions in this file are used by the public FreeType headers
   * and thus should be considered part of the public API.
   *
   * Other compiler-specific macro definitions that are not exposed by the
   * FreeType API should go into
   * `include/freetype/internal/compiler-macros.h` instead.
    }
{$ifndef FREETYPE_CONFIG_PUBLIC_MACROS_H_}
{$define FREETYPE_CONFIG_PUBLIC_MACROS_H_}
{
   * `` and `` might have already been defined
   * by `freetype/config/ftheader.h`, but we don't want to include this
   * header here, so redefine the macros here only when needed.  Their
   * definition is very stable, so keeping them in sync with the ones in the
   * header should not be a maintenance issue.
    }
{$ifndef }
{#define   /* empty */ }
{$endif}
{   }
{$ifndef }
{#define   /* empty */ }
{$endif}
{   }
{
   * Mark a function declaration as public.  This ensures it will be
   * properly exported to client code.  Place this before a function
   * declaration.
   *
   * NOTE: This macro should be considered an internal implementation
   * detail, and not part of the FreeType API.  It is only defined here
   * because it is needed by `FT_EXPORT`.
    }
{ Visual C, mingw  }
{$if defined( _WIN32 )}
{$if defined( FT2_BUILD_LIBRARY ) && defined( DLL_EXPORT )}

{ was #define dname def_expr }
function FT_PUBLIC_FUNCTION_ATTRIBUTE : longint; { return type might be wrong }

(*** was #elif ****){$else defined( DLL_IMPORT )}

{ was #define dname def_expr }
function FT_PUBLIC_FUNCTION_ATTRIBUTE : longint; { return type might be wrong }

{$endif}
{ gcc, clang  }
(*** was #elif ****){$else ( defined( __GNUC__ ) && __GNUC__ >= 4 ) || defined( __clang__ )}

{ was #define dname def_expr }
function FT_PUBLIC_FUNCTION_ATTRIBUTE : longint; { return type might be wrong }

{ Sun  }
(*** was #elif ****){$else defined( __SUNPRO_C ) && __SUNPRO_C >= 0x550}

const
  FT_PUBLIC_FUNCTION_ATTRIBUTE = __global;  
{$endif}
{$ifndef FT_PUBLIC_FUNCTION_ATTRIBUTE}
{ empty  }
{$define FT_PUBLIC_FUNCTION_ATTRIBUTE}
{$endif}
{
   * Define a public FreeType API function.  This ensures it is properly
   * exported or imported at build time.  The macro parameter is the
   * function's return type as in:
   *
   *    FT_Bool 
   *   FT_Object_Method( FT_Object  obj,
   *                     ... );
   *
   * NOTE: This requires that all `FT_EXPORT` uses are inside
   * ` ... ` blocks.  This guarantees that the
   * functions are exported with C linkage, even when the header is included
   * by a C++ source file.
    }
{#define  x   FT_PUBLIC_FUNCTION_ATTRIBUTE extern x }
{
   * `FT_UNUSED` indicates that a given parameter is not used -- this is
   * only used to get rid of unpleasant compiler warnings.
   *
   * Technically, this was not meant to be part of the public API, but some
   * third-party code depends on it.
    }
{$ifndef FT_UNUSED}
{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function FT_UNUSED(arg : longint) : longint;

{$endif}
{
   * Support for casts in both C and C++.
    }
{ was #define dname(params) para_def_expr }
{ argument types are unknown }

function FT_STATIC_CAST(_type,var : longint) : Ttype;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function FT_REINTERPRET_CAST(_type,var : longint) : Ttype;

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
function FT_STATIC_BYTE_CAST(_type,var : longint) : Ttype;

{$endif}
{ FREETYPE_CONFIG_PUBLIC_MACROS_H_  }
{ }
{ ===========================  ftoption.h  =========================== }
{ }
{***************************************************************************
 *
 * ftoption.h
 *
 *   User-selectable configuration macros (specification only).
 *
 * Copyright (C) 1996-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{$ifndef FTOPTION_H_}
{$define FTOPTION_H_}
{$include <ft2build.h>}
{*************************************************************************
   *
   *                USER-SELECTABLE CONFIGURATION MACROS
   *
   * This file contains the default configuration macro definitions for a
   * standard build of the FreeType library.  There are three ways to use
   * this file to build project-specific versions of the library_:
   *
   * - You can modify this file by hand, but this is not recommended in
   *   cases where you would like to build several versions of the library
   *   from a single source directory.
   *
   * - You can put a copy of this file in your build directory, more
   *   precisely in `$BUILD/freetype/config/ftoption.h`, where `$BUILD` is
   *   the name of a directory that is included _before_ the FreeType include
   *   path during compilation.
   *
   *   The default FreeType Makefiles use the build directory
   *   `builds/<system>` by default, but you can easily change that for your
   *   own projects.
   *
   * - Copy the file <ft2build.h> to `$BUILD/ft2build.h` and modify it
   *   slightly to pre-define the macro `FT_CONFIG_OPTIONS_H` used to locate
   *   this file during the build.  For example,
   *
   *   ```
   *     #define FT_CONFIG_OPTIONS_H  <myftoptions.h>
   *     #include <freetype/config/ftheader.h>
   *   ```
   *
   *   will use `$BUILD/myftoptions.h` instead of this file for macro
   *   definitions.
   *
   *   Note also that you can similarly pre-define the macro
   *   `FT_CONFIG_MODULES_H` used to locate the file listing of the modules
   *   that are statically linked to the library at compile time.  By
   *   default, this file is `<freetype/config/ftmodule.h>`.
   *
   * We highly recommend using the third method whenever possible.
   *
    }
{*********************************************************************** }
{*********************************************************************** }
{***                                                                 *** }
{*** G E N E R A L   F R E E T Y P E   2   C O N F I G U R A T I O N *** }
{***                                                                 *** }
{*********************************************************************** }
{*********************************************************************** }
{#************************************************************************
   *
   * If you enable this configuration option, FreeType recognizes an
   * environment variable called `FREETYPE_PROPERTIES`, which can be used to
   * control the various font drivers and modules.  The controllable
   * properties are listed in the section @properties.
   *
   * You have to undefine this configuration option on platforms that lack
   * the concept of environment variables (and thus don't have the `getenv`
   * function), for example Windows CE.
   *
   * `FREETYPE_PROPERTIES` has the following syntax form (broken here into
   * multiple lines for better readability).
   *
   * ```
   *   <optional whitespace>
   *   <module-name1> ':'
   *   <property-name1> '=' <property-value1>
   *   <whitespace>
   *   <module-name2> ':'
   *   <property-name2> '=' <property-value2>
   *   ...
   * ```
   *
   * Example:
   *
   * ```
   *   FREETYPE_PROPERTIES=truetype:interpreter-version=35 \
   *                       cff:no-stem-darkening=1
   * ```
   *
    }
{$define FT_CONFIG_OPTION_ENVIRONMENT_PROPERTIES}
{*************************************************************************
   *
   * Uncomment the line below if you want to activate LCD rendering
   * technology similar to ClearType in this build of the library.  This
   * technology triples the resolution in the direction color subpixels.  To
   * mitigate color fringes inherent to this technology, you also need to
   * explicitly set up LCD filtering.
   *
   * When this macro is not defined, FreeType offers alternative LCD
   * rendering technology that produces excellent output.
    }
{ #define FT_CONFIG_OPTION_SUBPIXEL_RENDERING  }
{*************************************************************************
   *
   * Many compilers provide a non-ANSI 64-bit data type that can be used by
   * FreeType to speed up some computations.  However, this will create some
   * problems when compiling the library in strict ANSI mode.
   *
   * For this reason, the use of 64-bit integers is normally disabled when
   * the `__STDC__` macro is defined.  You can however disable this by
   * defining the macro `FT_CONFIG_OPTION_FORCE_INT64` here.
   *
   * For most compilers, this will only create compilation warnings when
   * building the library.
   *
   * ObNote: The compiler-specific 64-bit integers are detected in the
   *         file `ftconfig.h` either statically or through the `configure`
   *         script on supported platforms.
    }
{$undef FT_CONFIG_OPTION_FORCE_INT64}
{*************************************************************************
   *
   * If this macro is defined, do not try to use an assembler version of
   * performance-critical functions (e.g., @FT_MulFix).  You should only do
   * that to verify that the assembler function works properly, or to execute
   * benchmark tests of the various implementations.
    }
{ #define FT_CONFIG_OPTION_NO_ASSEMBLER  }
{*************************************************************************
   *
   * If this macro is defined, try to use an inlined assembler version of the
   * @FT_MulFix function, which is a 'hotspot' when loading and hinting
   * glyphs, and which should be executed as fast as possible.
   *
   * Note that if your compiler or CPU is not supported, this will default to
   * the standard and portable implementation found in `ftcalc.c`.
    }
{$define FT_CONFIG_OPTION_INLINE_MULFIX}
{*************************************************************************
   *
   * LZW-compressed file support.
   *
   *   FreeType now handles font files that have been compressed with the
   *   `compress` program.  This is mostly used to parse many of the PCF
   *   files that come with various X11 distributions.  The implementation
   *   uses NetBSD's `zopen` to partially uncompress the file on the fly (see
   *   `src/lzw/ftgzip.c`).
   *
   *   Define this macro if you want to enable this 'feature'.
    }
{$define FT_CONFIG_OPTION_USE_LZW}
{*************************************************************************
   *
   * Gzip-compressed file support.
   *
   *   FreeType now handles font files that have been compressed with the
   *   `gzip` program.  This is mostly used to parse many of the PCF files
   *   that come with XFree86.  The implementation uses 'zlib' to partially
   *   uncompress the file on the fly (see `src/gzip/ftgzip.c`).
   *
   *   Define this macro if you want to enable this 'feature'.  See also the
   *   macro `FT_CONFIG_OPTION_SYSTEM_ZLIB` below.
    }
{$define FT_CONFIG_OPTION_USE_ZLIB}
{*************************************************************************
   *
   * ZLib library selection
   *
   *   This macro is only used when `FT_CONFIG_OPTION_USE_ZLIB` is defined.
   *   It allows FreeType's 'ftgzip' component to link to the system's
   *   installation of the ZLib library.  This is useful on systems like
   *   Unix or VMS where it generally is already available.
   *
   *   If you let it undefined, the component will use its own copy of the
   *   zlib sources instead.  These have been modified to be included
   *   directly within the component and **not** export external function
   *   names.  This allows you to link any program with FreeType _and_ ZLib
   *   without linking conflicts.
   *
   *   Do not `#undef` this macro here since the build system might define
   *   it for certain configurations only.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
   *
   *   If you use the GNU make build system directly (that is, without the
   *   `configure` script) and you define this macro, you also have to pass
   *   `SYSTEM_ZLIB=yes` as an argument to make.
    }
{ #define FT_CONFIG_OPTION_SYSTEM_ZLIB  }
{*************************************************************************
   *
   * Bzip2-compressed file support.
   *
   *   FreeType now handles font files that have been compressed with the
   *   `bzip2` program.  This is mostly used to parse many of the PCF files
   *   that come with XFree86.  The implementation uses `libbz2` to partially
   *   uncompress the file on the fly (see `src/bzip2/ftbzip2.c`).  Contrary
   *   to gzip, bzip2 currently is not included and need to use the system
   *   available bzip2 implementation.
   *
   *   Define this macro if you want to enable this 'feature'.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
    }
{ #define FT_CONFIG_OPTION_USE_BZIP2  }
{*************************************************************************
   *
   * Define to disable the use of file stream functions and types, `FILE`,
   * `fopen`, etc.  Enables the use of smaller system libraries on embedded
   * systems that have multiple system libraries, some with or without file
   * stream support, in the cases where file stream support is not necessary
   * such as memory loading of font files.
    }
{ #define FT_CONFIG_OPTION_DISABLE_STREAM_SUPPORT  }
{*************************************************************************
   *
   * PNG bitmap support.
   *
   *   FreeType now handles loading color bitmap glyphs in the PNG format.
   *   This requires help from the external libpng library.  Uncompressed
   *   color bitmaps do not need any external libraries and will be supported
   *   regardless of this configuration.
   *
   *   Define this macro if you want to enable this 'feature'.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
    }
{$define FT_CONFIG_OPTION_USE_PNG}
{*************************************************************************
   *
   * HarfBuzz support.
   *
   *   FreeType uses the HarfBuzz library to improve auto-hinting of OpenType
   *   fonts.  If available, many glyphs not directly addressable by a font's
   *   character map will be hinted also.
   *
   *   Define this macro if you want to enable this 'feature'.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
    }
{ #define FT_CONFIG_OPTION_USE_HARFBUZZ  }
{*************************************************************************
   *
   * Brotli support.
   *
   *   FreeType uses the Brotli library to provide support for decompressing
   *   WOFF2 streams.
   *
   *   Define this macro if you want to enable this 'feature'.
   *
   *   If you use a build system like cmake or the `configure` script,
   *   options set by those programs have precedence, overwriting the value
   *   here with the configured one.
    }
{ #define FT_CONFIG_OPTION_USE_BROTLI  }
{*************************************************************************
   *
   * Glyph Postscript Names handling
   *
   *   By default, FreeType 2 is compiled with the 'psnames' module.  This
   *   module is in charge of converting a glyph name string into a Unicode
   *   value, or return a Macintosh standard glyph name for the use with the
   *   TrueType 'post' table.
   *
   *   Undefine this macro if you do not want 'psnames' compiled in your
   *   build of FreeType.  This has the following effects:
   *
   *   - The TrueType driver will provide its own set of glyph names, if you
   *     build it to support postscript names in the TrueType 'post' table,
   *     but will not synthesize a missing Unicode charmap.
   *
   *   - The Type~1 driver will not be able to synthesize a Unicode charmap
   *     out of the glyphs found in the fonts.
   *
   *   You would normally undefine this configuration macro when building a
   *   version of FreeType that doesn't contain a Type~1 or CFF driver.
    }
{$define FT_CONFIG_OPTION_POSTSCRIPT_NAMES}
{*************************************************************************
   *
   * Postscript Names to Unicode Values support
   *
   *   By default, FreeType~2 is built with the 'psnames' module compiled in.
   *   Among other things, the module is used to convert a glyph name into a
   *   Unicode value.  This is especially useful in order to synthesize on
   *   the fly a Unicode charmap from the CFF/Type~1 driver through a big
   *   table named the 'Adobe Glyph List' (AGL).
   *
   *   Undefine this macro if you do not want the Adobe Glyph List compiled
   *   in your 'psnames' module.  The Type~1 driver will not be able to
   *   synthesize a Unicode charmap out of the glyphs found in the fonts.
    }
{$define FT_CONFIG_OPTION_ADOBE_GLYPH_LIST}
{*************************************************************************
   *
   * Support for Mac fonts
   *
   *   Define this macro if you want support for outline fonts in Mac format
   *   (mac dfont, mac resource, macbinary containing a mac resource) on
   *   non-Mac platforms.
   *
   *   Note that the 'FOND' resource isn't checked.
    }
{$define FT_CONFIG_OPTION_MAC_FONTS}
{*************************************************************************
   *
   * Guessing methods to access embedded resource forks
   *
   *   Enable extra Mac fonts support on non-Mac platforms (e.g., GNU/Linux).
   *
   *   Resource forks which include fonts data are stored sometimes in
   *   locations which users or developers don't expected.  In some cases,
   *   resource forks start with some offset from the head of a file.  In
   *   other cases, the actual resource fork is stored in file different from
   *   what the user specifies.  If this option is activated, FreeType tries
   *   to guess whether such offsets or different file names must be used.
   *
   *   Note that normal, direct access of resource forks is controlled via
   *   the `FT_CONFIG_OPTION_MAC_FONTS` option.
    }
{$ifdef FT_CONFIG_OPTION_MAC_FONTS}
{$define FT_CONFIG_OPTION_GUESSING_EMBEDDED_RFORK}
{$endif}
{*************************************************************************
   *
   * Allow the use of `FT_Incremental_Interface` to load typefaces that
   * contain no glyph data, but supply it via a callback function.  This is
   * required by clients supporting document formats which supply font data
   * incrementally as the document is parsed, such as the Ghostscript
   * interpreter for the PostScript language.
    }
{$define FT_CONFIG_OPTION_INCREMENTAL}
{*************************************************************************
   *
   * The size in bytes of the render pool used by the scan-line converter to
   * do all of its work.
    }

const
  FT_RENDER_POOL_SIZE = 16384;  
{*************************************************************************
   *
   * FT_MAX_MODULES
   *
   *   The maximum number of modules that can be registered in a single
   *   FreeType library object.  32~is the default.
    }
  FT_MAX_MODULES = 32;  
{*************************************************************************
   *
   * Debug level
   *
   *   FreeType can be compiled in debug or trace mode.  In debug mode,
   *   errors are reported through the 'ftdebug' component.  In trace mode,
   *   additional messages are sent to the standard output during execution.
   *
   *   Define `FT_DEBUG_LEVEL_ERROR` to build the library in debug mode.
   *   Define `FT_DEBUG_LEVEL_TRACE` to build it in trace mode.
   *
   *   Don't define any of these macros to compile in 'release' mode!
   *
   *   Do not `#undef` these macros here since the build system might define
   *   them for certain configurations only.
    }
{ #define FT_DEBUG_LEVEL_ERROR  }
{ #define FT_DEBUG_LEVEL_TRACE  }
{*************************************************************************
   *
   * Logging
   *
   *   Compiling FreeType in debug or trace mode makes FreeType write error
   *   and trace log messages to `stderr`.  Enabling this macro
   *   automatically forces the `FT_DEBUG_LEVEL_ERROR` and
   *   `FT_DEBUG_LEVEL_TRACE` macros and allows FreeType to write error and
   *   trace log messages to a file instead of `stderr`.  For writing logs
   *   to a file, FreeType uses an the external `dlg` library (the source
   *   code is in `src/dlg`).
   *
   *   This option needs a C99 compiler.
    }
{ #define FT_DEBUG_LOGGING  }
{*************************************************************************
   *
   * Autofitter debugging
   *
   *   If `FT_DEBUG_AUTOFIT` is defined, FreeType provides some means to
   *   control the autofitter behaviour for debugging purposes with global
   *   boolean variables (consequently, you should **never** enable this
   *   while compiling in 'release' mode):
   *
   *   ```
   *     af_debug_disable_horz_hints_
   *     af_debug_disable_vert_hints_
   *     af_debug_disable_blue_hints_
   *   ```
   *
   *   Additionally, the following functions provide dumps of various
   *   internal autofit structures to stdout (using `printf`):
   *
   *   ```
   *     af_glyph_hints_dump_points
   *     af_glyph_hints_dump_segments
   *     af_glyph_hints_dump_edges
   *     af_glyph_hints_get_num_segments
   *     af_glyph_hints_get_segment_offset
   *   ```
   *
   *   As an argument, they use another global variable:
   *
   *   ```
   *     af_debug_hints_
   *   ```
   *
   *   Please have a look at the `ftgrid` demo program to see how those
   *   variables and macros should be used.
   *
   *   Do not `#undef` these macros here since the build system might define
   *   them for certain configurations only.
    }
{ #define FT_DEBUG_AUTOFIT  }
{*************************************************************************
   *
   * Memory Debugging
   *
   *   FreeType now comes with an integrated memory debugger that is capable
   *   of detecting simple errors like memory leaks or double deletes.  To
   *   compile it within your build of the library, you should define
   *   `FT_DEBUG_MEMORY` here.
   *
   *   Note that the memory debugger is only activated at runtime when when
   *   the _environment_ variable `FT2_DEBUG_MEMORY` is defined also!
   *
   *   Do not `#undef` this macro here since the build system might define it
   *   for certain configurations only.
    }
{ #define FT_DEBUG_MEMORY  }
{*************************************************************************
   *
   * Module errors
   *
   *   If this macro is set (which is _not_ the default), the higher byte of
   *   an error code gives the module in which the error has occurred, while
   *   the lower byte is the real error code.
   *
   *   Setting this macro makes sense for debugging purposes only, since it
   *   would break source compatibility of certain programs that use
   *   FreeType~2.
   *
   *   More details can be found in the files `ftmoderr.h` and `fterrors.h`.
    }
{$undef FT_CONFIG_OPTION_USE_MODULE_ERRORS}
{*************************************************************************
   *
   * OpenType SVG Glyph Support
   *
   *   Setting this macro enables support for OpenType SVG glyphs.  By
   *   default, FreeType can only fetch SVG documents.  However, it can also
   *   render them if external rendering hook functions are plugged in at
   *   runtime.
   *
   *   More details on the hooks can be found in file `otsvg.h`.
    }
{$define FT_CONFIG_OPTION_SVG}
{*************************************************************************
   *
   * Error Strings
   *
   *   If this macro is set, `FT_Error_String` will return meaningful
   *   descriptions.  This is not enabled by default to reduce the overall
   *   size of FreeType.
   *
   *   More details can be found in the file `fterrors.h`.
    }
{ #define FT_CONFIG_OPTION_ERROR_STRINGS  }
{*********************************************************************** }
{*********************************************************************** }
{***                                                                 *** }
{***        S F N T   D R I V E R    C O N F I G U R A T I O N       *** }
{***                                                                 *** }
{*********************************************************************** }
{*********************************************************************** }
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_EMBEDDED_BITMAPS` if you want to support
   * embedded bitmaps in all formats using the 'sfnt' module (namely
   * TrueType~& OpenType).
    }
{$define TT_CONFIG_OPTION_EMBEDDED_BITMAPS}
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_COLOR_LAYERS` if you want to support colored
   * outlines (from the 'COLR'/'CPAL' tables) in all formats using the 'sfnt'
   * module (namely TrueType~& OpenType).
    }
{$define TT_CONFIG_OPTION_COLOR_LAYERS}
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_POSTSCRIPT_NAMES` if you want to be able to
   * load and enumerate Postscript names of glyphs in a TrueType or OpenType
   * file.
   *
   * Note that if you do not compile the 'psnames' module by undefining the
   * above `FT_CONFIG_OPTION_POSTSCRIPT_NAMES` macro, the 'sfnt' module will
   * contain additional code to read the PostScript name table from a font.
   *
   * (By default, the module uses 'psnames' to extract glyph names.)
    }
{$define TT_CONFIG_OPTION_POSTSCRIPT_NAMES}
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_SFNT_NAMES` if your applications need to access
   * the internal name table in a SFNT-based format like TrueType or
   * OpenType.  The name table contains various strings used to describe the
   * font, like family name, copyright, version, etc.  It does not contain
   * any glyph name though.
   *
   * Accessing SFNT names is done through the functions declared in
   * `ftsnames.h`.
    }
{$define TT_CONFIG_OPTION_SFNT_NAMES}
{*************************************************************************
   *
   * TrueType CMap support
   *
   *   Here you can fine-tune which TrueType CMap table format shall be
   *   supported.
    }
{$define TT_CONFIG_CMAP_FORMAT_0}
{$define TT_CONFIG_CMAP_FORMAT_2}
{$define TT_CONFIG_CMAP_FORMAT_4}
{$define TT_CONFIG_CMAP_FORMAT_6}
{$define TT_CONFIG_CMAP_FORMAT_8}
{$define TT_CONFIG_CMAP_FORMAT_10}
{$define TT_CONFIG_CMAP_FORMAT_12}
{$define TT_CONFIG_CMAP_FORMAT_13}
{$define TT_CONFIG_CMAP_FORMAT_14}
{*********************************************************************** }
{*********************************************************************** }
{***                                                                 *** }
{***    T R U E T Y P E   D R I V E R    C O N F I G U R A T I O N   *** }
{***                                                                 *** }
{*********************************************************************** }
{*********************************************************************** }
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_BYTECODE_INTERPRETER` if you want to compile a
   * bytecode interpreter in the TrueType driver.
   *
   * By undefining this, you will only compile the code necessary to load
   * TrueType glyphs without hinting.
   *
   * Do not `#undef` this macro here, since the build system might define it
   * for certain configurations only.
    }
{$define TT_CONFIG_OPTION_BYTECODE_INTERPRETER}
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_SUBPIXEL_HINTING` if you want to compile
   * subpixel hinting support into the TrueType driver.  This modifies the
   * TrueType hinting mechanism when anything but `FT_RENDER_MODE_MONO` is
   * requested.
   *
   * In particular, it modifies the bytecode interpreter to interpret (or
   * not) instructions in a certain way so that all TrueType fonts look like
   * they do in a Windows ClearType (DirectWrite) environment.  See [1] for a
   * technical overview on what this means.  See `ttinterp.h` for more
   * details on this option.
   *
   * The new default mode focuses on applying a minimal set of rules to all
   * fonts indiscriminately so that modern and web fonts render well while
   * legacy fonts render okay.  The corresponding interpreter version is v40.
   * The so-called Infinality mode (v38) is no longer available in FreeType.
   *
   * By undefining these, you get rendering behavior like on Windows without
   * ClearType, i.e., Windows XP without ClearType enabled and Win9x
   * (interpreter version v35).  Or not, depending on how much hinting blood
   * and testing tears the font designer put into a given font.  If you
   * define one or both subpixel hinting options, you can switch between
   * between v35 and the ones you define (using `FT_Property_Set`).
   *
   * This option requires `TT_CONFIG_OPTION_BYTECODE_INTERPRETER` to be
   * defined.
   *
   * [1]
   * https://www.microsoft.com/typography/cleartype/truetypecleartype.aspx
    }
{$define TT_CONFIG_OPTION_SUBPIXEL_HINTING}
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_COMPONENT_OFFSET_SCALED` to compile the
   * TrueType glyph loader to use Apple's definition of how to handle
   * component offsets in composite glyphs.
   *
   * Apple and MS disagree on the default behavior of component offsets in
   * composites.  Apple says that they should be scaled by the scaling
   * factors in the transformation matrix (roughly, it's more complex) while
   * MS says they should not.  OpenType defines two bits in the composite
   * flags array which can be used to disambiguate, but old fonts will not
   * have them.
   *
   *   https://www.microsoft.com/typography/otspec/glyf.htm
   *   https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6glyf.html
    }
{$undef TT_CONFIG_OPTION_COMPONENT_OFFSET_SCALED}
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_GX_VAR_SUPPORT` if you want to include support
   * for Apple's distortable font technology ('fvar', 'gvar', 'cvar', and
   * 'avar' tables).  Tagged 'Font Variations', this is now part of OpenType
   * also.  This has many similarities to Type~1 Multiple Masters support.
    }
{$define TT_CONFIG_OPTION_GX_VAR_SUPPORT}
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_NO_BORING_EXPANSION` if you want to exclude
   * support for 'boring' OpenType specification expansions.
   *
   *   https://github.com/harfbuzz/boring-expansion-spec
   *
   * Right now, the following features are covered:
   *
   *   - 'avar' version 2.0
   *
   * Most likely, this is a temporary configuration option to be removed in
   * the near future, since it is assumed that eventually those features are
   * added to the OpenType standard.
    }
{ #define TT_CONFIG_OPTION_NO_BORING_EXPANSION  }
{*************************************************************************
   *
   * Define `TT_CONFIG_OPTION_BDF` if you want to include support for an
   * embedded 'BDF~' table within SFNT-based bitmap formats.
    }
{$define TT_CONFIG_OPTION_BDF}
{*************************************************************************
   *
   * Option `TT_CONFIG_OPTION_MAX_RUNNABLE_OPCODES` controls the maximum
   * number of bytecode instructions executed for a single run of the
   * bytecode interpreter, needed to prevent infinite loops.  You don't want
   * to change this except for very special situations (e.g., making a
   * library fuzzer spend less time to handle broken fonts).
   *
   * It is not expected that this value is ever modified by a configuring
   * script; instead, it gets surrounded with `#ifndef ... #endif` so that
   * the value can be set as a preprocessor option on the compiler's command
   * line.
    }
{$ifndef TT_CONFIG_OPTION_MAX_RUNNABLE_OPCODES}

const
  TT_CONFIG_OPTION_MAX_RUNNABLE_OPCODES = 1000000;  
{$endif}
{*************************************************************************
   *
   * Option `TT_CONFIG_OPTION_GPOS_KERNING` enables a basic GPOS kerning
   * implementation (for TrueType fonts only).  With this defined, FreeType
   * is able to get kerning pair data from the GPOS 'kern' feature as well as
   * legacy 'kern' tables; without this defined, FreeType will only be able
   * to use legacy 'kern' tables.
   *
   * Note that FreeType does not support more advanced GPOS layout features;
   * even the 'kern' feature implemented here doesn't handle more
   * sophisticated kerning variants.  Use a higher-level library like
   * HarfBuzz instead for that.
    }
{ #define TT_CONFIG_OPTION_GPOS_KERNING  }
{*********************************************************************** }
{*********************************************************************** }
{***                                                                 *** }
{***      T Y P E 1   D R I V E R    C O N F I G U R A T I O N       *** }
{***                                                                 *** }
{*********************************************************************** }
{*********************************************************************** }
{*************************************************************************
   *
   * `T1_MAX_DICT_DEPTH` is the maximum depth of nest dictionaries and arrays
   * in the Type~1 stream (see `t1load.c`).  A minimum of~4 is required.
    }

const
  T1_MAX_DICT_DEPTH = 5;  
{*************************************************************************
   *
   * `T1_MAX_SUBRS_CALLS` details the maximum number of nested sub-routine
   * calls during glyph loading.
    }
  T1_MAX_SUBRS_CALLS = 16;  
{*************************************************************************
   *
   * `T1_MAX_CHARSTRING_OPERANDS` is the charstring stack's capacity.  A
   * minimum of~16 is required.
   *
   * The Chinese font 'MingTiEG-Medium' (covering the CNS 11643 character
   * set) needs 256.
    }
  T1_MAX_CHARSTRINGS_OPERANDS = 256;  
{*************************************************************************
   *
   * Define this configuration macro if you want to prevent the compilation
   * of the 't1afm' module, which is in charge of reading Type~1 AFM files
   * into an existing face.  Note that if set, the Type~1 driver will be
   * unable to produce kerning distances.
    }
{$undef T1_CONFIG_OPTION_NO_AFM}
{*************************************************************************
   *
   * Define this configuration macro if you want to prevent the compilation
   * of the Multiple Masters font support in the Type~1 driver.
    }
{$undef T1_CONFIG_OPTION_NO_MM_SUPPORT}
{*************************************************************************
   *
   * `T1_CONFIG_OPTION_OLD_ENGINE` controls whether the pre-Adobe Type~1
   * engine gets compiled into FreeType.  If defined, it is possible to
   * switch between the two engines using the `hinting-engine` property of
   * the 'type1' driver module.
    }
{ #define T1_CONFIG_OPTION_OLD_ENGINE  }
{*********************************************************************** }
{*********************************************************************** }
{***                                                                 *** }
{***         C F F   D R I V E R    C O N F I G U R A T I O N        *** }
{***                                                                 *** }
{*********************************************************************** }
{*********************************************************************** }
{*************************************************************************
   *
   * Using `CFF_CONFIG_OPTION_DARKENING_PARAMETER_X,Y1,2,3,4` it is
   * possible to set up the default values of the four control points that
   * define the stem darkening behaviour of the (new) CFF engine.  For more
   * details please read the documentation of the `darkening-parameters`
   * property (file `ftdriver.h`), which allows the control at run-time.
   *
   * Do **not** undefine these macros!
    }
  CFF_CONFIG_OPTION_DARKENING_PARAMETER_X1 = 500;  
  CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y1 = 400;  
  CFF_CONFIG_OPTION_DARKENING_PARAMETER_X2 = 1000;  
  CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y2 = 275;  
  CFF_CONFIG_OPTION_DARKENING_PARAMETER_X3 = 1667;  
  CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y3 = 275;  
  CFF_CONFIG_OPTION_DARKENING_PARAMETER_X4 = 2333;  
  CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y4 = 0;  
{*************************************************************************
   *
   * `CFF_CONFIG_OPTION_OLD_ENGINE` controls whether the pre-Adobe CFF engine
   * gets compiled into FreeType.  If defined, it is possible to switch
   * between the two engines using the `hinting-engine` property of the 'cff'
   * driver module.
    }
{ #define CFF_CONFIG_OPTION_OLD_ENGINE  }
{*********************************************************************** }
{*********************************************************************** }
{***                                                                 *** }
{***         P C F   D R I V E R    C O N F I G U R A T I O N        *** }
{***                                                                 *** }
{*********************************************************************** }
{*********************************************************************** }
{*************************************************************************
   *
   * There are many PCF fonts just called 'Fixed' which look completely
   * different, and which have nothing to do with each other.  When selecting
   * 'Fixed' in KDE or Gnome one gets results that appear rather random, the
   * style changes often if one changes the size and one cannot select some
   * fonts at all.  This option makes the 'pcf' module prepend the foundry
   * name (plus a space) to the family name.
   *
   * We also check whether we have 'wide' characters; all put together, we
   * get family names like 'Sony Fixed' or 'Misc Fixed Wide'.
   *
   * If this option is activated, it can be controlled with the
   * `no-long-family-names` property of the 'pcf' driver module.
    }
{ #define PCF_CONFIG_OPTION_LONG_FAMILY_NAMES  }
{*********************************************************************** }
{*********************************************************************** }
{***                                                                 *** }
{***    A U T O F I T   M O D U L E    C O N F I G U R A T I O N     *** }
{***                                                                 *** }
{*********************************************************************** }
{*********************************************************************** }
{*************************************************************************
   *
   * Compile 'autofit' module with CJK (Chinese, Japanese, Korean) script
   * support.
    }
{$define AF_CONFIG_OPTION_CJK}
{*************************************************************************
   *
   * Compile 'autofit' module with fallback Indic script support, covering
   * some scripts that the 'latin' submodule of the 'autofit' module doesn't
   * (yet) handle.  Currently, this needs option `AF_CONFIG_OPTION_CJK`.
    }
{$ifdef AF_CONFIG_OPTION_CJK}
{$define AF_CONFIG_OPTION_INDIC}
{$endif}
{*************************************************************************
   *
   * Use TrueType-like size metrics for 'light' auto-hinting.
   *
   * It is strongly recommended to avoid this option, which exists only to
   * help some legacy applications retain its appearance and behaviour with
   * respect to auto-hinted TrueType fonts.
   *
   * The very reason this option exists at all are GNU/Linux distributions
   * like Fedora that did not un-patch the following change (which was
   * present in FreeType between versions 2.4.6 and 2.7.1, inclusive).
   *
   * ```
   *   2011-07-16  Steven Chu  <steven.f.chu@gmail.com>
   *
   *     [truetype] Fix metrics on size request for scalable fonts.
   * ```
   *
   * This problematic commit is now reverted (more or less).
    }
{ #define AF_CONFIG_OPTION_TT_SIZE_METRICS  }
{  }
{
   * This macro is obsolete.  Support has been removed in FreeType version
   * 2.5.
    }
{ #define FT_CONFIG_OPTION_OLD_INTERNALS  }
{
   * The next two macros are defined if native TrueType hinting is
   * requested by the definitions above.  Don't change this.
    }
{$ifdef TT_CONFIG_OPTION_BYTECODE_INTERPRETER}
{$define TT_USE_BYTECODE_INTERPRETER}
{$ifdef TT_CONFIG_OPTION_SUBPIXEL_HINTING}
{$define TT_SUPPORT_SUBPIXEL_HINTING_MINIMAL}
{$endif}
{$endif}
{
   * The TT_SUPPORT_COLRV1 macro is defined to indicate to clients that this
   * version of FreeType has support for 'COLR' v1 API.  This definition is
   * useful to FreeType clients that want to build in support for 'COLR' v1
   * depending on a tip-of-tree checkout before it is officially released in
   * FreeType, and while the feature cannot yet be tested against using
   * version macros.  Don't change this macro.  This may be removed once the
   * feature is in a FreeType release version and version macros can be used
   * to test for availability.
    }
{$ifdef TT_CONFIG_OPTION_COLOR_LAYERS}
{$define TT_SUPPORT_COLRV1}
{$endif}
{
   * Check CFF darkening parameters.  The checks are the same as in function
   * `cff_property_set` in file `cffdrivr.c`.

#if CFF_CONFIG_OPTION_DARKENING_PARAMETER_X1 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X2 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X3 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X4 < 0   || \
                                                      \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y1 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y2 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y3 < 0   || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y4 < 0   || \
                                                      \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X1 >        \
      CFF_CONFIG_OPTION_DARKENING_PARAMETER_X2     || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X2 >        \
      CFF_CONFIG_OPTION_DARKENING_PARAMETER_X3     || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_X3 >        \
      CFF_CONFIG_OPTION_DARKENING_PARAMETER_X4     || \
                                                      \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y1 > 500 || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y2 > 500 || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y3 > 500 || \
    CFF_CONFIG_OPTION_DARKENING_PARAMETER_Y4 > 500
#error "Invalid CFF darkening parameters!"
#endif

    }
{$endif}
{ FTOPTION_H_  }
{ END  }
{ }
{ ===========================  integer-types.h  =========================== }
{ ===========================  ftmodule.h  =========================== }
{ }
{ This is a generated file. 
FT_USE_MODULE( FT_Driver_ClassRec, tt_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, t1_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, cff_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, t1cid_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, pfr_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, t42_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, winfnt_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, pcf_driver_class )
FT_USE_MODULE( FT_Driver_ClassRec, bdf_driver_class )
FT_USE_MODULE( FT_Module_Class, sfnt_module_class )
FT_USE_MODULE( FT_Module_Class, autofit_module_class )
FT_USE_MODULE( FT_Module_Class, pshinter_module_class )
FT_USE_MODULE( FT_Renderer_Class, ft_smooth_renderer_class )
FT_USE_MODULE( FT_Renderer_Class, ft_raster1_renderer_class )
FT_USE_MODULE( FT_Renderer_Class, ft_svg_renderer_class )
FT_USE_MODULE( FT_Renderer_Class, ft_sdf_renderer_class )
FT_USE_MODULE( FT_Renderer_Class, ft_bitmap_sdf_renderer_class )
FT_USE_MODULE( FT_Module_Class, psaux_module_class )
FT_USE_MODULE( FT_Module_Class, psnames_module_class )
/* EOF  }

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

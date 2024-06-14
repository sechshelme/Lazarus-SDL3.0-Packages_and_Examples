
----- 11364


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


------ 17155

  /**************************************************************************
   *
   * @enum:
   *   FT_Err_XXX
   *
   */

  /* generic errors */

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
...........

----------- 19271

#ifndef FT_ENC_TAG

#define FT_ENC_TAG( value, a, b, c, d )                             \
          value = ( ( FT_STATIC_BYTE_CAST( FT_UInt32, a ) << 24 ) | \
                    ( FT_STATIC_BYTE_CAST( FT_UInt32, b ) << 16 ) | \
                    ( FT_STATIC_BYTE_CAST( FT_UInt32, c ) <<  8 ) | \
                      FT_STATIC_BYTE_CAST( FT_UInt32, d )         )

#endif /* FT_ENC_TAG */


-------- 19426

  typedef enum  FT_Encoding_
  {
    FT_ENC_TAG( FT_ENCODING_NONE, 0, 0, 0, 0 ),

    FT_ENC_TAG( FT_ENCODING_MS_SYMBOL, 's', 'y', 'm', 'b' ),
    FT_ENC_TAG( FT_ENCODING_UNICODE,   'u', 'n', 'i', 'c' ),

    FT_ENC_TAG( FT_ENCODING_SJIS,    's', 'j', 'i', 's' ),
    FT_ENC_TAG( FT_ENCODING_PRC,     'g', 'b', ' ', ' ' ),
    FT_ENC_TAG( FT_ENCODING_BIG5,    'b', 'i', 'g', '5' ),
    FT_ENC_TAG( FT_ENCODING_WANSUNG, 'w', 'a', 'n', 's' ),
    FT_ENC_TAG( FT_ENCODING_JOHAB,   'j', 'o', 'h', 'a' ),

    /* for backward compatibility */
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
  } FT_Encoding;

------ 23868

#define FT_ANGLE_2PI  ( FT_ANGLE_PI * 2 )


---------26739

  /**************************************************************************
   *
   * @type:
   *   FT_Int32
   *
   * @description:
   *   A typedef for a 32bit signed integer type.  The size depends on the
   *   configuration.
   */
  typedef signed XXX  FT_Int32;


  /**************************************************************************
   *
   * @type:
   *   FT_UInt32
   *
   *   A typedef for a 32bit unsigned integer type.  The size depends on the
   *   configuration.
   */
  typedef unsigned XXX  FT_UInt32;


  /**************************************************************************
   *
   * @type:
   *   FT_Int64
   *
   *   A typedef for a 64bit signed integer type.  The size depends on the
   *   configuration.  Only defined if there is real 64bit support;
   *   otherwise, it gets emulated with a structure (if necessary).
   */
  typedef signed XXX  FT_Int64;


  /**************************************************************************
   *
   * @type:
   *   FT_UInt64
   *
   *   A typedef for a 64bit unsigned integer type.  The size depends on the
   *   configuration.  Only defined if there is real 64bit support;
   *   otherwise, it gets emulated with a structure (if necessary).
   */
  typedef unsigned XXX  FT_UInt64;

---- 26883

/* This is a generated file. */
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
/* EOF */




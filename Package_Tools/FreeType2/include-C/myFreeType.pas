
unit myFreeType;
interface


{ }
{ ===========================  ftcache.h  =========================== }
{ }

{ was #define dname(params) para_def_expr }
{ argument types are unknown }
{ return type might be wrong }   

function FTC_IMAGE_TYPE_COMPARE(d1,d2 : longint) : longint;




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
{ ===========================  freetype.h  =========================== }
{ }




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

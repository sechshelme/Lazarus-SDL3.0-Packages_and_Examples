unit integer_types;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{***************************************************************************
 *
 * config/integer-types.h
 *
 *   FreeType integer types definitions.
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
{$ifndef FREETYPE_CONFIG_INTEGER_TYPES_H_}
{$define FREETYPE_CONFIG_INTEGER_TYPES_H_}
{ There are systems (like the Texas Instruments 'C54x) where a `char`   }
{ has 16~bits.  ANSI~C says that `sizeof(char)` is always~1.  Since an  }
{ `int` has 16~bits also for this system, `sizeof(int)` gives~1 which   }
{ is probably unexpected.                                               }
{                                                                       }
{ `CHAR_BIT` (defined in `limits.h`) gives the number of bits in a      }
{ `char` type.                                                          }
{$ifndef FT_CHAR_BIT}

const
  FT_CHAR_BIT = CHAR_BIT;  
{$endif}
{$ifndef FT_SIZEOF_INT}
{ The size of an `int` type.  }
{$if                                 FT_UINT_MAX == 0xFFFFUL}

const
  FT_SIZEOF_INT = 16/FT_CHAR_BIT;  
(*** was #elif ****){$else                               FT_UINT_MAX == 0xFFFFFFFFUL}

const
  FT_SIZEOF_INT = 32/FT_CHAR_BIT;  
(*** was #elif ****){$else FT_UINT_MAX > 0xFFFFFFFFUL && FT_UINT_MAX == 0xFFFFFFFFFFFFFFFFUL}

const
  FT_SIZEOF_INT = 64/FT_CHAR_BIT;  
{$else}
{$error "Unsupported size of `int' type!"}
{$endif}
{$endif}
{ !defined(FT_SIZEOF_INT)  }
{$ifndef FT_SIZEOF_LONG}
{ The size of a `long` type.  A five-byte `long` (as used e.g. on the  }
{ DM642) is recognized but avoided.                                    }
{$if                                  FT_ULONG_MAX == 0xFFFFFFFFUL}

const
  FT_SIZEOF_LONG = 32/FT_CHAR_BIT;  
(*** was #elif ****){$else FT_ULONG_MAX > 0xFFFFFFFFUL && FT_ULONG_MAX == 0xFFFFFFFFFFUL}

const
  FT_SIZEOF_LONG = 32/FT_CHAR_BIT;  
(*** was #elif ****){$else FT_ULONG_MAX > 0xFFFFFFFFUL && FT_ULONG_MAX == 0xFFFFFFFFFFFFFFFFUL}

const
  FT_SIZEOF_LONG = 64/FT_CHAR_BIT;  
{$else}
{$error "Unsupported size of `long' type!"}
{$endif}
{$endif}
{ !defined(FT_SIZEOF_LONG)  }
{$ifndef FT_SIZEOF_LONG_LONG}
{ The size of a `long long` type if available  }
{$if defined( FT_ULLONG_MAX ) && FT_ULLONG_MAX >= 0xFFFFFFFFFFFFFFFFULL}

const
  FT_SIZEOF_LONG_LONG = 64/FT_CHAR_BIT;  
{$else}

const
  FT_SIZEOF_LONG_LONG = 0;  
{$endif}
{$endif}
{ !defined(FT_SIZEOF_LONG_LONG)  }
{*************************************************************************
   *
   * @section:
   *   basic_types
   *
    }
{*************************************************************************
   *
   * @type:
   *   FT_Int16
   *
   * @description:
   *   A typedef for a 16bit signed integer type.
    }
type
  PFT_Int16 = ^TFT_Int16;
  TFT_Int16 = smallint;
{*************************************************************************
   *
   * @type:
   *   FT_UInt16
   *
   * @description:
   *   A typedef for a 16bit unsigned integer type.
    }

  PFT_UInt16 = ^TFT_UInt16;
  TFT_UInt16 = word;
{  }
{ this #if 0 ... #endif clause is for documentation purposes  }
{#if 0 }
{*************************************************************************
   *
   * @type:
   *   FT_Int32
   *
   * @description:
   *   A typedef for a 32bit signed integer type.  The size depends on the
   *   configuration.
    }

  PFT_Int32 = ^TFT_Int32;
  TFT_Int32 = longint;
{*************************************************************************
   *
   * @type:
   *   FT_UInt32
   *
   *   A typedef for a 32bit unsigned integer type.  The size depends on the
   *   configuration.
    }

  PFT_UInt32 = ^TFT_UInt32;
  TFT_UInt32 = dword;
{*************************************************************************
   *
   * @type:
   *   FT_Int64
   *
   *   A typedef for a 64bit signed integer type.  The size depends on the
   *   configuration.  Only defined if there is real 64bit support;
   *   otherwise, it gets emulated with a structure (if necessary).
    }

  PFT_Int64 = ^TFT_Int64;
  TFT_Int64 = int64;
{*************************************************************************
   *
   * @type:
   *   FT_UInt64
   *
   *   A typedef for a 64bit unsigned integer type.  The size depends on the
   *   configuration.  Only defined if there is real 64bit support;
   *   otherwise, it gets emulated with a structure (if necessary).
    }

  PFT_UInt64 = ^TFT_UInt64;
  TFT_UInt64 = qword;
{  }
{$endif}
{$if FT_SIZEOF_INT == ( 32 / FT_CHAR_BIT )}
type
  PFT_Int32 = ^TFT_Int32;
  TFT_Int32 = longint;

  PFT_UInt32 = ^TFT_UInt32;
  TFT_UInt32 = dword;
(*** was #elif ****){$else FT_SIZEOF_LONG == ( 32 / FT_CHAR_BIT )}
type
  PFT_Int32 = ^TFT_Int32;
  TFT_Int32 = longint;

  PFT_UInt32 = ^TFT_UInt32;
  TFT_UInt32 = dword;
{$else}
{$error "no 32bit type found -- please check your configuration files"}
{$endif}
{ look up an integer type that is at least 32~bits  }
{$if FT_SIZEOF_INT >= ( 32 / FT_CHAR_BIT )}
type
  PFT_Fast = ^TFT_Fast;
  TFT_Fast = longint;

  PFT_UFast = ^TFT_UFast;
  TFT_UFast = dword;
(*** was #elif ****){$else FT_SIZEOF_LONG >= ( 32 / FT_CHAR_BIT )}
type
  PFT_Fast = ^TFT_Fast;
  TFT_Fast = longint;

  PFT_UFast = ^TFT_UFast;
  TFT_UFast = dword;
{$endif}
{ determine whether we have a 64-bit integer type  }
{$if FT_SIZEOF_LONG == ( 64 / FT_CHAR_BIT )}

const
  FT_INT64 = longint;  
  FT_UINT64 = dword;  
(*** was #elif ****){$else FT_SIZEOF_LONG_LONG >= ( 64 / FT_CHAR_BIT )}

const
  FT_INT64 = int64;  
  FT_UINT64 = qword;  
{*************************************************************************
   *
   * A 64-bit data type may create compilation problems if you compile in
   * strict ANSI mode.  To avoid them, we disable other 64-bit data types if
   * `__STDC__` is defined.  You can however ignore this rule by defining the
   * `FT_CONFIG_OPTION_FORCE_INT64` configuration macro.
    }
(*** was #elif ****){$else !defined( __STDC__ ) || defined( FT_CONFIG_OPTION_FORCE_INT64 )}
{$if defined( _MSC_VER ) && _MSC_VER >= 900 /* Visual C++ (and Intel C++) */}
{ this compiler provides the `__int64` type  }

const
  FT_INT64 = int64;  
  FT_UINT64 = qword;  
(*** was #elif ****){$else defined( __BORLANDC__ )  /* Borland C++ */}
{ XXXX: We should probably check the value of `__BORLANDC__` in order  }
{       to test the compiler version.                                  }
{ this compiler provides the `__int64` type  }

const
  FT_INT64 = int64;  
  FT_UINT64 = qword;  
(*** was #elif ****){$else defined( __WATCOMC__ ) && __WATCOMC__ >= 1100  /* Watcom C++ */}

const
  FT_INT64 = int64;  
  FT_UINT64 = qword;  
(*** was #elif ****){$else defined( __MWERKS__ )    /* Metrowerks CodeWarrior */}

const
  FT_INT64 = int64;  
  FT_UINT64 = qword;  
(*** was #elif ****){$else defined( __GNUC__ )}
{ GCC provides the `long long` type  }

const
  FT_INT64 = int64;  
  FT_UINT64 = qword;  
{$endif}
{ !__STDC__  }
{$endif}
{ FT_SIZEOF_LONG == (64 / FT_CHAR_BIT)  }
{$ifdef FT_INT64}
type
  PFT_Int64 = ^TFT_Int64;
  TFT_Int64 = TFT_INT64;

  PFT_UInt64 = ^TFT_UInt64;
  TFT_UInt64 = TFT_UINT64;
{$endif}
{$endif}
{ FREETYPE_CONFIG_INTEGER_TYPES_H_  }

implementation


end.

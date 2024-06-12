
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

{$endif}
{ FTTYPES_H_  }
{ END  }
{ }
{ ===========================  ftotval.h  =========================== }
{ }
{***************************************************************************
 *
 * ftotval.h
 *
 *   FreeType API for validating OpenType tables (specification).
 *
 * Copyright (C) 2004-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{***************************************************************************
 *
 *
 * Warning: This module might be moved to a different library in the
 *          future to avoid a tight dependency between FreeType and the
 *          OpenType specification.
 *
 *
  }
{$ifndef FTOTVAL_H_}
{$define FTOTVAL_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *   ot_validation
   *
   * @title:
   *   OpenType Validation
   *
   * @abstract:
   *   An API to validate OpenType tables.
   *
   * @description:
   *   This section contains the declaration of functions to validate some
   *   OpenType tables (BASE, GDEF, GPOS, GSUB, JSTF, MATH).
   *
   * @order:
   *   FT_OpenType_Validate
   *   FT_OpenType_Free
   *
   *   FT_VALIDATE_OTXXX
   *
    }
{*************************************************************************
   *
   * @enum:
   *    FT_VALIDATE_OTXXX
   *
   * @description:
   *    A list of bit-field constants used with @FT_OpenType_Validate to
   *    indicate which OpenType tables should be validated.
   *
   * @values:
   *    FT_VALIDATE_BASE ::
   *      Validate BASE table.
   *
   *    FT_VALIDATE_GDEF ::
   *      Validate GDEF table.
   *
   *    FT_VALIDATE_GPOS ::
   *      Validate GPOS table.
   *
   *    FT_VALIDATE_GSUB ::
   *      Validate GSUB table.
   *
   *    FT_VALIDATE_JSTF ::
   *      Validate JSTF table.
   *
   *    FT_VALIDATE_MATH ::
   *      Validate MATH table.
   *
   *    FT_VALIDATE_OT ::
   *      Validate all OpenType tables (BASE, GDEF, GPOS, GSUB, JSTF, MATH).
   *
    }

const
  FT_VALIDATE_BASE = $0100;  
  FT_VALIDATE_GDEF = $0200;  
  FT_VALIDATE_GPOS = $0400;  
  FT_VALIDATE_GSUB = $0800;  
  FT_VALIDATE_JSTF = $1000;  
  FT_VALIDATE_MATH = $2000;  
  FT_VALIDATE_OT = ((((FT_VALIDATE_BASE or FT_VALIDATE_GDEF) or FT_VALIDATE_GPOS) or FT_VALIDATE_GSUB) or FT_VALIDATE_JSTF) or FT_VALIDATE_MATH;  
{*************************************************************************
   *
   * @function:
   *    FT_OpenType_Validate
   *
   * @description:
   *    Validate various OpenType tables to assure that all offsets and
   *    indices are valid.  The idea is that a higher-level library that
   *    actually does the text layout can access those tables without error
   *    checking (which can be quite time consuming).
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    validation_flags ::
   *      A bit field that specifies the tables to be validated.  See
   *      @FT_VALIDATE_OTXXX for possible values.
   *
   * @output:
   *    BASE_table ::
   *      A pointer to the BASE table.
   *
   *    GDEF_table ::
   *      A pointer to the GDEF table.
   *
   *    GPOS_table ::
   *      A pointer to the GPOS table.
   *
   *    GSUB_table ::
   *      A pointer to the GSUB table.
   *
   *    JSTF_table ::
   *      A pointer to the JSTF table.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function only works with OpenType fonts, returning an error
   *   otherwise.
   *
   *   After use, the application should deallocate the five tables with
   *   @FT_OpenType_Free.  A `NULL` value indicates that the table either
   *   doesn't exist in the font, or the application hasn't asked for
   *   validation.
    }

function FT_OpenType_Validate(face:TFT_Face; validation_flags:TFT_UInt; BASE_table:PFT_Bytes; GDEF_table:PFT_Bytes; GPOS_table:PFT_Bytes; 
           GSUB_table:PFT_Bytes; JSTF_table:PFT_Bytes):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *    FT_OpenType_Free
   *
   * @description:
   *    Free the buffer allocated by OpenType validator.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   *    table ::
   *      The pointer to the buffer that is allocated by
   *      @FT_OpenType_Validate.
   *
   * @note:
   *   This function must be used to free the buffer allocated by
   *   @FT_OpenType_Validate only.
    }
procedure FT_OpenType_Free(face:TFT_Face; table:TFT_Bytes);cdecl; external freetype_lib;
{  }
{$endif}
{ FTOTVAL_H_  }
{ END  }
{ }
{ ===========================  ftlist.h  =========================== }
{ }
{***************************************************************************
 *
 * ftlist.h
 *
 *   Generic list support for FreeType (specification).
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
   * This file implements functions relative to list processing.  Its data
   * structures are defined in `freetype.h`.
   *
    }
{$ifndef FTLIST_H_}
{$define FTLIST_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *   list_processing
   *
   * @title:
   *   List Processing
   *
   * @abstract:
   *   Simple management of lists.
   *
   * @description:
   *   This section contains various definitions related to list processing
   *   using doubly-linked nodes.
   *
   * @order:
   *   FT_List
   *   FT_ListNode
   *   FT_ListRec
   *   FT_ListNodeRec
   *
   *   FT_List_Add
   *   FT_List_Insert
   *   FT_List_Find
   *   FT_List_Remove
   *   FT_List_Up
   *   FT_List_Iterate
   *   FT_List_Iterator
   *   FT_List_Finalize
   *   FT_List_Destructor
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_List_Find
   *
   * @description:
   *   Find the list node for a given listed object.
   *
   * @input:
   *   list ::
   *     A pointer to the parent list.
   *   data ::
   *     The address of the listed object.
   *
   * @return:
   *   List node.  `NULL` if it wasn't found.
    }

function FT_List_Find(list:TFT_List; data:pointer):TFT_ListNode;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_List_Add
   *
   * @description:
   *   Append an element to the end of a list.
   *
   * @inout:
   *   list ::
   *     A pointer to the parent list.
   *   node ::
   *     The node to append.
    }
procedure FT_List_Add(list:TFT_List; node:TFT_ListNode);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_List_Insert
   *
   * @description:
   *   Insert an element at the head of a list.
   *
   * @inout:
   *   list ::
   *     A pointer to parent list.
   *   node ::
   *     The node to insert.
    }
procedure FT_List_Insert(list:TFT_List; node:TFT_ListNode);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_List_Remove
   *
   * @description:
   *   Remove a node from a list.  This function doesn't check whether the
   *   node is in the list!
   *
   * @input:
   *   node ::
   *     The node to remove.
   *
   * @inout:
   *   list ::
   *     A pointer to the parent list.
    }
procedure FT_List_Remove(list:TFT_List; node:TFT_ListNode);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_List_Up
   *
   * @description:
   *   Move a node to the head/top of a list.  Used to maintain LRU lists.
   *
   * @inout:
   *   list ::
   *     A pointer to the parent list.
   *   node ::
   *     The node to move.
    }
procedure FT_List_Up(list:TFT_List; node:TFT_ListNode);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @functype:
   *   FT_List_Iterator
   *
   * @description:
   *   An FT_List iterator function that is called during a list parse by
   *   @FT_List_Iterate.
   *
   * @input:
   *   node ::
   *     The current iteration list node.
   *
   *   user ::
   *     A typeless pointer passed to @FT_List_Iterate.  Can be used to point
   *     to the iteration's state.
    }
type

  TFT_List_Iterator = function (node:TFT_ListNode; user:pointer):TFT_Error;cdecl;
{*************************************************************************
   *
   * @function:
   *   FT_List_Iterate
   *
   * @description:
   *   Parse a list and calls a given iterator function on each element.
   *   Note that parsing is stopped as soon as one of the iterator calls
   *   returns a non-zero value.
   *
   * @input:
   *   list ::
   *     A handle to the list.
   *   iterator ::
   *     An iterator function, called on each node of the list.
   *   user ::
   *     A user-supplied field that is passed as the second argument to the
   *     iterator.
   *
   * @return:
   *   The result (a FreeType error code) of the last iterator call.
    }

function FT_List_Iterate(list:TFT_List; iterator:TFT_List_Iterator; user:pointer):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @functype:
   *   FT_List_Destructor
   *
   * @description:
   *   An @FT_List iterator function that is called during a list
   *   finalization by @FT_List_Finalize to destroy all elements in a given
   *   list.
   *
   * @input:
   *   system ::
   *     The current system object.
   *
   *   data ::
   *     The current object to destroy.
   *
   *   user ::
   *     A typeless pointer passed to @FT_List_Iterate.  It can be used to
   *     point to the iteration's state.
    }
type

  TFT_List_Destructor = procedure (memory:TFT_Memory; data:pointer; user:pointer);cdecl;
{*************************************************************************
   *
   * @function:
   *   FT_List_Finalize
   *
   * @description:
   *   Destroy all elements in the list as well as the list itself.
   *
   * @input:
   *   list ::
   *     A handle to the list.
   *
   *   destroy ::
   *     A list destructor that will be applied to each element of the list.
   *     Set this to `NULL` if not needed.
   *
   *   memory ::
   *     The current memory object that handles deallocation.
   *
   *   user ::
   *     A user-supplied field that is passed as the last argument to the
   *     destructor.
   *
   * @note:
   *   This function expects that all nodes added by @FT_List_Add or
   *   @FT_List_Insert have been dynamically allocated.
    }

procedure FT_List_Finalize(list:TFT_List; destroy:TFT_List_Destructor; memory:TFT_Memory; user:pointer);cdecl; external freetype_lib;
{  }
{$endif}
{ FTLIST_H_  }
{ END  }
{ }
{ ===========================  ftsizes.h  =========================== }
{ }
{***************************************************************************
 *
 * ftsizes.h
 *
 *   FreeType size objects management (specification).
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
   * Typical application would normally not need to use these functions.
   * However, they have been placed in a public API for the rare cases where
   * they are needed.
   *
    }
{$ifndef FTSIZES_H_}
{$define FTSIZES_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *   sizes_management
   *
   * @title:
   *   Size Management
   *
   * @abstract:
   *   Managing multiple sizes per face.
   *
   * @description:
   *   When creating a new face object (e.g., with @FT_New_Face), an @FT_Size
   *   object is automatically created and used to store all pixel-size
   *   dependent information, available in the `face->size` field.
   *
   *   It is however possible to create more sizes for a given face, mostly
   *   in order to manage several character pixel sizes of the same font
   *   family and style.  See @FT_New_Size and @FT_Done_Size.
   *
   *   Note that @FT_Set_Pixel_Sizes and @FT_Set_Char_Size only modify the
   *   contents of the current 'active' size; you thus need to use
   *   @FT_Activate_Size to change it.
   *
   *   99% of applications won't need the functions provided here, especially
   *   if they use the caching sub-system, so be cautious when using these.
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_New_Size
   *
   * @description:
   *   Create a new size object from a given face object.
   *
   * @input:
   *   face ::
   *     A handle to a parent face object.
   *
   * @output:
   *   asize ::
   *     A handle to a new size object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You need to call @FT_Activate_Size in order to select the new size for
   *   upcoming calls to @FT_Set_Pixel_Sizes, @FT_Set_Char_Size,
   *   @FT_Load_Glyph, @FT_Load_Char, etc.
    }

function FT_New_Size(face:TFT_Face; size:PFT_Size):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Done_Size
   *
   * @description:
   *   Discard a given size object.  Note that @FT_Done_Face automatically
   *   discards all size objects allocated with @FT_New_Size.
   *
   * @input:
   *   size ::
   *     A handle to a target size object.
   *
   * @return:
   *   FreeType error code.  0~means success.
    }
function FT_Done_Size(size:TFT_Size):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Activate_Size
   *
   * @description:
   *   Even though it is possible to create several size objects for a given
   *   face (see @FT_New_Size for details), functions like @FT_Load_Glyph or
   *   @FT_Load_Char only use the one that has been activated last to
   *   determine the 'current character pixel size'.
   *
   *   This function can be used to 'activate' a previously created size
   *   object.
   *
   * @input:
   *   size ::
   *     A handle to a target size object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If `face` is the size's parent face object, this function changes the
   *   value of `face->size` to the input size handle.
    }
function FT_Activate_Size(size:TFT_Size):TFT_Error;cdecl; external freetype_lib;
{  }
{$endif}
{ FTSIZES_H_  }
{ END  }
{ }
{ ===========================  ftmoderr.h  =========================== }
{ }
{***************************************************************************
 *
 * ftmoderr.h
 *
 *   FreeType module error offsets (specification).
 *
 * Copyright (C) 2001-2024 by
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
   * This file is used to define the FreeType module error codes.
   *
   * If the macro `FT_CONFIG_OPTION_USE_MODULE_ERRORS` in `ftoption.h` is
   * set, the lower byte of an error value identifies the error code as
   * usual.  In addition, the higher byte identifies the module.  For
   * example, the error `FT_Err_Invalid_File_Format` has value 0x0003, the
   * error `TT_Err_Invalid_File_Format` has value 0x1303, the error
   * `T1_Err_Invalid_File_Format` has value 0x1403, etc.
   *
   * Note that `FT_Err_Ok`, `TT_Err_Ok`, etc. are always equal to zero,
   * including the high byte.
   *
   * If `FT_CONFIG_OPTION_USE_MODULE_ERRORS` isn't set, the higher byte of an
   * error value is set to zero.
   *
   * To hide the various `XXX_Err_` prefixes in the source code, FreeType
   * provides some macros in `fttypes.h`.
   *
   *   FT_ERR( err )
   *
   *     Add current error module prefix (as defined with the `FT_ERR_PREFIX`
   *     macro) to `err`.  For example, in the BDF module the line
   *
   *     ```
   *       error = FT_ERR( Invalid_Outline );
   *     ```
   *
   *     expands to
   *
   *     ```
   *       error = BDF_Err_Invalid_Outline;
   *     ```
   *
   *     For simplicity, you can always use `FT_Err_Ok` directly instead of
   *     `FT_ERR( Ok )`.
   *
   *   FT_ERR_EQ( errcode, err )
   *   FT_ERR_NEQ( errcode, err )
   *
   *     Compare error code `errcode` with the error `err` for equality and
   *     inequality, respectively.  Example:
   *
   *     ```
   *       if ( FT_ERR_EQ( error, Invalid_Outline ) )
   *         ...
   *     ```
   *
   *     Using this macro you don't have to think about error prefixes.  Of
   *     course, if module errors are not active, the above example is the
   *     same as
   *
   *     ```
   *       if ( error == FT_Err_Invalid_Outline )
   *         ...
   *     ```
   *
   *   FT_ERROR_BASE( errcode )
   *   FT_ERROR_MODULE( errcode )
   *
   *     Get base error and module error code, respectively.
   *
   * It can also be used to create a module error message table easily with
   * something like
   *
   * ```
   *   #undef FTMODERR_H_
   *   #define FT_MODERRDEF( e, v, s )   FT_Mod_Err_ ## e, s ,
   *   #define FT_MODERR_START_LIST     
   *   #define FT_MODERR_END_LIST        0, 0  ;
   *
   *   const struct
   *   
   *     int          mod_err_offset;
   *     const char*  mod_err_msg
   *    ft_mod_errors[] =
   *
   *   #include <freetype/ftmoderr.h>
   * ```
   *
    }
{$ifndef FTMODERR_H_}
{$define FTMODERR_H_}
{***************************************************************** }
{***************************************************************** }
{****                                                         **** }
{****                       SETUP MACROS                      **** }
{****                                                         **** }
{***************************************************************** }
{***************************************************************** }
{$undef  FT_NEED_EXTERN_C}
{$ifndef FT_MODERRDEF}
{#ifdef FT_CONFIG_OPTION_USE_MODULE_ERRORS }
{#define FT_MODERRDEF( e, v, s )  FT_Mod_Err_ ## e = v, }
{#else }
{#define FT_MODERRDEF( e, v, s )  FT_Mod_Err_ ## e = 0, }
{#endif }
{#define FT_MODERR_START_LIST  enum  }
{#define FT_MODERR_END_LIST    FT_Mod_Err_Max ; }
{#ifdef __cplusplus }
{#define FT_NEED_EXTERN_C }
{  extern "C"  }
{#endif }
{$endif}
{ !FT_MODERRDEF  }
{***************************************************************** }
{***************************************************************** }
{****                                                         **** }
{****               LIST MODULE ERROR BASES                   **** }
{****                                                         **** }
{***************************************************************** }
{***************************************************************** }
{#ifdef FT_MODERR_START_LIST }
{  FT_MODERR_START_LIST }
{#endif }
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


#ifdef FT_MODERR_END_LIST
  FT_MODERR_END_LIST
#endif

 }
{***************************************************************** }
{***************************************************************** }
{****                                                         **** }
{****                      CLEANUP                            **** }
{****                                                         **** }
{***************************************************************** }
{***************************************************************** }
{#ifdef FT_NEED_EXTERN_C }
{   }
{#endif }
{$undef FT_MODERR_START_LIST}
{$undef FT_MODERR_END_LIST}
{$undef FT_MODERRDEF}
{$undef FT_NEED_EXTERN_C}
{$endif}
{ FTMODERR_H_  }
{ END  }
{ }
{ ===========================  ftdriver.h  =========================== }
{ }
{***************************************************************************
 *
 * ftdriver.h
 *
 *   FreeType API for controlling driver modules (specification only).
 *
 * Copyright (C) 2017-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{$ifndef FTDRIVER_H_}
{$define FTDRIVER_H_}
{$include <freetype/freetype.h>}
{$include <freetype/ftparams.h>}
{#ifdef FREETYPE_H }
{#error "freetype.h of FreeType 1 has been loaded!" }
{#error "Please fix the directory search order for header files" }
{#error "so that freetype.h of FreeType 2 is found first." }
{#endif }
{*************************************************************************
   *
   * @section:
   *   auto_hinter
   *
   * @title:
   *   The auto-hinter
   *
   * @abstract:
   *   Controlling the auto-hinting module.
   *
   * @description:
   *   While FreeType's auto-hinter doesn't expose API functions by itself,
   *   it is possible to control its behaviour with @FT_Property_Set and
   *   @FT_Property_Get.  The following lists the available properties
   *   together with the necessary macros and structures.
   *
   *   Note that the auto-hinter's module name is 'autofitter' for historical
   *   reasons.
   *
   *   Available properties are @increase-x-height, @no-stem-darkening
   *   (experimental), @darkening-parameters (experimental),
   *   @glyph-to-script-map (experimental), @fallback-script (experimental),
   *   and @default-script (experimental), as documented in the @properties
   *   section.
   *
    }
{*************************************************************************
   *
   * @section:
   *   cff_driver
   *
   * @title:
   *   The CFF driver
   *
   * @abstract:
   *   Controlling the CFF driver module.
   *
   * @description:
   *   While FreeType's CFF driver doesn't expose API functions by itself, it
   *   is possible to control its behaviour with @FT_Property_Set and
   *   @FT_Property_Get.
   *
   *   The CFF driver's module name is 'cff'.
   *
   *   Available properties are @hinting-engine, @no-stem-darkening,
   *   @darkening-parameters, and @random-seed, as documented in the
   *   @properties section.
   *
   *
   *   **Hinting and anti-aliasing principles of the new engine**
   *
   *   The rasterizer is positioning horizontal features (e.g., ascender
   *   height & x-height, or crossbars) on the pixel grid and minimizing the
   *   amount of anti-aliasing applied to them, while placing vertical
   *   features (vertical stems) on the pixel grid without hinting, thus
   *   representing the stem position and weight accurately.  Sometimes the
   *   vertical stems may be only partially black.  In this context,
   *   'anti-aliasing' means that stems are not positioned exactly on pixel
   *   borders, causing a fuzzy appearance.
   *
   *   There are two principles behind this approach.
   *
   *   1) No hinting in the horizontal direction: Unlike 'superhinted'
   *   TrueType, which changes glyph widths to accommodate regular
   *   inter-glyph spacing, Adobe's approach is 'faithful to the design' in
   *   representing both the glyph width and the inter-glyph spacing designed
   *   for the font.  This makes the screen display as close as it can be to
   *   the result one would get with infinite resolution, while preserving
   *   what is considered the key characteristics of each glyph.  Note that
   *   the distances between unhinted and grid-fitted positions at small
   *   sizes are comparable to kerning values and thus would be noticeable
   *   (and distracting) while reading if hinting were applied.
   *
   *   One of the reasons to not hint horizontally is anti-aliasing for LCD
   *   screens: The pixel geometry of modern displays supplies three vertical
   *   subpixels as the eye moves horizontally across each visible pixel.  On
   *   devices where we can be certain this characteristic is present a
   *   rasterizer can take advantage of the subpixels to add increments of
   *   weight.  In Western writing systems this turns out to be the more
   *   critical direction anyway; the weights and spacing of vertical stems
   *   (see above) are central to Armenian, Cyrillic, Greek, and Latin type
   *   designs.  Even when the rasterizer uses greyscale anti-aliasing instead
   *   of color (a necessary compromise when one doesn't know the screen
   *   characteristics), the unhinted vertical features preserve the design's
   *   weight and spacing much better than aliased type would.
   *
   *   2) Alignment in the vertical direction: Weights and spacing along the
   *   y~axis are less critical; what is much more important is the visual
   *   alignment of related features (like cap-height and x-height).  The
   *   sense of alignment for these is enhanced by the sharpness of grid-fit
   *   edges, while the cruder vertical resolution (full pixels instead of
   *   1/3 pixels) is less of a problem.
   *
   *   On the technical side, horizontal alignment zones for ascender,
   *   x-height, and other important height values (traditionally called
   *   'blue zones') as defined in the font are positioned independently,
   *   each being rounded to the nearest pixel edge, taking care of overshoot
   *   suppression at small sizes, stem darkening, and scaling.
   *
   *   Hstems (that is, hint values defined in the font to help align
   *   horizontal features) that fall within a blue zone are said to be
   *   'captured' and are aligned to that zone.  Uncaptured stems are moved
   *   in one of four ways, top edge up or down, bottom edge up or down.
   *   Unless there are conflicting hstems, the smallest movement is taken to
   *   minimize distortion.
   *
    }
{*************************************************************************
   *
   * @section:
   *   pcf_driver
   *
   * @title:
   *   The PCF driver
   *
   * @abstract:
   *   Controlling the PCF driver module.
   *
   * @description:
   *   While FreeType's PCF driver doesn't expose API functions by itself, it
   *   is possible to control its behaviour with @FT_Property_Set and
   *   @FT_Property_Get.  Right now, there is a single property
   *   @no-long-family-names available if FreeType is compiled with
   *   PCF_CONFIG_OPTION_LONG_FAMILY_NAMES.
   *
   *   The PCF driver's module name is 'pcf'.
   *
    }
{*************************************************************************
   *
   * @section:
   *   t1_cid_driver
   *
   * @title:
   *   The Type 1 and CID drivers
   *
   * @abstract:
   *   Controlling the Type~1 and CID driver modules.
   *
   * @description:
   *   It is possible to control the behaviour of FreeType's Type~1 and
   *   Type~1 CID drivers with @FT_Property_Set and @FT_Property_Get.
   *
   *   Behind the scenes, both drivers use the Adobe CFF engine for hinting;
   *   however, the used properties must be specified separately.
   *
   *   The Type~1 driver's module name is 'type1'; the CID driver's module
   *   name is 't1cid'.
   *
   *   Available properties are @hinting-engine, @no-stem-darkening,
   *   @darkening-parameters, and @random-seed, as documented in the
   *   @properties section.
   *
   *   Please see the @cff_driver section for more details on the new hinting
   *   engine.
   *
    }
{*************************************************************************
   *
   * @section:
   *   tt_driver
   *
   * @title:
   *   The TrueType driver
   *
   * @abstract:
   *   Controlling the TrueType driver module.
   *
   * @description:
   *   While FreeType's TrueType driver doesn't expose API functions by
   *   itself, it is possible to control its behaviour with @FT_Property_Set
   *   and @FT_Property_Get.
   *
   *   The TrueType driver's module name is 'truetype'; a single property
   *   @interpreter-version is available, as documented in the @properties
   *   section.
   *
   *   To help understand the differences between interpreter versions, we
   *   introduce a list of definitions, kindly provided by Greg Hitchcock.
   *
   *   _Bi-Level Rendering_
   *
   *   Monochromatic rendering, exclusively used in the early days of
   *   TrueType by both Apple and Microsoft.  Microsoft's GDI interface
   *   supported hinting of the right-side bearing point, such that the
   *   advance width could be non-linear.  Most often this was done to
   *   achieve some level of glyph symmetry.  To enable reasonable
   *   performance (e.g., not having to run hinting on all glyphs just to get
   *   the widths) there was a bit in the head table indicating if the side
   *   bearing was hinted, and additional tables, 'hdmx' and 'LTSH', to cache
   *   hinting widths across multiple sizes and device aspect ratios.
   *
   *   _Font Smoothing_
   *
   *   Microsoft's GDI implementation of anti-aliasing.  Not traditional
   *   anti-aliasing as the outlines were hinted before the sampling.  The
   *   widths matched the bi-level rendering.
   *
   *   _ClearType Rendering_
   *
   *   Technique that uses physical subpixels to improve rendering on LCD
   *   (and other) displays.  Because of the higher resolution, many methods
   *   of improving symmetry in glyphs through hinting the right-side bearing
   *   were no longer necessary.  This lead to what GDI calls 'natural
   *   widths' ClearType, see
   *   http://rastertragedy.com/RTRCh4.htm#Sec21.  Since hinting
   *   has extra resolution, most non-linearity went away, but it is still
   *   possible for hints to change the advance widths in this mode.
   *
   *   _ClearType Compatible Widths_
   *
   *   One of the earliest challenges with ClearType was allowing the
   *   implementation in GDI to be selected without requiring all UI and
   *   documents to reflow.  To address this, a compatible method of
   *   rendering ClearType was added where the font hints are executed once
   *   to determine the width in bi-level rendering, and then re-run in
   *   ClearType, with the difference in widths being absorbed in the font
   *   hints for ClearType (mostly in the white space of hints); see
   *   http://rastertragedy.com/RTRCh4.htm#Sec20.  Somewhat by
   *   definition, compatible width ClearType allows for non-linear widths,
   *   but only when the bi-level version has non-linear widths.
   *
   *   _ClearType Subpixel Positioning_
   *
   *   One of the nice benefits of ClearType is the ability to more crisply
   *   display fractional widths; unfortunately, the GDI model of integer
   *   bitmaps did not support this.  However, the WPF and Direct Write
   *   frameworks do support fractional widths.  DWrite calls this 'natural
   *   mode', not to be confused with GDI's 'natural widths'.  Subpixel
   *   positioning, in the current implementation of Direct Write,
   *   unfortunately does not support hinted advance widths, see
   *   http://rastertragedy.com/RTRCh4.htm#Sec22.  Note that the
   *   TrueType interpreter fully allows the advance width to be adjusted in
   *   this mode, just the DWrite client will ignore those changes.
   *
   *   _ClearType Backward Compatibility_
   *
   *   This is a set of exceptions made in the TrueType interpreter to
   *   minimize hinting techniques that were problematic with the extra
   *   resolution of ClearType; see
   *   http://rastertragedy.com/RTRCh4.htm#Sec1 and
   *   https://www.microsoft.com/typography/cleartype/truetypecleartype.aspx.
   *   This technique is not to be confused with ClearType compatible widths.
   *   ClearType backward compatibility has no direct impact on changing
   *   advance widths, but there might be an indirect impact on disabling
   *   some deltas.  This could be worked around in backward compatibility
   *   mode.
   *
   *   _Native ClearType Mode_
   *
   *   (Not to be confused with 'natural widths'.)  This mode removes all the
   *   exceptions in the TrueType interpreter when running with ClearType.
   *   Any issues on widths would still apply, though.
   *
    }
{*************************************************************************
   *
   * @section:
   *   ot_svg_driver
   *
   * @title:
   *   The SVG driver
   *
   * @abstract:
   *   Controlling the external rendering of OT-SVG glyphs.
   *
   * @description:
   *   By default, FreeType can only load the 'SVG~' table of OpenType fonts
   *   if configuration macro `FT_CONFIG_OPTION_SVG` is defined.  To make it
   *   render SVG glyphs, an external SVG rendering library is needed.  All
   *   details on the interface between FreeType and the external library
   *   via function hooks can be found in section @svg_fonts.
   *
   *   The OT-SVG driver's module name is 'ot-svg'; it supports a single
   *   property called @svg-hooks, documented below in the @properties
   *   section.
   *
    }
{*************************************************************************
   *
   * @section:
   *   properties
   *
   * @title:
   *   Driver properties
   *
   * @abstract:
   *   Controlling driver modules.
   *
   * @description:
   *   Driver modules can be controlled by setting and unsetting properties,
   *   using the functions @FT_Property_Set and @FT_Property_Get.  This
   *   section documents the available properties, together with auxiliary
   *   macros and structures.
   *
    }
{*************************************************************************
   *
   * @enum:
   *   FT_HINTING_XXX
   *
   * @description:
   *   A list of constants used for the @hinting-engine property to select
   *   the hinting engine for CFF, Type~1, and CID fonts.
   *
   * @values:
   *   FT_HINTING_FREETYPE ::
   *     Use the old FreeType hinting engine.
   *
   *   FT_HINTING_ADOBE ::
   *     Use the hinting engine contributed by Adobe.
   *
   * @since:
   *   2.9
   *
    }

const
  FT_HINTING_FREETYPE = 0;  
  FT_HINTING_ADOBE = 1;  
{ these constants (introduced in 2.4.12) are deprecated  }
  FT_CFF_HINTING_FREETYPE = FT_HINTING_FREETYPE;  
  FT_CFF_HINTING_ADOBE = FT_HINTING_ADOBE;  
{*************************************************************************
   *
   * @property:
   *   hinting-engine
   *
   * @description:
   *   Thanks to Adobe, which contributed a new hinting (and parsing) engine,
   *   an application can select between 'freetype' and 'adobe' if compiled
   *   with `CFF_CONFIG_OPTION_OLD_ENGINE`.  If this configuration macro
   *   isn't defined, 'hinting-engine' does nothing.
   *
   *   The same holds for the Type~1 and CID modules if compiled with
   *   `T1_CONFIG_OPTION_OLD_ENGINE`.
   *
   *   For the 'cff' module, the default engine is 'adobe'.  For both the
   *   'type1' and 't1cid' modules, the default engine is 'adobe', too.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable (using values 'adobe' or 'freetype').
   *
   * @example:
   *   The following example code demonstrates how to select Adobe's hinting
   *   engine for the 'cff' module (omitting the error handling).
   *
   *   ```
   *     FT_Library  library;
   *     FT_UInt     hinting_engine = FT_HINTING_ADOBE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "cff",
   *                               "hinting-engine", &hinting_engine );
   *   ```
   *
   * @since:
   *   2.4.12 (for 'cff' module)
   *
   *   2.9 (for 'type1' and 't1cid' modules)
   *
    }
{*************************************************************************
   *
   * @property:
   *   no-stem-darkening
   *
   * @description:
   *   All glyphs that pass through the auto-hinter will be emboldened unless
   *   this property is set to TRUE.  The same is true for the CFF, Type~1,
   *   and CID font modules if the 'Adobe' engine is selected (which is the
   *   default).
   *
   *   Stem darkening emboldens glyphs at smaller sizes to make them more
   *   readable on common low-DPI screens when using linear alpha blending
   *   and gamma correction, see @FT_Render_Glyph.  When not using linear
   *   alpha blending and gamma correction, glyphs will appear heavy and
   *   fuzzy!
   *
   *   Gamma correction essentially lightens fonts since shades of grey are
   *   shifted to higher pixel values (=~higher brightness) to match the
   *   original intention to the reality of our screens.  The side-effect is
   *   that glyphs 'thin out'.  Mac OS~X and Adobe's proprietary font
   *   rendering library implement a counter-measure: stem darkening at
   *   smaller sizes where shades of gray dominate.  By emboldening a glyph
   *   slightly in relation to its pixel size, individual pixels get higher
   *   coverage of filled-in outlines and are therefore 'blacker'.  This
   *   counteracts the 'thinning out' of glyphs, making text remain readable
   *   at smaller sizes.
   *
   *   For the auto-hinter, stem-darkening is experimental currently and thus
   *   switched off by default (that is, `no-stem-darkening` is set to TRUE
   *   by default).  Total consistency with the CFF driver is not achieved
   *   right now because the emboldening method differs and glyphs must be
   *   scaled down on the Y-axis to keep outline points inside their
   *   precomputed blue zones.  The smaller the size (especially 9ppem and
   *   down), the higher the loss of emboldening versus the CFF driver.
   *
   *   Note that stem darkening is never applied if @FT_LOAD_NO_SCALE is set.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable (using values 1 and 0 for 'on' and 'off', respectively).  It
   *   can also be set per face using @FT_Face_Properties with
   *   @FT_PARAM_TAG_STEM_DARKENING.
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_Bool     no_stem_darkening = TRUE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "cff",
   *                               "no-stem-darkening", &no_stem_darkening );
   *   ```
   *
   * @since:
   *   2.4.12 (for 'cff' module)
   *
   *   2.6.2 (for 'autofitter' module)
   *
   *   2.9 (for 'type1' and 't1cid' modules)
   *
    }
{*************************************************************************
   *
   * @property:
   *   darkening-parameters
   *
   * @description:
   *   By default, the Adobe hinting engine, as used by the CFF, Type~1, and
   *   CID font drivers, darkens stems as follows (if the `no-stem-darkening`
   *   property isn't set):
   *
   *   ```
   *     stem width <= 0.5px:   darkening amount = 0.4px
   *     stem width  = 1px:     darkening amount = 0.275px
   *     stem width  = 1.667px: darkening amount = 0.275px
   *     stem width >= 2.333px: darkening amount = 0px
   *   ```
   *
   *   and piecewise linear in-between.  At configuration time, these four
   *   control points can be set with the macro
   *   `CFF_CONFIG_OPTION_DARKENING_PARAMETERS`; the CFF, Type~1, and CID
   *   drivers share these values.  At runtime, the control points can be
   *   changed using the `darkening-parameters` property (see the example
   *   below that demonstrates this for the Type~1 driver).
   *
   *   The x~values give the stem width, and the y~values the darkening
   *   amount.  The unit is 1000th of pixels.  All coordinate values must be
   *   positive; the x~values must be monotonically increasing; the y~values
   *   must be monotonically decreasing and smaller than or equal to 500
   *   (corresponding to half a pixel); the slope of each linear piece must
   *   be shallower than -1 (e.g., -.4).
   *
   *   The auto-hinter provides this property, too, as an experimental
   *   feature.  See @no-stem-darkening for more.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable, using eight comma-separated integers without spaces.  Here
   *   the above example, using `\` to break the line for readability.
   *
   *   ```
   *     FREETYPE_PROPERTIES=\
   *     type1:darkening-parameters=500,300,1000,200,1500,100,2000,0
   *   ```
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_Int      darken_params[8] =   500, 300,   // x1, y1
   *                                      1000, 200,   // x2, y2
   *                                      1500, 100,   // x3, y3
   *                                      2000,   0 ; // x4, y4
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "type1",
   *                               "darkening-parameters", darken_params );
   *   ```
   *
   * @since:
   *   2.5.1 (for 'cff' module)
   *
   *   2.6.2 (for 'autofitter' module)
   *
   *   2.9 (for 'type1' and 't1cid' modules)
   *
    }
{*************************************************************************
   *
   * @property:
   *   random-seed
   *
   * @description:
   *   By default, the seed value for the CFF 'random' operator and the
   *   similar '0 28 callothersubr pop' command for the Type~1 and CID
   *   drivers is set to a random value.  However, mainly for debugging
   *   purposes, it is often necessary to use a known value as a seed so that
   *   the pseudo-random number sequences generated by 'random' are
   *   repeatable.
   *
   *   The `random-seed` property does that.  Its argument is a signed 32bit
   *   integer; if the value is zero or negative, the seed given by the
   *   `intitialRandomSeed` private DICT operator in a CFF file gets used (or
   *   a default value if there is no such operator).  If the value is
   *   positive, use it instead of `initialRandomSeed`, which is consequently
   *   ignored.
   *
   * @note:
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable.  It can also be set per face using @FT_Face_Properties with
   *   @FT_PARAM_TAG_RANDOM_SEED.
   *
   * @since:
   *   2.8 (for 'cff' module)
   *
   *   2.9 (for 'type1' and 't1cid' modules)
   *
    }
{*************************************************************************
   *
   * @property:
   *   no-long-family-names
   *
   * @description:
   *   If `PCF_CONFIG_OPTION_LONG_FAMILY_NAMES` is active while compiling
   *   FreeType, the PCF driver constructs long family names.
   *
   *   There are many PCF fonts just called 'Fixed' which look completely
   *   different, and which have nothing to do with each other.  When
   *   selecting 'Fixed' in KDE or Gnome one gets results that appear rather
   *   random, the style changes often if one changes the size and one cannot
   *   select some fonts at all.  The improve this situation, the PCF module
   *   prepends the foundry name (plus a space) to the family name.  It also
   *   checks whether there are 'wide' characters; all put together, family
   *   names like 'Sony Fixed' or 'Misc Fixed Wide' are constructed.
   *
   *   If `no-long-family-names` is set, this feature gets switched off.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable (using values 1 and 0 for 'on' and 'off', respectively).
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_Bool     no_long_family_names = TRUE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "pcf",
   *                               "no-long-family-names",
   *                               &no_long_family_names );
   *   ```
   *
   * @since:
   *   2.8
    }
{*************************************************************************
   *
   * @enum:
   *   TT_INTERPRETER_VERSION_XXX
   *
   * @description:
   *   A list of constants used for the @interpreter-version property to
   *   select the hinting engine for Truetype fonts.
   *
   *   The numeric value in the constant names represents the version number
   *   as returned by the 'GETINFO' bytecode instruction.
   *
   * @values:
   *   TT_INTERPRETER_VERSION_35 ::
   *     Version~35 corresponds to MS rasterizer v.1.7 as used e.g. in
   *     Windows~98; only grayscale and B/W rasterizing is supported.
   *
   *   TT_INTERPRETER_VERSION_38 ::
   *     Version~38 is the same Version~40. The original 'Infinality' code is
   *     no longer available.
   *
   *   TT_INTERPRETER_VERSION_40 ::
   *     Version~40 corresponds to MS rasterizer v.2.1; it is roughly
   *     equivalent to the hinting provided by DirectWrite ClearType (as can
   *     be found, for example, in Microsoft's Edge Browser on Windows~10).
   *     It is used in FreeType to select the 'minimal' subpixel hinting
   *     code, a stripped-down and higher performance version of the
   *     'Infinality' code.
   *
   * @note:
   *   This property controls the behaviour of the bytecode interpreter and
   *   thus how outlines get hinted.  It does **not** control how glyph get
   *   rasterized!  In particular, it does not control subpixel color
   *   filtering.
   *
   *   If FreeType has not been compiled with the configuration option
   *   `TT_CONFIG_OPTION_SUBPIXEL_HINTING`, selecting version~38 or~40 causes
   *   an `FT_Err_Unimplemented_Feature` error.
   *
   *   Depending on the graphics framework, Microsoft uses different bytecode
   *   and rendering engines.  As a consequence, the version numbers returned
   *   by a call to the 'GETINFO' bytecode instruction are more convoluted
   *   than desired.
   *
   *   Here are two tables that try to shed some light on the possible values
   *   for the MS rasterizer engine, together with the additional features
   *   introduced by it.
   *
   *   ```
   *     GETINFO framework               version feature
   *     -------------------------------------------------------------------
   *         3   GDI (Win 3.1),            v1.0  16-bit, first version
   *             TrueImage
   *        33   GDI (Win NT 3.1),         v1.5  32-bit
   *             HP Laserjet
   *        34   GDI (Win 95)              v1.6  font smoothing,
   *                                             new SCANTYPE opcode
   *        35   GDI (Win 98/2000)         v1.7  (UN)SCALED_COMPONENT_OFFSET
   *                                               bits in composite glyphs
   *        36   MGDI (Win CE 2)           v1.6+ classic ClearType
   *        37   GDI (XP and later),       v1.8  ClearType
   *             GDI+ old (before Vista)
   *        38   GDI+ old (Vista, Win 7),  v1.9  subpixel ClearType,
   *             WPF                             Y-direction ClearType,
   *                                             additional error checking
   *        39   DWrite (before Win 8)     v2.0  subpixel ClearType flags
   *                                               in GETINFO opcode,
   *                                             bug fixes
   *        40   GDI+ (after Win 7),       v2.1  Y-direction ClearType flag
   *             DWrite (Win 8)                    in GETINFO opcode,
   *                                             Gray ClearType
   *   ```
   *
   *   The 'version' field gives a rough orientation only, since some
   *   applications provided certain features much earlier (as an example,
   *   Microsoft Reader used subpixel and Y-direction ClearType already in
   *   Windows 2000).  Similarly, updates to a given framework might include
   *   improved hinting support.
   *
   *   ```
   *      version   sampling          rendering        comment
   *               x        y       x           y
   *     --------------------------------------------------------------
   *       v1.0   normal  normal  B/W           B/W    bi-level
   *       v1.6   high    high    gray          gray   grayscale
   *       v1.8   high    normal  color-filter  B/W    (GDI) ClearType
   *       v1.9   high    high    color-filter  gray   Color ClearType
   *       v2.1   high    normal  gray          B/W    Gray ClearType
   *       v2.1   high    high    gray          gray   Gray ClearType
   *   ```
   *
   *   Color and Gray ClearType are the two available variants of
   *   'Y-direction ClearType', meaning grayscale rasterization along the
   *   Y-direction; the name used in the TrueType specification for this
   *   feature is 'symmetric smoothing'.  'Classic ClearType' is the original
   *   algorithm used before introducing a modified version in Win~XP.
   *   Another name for v1.6's grayscale rendering is 'font smoothing', and
   *   'Color ClearType' is sometimes also called 'DWrite ClearType'.  To
   *   differentiate between today's Color ClearType and the earlier
   *   ClearType variant with B/W rendering along the vertical axis, the
   *   latter is sometimes called 'GDI ClearType'.
   *
   *   'Normal' and 'high' sampling describe the (virtual) resolution to
   *   access the rasterized outline after the hinting process.  'Normal'
   *   means 1 sample per grid line (i.e., B/W).  In the current Microsoft
   *   implementation, 'high' means an extra virtual resolution of 16x16 (or
   *   16x1) grid lines per pixel for bytecode instructions like 'MIRP'.
   *   After hinting, these 16 grid lines are mapped to 6x5 (or 6x1) grid
   *   lines for color filtering if Color ClearType is activated.
   *
   *   Note that 'Gray ClearType' is essentially the same as v1.6's grayscale
   *   rendering.  However, the GETINFO instruction handles it differently:
   *   v1.6 returns bit~12 (hinting for grayscale), while v2.1 returns
   *   bits~13 (hinting for ClearType), 18 (symmetrical smoothing), and~19
   *   (Gray ClearType).  Also, this mode respects bits 2 and~3 for the
   *   version~1 gasp table exclusively (like Color ClearType), while v1.6
   *   only respects the values of version~0 (bits 0 and~1).
   *
   *   Keep in mind that the features of the above interpreter versions might
   *   not map exactly to FreeType features or behavior because it is a
   *   fundamentally different library with different internals.
   *
    }
  TT_INTERPRETER_VERSION_35 = 35;  
  TT_INTERPRETER_VERSION_38 = 38;  
  TT_INTERPRETER_VERSION_40 = 40;  
{*************************************************************************
   *
   * @property:
   *   interpreter-version
   *
   * @description:
   *   Currently, three versions are available, two representing the bytecode
   *   interpreter with subpixel hinting support (old 'Infinality' code and
   *   new stripped-down and higher performance 'minimal' code) and one
   *   without, respectively.  The default is subpixel support if
   *   `TT_CONFIG_OPTION_SUBPIXEL_HINTING` is defined, and no subpixel
   *   support otherwise (since it isn't available then).
   *
   *   If subpixel hinting is on, many TrueType bytecode instructions behave
   *   differently compared to B/W or grayscale rendering (except if 'native
   *   ClearType' is selected by the font).  Microsoft's main idea is to
   *   render at a much increased horizontal resolution, then sampling down
   *   the created output to subpixel precision.  However, many older fonts
   *   are not suited to this and must be specially taken care of by applying
   *   (hardcoded) tweaks in Microsoft's interpreter.
   *
   *   Details on subpixel hinting and some of the necessary tweaks can be
   *   found in Greg Hitchcock's whitepaper at
   *   'https://www.microsoft.com/typography/cleartype/truetypecleartype.aspx'.
   *   Note that FreeType currently doesn't really 'subpixel hint' (6x1, 6x2,
   *   or 6x5 supersampling) like discussed in the paper.  Depending on the
   *   chosen interpreter, it simply ignores instructions on vertical stems
   *   to arrive at very similar results.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   This property can be set via the `FREETYPE_PROPERTIES` environment
   *   variable (using values '35', '38', or '40').
   *
   * @example:
   *   The following example code demonstrates how to deactivate subpixel
   *   hinting (omitting the error handling).
   *
   *   ```
   *     FT_Library  library;
   *     FT_Face     face;
   *     FT_UInt     interpreter_version = TT_INTERPRETER_VERSION_35;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "truetype",
   *                               "interpreter-version",
   *                               &interpreter_version );
   *   ```
   *
   * @since:
   *   2.5
    }
{*************************************************************************
   *
   * @property:
   *   spread
   *
   * @description:
   *   This property of the 'sdf' and 'bsdf' renderers defines how the signed
   *   distance field (SDF) is represented in the output bitmap.  The output
   *   values are calculated as follows, '128 * ( SDF / spread + 1 )', with
   *   the result clamped to the 8-bit range [0..255].  Therefore, 'spread'
   *   is also the maximum euclidean distance from the edge after which the
   *   values are clamped.  The spread is specified in pixels with the
   *   default value of 8.  For accurate SDF texture mapping (interpolation),
   *   the spread should be large enough to accommodate the target grid unit.
   *
   * @example:
   *   The following example code demonstrates how to set the SDF spread
   *   (omitting the error handling).
   *
   *   ```
   *     FT_Library  library;
   *     FT_Int      spread = 2;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "sdf", "spread", &spread );
   *   ```
   *
   * @note
   *   FreeType has two rasterizers for generating SDF, namely:
   *
   *   1. `sdf` for generating SDF directly from glyph's outline, and
   *
   *   2. `bsdf` for generating SDF from rasterized bitmaps.
   *
   *   Depending on the glyph type (i.e., outline or bitmap), one of the two
   *   rasterizers is chosen at runtime and used for generating SDFs.  To
   *   force the use of `bsdf` you should render the glyph with any of the
   *   FreeType's other rendering modes (e.g., `FT_RENDER_MODE_NORMAL`) and
   *   then re-render with `FT_RENDER_MODE_SDF`.
   *
   *   There are some issues with stability and possible failures of the SDF
   *   renderers (specifically `sdf`).
   *
   *   1. The `sdf` rasterizer is sensitive to really small features (e.g.,
   *      sharp turns that are less than 1~pixel) and imperfections in the
   *      glyph's outline, causing artifacts in the final output.
   *
   *   2. The `sdf` rasterizer has limited support for handling intersecting
   *      contours and *cannot* handle self-intersecting contours whatsoever.
   *      Self-intersection happens when a single connected contour
   *      intersects itself at some point; having these in your font
   *      definitely poses a problem to the rasterizer and cause artifacts,
   *      too.
   *
   *   3. Generating SDF for really small glyphs may result in undesirable
   *      output; the pixel grid (which stores distance information) becomes
   *      too coarse.
   *
   *   4. Since the output buffer is normalized, precision at smaller spreads
   *      is greater than precision at larger spread values because the
   *      output range of [0..255] gets mapped to a smaller SDF range.  A
   *      spread of~2 should be sufficient in most cases.
   *
   *   Points (1) and (2) can be avoided by using the `bsdf` rasterizer,
   *   which is more stable than the `sdf` rasterizer in general.
   *
   * @since:
   *   2.11
    }
{*************************************************************************
   *
   * @property:
   *   svg-hooks
   *
   * @description:
   *   Set up the interface between FreeType and an extern SVG rendering
   *   library like 'librsvg'.  All details on the function hooks can be
   *   found in section @svg_fonts.
   *
   * @example:
   *   The following example code expects that the four hook functions
   *   `svg_*` are defined elsewhere.  Error handling is omitted, too.
   *
   *   ```
   *     FT_Library  library;
   *     SVG_RendererHooks  hooks = 
   *                          (SVG_Lib_Init_Func)svg_init,
   *                          (SVG_Lib_Free_Func)svg_free,
   *                          (SVG_Lib_Render_Func)svg_render,
   *                          (SVG_Lib_Preset_Slot_Func)svg_preset_slot ;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "ot-svg",
   *                               "svg-hooks", &hooks );
   *   ```
   *
   * @since:
   *   2.12
    }
{*************************************************************************
   *
   * @property:
   *   glyph-to-script-map
   *
   * @description:
   *   **Experimental only**
   *
   *   The auto-hinter provides various script modules to hint glyphs.
   *   Examples of supported scripts are Latin or CJK.  Before a glyph is
   *   auto-hinted, the Unicode character map of the font gets examined, and
   *   the script is then determined based on Unicode character ranges, see
   *   below.
   *
   *   OpenType fonts, however, often provide much more glyphs than character
   *   codes (small caps, superscripts, ligatures, swashes, etc.), to be
   *   controlled by so-called 'features'.  Handling OpenType features can be
   *   quite complicated and thus needs a separate library on top of
   *   FreeType.
   *
   *   The mapping between glyph indices and scripts (in the auto-hinter
   *   sense, see the @FT_AUTOHINTER_SCRIPT_XXX values) is stored as an array
   *   with `num_glyphs` elements, as found in the font's @FT_Face structure.
   *   The `glyph-to-script-map` property returns a pointer to this array,
   *   which can be modified as needed.  Note that the modification should
   *   happen before the first glyph gets processed by the auto-hinter so
   *   that the global analysis of the font shapes actually uses the modified
   *   mapping.
   *
   * @example:
   *   The following example code demonstrates how to access it (omitting the
   *   error handling).
   *
   *   ```
   *     FT_Library                library;
   *     FT_Face                   face;
   *     FT_Prop_GlyphToScriptMap  prop;
   *
   *
   *     FT_Init_FreeType( &library );
   *     FT_New_Face( library, "foo.ttf", 0, &face );
   *
   *     prop.face = face;
   *
   *     FT_Property_Get( library, "autofitter",
   *                               "glyph-to-script-map", &prop );
   *
   *     // adjust `prop.map' as needed right here
   *
   *     FT_Load_Glyph( face, ..., FT_LOAD_FORCE_AUTOHINT );
   *   ```
   *
   * @since:
   *   2.4.11
   *
    }
{*************************************************************************
   *
   * @enum:
   *   FT_AUTOHINTER_SCRIPT_XXX
   *
   * @description:
   *   **Experimental only**
   *
   *   A list of constants used for the @glyph-to-script-map property to
   *   specify the script submodule the auto-hinter should use for hinting a
   *   particular glyph.
   *
   * @values:
   *   FT_AUTOHINTER_SCRIPT_NONE ::
   *     Don't auto-hint this glyph.
   *
   *   FT_AUTOHINTER_SCRIPT_LATIN ::
   *     Apply the latin auto-hinter.  For the auto-hinter, 'latin' is a very
   *     broad term, including Cyrillic and Greek also since characters from
   *     those scripts share the same design constraints.
   *
   *     By default, characters from the following Unicode ranges are
   *     assigned to this submodule.
   *
   *     ```
   *       U+0020 - U+007F  // Basic Latin (no control characters)
   *       U+00A0 - U+00FF  // Latin-1 Supplement (no control characters)
   *       U+0100 - U+017F  // Latin Extended-A
   *       U+0180 - U+024F  // Latin Extended-B
   *       U+0250 - U+02AF  // IPA Extensions
   *       U+02B0 - U+02FF  // Spacing Modifier Letters
   *       U+0300 - U+036F  // Combining Diacritical Marks
   *       U+0370 - U+03FF  // Greek and Coptic
   *       U+0400 - U+04FF  // Cyrillic
   *       U+0500 - U+052F  // Cyrillic Supplement
   *       U+1D00 - U+1D7F  // Phonetic Extensions
   *       U+1D80 - U+1DBF  // Phonetic Extensions Supplement
   *       U+1DC0 - U+1DFF  // Combining Diacritical Marks Supplement
   *       U+1E00 - U+1EFF  // Latin Extended Additional
   *       U+1F00 - U+1FFF  // Greek Extended
   *       U+2000 - U+206F  // General Punctuation
   *       U+2070 - U+209F  // Superscripts and Subscripts
   *       U+20A0 - U+20CF  // Currency Symbols
   *       U+2150 - U+218F  // Number Forms
   *       U+2460 - U+24FF  // Enclosed Alphanumerics
   *       U+2C60 - U+2C7F  // Latin Extended-C
   *       U+2DE0 - U+2DFF  // Cyrillic Extended-A
   *       U+2E00 - U+2E7F  // Supplemental Punctuation
   *       U+A640 - U+A69F  // Cyrillic Extended-B
   *       U+A720 - U+A7FF  // Latin Extended-D
   *       U+FB00 - U+FB06  // Alphab. Present. Forms (Latin Ligatures)
   *      U+1D400 - U+1D7FF // Mathematical Alphanumeric Symbols
   *      U+1F100 - U+1F1FF // Enclosed Alphanumeric Supplement
   *     ```
   *
   *   FT_AUTOHINTER_SCRIPT_CJK ::
   *     Apply the CJK auto-hinter, covering Chinese, Japanese, Korean, old
   *     Vietnamese, and some other scripts.
   *
   *     By default, characters from the following Unicode ranges are
   *     assigned to this submodule.
   *
   *     ```
   *       U+1100 - U+11FF  // Hangul Jamo
   *       U+2E80 - U+2EFF  // CJK Radicals Supplement
   *       U+2F00 - U+2FDF  // Kangxi Radicals
   *       U+2FF0 - U+2FFF  // Ideographic Description Characters
   *       U+3000 - U+303F  // CJK Symbols and Punctuation
   *       U+3040 - U+309F  // Hiragana
   *       U+30A0 - U+30FF  // Katakana
   *       U+3100 - U+312F  // Bopomofo
   *       U+3130 - U+318F  // Hangul Compatibility Jamo
   *       U+3190 - U+319F  // Kanbun
   *       U+31A0 - U+31BF  // Bopomofo Extended
   *       U+31C0 - U+31EF  // CJK Strokes
   *       U+31F0 - U+31FF  // Katakana Phonetic Extensions
   *       U+3200 - U+32FF  // Enclosed CJK Letters and Months
   *       U+3300 - U+33FF  // CJK Compatibility
   *       U+3400 - U+4DBF  // CJK Unified Ideographs Extension A
   *       U+4DC0 - U+4DFF  // Yijing Hexagram Symbols
   *       U+4E00 - U+9FFF  // CJK Unified Ideographs
   *       U+A960 - U+A97F  // Hangul Jamo Extended-A
   *       U+AC00 - U+D7AF  // Hangul Syllables
   *       U+D7B0 - U+D7FF  // Hangul Jamo Extended-B
   *       U+F900 - U+FAFF  // CJK Compatibility Ideographs
   *       U+FE10 - U+FE1F  // Vertical forms
   *       U+FE30 - U+FE4F  // CJK Compatibility Forms
   *       U+FF00 - U+FFEF  // Halfwidth and Fullwidth Forms
   *      U+1B000 - U+1B0FF // Kana Supplement
   *      U+1D300 - U+1D35F // Tai Xuan Hing Symbols
   *      U+1F200 - U+1F2FF // Enclosed Ideographic Supplement
   *      U+20000 - U+2A6DF // CJK Unified Ideographs Extension B
   *      U+2A700 - U+2B73F // CJK Unified Ideographs Extension C
   *      U+2B740 - U+2B81F // CJK Unified Ideographs Extension D
   *      U+2F800 - U+2FA1F // CJK Compatibility Ideographs Supplement
   *     ```
   *
   *   FT_AUTOHINTER_SCRIPT_INDIC ::
   *     Apply the indic auto-hinter, covering all major scripts from the
   *     Indian sub-continent and some other related scripts like Thai, Lao,
   *     or Tibetan.
   *
   *     By default, characters from the following Unicode ranges are
   *     assigned to this submodule.
   *
   *     ```
   *       U+0900 - U+0DFF  // Indic Range
   *       U+0F00 - U+0FFF  // Tibetan
   *       U+1900 - U+194F  // Limbu
   *       U+1B80 - U+1BBF  // Sundanese
   *       U+A800 - U+A82F  // Syloti Nagri
   *       U+ABC0 - U+ABFF  // Meetei Mayek
   *      U+11800 - U+118DF // Sharada
   *     ```
   *
   *     Note that currently Indic support is rudimentary only, missing blue
   *     zone support.
   *
   * @since:
   *   2.4.11
   *
    }
  FT_AUTOHINTER_SCRIPT_NONE = 0;  
  FT_AUTOHINTER_SCRIPT_LATIN = 1;  
  FT_AUTOHINTER_SCRIPT_CJK = 2;  
  FT_AUTOHINTER_SCRIPT_INDIC = 3;  
{*************************************************************************
   *
   * @struct:
   *   FT_Prop_GlyphToScriptMap
   *
   * @description:
   *   **Experimental only**
   *
   *   The data exchange structure for the @glyph-to-script-map property.
   *
   * @since:
   *   2.4.11
   *
    }
type
  PFT_Prop_GlyphToScriptMap_ = ^TFT_Prop_GlyphToScriptMap_;
  TFT_Prop_GlyphToScriptMap_ = record
      face : TFT_Face;
      map : PFT_UShort;
    end;
  TFT_Prop_GlyphToScriptMap = TFT_Prop_GlyphToScriptMap_;
  PFT_Prop_GlyphToScriptMap = ^TFT_Prop_GlyphToScriptMap;
{*************************************************************************
   *
   * @property:
   *   fallback-script
   *
   * @description:
   *   **Experimental only**
   *
   *   If no auto-hinter script module can be assigned to a glyph, a fallback
   *   script gets assigned to it (see also the @glyph-to-script-map
   *   property).  By default, this is @FT_AUTOHINTER_SCRIPT_CJK.  Using the
   *   `fallback-script` property, this fallback value can be changed.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   It's important to use the right timing for changing this value: The
   *   creation of the glyph-to-script map that eventually uses the fallback
   *   script value gets triggered either by setting or reading a
   *   face-specific property like @glyph-to-script-map, or by auto-hinting
   *   any glyph from that face.  In particular, if you have already created
   *   an @FT_Face structure but not loaded any glyph (using the
   *   auto-hinter), a change of the fallback script will affect this face.
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_UInt     fallback_script = FT_AUTOHINTER_SCRIPT_NONE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "autofitter",
   *                               "fallback-script", &fallback_script );
   *   ```
   *
   * @since:
   *   2.4.11
   *
    }
{*************************************************************************
   *
   * @property:
   *   default-script
   *
   * @description:
   *   **Experimental only**
   *
   *   If FreeType gets compiled with `FT_CONFIG_OPTION_USE_HARFBUZZ` to make
   *   the HarfBuzz library access OpenType features for getting better glyph
   *   coverages, this property sets the (auto-fitter) script to be used for
   *   the default (OpenType) script data of a font's GSUB table.  Features
   *   for the default script are intended for all scripts not explicitly
   *   handled in GSUB; an example is a 'dlig' feature, containing the
   *   combination of the characters 'T', 'E', and 'L' to form a 'TEL'
   *   ligature.
   *
   *   By default, this is @FT_AUTOHINTER_SCRIPT_LATIN.  Using the
   *   `default-script` property, this default value can be changed.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   It's important to use the right timing for changing this value: The
   *   creation of the glyph-to-script map that eventually uses the default
   *   script value gets triggered either by setting or reading a
   *   face-specific property like @glyph-to-script-map, or by auto-hinting
   *   any glyph from that face.  In particular, if you have already created
   *   an @FT_Face structure but not loaded any glyph (using the
   *   auto-hinter), a change of the default script will affect this face.
   *
   * @example:
   *   ```
   *     FT_Library  library;
   *     FT_UInt     default_script = FT_AUTOHINTER_SCRIPT_NONE;
   *
   *
   *     FT_Init_FreeType( &library );
   *
   *     FT_Property_Set( library, "autofitter",
   *                               "default-script", &default_script );
   *   ```
   *
   * @since:
   *   2.5.3
   *
    }
{*************************************************************************
   *
   * @property:
   *   increase-x-height
   *
   * @description:
   *   For ppem values in the range 6~<= ppem <= `increase-x-height`, round
   *   up the font's x~height much more often than normally.  If the value is
   *   set to~0, which is the default, this feature is switched off.  Use
   *   this property to improve the legibility of small font sizes if
   *   necessary.
   *
   * @note:
   *   This property can be used with @FT_Property_Get also.
   *
   *   Set this value right after calling @FT_Set_Char_Size, but before
   *   loading any glyph (using the auto-hinter).
   *
   * @example:
   *   ```
   *     FT_Library               library;
   *     FT_Face                  face;
   *     FT_Prop_IncreaseXHeight  prop;
   *
   *
   *     FT_Init_FreeType( &library );
   *     FT_New_Face( library, "foo.ttf", 0, &face );
   *     FT_Set_Char_Size( face, 10 * 64, 0, 72, 0 );
   *
   *     prop.face  = face;
   *     prop.limit = 14;
   *
   *     FT_Property_Set( library, "autofitter",
   *                               "increase-x-height", &prop );
   *   ```
   *
   * @since:
   *   2.4.11
   *
    }
{*************************************************************************
   *
   * @struct:
   *   FT_Prop_IncreaseXHeight
   *
   * @description:
   *   The data exchange structure for the @increase-x-height property.
   *
    }

  PFT_Prop_IncreaseXHeight_ = ^TFT_Prop_IncreaseXHeight_;
  TFT_Prop_IncreaseXHeight_ = record
      face : TFT_Face;
      limit : TFT_UInt;
    end;
  TFT_Prop_IncreaseXHeight = TFT_Prop_IncreaseXHeight_;
  PFT_Prop_IncreaseXHeight = ^TFT_Prop_IncreaseXHeight;
{*************************************************************************
   *
   * @property:
   *   warping
   *
   * @description:
   *   **Obsolete**
   *
   *   This property was always experimental and probably never worked
   *   correctly.  It was entirely removed from the FreeType~2 sources.  This
   *   entry is only here for historical reference.
   *
   *   Warping only worked in 'normal' auto-hinting mode replacing it.  The
   *   idea of the code was to slightly scale and shift a glyph along the
   *   non-hinted dimension (which is usually the horizontal axis) so that as
   *   much of its segments were aligned (more or less) to the grid.  To find
   *   out a glyph's optimal scaling and shifting value, various parameter
   *   combinations were tried and scored.
   *
   * @since:
   *   2.6
   *
    }
{  }
{$endif}
{ FTDRIVER_H_  }
{ END  }
{ }
{ ===========================  ftgasp.h  =========================== }
{ }
{***************************************************************************
 *
 * ftgasp.h
 *
 *   Access of TrueType's 'gasp' table (specification).
 *
 * Copyright (C) 2007-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{$ifndef FTGASP_H_}
{$define FTGASP_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *   gasp_table
   *
   * @title:
   *   Gasp Table
   *
   * @abstract:
   *   Retrieving TrueType 'gasp' table entries.
   *
   * @description:
   *   The function @FT_Get_Gasp can be used to query a TrueType or OpenType
   *   font for specific entries in its 'gasp' table, if any.  This is mainly
   *   useful when implementing native TrueType hinting with the bytecode
   *   interpreter to duplicate the Windows text rendering results.
    }
{*************************************************************************
   *
   * @enum:
   *   FT_GASP_XXX
   *
   * @description:
   *   A list of values and/or bit-flags returned by the @FT_Get_Gasp
   *   function.
   *
   * @values:
   *   FT_GASP_NO_TABLE ::
   *     This special value means that there is no GASP table in this face.
   *     It is up to the client to decide what to do.
   *
   *   FT_GASP_DO_GRIDFIT ::
   *     Grid-fitting and hinting should be performed at the specified ppem.
   *     This **really** means TrueType bytecode interpretation.  If this bit
   *     is not set, no hinting gets applied.
   *
   *   FT_GASP_DO_GRAY ::
   *     Anti-aliased rendering should be performed at the specified ppem.
   *     If not set, do monochrome rendering.
   *
   *   FT_GASP_SYMMETRIC_SMOOTHING ::
   *     If set, smoothing along multiple axes must be used with ClearType.
   *
   *   FT_GASP_SYMMETRIC_GRIDFIT ::
   *     Grid-fitting must be used with ClearType's symmetric smoothing.
   *
   * @note:
   *   The bit-flags `FT_GASP_DO_GRIDFIT` and `FT_GASP_DO_GRAY` are to be
   *   used for standard font rasterization only.  Independently of that,
   *   `FT_GASP_SYMMETRIC_SMOOTHING` and `FT_GASP_SYMMETRIC_GRIDFIT` are to
   *   be used if ClearType is enabled (and `FT_GASP_DO_GRIDFIT` and
   *   `FT_GASP_DO_GRAY` are consequently ignored).
   *
   *   'ClearType' is Microsoft's implementation of LCD rendering, partly
   *   protected by patents.
   *
   * @since:
   *   2.3.0
    }

const
  FT_GASP_NO_TABLE = -(1);  
  FT_GASP_DO_GRIDFIT = $01;  
  FT_GASP_DO_GRAY = $02;  
  FT_GASP_SYMMETRIC_GRIDFIT = $04;  
  FT_GASP_SYMMETRIC_SMOOTHING = $08;  
{*************************************************************************
   *
   * @function:
   *   FT_Get_Gasp
   *
   * @description:
   *   For a TrueType or OpenType font file, return the rasterizer behaviour
   *   flags from the font's 'gasp' table corresponding to a given character
   *   pixel size.
   *
   * @input:
   *   face ::
   *     The source face handle.
   *
   *   ppem ::
   *     The vertical character pixel size.
   *
   * @return:
   *   Bit flags (see @FT_GASP_XXX), or @FT_GASP_NO_TABLE if there is no
   *   'gasp' table in the face.
   *
   * @note:
   *   If you want to use the MM functionality of OpenType variation fonts
   *   (i.e., using @FT_Set_Var_Design_Coordinates and friends), call this
   *   function **after** setting an instance since the return values can
   *   change.
   *
   * @since:
   *   2.3.0
    }

function FT_Get_Gasp(face:TFT_Face; ppem:TFT_UInt):TFT_Int;cdecl; external freetype_lib;
{  }
{$endif}
{ FTGASP_H_  }
{ END  }
{ }
{ ===========================  ftfntfmt.h  =========================== }
{ }
{***************************************************************************
 *
 * ftfntfmt.h
 *
 *   Support functions for font formats.
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
{$ifndef FTFNTFMT_H_}
{$define FTFNTFMT_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *  font_formats
   *
   * @title:
   *  Font Formats
   *
   * @abstract:
   *  Getting the font format.
   *
   * @description:
   *  The single function in this section can be used to get the font format.
   *  Note that this information is not needed normally; however, there are
   *  special cases (like in PDF devices) where it is important to
   *  differentiate, in spite of FreeType's uniform API.
   *
    }
{*************************************************************************
   *
   * @function:
   *  FT_Get_Font_Format
   *
   * @description:
   *  Return a string describing the format of a given face.  Possible values
   *  are 'TrueType', 'Type~1', 'BDF', 'PCF', 'Type~42', 'CID~Type~1', 'CFF',
   *  'PFR', and 'Windows~FNT'.
   *
   *  The return value is suitable to be used as an X11 FONT_PROPERTY.
   *
   * @input:
   *  face ::
   *    Input face handle.
   *
   * @return:
   *  Font format string.  `NULL` in case of error.
   *
   * @note:
   *  A deprecated name for the same function is `FT_Get_X11_Font_Format`.
    }
(* Const before type ignored *)

function FT_Get_Font_Format(face:TFT_Face):Pchar;cdecl; external freetype_lib;
{ deprecated  }
(* Const before type ignored *)
function FT_Get_X11_Font_Format(face:TFT_Face):Pchar;cdecl; external freetype_lib;
{  }
{$endif}
{ FTFNTFMT_H_  }
{ END  }
{ }
{ ===========================  ftwinfnt.h  =========================== }
{ }
{***************************************************************************
 *
 * ftwinfnt.h
 *
 *   FreeType API for accessing Windows fnt-specific data.
 *
 * Copyright (C) 2003-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{$ifndef FTWINFNT_H_}
{$define FTWINFNT_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *   winfnt_fonts
   *
   * @title:
   *   Window FNT Files
   *
   * @abstract:
   *   Windows FNT-specific API.
   *
   * @description:
   *   This section contains the declaration of Windows FNT-specific
   *   functions.
   *
    }
{*************************************************************************
   *
   * @enum:
   *   FT_WinFNT_ID_XXX
   *
   * @description:
   *   A list of valid values for the `charset` byte in @FT_WinFNT_HeaderRec.
   *   Exact mapping tables for the various 'cpXXXX' encodings (except for
   *   'cp1361') can be found at 'ftp://ftp.unicode.org/Public/' in the
   *   `MAPPINGS/VENDORS/MICSFT/WINDOWS` subdirectory.  'cp1361' is roughly a
   *   superset of `MAPPINGS/OBSOLETE/EASTASIA/KSC/JOHAB.TXT`.
   *
   * @values:
   *   FT_WinFNT_ID_DEFAULT ::
   *     This is used for font enumeration and font creation as a 'don't
   *     care' value.  Valid font files don't contain this value.  When
   *     querying for information about the character set of the font that is
   *     currently selected into a specified device context, this return
   *     value (of the related Windows API) simply denotes failure.
   *
   *   FT_WinFNT_ID_SYMBOL ::
   *     There is no known mapping table available.
   *
   *   FT_WinFNT_ID_MAC ::
   *     Mac Roman encoding.
   *
   *   FT_WinFNT_ID_OEM ::
   *     From Michael Poettgen <michael@poettgen.de>:
   *
   *     The 'Windows Font Mapping' article says that `FT_WinFNT_ID_OEM` is
   *     used for the charset of vector fonts, like `modern.fon`,
   *     `roman.fon`, and `script.fon` on Windows.
   *
   *     The 'CreateFont' documentation says: The `FT_WinFNT_ID_OEM` value
   *     specifies a character set that is operating-system dependent.
   *
   *     The 'IFIMETRICS' documentation from the 'Windows Driver Development
   *     Kit' says: This font supports an OEM-specific character set.  The
   *     OEM character set is system dependent.
   *
   *     In general OEM, as opposed to ANSI (i.e., 'cp1252'), denotes the
   *     second default codepage that most international versions of Windows
   *     have.  It is one of the OEM codepages from
   *
   *     https://docs.microsoft.com/en-us/windows/desktop/intl/code-page-identifiers
   *     ,
   *
   *     and is used for the 'DOS boxes', to support legacy applications.  A
   *     German Windows version for example usually uses ANSI codepage 1252
   *     and OEM codepage 850.
   *
   *   FT_WinFNT_ID_CP874 ::
   *     A superset of Thai TIS 620 and ISO 8859-11.
   *
   *   FT_WinFNT_ID_CP932 ::
   *     A superset of Japanese Shift-JIS (with minor deviations).
   *
   *   FT_WinFNT_ID_CP936 ::
   *     A superset of simplified Chinese GB 2312-1980 (with different
   *     ordering and minor deviations).
   *
   *   FT_WinFNT_ID_CP949 ::
   *     A superset of Korean Hangul KS~C 5601-1987 (with different ordering
   *     and minor deviations).
   *
   *   FT_WinFNT_ID_CP950 ::
   *     A superset of traditional Chinese Big~5 ETen (with different
   *     ordering and minor deviations).
   *
   *   FT_WinFNT_ID_CP1250 ::
   *     A superset of East European ISO 8859-2 (with slightly different
   *     ordering).
   *
   *   FT_WinFNT_ID_CP1251 ::
   *     A superset of Russian ISO 8859-5 (with different ordering).
   *
   *   FT_WinFNT_ID_CP1252 ::
   *     ANSI encoding.  A superset of ISO 8859-1.
   *
   *   FT_WinFNT_ID_CP1253 ::
   *     A superset of Greek ISO 8859-7 (with minor modifications).
   *
   *   FT_WinFNT_ID_CP1254 ::
   *     A superset of Turkish ISO 8859-9.
   *
   *   FT_WinFNT_ID_CP1255 ::
   *     A superset of Hebrew ISO 8859-8 (with some modifications).
   *
   *   FT_WinFNT_ID_CP1256 ::
   *     A superset of Arabic ISO 8859-6 (with different ordering).
   *
   *   FT_WinFNT_ID_CP1257 ::
   *     A superset of Baltic ISO 8859-13 (with some deviations).
   *
   *   FT_WinFNT_ID_CP1258 ::
   *     For Vietnamese.  This encoding doesn't cover all necessary
   *     characters.
   *
   *   FT_WinFNT_ID_CP1361 ::
   *     Korean (Johab).
    }

const
  FT_WinFNT_ID_CP1252 = 0;  
  FT_WinFNT_ID_DEFAULT = 1;  
  FT_WinFNT_ID_SYMBOL = 2;  
  FT_WinFNT_ID_MAC = 77;  
  FT_WinFNT_ID_CP932 = 128;  
  FT_WinFNT_ID_CP949 = 129;  
  FT_WinFNT_ID_CP1361 = 130;  
  FT_WinFNT_ID_CP936 = 134;  
  FT_WinFNT_ID_CP950 = 136;  
  FT_WinFNT_ID_CP1253 = 161;  
  FT_WinFNT_ID_CP1254 = 162;  
  FT_WinFNT_ID_CP1258 = 163;  
  FT_WinFNT_ID_CP1255 = 177;  
  FT_WinFNT_ID_CP1256 = 178;  
  FT_WinFNT_ID_CP1257 = 186;  
  FT_WinFNT_ID_CP1251 = 204;  
  FT_WinFNT_ID_CP874 = 222;  
  FT_WinFNT_ID_CP1250 = 238;  
  FT_WinFNT_ID_OEM = 255;  
{*************************************************************************
   *
   * @struct:
   *   FT_WinFNT_HeaderRec
   *
   * @description:
   *   Windows FNT Header info.
    }
type
  PFT_WinFNT_HeaderRec_ = ^TFT_WinFNT_HeaderRec_;
  TFT_WinFNT_HeaderRec_ = record
      version : TFT_UShort;
      file_size : TFT_ULong;
      copyright : array[0..59] of TFT_Byte;
      file_type : TFT_UShort;
      nominal_point_size : TFT_UShort;
      vertical_resolution : TFT_UShort;
      horizontal_resolution : TFT_UShort;
      ascent : TFT_UShort;
      internal_leading : TFT_UShort;
      external_leading : TFT_UShort;
      italic : TFT_Byte;
      underline : TFT_Byte;
      strike_out : TFT_Byte;
      weight : TFT_UShort;
      charset : TFT_Byte;
      pixel_width : TFT_UShort;
      pixel_height : TFT_UShort;
      pitch_and_family : TFT_Byte;
      avg_width : TFT_UShort;
      max_width : TFT_UShort;
      first_char : TFT_Byte;
      last_char : TFT_Byte;
      default_char : TFT_Byte;
      break_char : TFT_Byte;
      bytes_per_row : TFT_UShort;
      device_offset : TFT_ULong;
      face_name_offset : TFT_ULong;
      bits_pointer : TFT_ULong;
      bits_offset : TFT_ULong;
      reserved : TFT_Byte;
      flags : TFT_ULong;
      A_space : TFT_UShort;
      B_space : TFT_UShort;
      C_space : TFT_UShort;
      color_table_offset : TFT_UShort;
      reserved1 : array[0..3] of TFT_ULong;
    end;
  TFT_WinFNT_HeaderRec = TFT_WinFNT_HeaderRec_;
  PFT_WinFNT_HeaderRec = ^TFT_WinFNT_HeaderRec;
{*************************************************************************
   *
   * @struct:
   *   FT_WinFNT_Header
   *
   * @description:
   *   A handle to an @FT_WinFNT_HeaderRec structure.
    }

  PFT_WinFNT_Header = ^TFT_WinFNT_Header;
  TFT_WinFNT_Header = PFT_WinFNT_HeaderRec_;
{*************************************************************************
   *
   * @function:
   *    FT_Get_WinFNT_Header
   *
   * @description:
   *    Retrieve a Windows FNT font info header.
   *
   * @input:
   *    face ::
   *      A handle to the input face.
   *
   * @output:
   *    aheader ::
   *      The WinFNT header.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function only works with Windows FNT faces, returning an error
   *   otherwise.
    }

function FT_Get_WinFNT_Header(face:TFT_Face; aheader:PFT_WinFNT_HeaderRec):TFT_Error;cdecl; external freetype_lib;
{  }
{$endif}
{ FTWINFNT_H_  }
{ END  }
{ Local Variables:  }
{ coding: utf-8     }
{ End:              }
{$endif}
{ FTCOLOR_H_  }
{ END  }
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



{ ===========================  ftbbox.h  =========================== }
{ }
{***************************************************************************
 *
 * ftbbox.h
 *
 *   FreeType exact bbox computation (specification).
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
   * This component has a _single_ role: to compute exact outline bounding
   * boxes.
   *
   * It is separated from the rest of the engine for various technical
   * reasons.  It may well be integrated in 'ftoutln' later.
   *
    }
{$ifndef FTBBOX_H_}
{$define FTBBOX_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *   outline_processing
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_Outline_Get_BBox
   *
   * @description:
   *   Compute the exact bounding box of an outline.  This is slower than
   *   computing the control box.  However, it uses an advanced algorithm
   *   that returns _very_ quickly when the two boxes coincide.  Otherwise,
   *   the outline Bezier arcs are traversed to extract their extrema.
   *
   * @input:
   *   outline ::
   *     A pointer to the source outline.
   *
   * @output:
   *   abbox ::
   *     The outline's exact bounding box.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If the font is tricky and the glyph has been loaded with
   *   @FT_LOAD_NO_SCALE, the resulting BBox is meaningless.  To get
   *   reasonable values for the BBox it is necessary to load the glyph at a
   *   large ppem value (so that the hinting instructions can properly shift
   *   and scale the subglyphs), then extracting the BBox, which can be
   *   eventually converted back to font units.
    }

function FT_Outline_Get_BBox(outline:PFT_Outline; abbox:PFT_BBox):TFT_Error;cdecl; external freetype_lib;
{  }
{$endif}
{ FTBBOX_H_  }
{ END  }
{ Local Variables:  }
{ coding: utf-8     }
{ End:              }
{ }
{ ===========================  ftstroke.h  =========================== }
{ }
{***************************************************************************
 *
 * ftstroke.h
 *
 *   FreeType path stroker (specification).
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
{$ifndef FTSTROKE_H_}
{$define FTSTROKE_H_}
{$include <freetype/ftoutln.h>}
{$include <freetype/ftglyph.h>}
{*************************************************************************
   *
   * @section:
   *    glyph_stroker
   *
   * @title:
   *    Glyph Stroker
   *
   * @abstract:
   *    Generating bordered and stroked glyphs.
   *
   * @description:
   *    This component generates stroked outlines of a given vectorial glyph.
   *    It also allows you to retrieve the 'outside' and/or the 'inside'
   *    borders of the stroke.
   *
   *    This can be useful to generate 'bordered' glyph, i.e., glyphs
   *    displayed with a colored (and anti-aliased) border around their
   *    shape.
   *
   * @order:
   *    FT_Stroker
   *
   *    FT_Stroker_LineJoin
   *    FT_Stroker_LineCap
   *    FT_StrokerBorder
   *
   *    FT_Outline_GetInsideBorder
   *    FT_Outline_GetOutsideBorder
   *
   *    FT_Glyph_Stroke
   *    FT_Glyph_StrokeBorder
   *
   *    FT_Stroker_New
   *    FT_Stroker_Set
   *    FT_Stroker_Rewind
   *    FT_Stroker_ParseOutline
   *    FT_Stroker_Done
   *
   *    FT_Stroker_BeginSubPath
   *    FT_Stroker_EndSubPath
   *
   *    FT_Stroker_LineTo
   *    FT_Stroker_ConicTo
   *    FT_Stroker_CubicTo
   *
   *    FT_Stroker_GetBorderCounts
   *    FT_Stroker_ExportBorder
   *    FT_Stroker_GetCounts
   *    FT_Stroker_Export
   *
    }
{*************************************************************************
   *
   * @type:
   *   FT_Stroker
   *
   * @description:
   *   Opaque handle to a path stroker object.
    }
type
  PFT_Stroker = ^TFT_Stroker;
  TFT_Stroker = PFT_StrokerRec_;
{*************************************************************************
   *
   * @enum:
   *   FT_Stroker_LineJoin
   *
   * @description:
   *   These values determine how two joining lines are rendered in a
   *   stroker.
   *
   * @values:
   *   FT_STROKER_LINEJOIN_ROUND ::
   *     Used to render rounded line joins.  Circular arcs are used to join
   *     two lines smoothly.
   *
   *   FT_STROKER_LINEJOIN_BEVEL ::
   *     Used to render beveled line joins.  The outer corner of the joined
   *     lines is filled by enclosing the triangular region of the corner
   *     with a straight line between the outer corners of each stroke.
   *
   *   FT_STROKER_LINEJOIN_MITER_FIXED ::
   *     Used to render mitered line joins, with fixed bevels if the miter
   *     limit is exceeded.  The outer edges of the strokes for the two
   *     segments are extended until they meet at an angle.  A bevel join
   *     (see above) is used if the segments meet at too sharp an angle and
   *     the outer edges meet beyond a distance corresponding to the meter
   *     limit.  This prevents long spikes being created.
   *     `FT_STROKER_LINEJOIN_MITER_FIXED` generates a miter line join as
   *     used in PostScript and PDF.
   *
   *   FT_STROKER_LINEJOIN_MITER_VARIABLE ::
   *   FT_STROKER_LINEJOIN_MITER ::
   *     Used to render mitered line joins, with variable bevels if the miter
   *     limit is exceeded.  The intersection of the strokes is clipped
   *     perpendicularly to the bisector, at a distance corresponding to
   *     the miter limit. This prevents long spikes being created.
   *     `FT_STROKER_LINEJOIN_MITER_VARIABLE` generates a mitered line join
   *     as used in XPS.  `FT_STROKER_LINEJOIN_MITER` is an alias for
   *     `FT_STROKER_LINEJOIN_MITER_VARIABLE`, retained for backward
   *     compatibility.
    }

  PFT_Stroker_LineJoin_ = ^TFT_Stroker_LineJoin_;
  TFT_Stroker_LineJoin_ =  Longint;
  Const
    FT_STROKER_LINEJOIN_ROUND = 0;
    FT_STROKER_LINEJOIN_BEVEL = 1;
    FT_STROKER_LINEJOIN_MITER_VARIABLE = 2;
    FT_STROKER_LINEJOIN_MITER = FT_STROKER_LINEJOIN_MITER_VARIABLE;
    FT_STROKER_LINEJOIN_MITER_FIXED = 3;
;
  TFT_Stroker_LineJoin = TFT_Stroker_LineJoin_;
  PFT_Stroker_LineJoin = ^TFT_Stroker_LineJoin;
{*************************************************************************
   *
   * @enum:
   *   FT_Stroker_LineCap
   *
   * @description:
   *   These values determine how the end of opened sub-paths are rendered in
   *   a stroke.
   *
   * @values:
   *   FT_STROKER_LINECAP_BUTT ::
   *     The end of lines is rendered as a full stop on the last point
   *     itself.
   *
   *   FT_STROKER_LINECAP_ROUND ::
   *     The end of lines is rendered as a half-circle around the last point.
   *
   *   FT_STROKER_LINECAP_SQUARE ::
   *     The end of lines is rendered as a square around the last point.
    }
type
  PFT_Stroker_LineCap_ = ^TFT_Stroker_LineCap_;
  TFT_Stroker_LineCap_ =  Longint;
  Const
    FT_STROKER_LINECAP_BUTT = 0;
    FT_STROKER_LINECAP_ROUND = 1;
    FT_STROKER_LINECAP_SQUARE = 2;
;
  TFT_Stroker_LineCap = TFT_Stroker_LineCap_;
  PFT_Stroker_LineCap = ^TFT_Stroker_LineCap;
{*************************************************************************
   *
   * @enum:
   *   FT_StrokerBorder
   *
   * @description:
   *   These values are used to select a given stroke border in
   *   @FT_Stroker_GetBorderCounts and @FT_Stroker_ExportBorder.
   *
   * @values:
   *   FT_STROKER_BORDER_LEFT ::
   *     Select the left border, relative to the drawing direction.
   *
   *   FT_STROKER_BORDER_RIGHT ::
   *     Select the right border, relative to the drawing direction.
   *
   * @note:
   *   Applications are generally interested in the 'inside' and 'outside'
   *   borders.  However, there is no direct mapping between these and the
   *   'left' and 'right' ones, since this really depends on the glyph's
   *   drawing orientation, which varies between font formats.
   *
   *   You can however use @FT_Outline_GetInsideBorder and
   *   @FT_Outline_GetOutsideBorder to get these.
    }
type
  PFT_StrokerBorder_ = ^TFT_StrokerBorder_;
  TFT_StrokerBorder_ =  Longint;
  Const
    FT_STROKER_BORDER_LEFT = 0;
    FT_STROKER_BORDER_RIGHT = 1;
;
  TFT_StrokerBorder = TFT_StrokerBorder_;
  PFT_StrokerBorder = ^TFT_StrokerBorder;
{*************************************************************************
   *
   * @function:
   *   FT_Outline_GetInsideBorder
   *
   * @description:
   *   Retrieve the @FT_StrokerBorder value corresponding to the 'inside'
   *   borders of a given outline.
   *
   * @input:
   *   outline ::
   *     The source outline handle.
   *
   * @return:
   *   The border index.  @FT_STROKER_BORDER_RIGHT for empty or invalid
   *   outlines.
    }

function FT_Outline_GetInsideBorder(outline:PFT_Outline):TFT_StrokerBorder;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Outline_GetOutsideBorder
   *
   * @description:
   *   Retrieve the @FT_StrokerBorder value corresponding to the 'outside'
   *   borders of a given outline.
   *
   * @input:
   *   outline ::
   *     The source outline handle.
   *
   * @return:
   *   The border index.  @FT_STROKER_BORDER_LEFT for empty or invalid
   *   outlines.
    }
function FT_Outline_GetOutsideBorder(outline:PFT_Outline):TFT_StrokerBorder;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_New
   *
   * @description:
   *   Create a new stroker object.
   *
   * @input:
   *   library ::
   *     FreeType library handle.
   *
   * @output:
   *   astroker ::
   *     A new stroker object handle.  `NULL` in case of error.
   *
   * @return:
   *    FreeType error code.  0~means success.
    }
function FT_Stroker_New(library_:TFT_Library; astroker:PFT_Stroker):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_Set
   *
   * @description:
   *   Reset a stroker object's attributes.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   radius ::
   *     The border radius.
   *
   *   line_cap ::
   *     The line cap style.
   *
   *   line_join ::
   *     The line join style.
   *
   *   miter_limit ::
   *     The maximum reciprocal sine of half-angle at the miter join,
   *     expressed as 16.16 fixed-point value.
   *
   * @note:
   *   The `radius` is expressed in the same units as the outline
   *   coordinates.
   *
   *   The `miter_limit` multiplied by the `radius` gives the maximum size
   *   of a miter spike, at which it is clipped for
   *   @FT_STROKER_LINEJOIN_MITER_VARIABLE or replaced with a bevel join for
   *   @FT_STROKER_LINEJOIN_MITER_FIXED.
   *
   *   This function calls @FT_Stroker_Rewind automatically.
    }
procedure FT_Stroker_Set(stroker:TFT_Stroker; radius:TFT_Fixed; line_cap:TFT_Stroker_LineCap; line_join:TFT_Stroker_LineJoin; miter_limit:TFT_Fixed);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_Rewind
   *
   * @description:
   *   Reset a stroker object without changing its attributes.  You should
   *   call this function before beginning a new series of calls to
   *   @FT_Stroker_BeginSubPath or @FT_Stroker_EndSubPath.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
    }
procedure FT_Stroker_Rewind(stroker:TFT_Stroker);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_ParseOutline
   *
   * @description:
   *   A convenience function used to parse a whole outline with the stroker.
   *   The resulting outline(s) can be retrieved later by functions like
   *   @FT_Stroker_GetCounts and @FT_Stroker_Export.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   outline ::
   *     The source outline.
   *
   *   opened ::
   *     A boolean.  If~1, the outline is treated as an open path instead of
   *     a closed one.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If `opened` is~0 (the default), the outline is treated as a closed
   *   path, and the stroker generates two distinct 'border' outlines.
   *
   *   If `opened` is~1, the outline is processed as an open path, and the
   *   stroker generates a single 'stroke' outline.
   *
   *   This function calls @FT_Stroker_Rewind automatically.
    }
function FT_Stroker_ParseOutline(stroker:TFT_Stroker; outline:PFT_Outline; opened:TFT_Bool):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_BeginSubPath
   *
   * @description:
   *   Start a new sub-path in the stroker.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   to ::
   *     A pointer to the start vector.
   *
   *   open ::
   *     A boolean.  If~1, the sub-path is treated as an open one.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function is useful when you need to stroke a path that is not
   *   stored as an @FT_Outline object.
    }
function FT_Stroker_BeginSubPath(stroker:TFT_Stroker; to:PFT_Vector; open:TFT_Bool):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_EndSubPath
   *
   * @description:
   *   Close the current sub-path in the stroker.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You should call this function after @FT_Stroker_BeginSubPath.  If the
   *   subpath was not 'opened', this function 'draws' a single line segment
   *   to the start position when needed.
    }
function FT_Stroker_EndSubPath(stroker:TFT_Stroker):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_LineTo
   *
   * @description:
   *   'Draw' a single line segment in the stroker's current sub-path, from
   *   the last position.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   to ::
   *     A pointer to the destination point.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You should call this function between @FT_Stroker_BeginSubPath and
   *   @FT_Stroker_EndSubPath.
    }
function FT_Stroker_LineTo(stroker:TFT_Stroker; to:PFT_Vector):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_ConicTo
   *
   * @description:
   *   'Draw' a single quadratic Bezier in the stroker's current sub-path,
   *   from the last position.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   control ::
   *     A pointer to a Bezier control point.
   *
   *   to ::
   *     A pointer to the destination point.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You should call this function between @FT_Stroker_BeginSubPath and
   *   @FT_Stroker_EndSubPath.
    }
function FT_Stroker_ConicTo(stroker:TFT_Stroker; control:PFT_Vector; to:PFT_Vector):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_CubicTo
   *
   * @description:
   *   'Draw' a single cubic Bezier in the stroker's current sub-path, from
   *   the last position.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   control1 ::
   *     A pointer to the first Bezier control point.
   *
   *   control2 ::
   *     A pointer to second Bezier control point.
   *
   *   to ::
   *     A pointer to the destination point.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   You should call this function between @FT_Stroker_BeginSubPath and
   *   @FT_Stroker_EndSubPath.
    }
function FT_Stroker_CubicTo(stroker:TFT_Stroker; control1:PFT_Vector; control2:PFT_Vector; to:PFT_Vector):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_GetBorderCounts
   *
   * @description:
   *   Call this function once you have finished parsing your paths with the
   *   stroker.  It returns the number of points and contours necessary to
   *   export one of the 'border' or 'stroke' outlines generated by the
   *   stroker.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   border ::
   *     The border index.
   *
   * @output:
   *   anum_points ::
   *     The number of points.
   *
   *   anum_contours ::
   *     The number of contours.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   When an outline, or a sub-path, is 'closed', the stroker generates two
   *   independent 'border' outlines, named 'left' and 'right'.
   *
   *   When the outline, or a sub-path, is 'opened', the stroker merges the
   *   'border' outlines with caps.  The 'left' border receives all points,
   *   while the 'right' border becomes empty.
   *
   *   Use the function @FT_Stroker_GetCounts instead if you want to retrieve
   *   the counts associated to both borders.
    }
function FT_Stroker_GetBorderCounts(stroker:TFT_Stroker; border:TFT_StrokerBorder; anum_points:PFT_UInt; anum_contours:PFT_UInt):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_ExportBorder
   *
   * @description:
   *   Call this function after @FT_Stroker_GetBorderCounts to export the
   *   corresponding border to your own @FT_Outline structure.
   *
   *   Note that this function appends the border points and contours to your
   *   outline, but does not try to resize its arrays.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   border ::
   *     The border index.
   *
   *   outline ::
   *     The target outline handle.
   *
   * @note:
   *   Always call this function after @FT_Stroker_GetBorderCounts to get
   *   sure that there is enough room in your @FT_Outline object to receive
   *   all new data.
   *
   *   When an outline, or a sub-path, is 'closed', the stroker generates two
   *   independent 'border' outlines, named 'left' and 'right'.
   *
   *   When the outline, or a sub-path, is 'opened', the stroker merges the
   *   'border' outlines with caps.  The 'left' border receives all points,
   *   while the 'right' border becomes empty.
   *
   *   Use the function @FT_Stroker_Export instead if you want to retrieve
   *   all borders at once.
    }
procedure FT_Stroker_ExportBorder(stroker:TFT_Stroker; border:TFT_StrokerBorder; outline:PFT_Outline);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_GetCounts
   *
   * @description:
   *   Call this function once you have finished parsing your paths with the
   *   stroker.  It returns the number of points and contours necessary to
   *   export all points/borders from the stroked outline/path.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   * @output:
   *   anum_points ::
   *     The number of points.
   *
   *   anum_contours ::
   *     The number of contours.
   *
   * @return:
   *   FreeType error code.  0~means success.
    }
function FT_Stroker_GetCounts(stroker:TFT_Stroker; anum_points:PFT_UInt; anum_contours:PFT_UInt):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_Export
   *
   * @description:
   *   Call this function after @FT_Stroker_GetBorderCounts to export all
   *   borders to your own @FT_Outline structure.
   *
   *   Note that this function appends the border points and contours to your
   *   outline, but does not try to resize its arrays.
   *
   * @input:
   *   stroker ::
   *     The target stroker handle.
   *
   *   outline ::
   *     The target outline handle.
    }
procedure FT_Stroker_Export(stroker:TFT_Stroker; outline:PFT_Outline);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Stroker_Done
   *
   * @description:
   *   Destroy a stroker object.
   *
   * @input:
   *   stroker ::
   *     A stroker handle.  Can be `NULL`.
    }
procedure FT_Stroker_Done(stroker:TFT_Stroker);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Glyph_Stroke
   *
   * @description:
   *   Stroke a given outline glyph object with a given stroker.
   *
   * @inout:
   *   pglyph ::
   *     Source glyph handle on input, new glyph handle on output.
   *
   * @input:
   *   stroker ::
   *     A stroker handle.
   *
   *   destroy ::
   *     A Boolean.  If~1, the source glyph object is destroyed on success.
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *   The source glyph is untouched in case of error.
   *
   *   Adding stroke may yield a significantly wider and taller glyph
   *   depending on how large of a radius was used to stroke the glyph.  You
   *   may need to manually adjust horizontal and vertical advance amounts to
   *   account for this added size.
    }
function FT_Glyph_Stroke(pglyph:PFT_Glyph; stroker:TFT_Stroker; destroy:TFT_Bool):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Glyph_StrokeBorder
   *
   * @description:
   *   Stroke a given outline glyph object with a given stroker, but only
   *   return either its inside or outside border.
   *
   * @inout:
   *   pglyph ::
   *     Source glyph handle on input, new glyph handle on output.
   *
   * @input:
   *   stroker ::
   *     A stroker handle.
   *
   *   inside ::
   *     A Boolean.  If~1, return the inside border, otherwise the outside
   *     border.
   *
   *   destroy ::
   *     A Boolean.  If~1, the source glyph object is destroyed on success.
   *
   * @return:
   *    FreeType error code.  0~means success.
   *
   * @note:
   *   The source glyph is untouched in case of error.
   *
   *   Adding stroke may yield a significantly wider and taller glyph
   *   depending on how large of a radius was used to stroke the glyph.  You
   *   may need to manually adjust horizontal and vertical advance amounts to
   *   account for this added size.
    }
function FT_Glyph_StrokeBorder(pglyph:PFT_Glyph; stroker:TFT_Stroker; inside:TFT_Bool; destroy:TFT_Bool):TFT_Error;cdecl; external freetype_lib;
{  }
{$endif}
{ FTSTROKE_H_  }
{ END  }
{ Local Variables:  }
{ coding: utf-8     }
{ End:              }
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
{ ===========================  ftsynth.h  =========================== }
{ }
{***************************************************************************
 *
 * ftsynth.h
 *
 *   FreeType synthesizing code for emboldening and slanting
 *   (specification).
 *
 * Copyright (C) 2000-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{*********************************************************************** }
{*********************************************************************** }
{*********************************************************************** }
{*********************************************************************** }
{*********************************************************************** }
{********                                                       ******** }
{********        WARNING, THIS IS ALPHA CODE!  THIS API         ******** }
{********    IS DUE TO CHANGE UNTIL STRICTLY NOTIFIED BY THE    ******** }
{********            FREETYPE DEVELOPMENT TEAM                  ******** }
{********                                                       ******** }
{*********************************************************************** }
{*********************************************************************** }
{*********************************************************************** }
{*********************************************************************** }
{*********************************************************************** }
{ Main reason for not lifting the functions in this module to a   }
{ 'standard' API is that the used parameters for emboldening and  }
{ slanting are not configurable.  Consider the functions as a     }
{ code resource that should be copied into the application and    }
{ adapted to the particular needs.                                }
{$ifndef FTSYNTH_H_}
{$define FTSYNTH_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{ Embolden a glyph by a 'reasonable' value (which is highly a matter of  }
{ taste).  This function is actually a convenience function, providing   }
{ a wrapper for @FT_Outline_Embolden and @FT_Bitmap_Embolden.            }
{                                                                        }
{ For emboldened outlines the height, width, and advance metrics are     }
{ increased by the strength of the emboldening -- this even affects      }
{ mono-width fonts!                                                      }
{                                                                        }
{ You can also call @FT_Outline_Get_CBox to get precise values.          }

procedure FT_GlyphSlot_Embolden(slot:TFT_GlyphSlot);cdecl; external freetype_lib;
{ Precisely adjust the glyph weight either horizontally or vertically.   }
{ The `xdelta` and `ydelta` values are fractions of the face Em size     }
{ (in fixed-point format).  Considering that a regular face would have   }
{ stem widths on the order of 0.1 Em, a delta of 0.05 (0x0CCC) should    }
{ be very noticeable.  To increase or decrease the weight, use positive  }
{ or negative values, respectively.                                      }
procedure FT_GlyphSlot_AdjustWeight(slot:TFT_GlyphSlot; xdelta:TFT_Fixed; ydelta:TFT_Fixed);cdecl; external freetype_lib;
{ Slant an outline glyph to the right by about 12 degrees.               }
procedure FT_GlyphSlot_Oblique(slot:TFT_GlyphSlot);cdecl; external freetype_lib;
{ Slant an outline glyph by a given sine of an angle.  You can apply     }
{ slant along either x- or y-axis by choosing a corresponding non-zero   }
{ argument.  If both slants are non-zero, some affine transformation     }
{ will result.                                                           }
procedure FT_GlyphSlot_Slant(slot:TFT_GlyphSlot; xslant:TFT_Fixed; yslant:TFT_Fixed);cdecl; external freetype_lib;
{  }
{$endif}
{ FTSYNTH_H_  }
{ END  }
{ }
{ ===========================  ftglyph.h  =========================== }
{ }
{*************************************************************************
   *
   * @struct:
   *   FT_BitmapGlyphRec
   *
   * @description:
   *   A structure used for bitmap glyph images.  This really is a
   *   'sub-class' of @FT_GlyphRec.
   *
   * @fields:
   *   root ::
   *     The root fields of @FT_Glyph.
   *
   *   left ::
   *     The left-side bearing, i.e., the horizontal distance from the
   *     current pen position to the left border of the glyph bitmap.
   *
   *   top ::
   *     The top-side bearing, i.e., the vertical distance from the current
   *     pen position to the top border of the glyph bitmap.  This distance
   *     is positive for upwards~y!
   *
   *   bitmap ::
   *     A descriptor for the bitmap.
   *
   * @note:
   *   You can typecast an @FT_Glyph to @FT_BitmapGlyph if you have
   *   `glyph->format == FT_GLYPH_FORMAT_BITMAP`.  This lets you access the
   *   bitmap's contents easily.
   *
   *   The corresponding pixel buffer is always owned by @FT_BitmapGlyph and
   *   is thus created and destroyed with it.
    }
{*************************************************************************
   *
   * @type:
   *   FT_OutlineGlyph
   *
   * @description:
   *   A handle to an object used to model an outline glyph image.  This is a
   *   'sub-class' of @FT_Glyph, and a pointer to @FT_OutlineGlyphRec.
    }

  PFT_OutlineGlyph = ^TFT_OutlineGlyph;
  TFT_OutlineGlyph = PFT_OutlineGlyphRec_;
{*************************************************************************
   *
   * @struct:
   *   FT_OutlineGlyphRec
   *
   * @description:
   *   A structure used for outline (vectorial) glyph images.  This really is
   *   a 'sub-class' of @FT_GlyphRec.
   *
   * @fields:
   *   root ::
   *     The root @FT_Glyph fields.
   *
   *   outline ::
   *     A descriptor for the outline.
   *
   * @note:
   *   You can typecast an @FT_Glyph to @FT_OutlineGlyph if you have
   *   `glyph->format == FT_GLYPH_FORMAT_OUTLINE`.  This lets you access the
   *   outline's content easily.
   *
   *   As the outline is extracted from a glyph slot, its coordinates are
   *   expressed normally in 26.6 pixels, unless the flag @FT_LOAD_NO_SCALE
   *   was used in @FT_Load_Glyph or @FT_Load_Char.
   *
   *   The outline's tables are always owned by the object and are destroyed
   *   with it.
    }

  PFT_OutlineGlyphRec_ = ^TFT_OutlineGlyphRec_;
  TFT_OutlineGlyphRec_ = record
      root : TFT_GlyphRec;
      outline : TFT_Outline;
    end;
  TFT_OutlineGlyphRec = TFT_OutlineGlyphRec_;
  PFT_OutlineGlyphRec = ^TFT_OutlineGlyphRec;
{*************************************************************************
   *
   * @type:
   *   FT_SvgGlyph
   *
   * @description:
   *   A handle to an object used to model an SVG glyph.  This is a
   *   'sub-class' of @FT_Glyph, and a pointer to @FT_SvgGlyphRec.
   *
   * @since:
   *   2.12
    }

  PFT_SvgGlyph = ^TFT_SvgGlyph;
  TFT_SvgGlyph = PFT_SvgGlyphRec_;
{*************************************************************************
   *
   * @struct:
   *   FT_SvgGlyphRec
   *
   * @description:
   *   A structure used for OT-SVG glyphs.  This is a 'sub-class' of
   *   @FT_GlyphRec.
   *
   * @fields:
   *   root ::
   *     The root @FT_GlyphRec fields.
   *
   *   svg_document ::
   *     A pointer to the SVG document.
   *
   *   svg_document_length ::
   *     The length of `svg_document`.
   *
   *   glyph_index ::
   *     The index of the glyph to be rendered.
   *
   *   metrics ::
   *     A metrics object storing the size information.
   *
   *   units_per_EM ::
   *     The size of the EM square.
   *
   *   start_glyph_id ::
   *     The first glyph ID in the glyph range covered by this document.
   *
   *   end_glyph_id ::
   *     The last glyph ID in the glyph range covered by this document.
   *
   *   transform ::
   *     A 2x2 transformation matrix to apply to the glyph while rendering
   *     it.
   *
   *   delta ::
   *     Translation to apply to the glyph while rendering.
   *
   * @note:
   *   The Glyph Management API requires @FT_Glyph or its 'sub-class' to have
   *   all the information needed to completely define the glyph's rendering.
   *   Outline-based glyphs can directly apply transformations to the outline
   *   but this is not possible for an SVG document that hasn't been parsed.
   *   Therefore, the transformation is stored along with the document.  In
   *   the absence of a 'ViewBox' or 'Width'/'Height' attribute, the size of
   *   the ViewPort should be assumed to be 'units_per_EM'.
    }

  PFT_SvgGlyphRec_ = ^TFT_SvgGlyphRec_;
  TFT_SvgGlyphRec_ = record
      root : TFT_GlyphRec;
      svg_document : PFT_Byte;
      svg_document_length : TFT_ULong;
      glyph_index : TFT_UInt;
      metrics : TFT_Size_Metrics;
      units_per_EM : TFT_UShort;
      start_glyph_id : TFT_UShort;
      end_glyph_id : TFT_UShort;
      transform : TFT_Matrix;
      delta : TFT_Vector;
    end;
  TFT_SvgGlyphRec = TFT_SvgGlyphRec_;
  PFT_SvgGlyphRec = ^TFT_SvgGlyphRec;
{*************************************************************************
   *
   * @function:
   *   FT_New_Glyph
   *
   * @description:
   *   A function used to create a new empty glyph image.  Note that the
   *   created @FT_Glyph object must be released with @FT_Done_Glyph.
   *
   * @input:
   *   library ::
   *     A handle to the FreeType library object.
   *
   *   format ::
   *     The format of the glyph's image.
   *
   * @output:
   *   aglyph ::
   *     A handle to the glyph object.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @since:
   *   2.10
    }

function FT_New_Glyph(library_:TFT_Library; format:TFT_Glyph_Format; aglyph:PFT_Glyph):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Get_Glyph
   *
   * @description:
   *   A function used to extract a glyph image from a slot.  Note that the
   *   created @FT_Glyph object must be released with @FT_Done_Glyph.
   *
   * @input:
   *   slot ::
   *     A handle to the source glyph slot.
   *
   * @output:
   *   aglyph ::
   *     A handle to the glyph object.  `NULL` in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Because `*aglyph->advance.x` and `*aglyph->advance.y` are 16.16
   *   fixed-point numbers, `slot->advance.x` and `slot->advance.y` (which
   *   are in 26.6 fixed-point format) must be in the range ]-32768;32768[.
    }
function FT_Get_Glyph(slot:TFT_GlyphSlot; aglyph:PFT_Glyph):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Glyph_Copy
   *
   * @description:
   *   A function used to copy a glyph image.  Note that the created
   *   @FT_Glyph object must be released with @FT_Done_Glyph.
   *
   * @input:
   *   source ::
   *     A handle to the source glyph object.
   *
   * @output:
   *   target ::
   *     A handle to the target glyph object.  `NULL` in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
    }
function FT_Glyph_Copy(source:TFT_Glyph; target:PFT_Glyph):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Glyph_Transform
   *
   * @description:
   *   Transform a glyph image if its format is scalable.
   *
   * @inout:
   *   glyph ::
   *     A handle to the target glyph object.
   *
   * @input:
   *   matrix ::
   *     A pointer to a 2x2 matrix to apply.
   *
   *   delta ::
   *     A pointer to a 2d vector to apply.  Coordinates are expressed in
   *     1/64 of a pixel.
   *
   * @return:
   *   FreeType error code (if not 0, the glyph format is not scalable).
   *
   * @note:
   *   The 2x2 transformation matrix is also applied to the glyph's advance
   *   vector.
    }
(* Const before type ignored *)
(* Const before type ignored *)
function FT_Glyph_Transform(glyph:TFT_Glyph; matrix:PFT_Matrix; delta:PFT_Vector):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @enum:
   *   FT_Glyph_BBox_Mode
   *
   * @description:
   *   The mode how the values of @FT_Glyph_Get_CBox are returned.
   *
   * @values:
   *   FT_GLYPH_BBOX_UNSCALED ::
   *     Return unscaled font units.
   *
   *   FT_GLYPH_BBOX_SUBPIXELS ::
   *     Return unfitted 26.6 coordinates.
   *
   *   FT_GLYPH_BBOX_GRIDFIT ::
   *     Return grid-fitted 26.6 coordinates.
   *
   *   FT_GLYPH_BBOX_TRUNCATE ::
   *     Return coordinates in integer pixels.
   *
   *   FT_GLYPH_BBOX_PIXELS ::
   *     Return grid-fitted pixel coordinates.
    }
type
  PFT_Glyph_BBox_Mode_ = ^TFT_Glyph_BBox_Mode_;
  TFT_Glyph_BBox_Mode_ =  Longint;
  Const
    FT_GLYPH_BBOX_UNSCALED = 0;
    FT_GLYPH_BBOX_SUBPIXELS = 0;
    FT_GLYPH_BBOX_GRIDFIT = 1;
    FT_GLYPH_BBOX_TRUNCATE = 2;
    FT_GLYPH_BBOX_PIXELS = 3;
;
  TFT_Glyph_BBox_Mode = TFT_Glyph_BBox_Mode_;
  PFT_Glyph_BBox_Mode = ^TFT_Glyph_BBox_Mode;
{ these constants are deprecated; use the corresponding  }
{ `FT_Glyph_BBox_Mode` values instead                    }
  ft_glyph_bbox_unscaled = FT_GLYPH_BBOX_UNSCALED;  
  ft_glyph_bbox_subpixels = FT_GLYPH_BBOX_SUBPIXELS;  
  ft_glyph_bbox_gridfit = FT_GLYPH_BBOX_GRIDFIT;  
  ft_glyph_bbox_truncate = FT_GLYPH_BBOX_TRUNCATE;  
  ft_glyph_bbox_pixels = FT_GLYPH_BBOX_PIXELS;  
{*************************************************************************
   *
   * @function:
   *   FT_Glyph_Get_CBox
   *
   * @description:
   *   Return a glyph's 'control box'.  The control box encloses all the
   *   outline's points, including Bezier control points.  Though it
   *   coincides with the exact bounding box for most glyphs, it can be
   *   slightly larger in some situations (like when rotating an outline that
   *   contains Bezier outside arcs).
   *
   *   Computing the control box is very fast, while getting the bounding box
   *   can take much more time as it needs to walk over all segments and arcs
   *   in the outline.  To get the latter, you can use the 'ftbbox'
   *   component, which is dedicated to this single task.
   *
   * @input:
   *   glyph ::
   *     A handle to the source glyph object.
   *
   *   mode ::
   *     The mode that indicates how to interpret the returned bounding box
   *     values.
   *
   * @output:
   *   acbox ::
   *     The glyph coordinate bounding box.  Coordinates are expressed in
   *     1/64 of pixels if it is grid-fitted.
   *
   * @note:
   *   Coordinates are relative to the glyph origin, using the y~upwards
   *   convention.
   *
   *   If the glyph has been loaded with @FT_LOAD_NO_SCALE, `bbox_mode` must
   *   be set to @FT_GLYPH_BBOX_UNSCALED to get unscaled font units in 26.6
   *   pixel format.  The value @FT_GLYPH_BBOX_SUBPIXELS is another name for
   *   this constant.
   *
   *   If the font is tricky and the glyph has been loaded with
   *   @FT_LOAD_NO_SCALE, the resulting CBox is meaningless.  To get
   *   reasonable values for the CBox it is necessary to load the glyph at a
   *   large ppem value (so that the hinting instructions can properly shift
   *   and scale the subglyphs), then extracting the CBox, which can be
   *   eventually converted back to font units.
   *
   *   Note that the maximum coordinates are exclusive, which means that one
   *   can compute the width and height of the glyph image (be it in integer
   *   or 26.6 pixels) as:
   *
   *   ```
   *     width  = bbox.xMax - bbox.xMin;
   *     height = bbox.yMax - bbox.yMin;
   *   ```
   *
   *   Note also that for 26.6 coordinates, if `bbox_mode` is set to
   *   @FT_GLYPH_BBOX_GRIDFIT, the coordinates will also be grid-fitted,
   *   which corresponds to:
   *
   *   ```
   *     bbox.xMin = FLOOR(bbox.xMin);
   *     bbox.yMin = FLOOR(bbox.yMin);
   *     bbox.xMax = CEILING(bbox.xMax);
   *     bbox.yMax = CEILING(bbox.yMax);
   *   ```
   *
   *   To get the bbox in pixel coordinates, set `bbox_mode` to
   *   @FT_GLYPH_BBOX_TRUNCATE.
   *
   *   To get the bbox in grid-fitted pixel coordinates, set `bbox_mode` to
   *   @FT_GLYPH_BBOX_PIXELS.
    }

procedure FT_Glyph_Get_CBox(glyph:TFT_Glyph; bbox_mode:TFT_UInt; acbox:PFT_BBox);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Glyph_To_Bitmap
   *
   * @description:
   *   Convert a given glyph object to a bitmap glyph object.
   *
   * @inout:
   *   the_glyph ::
   *     A pointer to a handle to the target glyph.
   *
   * @input:
   *   render_mode ::
   *     An enumeration that describes how the data is rendered.
   *
   *   origin ::
   *     A pointer to a vector used to translate the glyph image before
   *     rendering.  Can be~0 (if no translation).  The origin is expressed
   *     in 26.6 pixels.
   *
   *   destroy ::
   *     A boolean that indicates that the original glyph image should be
   *     destroyed by this function.  It is never destroyed in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function does nothing if the glyph format isn't scalable.
   *
   *   The glyph image is translated with the `origin` vector before
   *   rendering.
   *
   *   The first parameter is a pointer to an @FT_Glyph handle that will be
   *   _replaced_ by this function (with newly allocated data).  Typically,
   *   you would do something like the following (omitting error handling).
   *
   *   ```
   *     FT_Glyph        glyph;
   *     FT_BitmapGlyph  glyph_bitmap;
   *
   *
   *     // load glyph
   *     error = FT_Load_Char( face, glyph_index, FT_LOAD_DEFAULT );
   *
   *     // extract glyph image
   *     error = FT_Get_Glyph( face->glyph, &glyph );
   *
   *     // convert to a bitmap (default render mode + destroying old)
   *     if ( glyph->format != FT_GLYPH_FORMAT_BITMAP )
   *     
   *       error = FT_Glyph_To_Bitmap( &glyph, FT_RENDER_MODE_NORMAL,
   *                                   0, 1 );
   *       if ( error ) // `glyph' unchanged
   *         ...
   *     
   *
   *     // access bitmap content by typecasting
   *     glyph_bitmap = (FT_BitmapGlyph)glyph;
   *
   *     // do funny stuff with it, like blitting/drawing
   *     ...
   *
   *     // discard glyph image (bitmap or not)
   *     FT_Done_Glyph( glyph );
   *   ```
   *
   *   Here is another example, again without error handling.
   *
   *   ```
   *     FT_Glyph  glyphs[MAX_GLYPHS]
   *
   *
   *     ...
   *
   *     for ( idx = 0; i < MAX_GLYPHS; i++ )
   *       error = FT_Load_Glyph( face, idx, FT_LOAD_DEFAULT ) ||
   *               FT_Get_Glyph ( face->glyph, &glyphs[idx] );
   *
   *     ...
   *
   *     for ( idx = 0; i < MAX_GLYPHS; i++ )
   *     
   *       FT_Glyph  bitmap = glyphs[idx];
   *
   *
   *       ...
   *
   *       // after this call, `bitmap' no longer points into
   *       // the `glyphs' array (and the old value isn't destroyed)
   *       FT_Glyph_To_Bitmap( &bitmap, FT_RENDER_MODE_MONO, 0, 0 );
   *
   *       ...
   *
   *       FT_Done_Glyph( bitmap );
   *     
   *
   *     ...
   *
   *     for ( idx = 0; i < MAX_GLYPHS; i++ )
   *       FT_Done_Glyph( glyphs[idx] );
   *   ```
    }
(* Const before type ignored *)
function FT_Glyph_To_Bitmap(the_glyph:PFT_Glyph; render_mode:TFT_Render_Mode; origin:PFT_Vector; destroy:TFT_Bool):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Done_Glyph
   *
   * @description:
   *   Destroy a given glyph.
   *
   * @input:
   *   glyph ::
   *     A handle to the target glyph object.  Can be `NULL`.
    }
procedure FT_Done_Glyph(glyph:TFT_Glyph);cdecl; external freetype_lib;
{  }
{ other helpful functions  }
{*************************************************************************
   *
   * @section:
   *   computations
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_Matrix_Multiply
   *
   * @description:
   *   Perform the matrix operation `b = a*b`.
   *
   * @input:
   *   a ::
   *     A pointer to matrix `a`.
   *
   * @inout:
   *   b ::
   *     A pointer to matrix `b`.
   *
   * @note:
   *   The result is undefined if either `a` or `b` is zero.
   *
   *   Since the function uses wrap-around arithmetic, results become
   *   meaningless if the arguments are very large.
    }
(* Const before type ignored *)
procedure FT_Matrix_Multiply(a:PFT_Matrix; b:PFT_Matrix);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Matrix_Invert
   *
   * @description:
   *   Invert a 2x2 matrix.  Return an error if it can't be inverted.
   *
   * @inout:
   *   matrix ::
   *     A pointer to the target matrix.  Remains untouched in case of error.
   *
   * @return:
   *   FreeType error code.  0~means success.
    }
function FT_Matrix_Invert(matrix:PFT_Matrix):TFT_Error;cdecl; external freetype_lib;
{  }
{$endif}
{ FTGLYPH_H_  }
{ END  }
{ Local Variables:  }
{ coding: utf-8     }
{ End:              }
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




{*********************************************************************** }
{*********************************************************************** }
{                                                                        }
{                         F U N C T I O N S                              }
{                                                                        }
{*********************************************************************** }
{*********************************************************************** }


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


function FT_Render_Glyph(slot:TFT_GlyphSlot; render_mode:TFT_Render_Mode):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @enum:
   *   FT_Kerning_Mode
   *
   * @description:
   *   An enumeration to specify the format of kerning values returned by
   *   @FT_Get_Kerning.
   *
   * @values:
   *   FT_KERNING_DEFAULT ::
   *     Return grid-fitted kerning distances in 26.6 fractional pixels.
   *
   *   FT_KERNING_UNFITTED ::
   *     Return un-grid-fitted kerning distances in 26.6 fractional pixels.
   *
   *   FT_KERNING_UNSCALED ::
   *     Return the kerning vector in original font units.
   *
   * @note:
   *   `FT_KERNING_DEFAULT` returns full pixel values; it also makes FreeType
   *   heuristically scale down kerning distances at small ppem values so
   *   that they don't become too big.
   *
   *   Both `FT_KERNING_DEFAULT` and `FT_KERNING_UNFITTED` use the current
   *   horizontal scaling factor (as set e.g. with @FT_Set_Char_Size) to
   *   convert font units to pixels.
    }
type
  PFT_Kerning_Mode_ = ^TFT_Kerning_Mode_;
  TFT_Kerning_Mode_ =  Longint;
  Const
    FT_KERNING_DEFAULT = 0;
    FT_KERNING_UNFITTED = 1;
    FT_KERNING_UNSCALED = 2;
;
  TFT_Kerning_Mode = TFT_Kerning_Mode_;
  PFT_Kerning_Mode = ^TFT_Kerning_Mode;
{ these constants are deprecated; use the corresponding  }
{ `FT_Kerning_Mode` values instead                       }
  ft_kerning_default = FT_KERNING_DEFAULT;  
  ft_kerning_unfitted = FT_KERNING_UNFITTED;  
  ft_kerning_unscaled = FT_KERNING_UNSCALED;  
{*************************************************************************
   *
   * @function:
   *   FT_Get_Kerning
   *
   * @description:
   *   Return the kerning vector between two glyphs of the same face.
   *
   * @input:
   *   face ::
   *     A handle to a source face object.
   *
   *   left_glyph ::
   *     The index of the left glyph in the kern pair.
   *
   *   right_glyph ::
   *     The index of the right glyph in the kern pair.
   *
   *   kern_mode ::
   *     See @FT_Kerning_Mode for more information.  Determines the scale and
   *     dimension of the returned kerning vector.
   *
   * @output:
   *   akerning ::
   *     The kerning vector.  This is either in font units, fractional pixels
   *     (26.6 format), or pixels for scalable formats, and in pixels for
   *     fixed-sizes formats.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Only horizontal layouts (left-to-right & right-to-left) are supported
   *   by this method.  Other layouts, or more sophisticated kernings, are
   *   out of the scope of this API function -- they can be implemented
   *   through format-specific interfaces.
   *
   *   Note that, for TrueType fonts only, this can extract data from both
   *   the 'kern' table and the basic, pair-wise kerning feature from the
   *   GPOS table (with `TT_CONFIG_OPTION_GPOS_KERNING` enabled), though
   *   FreeType does not support the more advanced GPOS layout features; use
   *   a library like HarfBuzz for those instead.  If a font has both a
   *   'kern' table and kern features of a GPOS table, the 'kern' table will
   *   be used.
   *
   *   Also note for right-to-left scripts, the functionality may differ for
   *   fonts with GPOS tables vs. 'kern' tables.  For GPOS, right-to-left
   *   fonts typically use both a placement offset and an advance for pair
   *   positioning, which this API does not support, so it would output
   *   kerning values of zero; though if the right-to-left font used only
   *   advances in GPOS pair positioning, then this API could output kerning
   *   values for it, but it would use `left_glyph` to mean the first glyph
   *   for that case.  Whereas 'kern' tables are always advance-only and
   *   always store the left glyph first.
   *
   *   Use @FT_HAS_KERNING to find out whether a font has data that can be
   *   extracted with `FT_Get_Kerning`.
    }

function FT_Get_Kerning(face:TFT_Face; left_glyph:TFT_UInt; right_glyph:TFT_UInt; kern_mode:TFT_UInt; akerning:PFT_Vector):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Get_Track_Kerning
   *
   * @description:
   *   Return the track kerning for a given face object at a given size.
   *
   * @input:
   *   face ::
   *     A handle to a source face object.
   *
   *   point_size ::
   *     The point size in 16.16 fractional points.
   *
   *   degree ::
   *     The degree of tightness.  Increasingly negative values represent
   *     tighter track kerning, while increasingly positive values represent
   *     looser track kerning.  Value zero means no track kerning.
   *
   * @output:
   *   akerning ::
   *     The kerning in 16.16 fractional points, to be uniformly applied
   *     between all glyphs.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   Currently, only the Type~1 font driver supports track kerning, using
   *   data from AFM files (if attached with @FT_Attach_File or
   *   @FT_Attach_Stream).
   *
   *   Only very few AFM files come with track kerning data; please refer to
   *   Adobe's AFM specification for more details.
    }
function FT_Get_Track_Kerning(face:TFT_Face; point_size:TFT_Fixed; degree:TFT_Int; akerning:PFT_Fixed):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @section:
   *   character_mapping
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_Select_Charmap
   *
   * @description:
   *   Select a given charmap by its encoding tag (as listed in
   *   `freetype.h`).
   *
   * @inout:
   *   face ::
   *     A handle to the source face object.
   *
   * @input:
   *   encoding ::
   *     A handle to the selected encoding.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function returns an error if no charmap in the face corresponds
   *   to the encoding queried here.
   *
   *   Because many fonts contain more than a single cmap for Unicode
   *   encoding, this function has some special code to select the one that
   *   covers Unicode best ('best' in the sense that a UCS-4 cmap is
   *   preferred to a UCS-2 cmap).  It is thus preferable to @FT_Set_Charmap
   *   in this case.
    }
function FT_Select_Charmap(face:TFT_Face; encoding:TFT_Encoding):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Set_Charmap
   *
   * @description:
   *   Select a given charmap for character code to glyph index mapping.
   *
   * @inout:
   *   face ::
   *     A handle to the source face object.
   *
   * @input:
   *   charmap ::
   *     A handle to the selected charmap.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   This function returns an error if the charmap is not part of the face
   *   (i.e., if it is not listed in the `face->charmaps` table).
   *
   *   It also fails if an OpenType type~14 charmap is selected (which
   *   doesn't map character codes to glyph indices at all).
    }
function FT_Set_Charmap(face:TFT_Face; charmap:TFT_CharMap):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Get_Charmap_Index
   *
   * @description:
   *   Retrieve index of a given charmap.
   *
   * @input:
   *   charmap ::
   *     A handle to a charmap.
   *
   * @return:
   *   The index into the array of character maps within the face to which
   *   `charmap` belongs.  If an error occurs, -1 is returned.
   *
    }
function FT_Get_Charmap_Index(charmap:TFT_CharMap):TFT_Int;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Get_Char_Index
   *
   * @description:
   *   Return the glyph index of a given character code.  This function uses
   *   the currently selected charmap to do the mapping.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   charcode ::
   *     The character code.
   *
   * @return:
   *   The glyph index.  0~means 'undefined character code'.
   *
   * @note:
   *   If you use FreeType to manipulate the contents of font files directly,
   *   be aware that the glyph index returned by this function doesn't always
   *   correspond to the internal indices used within the file.  This is done
   *   to ensure that value~0 always corresponds to the 'missing glyph'.  If
   *   the first glyph is not named '.notdef', then for Type~1 and Type~42
   *   fonts, '.notdef' will be moved into the glyph ID~0 position, and
   *   whatever was there will be moved to the position '.notdef' had.  For
   *   Type~1 fonts, if there is no '.notdef' glyph at all, then one will be
   *   created at index~0 and whatever was there will be moved to the last
   *   index -- Type~42 fonts are considered invalid under this condition.
    }
function FT_Get_Char_Index(face:TFT_Face; charcode:TFT_ULong):TFT_UInt;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Get_First_Char
   *
   * @description:
   *   Return the first character code in the current charmap of a given
   *   face, together with its corresponding glyph index.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   * @output:
   *   agindex ::
   *     Glyph index of first character code.  0~if charmap is empty.
   *
   * @return:
   *   The charmap's first character code.
   *
   * @note:
   *   You should use this function together with @FT_Get_Next_Char to parse
   *   all character codes available in a given charmap.  The code should
   *   look like this:
   *
   *   ```
   *     FT_ULong  charcode;
   *     FT_UInt   gindex;
   *
   *
   *     charcode = FT_Get_First_Char( face, &gindex );
   *     while ( gindex != 0 )
   *     
   *       ... do something with (charcode,gindex) pair ...
   *
   *       charcode = FT_Get_Next_Char( face, charcode, &gindex );
   *     
   *   ```
   *
   *   Be aware that character codes can have values up to 0xFFFFFFFF; this
   *   might happen for non-Unicode or malformed cmaps.  However, even with
   *   regular Unicode encoding, so-called 'last resort fonts' (using SFNT
   *   cmap format 13, see function @FT_Get_CMap_Format) normally have
   *   entries for all Unicode characters up to 0x1FFFFF, which can cause *a
   *   lot* of iterations.
   *
   *   Note that `*agindex` is set to~0 if the charmap is empty.  The result
   *   itself can be~0 in two cases: if the charmap is empty or if the
   *   value~0 is the first valid character code.
    }
function FT_Get_First_Char(face:TFT_Face; agindex:PFT_UInt):TFT_ULong;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Get_Next_Char
   *
   * @description:
   *   Return the next character code in the current charmap of a given face
   *   following the value `char_code`, as well as the corresponding glyph
   *   index.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   char_code ::
   *     The starting character code.
   *
   * @output:
   *   agindex ::
   *     Glyph index of next character code.  0~if charmap is empty.
   *
   * @return:
   *   The charmap's next character code.
   *
   * @note:
   *   You should use this function with @FT_Get_First_Char to walk over all
   *   character codes available in a given charmap.  See the note for that
   *   function for a simple code example.
   *
   *   Note that `*agindex` is set to~0 when there are no more codes in the
   *   charmap.
    }
function FT_Get_Next_Char(face:TFT_Face; char_code:TFT_ULong; agindex:PFT_UInt):TFT_ULong;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @section:
   *   face_creation
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_Face_Properties
   *
   * @description:
   *   Set or override certain (library or module-wide) properties on a
   *   face-by-face basis.  Useful for finer-grained control and avoiding
   *   locks on shared structures (threads can modify their own faces as they
   *   see fit).
   *
   *   Contrary to @FT_Property_Set, this function uses @FT_Parameter so that
   *   you can pass multiple properties to the target face in one call.  Note
   *   that only a subset of the available properties can be controlled.
   *
   *   * @FT_PARAM_TAG_STEM_DARKENING (stem darkening, corresponding to the
   *     property `no-stem-darkening` provided by the 'autofit', 'cff',
   *     'type1', and 't1cid' modules; see @no-stem-darkening).
   *
   *   * @FT_PARAM_TAG_LCD_FILTER_WEIGHTS (LCD filter weights, corresponding
   *     to function @FT_Library_SetLcdFilterWeights).
   *
   *   * @FT_PARAM_TAG_RANDOM_SEED (seed value for the CFF, Type~1, and CID
   *     'random' operator, corresponding to the `random-seed` property
   *     provided by the 'cff', 'type1', and 't1cid' modules; see
   *     @random-seed).
   *
   *   Pass `NULL` as `data` in @FT_Parameter for a given tag to reset the
   *   option and use the library or module default again.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   num_properties ::
   *     The number of properties that follow.
   *
   *   properties ::
   *     A handle to an @FT_Parameter array with `num_properties` elements.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @example:
   *   Here is an example that sets three properties.  You must define
   *   `FT_CONFIG_OPTION_SUBPIXEL_RENDERING` to make the LCD filter examples
   *   work.
   *
   *   ```
   *     FT_Parameter         property1;
   *     FT_Bool              darken_stems = 1;
   *
   *     FT_Parameter         property2;
   *     FT_LcdFiveTapFilter  custom_weight =
   *                             0x11, 0x44, 0x56, 0x44, 0x11 ;
   *
   *     FT_Parameter         property3;
   *     FT_Int32             random_seed = 314159265;
   *
   *     FT_Parameter         properties[3] =  property1,
   *                                            property2,
   *                                            property3 ;
   *
   *
   *     property1.tag  = FT_PARAM_TAG_STEM_DARKENING;
   *     property1.data = &darken_stems;
   *
   *     property2.tag  = FT_PARAM_TAG_LCD_FILTER_WEIGHTS;
   *     property2.data = custom_weight;
   *
   *     property3.tag  = FT_PARAM_TAG_RANDOM_SEED;
   *     property3.data = &random_seed;
   *
   *     FT_Face_Properties( face, 3, properties );
   *   ```
   *
   *   The next example resets a single property to its default value.
   *
   *   ```
   *     FT_Parameter  property;
   *
   *
   *     property.tag  = FT_PARAM_TAG_LCD_FILTER_WEIGHTS;
   *     property.data = NULL;
   *
   *     FT_Face_Properties( face, 1, &property );
   *   ```
   *
   * @since:
   *   2.8
   *
    }
function FT_Face_Properties(face:TFT_Face; num_properties:TFT_UInt; properties:PFT_Parameter):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @section:
   *   information_retrieval
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_Get_Name_Index
   *
   * @description:
   *   Return the glyph index of a given glyph name.  This only works
   *   for those faces where @FT_HAS_GLYPH_NAMES returns true.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   glyph_name ::
   *     The glyph name.
   *
   * @return:
   *   The glyph index.  0~means 'undefined character code'.
   *
   * @note:
   *   Acceptable glyph names might come from the [Adobe Glyph
   *   List](https://github.com/adobe-type-tools/agl-aglfn).  See
   *   @FT_Get_Glyph_Name for the inverse functionality.
   *
   *   This function has limited capabilities if the config macro
   *   `FT_CONFIG_OPTION_POSTSCRIPT_NAMES` is not defined in `ftoption.h`:
   *   It then works only for fonts that actually embed glyph names (which
   *   many recent OpenType fonts do not).
    }
(* Const before type ignored *)
function FT_Get_Name_Index(face:TFT_Face; glyph_name:PFT_String):TFT_UInt;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Get_Glyph_Name
   *
   * @description:
   *   Retrieve the ASCII name of a given glyph in a face.  This only works
   *   for those faces where @FT_HAS_GLYPH_NAMES returns true.
   *
   * @input:
   *   face ::
   *     A handle to a source face object.
   *
   *   glyph_index ::
   *     The glyph index.
   *
   *   buffer_max ::
   *     The maximum number of bytes available in the buffer.
   *
   * @output:
   *   buffer ::
   *     A pointer to a target buffer where the name is copied to.
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   An error is returned if the face doesn't provide glyph names or if the
   *   glyph index is invalid.  In all cases of failure, the first byte of
   *   `buffer` is set to~0 to indicate an empty name.
   *
   *   The glyph name is truncated to fit within the buffer if it is too
   *   long.  The returned string is always zero-terminated.
   *
   *   Be aware that FreeType reorders glyph indices internally so that glyph
   *   index~0 always corresponds to the 'missing glyph' (called '.notdef').
   *
   *   This function has limited capabilities if the config macro
   *   `FT_CONFIG_OPTION_POSTSCRIPT_NAMES` is not defined in `ftoption.h`:
   *   It then works only for fonts that actually embed glyph names (which
   *   many recent OpenType fonts do not).
    }
function FT_Get_Glyph_Name(face:TFT_Face; glyph_index:TFT_UInt; buffer:TFT_Pointer; buffer_max:TFT_UInt):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Get_Postscript_Name
   *
   * @description:
   *   Retrieve the ASCII PostScript name of a given face, if available.
   *   This only works with PostScript, TrueType, and OpenType fonts.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   * @return:
   *   A pointer to the face's PostScript name.  `NULL` if unavailable.
   *
   * @note:
   *   The returned pointer is owned by the face and is destroyed with it.
   *
   *   For variation fonts, this string changes if you select a different
   *   instance, and you have to call `FT_Get_PostScript_Name` again to
   *   retrieve it.  FreeType follows Adobe TechNote #5902, 'Generating
   *   PostScript Names for Fonts Using OpenType Font Variations'.
   *
   *     https://download.macromedia.com/pub/developer/opentype/tech-notes/5902.AdobePSNameGeneration.html
   *
   *   [Since 2.9] Special PostScript names for named instances are only
   *   returned if the named instance is set with @FT_Set_Named_Instance (and
   *   the font has corresponding entries in its 'fvar' table or is the
   *   default named instance).  If @FT_IS_VARIATION returns true, the
   *   algorithmically derived PostScript name is provided, not looking up
   *   special entries for named instances.
    }
(* Const before type ignored *)
function FT_Get_Postscript_Name(face:TFT_Face):Pchar;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @enum:
   *   FT_SUBGLYPH_FLAG_XXX
   *
   * @description:
   *   A list of constants describing subglyphs.  Please refer to the 'glyf'
   *   table description in the OpenType specification for the meaning of the
   *   various flags (which get synthesized for non-OpenType subglyphs).
   *
   *     https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#composite-glyph-description
   *
   * @values:
   *   FT_SUBGLYPH_FLAG_ARGS_ARE_WORDS ::
   *   FT_SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES ::
   *   FT_SUBGLYPH_FLAG_ROUND_XY_TO_GRID ::
   *   FT_SUBGLYPH_FLAG_SCALE ::
   *   FT_SUBGLYPH_FLAG_XY_SCALE ::
   *   FT_SUBGLYPH_FLAG_2X2 ::
   *   FT_SUBGLYPH_FLAG_USE_MY_METRICS ::
   *
    }
const
  FT_SUBGLYPH_FLAG_ARGS_ARE_WORDS = 1;  
  FT_SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES = 2;  
  FT_SUBGLYPH_FLAG_ROUND_XY_TO_GRID = 4;  
  FT_SUBGLYPH_FLAG_SCALE = 8;  
  FT_SUBGLYPH_FLAG_XY_SCALE = $40;  
  FT_SUBGLYPH_FLAG_2X2 = $80;  
  FT_SUBGLYPH_FLAG_USE_MY_METRICS = $200;  
{*************************************************************************
   *
   * @function:
   *   FT_Get_SubGlyph_Info
   *
   * @description:
   *   Retrieve a description of a given subglyph.  Only use it if
   *   `glyph->format` is @FT_GLYPH_FORMAT_COMPOSITE; an error is returned
   *   otherwise.
   *
   * @input:
   *   glyph ::
   *     The source glyph slot.
   *
   *   sub_index ::
   *     The index of the subglyph.  Must be less than
   *     `glyph->num_subglyphs`.
   *
   * @output:
   *   p_index ::
   *     The glyph index of the subglyph.
   *
   *   p_flags ::
   *     The subglyph flags, see @FT_SUBGLYPH_FLAG_XXX.
   *
   *   p_arg1 ::
   *     The subglyph's first argument (if any).
   *
   *   p_arg2 ::
   *     The subglyph's second argument (if any).
   *
   *   p_transform ::
   *     The subglyph transformation (if any).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   The values of `*p_arg1`, `*p_arg2`, and `*p_transform` must be
   *   interpreted depending on the flags returned in `*p_flags`.  See the
   *   OpenType specification for details.
   *
   *     https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#composite-glyph-description
   *
    }

function FT_Get_SubGlyph_Info(glyph:TFT_GlyphSlot; sub_index:TFT_UInt; p_index:PFT_Int; p_flags:PFT_UInt; p_arg1:PFT_Int; 
           p_arg2:PFT_Int; p_transform:PFT_Matrix):TFT_Error;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @enum:
   *   FT_FSTYPE_XXX
   *
   * @description:
   *   A list of bit flags used in the `fsType` field of the OS/2 table in a
   *   TrueType or OpenType font and the `FSType` entry in a PostScript font.
   *   These bit flags are returned by @FT_Get_FSType_Flags; they inform
   *   client applications of embedding and subsetting restrictions
   *   associated with a font.
   *
   *   See
   *   https://www.adobe.com/content/dam/Adobe/en/devnet/acrobat/pdfs/FontPolicies.pdf
   *   for more details.
   *
   * @values:
   *   FT_FSTYPE_INSTALLABLE_EMBEDDING ::
   *     Fonts with no fsType bit set may be embedded and permanently
   *     installed on the remote system by an application.
   *
   *   FT_FSTYPE_RESTRICTED_LICENSE_EMBEDDING ::
   *     Fonts that have only this bit set must not be modified, embedded or
   *     exchanged in any manner without first obtaining permission of the
   *     font software copyright owner.
   *
   *   FT_FSTYPE_PREVIEW_AND_PRINT_EMBEDDING ::
   *     The font may be embedded and temporarily loaded on the remote
   *     system.  Documents containing Preview & Print fonts must be opened
   *     'read-only'; no edits can be applied to the document.
   *
   *   FT_FSTYPE_EDITABLE_EMBEDDING ::
   *     The font may be embedded but must only be installed temporarily on
   *     other systems.  In contrast to Preview & Print fonts, documents
   *     containing editable fonts may be opened for reading, editing is
   *     permitted, and changes may be saved.
   *
   *   FT_FSTYPE_NO_SUBSETTING ::
   *     The font may not be subsetted prior to embedding.
   *
   *   FT_FSTYPE_BITMAP_EMBEDDING_ONLY ::
   *     Only bitmaps contained in the font may be embedded; no outline data
   *     may be embedded.  If there are no bitmaps available in the font,
   *     then the font is unembeddable.
   *
   * @note:
   *   The flags are ORed together, thus more than a single value can be
   *   returned.
   *
   *   While the `fsType` flags can indicate that a font may be embedded, a
   *   license with the font vendor may be separately required to use the
   *   font in this way.
    }
const
  FT_FSTYPE_INSTALLABLE_EMBEDDING = $0000;  
  FT_FSTYPE_RESTRICTED_LICENSE_EMBEDDING = $0002;  
  FT_FSTYPE_PREVIEW_AND_PRINT_EMBEDDING = $0004;  
  FT_FSTYPE_EDITABLE_EMBEDDING = $0008;  
  FT_FSTYPE_NO_SUBSETTING = $0100;  
  FT_FSTYPE_BITMAP_EMBEDDING_ONLY = $0200;  
{*************************************************************************
   *
   * @function:
   *   FT_Get_FSType_Flags
   *
   * @description:
   *   Return the `fsType` flags for a font.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   * @return:
   *   The `fsType` flags, see @FT_FSTYPE_XXX.
   *
   * @note:
   *   Use this function rather than directly reading the `fs_type` field in
   *   the @PS_FontInfoRec structure, which is only guaranteed to return the
   *   correct results for Type~1 fonts.
   *
   * @since:
   *   2.3.8
   *
    }

function FT_Get_FSType_Flags(face:TFT_Face):TFT_UShort;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @section:
   *   glyph_variants
   *
   * @title:
   *   Unicode Variation Sequences
   *
   * @abstract:
   *   The FreeType~2 interface to Unicode Variation Sequences (UVS), using
   *   the SFNT cmap format~14.
   *
   * @description:
   *   Many characters, especially for CJK scripts, have variant forms.  They
   *   are a sort of grey area somewhere between being totally irrelevant and
   *   semantically distinct; for this reason, the Unicode consortium decided
   *   to introduce Variation Sequences (VS), consisting of a Unicode base
   *   character and a variation selector instead of further extending the
   *   already huge number of characters.
   *
   *   Unicode maintains two different sets, namely 'Standardized Variation
   *   Sequences' and registered 'Ideographic Variation Sequences' (IVS),
   *   collected in the 'Ideographic Variation Database' (IVD).
   *
   *     https://unicode.org/Public/UCD/latest/ucd/StandardizedVariants.txt
   *     https://unicode.org/reports/tr37/ https://unicode.org/ivd/
   *
   *   To date (January 2017), the character with the most ideographic
   *   variations is U+9089, having 32 such IVS.
   *
   *   Three Mongolian Variation Selectors have the values U+180B-U+180D; 256
   *   generic Variation Selectors are encoded in the ranges U+FE00-U+FE0F
   *   and U+E0100-U+E01EF.  IVS currently use Variation Selectors from the
   *   range U+E0100-U+E01EF only.
   *
   *   A VS consists of the base character value followed by a single
   *   Variation Selector.  For example, to get the first variation of
   *   U+9089, you have to write the character sequence `U+9089 U+E0100`.
   *
   *   Adobe and MS decided to support both standardized and ideographic VS
   *   with a new cmap subtable (format~14).  It is an odd subtable because
   *   it is not a mapping of input code points to glyphs, but contains lists
   *   of all variations supported by the font.
   *
   *   A variation may be either 'default' or 'non-default' for a given font.
   *   A default variation is the one you will get for that code point if you
   *   look it up in the standard Unicode cmap.  A non-default variation is a
   *   different glyph.
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_Face_GetCharVariantIndex
   *
   * @description:
   *   Return the glyph index of a given character code as modified by the
   *   variation selector.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   charcode ::
   *     The character code point in Unicode.
   *
   *   variantSelector ::
   *     The Unicode code point of the variation selector.
   *
   * @return:
   *   The glyph index.  0~means either 'undefined character code', or
   *   'undefined selector code', or 'no variation selector cmap subtable',
   *   or 'current CharMap is not Unicode'.
   *
   * @note:
   *   If you use FreeType to manipulate the contents of font files directly,
   *   be aware that the glyph index returned by this function doesn't always
   *   correspond to the internal indices used within the file.  This is done
   *   to ensure that value~0 always corresponds to the 'missing glyph'.
   *
   *   This function is only meaningful if
   *     a) the font has a variation selector cmap sub table, and
   *     b) the current charmap has a Unicode encoding.
   *
   * @since:
   *   2.3.6
   *
    }
function FT_Face_GetCharVariantIndex(face:TFT_Face; charcode:TFT_ULong; variantSelector:TFT_ULong):TFT_UInt;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Face_GetCharVariantIsDefault
   *
   * @description:
   *   Check whether this variation of this Unicode character is the one to
   *   be found in the charmap.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   charcode ::
   *     The character codepoint in Unicode.
   *
   *   variantSelector ::
   *     The Unicode codepoint of the variation selector.
   *
   * @return:
   *   1~if found in the standard (Unicode) cmap, 0~if found in the variation
   *   selector cmap, or -1 if it is not a variation.
   *
   * @note:
   *   This function is only meaningful if the font has a variation selector
   *   cmap subtable.
   *
   * @since:
   *   2.3.6
   *
    }
function FT_Face_GetCharVariantIsDefault(face:TFT_Face; charcode:TFT_ULong; variantSelector:TFT_ULong):TFT_Int;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Face_GetVariantSelectors
   *
   * @description:
   *   Return a zero-terminated list of Unicode variation selectors found in
   *   the font.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   * @return:
   *   A pointer to an array of selector code points, or `NULL` if there is
   *   no valid variation selector cmap subtable.
   *
   * @note:
   *   The last item in the array is~0; the array is owned by the @FT_Face
   *   object but can be overwritten or released on the next call to a
   *   FreeType function.
   *
   * @since:
   *   2.3.6
   *
    }
function FT_Face_GetVariantSelectors(face:TFT_Face):PFT_UInt32;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Face_GetVariantsOfChar
   *
   * @description:
   *   Return a zero-terminated list of Unicode variation selectors found for
   *   the specified character code.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   charcode ::
   *     The character codepoint in Unicode.
   *
   * @return:
   *   A pointer to an array of variation selector code points that are
   *   active for the given character, or `NULL` if the corresponding list is
   *   empty.
   *
   * @note:
   *   The last item in the array is~0; the array is owned by the @FT_Face
   *   object but can be overwritten or released on the next call to a
   *   FreeType function.
   *
   * @since:
   *   2.3.6
   *
    }
function FT_Face_GetVariantsOfChar(face:TFT_Face; charcode:TFT_ULong):PFT_UInt32;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Face_GetCharsOfVariant
   *
   * @description:
   *   Return a zero-terminated list of Unicode character codes found for the
   *   specified variation selector.
   *
   * @input:
   *   face ::
   *     A handle to the source face object.
   *
   *   variantSelector ::
   *     The variation selector code point in Unicode.
   *
   * @return:
   *   A list of all the code points that are specified by this selector
   *   (both default and non-default codes are returned) or `NULL` if there
   *   is no valid cmap or the variation selector is invalid.
   *
   * @note:
   *   The last item in the array is~0; the array is owned by the @FT_Face
   *   object but can be overwritten or released on the next call to a
   *   FreeType function.
   *
   * @since:
   *   2.3.6
   *
    }
function FT_Face_GetCharsOfVariant(face:TFT_Face; variantSelector:TFT_ULong):PFT_UInt32;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @section:
   *   computations
   *
   * @title:
   *   Computations
   *
   * @abstract:
   *   Crunching fixed numbers and vectors.
   *
   * @description:
   *   This section contains various functions used to perform computations
   *   on 16.16 fixed-point numbers or 2D vectors.  FreeType does not use
   *   floating-point data types.
   *
   *   **Attention**: Most arithmetic functions take `FT_Long` as arguments.
   *   For historical reasons, FreeType was designed under the assumption
   *   that `FT_Long` is a 32-bit integer; results can thus be undefined if
   *   the arguments don't fit into 32 bits.
   *
   * @order:
   *   FT_MulDiv
   *   FT_MulFix
   *   FT_DivFix
   *   FT_RoundFix
   *   FT_CeilFix
   *   FT_FloorFix
   *   FT_Vector_Transform
   *   FT_Matrix_Multiply
   *   FT_Matrix_Invert
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_MulDiv
   *
   * @description:
   *   Compute `(a*b)/c` with maximum accuracy, using a 64-bit intermediate
   *   integer whenever necessary.
   *
   *   This function isn't necessarily as fast as some processor-specific
   *   operations, but is at least completely portable.
   *
   * @input:
   *   a ::
   *     The first multiplier.
   *
   *   b ::
   *     The second multiplier.
   *
   *   c ::
   *     The divisor.
   *
   * @return:
   *   The result of `(a*b)/c`.  This function never traps when trying to
   *   divide by zero; it simply returns 'MaxInt' or 'MinInt' depending on
   *   the signs of `a` and `b`.
    }
function FT_MulDiv(a:TFT_Long; b:TFT_Long; c:TFT_Long):TFT_Long;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_MulFix
   *
   * @description:
   *   Compute `(a*b)/0x10000` with maximum accuracy.  Its main use is to
   *   multiply a given value by a 16.16 fixed-point factor.
   *
   * @input:
   *   a ::
   *     The first multiplier.
   *
   *   b ::
   *     The second multiplier.  Use a 16.16 factor here whenever possible
   *     (see note below).
   *
   * @return:
   *   The result of `(a*b)/0x10000`.
   *
   * @note:
   *   This function has been optimized for the case where the absolute value
   *   of `a` is less than 2048, and `b` is a 16.16 scaling factor.  As this
   *   happens mainly when scaling from notional units to fractional pixels
   *   in FreeType, it resulted in noticeable speed improvements between
   *   versions 2.x and 1.x.
   *
   *   As a conclusion, always try to place a 16.16 factor as the _second_
   *   argument of this function; this can make a great difference.
    }
function FT_MulFix(a:TFT_Long; b:TFT_Long):TFT_Long;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_DivFix
   *
   * @description:
   *   Compute `(a*0x10000)/b` with maximum accuracy.  Its main use is to
   *   divide a given value by a 16.16 fixed-point factor.
   *
   * @input:
   *   a ::
   *     The numerator.
   *
   *   b ::
   *     The denominator.  Use a 16.16 factor here.
   *
   * @return:
   *   The result of `(a*0x10000)/b`.
    }
function FT_DivFix(a:TFT_Long; b:TFT_Long):TFT_Long;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_RoundFix
   *
   * @description:
   *   Round a 16.16 fixed number.
   *
   * @input:
   *   a ::
   *     The number to be rounded.
   *
   * @return:
   *   `a` rounded to the nearest 16.16 fixed integer, halfway cases away
   *   from zero.
   *
   * @note:
   *   The function uses wrap-around arithmetic.
    }
function FT_RoundFix(a:TFT_Fixed):TFT_Fixed;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_CeilFix
   *
   * @description:
   *   Compute the smallest following integer of a 16.16 fixed number.
   *
   * @input:
   *   a ::
   *     The number for which the ceiling function is to be computed.
   *
   * @return:
   *   `a` rounded towards plus infinity.
   *
   * @note:
   *   The function uses wrap-around arithmetic.
    }
function FT_CeilFix(a:TFT_Fixed):TFT_Fixed;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_FloorFix
   *
   * @description:
   *   Compute the largest previous integer of a 16.16 fixed number.
   *
   * @input:
   *   a ::
   *     The number for which the floor function is to be computed.
   *
   * @return:
   *   `a` rounded towards minus infinity.
    }
function FT_FloorFix(a:TFT_Fixed):TFT_Fixed;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Vector_Transform
   *
   * @description:
   *   Transform a single vector through a 2x2 matrix.
   *
   * @inout:
   *   vector ::
   *     The target vector to transform.
   *
   * @input:
   *   matrix ::
   *     A pointer to the source 2x2 matrix.
   *
   * @note:
   *   The result is undefined if either `vector` or `matrix` is invalid.
    }
(* Const before type ignored *)
procedure FT_Vector_Transform(vector:PFT_Vector; matrix:PFT_Matrix);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @section:
   *   library_setup
   *
    }
{*************************************************************************
   *
   * @enum:
   *   FREETYPE_XXX
   *
   * @description:
   *   These three macros identify the FreeType source code version.  Use
   *   @FT_Library_Version to access them at runtime.
   *
   * @values:
   *   FREETYPE_MAJOR ::
   *     The major version number.
   *   FREETYPE_MINOR ::
   *     The minor version number.
   *   FREETYPE_PATCH ::
   *     The patch level.
   *
   * @note:
   *   The version number of FreeType if built as a dynamic link library with
   *   the 'libtool' package is _not_ controlled by these three macros.
   *
    }
const
  FREETYPE_MAJOR = 2;  
  FREETYPE_MINOR = 13;  
  FREETYPE_PATCH = 2;  
{*************************************************************************
   *
   * @function:
   *   FT_Library_Version
   *
   * @description:
   *   Return the version of the FreeType library being used.  This is useful
   *   when dynamically linking to the library, since one cannot use the
   *   macros @FREETYPE_MAJOR, @FREETYPE_MINOR, and @FREETYPE_PATCH.
   *
   * @input:
   *   library ::
   *     A source library handle.
   *
   * @output:
   *   amajor ::
   *     The major version number.
   *
   *   aminor ::
   *     The minor version number.
   *
   *   apatch ::
   *     The patch version number.
   *
   * @note:
   *   The reason why this function takes a `library` argument is because
   *   certain programs implement library initialization in a custom way that
   *   doesn't use @FT_Init_FreeType.
   *
   *   In such cases, the library version might not be available before the
   *   library object has been created.
    }

procedure FT_Library_Version(library_:TFT_Library; amajor:PFT_Int; aminor:PFT_Int; apatch:PFT_Int);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @section:
   *   other_api_data
   *
    }
{*************************************************************************
   *
   * @function:
   *   FT_Face_CheckTrueTypePatents
   *
   * @description:
   *   Deprecated, does nothing.
   *
   * @input:
   *   face ::
   *     A face handle.
   *
   * @return:
   *   Always returns false.
   *
   * @note:
   *   Since May 2010, TrueType hinting is no longer patented.
   *
   * @since:
   *   2.3.5
   *
    }
function FT_Face_CheckTrueTypePatents(face:TFT_Face):TFT_Bool;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Face_SetUnpatentedHinting
   *
   * @description:
   *   Deprecated, does nothing.
   *
   * @input:
   *   face ::
   *     A face handle.
   *
   *   value ::
   *     New boolean setting.
   *
   * @return:
   *   Always returns false.
   *
   * @note:
   *   Since May 2010, TrueType hinting is no longer patented.
   *
   * @since:
   *   2.3.5
   *
    }
function FT_Face_SetUnpatentedHinting(face:TFT_Face; value:TFT_Bool):TFT_Bool;cdecl; external freetype_lib;
{  }
{$endif}
{ FREETYPE_H_  }
{ END  }
{ }
{ ===========================  fttrigon.h  =========================== }
{ }
{***************************************************************************
 *
 * fttrigon.h
 *
 *   FreeType trigonometric functions (specification).
 *
 * Copyright (C) 2001-2024 by
 * David Turner, Robert Wilhelm, and Werner Lemberg.
 *
 * This file is part of the FreeType project, and may only be used,
 * modified, and distributed under the terms of the FreeType project
 * license, LICENSE.TXT.  By continuing to use, modify, or distribute
 * this file you indicate that you have read the license and
 * understand and accept it fully.
 *
  }
{$ifndef FTTRIGON_H_}
{$define FTTRIGON_H_}
{$include <freetype/freetype.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *  computations
   *
    }
{*************************************************************************
   *
   * @type:
   *   FT_Angle
   *
   * @description:
   *   This type is used to model angle values in FreeType.  Note that the
   *   angle is a 16.16 fixed-point value expressed in degrees.
   *
    }
type
  PFT_Angle = ^TFT_Angle;
  TFT_Angle = TFT_Fixed;
{*************************************************************************
   *
   * @macro:
   *   FT_ANGLE_PI
   *
   * @description:
   *   The angle pi expressed in @FT_Angle units.
   *
    }

const
  FT_ANGLE_PI = 180 shl 16;  
{*************************************************************************
   *
   * @macro:
   *   FT_ANGLE_2PI
   *
   * @description:
   *   The angle 2*pi expressed in @FT_Angle units.
   *
    }
{ #define FT_ANGLE_2PI  ( FT_ANGLE_PI * 2 ) }
{*************************************************************************
   *
   * @macro:
   *   FT_ANGLE_PI2
   *
   * @description:
   *   The angle pi/2 expressed in @FT_Angle units.
   *
    }
  FT_ANGLE_PI2 = FT_ANGLE_PI/2;  
{*************************************************************************
   *
   * @macro:
   *   FT_ANGLE_PI4
   *
   * @description:
   *   The angle pi/4 expressed in @FT_Angle units.
   *
    }
  FT_ANGLE_PI4 = FT_ANGLE_PI/4;  
{*************************************************************************
   *
   * @function:
   *   FT_Sin
   *
   * @description:
   *   Return the sinus of a given angle in fixed-point format.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
   * @return:
   *   The sinus value.
   *
   * @note:
   *   If you need both the sinus and cosinus for a given angle, use the
   *   function @FT_Vector_Unit.
   *
    }

function FT_Sin(angle:TFT_Angle):TFT_Fixed;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Cos
   *
   * @description:
   *   Return the cosinus of a given angle in fixed-point format.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
   * @return:
   *   The cosinus value.
   *
   * @note:
   *   If you need both the sinus and cosinus for a given angle, use the
   *   function @FT_Vector_Unit.
   *
    }
function FT_Cos(angle:TFT_Angle):TFT_Fixed;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Tan
   *
   * @description:
   *   Return the tangent of a given angle in fixed-point format.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
   * @return:
   *   The tangent value.
   *
    }
function FT_Tan(angle:TFT_Angle):TFT_Fixed;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Atan2
   *
   * @description:
   *   Return the arc-tangent corresponding to a given vector (x,y) in the 2d
   *   plane.
   *
   * @input:
   *   x ::
   *     The horizontal vector coordinate.
   *
   *   y ::
   *     The vertical vector coordinate.
   *
   * @return:
   *   The arc-tangent value (i.e. angle).
   *
    }
function FT_Atan2(x:TFT_Fixed; y:TFT_Fixed):TFT_Angle;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Angle_Diff
   *
   * @description:
   *   Return the difference between two angles.  The result is always
   *   constrained to the ]-PI..PI] interval.
   *
   * @input:
   *   angle1 ::
   *     First angle.
   *
   *   angle2 ::
   *     Second angle.
   *
   * @return:
   *   Constrained value of `angle2-angle1`.
   *
    }
function FT_Angle_Diff(angle1:TFT_Angle; angle2:TFT_Angle):TFT_Angle;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Vector_Unit
   *
   * @description:
   *   Return the unit vector corresponding to a given angle.  After the
   *   call, the value of `vec.x` will be `cos(angle)`, and the value of
   *   `vec.y` will be `sin(angle)`.
   *
   *   This function is useful to retrieve both the sinus and cosinus of a
   *   given angle quickly.
   *
   * @output:
   *   vec ::
   *     The address of target vector.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
    }
procedure FT_Vector_Unit(vec:PFT_Vector; angle:TFT_Angle);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Vector_Rotate
   *
   * @description:
   *   Rotate a vector by a given angle.
   *
   * @inout:
   *   vec ::
   *     The address of target vector.
   *
   * @input:
   *   angle ::
   *     The input angle.
   *
    }
procedure FT_Vector_Rotate(vec:PFT_Vector; angle:TFT_Angle);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Vector_Length
   *
   * @description:
   *   Return the length of a given vector.
   *
   * @input:
   *   vec ::
   *     The address of target vector.
   *
   * @return:
   *   The vector length, expressed in the same units that the original
   *   vector coordinates.
   *
    }
function FT_Vector_Length(vec:PFT_Vector):TFT_Fixed;cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Vector_Polarize
   *
   * @description:
   *   Compute both the length and angle of a given vector.
   *
   * @input:
   *   vec ::
   *     The address of source vector.
   *
   * @output:
   *   length ::
   *     The vector length.
   *
   *   angle ::
   *     The vector angle.
   *
    }
procedure FT_Vector_Polarize(vec:PFT_Vector; length:PFT_Fixed; angle:PFT_Angle);cdecl; external freetype_lib;
{*************************************************************************
   *
   * @function:
   *   FT_Vector_From_Polar
   *
   * @description:
   *   Compute vector coordinates from a length and angle.
   *
   * @output:
   *   vec ::
   *     The address of source vector.
   *
   * @input:
   *   length ::
   *     The vector length.
   *
   *   angle ::
   *     The vector angle.
   *
    }
procedure FT_Vector_From_Polar(vec:PFT_Vector; length:TFT_Fixed; angle:TFT_Angle);cdecl; external freetype_lib;
{  }
{$endif}
{ FTTRIGON_H_  }
{ END  }
{ }
{ ===========================  ftincrem.h  =========================== }
{ }
{***************************************************************************
 *
 * ftincrem.h
 *
 *   FreeType incremental loading (specification).
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
{$ifndef FTINCREM_H_}
{$define FTINCREM_H_}
{$include <freetype/freetype.h>}
{$include <freetype/ftparams.h>}
{$ifdef FREETYPE_H}
{$error "freetype.h of FreeType 1 has been loaded!"}
{$error "Please fix the directory search order for header files"}
{$error "so that freetype.h of FreeType 2 is found first."}
{$endif}
{*************************************************************************
   *
   * @section:
   *    incremental
   *
   * @title:
   *    Incremental Loading
   *
   * @abstract:
   *    Custom Glyph Loading.
   *
   * @description:
   *   This section contains various functions used to perform so-called
   *   'incremental' glyph loading.  This is a mode where all glyphs loaded
   *   from a given @FT_Face are provided by the client application.
   *
   *   Apart from that, all other tables are loaded normally from the font
   *   file.  This mode is useful when FreeType is used within another
   *   engine, e.g., a PostScript Imaging Processor.
   *
   *   To enable this mode, you must use @FT_Open_Face, passing an
   *   @FT_Parameter with the @FT_PARAM_TAG_INCREMENTAL tag and an
   *   @FT_Incremental_Interface value.  See the comments for
   *   @FT_Incremental_InterfaceRec for an example.
   *
    }
{*************************************************************************
   *
   * @type:
   *   FT_Incremental
   *
   * @description:
   *   An opaque type describing a user-provided object used to implement
   *   'incremental' glyph loading within FreeType.  This is used to support
   *   embedded fonts in certain environments (e.g., PostScript
   *   interpreters), where the glyph data isn't in the font file, or must be
   *   overridden by different values.
   *
   * @note:
   *   It is up to client applications to create and implement
   *   @FT_Incremental objects, as long as they provide implementations for
   *   the methods @FT_Incremental_GetGlyphDataFunc,
   *   @FT_Incremental_FreeGlyphDataFunc and
   *   @FT_Incremental_GetGlyphMetricsFunc.
   *
   *   See the description of @FT_Incremental_InterfaceRec to understand how
   *   to use incremental objects with FreeType.
   *
    }
type
  PFT_Incremental = ^TFT_Incremental;
  TFT_Incremental = PFT_IncrementalRec_;
{*************************************************************************
   *
   * @struct:
   *   FT_Incremental_MetricsRec
   *
   * @description:
   *   A small structure used to contain the basic glyph metrics returned by
   *   the @FT_Incremental_GetGlyphMetricsFunc method.
   *
   * @fields:
   *   bearing_x ::
   *     Left bearing, in font units.
   *
   *   bearing_y ::
   *     Top bearing, in font units.
   *
   *   advance ::
   *     Horizontal component of glyph advance, in font units.
   *
   *   advance_v ::
   *     Vertical component of glyph advance, in font units.
   *
   * @note:
   *   These correspond to horizontal or vertical metrics depending on the
   *   value of the `vertical` argument to the function
   *   @FT_Incremental_GetGlyphMetricsFunc.
   *
    }
{ since 2.3.12  }

  PFT_Incremental_MetricsRec_ = ^TFT_Incremental_MetricsRec_;
  TFT_Incremental_MetricsRec_ = record
      bearing_x : TFT_Long;
      bearing_y : TFT_Long;
      advance : TFT_Long;
      advance_v : TFT_Long;
    end;
  TFT_Incremental_MetricsRec = TFT_Incremental_MetricsRec_;
  PFT_Incremental_MetricsRec = ^TFT_Incremental_MetricsRec;
{*************************************************************************
   *
   * @struct:
   *   FT_Incremental_Metrics
   *
   * @description:
   *   A handle to an @FT_Incremental_MetricsRec structure.
   *
    }

  PFT_Incremental_Metrics = ^TFT_Incremental_Metrics;
  TFT_Incremental_Metrics = PFT_Incremental_MetricsRec_;
{*************************************************************************
   *
   * @type:
   *   FT_Incremental_GetGlyphDataFunc
   *
   * @description:
   *   A function called by FreeType to access a given glyph's data bytes
   *   during @FT_Load_Glyph or @FT_Load_Char if incremental loading is
   *   enabled.
   *
   *   Note that the format of the glyph's data bytes depends on the font
   *   file format.  For TrueType, it must correspond to the raw bytes within
   *   the 'glyf' table.  For PostScript formats, it must correspond to the
   *   **unencrypted** charstring bytes, without any `lenIV` header.  It is
   *   undefined for any other format.
   *
   * @input:
   *   incremental ::
   *     Handle to an opaque @FT_Incremental handle provided by the client
   *     application.
   *
   *   glyph_index ::
   *     Index of relevant glyph.
   *
   * @output:
   *   adata ::
   *     A structure describing the returned glyph data bytes (which will be
   *     accessed as a read-only byte block).
   *
   * @return:
   *   FreeType error code.  0~means success.
   *
   * @note:
   *   If this function returns successfully the method
   *   @FT_Incremental_FreeGlyphDataFunc will be called later to release the
   *   data bytes.
   *
   *   Nested calls to @FT_Incremental_GetGlyphDataFunc can happen for
   *   compound glyphs.
   *
    }

  TFT_Incremental_GetGlyphDataFunc = function (incremental:TFT_Incremental; glyph_index:TFT_UInt; adata:PFT_Data):TFT_Error;cdecl;
{*************************************************************************
   *
   * @type:
   *   FT_Incremental_FreeGlyphDataFunc
   *
   * @description:
   *   A function used to release the glyph data bytes returned by a
   *   successful call to @FT_Incremental_GetGlyphDataFunc.
   *
   * @input:
   *   incremental ::
   *     A handle to an opaque @FT_Incremental handle provided by the client
   *     application.
   *
   *   data ::
   *     A structure describing the glyph data bytes (which will be accessed
   *     as a read-only byte block).
   *
    }

  TFT_Incremental_FreeGlyphDataFunc = procedure (incremental:TFT_Incremental; data:PFT_Data);cdecl;
{*************************************************************************
   *
   * @type:
   *   FT_Incremental_GetGlyphMetricsFunc
   *
   * @description:
   *   A function used to retrieve the basic metrics of a given glyph index
   *   before accessing its data.  This allows for handling font types such
   *   as PCL~XL Format~1, Class~2 downloaded TrueType fonts, where the glyph
   *   metrics (`hmtx` and `vmtx` tables) are permitted to be omitted from
   *   the font, and the relevant metrics included in the header of the glyph
   *   outline data.  Importantly, this is not intended to allow custom glyph
   *   metrics (for example, Postscript Metrics dictionaries), because that
   *   conflicts with the requirements of outline hinting.  Such custom
   *   metrics must be handled separately, by the calling application.
   *
   * @input:
   *   incremental ::
   *     A handle to an opaque @FT_Incremental handle provided by the client
   *     application.
   *
   *   glyph_index ::
   *     Index of relevant glyph.
   *
   *   vertical ::
   *     If true, return vertical metrics.
   *
   *   ametrics ::
   *     This parameter is used for both input and output.  The original
   *     glyph metrics, if any, in font units.  If metrics are not available
   *     all the values must be set to zero.
   *
   * @output:
   *   ametrics ::
   *     The glyph metrics in font units.
   *
    }

  TFT_Incremental_GetGlyphMetricsFunc = function (incremental:TFT_Incremental; glyph_index:TFT_UInt; vertical:TFT_Bool; ametrics:PFT_Incremental_MetricsRec):TFT_Error;cdecl;
{*************************************************************************
   *
   * @struct:
   *   FT_Incremental_FuncsRec
   *
   * @description:
   *   A table of functions for accessing fonts that load data incrementally.
   *   Used in @FT_Incremental_InterfaceRec.
   *
   * @fields:
   *   get_glyph_data ::
   *     The function to get glyph data.  Must not be null.
   *
   *   free_glyph_data ::
   *     The function to release glyph data.  Must not be null.
   *
   *   get_glyph_metrics ::
   *     The function to get glyph metrics.  May be null if the font does not
   *     require it.
   *
    }

  PFT_Incremental_FuncsRec_ = ^TFT_Incremental_FuncsRec_;
  TFT_Incremental_FuncsRec_ = record
      get_glyph_data : TFT_Incremental_GetGlyphDataFunc;
      free_glyph_data : TFT_Incremental_FreeGlyphDataFunc;
      get_glyph_metrics : TFT_Incremental_GetGlyphMetricsFunc;
    end;
  TFT_Incremental_FuncsRec = TFT_Incremental_FuncsRec_;
  PFT_Incremental_FuncsRec = ^TFT_Incremental_FuncsRec;
{*************************************************************************
   *
   * @struct:
   *   FT_Incremental_InterfaceRec
   *
   * @description:
   *   A structure to be used with @FT_Open_Face to indicate that the user
   *   wants to support incremental glyph loading.  You should use it with
   *   @FT_PARAM_TAG_INCREMENTAL as in the following example:
   *
   *   ```
   *     FT_Incremental_InterfaceRec  inc_int;
   *     FT_Parameter                 parameter;
   *     FT_Open_Args                 open_args;
   *
   *
   *     // set up incremental descriptor
   *     inc_int.funcs  = my_funcs;
   *     inc_int.object = my_object;
   *
   *     // set up optional parameter
   *     parameter.tag  = FT_PARAM_TAG_INCREMENTAL;
   *     parameter.data = &inc_int;
   *
   *     // set up FT_Open_Args structure
   *     open_args.flags      = FT_OPEN_PATHNAME | FT_OPEN_PARAMS;
   *     open_args.pathname   = my_font_pathname;
   *     open_args.num_params = 1;
   *     open_args.params     = &parameter; // we use one optional argument
   *
   *     // open the font
   *     error = FT_Open_Face( library, &open_args, index, &face );
   *     ...
   *   ```
   *
    }
(* Const before type ignored *)

  PFT_Incremental_InterfaceRec_ = ^TFT_Incremental_InterfaceRec_;
  TFT_Incremental_InterfaceRec_ = record
      funcs : PFT_Incremental_FuncsRec;
      object : TFT_Incremental;
    end;
  TFT_Incremental_InterfaceRec = TFT_Incremental_InterfaceRec_;
  PFT_Incremental_InterfaceRec = ^TFT_Incremental_InterfaceRec;
{*************************************************************************
   *
   * @type:
   *   FT_Incremental_Interface
   *
   * @description:
   *   A pointer to an @FT_Incremental_InterfaceRec structure.
   *
    }

  PFT_Incremental_Interface = ^TFT_Incremental_Interface;
  TFT_Incremental_Interface = PFT_Incremental_InterfaceRec;
{  }
{$endif}
{ FTINCREM_H_  }
{ END  }
{ }
{ ===========================  fterrors.h  =========================== }
{ }
{***************************************************************************
 *
 * fterrors.h
 *
 *   FreeType error code handling (specification).
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
   * @section:
   *   error_enumerations
   *
   * @title:
   *   Error Enumerations
   *
   * @abstract:
   *   How to handle errors and error strings.
   *
   * @description:
   *   The header file `fterrors.h` (which is automatically included by
   *   `freetype.h`) defines the handling of FreeType's enumeration
   *   constants.  It can also be used to generate error message strings
   *   with a small macro trick explained below.
   *
   *   **Error Formats**
   *
   *   The configuration macro `FT_CONFIG_OPTION_USE_MODULE_ERRORS` can be
   *   defined in `ftoption.h` in order to make the higher byte indicate the
   *   module where the error has happened (this is not compatible with
   *   standard builds of FreeType~2, however).  See the file `ftmoderr.h`
   *   for more details.
   *
   *   **Error Message Strings**
   *
   *   Error definitions are set up with special macros that allow client
   *   applications to build a table of error message strings.  The strings
   *   are not included in a normal build of FreeType~2 to save space (most
   *   client applications do not use them).
   *
   *   To do so, you have to define the following macros before including
   *   this file.
   *
   *   ```
   *     FT_ERROR_START_LIST
   *   ```
   *
   *   This macro is called before anything else to define the start of the
   *   error list.  It is followed by several `FT_ERROR_DEF` calls.
   *
   *   ```
   *     FT_ERROR_DEF( e, v, s )
   *   ```
   *
   *   This macro is called to define one single error.  'e' is the error
   *   code identifier (e.g., `Invalid_Argument`), 'v' is the error's
   *   numerical value, and 's' is the corresponding error string.
   *
   *   ```
   *     FT_ERROR_END_LIST
   *   ```
   *
   *   This macro ends the list.
   *
   *   Additionally, you have to undefine `FTERRORS_H_` before #including
   *   this file.
   *
   *   Here is a simple example.
   *
   *   ```
   *     #undef FTERRORS_H_
   *     #define FT_ERRORDEF( e, v, s )   e, s ,
   *     #define FT_ERROR_START_LIST     
   *     #define FT_ERROR_END_LIST        0, NULL  ;
   *
   *     const struct
   *     
   *       int          err_code;
   *       const char*  err_msg;
   *      ft_errors[] =
   *
   *     #include <freetype/fterrors.h>
   *   ```
   *
   *   An alternative to using an array is a switch statement.
   *
   *   ```
   *     #undef FTERRORS_H_
   *     #define FT_ERROR_START_LIST     switch ( error_code ) 
   *     #define FT_ERRORDEF( e, v, s )    case v: return s;
   *     #define FT_ERROR_END_LIST       
   *   ```
   *
   *   If you use `FT_CONFIG_OPTION_USE_MODULE_ERRORS`, `error_code` should
   *   be replaced with `FT_ERROR_BASE(error_code)` in the last example.
    }
{  }
{ In previous FreeType versions we used `__FTERRORS_H__`.  However,  }
{ using two successive underscores in a non-system symbol name       }
{ violates the C (and C++) standard, so it was changed to the        }
{ current form.  In spite of this, we have to make                   }
{                                                                    }
{ ```                                                                }
{   #undefine __FTERRORS_H__                                         }
{ ```                                                                }
{                                                                    }
{ work for backward compatibility.                                   }
{                                                                    }
{$if !( defined( FTERRORS_H_ ) && defined ( __FTERRORS_H__ ) )}
{$define FTERRORS_H_}
{$define __FTERRORS_H__}
{ include module base error codes  }
{$include <freetype/ftmoderr.h>}
{***************************************************************** }
{***************************************************************** }
{****                                                         **** }
{****                       SETUP MACROS                      **** }
{****                                                         **** }
{***************************************************************** }
{***************************************************************** }
{$undef  FT_NEED_EXTERN_C}
{ FT_ERR_PREFIX is used as a prefix for error identifiers.  }
{ By default, we use `FT_Err_`.                             }
{                                                           }
{$ifndef FT_ERR_PREFIX}

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
{#ifdef FT_NEED_EXTERN_C }
{   }
{#endif }
{#undef FT_ERROR_START_LIST }
{#undef FT_ERROR_END_LIST }
{#undef FT_ERRORDEF }
{#undef FT_ERRORDEF_ }
{#undef FT_NOERRORDEF_ }
{#undef FT_NEED_EXTERN_C }
{#undef FT_ERR_BASE }
{ FT_ERR_PREFIX is needed internally  }
{#ifndef FT2_BUILD_LIBRARY }
{#undef FT_ERR_PREFIX }
{#endif }
{ FT_INCLUDE_ERR_PROTOS: Control whether function prototypes should be  }
{                        included with                                  }
{                                                                       }
{                          #include <freetype/fterrors.h>               }
{                                                                       }
{                        This is only true where `FT_ERRORDEF` is       }
{                        undefined.                                     }
{                                                                       }
{ FT_ERR_PROTOS_DEFINED: Actual multiple-inclusion protection of        }
{                        `fterrors.h`.                                  }
{#ifdef FT_INCLUDE_ERR_PROTOS }
{#undef FT_INCLUDE_ERR_PROTOS }
{#ifndef FT_ERR_PROTOS_DEFINED }
{#define FT_ERR_PROTOS_DEFINED }
{*************************************************************************
   *
   * @function:
   *   FT_Error_String
   *
   * @description:
   *   Retrieve the description of a valid FreeType error code.
   *
   * @input:
   *   error_code ::
   *     A valid FreeType error code.
   *
   * @return:
   *   A C~string or `NULL`, if any error occurred.
   *
   * @note:
   *   FreeType has to be compiled with `FT_CONFIG_OPTION_ERROR_STRINGS` or
   *   `FT_DEBUG_LEVEL_ERROR` to get meaningful descriptions.
   *   'error_string' will be `NULL` otherwise.
   *
   *   Module identification will be ignored:
   *
   *   ```c
   *     strcmp( FT_Error_String(  FT_Err_Unknown_File_Format ),
   *             FT_Error_String( BDF_Err_Unknown_File_Format ) ) == 0;
   *   ```
    }
(* Const before type ignored *)

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

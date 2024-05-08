
unit ftsystem;
interface

{
  Automatically converted by H2Pas 1.0.0 from ftsystem.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    ftsystem.h
}

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

Type
Pbyte  = ^byte;
PFT_Alloc_Func  = ^FT_Alloc_Func;
PFT_Memory  = ^FT_Memory;
PFT_MemoryRec_  = ^FT_MemoryRec_;
PFT_Realloc_Func  = ^FT_Realloc_Func;
PFT_Stream  = ^FT_Stream;
PFT_StreamDesc  = ^FT_StreamDesc;
PFT_StreamDesc_  = ^FT_StreamDesc_;
PFT_StreamRec  = ^FT_StreamRec;
PFT_StreamRec_  = ^FT_StreamRec_;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{***************************************************************************
 *
 * ftsystem.h
 *
 *   FreeType low-level system interface definition (specification).
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
{$ifndef FTSYSTEM_H_}
{$define FTSYSTEM_H_}
{*************************************************************************
   *
   * @section:
   *  system_interface
   *
   * @title:
   *  System Interface
   *
   * @abstract:
   *  How FreeType manages memory and i/o.
   *
   * @description:
   *  This section contains various definitions related to memory management
   *  and i/o access.  You need to understand this information if you want to
   *  use a custom memory manager or you own i/o streams.
   *
    }
{*************************************************************************
   *
   *                 M E M O R Y   M A N A G E M E N T
   *
    }
{*************************************************************************
   *
   * @type:
   *   FT_Memory
   *
   * @description:
   *   A handle to a given memory manager object, defined with an
   *   @FT_MemoryRec structure.
   *
    }
type
  PFT_Memory = ^TFT_Memory;
  TFT_Memory = PFT_MemoryRec_;
{*************************************************************************
   *
   * @functype:
   *   FT_Alloc_Func
   *
   * @description:
   *   A function used to allocate `size` bytes from `memory`.
   *
   * @input:
   *   memory ::
   *     A handle to the source memory manager.
   *
   *   size ::
   *     The size in bytes to allocate.
   *
   * @return:
   *   Address of new memory block.  0~in case of failure.
   *
    }

  PFT_Alloc_Func = ^TFT_Alloc_Func;
  TFT_Alloc_Func = function (memory:TFT_Memory; size:longint):pointer;cdecl;
{*************************************************************************
   *
   * @functype:
   *   FT_Free_Func
   *
   * @description:
   *   A function used to release a given block of memory.
   *
   * @input:
   *   memory ::
   *     A handle to the source memory manager.
   *
   *   block ::
   *     The address of the target memory block.
   *
    }

  TFT_Free_Func = procedure (memory:TFT_Memory; block:pointer);cdecl;
{*************************************************************************
   *
   * @functype:
   *   FT_Realloc_Func
   *
   * @description:
   *   A function used to re-allocate a given block of memory.
   *
   * @input:
   *   memory ::
   *     A handle to the source memory manager.
   *
   *   cur_size ::
   *     The block's current size in bytes.
   *
   *   new_size ::
   *     The block's requested new size.
   *
   *   block ::
   *     The block's current address.
   *
   * @return:
   *   New block address.  0~in case of memory shortage.
   *
   * @note:
   *   In case of error, the old block must still be available.
   *
    }

  PFT_Realloc_Func = ^TFT_Realloc_Func;
  TFT_Realloc_Func = function (memory:TFT_Memory; cur_size:longint; new_size:longint; block:pointer):pointer;cdecl;
{*************************************************************************
   *
   * @struct:
   *   FT_MemoryRec
   *
   * @description:
   *   A structure used to describe a given memory manager to FreeType~2.
   *
   * @fields:
   *   user ::
   *     A generic typeless pointer for user data.
   *
   *   alloc ::
   *     A pointer type to an allocation function.
   *
   *   free ::
   *     A pointer type to an memory freeing function.
   *
   *   realloc ::
   *     A pointer type to a reallocation function.
   *
    }
  PFT_MemoryRec_ = ^TFT_MemoryRec_;
  TFT_MemoryRec_ = record
      user : pointer;cdecl;
      alloc : TFT_Alloc_Func;
      free : TFT_Free_Func;
      realloc : TFT_Realloc_Func;
    end;

{*************************************************************************
   *
   *                      I / O   M A N A G E M E N T
   *
    }
{*************************************************************************
   *
   * @type:
   *   FT_Stream
   *
   * @description:
   *   A handle to an input stream.
   *
   * @also:
   *   See @FT_StreamRec for the publicly accessible fields of a given stream
   *   object.
   *
    }

  PFT_Stream = ^TFT_Stream;
  TFT_Stream = PFT_StreamRec_;
{*************************************************************************
   *
   * @struct:
   *   FT_StreamDesc
   *
   * @description:
   *   A union type used to store either a long or a pointer.  This is used
   *   to store a file descriptor or a `FILE*` in an input stream.
   *
    }

  PFT_StreamDesc_ = ^TFT_StreamDesc_;
  TFT_StreamDesc_ = record
      case longint of
        0 : ( value : longint );
        1 : ( pointer : pointer );
      end;
  TFT_StreamDesc = TFT_StreamDesc_;
  PFT_StreamDesc = ^TFT_StreamDesc;
{*************************************************************************
   *
   * @functype:
   *   FT_Stream_IoFunc
   *
   * @description:
   *   A function used to seek and read data from a given input stream.
   *
   * @input:
   *   stream ::
   *     A handle to the source stream.
   *
   *   offset ::
   *     The offset from the start of the stream to seek to.
   *
   *   buffer ::
   *     The address of the read buffer.
   *
   *   count ::
   *     The number of bytes to read from the stream.
   *
   * @return:
   *   If count >~0, return the number of bytes effectively read by the
   *   stream (after seeking to `offset`).  If count ==~0, return the status
   *   of the seek operation (non-zero indicates an error).
   *
    }

  TFT_Stream_IoFunc = function (stream:TFT_Stream; offset:dword; buffer:Pbyte; count:dword):dword;cdecl;
{*************************************************************************
   *
   * @functype:
   *   FT_Stream_CloseFunc
   *
   * @description:
   *   A function used to close a given input stream.
   *
   * @input:
   *  stream ::
   *    A handle to the target stream.
   *
    }

  TFT_Stream_CloseFunc = procedure (stream:TFT_Stream);cdecl;
{*************************************************************************
   *
   * @struct:
   *   FT_StreamRec
   *
   * @description:
   *   A structure used to describe an input stream.
   *
   * @input:
   *   base ::
   *     For memory-based streams, this is the address of the first stream
   *     byte in memory.  This field should always be set to `NULL` for
   *     disk-based streams.
   *
   *   size ::
   *     The stream size in bytes.
   *
   *     In case of compressed streams where the size is unknown before
   *     actually doing the decompression, the value is set to 0x7FFFFFFF.
   *     (Note that this size value can occur for normal streams also; it is
   *     thus just a hint.)
   *
   *   pos ::
   *     The current position within the stream.
   *
   *   descriptor ::
   *     This field is a union that can hold an integer or a pointer.  It is
   *     used by stream implementations to store file descriptors or `FILE*`
   *     pointers.
   *
   *   pathname ::
   *     This field is completely ignored by FreeType.  However, it is often
   *     useful during debugging to use it to store the stream's filename
   *     (where available).
   *
   *   read ::
   *     The stream's input function.
   *
   *   close ::
   *     The stream's close function.
   *
   *   memory ::
   *     The memory manager to use to preload frames.  This is set internally
   *     by FreeType and shouldn't be touched by stream implementations.
   *
   *   cursor ::
   *     This field is set and used internally by FreeType when parsing
   *     frames.  In particular, the `FT_GET_XXX` macros use this instead of
   *     the `pos` field.
   *
   *   limit ::
   *     This field is set and used internally by FreeType when parsing
   *     frames.
   *
    }

  PFT_StreamRec_ = ^TFT_StreamRec_;
  TFT_StreamRec_ = record
      base : Pbyte;
      size : dword;
      pos : dword;
      descriptor : TFT_StreamDesc;
      pathname : TFT_StreamDesc;
      read : TFT_Stream_IoFunc;
      close : TFT_Stream_CloseFunc;
      memory : TFT_Memory;
      cursor : Pbyte;
      limit : Pbyte;
    end;
  TFT_StreamRec = TFT_StreamRec_;
  PFT_StreamRec = ^TFT_StreamRec;
{  }
{$endif}
{ FTSYSTEM_H_  }
{ END  }

implementation


end.

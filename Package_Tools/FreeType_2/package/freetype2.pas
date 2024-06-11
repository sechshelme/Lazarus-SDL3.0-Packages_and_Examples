unit FreeType2;

interface

uses
  ctypes;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
  FT_CHAR_BIT = 8;
  FT_SIZEOF_INT = Sizeof(cint);
  FT_SIZEOF_LONG =  Sizeof(clong);
  FT_SIZEOF_LONG_LONG =  Sizeof(clonglong);

type
  // ======= integer-types.h
  PFT_Int16 = ^TFT_Int16;
  TFT_Int16 = Int16;

  PFT_UInt16 = ^TFT_UInt16;
  TFT_UInt16 = UInt16;

  PFT_Int32 = ^TFT_Int32;
  TFT_Int32 = Int32;

  PFT_UInt32 = ^TFT_UInt32;
  TFT_UInt32 = UInt32;

  PFT_Int64 = ^TFT_Int64;
  TFT_Int64 = int64;

  PFT_UInt64 = ^TFT_UInt64;
  TFT_UInt64 = UInt64;

  PFT_Fast = ^TFT_Fast;
  TFT_Fast = longint;

  PFT_UFast = ^TFT_UFast;
  TFT_UFast = dword;

  // ======== fttypes.h

  PFT_Bool = ^TFT_Bool;
  TFT_Bool = Boolean8;

  PFT_FWord = ^TFT_FWord;
  TFT_FWord = csshort;

  PFT_UFWord = ^TFT_UFWord;
  TFT_UFWord = cushort;

  PFT_Char = ^TFT_Char;
  TFT_Char = cschar;

  PPFT_Byte = ^PFT_Byte;
  PFT_Byte = ^TFT_Byte;
  TFT_Byte = cuchar;

  PFT_Bytes = ^TFT_Bytes;
  TFT_Bytes = PFT_Byte;

  PFT_Tag = ^TFT_Tag;
  TFT_Tag = TFT_UInt32;

  PFT_String = ^TFT_String;
  TFT_String = char;

  PFT_Short = ^TFT_Short;
  TFT_Short = csshort;

  PFT_UShort = ^TFT_UShort;
  TFT_UShort = cushort;

  PFT_Int = ^TFT_Int;
  TFT_Int = csint;

  PFT_UInt = ^TFT_UInt;
  TFT_UInt = cuint;

  PFT_Long = ^TFT_Long;
  TFT_Long = cslong;

  PFT_ULong = ^TFT_ULong;
  TFT_ULong = culong;

  PFT_F2Dot14 = ^TFT_F2Dot14;
  TFT_F2Dot14 = csshort;

  PFT_F26Dot6 = ^TFT_F26Dot6;
  TFT_F26Dot6 = cslong;

  PFT_Fixed = ^TFT_Fixed;
  TFT_Fixed = cslong;

  PFT_Error = ^TFT_Error;
  TFT_Error = cint;

  PFT_Pointer = ^TFT_Pointer;
  TFT_Pointer = Pointer;

  PFT_Offset = ^TFT_Offset;
  TFT_Offset = csize_t;

  PFT_PtrDist = ^TFT_PtrDist;
  TFT_PtrDist = SizeUInt;
// =====================


PFT_Library = ^TFT_Library;
TFT_Library = Pointer;


PFT_FaceRec = ^TFT_FaceRec;
TFT_FaceRec = record
    num_faces : TFT_Long;
    face_index : TFT_Long;
    face_flags : TFT_Long;
    style_flags : TFT_Long;
    num_glyphs : TFT_Long;
    family_name : PFT_String;
    style_name : PFT_String;
    num_fixed_sizes : TFT_Int;
    available_sizes : PFT_Bitmap_Size;
    num_charmaps : TFT_Int;
    charmaps : PFT_CharMap;
    generic : TFT_Generic;
    bbox : TFT_BBox;
    units_per_EM : TFT_UShort;
    ascender : TFT_Short;
    descender : TFT_Short;
    height : TFT_Short;
    max_advance_width : TFT_Short;
    max_advance_height : TFT_Short;
    underline_position : TFT_Short;
    underline_thickness : TFT_Short;
    glyph : TFT_GlyphSlot;
    size : TFT_Size;
    charmap : TFT_CharMap;
    driver : TFT_Driver;
    memory : TFT_Memory;
    stream : TFT_Stream;
    sizes_list : TFT_ListRec;
    autohint : TFT_Generic;
    extensions : pointer;
    internal : TFT_Face_Internal;
  end;

PFT_Face = ^TFT_Face;
TFT_Face = ^TFT_FaceRec;

PFT_Bitmap = ^TFT_Bitmap;
TFT_Bitmap = record
    rows : dword;
    width : dword;
    pitch : longint;
    buffer : Pbyte;
    num_grays : word;
    pixel_mode : byte;
    palette_mode : byte;
    palette : pointer;
  end;

implementation

end.


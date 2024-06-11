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
  FT_SIZEOF_LONG = Sizeof(clong);
  FT_SIZEOF_LONG_LONG = Sizeof(clonglong);

type
  // ======= integer-types.h
  PFT_Int16 = ^TFT_Int16;
  TFT_Int16 = int16;

  PFT_UInt16 = ^TFT_UInt16;
  TFT_UInt16 = uint16;

  PFT_Int32 = ^TFT_Int32;
  TFT_Int32 = int32;

  PFT_UInt32 = ^TFT_UInt32;
  TFT_UInt32 = uint32;

  PFT_Int64 = ^TFT_Int64;
  TFT_Int64 = int64;

  PFT_UInt64 = ^TFT_UInt64;
  TFT_UInt64 = uint64;

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

  PFT_Pos = ^TFT_Pos;
  TFT_Pos = longint;

  PFT_Vector = ^TFT_Vector;

  TFT_Vector = record
    x: TFT_Pos;
    y: TFT_Pos;
  end;

  PFT_BBox_ = ^TFT_BBox;

  TFT_BBox = record
    xMin: TFT_Pos;
    yMin: TFT_Pos;
    xMax: TFT_Pos;
    yMax: TFT_Pos;
  end;

  // =====================

  TFT_Encoding = integer; // enum, ??????????????'
  TFT_Glyph_Format = integer;


  PFT_Library = ^TFT_Library;
  TFT_Library = Pointer;  // PFT_LibraryRec_

  // =====================

  PFT_Glyph_Metrics = ^TFT_Glyph_Metrics;

  TFT_Glyph_Metrics = record
    Width: TFT_Pos;
    Height: TFT_Pos;
    horiBearingX: TFT_Pos;
    horiBearingY: TFT_Pos;
    horiAdvance: TFT_Pos;
    vertBearingX: TFT_Pos;
    vertBearingY: TFT_Pos;
    vertAdvance: TFT_Pos;
  end;


  TFT_Generic_Finalizer = procedure(object_: pointer); cdecl;
  PFT_Generic = ^TFT_Generic;

  TFT_Generic = record
    Data: pointer;
    finalizer: TFT_Generic_Finalizer;
  end;

  PFT_Bitmap = ^TFT_Bitmap;

  TFT_Bitmap = record
    rows: dword;
    Width: dword;
    pitch: longint;
    buffer: pbyte;
    num_grays: word;
    pixel_mode: byte;
    palette_mode: byte;
    palette: pointer;
  end;

  PFT_Size_Metrics = ^TFT_Size_Metrics;

  TFT_Size_Metrics = record
    x_ppem: TFT_UShort;
    y_ppem: TFT_UShort;
    x_scale: TFT_Fixed;
    y_scale: TFT_Fixed;
    ascender: TFT_Pos;
    descender: TFT_Pos;
    Height: TFT_Pos;
    max_advance: TFT_Pos;
  end;

  PFT_Size_Internal = ^TFT_Size_Internal;
  TFT_Size_Internal = Pointer; // PFT_Size_InternalRec_;


  PFT_SizeRec = ^TFT_SizeRec;

  TFT_SizeRec = record
    face: ^TFT_FaceRec;
    generic_: TFT_Generic;
    metrics: TFT_Size_Metrics;
    internal: TFT_Size_Internal;
  end;

  PFT_Size = ^TFT_Size;
  TFT_Size = PFT_SizeRec;


  PFT_Bitmap_Size = ^TFT_Bitmap_Size;

  TFT_Bitmap_Size = record
    Height: TFT_Short;
    Width: TFT_Short;
    size: TFT_Pos;
    x_ppem: TFT_Pos;
    y_ppem: TFT_Pos;
  end;

  PFT_Outline = ^TFT_Outline;

  TFT_Outline = record
    n_contours: smallint;
    n_points: smallint;
    points: PFT_Vector;
    tags: PChar;
    contours: Psmallint;
    flags: longint;
  end;

  PFT_SubGlyph = ^TFT_SubGlyph;
  TFT_SubGlyph = Pointer;

  PFT_Slot_Internal = ^TFT_Slot_Internal;
  TFT_Slot_Internal = Pointer; // PFT_Slot_InternalRec_


  PFT_GlyphSlotRec = ^TFT_GlyphSlotRec;

  TFT_GlyphSlotRec = record
    library_: TFT_Library;
    face: ^TFT_Face;
    Next: ^TFT_GlyphSlotRec;
    glyph_index: TFT_UInt;
    generic_: TFT_Generic;
    metrics: TFT_Glyph_Metrics;
    linearHoriAdvance: TFT_Fixed;
    linearVertAdvance: TFT_Fixed;
    advance: TFT_Vector;
    format: TFT_Glyph_Format;
    bitmap: TFT_Bitmap;
    bitmap_left: TFT_Int;
    bitmap_top: TFT_Int;
    outline: TFT_Outline;
    num_subglyphs: TFT_UInt;
    subglyphs: TFT_SubGlyph;
    control_data: pointer;
    control_len: longint;
    lsb_delta: TFT_Pos;
    rsb_delta: TFT_Pos;
    other: pointer;
    internal: TFT_Slot_Internal;
  end;


  PFT_GlyphSlot = ^TFT_GlyphSlot;
  TFT_GlyphSlot = PFT_GlyphSlotRec;


  TFT_CharMapRec = record
    face: ^TFT_FaceRec;
    encoding: TFT_Encoding;
    platform_id: TFT_UShort;
    encoding_id: TFT_UShort;
  end;
  PFT_CharMapRec = ^TFT_CharMapRec;

  PFT_CharMap = ^TFT_CharMap;
  TFT_CharMap = ^TFT_CharMapRec;

  PFT_Driver = ^TFT_Driver;
  TFT_Driver = Pointer; // PFT_DriverRec_;

  PFT_Memory = ^TFT_Memory;
  TFT_Memory = ^TFT_MemoryRec;

  PFT_Alloc_Func = ^TFT_Alloc_Func;
  TFT_Alloc_Func = function(memory: TFT_Memory; size: longint): pointer; cdecl;
  TFT_Free_Func = procedure(memory: TFT_Memory; block: pointer); cdecl;
  PFT_Realloc_Func = ^TFT_Realloc_Func;
  TFT_Realloc_Func = function(memory: TFT_Memory; cur_size: longint; new_size: longint; block: pointer): pointer; cdecl;
  PFT_MemoryRec = ^TFT_MemoryRec;

  TFT_MemoryRec = record
    user: pointer;//cdecl;
    alloc: TFT_Alloc_Func;
    Free: TFT_Free_Func;
    realloc: TFT_Realloc_Func;
  end;


  PFT_Stream = ^TFT_Stream;
  TFT_Stream = ^TFT_StreamRec;
  TFT_Stream_CloseFunc = procedure(stream: TFT_Stream); cdecl;
  TFT_Stream_IoFunc = function(stream: TFT_Stream; offset: dword; buffer: pbyte; Count: dword): dword; cdecl;

  PFT_StreamDesc = ^TFT_StreamDesc;
  TFT_StreamDesc = record
    case longint of
      0: (Value: longint);
      1: (pointer_: pointer);
  end;


  PFT_StreamRec = ^TFT_StreamRec;

  TFT_StreamRec = record
    base: pbyte;
    size: dword;
    pos: dword;
    descriptor: TFT_StreamDesc;
    pathname: TFT_StreamDesc;
    Read: TFT_Stream_IoFunc;
    Close: TFT_Stream_CloseFunc;
    memory: TFT_Memory;
    cursor: pbyte;
    limit: pbyte;
  end;

  PFT_ListNode = ^TFT_ListNode;
  TFT_ListNode = Pointer; // PFT_ListNodeRec_;

  PFT_ListRec = ^TFT_ListRec;

  TFT_ListRec = record
    head: TFT_ListNode;
    tail: TFT_ListNode;
  end;

  PFT_Face_Internal = ^TFT_Face_Internal;
  TFT_Face_Internal = Pointer; // PFT_Face_InternalRec_;

  PFT_FaceRec = ^TFT_FaceRec;

  TFT_FaceRec = record
    num_faces: TFT_Long;
    face_index: TFT_Long;
    face_flags: TFT_Long;
    style_flags: TFT_Long;
    num_glyphs: TFT_Long;
    family_name: PFT_String;
    style_name: PFT_String;
    num_fixed_sizes: TFT_Int;
    available_sizes: ^TFT_Bitmap_Size;
    num_charmaps: TFT_Int;
    charmaps: ^TFT_CharMap;
    generic_: TFT_Generic;
    bbox: TFT_BBox;
    units_per_EM: TFT_UShort;
    ascender: TFT_Short;
    descender: TFT_Short;
    Height: TFT_Short;
    max_advance_width: TFT_Short;
    max_advance_height: TFT_Short;
    underline_position: TFT_Short;
    underline_thickness: TFT_Short;
    glyph: TFT_GlyphSlot;
    size: TFT_Size;
    charmap: TFT_CharMap;
    driver: TFT_Driver;
    memory: TFT_Memory;
    stream: TFT_Stream;
    sizes_list: TFT_ListRec;
    autohint: TFT_Generic;
    extensions: pointer;
    internal: TFT_Face_Internal;
  end;

  PFT_Face = ^TFT_Face;
  TFT_Face = ^TFT_FaceRec;

implementation

end.

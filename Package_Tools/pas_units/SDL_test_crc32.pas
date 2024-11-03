unit SDL_test_crc32;

interface

uses
  ctypes, SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  TCrcUint32 = dword;
  PCrcUint32 = ^TCrcUint32;
  TCrcUint8 = pbyte;
  PCrcUint8 = ^TCrcUint8;

const
  CRC32_POLY = $04c11db7;

type
  PSDLTest_Crc32Context = ^TSDLTest_Crc32Context;

  TSDLTest_Crc32Context = record
    crc32_table: array[0..255] of TCrcUint32;
  end;

function SDLTest_Crc32Init(crcContext: PSDLTest_Crc32Context): Tbool; cdecl; external;
function SDLTest_Crc32Calc(crcContext: PSDLTest_Crc32Context; inBuf: PCrcUint8; inLen: TCrcUint32; crc32: PCrcUint32): Tbool; cdecl; external;
function SDLTest_Crc32CalcStart(crcContext: PSDLTest_Crc32Context; crc32: PCrcUint32): Tbool; cdecl; external;
function SDLTest_Crc32CalcEnd(crcContext: PSDLTest_Crc32Context; crc32: PCrcUint32): Tbool; cdecl; external;
function SDLTest_Crc32CalcBuffer(crcContext: PSDLTest_Crc32Context; inBuf: PCrcUint8; inLen: TCrcUint32; crc32: PCrcUint32): Tbool; cdecl; external;
function SDLTest_Crc32Done(crcContext: PSDLTest_Crc32Context): Tbool; cdecl; external;

implementation


end.

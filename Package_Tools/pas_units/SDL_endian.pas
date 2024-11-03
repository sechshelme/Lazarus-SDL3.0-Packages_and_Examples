unit SDL_endian;

interface

uses
  ctypes, SDL3;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

const
  SDL_LIL_ENDIAN = 1234;
  SDL_BIG_ENDIAN = 4321;
  SDL_BYTEORDER = SDL_LIL_ENDIAN;

function SDL_Swap16(x: uint16): uint16; inline;
function SDL_Swap32(x: uint32): uint32; inline;
function SDL_Swap64(x: uint64): uint64; inline;
function SDL_SwapFloat(x: cfloat): cfloat; inline;

function SDL_Swap16LE(x: uint16): uint16; inline;
function SDL_Swap32LE(x: uint32): uint32; inline;
function SDL_Swap64LE(x: uint64): uint64; inline;
function SDL_SwapFloatLE(x: cfloat): cfloat; inline;

function SDL_Swap16BE(x: uint16): uint16; inline;
function SDL_Swap32BE(x: uint32): uint32; inline;
function SDL_Swap64BE(x: uint64): uint64; inline;
function SDL_SwapFloatBE(x: cfloat): cfloat; inline;


implementation

function SDL_Swap16(x: uint16): uint16;
begin
  Result := Swap(x);
end;

function SDL_Swap32(x: uint32): uint32;
begin
  Result := Swap(x);
end;

function SDL_Swap64(x: uint64): uint64;
begin
  Result := Swap(x);
end;

function SDL_SwapFloat(x: cfloat): cfloat;
var
  swapper: record
    case integer of
      0: (f: single);
      1: (ui32: uint32);
      end;
begin
  swapper.f := x;
  swapper.ui32 := SDL_Swap32(swapper.ui32);
  Result := swapper.f;
end;

function SDL_Swap16LE(x: uint16): uint16;
begin
  Result := x;
end;

function SDL_Swap32LE(x: uint32): uint32;
begin
  Result := x;
end;

function SDL_Swap64LE(x: uint64): uint64;
begin
  Result := x;
end;

function SDL_SwapFloatLE(x: cfloat): cfloat;
begin
  Result := x;
end;

function SDL_Swap16BE(x: uint16): uint16;
begin
  Result := Swap(x);
end;

function SDL_Swap32BE(x: uint32): uint32;
begin
  Result := Swap(x);
end;

function SDL_Swap64BE(x: uint64): uint64;
begin
  Result := Swap(x);
end;

function SDL_SwapFloatBE(x: cfloat): cfloat;
begin
  Result := SDL_SwapFloat(x);
end;



end.

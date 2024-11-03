/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 03-11-2024   16:27                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

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


{$ENDIF read_interface}


{$IFDEF read_implementation}

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



{$ENDIF read_implementation}

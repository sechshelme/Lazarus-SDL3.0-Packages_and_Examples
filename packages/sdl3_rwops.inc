/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

const
  SDL_RWOPS_UNKNOWN = 0;
  SDL_RWOPS_WINFILE = 1;
  SDL_RWOPS_STDFILE = 2;
  SDL_RWOPS_JNIFILE = 3;
  SDL_RWOPS_MEMORY = 4;
  SDL_RWOPS_MEMORY_RO = 5;
  SDL_RWOPS_STATUS_READY = 0;
  SDL_RWOPS_STATUS_ERROR = 1;
  SDL_RWOPS_STATUS_EOF = 2;
  SDL_RWOPS_STATUS_NOT_READY = 3;
  SDL_RWOPS_STATUS_READONLY = 4;
  SDL_RWOPS_STATUS_WRITEONLY = 5;

type
  PSDL_RWops = ^TSDL_RWops;

  TSDL_RWops = record
    size: function(context: PSDL_RWops): int64; cdecl;
    seek: function(context: PSDL_RWops; offset: int64; whence: longint): int64; cdecl;
    Read: function(context: PSDL_RWops; ptr: pointer; size: Tsize_t): Tsize_t; cdecl;
    Write: function(context: PSDL_RWops; ptr: pointer; size: Tsize_t): Tsize_t; cdecl;
    Close: function(context: PSDL_RWops): longint; cdecl;
    _type: uint32;
    status: uint32;
    props: TSDL_PropertiesID;
    hidden: record
      case longint of
        2: (stdio: record
            autoclose: TSDL_bool;
            fp: pointer;
            end);
        3: (mem: record
            base: PUint8;
            here: PUint8;
            stop: PUint8;
            end);
        4: (unknown: record
            data1: pointer;
            data2: pointer;
            end);
      end;
  end;

function SDL_RWFromFile(file_: PChar; mode: PChar): PSDL_RWops; cdecl; external;
function SDL_RWFromMem(mem: pointer; size: Tsize_t): PSDL_RWops; cdecl; external;
function SDL_RWFromConstMem(mem: pointer; size: Tsize_t): PSDL_RWops; cdecl; external;
function SDL_CreateRW: PSDL_RWops; cdecl; external;
procedure SDL_DestroyRW(context: PSDL_RWops); cdecl; external;
function SDL_GetRWProperties(context: PSDL_RWops): TSDL_PropertiesID; cdecl; external;

const
  SDL_RW_SEEK_SET = 0;
  SDL_RW_SEEK_CUR = 1;
  SDL_RW_SEEK_END = 2;

function SDL_RWsize(context: PSDL_RWops): int64; cdecl; external;
function SDL_RWseek(context: PSDL_RWops; offset: int64; whence: longint): int64; cdecl; external;
function SDL_RWtell(context: PSDL_RWops): int64; cdecl; external;
function SDL_RWread(context: PSDL_RWops; ptr: pointer; size: Tsize_t): Tsize_t; cdecl; external;
function SDL_RWwrite(context: PSDL_RWops; ptr: pointer; size: Tsize_t): Tsize_t; cdecl; external;
function SDL_RWprintf(context: PSDL_RWops; fmt: PChar; args: array of const): Tsize_t; cdecl; external;
function SDL_RWprintf(context: PSDL_RWops; fmt: PChar): Tsize_t; cdecl; external;
function SDL_RWvprintf(context: PSDL_RWops; fmt: PChar): Tsize_t; varargs; cdecl; external;
function SDL_RWclose(context: PSDL_RWops): longint; cdecl; external;
function SDL_LoadFile_RW(src: PSDL_RWops; datasize: Psize_t; freesrc: TSDL_bool): pointer; cdecl; external;
function SDL_LoadFile(file_: PChar; datasize: Psize_t): pointer; cdecl; external;
function SDL_ReadU8(src: PSDL_RWops; Value: PUint8): TSDL_bool; cdecl; external;
function SDL_ReadU16LE(src: PSDL_RWops; Value: PUint16): TSDL_bool; cdecl; external;
function SDL_ReadS16LE(src: PSDL_RWops; Value: Pint16): TSDL_bool; cdecl; external;
function SDL_ReadU16BE(src: PSDL_RWops; Value: PUint16): TSDL_bool; cdecl; external;
function SDL_ReadS16BE(src: PSDL_RWops; Value: Pint16): TSDL_bool; cdecl; external;
function SDL_ReadU32LE(src: PSDL_RWops; Value: PUint32): TSDL_bool; cdecl; external;
function SDL_ReadS32LE(src: PSDL_RWops; Value: Pint32): TSDL_bool; cdecl; external;
function SDL_ReadU32BE(src: PSDL_RWops; Value: PUint32): TSDL_bool; cdecl; external;
function SDL_ReadS32BE(src: PSDL_RWops; Value: Pint32): TSDL_bool; cdecl; external;
function SDL_ReadU64LE(src: PSDL_RWops; Value: PUint64): TSDL_bool; cdecl; external;
function SDL_ReadS64LE(src: PSDL_RWops; Value: Pint64): TSDL_bool; cdecl; external;
function SDL_ReadU64BE(src: PSDL_RWops; Value: PUint64): TSDL_bool; cdecl; external;
function SDL_ReadS64BE(src: PSDL_RWops; Value: Pint64): TSDL_bool; cdecl; external;
function SDL_WriteU8(dst: PSDL_RWops; Value: uint8): TSDL_bool; cdecl; external;
function SDL_WriteU16LE(dst: PSDL_RWops; Value: uint16): TSDL_bool; cdecl; external;
function SDL_WriteS16LE(dst: PSDL_RWops; Value: int16): TSDL_bool; cdecl; external;
function SDL_WriteU16BE(dst: PSDL_RWops; Value: uint16): TSDL_bool; cdecl; external;
function SDL_WriteS16BE(dst: PSDL_RWops; Value: int16): TSDL_bool; cdecl; external;
function SDL_WriteU32LE(dst: PSDL_RWops; Value: uint32): TSDL_bool; cdecl; external;
function SDL_WriteS32LE(dst: PSDL_RWops; Value: int32): TSDL_bool; cdecl; external;
function SDL_WriteU32BE(dst: PSDL_RWops; Value: uint32): TSDL_bool; cdecl; external;
function SDL_WriteS32BE(dst: PSDL_RWops; Value: int32): TSDL_bool; cdecl; external;
function SDL_WriteU64LE(dst: PSDL_RWops; Value: uint64): TSDL_bool; cdecl; external;
function SDL_WriteS64LE(dst: PSDL_RWops; Value: int64): TSDL_bool; cdecl; external;
function SDL_WriteU64BE(dst: PSDL_RWops; Value: uint64): TSDL_bool; cdecl; external;
function SDL_WriteS64BE(dst: PSDL_RWops; Value: int64): TSDL_bool; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

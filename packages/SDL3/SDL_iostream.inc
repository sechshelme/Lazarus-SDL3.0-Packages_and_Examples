/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_IOStatus = ^TSDL_IOStatus;
  TSDL_IOStatus = longint;

const
  SDL_IO_STATUS_READY = 0;
  SDL_IO_STATUS_ERROR = 1;
  SDL_IO_STATUS_EOF = 2;
  SDL_IO_STATUS_NOT_READY = 3;
  SDL_IO_STATUS_READONLY = 4;
  SDL_IO_STATUS_WRITEONLY = 5;

type
  PSDL_IOWhence = ^TSDL_IOWhence;
  TSDL_IOWhence = longint;

const
  SDL_IO_SEEK_SET = 0;
  SDL_IO_SEEK_CUR = 1;
  SDL_IO_SEEK_END = 2;

type
  TSDL_IOStreamInterface = record
    version: TUint32;
    size: function(userdata: pointer): TSint64; cdecl;
    seek: function(userdata: pointer; offset: TSint64; whence: TSDL_IOWhence): TSint64; cdecl;
    Read: function(userdata: pointer; ptr: pointer; size: Tsize_t; status: PSDL_IOStatus): Tsize_t; cdecl;
    Write: function(userdata: pointer; ptr: pointer; size: Tsize_t; status: PSDL_IOStatus): Tsize_t; cdecl;
    flush: function(userdata: pointer; status: PSDL_IOStatus): Tbool; cdecl;
    Close: function(userdata: pointer): Tbool; cdecl;
  end;
  PSDL_IOStreamInterface = ^TSDL_IOStreamInterface;

  TSDL_IOStream = record
  end;
  PSDL_IOStream = ^TSDL_IOStream;

function SDL_IOFromFile(file_: pansichar; mode: pansichar): PSDL_IOStream; cdecl; external libSDL3;

const
  SDL_PROP_IOSTREAM_WINDOWS_HANDLE_POINTER = 'SDL.iostream.windows.handle';
  SDL_PROP_IOSTREAM_STDIO_FILE_POINTER = 'SDL.iostream.stdio.file';
  SDL_PROP_IOSTREAM_FILE_DESCRIPTOR_NUMBER = 'SDL.iostream.file_descriptor';
  SDL_PROP_IOSTREAM_ANDROID_AASSET_POINTER = 'SDL.iostream.android.aasset';

function SDL_IOFromMem(mem: pointer; size: Tsize_t): PSDL_IOStream; cdecl; external libSDL3;

const
  SDL_PROP_IOSTREAM_MEMORY_POINTER = 'SDL.iostream.memory.base';
  SDL_PROP_IOSTREAM_MEMORY_SIZE_NUMBER = 'SDL.iostream.memory.size';

function SDL_IOFromConstMem(mem: pointer; size: Tsize_t): PSDL_IOStream; cdecl; external libSDL3;
function SDL_IOFromDynamicMem: PSDL_IOStream; cdecl; external libSDL3;

const
  SDL_PROP_IOSTREAM_DYNAMIC_MEMORY_POINTER = 'SDL.iostream.dynamic.memory';
  SDL_PROP_IOSTREAM_DYNAMIC_CHUNKSIZE_NUMBER = 'SDL.iostream.dynamic.chunksize';

function SDL_OpenIO(iface: PSDL_IOStreamInterface; userdata: pointer): PSDL_IOStream; cdecl; external libSDL3;
function SDL_CloseIO(context: PSDL_IOStream): Tbool; cdecl; external libSDL3;
function SDL_GetIOProperties(context: PSDL_IOStream): TSDL_PropertiesID; cdecl; external libSDL3;
function SDL_GetIOStatus(context: PSDL_IOStream): TSDL_IOStatus; cdecl; external libSDL3;
function SDL_GetIOSize(context: PSDL_IOStream): TSint64; cdecl; external libSDL3;
function SDL_SeekIO(context: PSDL_IOStream; offset: TSint64; whence: TSDL_IOWhence): TSint64; cdecl; external libSDL3;
function SDL_TellIO(context: PSDL_IOStream): TSint64; cdecl; external libSDL3;
function SDL_ReadIO(context: PSDL_IOStream; ptr: pointer; size: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_WriteIO(context: PSDL_IOStream; ptr: pointer; size: Tsize_t): Tsize_t; cdecl; external libSDL3;
function SDL_IOprintf(context: PSDL_IOStream; fmt: pansichar; args: array of const): Tsize_t; cdecl; external libSDL3;
function SDL_IOprintf(context: PSDL_IOStream; fmt: pansichar): Tsize_t; cdecl; external libSDL3;
function SDL_IOvprintf(context: PSDL_IOStream; fmt: pansichar; ap: Tva_list): Tsize_t; cdecl; external libSDL3;
function SDL_FlushIO(context: PSDL_IOStream): Tbool; cdecl; external libSDL3;
function SDL_LoadFile_IO(src: PSDL_IOStream; datasize: Psize_t; closeio: Tbool): pointer; cdecl; external libSDL3;
function SDL_LoadFile(file_: pansichar; datasize: Psize_t): pointer; cdecl; external libSDL3;
function SDL_ReadU8(src: PSDL_IOStream; Value: PUint8): Tbool; cdecl; external libSDL3;
function SDL_ReadS8(src: PSDL_IOStream; Value: PSint8): Tbool; cdecl; external libSDL3;
function SDL_ReadU16LE(src: PSDL_IOStream; Value: PUint16): Tbool; cdecl; external libSDL3;
function SDL_ReadS16LE(src: PSDL_IOStream; Value: PSint16): Tbool; cdecl; external libSDL3;
function SDL_ReadU16BE(src: PSDL_IOStream; Value: PUint16): Tbool; cdecl; external libSDL3;
function SDL_ReadS16BE(src: PSDL_IOStream; Value: PSint16): Tbool; cdecl; external libSDL3;
function SDL_ReadU32LE(src: PSDL_IOStream; Value: PUint32): Tbool; cdecl; external libSDL3;
function SDL_ReadS32LE(src: PSDL_IOStream; Value: PSint32): Tbool; cdecl; external libSDL3;
function SDL_ReadU32BE(src: PSDL_IOStream; Value: PUint32): Tbool; cdecl; external libSDL3;
function SDL_ReadS32BE(src: PSDL_IOStream; Value: PSint32): Tbool; cdecl; external libSDL3;
function SDL_ReadU64LE(src: PSDL_IOStream; Value: PUint64): Tbool; cdecl; external libSDL3;
function SDL_ReadS64LE(src: PSDL_IOStream; Value: PSint64): Tbool; cdecl; external libSDL3;
function SDL_ReadU64BE(src: PSDL_IOStream; Value: PUint64): Tbool; cdecl; external libSDL3;
function SDL_ReadS64BE(src: PSDL_IOStream; Value: PSint64): Tbool; cdecl; external libSDL3;
function SDL_WriteU8(dst: PSDL_IOStream; Value: TUint8): Tbool; cdecl; external libSDL3;
function SDL_WriteS8(dst: PSDL_IOStream; Value: TSint8): Tbool; cdecl; external libSDL3;
function SDL_WriteU16LE(dst: PSDL_IOStream; Value: TUint16): Tbool; cdecl; external libSDL3;
function SDL_WriteS16LE(dst: PSDL_IOStream; Value: TSint16): Tbool; cdecl; external libSDL3;
function SDL_WriteU16BE(dst: PSDL_IOStream; Value: TUint16): Tbool; cdecl; external libSDL3;
function SDL_WriteS16BE(dst: PSDL_IOStream; Value: TSint16): Tbool; cdecl; external libSDL3;
function SDL_WriteU32LE(dst: PSDL_IOStream; Value: TUint32): Tbool; cdecl; external libSDL3;
function SDL_WriteS32LE(dst: PSDL_IOStream; Value: TSint32): Tbool; cdecl; external libSDL3;
function SDL_WriteU32BE(dst: PSDL_IOStream; Value: TUint32): Tbool; cdecl; external libSDL3;
function SDL_WriteS32BE(dst: PSDL_IOStream; Value: TSint32): Tbool; cdecl; external libSDL3;
function SDL_WriteU64LE(dst: PSDL_IOStream; Value: TUint64): Tbool; cdecl; external libSDL3;
function SDL_WriteS64LE(dst: PSDL_IOStream; Value: TSint64): Tbool; cdecl; external libSDL3;
function SDL_WriteU64BE(dst: PSDL_IOStream; Value: TUint64): Tbool; cdecl; external libSDL3;
function SDL_WriteS64BE(dst: PSDL_IOStream; Value: TSint64): Tbool; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}

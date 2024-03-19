/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_AudioFormat = ^TSDL_AudioFormat;
  TSDL_AudioFormat = Uint16;

const
  SDL_AUDIO_MASK_BITSIZE = $FF;  
  SDL_AUDIO_MASK_FLOAT = 1 shl 8;  
  SDL_AUDIO_MASK_BIG_ENDIAN = 1 shl 12;  
  SDL_AUDIO_MASK_SIGNED = 1 shl 15;  

function SDL_AUDIO_BITSIZE(x : uint32) : uint32;
function SDL_AUDIO_BYTESIZE(x : uint32) : uint32;
function SDL_AUDIO_ISFLOAT(x : uint32) : uint32;
function SDL_AUDIO_ISBIGENDIAN(x : uint32) : uint32;
function SDL_AUDIO_ISLITTLEENDIAN(x : uint32) : uint32;
function SDL_AUDIO_ISSIGNED(x : uint32) : uint32;
function SDL_AUDIO_ISINT(x : uint32) : uint32;
function SDL_AUDIO_ISUNSIGNED(x : uint32) : uint32;

const
  SDL_AUDIO_U8 = $0008;  
  SDL_AUDIO_S8 = $8008;
  SDL_AUDIO_S16LE = $8010;
  SDL_AUDIO_S16BE = $9010;
  SDL_AUDIO_S32LE = $8020;
  SDL_AUDIO_S32BE = $9020;
  SDL_AUDIO_F32LE = $8120;
  SDL_AUDIO_F32BE = $9120;

//  #if SDL_BYTEORDER == SDL_LIL_ENDIAN
const
  SDL_AUDIO_S16 = SDL_AUDIO_S16LE;  
  SDL_AUDIO_S32 = SDL_AUDIO_S32LE;  
  SDL_AUDIO_F32 = SDL_AUDIO_F32LE;  
//  #else
//const
//  SDL_AUDIO_S16 = SDL_AUDIO_S16BE;  
//  SDL_AUDIO_S32 = SDL_AUDIO_S32BE;  
//  SDL_AUDIO_F32 = SDL_AUDIO_F32BE;  
// #endif
type
  PSDL_AudioDeviceID = ^TSDL_AudioDeviceID;
  TSDL_AudioDeviceID = Uint32;

function SDL_AUDIO_DEVICE_DEFAULT_OUTPUT : TSDL_AudioDeviceID;
function SDL_AUDIO_DEVICE_DEFAULT_CAPTURE : TSDL_AudioDeviceID;
type
  PSDL_AudioSpec = ^TSDL_AudioSpec;
  TSDL_AudioSpec = record
      format : TSDL_AudioFormat;
      channels : longint;
      freq : longint;
    end;

function SDL_AUDIO_FRAMESIZE(x : TSDL_AudioSpec) : longint;
type
  PPSDL_AudioStream = ^PSDL_AudioStream;
  PSDL_AudioStream = ^TSDL_AudioStream;
  TSDL_AudioStream = Pointer;      {undefined structure}

function SDL_GetNumAudioDrivers:longint;cdecl;external;
function SDL_GetAudioDriver(index:longint):Pchar;cdecl;external;
function SDL_GetCurrentAudioDriver:Pchar;cdecl;external;
function SDL_GetAudioOutputDevices(count:Plongint):PSDL_AudioDeviceID;cdecl;external;
function SDL_GetAudioCaptureDevices(count:Plongint):PSDL_AudioDeviceID;cdecl;external;
function SDL_GetAudioDeviceName(devid:TSDL_AudioDeviceID):Pchar;cdecl;external;
function SDL_GetAudioDeviceFormat(devid:TSDL_AudioDeviceID; spec:PSDL_AudioSpec; sample_frames:Plongint):longint;cdecl;external;
function SDL_OpenAudioDevice(devid:TSDL_AudioDeviceID; spec:PSDL_AudioSpec):TSDL_AudioDeviceID;cdecl;external;
function SDL_PauseAudioDevice(dev:TSDL_AudioDeviceID):longint;cdecl;external;
function SDL_ResumeAudioDevice(dev:TSDL_AudioDeviceID):longint;cdecl;external;
function SDL_AudioDevicePaused(dev:TSDL_AudioDeviceID):TSDL_bool;cdecl;external;
procedure SDL_CloseAudioDevice(devid:TSDL_AudioDeviceID);cdecl;external;
function SDL_BindAudioStreams(devid: TSDL_AudioDeviceID; streams: PPSDL_AudioStream; num_streams: longint): longint; cdecl; external;
function SDL_BindAudioStream(devid:TSDL_AudioDeviceID; stream:PSDL_AudioStream):longint;cdecl;external;
procedure SDL_UnbindAudioStreams(streams:PPSDL_AudioStream; num_streams:longint);cdecl;external;
procedure SDL_UnbindAudioStream(stream:PSDL_AudioStream);cdecl;external;
function SDL_GetAudioStreamDevice(stream:PSDL_AudioStream):TSDL_AudioDeviceID;cdecl;external;
function SDL_CreateAudioStream(src_spec:PSDL_AudioSpec; dst_spec:PSDL_AudioSpec):PSDL_AudioStream;cdecl;external;
function SDL_GetAudioStreamProperties(stream:PSDL_AudioStream):TSDL_PropertiesID;cdecl;external;
function SDL_GetAudioStreamFormat(stream:PSDL_AudioStream; src_spec:PSDL_AudioSpec; dst_spec:PSDL_AudioSpec):longint;cdecl;external;
function SDL_SetAudioStreamFormat(stream:PSDL_AudioStream; src_spec:PSDL_AudioSpec; dst_spec:PSDL_AudioSpec):longint;cdecl;external;
function SDL_GetAudioStreamFrequencyRatio(stream:PSDL_AudioStream):single;cdecl;external;
function SDL_SetAudioStreamFrequencyRatio(stream:PSDL_AudioStream; ratio:single):longint;cdecl;external;
function SDL_PutAudioStreamData(stream:PSDL_AudioStream; buf:pointer; len:longint):longint;cdecl;external;
function SDL_GetAudioStreamData(stream:PSDL_AudioStream; buf:pointer; len:longint):longint;cdecl;external;
function SDL_GetAudioStreamAvailable(stream:PSDL_AudioStream):longint;cdecl;external;
function SDL_GetAudioStreamQueued(stream:PSDL_AudioStream):longint;cdecl;external;
function SDL_FlushAudioStream(stream:PSDL_AudioStream):longint;cdecl;external;
function SDL_ClearAudioStream(stream:PSDL_AudioStream):longint;cdecl;external;
function SDL_LockAudioStream(stream:PSDL_AudioStream):longint;cdecl;external;
function SDL_UnlockAudioStream(stream:PSDL_AudioStream):longint;cdecl;external;
type
  TSDL_AudioStreamCallback = procedure (userdata:pointer; stream:PSDL_AudioStream; additional_amount:longint; total_amount:longint);cdecl;

function SDL_SetAudioStreamGetCallback(stream:PSDL_AudioStream; callback:TSDL_AudioStreamCallback; userdata:pointer):longint;cdecl;external;
function SDL_SetAudioStreamPutCallback(stream:PSDL_AudioStream; callback:TSDL_AudioStreamCallback; userdata:pointer):longint;cdecl;external;
procedure SDL_DestroyAudioStream(stream:PSDL_AudioStream);cdecl;external;
function SDL_OpenAudioDeviceStream(devid:TSDL_AudioDeviceID; spec:PSDL_AudioSpec; callback:TSDL_AudioStreamCallback; userdata:pointer):PSDL_AudioStream;cdecl;external;
type
  TSDL_AudioPostmixCallback = procedure (userdata:pointer; spec:PSDL_AudioSpec; buffer:Psingle; buflen:longint);cdecl;
function SDL_SetAudioPostmixCallback(devid:TSDL_AudioDeviceID; callback:TSDL_AudioPostmixCallback; userdata:pointer):longint;cdecl;external;
function SDL_LoadWAV_RW(src:PSDL_RWops; freesrc:TSDL_bool; spec:PSDL_AudioSpec; audio_buf:PPUint8; audio_len:PUint32):longint;cdecl;external;
function SDL_LoadWAV(path:Pchar; spec:PSDL_AudioSpec; audio_buf:PPUint8; audio_len:PUint32):longint;cdecl;external;
const
  SDL_MIX_MAXVOLUME = 128;  
function SDL_MixAudioFormat(dst:PUint8; src:PUint8; format:TSDL_AudioFormat; len:Uint32; volume:longint):longint;cdecl;external;
function SDL_ConvertAudioSamples(src_spec:PSDL_AudioSpec; src_data:PUint8; src_len:longint; dst_spec:PSDL_AudioSpec; dst_data:PPUint8;           dst_len:Plongint):longint;cdecl;external;
function SDL_GetSilenceValueForFormat(format:TSDL_AudioFormat):longint;cdecl;external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_AUDIO_BITSIZE(x: uint32): uint32;
begin
  SDL_AUDIO_BITSIZE:=x and SDL_AUDIO_MASK_BITSIZE;
end;

function SDL_AUDIO_BYTESIZE(x : uint32) : uint32;
begin
  SDL_AUDIO_BYTESIZE:=(SDL_AUDIO_BITSIZE(x))div 8;
end;

function SDL_AUDIO_ISFLOAT(x: uint32): uint32;
begin
  SDL_AUDIO_ISFLOAT:=x and SDL_AUDIO_MASK_FLOAT;
end;

function SDL_AUDIO_ISBIGENDIAN(x: uint32): uint32;
begin
  SDL_AUDIO_ISBIGENDIAN:=x and SDL_AUDIO_MASK_BIG_ENDIAN;
end;

function SDL_AUDIO_ISLITTLEENDIAN(x : uint32) : uint32;
begin
  SDL_AUDIO_ISLITTLEENDIAN:= not (SDL_AUDIO_ISBIGENDIAN(x));
end;

function SDL_AUDIO_ISSIGNED(x: uint32): uint32;
begin
  SDL_AUDIO_ISSIGNED:=x and SDL_AUDIO_MASK_SIGNED;
end;

function SDL_AUDIO_ISINT(x : uint32) : uint32;
begin
  SDL_AUDIO_ISINT:= not (SDL_AUDIO_ISFLOAT(x));
end;

function SDL_AUDIO_ISUNSIGNED(x : uint32) : uint32;
begin
  SDL_AUDIO_ISUNSIGNED:= not (SDL_AUDIO_ISSIGNED(x));
end;

function SDL_AUDIO_DEVICE_DEFAULT_OUTPUT : TSDL_AudioDeviceID;
  begin
    SDL_AUDIO_DEVICE_DEFAULT_OUTPUT:=TSDL_AudioDeviceID($FFFFFFFF);
  end;

function SDL_AUDIO_DEVICE_DEFAULT_CAPTURE : TSDL_AudioDeviceID;
  begin
    SDL_AUDIO_DEVICE_DEFAULT_CAPTURE:=TSDL_AudioDeviceID($FFFFFFFE);
  end;

function SDL_AUDIO_FRAMESIZE(x : TSDL_AudioSpec) : longint;
begin
  SDL_AUDIO_FRAMESIZE:=(SDL_AUDIO_BYTESIZE(x.format))*(x.channels);
end;


{$ENDIF read_implementation}

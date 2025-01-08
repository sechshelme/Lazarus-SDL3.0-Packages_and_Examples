
{$IFDEF read_interface}


const
  SDL_AUDIO_MASK_BITSIZE = $FF;
  SDL_AUDIO_MASK_FLOAT = 1 shl 8;
  SDL_AUDIO_MASK_BIG_ENDIAN = 1 shl 12;
  SDL_AUDIO_MASK_SIGNED = 1 shl 15;

type
  PSDL_AudioFormat = ^TSDL_AudioFormat;
  TSDL_AudioFormat = longint;

const
  SDL_AUDIO_UNKNOWN = $0000;
  SDL_AUDIO_U8 = $0008;
  SDL_AUDIO_S8 = $8008;
  SDL_AUDIO_S16LE = $8010;
  SDL_AUDIO_S16BE = $9010;
  SDL_AUDIO_S32LE = $8020;
  SDL_AUDIO_S32BE = $9020;
  SDL_AUDIO_F32LE = $8120;
  SDL_AUDIO_F32BE = $9120;
  {$ifdef endian_little}
  SDL_AUDIO_S16 = SDL_AUDIO_S16LE;
  SDL_AUDIO_S32 = SDL_AUDIO_S32LE;
  SDL_AUDIO_F32 = SDL_AUDIO_F32LE;
  {$else}
  SDL_AUDIO_S16 = SDL_AUDIO_S16BE;
  SDL_AUDIO_S32 = SDL_AUDIO_S32BE;
  SDL_AUDIO_F32 = SDL_AUDIO_F32BE;
  {$endif}

type
  TSDL_AudioDeviceID = TUint32;
  PSDL_AudioDeviceID = ^TSDL_AudioDeviceID;

type
  TSDL_AudioSpec = record
    format: TSDL_AudioFormat;
    channels: longint;
    freq: longint;
  end;
  PSDL_AudioSpec = ^TSDL_AudioSpec;

  TSDL_AudioStream = record
  end;
  PSDL_AudioStream = ^TSDL_AudioStream;
  PPSDL_AudioStream = ^PSDL_AudioStream;

function SDL_GetNumAudioDrivers: longint; cdecl; external libSDL3;
function SDL_GetAudioDriver(index: longint): pansichar; cdecl; external libSDL3;
function SDL_GetCurrentAudioDriver: pansichar; cdecl; external libSDL3;
function SDL_GetAudioPlaybackDevices(Count: Plongint): PSDL_AudioDeviceID; cdecl; external libSDL3;
function SDL_GetAudioRecordingDevices(Count: Plongint): PSDL_AudioDeviceID; cdecl; external libSDL3;
function SDL_GetAudioDeviceName(devid: TSDL_AudioDeviceID): pansichar; cdecl; external libSDL3;
function SDL_GetAudioDeviceFormat(devid: TSDL_AudioDeviceID; spec: PSDL_AudioSpec; sample_frames: Plongint): Tbool; cdecl; external libSDL3;
function SDL_GetAudioDeviceChannelMap(devid: TSDL_AudioDeviceID; Count: Plongint): Plongint; cdecl; external libSDL3;
function SDL_OpenAudioDevice(devid: TSDL_AudioDeviceID; spec: PSDL_AudioSpec): TSDL_AudioDeviceID; cdecl; external libSDL3;
function SDL_IsAudioDevicePhysical(devid:TSDL_AudioDeviceID):Tbool;cdecl;external libSDL3;
function SDL_IsAudioDevicePlayback(devid:TSDL_AudioDeviceID):Tbool;cdecl;external libSDL3;
function SDL_PauseAudioDevice(dev: TSDL_AudioDeviceID): Tbool; cdecl; external libSDL3;
function SDL_ResumeAudioDevice(dev: TSDL_AudioDeviceID): Tbool; cdecl; external libSDL3;
function SDL_AudioDevicePaused(dev: TSDL_AudioDeviceID): Tbool; cdecl; external libSDL3;
function SDL_GetAudioDeviceGain(devid: TSDL_AudioDeviceID): single; cdecl; external libSDL3;
function SDL_SetAudioDeviceGain(devid: TSDL_AudioDeviceID; gain: single): Tbool; cdecl; external libSDL3;
procedure SDL_CloseAudioDevice(devid: TSDL_AudioDeviceID); cdecl; external libSDL3;
function SDL_BindAudioStreams(devid: TSDL_AudioDeviceID; streams: PPSDL_AudioStream; num_streams: longint): Tbool; cdecl; external libSDL3;
function SDL_BindAudioStream(devid: TSDL_AudioDeviceID; stream: PSDL_AudioStream): Tbool; cdecl; external libSDL3;
procedure SDL_UnbindAudioStreams(streams: PPSDL_AudioStream; num_streams: longint); cdecl; external libSDL3;
procedure SDL_UnbindAudioStream(stream: PSDL_AudioStream); cdecl; external libSDL3;
function SDL_GetAudioStreamDevice(stream: PSDL_AudioStream): TSDL_AudioDeviceID; cdecl; external libSDL3;
function SDL_CreateAudioStream(src_spec: PSDL_AudioSpec; dst_spec: PSDL_AudioSpec): PSDL_AudioStream; cdecl; external libSDL3;
function SDL_GetAudioStreamProperties(stream: PSDL_AudioStream): TSDL_PropertiesID; cdecl; external libSDL3;
function SDL_GetAudioStreamFormat(stream: PSDL_AudioStream; src_spec: PSDL_AudioSpec; dst_spec: PSDL_AudioSpec): Tbool; cdecl; external libSDL3;
function SDL_SetAudioStreamFormat(stream: PSDL_AudioStream; src_spec: PSDL_AudioSpec; dst_spec: PSDL_AudioSpec): Tbool; cdecl; external libSDL3;
function SDL_GetAudioStreamFrequencyRatio(stream: PSDL_AudioStream): single; cdecl; external libSDL3;
function SDL_SetAudioStreamFrequencyRatio(stream: PSDL_AudioStream; ratio: single): Tbool; cdecl; external libSDL3;
function SDL_GetAudioStreamGain(stream: PSDL_AudioStream): single; cdecl; external libSDL3;
function SDL_SetAudioStreamGain(stream: PSDL_AudioStream; gain: single): Tbool; cdecl; external libSDL3;
function SDL_GetAudioStreamInputChannelMap(stream: PSDL_AudioStream; Count: Plongint): Plongint; cdecl; external libSDL3;
function SDL_GetAudioStreamOutputChannelMap(stream: PSDL_AudioStream; Count: Plongint): Plongint; cdecl; external libSDL3;
function SDL_SetAudioStreamInputChannelMap(stream: PSDL_AudioStream; chmap: Plongint; Count: longint): Tbool; cdecl; external libSDL3;
function SDL_SetAudioStreamOutputChannelMap(stream: PSDL_AudioStream; chmap: Plongint; Count: longint): Tbool; cdecl; external libSDL3;
function SDL_PutAudioStreamData(stream: PSDL_AudioStream; buf: pointer; len: longint): Tbool; cdecl; external libSDL3;
function SDL_GetAudioStreamData(stream: PSDL_AudioStream; buf: pointer; len: longint): longint; cdecl; external libSDL3;
function SDL_GetAudioStreamAvailable(stream: PSDL_AudioStream): longint; cdecl; external libSDL3;
function SDL_GetAudioStreamQueued(stream: PSDL_AudioStream): longint; cdecl; external libSDL3;
function SDL_FlushAudioStream(stream: PSDL_AudioStream): Tbool; cdecl; external libSDL3;
function SDL_ClearAudioStream(stream: PSDL_AudioStream): Tbool; cdecl; external libSDL3;
function SDL_PauseAudioStreamDevice(stream: PSDL_AudioStream): Tbool; cdecl; external libSDL3;
function SDL_ResumeAudioStreamDevice(stream: PSDL_AudioStream): Tbool; cdecl; external libSDL3;
function SDL_LockAudioStream(stream: PSDL_AudioStream): Tbool; cdecl; external libSDL3;
function SDL_UnlockAudioStream(stream: PSDL_AudioStream): Tbool; cdecl; external libSDL3;

type
  TSDL_AudioStreamCallback = procedure(userdata: pointer; stream: PSDL_AudioStream; additional_amount: longint; total_amount: longint); cdecl;

function SDL_SetAudioStreamGetCallback(stream: PSDL_AudioStream; callback: TSDL_AudioStreamCallback; userdata: pointer): Tbool; cdecl; external libSDL3;
function SDL_SetAudioStreamPutCallback(stream: PSDL_AudioStream; callback: TSDL_AudioStreamCallback; userdata: pointer): Tbool; cdecl; external libSDL3;
procedure SDL_DestroyAudioStream(stream: PSDL_AudioStream); cdecl; external libSDL3;
function SDL_OpenAudioDeviceStream(devid: TSDL_AudioDeviceID; spec: PSDL_AudioSpec; callback: TSDL_AudioStreamCallback; userdata: pointer): PSDL_AudioStream; cdecl; external libSDL3;

type
  TSDL_AudioPostmixCallback = procedure(userdata: pointer; spec: PSDL_AudioSpec; buffer: Psingle; buflen: longint); cdecl;

function SDL_SetAudioPostmixCallback(devid: TSDL_AudioDeviceID; callback: TSDL_AudioPostmixCallback; userdata: pointer): Tbool; cdecl; external libSDL3;
function SDL_LoadWAV_IO(src: PSDL_IOStream; closeio: Tbool; spec: PSDL_AudioSpec; audio_buf: PPUint8; audio_len: PUint32): Tbool; cdecl; external libSDL3;
function SDL_LoadWAV(path: pansichar; spec: PSDL_AudioSpec; audio_buf: PPUint8; audio_len: PUint32): Tbool; cdecl; external libSDL3;
function SDL_MixAudio(dst: PUint8; src: PUint8; format: TSDL_AudioFormat; len: TUint32; volume: single): Tbool; cdecl; external libSDL3;
function SDL_ConvertAudioSamples(src_spec: PSDL_AudioSpec; src_data: PUint8; src_len: longint; dst_spec: PSDL_AudioSpec; dst_data: PPUint8;
  dst_len: Plongint): Tbool; cdecl; external libSDL3;
function SDL_GetAudioFormatName(format: TSDL_AudioFormat): pansichar; cdecl; external libSDL3;
function SDL_GetSilenceValueForFormat(format: TSDL_AudioFormat): longint; cdecl; external libSDL3;


const
  SDL_AUDIO_DEVICE_DEFAULT_PLAYBACK = TSDL_AudioDeviceID($FFFFFFFF);
  SDL_AUDIO_DEVICE_DEFAULT_RECORDING = TSDL_AudioDeviceID($FFFFFFFE);

function SDL_AUDIO_BITSIZE(x: longint): longint;
function SDL_AUDIO_BYTESIZE(x: longint): longint;
function SDL_AUDIO_ISFLOAT(x: longint): boolean;
function SDL_AUDIO_ISBIGENDIAN(x: longint): boolean;
function SDL_AUDIO_ISLITTLEENDIAN(x: longint): boolean;
function SDL_AUDIO_ISSIGNED(x: longint): boolean;
function SDL_AUDIO_ISINT(x: longint): boolean;
function SDL_AUDIO_ISUNSIGNED(x: longint): boolean;
function SDL_AUDIO_FRAMESIZE(x: TSDL_AudioSpec): longint;

function SDL_DEFINE_AUDIO_FORMAT(signed, bigendian, flt, size: UInt16): UInt32;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_AUDIO_BITSIZE(x: longint): longint;
begin
  SDL_AUDIO_BITSIZE := x and SDL_AUDIO_MASK_BITSIZE;
end;

function SDL_AUDIO_BYTESIZE(x: longint): longint;
begin
  SDL_AUDIO_BYTESIZE := SDL_AUDIO_BITSIZE(x) div 8;
end;

function SDL_AUDIO_ISFLOAT(x: longint): boolean;
begin
  SDL_AUDIO_ISFLOAT := x and SDL_AUDIO_MASK_FLOAT <> 0;
end;

function SDL_AUDIO_ISBIGENDIAN(x: longint): boolean;
begin
  SDL_AUDIO_ISBIGENDIAN := x and SDL_AUDIO_MASK_BIG_ENDIAN <> 0;
end;

function SDL_AUDIO_ISLITTLEENDIAN(x: longint): boolean;
begin
  SDL_AUDIO_ISLITTLEENDIAN := not (SDL_AUDIO_ISBIGENDIAN(x));
end;

function SDL_AUDIO_ISSIGNED(x: longint): boolean;
begin
  SDL_AUDIO_ISSIGNED := x and SDL_AUDIO_MASK_SIGNED <> 0;
end;

function SDL_AUDIO_ISINT(x: longint): boolean;
begin
  SDL_AUDIO_ISINT := not SDL_AUDIO_ISFLOAT(x);
end;

function SDL_AUDIO_ISUNSIGNED(x: longint): boolean;
begin
  SDL_AUDIO_ISUNSIGNED := not SDL_AUDIO_ISSIGNED(x);
end;

function SDL_AUDIO_FRAMESIZE(x: TSDL_AudioSpec): longint;
begin
  SDL_AUDIO_FRAMESIZE := (SDL_AUDIO_BYTESIZE(x.format)) * (x.channels);
end;

function SDL_DEFINE_AUDIO_FORMAT(signed, bigendian, flt, size: UInt16): UInt32;
begin
  Result := (signed shl 15) or (bigendian shl 12) or (flt shl 8) or (size and SDL_AUDIO_MASK_BITSIZE);
end;



{$ENDIF read_implementation}


{$IFDEF read_interface}

type
  TSDL_CameraID = TUint32;
  PSDL_CameraID = ^TSDL_CameraID;

  TSDL_CameraSpec = record
    format: TSDL_PixelFormat;
    colorspace: TSDL_Colorspace;
    Width: longint;
    Height: longint;
    framerate_numerator: longint;
    framerate_denominator: longint;
  end;
  PSDL_CameraSpec = ^TSDL_CameraSpec;
  PPSDL_CameraSpec = ^PSDL_CameraSpec;

  PSDL_CameraPosition = ^TSDL_CameraPosition;
  TSDL_CameraPosition = longint;

const
  SDL_CAMERA_POSITION_UNKNOWN = 0;
  SDL_CAMERA_POSITION_FRONT_FACING = 1;
  SDL_CAMERA_POSITION_BACK_FACING = 2;

type
  TSDL_Camera = record
  end;
  PSDL_Camera = ^TSDL_Camera;

function SDL_GetNumCameraDrivers: longint; cdecl; external libSDL3;
function SDL_GetCameraDriver(index: longint): pansichar; cdecl; external libSDL3;
function SDL_GetCurrentCameraDriver: pansichar; cdecl; external libSDL3;
function SDL_GetCameras(Count: Plongint): PSDL_CameraID; cdecl; external libSDL3;
function SDL_GetCameraSupportedFormats(devid: TSDL_CameraID; Count: Plongint): PPSDL_CameraSpec; cdecl; external libSDL3;
function SDL_GetCameraName(instance_id: TSDL_CameraID): pansichar; cdecl; external libSDL3;
function SDL_GetCameraPosition(instance_id: TSDL_CameraID): TSDL_CameraPosition; cdecl; external libSDL3;
function SDL_OpenCamera(instance_id: TSDL_CameraID; spec: PSDL_CameraSpec): PSDL_Camera; cdecl; external libSDL3;
function SDL_GetCameraPermissionState(camera: PSDL_Camera): longint; cdecl; external libSDL3;
function SDL_GetCameraID(camera: PSDL_Camera): TSDL_CameraID; cdecl; external libSDL3;
function SDL_GetCameraProperties(camera: PSDL_Camera): TSDL_PropertiesID; cdecl; external libSDL3;
function SDL_GetCameraFormat(camera: PSDL_Camera; spec: PSDL_CameraSpec): Tbool; cdecl; external libSDL3;
function SDL_AcquireCameraFrame(camera: PSDL_Camera; timestampNS: PUint64): PSDL_Surface; cdecl; external libSDL3;
procedure SDL_ReleaseCameraFrame(camera: PSDL_Camera; frame: PSDL_Surface); cdecl; external libSDL3;
procedure SDL_CloseCamera(camera: PSDL_Camera); cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}

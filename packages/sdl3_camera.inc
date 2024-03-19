/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_CameraDeviceID = ^TSDL_CameraDeviceID;
  TSDL_CameraDeviceID = Uint32;

  PSDL_Camera = ^TSDL_Camera;
  TSDL_Camera = Pointer;      {undefined structure}

  PSDL_CameraSpec = ^TSDL_CameraSpec;
  TSDL_CameraSpec = record
      format : TSDL_PixelFormatEnum;
      width : longint;
      height : longint;
      interval_numerator : longint;
      interval_denominator : longint;
    end;

  PSDL_CameraPosition = ^TSDL_CameraPosition;
  TSDL_CameraPosition =  Longint;
  Const
    SDL_CAMERA_POSITION_UNKNOWN = 0;
    SDL_CAMERA_POSITION_FRONT_FACING = 1;
    SDL_CAMERA_POSITION_BACK_FACING = 2;

function SDL_GetNumCameraDrivers:longint;cdecl;external;
function SDL_GetCameraDriver(index:longint):Pchar;cdecl;external;
function SDL_GetCurrentCameraDriver:Pchar;cdecl;external;
function SDL_GetCameraDevices(count:Plongint):PSDL_CameraDeviceID;cdecl;external;
function SDL_GetCameraDeviceSupportedFormats(devid:TSDL_CameraDeviceID; count:Plongint):PSDL_CameraSpec;cdecl;external;
function SDL_GetCameraDeviceName(instance_id:TSDL_CameraDeviceID):Pchar;cdecl;external;
function SDL_GetCameraDevicePosition(instance_id:TSDL_CameraDeviceID):TSDL_CameraPosition;cdecl;external;
function SDL_OpenCameraDevice(instance_id:TSDL_CameraDeviceID; spec:PSDL_CameraSpec):PSDL_Camera;cdecl;external;
function SDL_GetCameraPermissionState(camera:PSDL_Camera):longint;cdecl;external;
function SDL_GetCameraInstanceID(camera:PSDL_Camera):TSDL_CameraDeviceID;cdecl;external;
function SDL_GetCameraProperties(camera:PSDL_Camera):TSDL_PropertiesID;cdecl;external;
function SDL_GetCameraFormat(camera:PSDL_Camera; spec:PSDL_CameraSpec):longint;cdecl;external;
function SDL_AcquireCameraFrame(camera:PSDL_Camera; timestampNS:PUint64):PSDL_Surface;cdecl;external;
function SDL_ReleaseCameraFrame(camera:PSDL_Camera; frame:PSDL_Surface):longint;cdecl;external;
procedure SDL_CloseCamera(camera:PSDL_Camera);cdecl;external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

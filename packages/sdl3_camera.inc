/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 22-06-2024   20:03                        //
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
       colorspace:  TSDL_Colorspace;
      width : longint;
      height : longint;
      framerate_numerator : cint;
      framerate_denominator : cint;
    end;

  PSDL_CameraPosition = ^TSDL_CameraPosition;
  TSDL_CameraPosition =  Longint;
  Const
    SDL_CAMERA_POSITION_UNKNOWN = 0;
    SDL_CAMERA_POSITION_FRONT_FACING = 1;
    SDL_CAMERA_POSITION_BACK_FACING = 2;

function SDL_GetNumCameraDrivers:longint;cdecl;external sdl3_lib;
function SDL_GetCameraDriver(index:longint):Pchar;cdecl;external sdl3_lib;
function SDL_GetCurrentCameraDriver:Pchar;cdecl;external sdl3_lib;
function SDL_GetCameraDevices(count:Plongint):PSDL_CameraDeviceID;cdecl;external sdl3_lib;
function SDL_GetCameraDeviceSupportedFormats(devid:TSDL_CameraDeviceID; count:Plongint):PSDL_CameraSpec;cdecl;external sdl3_lib;
function SDL_GetCameraDeviceName(instance_id:TSDL_CameraDeviceID):Pchar;cdecl;external sdl3_lib;
function SDL_GetCameraDevicePosition(instance_id:TSDL_CameraDeviceID):TSDL_CameraPosition;cdecl;external sdl3_lib;
function SDL_OpenCameraDevice(instance_id:TSDL_CameraDeviceID; spec:PSDL_CameraSpec):PSDL_Camera;cdecl;external sdl3_lib;
function SDL_GetCameraPermissionState(camera:PSDL_Camera):longint;cdecl;external sdl3_lib;
function SDL_GetCameraInstanceID(camera:PSDL_Camera):TSDL_CameraDeviceID;cdecl;external sdl3_lib;
function SDL_GetCameraProperties(camera:PSDL_Camera):TSDL_PropertiesID;cdecl;external sdl3_lib;
function SDL_GetCameraFormat(camera:PSDL_Camera; spec:PSDL_CameraSpec):longint;cdecl;external sdl3_lib;
function SDL_AcquireCameraFrame(camera:PSDL_Camera; timestampNS:PUint64):PSDL_Surface;cdecl;external sdl3_lib;
function SDL_ReleaseCameraFrame(camera:PSDL_Camera; frame:PSDL_Surface):longint;cdecl;external sdl3_lib;
procedure SDL_CloseCamera(camera:PSDL_Camera);cdecl;external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

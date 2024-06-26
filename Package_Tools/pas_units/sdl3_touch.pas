unit SDL3_touch;

interface

uses
  ctypes;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_TouchID = ^TSDL_TouchID;
  TSDL_TouchID = uint64;

  PSDL_FingerID = ^TSDL_FingerID;
  TSDL_FingerID = uint64;

  PSDL_TouchDeviceType = ^TSDL_TouchDeviceType;
  TSDL_TouchDeviceType = longint;

const
  SDL_TOUCH_DEVICE_INVALID = -(1);
  SDL_TOUCH_DEVICE_DIRECT = (-(1)) + 1;
  SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE = (-(1)) + 2;
  SDL_TOUCH_DEVICE_INDIRECT_RELATIVE = (-(1)) + 3;

type
  PSDL_Finger = ^TSDL_Finger;

  TSDL_Finger = record
    id: TSDL_FingerID;
    x: cfloat;
    y: cfloat;
    pressure: cfloat;
  end;

const
  SDL_TOUCH_MOUSEID = uint32(-1);
  SDL_MOUSE_TOUCHID = uint64(-1);

function SDL_GetTouchDevices(Count: Plongint): PSDL_TouchID; cdecl; external sdl3_lib;
function SDL_GetTouchDeviceName(touchID: TSDL_TouchID): PChar; cdecl; external sdl3_lib;
function SDL_GetTouchDeviceType(touchID: TSDL_TouchID): TSDL_TouchDeviceType; cdecl; external sdl3_lib;
function SDL_GetNumTouchFingers(touchID: TSDL_TouchID): longint; cdecl; external sdl3_lib;
function SDL_GetTouchFinger(touchID: TSDL_TouchID; index: longint): PSDL_Finger; cdecl; external sdl3_lib;

implementation

end.

/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

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

function SDL_GetTouchDevices(Count: Plongint): PSDL_TouchID; cdecl; external;
function SDL_GetTouchDeviceName(touchID: TSDL_TouchID): PChar; cdecl; external;
function SDL_GetTouchDeviceType(touchID: TSDL_TouchID): TSDL_TouchDeviceType; cdecl; external;
function SDL_GetNumTouchFingers(touchID: TSDL_TouchID): longint; cdecl; external;
function SDL_GetTouchFinger(touchID: TSDL_TouchID; index: longint): PSDL_Finger; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

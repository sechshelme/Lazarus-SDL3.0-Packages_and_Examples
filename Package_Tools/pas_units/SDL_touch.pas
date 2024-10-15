unit SDL_touch;

interface

uses
  ctypes, SDL_stdinc, SDL_mouse;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_TouchID = ^TSDL_TouchID;
  TSDL_TouchID = TUint64;

  PSDL_FingerID = ^TSDL_FingerID;
  TSDL_FingerID = TUint64;

  PSDL_TouchDeviceType = ^TSDL_TouchDeviceType;
  TSDL_TouchDeviceType = longint;

const
  SDL_TOUCH_DEVICE_INVALID = -(1);
  SDL_TOUCH_DEVICE_DIRECT = (-(1)) + 1;
  SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE = (-(1)) + 2;
  SDL_TOUCH_DEVICE_INDIRECT_RELATIVE = (-(1)) + 3;

type
  TSDL_Finger = record
    id: TSDL_FingerID;
    x: single;
    y: single;
    pressure: single;
  end;
  PSDL_Finger = ^TSDL_Finger;
  PPSDL_Finger = ^PSDL_Finger;

const
  SDL_TOUCH_MOUSEID = TSDL_MouseID(-(1));
  SDL_MOUSE_TOUCHID = TSDL_TouchID(-(1));

function SDL_GetTouchDevices(Count: Plongint): PSDL_TouchID; cdecl; external libSDL3;
function SDL_GetTouchDeviceName(touchID: TSDL_TouchID): pansichar; cdecl; external libSDL3;
function SDL_GetTouchDeviceType(touchID: TSDL_TouchID): TSDL_TouchDeviceType; cdecl; external libSDL3;
function SDL_GetTouchFingers(touchID: TSDL_TouchID; Count: Plongint): PPSDL_Finger; cdecl; external libSDL3;

implementation



end.

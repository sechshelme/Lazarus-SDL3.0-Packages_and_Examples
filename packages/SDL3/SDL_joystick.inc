/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

var
  SDL_joystick_lock: PSDL_Mutex; cvar;external libSDL3;

type
  PSDL_JoystickID = ^TSDL_JoystickID;
  TSDL_JoystickID = TUint32;

  PSDL_JoystickType = ^TSDL_JoystickType;
  TSDL_JoystickType = longint;

const
  SDL_JOYSTICK_TYPE_UNKNOWN = 0;
  SDL_JOYSTICK_TYPE_GAMEPAD = 1;
  SDL_JOYSTICK_TYPE_WHEEL = 2;
  SDL_JOYSTICK_TYPE_ARCADE_STICK = 3;
  SDL_JOYSTICK_TYPE_FLIGHT_STICK = 4;
  SDL_JOYSTICK_TYPE_DANCE_PAD = 5;
  SDL_JOYSTICK_TYPE_GUITAR = 6;
  SDL_JOYSTICK_TYPE_DRUM_KIT = 7;
  SDL_JOYSTICK_TYPE_ARCADE_PAD = 8;
  SDL_JOYSTICK_TYPE_THROTTLE = 9;
  SDL_JOYSTICK_TYPE_COUNT = 10;

type
  PSDL_JoystickConnectionState = ^TSDL_JoystickConnectionState;
  TSDL_JoystickConnectionState = longint;

const
  SDL_JOYSTICK_CONNECTION_INVALID = -(1);
  SDL_JOYSTICK_CONNECTION_UNKNOWN = (-(1)) + 1;
  SDL_JOYSTICK_CONNECTION_WIRED = (-(1)) + 2;
  SDL_JOYSTICK_CONNECTION_WIRELESS = (-(1)) + 3;

const
  SDL_JOYSTICK_AXIS_MAX = 32767;
  SDL_JOYSTICK_AXIS_MIN = -(32768);

type
  TSDL_Joystick = record
  end;
  PSDL_Joystick = ^TSDL_Joystick;

procedure SDL_LockJoysticks; cdecl; external libSDL3;
procedure SDL_UnlockJoysticks; cdecl; external libSDL3;
function SDL_HasJoystick: Tbool; cdecl; external libSDL3;
function SDL_GetJoysticks(Count: Plongint): PSDL_JoystickID; cdecl; external libSDL3;
function SDL_GetJoystickNameForID(instance_id: TSDL_JoystickID): pansichar; cdecl; external libSDL3;
function SDL_GetJoystickPathForID(instance_id: TSDL_JoystickID): pansichar; cdecl; external libSDL3;
function SDL_GetJoystickPlayerIndexForID(instance_id: TSDL_JoystickID): longint; cdecl; external libSDL3;
function SDL_GetJoystickGUIDForID(instance_id: TSDL_JoystickID): TSDL_GUID; cdecl; external libSDL3;
function SDL_GetJoystickVendorForID(instance_id: TSDL_JoystickID): TUint16; cdecl; external libSDL3;
function SDL_GetJoystickProductForID(instance_id: TSDL_JoystickID): TUint16; cdecl; external libSDL3;
function SDL_GetJoystickProductVersionForID(instance_id: TSDL_JoystickID): TUint16; cdecl; external libSDL3;
function SDL_GetJoystickTypeForID(instance_id: TSDL_JoystickID): TSDL_JoystickType; cdecl; external libSDL3;
function SDL_OpenJoystick(instance_id: TSDL_JoystickID): PSDL_Joystick; cdecl; external libSDL3;
function SDL_GetJoystickFromID(instance_id: TSDL_JoystickID): PSDL_Joystick; cdecl; external libSDL3;
function SDL_GetJoystickFromPlayerIndex(player_index: longint): PSDL_Joystick; cdecl; external libSDL3;

type
  TSDL_VirtualJoystickTouchpadDesc = record
    nfingers: TUint16;
    padding: array[0..2] of TUint16;
  end;
  PSDL_VirtualJoystickTouchpadDesc = ^TSDL_VirtualJoystickTouchpadDesc;

  TSDL_VirtualJoystickSensorDesc = record
    _type: TSDL_SensorType;
    rate: single;
  end;
  PSDL_VirtualJoystickSensorDesc = ^TSDL_VirtualJoystickSensorDesc;

  TSDL_VirtualJoystickDesc = record
    version: TUint32;
    _type: TUint16;
    padding: TUint16;
    vendor_id: TUint16;
    product_id: TUint16;
    naxes: TUint16;
    nbuttons: TUint16;
    nballs: TUint16;
    nhats: TUint16;
    ntouchpads: TUint16;
    nsensors: TUint16;
    padding2: array[0..1] of TUint16;
    button_mask: TUint32;
    axis_mask: TUint32;
    Name: pansichar;
    touchpads: PSDL_VirtualJoystickTouchpadDesc;
    sensors: PSDL_VirtualJoystickSensorDesc;
    userdata: pointer;
    Update: procedure(userdata: pointer); cdecl;
    SetPlayerIndex: procedure(userdata: pointer; player_index: longint); cdecl;
    Rumble: function(userdata: pointer; low_frequency_rumble: TUint16; high_frequency_rumble: TUint16): Tbool; cdecl;
    RumbleTriggers: function(userdata: pointer; left_rumble: TUint16; right_rumble: TUint16): Tbool; cdecl;
    SetLED: function(userdata: pointer; red: TUint8; green: TUint8; blue: TUint8): Tbool; cdecl;
    SendEffect: function(userdata: pointer; Data: pointer; size: longint): Tbool; cdecl;
    SetSensorsEnabled: function(userdata: pointer; Enabled: Tbool): Tbool; cdecl;
    Cleanup: procedure(userdata: pointer); cdecl;
  end;
  PSDL_VirtualJoystickDesc = ^TSDL_VirtualJoystickDesc;

function SDL_AttachVirtualJoystick(desc: PSDL_VirtualJoystickDesc): TSDL_JoystickID; cdecl; external libSDL3;
function SDL_DetachVirtualJoystick(instance_id: TSDL_JoystickID): Tbool; cdecl; external libSDL3;
function SDL_IsJoystickVirtual(instance_id: TSDL_JoystickID): Tbool; cdecl; external libSDL3;
function SDL_SetJoystickVirtualAxis(joystick: PSDL_Joystick; axis: longint; Value: TSint16): Tbool; cdecl; external libSDL3;
function SDL_SetJoystickVirtualBall(joystick: PSDL_Joystick; ball: longint; xrel: TSint16; yrel: TSint16): Tbool; cdecl; external libSDL3;
function SDL_SetJoystickVirtualButton(joystick: PSDL_Joystick; button: longint; down: Tbool): Tbool; cdecl; external libSDL3;
function SDL_SetJoystickVirtualHat(joystick: PSDL_Joystick; hat: longint; Value: TUint8): Tbool; cdecl; external libSDL3;
function SDL_SetJoystickVirtualTouchpad(joystick: PSDL_Joystick; touchpad: longint; finger: longint; down: Tbool; x: single;
  y: single; pressure: single): Tbool; cdecl; external libSDL3;
function SDL_SendJoystickVirtualSensorData(joystick: PSDL_Joystick; _type: TSDL_SensorType; sensor_timestamp: TUint64; Data: Psingle; num_values: longint): Tbool; cdecl; external libSDL3;
function SDL_GetJoystickProperties(joystick: PSDL_Joystick): TSDL_PropertiesID; cdecl; external libSDL3;

const
  SDL_PROP_JOYSTICK_CAP_MONO_LED_BOOLEAN = 'SDL.joystick.cap.mono_led';
  SDL_PROP_JOYSTICK_CAP_RGB_LED_BOOLEAN = 'SDL.joystick.cap.rgb_led';
  SDL_PROP_JOYSTICK_CAP_PLAYER_LED_BOOLEAN = 'SDL.joystick.cap.player_led';
  SDL_PROP_JOYSTICK_CAP_RUMBLE_BOOLEAN = 'SDL.joystick.cap.rumble';
  SDL_PROP_JOYSTICK_CAP_TRIGGER_RUMBLE_BOOLEAN = 'SDL.joystick.cap.trigger_rumble';

function SDL_GetJoystickName(joystick: PSDL_Joystick): pansichar; cdecl; external libSDL3;
function SDL_GetJoystickPath(joystick: PSDL_Joystick): pansichar; cdecl; external libSDL3;
function SDL_GetJoystickPlayerIndex(joystick: PSDL_Joystick): longint; cdecl; external libSDL3;
function SDL_SetJoystickPlayerIndex(joystick: PSDL_Joystick; player_index: longint): Tbool; cdecl; external libSDL3;
function SDL_GetJoystickGUID(joystick: PSDL_Joystick): TSDL_GUID; cdecl; external libSDL3;
function SDL_GetJoystickVendor(joystick: PSDL_Joystick): TUint16; cdecl; external libSDL3;
function SDL_GetJoystickProduct(joystick: PSDL_Joystick): TUint16; cdecl; external libSDL3;
function SDL_GetJoystickProductVersion(joystick: PSDL_Joystick): TUint16; cdecl; external libSDL3;
function SDL_GetJoystickFirmwareVersion(joystick: PSDL_Joystick): TUint16; cdecl; external libSDL3;
function SDL_GetJoystickSerial(joystick: PSDL_Joystick): pansichar; cdecl; external libSDL3;
function SDL_GetJoystickType(joystick: PSDL_Joystick): TSDL_JoystickType; cdecl; external libSDL3;
procedure SDL_GetJoystickGUIDInfo(guid: TSDL_GUID; vendor: PUint16; product: PUint16; version: PUint16; crc16: PUint16); cdecl; external libSDL3;
function SDL_JoystickConnected(joystick: PSDL_Joystick): Tbool; cdecl; external libSDL3;
function SDL_GetJoystickID(joystick: PSDL_Joystick): TSDL_JoystickID; cdecl; external libSDL3;
function SDL_GetNumJoystickAxes(joystick: PSDL_Joystick): longint; cdecl; external libSDL3;
function SDL_GetNumJoystickBalls(joystick: PSDL_Joystick): longint; cdecl; external libSDL3;
function SDL_GetNumJoystickHats(joystick: PSDL_Joystick): longint; cdecl; external libSDL3;
function SDL_GetNumJoystickButtons(joystick: PSDL_Joystick): longint; cdecl; external libSDL3;
procedure SDL_SetJoystickEventsEnabled(Enabled: Tbool); cdecl; external libSDL3;
function SDL_JoystickEventsEnabled: Tbool; cdecl; external libSDL3;
procedure SDL_UpdateJoysticks; cdecl; external libSDL3;
function SDL_GetJoystickAxis(joystick: PSDL_Joystick; axis: longint): TSint16; cdecl; external libSDL3;
function SDL_GetJoystickAxisInitialState(joystick: PSDL_Joystick; axis: longint; state: PSint16): Tbool; cdecl; external libSDL3;
function SDL_GetJoystickBall(joystick: PSDL_Joystick; ball: longint; dx: Plongint; dy: Plongint): Tbool; cdecl; external libSDL3;
function SDL_GetJoystickHat(joystick: PSDL_Joystick; hat: longint): TUint8; cdecl; external libSDL3;

const
  SDL_HAT_CENTERED = $00;
  SDL_HAT_UP = $01;
  SDL_HAT_RIGHT = $02;
  SDL_HAT_DOWN = $04;
  SDL_HAT_LEFT = $08;
  SDL_HAT_RIGHTUP = SDL_HAT_RIGHT or SDL_HAT_UP;
  SDL_HAT_RIGHTDOWN = SDL_HAT_RIGHT or SDL_HAT_DOWN;
  SDL_HAT_LEFTUP = SDL_HAT_LEFT or SDL_HAT_UP;
  SDL_HAT_LEFTDOWN = SDL_HAT_LEFT or SDL_HAT_DOWN;

function SDL_GetJoystickButton(joystick: PSDL_Joystick; button: longint): Tbool; cdecl; external libSDL3;
function SDL_RumbleJoystick(joystick: PSDL_Joystick; low_frequency_rumble: TUint16; high_frequency_rumble: TUint16; duration_ms: TUint32): Tbool; cdecl; external libSDL3;
function SDL_RumbleJoystickTriggers(joystick: PSDL_Joystick; left_rumble: TUint16; right_rumble: TUint16; duration_ms: TUint32): Tbool; cdecl; external libSDL3;
function SDL_SetJoystickLED(joystick: PSDL_Joystick; red: TUint8; green: TUint8; blue: TUint8): Tbool; cdecl; external libSDL3;
function SDL_SendJoystickEffect(joystick: PSDL_Joystick; Data: pointer; size: longint): Tbool; cdecl; external libSDL3;
procedure SDL_CloseJoystick(joystick: PSDL_Joystick); cdecl; external libSDL3;
function SDL_GetJoystickConnectionState(joystick: PSDL_Joystick): TSDL_JoystickConnectionState; cdecl; external libSDL3;
function SDL_GetJoystickPowerInfo(joystick: PSDL_Joystick; percent: Plongint): TSDL_PowerState; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}

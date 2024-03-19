unit SDL3_joystick;

interface

uses
  SDL3_guid, SDL3_stdinc, SDL3_mutex;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

var
  SDL_joystick_lock: PSDL_Mutex; cvar;external;

type
  PSDL_Joystick = ^TSDL_Joystick;
  TSDL_Joystick = Pointer;      {undefined structure}

  PSDL_JoystickGUID = ^TSDL_JoystickGUID;
  TSDL_JoystickGUID = TSDL_GUID;

  PSDL_JoystickID = ^TSDL_JoystickID;
  TSDL_JoystickID = uint32;

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

type
  PSDL_JoystickPowerLevel = ^TSDL_JoystickPowerLevel;
  TSDL_JoystickPowerLevel = longint;

const
  SDL_JOYSTICK_POWER_UNKNOWN = -(1);
  SDL_JOYSTICK_POWER_EMPTY = (-(1)) + 1;
  SDL_JOYSTICK_POWER_LOW = (-(1)) + 2;
  SDL_JOYSTICK_POWER_MEDIUM = (-(1)) + 3;
  SDL_JOYSTICK_POWER_FULL = (-(1)) + 4;
  SDL_JOYSTICK_POWER_WIRED = (-(1)) + 5;
  SDL_JOYSTICK_POWER_MAX = (-(1)) + 6;

  SDL_JOYSTICK_AXIS_MAX = 32767;
  SDL_JOYSTICK_AXIS_MIN = -(32768);
  SDL_IPHONE_MAX_GFORCE = 5.0;

procedure SDL_LockJoysticks; cdecl; external;
procedure SDL_UnlockJoysticks; cdecl; external;
function SDL_GetJoysticks(Count: Plongint): PSDL_JoystickID; cdecl; external;
function SDL_GetJoystickInstanceName(instance_id: TSDL_JoystickID): PChar; cdecl; external;
function SDL_GetJoystickInstancePath(instance_id: TSDL_JoystickID): PChar; cdecl; external;
function SDL_GetJoystickInstancePlayerIndex(instance_id: TSDL_JoystickID): longint; cdecl; external;
function SDL_GetJoystickInstanceGUID(instance_id: TSDL_JoystickID): TSDL_JoystickGUID; cdecl; external;
function SDL_GetJoystickInstanceVendor(instance_id: TSDL_JoystickID): uint16; cdecl; external;
function SDL_GetJoystickInstanceProduct(instance_id: TSDL_JoystickID): uint16; cdecl; external;
function SDL_GetJoystickInstanceProductVersion(instance_id: TSDL_JoystickID): uint16; cdecl; external;
function SDL_GetJoystickInstanceType(instance_id: TSDL_JoystickID): TSDL_JoystickType; cdecl; external;
function SDL_OpenJoystick(instance_id: TSDL_JoystickID): PSDL_Joystick; cdecl; external;
function SDL_GetJoystickFromInstanceID(instance_id: TSDL_JoystickID): PSDL_Joystick; cdecl; external;
function SDL_GetJoystickFromPlayerIndex(player_index: longint): PSDL_Joystick; cdecl; external;
function SDL_AttachVirtualJoystick(_type: TSDL_JoystickType; naxes: longint; nbuttons: longint; nhats: longint): TSDL_JoystickID; cdecl; external;

type
  PSDL_VirtualJoystickDesc = ^TSDL_VirtualJoystickDesc;

  TSDL_VirtualJoystickDesc = record
    version: uint16;
    _type: uint16;
    naxes: uint16;
    nbuttons: uint16;
    nhats: uint16;
    vendor_id: uint16;
    product_id: uint16;
    padding: uint16;
    button_mask: uint32;
    axis_mask: uint32;
    Name: PChar;
    userdata: pointer;
    Update: procedure(userdata: pointer); cdecl;
    SetPlayerIndex: procedure(userdata: pointer; player_index: longint); cdecl;
    Rumble: function(userdata: pointer; low_frequency_rumble: uint16; high_frequency_rumble: uint16): longint; cdecl;
    RumbleTriggers: function(userdata: pointer; left_rumble: uint16; right_rumble: uint16): longint; cdecl;
    SetLED: function(userdata: pointer; red: uint8; green: uint8; blue: uint8): longint; cdecl;
    SendEffect: function(userdata: pointer; Data: pointer; size: longint): longint; cdecl;
  end;

const
  SDL_VIRTUAL_JOYSTICK_DESC_VERSION = 1;

function SDL_AttachVirtualJoystickEx(desc: PSDL_VirtualJoystickDesc): TSDL_JoystickID; cdecl; external;
function SDL_DetachVirtualJoystick(instance_id: TSDL_JoystickID): longint; cdecl; external;
function SDL_IsJoystickVirtual(instance_id: TSDL_JoystickID): TSDL_bool; cdecl; external;
function SDL_SetJoystickVirtualAxis(joystick: PSDL_Joystick; axis: longint; Value: int16): longint; cdecl; external;
function SDL_SetJoystickVirtualButton(joystick: PSDL_Joystick; button: longint; Value: Uint8): longint; cdecl; external;
function SDL_SetJoystickVirtualHat(joystick: PSDL_Joystick; hat: longint; Value: Uint8): longint; cdecl; external;
function SDL_GetJoystickProperties(joystick: PSDL_Joystick): TSDL_PropertiesID; cdecl; external;

const
  SDL_PROP_JOYSTICK_CAP_MONO_LED_BOOLEAN = 'SDL.joystick.cap.mono_led';
  SDL_PROP_JOYSTICK_CAP_RGB_LED_BOOLEAN = 'SDL.joystick.cap.rgb_led';
  SDL_PROP_JOYSTICK_CAP_PLAYER_LED_BOOLEAN = 'SDL.joystick.cap.player_led';
  SDL_PROP_JOYSTICK_CAP_RUMBLE_BOOLEAN = 'SDL.joystick.cap.rumble';
  SDL_PROP_JOYSTICK_CAP_TRIGGER_RUMBLE_BOOLEAN = 'SDL.joystick.cap.trigger_rumble';

function SDL_GetJoystickName(joystick: PSDL_Joystick): PChar; cdecl; external;
function SDL_GetJoystickPath(joystick: PSDL_Joystick): PChar; cdecl; external;
function SDL_GetJoystickPlayerIndex(joystick: PSDL_Joystick): longint; cdecl; external;
function SDL_SetJoystickPlayerIndex(joystick: PSDL_Joystick; player_index: longint): longint; cdecl; external;
function SDL_GetJoystickGUID(joystick: PSDL_Joystick): TSDL_JoystickGUID; cdecl; external;
function SDL_GetJoystickVendor(joystick: PSDL_Joystick): Uint16; cdecl; external;
function SDL_GetJoystickProduct(joystick: PSDL_Joystick): Uint16; cdecl; external;
function SDL_GetJoystickProductVersion(joystick: PSDL_Joystick): Uint16; cdecl; external;
function SDL_GetJoystickFirmwareVersion(joystick: PSDL_Joystick): Uint16; cdecl; external;
function SDL_GetJoystickSerial(joystick: PSDL_Joystick): PChar; cdecl; external;
function SDL_GetJoystickType(joystick: PSDL_Joystick): TSDL_JoystickType; cdecl; external;
function SDL_GetJoystickGUIDString(guid: TSDL_JoystickGUID; pszGUID: PChar; cbGUID: longint): longint; cdecl; external;
function SDL_GetJoystickGUIDFromString(pchGUID: PChar): TSDL_JoystickGUID; cdecl; external;
procedure SDL_GetJoystickGUIDInfo(guid: TSDL_JoystickGUID; vendor: PUint16; product: PUint16; version: PUint16; crc16: PUint16); cdecl; external;
function SDL_JoystickConnected(joystick: PSDL_Joystick): TSDL_bool; cdecl; external;
function SDL_GetJoystickInstanceID(joystick: PSDL_Joystick): TSDL_JoystickID; cdecl; external;
function SDL_GetNumJoystickAxes(joystick: PSDL_Joystick): longint; cdecl; external;
function SDL_GetNumJoystickHats(joystick: PSDL_Joystick): longint; cdecl; external;
function SDL_GetNumJoystickButtons(joystick: PSDL_Joystick): longint; cdecl; external;
procedure SDL_SetJoystickEventsEnabled(Enabled: TSDL_bool); cdecl; external;
function SDL_JoystickEventsEnabled: TSDL_bool; cdecl; external;
procedure SDL_UpdateJoysticks; cdecl; external;
function SDL_GetJoystickAxis(joystick: PSDL_Joystick; axis: longint): int16; cdecl; external;
function SDL_GetJoystickAxisInitialState(joystick: PSDL_Joystick; axis: longint; state: Pint16): TSDL_bool; cdecl; external;

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

function SDL_GetJoystickHat(joystick: PSDL_Joystick; hat: longint): Uint8; cdecl; external;
function SDL_GetJoystickButton(joystick: PSDL_Joystick; button: longint): Uint8; cdecl; external;
function SDL_RumbleJoystick(joystick: PSDL_Joystick; low_frequency_rumble: Uint16; high_frequency_rumble: Uint16; duration_ms: Uint32): longint; cdecl; external;
function SDL_RumbleJoystickTriggers(joystick: PSDL_Joystick; left_rumble: Uint16; right_rumble: Uint16; duration_ms: Uint32): longint; cdecl; external;
function SDL_SetJoystickLED(joystick: PSDL_Joystick; red: Uint8; green: Uint8; blue: Uint8): longint; cdecl; external;
function SDL_SendJoystickEffect(joystick: PSDL_Joystick; Data: pointer; size: longint): longint; cdecl; external;
procedure SDL_CloseJoystick(joystick: PSDL_Joystick); cdecl; external;
function SDL_GetJoystickPowerLevel(joystick: PSDL_Joystick): TSDL_JoystickPowerLevel; cdecl; external;

implementation

end.

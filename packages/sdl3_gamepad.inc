/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_Gamepad = ^TSDL_Gamepad;
  TSDL_Gamepad = Pointer;      {undefined structure}

  PSDL_GamepadType = ^TSDL_GamepadType;
  TSDL_GamepadType = longint;

const
  SDL_GAMEPAD_TYPE_UNKNOWN = 0;
  SDL_GAMEPAD_TYPE_STANDARD = 1;
  SDL_GAMEPAD_TYPE_XBOX360 = 2;
  SDL_GAMEPAD_TYPE_XBOXONE = 3;
  SDL_GAMEPAD_TYPE_PS3 = 4;
  SDL_GAMEPAD_TYPE_PS4 = 5;
  SDL_GAMEPAD_TYPE_PS5 = 6;
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_PRO = 7;
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_LEFT = 8;
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_RIGHT = 9;
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_PAIR = 10;
  SDL_GAMEPAD_TYPE_MAX = 11;

type
  PSDL_GamepadButton = ^TSDL_GamepadButton;
  TSDL_GamepadButton = longint;

const
  SDL_GAMEPAD_BUTTON_INVALID = -(1);
  SDL_GAMEPAD_BUTTON_SOUTH = (-(1)) + 1;
  SDL_GAMEPAD_BUTTON_EAST = (-(1)) + 2;
  SDL_GAMEPAD_BUTTON_WEST = (-(1)) + 3;
  SDL_GAMEPAD_BUTTON_NORTH = (-(1)) + 4;
  SDL_GAMEPAD_BUTTON_BACK = (-(1)) + 5;
  SDL_GAMEPAD_BUTTON_GUIDE = (-(1)) + 6;
  SDL_GAMEPAD_BUTTON_START = (-(1)) + 7;
  SDL_GAMEPAD_BUTTON_LEFT_STICK = (-(1)) + 8;
  SDL_GAMEPAD_BUTTON_RIGHT_STICK = (-(1)) + 9;
  SDL_GAMEPAD_BUTTON_LEFT_SHOULDER = (-(1)) + 10;
  SDL_GAMEPAD_BUTTON_RIGHT_SHOULDER = (-(1)) + 11;
  SDL_GAMEPAD_BUTTON_DPAD_UP = (-(1)) + 12;
  SDL_GAMEPAD_BUTTON_DPAD_DOWN = (-(1)) + 13;
  SDL_GAMEPAD_BUTTON_DPAD_LEFT = (-(1)) + 14;
  SDL_GAMEPAD_BUTTON_DPAD_RIGHT = (-(1)) + 15;
  SDL_GAMEPAD_BUTTON_MISC1 = (-(1)) + 16;
  SDL_GAMEPAD_BUTTON_RIGHT_PADDLE1 = (-(1)) + 17;
  SDL_GAMEPAD_BUTTON_LEFT_PADDLE1 = (-(1)) + 18;
  SDL_GAMEPAD_BUTTON_RIGHT_PADDLE2 = (-(1)) + 19;
  SDL_GAMEPAD_BUTTON_LEFT_PADDLE2 = (-(1)) + 20;
  SDL_GAMEPAD_BUTTON_TOUCHPAD = (-(1)) + 21;
  SDL_GAMEPAD_BUTTON_MISC2 = (-(1)) + 22;
  SDL_GAMEPAD_BUTTON_MISC3 = (-(1)) + 23;
  SDL_GAMEPAD_BUTTON_MISC4 = (-(1)) + 24;
  SDL_GAMEPAD_BUTTON_MISC5 = (-(1)) + 25;
  SDL_GAMEPAD_BUTTON_MISC6 = (-(1)) + 26;
  SDL_GAMEPAD_BUTTON_MAX = (-(1)) + 27;

type
  PSDL_GamepadButtonLabel = ^TSDL_GamepadButtonLabel;
  TSDL_GamepadButtonLabel = longint;

const
  SDL_GAMEPAD_BUTTON_LABEL_UNKNOWN = 0;
  SDL_GAMEPAD_BUTTON_LABEL_A = 1;
  SDL_GAMEPAD_BUTTON_LABEL_B = 2;
  SDL_GAMEPAD_BUTTON_LABEL_X = 3;
  SDL_GAMEPAD_BUTTON_LABEL_Y = 4;
  SDL_GAMEPAD_BUTTON_LABEL_CROSS = 5;
  SDL_GAMEPAD_BUTTON_LABEL_CIRCLE = 6;
  SDL_GAMEPAD_BUTTON_LABEL_SQUARE = 7;
  SDL_GAMEPAD_BUTTON_LABEL_TRIANGLE = 8;

type
  PSDL_GamepadAxis = ^TSDL_GamepadAxis;
  TSDL_GamepadAxis = longint;

const
  SDL_GAMEPAD_AXIS_INVALID = -(1);
  SDL_GAMEPAD_AXIS_LEFTX = (-(1)) + 1;
  SDL_GAMEPAD_AXIS_LEFTY = (-(1)) + 2;
  SDL_GAMEPAD_AXIS_RIGHTX = (-(1)) + 3;
  SDL_GAMEPAD_AXIS_RIGHTY = (-(1)) + 4;
  SDL_GAMEPAD_AXIS_LEFT_TRIGGER = (-(1)) + 5;
  SDL_GAMEPAD_AXIS_RIGHT_TRIGGER = (-(1)) + 6;
  SDL_GAMEPAD_AXIS_MAX = (-(1)) + 7;

type
  PSDL_GamepadBindingType = ^TSDL_GamepadBindingType;
  TSDL_GamepadBindingType = longint;

const
  SDL_GAMEPAD_BINDTYPE_NONE = 0;
  SDL_GAMEPAD_BINDTYPE_BUTTON = 1;
  SDL_GAMEPAD_BINDTYPE_AXIS = 2;
  SDL_GAMEPAD_BINDTYPE_HAT = 3;

type
  PPSDL_GamepadBinding = ^PSDL_GamepadBinding;
  PSDL_GamepadBinding = ^TSDL_GamepadBinding;

  TSDL_GamepadBinding = record
    input_type: TSDL_GamepadBindingType;
    input: record
      case longint of
        0: (button: longint);
        1: (axis: record
            axis: longint;
            axis_min: longint;
            axis_max: longint;
            end);
        2: (hat: record
            hat: longint;
            hat_mask: longint;
            end);
      end;
    output_type: TSDL_GamepadBindingType;
    output: record
      case longint of
        0: (button: TSDL_GamepadButton);
        1: (axis: record
            axis: TSDL_GamepadAxis;
            axis_min: longint;
            axis_max: longint;
            end);
      end;
  end;

function SDL_AddGamepadMapping(mapping: PChar): longint; cdecl; external;
function SDL_AddGamepadMappingsFromRW(src: PSDL_RWops; freesrc: TSDL_bool): longint; cdecl; external;
function SDL_AddGamepadMappingsFromFile(file_: PChar): longint; cdecl; external;
function SDL_ReloadGamepadMappings: longint; cdecl; external;
function SDL_GetGamepadMappings(Count: Plongint): PPchar; cdecl; external;
function SDL_GetGamepadMappingForGUID(guid: TSDL_JoystickGUID): PChar; cdecl; external;
function SDL_GetGamepadMapping(gamepad: PSDL_Gamepad): PChar; cdecl; external;
function SDL_SetGamepadMapping(instance_id: TSDL_JoystickID; mapping: PChar): longint; cdecl; external;
function SDL_GetGamepads(Count: Plongint): PSDL_JoystickID; cdecl; external;
function SDL_IsGamepad(instance_id: TSDL_JoystickID): TSDL_bool; cdecl; external;
function SDL_GetGamepadInstanceName(instance_id: TSDL_JoystickID): PChar; cdecl; external;
function SDL_GetGamepadInstancePath(instance_id: TSDL_JoystickID): PChar; cdecl; external;
function SDL_GetGamepadInstancePlayerIndex(instance_id: TSDL_JoystickID): longint; cdecl; external;
function SDL_GetGamepadInstanceGUID(instance_id: TSDL_JoystickID): TSDL_JoystickGUID; cdecl; external;
function SDL_GetGamepadInstanceVendor(instance_id: TSDL_JoystickID): uint16; cdecl; external;
function SDL_GetGamepadInstanceProduct(instance_id: TSDL_JoystickID): uint16; cdecl; external;
function SDL_GetGamepadInstanceProductVersion(instance_id: TSDL_JoystickID): uint16; cdecl; external;
function SDL_GetGamepadInstanceType(instance_id: TSDL_JoystickID): TSDL_GamepadType; cdecl; external;
function SDL_GetRealGamepadInstanceType(instance_id: TSDL_JoystickID): TSDL_GamepadType; cdecl; external;
function SDL_GetGamepadInstanceMapping(instance_id: TSDL_JoystickID): PChar; cdecl; external;
function SDL_OpenGamepad(instance_id: TSDL_JoystickID): PSDL_Gamepad; cdecl; external;
function SDL_GetGamepadFromInstanceID(instance_id: TSDL_JoystickID): PSDL_Gamepad; cdecl; external;
function SDL_GetGamepadFromPlayerIndex(player_index: longint): PSDL_Gamepad; cdecl; external;
function SDL_GetGamepadProperties(gamepad: PSDL_Gamepad): TSDL_PropertiesID; cdecl; external;

const
  SDL_PROP_GAMEPAD_CAP_MONO_LED_BOOLEAN = SDL_PROP_JOYSTICK_CAP_MONO_LED_BOOLEAN;
  SDL_PROP_GAMEPAD_CAP_RGB_LED_BOOLEAN = SDL_PROP_JOYSTICK_CAP_RGB_LED_BOOLEAN;
  SDL_PROP_GAMEPAD_CAP_PLAYER_LED_BOOLEAN = SDL_PROP_JOYSTICK_CAP_PLAYER_LED_BOOLEAN;
  SDL_PROP_GAMEPAD_CAP_RUMBLE_BOOLEAN = SDL_PROP_JOYSTICK_CAP_RUMBLE_BOOLEAN;
  SDL_PROP_GAMEPAD_CAP_TRIGGER_RUMBLE_BOOLEAN = SDL_PROP_JOYSTICK_CAP_TRIGGER_RUMBLE_BOOLEAN;

function SDL_GetGamepadInstanceID(gamepad: PSDL_Gamepad): TSDL_JoystickID; cdecl; external;
function SDL_GetGamepadName(gamepad: PSDL_Gamepad): PChar; cdecl; external;
function SDL_GetGamepadPath(gamepad: PSDL_Gamepad): PChar; cdecl; external;
function SDL_GetGamepadType(gamepad: PSDL_Gamepad): TSDL_GamepadType; cdecl; external;
function SDL_GetRealGamepadType(gamepad: PSDL_Gamepad): TSDL_GamepadType; cdecl; external;
function SDL_GetGamepadPlayerIndex(gamepad: PSDL_Gamepad): longint; cdecl; external;
function SDL_SetGamepadPlayerIndex(gamepad: PSDL_Gamepad; player_index: longint): longint; cdecl; external;
function SDL_GetGamepadVendor(gamepad: PSDL_Gamepad): uint16; cdecl; external;
function SDL_GetGamepadProduct(gamepad: PSDL_Gamepad): uint16; cdecl; external;
function SDL_GetGamepadProductVersion(gamepad: PSDL_Gamepad): uint16; cdecl; external;
function SDL_GetGamepadFirmwareVersion(gamepad: PSDL_Gamepad): uint16; cdecl; external;
function SDL_GetGamepadSerial(gamepad: PSDL_Gamepad): PChar; cdecl; external;
function SDL_GetGamepadSteamHandle(gamepad: PSDL_Gamepad): uint64; cdecl; external;
function SDL_GetGamepadPowerLevel(gamepad: PSDL_Gamepad): TSDL_JoystickPowerLevel; cdecl; external;
function SDL_GamepadConnected(gamepad: PSDL_Gamepad): TSDL_bool; cdecl; external;
function SDL_GetGamepadJoystick(gamepad: PSDL_Gamepad): PSDL_Joystick; cdecl; external;
procedure SDL_SetGamepadEventsEnabled(Enabled: TSDL_bool); cdecl; external;
function SDL_GamepadEventsEnabled: TSDL_bool; cdecl; external;
function SDL_GetGamepadBindings(gamepad: PSDL_Gamepad; Count: Plongint): PPSDL_GamepadBinding; cdecl; external;
procedure SDL_UpdateGamepads; cdecl; external;
function SDL_GetGamepadTypeFromString(str: PChar): TSDL_GamepadType; cdecl; external;
function SDL_GetGamepadStringForType(_type: TSDL_GamepadType): PChar; cdecl; external;
function SDL_GetGamepadAxisFromString(str: PChar): TSDL_GamepadAxis; cdecl; external;
function SDL_GetGamepadStringForAxis(axis: TSDL_GamepadAxis): PChar; cdecl; external;
function SDL_GamepadHasAxis(gamepad: PSDL_Gamepad; axis: TSDL_GamepadAxis): TSDL_bool; cdecl; external;
function SDL_GetGamepadAxis(gamepad: PSDL_Gamepad; axis: TSDL_GamepadAxis): int16; cdecl; external;
function SDL_GetGamepadButtonFromString(str: PChar): TSDL_GamepadButton; cdecl; external;
function SDL_GetGamepadStringForButton(button: TSDL_GamepadButton): PChar; cdecl; external;
function SDL_GamepadHasButton(gamepad: PSDL_Gamepad; button: TSDL_GamepadButton): TSDL_bool; cdecl; external;
function SDL_GetGamepadButton(gamepad: PSDL_Gamepad; button: TSDL_GamepadButton): uint8; cdecl; external;
function SDL_GetGamepadButtonLabelForType(_type: TSDL_GamepadType; button: TSDL_GamepadButton): TSDL_GamepadButtonLabel; cdecl; external;
function SDL_GetGamepadButtonLabel(gamepad: PSDL_Gamepad; button: TSDL_GamepadButton): TSDL_GamepadButtonLabel; cdecl; external;
function SDL_GetNumGamepadTouchpads(gamepad: PSDL_Gamepad): longint; cdecl; external;
function SDL_GetNumGamepadTouchpadFingers(gamepad: PSDL_Gamepad; touchpad: longint): longint; cdecl; external;
function SDL_GetGamepadTouchpadFinger(gamepad: PSDL_Gamepad; touchpad: longint; finger: longint; state: PUint8; x: Psingle;
  y: Psingle; pressure: Psingle): longint; cdecl; external;
function SDL_GamepadHasSensor(gamepad: PSDL_Gamepad; _type: TSDL_SensorType): TSDL_bool; cdecl; external;
function SDL_SetGamepadSensorEnabled(gamepad: PSDL_Gamepad; _type: TSDL_SensorType; Enabled: TSDL_bool): longint; cdecl; external;
function SDL_GamepadSensorEnabled(gamepad: PSDL_Gamepad; _type: TSDL_SensorType): TSDL_bool; cdecl; external;
function SDL_GetGamepadSensorDataRate(gamepad: PSDL_Gamepad; _type: TSDL_SensorType): single; cdecl; external;
function SDL_GetGamepadSensorData(gamepad: PSDL_Gamepad; _type: TSDL_SensorType; Data: Psingle; num_values: longint): longint; cdecl; external;
function SDL_RumbleGamepad(gamepad: PSDL_Gamepad; low_frequency_rumble: uint16; high_frequency_rumble: uint16; duration_ms: uint32): longint; cdecl; external;
function SDL_RumbleGamepadTriggers(gamepad: PSDL_Gamepad; left_rumble: uint16; right_rumble: uint16; duration_ms: uint32): longint; cdecl; external;
function SDL_SetGamepadLED(gamepad: PSDL_Gamepad; red: uint8; green: uint8; blue: uint8): longint; cdecl; external;
function SDL_SendGamepadEffect(gamepad: PSDL_Gamepad; Data: pointer; size: longint): longint; cdecl; external;
procedure SDL_CloseGamepad(gamepad: PSDL_Gamepad); cdecl; external;
function SDL_GetGamepadAppleSFSymbolsNameForButton(gamepad: PSDL_Gamepad; button: TSDL_GamepadButton): PChar; cdecl; external;
function SDL_GetGamepadAppleSFSymbolsNameForAxis(gamepad: PSDL_Gamepad; axis: TSDL_GamepadAxis): PChar; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

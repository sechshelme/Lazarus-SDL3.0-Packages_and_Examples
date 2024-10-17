/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_GamepadType = ^TSDL_GamepadType;
  TSDL_GamepadType = longint;

const
  SDL_GAMEPAD_TYPE_UNKNOWN = 0;
  SDL_GAMEPAD_TYPE_STANDARD = (0) + 1;
  SDL_GAMEPAD_TYPE_XBOX360 = (0) + 2;
  SDL_GAMEPAD_TYPE_XBOXONE = (0) + 3;
  SDL_GAMEPAD_TYPE_PS3 = (0) + 4;
  SDL_GAMEPAD_TYPE_PS4 = (0) + 5;
  SDL_GAMEPAD_TYPE_PS5 = (0) + 6;
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_PRO = (0) + 7;
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_LEFT = (0) + 8;
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_RIGHT = (0) + 9;
  SDL_GAMEPAD_TYPE_NINTENDO_SWITCH_JOYCON_PAIR = (0) + 10;
  SDL_GAMEPAD_TYPE_COUNT = (0) + 11;

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
  SDL_GAMEPAD_BUTTON_COUNT = (-(1)) + 27;

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
  SDL_GAMEPAD_AXIS_COUNT = (-(1)) + 7;

type
  PSDL_GamepadBindingType = ^TSDL_GamepadBindingType;
  TSDL_GamepadBindingType = longint;

const
  SDL_GAMEPAD_BINDTYPE_NONE = 0;
  SDL_GAMEPAD_BINDTYPE_BUTTON = (0) + 1;
  SDL_GAMEPAD_BINDTYPE_AXIS = (0) + 2;
  SDL_GAMEPAD_BINDTYPE_HAT = (0) + 3;

type
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
  PSDL_GamepadBinding = ^TSDL_GamepadBinding;
  PPSDL_GamepadBinding = ^PSDL_GamepadBinding;

  TPSDL_Gamepad = record
  end;
  PSDL_Gamepad = ^TPSDL_Gamepad;

function SDL_AddGamepadMapping(mapping: pansichar): longint; cdecl; external libSDL3;
function SDL_AddGamepadMappingsFromIO(src: PSDL_IOStream; closeio: Tbool): longint; cdecl; external libSDL3;
function SDL_AddGamepadMappingsFromFile(file_: pansichar): longint; cdecl; external libSDL3;
function SDL_ReloadGamepadMappings: Tbool; cdecl; external libSDL3;
function SDL_GetGamepadMappings(Count: Plongint): PPChar; cdecl; external libSDL3;
function SDL_GetGamepadMappingForGUID(guid: TSDL_GUID): pansichar; cdecl; external libSDL3;
function SDL_GetGamepadMapping(gamepad: PSDL_Gamepad): pansichar; cdecl; external libSDL3;
function SDL_SetGamepadMapping(instance_id: TSDL_JoystickID; mapping: pansichar): Tbool; cdecl; external libSDL3;
function SDL_HasGamepad: Tbool; cdecl; external libSDL3;
function SDL_GetGamepads(Count: Plongint): PSDL_JoystickID; cdecl; external libSDL3;
function SDL_IsGamepad(instance_id: TSDL_JoystickID): Tbool; cdecl; external libSDL3;
function SDL_GetGamepadNameForID(instance_id: TSDL_JoystickID): pansichar; cdecl; external libSDL3;
function SDL_GetGamepadPathForID(instance_id: TSDL_JoystickID): pansichar; cdecl; external libSDL3;
function SDL_GetGamepadPlayerIndexForID(instance_id: TSDL_JoystickID): longint; cdecl; external libSDL3;
function SDL_GetGamepadGUIDForID(instance_id: TSDL_JoystickID): TSDL_GUID; cdecl; external libSDL3;
function SDL_GetGamepadVendorForID(instance_id: TSDL_JoystickID): TUint16; cdecl; external libSDL3;
function SDL_GetGamepadProductForID(instance_id: TSDL_JoystickID): TUint16; cdecl; external libSDL3;
function SDL_GetGamepadProductVersionForID(instance_id: TSDL_JoystickID): TUint16; cdecl; external libSDL3;
function SDL_GetGamepadTypeForID(instance_id: TSDL_JoystickID): TSDL_GamepadType; cdecl; external libSDL3;
function SDL_GetRealGamepadTypeForID(instance_id: TSDL_JoystickID): TSDL_GamepadType; cdecl; external libSDL3;
function SDL_GetGamepadMappingForID(instance_id: TSDL_JoystickID): pansichar; cdecl; external libSDL3;
function SDL_OpenGamepad(instance_id: TSDL_JoystickID): PSDL_Gamepad; cdecl; external libSDL3;
function SDL_GetGamepadFromID(instance_id: TSDL_JoystickID): PSDL_Gamepad; cdecl; external libSDL3;
function SDL_GetGamepadFromPlayerIndex(player_index: longint): PSDL_Gamepad; cdecl; external libSDL3;
function SDL_GetGamepadProperties(gamepad: PSDL_Gamepad): TSDL_PropertiesID; cdecl; external libSDL3;

const
  SDL_PROP_GAMEPAD_CAP_MONO_LED_BOOLEAN = SDL_PROP_JOYSTICK_CAP_MONO_LED_BOOLEAN;
  SDL_PROP_GAMEPAD_CAP_RGB_LED_BOOLEAN = SDL_PROP_JOYSTICK_CAP_RGB_LED_BOOLEAN;
  SDL_PROP_GAMEPAD_CAP_PLAYER_LED_BOOLEAN = SDL_PROP_JOYSTICK_CAP_PLAYER_LED_BOOLEAN;
  SDL_PROP_GAMEPAD_CAP_RUMBLE_BOOLEAN = SDL_PROP_JOYSTICK_CAP_RUMBLE_BOOLEAN;
  SDL_PROP_GAMEPAD_CAP_TRIGGER_RUMBLE_BOOLEAN = SDL_PROP_JOYSTICK_CAP_TRIGGER_RUMBLE_BOOLEAN;

function SDL_GetGamepadID(gamepad: PSDL_Gamepad): TSDL_JoystickID; cdecl; external libSDL3;
function SDL_GetGamepadName(gamepad: PSDL_Gamepad): pansichar; cdecl; external libSDL3;
function SDL_GetGamepadPath(gamepad: PSDL_Gamepad): pansichar; cdecl; external libSDL3;
function SDL_GetGamepadType(gamepad: PSDL_Gamepad): TSDL_GamepadType; cdecl; external libSDL3;
function SDL_GetRealGamepadType(gamepad: PSDL_Gamepad): TSDL_GamepadType; cdecl; external libSDL3;
function SDL_GetGamepadPlayerIndex(gamepad: PSDL_Gamepad): longint; cdecl; external libSDL3;
function SDL_SetGamepadPlayerIndex(gamepad: PSDL_Gamepad; player_index: longint): Tbool; cdecl; external libSDL3;
function SDL_GetGamepadVendor(gamepad: PSDL_Gamepad): TUint16; cdecl; external libSDL3;
function SDL_GetGamepadProduct(gamepad: PSDL_Gamepad): TUint16; cdecl; external libSDL3;
function SDL_GetGamepadProductVersion(gamepad: PSDL_Gamepad): TUint16; cdecl; external libSDL3;
function SDL_GetGamepadFirmwareVersion(gamepad: PSDL_Gamepad): TUint16; cdecl; external libSDL3;
function SDL_GetGamepadSerial(gamepad: PSDL_Gamepad): pansichar; cdecl; external libSDL3;
function SDL_GetGamepadSteamHandle(gamepad: PSDL_Gamepad): TUint64; cdecl; external libSDL3;
function SDL_GetGamepadConnectionState(gamepad: PSDL_Gamepad): TSDL_JoystickConnectionState; cdecl; external libSDL3;
function SDL_GetGamepadPowerInfo(gamepad: PSDL_Gamepad; percent: Plongint): TSDL_PowerState; cdecl; external libSDL3;
function SDL_GamepadConnected(gamepad: PSDL_Gamepad): Tbool; cdecl; external libSDL3;
function SDL_GetGamepadJoystick(gamepad: PSDL_Gamepad): PSDL_Joystick; cdecl; external libSDL3;
procedure SDL_SetGamepadEventsEnabled(Enabled: Tbool); cdecl; external libSDL3;
function SDL_GamepadEventsEnabled: Tbool; cdecl; external libSDL3;
function SDL_GetGamepadBindings(gamepad: PSDL_Gamepad; Count: Plongint): PPSDL_GamepadBinding; cdecl; external libSDL3;
procedure SDL_UpdateGamepads; cdecl; external libSDL3;
function SDL_GetGamepadTypeFromString(str: pansichar): TSDL_GamepadType; cdecl; external libSDL3;
function SDL_GetGamepadStringForType(_type: TSDL_GamepadType): pansichar; cdecl; external libSDL3;
function SDL_GetGamepadAxisFromString(str: pansichar): TSDL_GamepadAxis; cdecl; external libSDL3;
function SDL_GetGamepadStringForAxis(axis: TSDL_GamepadAxis): pansichar; cdecl; external libSDL3;
function SDL_GamepadHasAxis(gamepad: PSDL_Gamepad; axis: TSDL_GamepadAxis): Tbool; cdecl; external libSDL3;
function SDL_GetGamepadAxis(gamepad: PSDL_Gamepad; axis: TSDL_GamepadAxis): TSint16; cdecl; external libSDL3;
function SDL_GetGamepadButtonFromString(str: pansichar): TSDL_GamepadButton; cdecl; external libSDL3;
function SDL_GetGamepadStringForButton(button: TSDL_GamepadButton): pansichar; cdecl; external libSDL3;
function SDL_GamepadHasButton(gamepad: PSDL_Gamepad; button: TSDL_GamepadButton): Tbool; cdecl; external libSDL3;
function SDL_GetGamepadButton(gamepad: PSDL_Gamepad; button: TSDL_GamepadButton): Tbool; cdecl; external libSDL3;
function SDL_GetGamepadButtonLabelForType(_type: TSDL_GamepadType; button: TSDL_GamepadButton): TSDL_GamepadButtonLabel; cdecl; external libSDL3;
function SDL_GetGamepadButtonLabel(gamepad: PSDL_Gamepad; button: TSDL_GamepadButton): TSDL_GamepadButtonLabel; cdecl; external libSDL3;
function SDL_GetNumGamepadTouchpads(gamepad: PSDL_Gamepad): longint; cdecl; external libSDL3;
function SDL_GetNumGamepadTouchpadFingers(gamepad: PSDL_Gamepad; touchpad: longint): longint; cdecl; external libSDL3;
function SDL_GetGamepadTouchpadFinger(gamepad: PSDL_Gamepad; touchpad: longint; finger: longint; down: PBoolean; x: Psingle;
  y: Psingle; pressure: Psingle): Tbool; cdecl; external libSDL3;
function SDL_GamepadHasSensor(gamepad: PSDL_Gamepad; _type: TSDL_SensorType): Tbool; cdecl; external libSDL3;
function SDL_SetGamepadSensorEnabled(gamepad: PSDL_Gamepad; _type: TSDL_SensorType; Enabled: Tbool): Tbool; cdecl; external libSDL3;
function SDL_GamepadSensorEnabled(gamepad: PSDL_Gamepad; _type: TSDL_SensorType): Tbool; cdecl; external libSDL3;
function SDL_GetGamepadSensorDataRate(gamepad: PSDL_Gamepad; _type: TSDL_SensorType): single; cdecl; external libSDL3;
function SDL_GetGamepadSensorData(gamepad: PSDL_Gamepad; _type: TSDL_SensorType; Data: Psingle; num_values: longint): Tbool; cdecl; external libSDL3;
function SDL_RumbleGamepad(gamepad: PSDL_Gamepad; low_frequency_rumble: TUint16; high_frequency_rumble: TUint16; duration_ms: TUint32): Tbool; cdecl; external libSDL3;
function SDL_RumbleGamepadTriggers(gamepad: PSDL_Gamepad; left_rumble: TUint16; right_rumble: TUint16; duration_ms: TUint32): Tbool; cdecl; external libSDL3;
function SDL_SetGamepadLED(gamepad: PSDL_Gamepad; red: TUint8; green: TUint8; blue: TUint8): Tbool; cdecl; external libSDL3;
function SDL_SendGamepadEffect(gamepad: PSDL_Gamepad; Data: pointer; size: longint): Tbool; cdecl; external libSDL3;
procedure SDL_CloseGamepad(gamepad: PSDL_Gamepad); cdecl; external libSDL3;
function SDL_GetGamepadAppleSFSymbolsNameForButton(gamepad: PSDL_Gamepad; button: TSDL_GamepadButton): pansichar; cdecl; external libSDL3;
function SDL_GetGamepadAppleSFSymbolsNameForAxis(gamepad: PSDL_Gamepad; axis: TSDL_GamepadAxis): pansichar; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}

unit SDL3_haptic;

interface

uses
  SDL3_stdinc, SDL3_joystick;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

type
  PSDL_Haptic = ^TSDL_Haptic;
  TSDL_Haptic = pointer;   {undefined structure}

const
  SDL_HAPTIC_CONSTANT = 1 shl 0;
  SDL_HAPTIC_SINE = 1 shl 1;
  SDL_HAPTIC_SQUARE = 1 shl 2;
  SDL_HAPTIC_TRIANGLE = 1 shl 3;
  SDL_HAPTIC_SAWTOOTHUP = 1 shl 4;
  SDL_HAPTIC_SAWTOOTHDOWN = 1 shl 5;
  SDL_HAPTIC_RAMP = 1 shl 6;
  SDL_HAPTIC_SPRING = 1 shl 7;
  SDL_HAPTIC_DAMPER = 1 shl 8;
  SDL_HAPTIC_INERTIA = 1 shl 9;
  SDL_HAPTIC_FRICTION = 1 shl 10;
  SDL_HAPTIC_LEFTRIGHT = 1 shl 11;
  SDL_HAPTIC_RESERVED1 = 1 shl 12;
  SDL_HAPTIC_RESERVED2 = 1 shl 13;
  SDL_HAPTIC_RESERVED3 = 1 shl 14;
  SDL_HAPTIC_CUSTOM = 1 shl 15;
  SDL_HAPTIC_GAIN = 1 shl 16;
  SDL_HAPTIC_AUTOCENTER = 1 shl 17;
  SDL_HAPTIC_STATUS = 1 shl 18;
  SDL_HAPTIC_PAUSE = 1 shl 19;
  SDL_HAPTIC_POLAR = 0;
  SDL_HAPTIC_CARTESIAN = 1;
  SDL_HAPTIC_SPHERICAL = 2;
  SDL_HAPTIC_STEERING_AXIS = 3;
  SDL_HAPTIC_INFINITY = 4294967295;

type
  PSDL_HapticDirection = ^TSDL_HapticDirection;

  TSDL_HapticDirection = record
    _type: uint8;
    dir: array[0..2] of int32;
  end;

  PSDL_HapticConstant = ^TSDL_HapticConstant;

  TSDL_HapticConstant = record
    _type: uint16;
    direction: TSDL_HapticDirection;
    length: uint32;
    delay: uint16;
    button: uint16;
    interval: uint16;
    level: int16;
    attack_length: uint16;
    attack_level: uint16;
    fade_length: uint16;
    fade_level: uint16;
  end;
  PSDL_HapticPeriodic = ^TSDL_HapticPeriodic;

  TSDL_HapticPeriodic = record
    _type: uint16;
    direction: TSDL_HapticDirection;
    length: uint32;
    delay: uint16;
    button: uint16;
    interval: uint16;
    period: uint16;
    magnitude: int16;
    offset: int16;
    phase: uint16;
    attack_length: uint16;
    attack_level: uint16;
    fade_length: uint16;
    fade_level: uint16;
  end;

  PSDL_HapticCondition = ^TSDL_HapticCondition;

  TSDL_HapticCondition = record
    _type: uint16;
    direction: TSDL_HapticDirection;
    length: uint32;
    delay: uint16;
    button: uint16;
    interval: uint16;
    right_sat: array[0..2] of uint16;
    left_sat: array[0..2] of uint16;
    right_coeff: array[0..2] of int16;
    left_coeff: array[0..2] of int16;
    deadband: array[0..2] of uint16;
    center: array[0..2] of int16;
  end;

  PSDL_HapticRamp = ^TSDL_HapticRamp;

  TSDL_HapticRamp = record
    _type: uint16;
    direction: TSDL_HapticDirection;
    length: uint32;
    delay: uint16;
    button: uint16;
    interval: uint16;
    start: int16;
    end_: int16;
    attack_length: uint16;
    attack_level: uint16;
    fade_length: uint16;
    fade_level: uint16;
  end;

  PSDL_HapticLeftRight = ^TSDL_HapticLeftRight;

  TSDL_HapticLeftRight = record
    _type: uint16;
    length: uint32;
    large_magnitude: uint16;
    small_magnitude: uint16;
  end;
  PSDL_HapticCustom = ^TSDL_HapticCustom;

  TSDL_HapticCustom = record
    _type: uint16;
    direction: TSDL_HapticDirection;
    length: uint32;
    delay: uint16;
    button: uint16;
    interval: uint16;
    channels: uint8;
    period: uint16;
    samples: uint16;
    Data: PUint16;
    attack_length: uint16;
    attack_level: uint16;
    fade_length: uint16;
    fade_level: uint16;
  end;
  PSDL_HapticEffect = ^TSDL_HapticEffect;
  TSDL_HapticEffect = record
    case longint of
      0: (_type: uint16);
      1: (constant: TSDL_HapticConstant);
      2: (periodic: TSDL_HapticPeriodic);
      3: (condition: TSDL_HapticCondition);
      4: (ramp: TSDL_HapticRamp);
      5: (leftright: TSDL_HapticLeftRight);
      6: (custom: TSDL_HapticCustom);
  end;

  PSDL_HapticID = ^TSDL_HapticID;
  TSDL_HapticID = uint32;

function SDL_GetHaptics(Count: Plongint): PSDL_HapticID; cdecl; external;
function SDL_GetHapticInstanceName(instance_id: TSDL_HapticID): PChar; cdecl; external;
function SDL_OpenHaptic(instance_id: TSDL_HapticID): PSDL_Haptic; cdecl; external;
function SDL_GetHapticFromInstanceID(instance_id: TSDL_HapticID): PSDL_Haptic; cdecl; external;
function SDL_GetHapticInstanceID(haptic: PSDL_Haptic): TSDL_HapticID; cdecl; external;
function SDL_GetHapticName(haptic: PSDL_Haptic): PChar; cdecl; external;
function SDL_IsMouseHaptic: TSDL_bool; cdecl; external;
function SDL_OpenHapticFromMouse: PSDL_Haptic; cdecl; external;
function SDL_IsJoystickHaptic(joystick: PSDL_Joystick): TSDL_bool; cdecl; external;
function SDL_OpenHapticFromJoystick(joystick: PSDL_Joystick): PSDL_Haptic; cdecl; external;
procedure SDL_CloseHaptic(haptic: PSDL_Haptic); cdecl; external;
function SDL_GetMaxHapticEffects(haptic: PSDL_Haptic): longint; cdecl; external;
function SDL_GetMaxHapticEffectsPlaying(haptic: PSDL_Haptic): longint; cdecl; external;
function SDL_GetHapticFeatures(haptic: PSDL_Haptic): uint32; cdecl; external;
function SDL_GetNumHapticAxes(haptic: PSDL_Haptic): longint; cdecl; external;
function SDL_HapticEffectSupported(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): TSDL_bool; cdecl; external;
function SDL_CreateHapticEffect(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): longint; cdecl; external;
function SDL_UpdateHapticEffect(haptic: PSDL_Haptic; effect: longint; Data: PSDL_HapticEffect): longint; cdecl; external;
function SDL_RunHapticEffect(haptic: PSDL_Haptic; effect: longint; iterations: uint32): longint; cdecl; external;
function SDL_StopHapticEffect(haptic: PSDL_Haptic; effect: longint): longint; cdecl; external;
procedure SDL_DestroyHapticEffect(haptic: PSDL_Haptic; effect: longint); cdecl; external;
function SDL_GetHapticEffectStatus(haptic: PSDL_Haptic; effect: longint): longint; cdecl; external;
function SDL_SetHapticGain(haptic: PSDL_Haptic; gain: longint): longint; cdecl; external;
function SDL_SetHapticAutocenter(haptic: PSDL_Haptic; autocenter: longint): longint; cdecl; external;
function SDL_PauseHaptic(haptic: PSDL_Haptic): longint; cdecl; external;
function SDL_ResumeHaptic(haptic: PSDL_Haptic): longint; cdecl; external;
function SDL_StopHapticEffects(haptic: PSDL_Haptic): longint; cdecl; external;
function SDL_HapticRumbleSupported(haptic: PSDL_Haptic): TSDL_bool; cdecl; external;
function SDL_InitHapticRumble(haptic: PSDL_Haptic): longint; cdecl; external;
function SDL_PlayHapticRumble(haptic: PSDL_Haptic; strength: single; length: uint32): longint; cdecl; external;
function SDL_StopHapticRumble(haptic: PSDL_Haptic): longint; cdecl; external;

implementation

end.

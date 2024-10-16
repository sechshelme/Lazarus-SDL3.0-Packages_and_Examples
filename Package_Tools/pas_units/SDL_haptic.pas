unit SDL_haptic;

interface

uses
  ctypes, SDL_stdinc, SDL_joystick;

  {$IFDEF FPC}
  {$PACKRECORDS C}
  {$ENDIF}

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
  TSDL_HapticDirection = record
    _type: TUint8;
    dir: array[0..2] of TSint32;
  end;
  PSDL_HapticDirection = ^TSDL_HapticDirection;

  TSDL_HapticConstant = record
    _type: TUint16;
    direction: TSDL_HapticDirection;
    length: TUint32;
    delay: TUint16;
    button: TUint16;
    interval: TUint16;
    level: TSint16;
    attack_length: TUint16;
    attack_level: TUint16;
    fade_length: TUint16;
    fade_level: TUint16;
  end;
  PSDL_HapticConstant = ^TSDL_HapticConstant;

  TSDL_HapticPeriodic = record
    _type: TUint16;
    direction: TSDL_HapticDirection;
    length: TUint32;
    delay: TUint16;
    button: TUint16;
    interval: TUint16;
    period: TUint16;
    magnitude: TSint16;
    offset: TSint16;
    phase: TUint16;
    attack_length: TUint16;
    attack_level: TUint16;
    fade_length: TUint16;
    fade_level: TUint16;
  end;
  PSDL_HapticPeriodic = ^TSDL_HapticPeriodic;

  TSDL_HapticCondition = record
    _type: TUint16;
    direction: TSDL_HapticDirection;
    length: TUint32;
    delay: TUint16;
    button: TUint16;
    interval: TUint16;
    right_sat: array[0..2] of TUint16;
    left_sat: array[0..2] of TUint16;
    right_coeff: array[0..2] of TSint16;
    left_coeff: array[0..2] of TSint16;
    deadband: array[0..2] of TUint16;
    center: array[0..2] of TSint16;
  end;
  PSDL_HapticCondition = ^TSDL_HapticCondition;

  TSDL_HapticRamp = record
    _type: TUint16;
    direction: TSDL_HapticDirection;
    length: TUint32;
    delay: TUint16;
    button: TUint16;
    interval: TUint16;
    start: TSint16;
    end_: TSint16;
    attack_length: TUint16;
    attack_level: TUint16;
    fade_length: TUint16;
    fade_level: TUint16;
  end;
  PSDL_HapticRamp = ^TSDL_HapticRamp;

  TSDL_HapticLeftRight = record
    _type: TUint16;
    length: TUint32;
    large_magnitude: TUint16;
    small_magnitude: TUint16;
  end;
  PSDL_HapticLeftRight = ^TSDL_HapticLeftRight;

  TSDL_HapticCustom = record
    _type: TUint16;
    direction: TSDL_HapticDirection;
    length: TUint32;
    delay: TUint16;
    button: TUint16;
    interval: TUint16;
    channels: TUint8;
    period: TUint16;
    samples: TUint16;
    Data: PUint16;
    attack_length: TUint16;
    attack_level: TUint16;
    fade_length: TUint16;
    fade_level: TUint16;
  end;
  PSDL_HapticCustom = ^TSDL_HapticCustom;

  TSDL_HapticEffect = record
    case longint of
      0: (_type: TUint16);
      1: (constant: TSDL_HapticConstant);
      2: (periodic: TSDL_HapticPeriodic);
      3: (condition: TSDL_HapticCondition);
      4: (ramp: TSDL_HapticRamp);
      5: (leftright: TSDL_HapticLeftRight);
      6: (custom: TSDL_HapticCustom);
  end;
  PSDL_HapticEffect = ^TSDL_HapticEffect;

  TSDL_HapticID = TUint32;
  PSDL_HapticID = ^TSDL_HapticID;

  TSDL_Haptic = record
  end;
  PSDL_Haptic = ^TSDL_Haptic;

function SDL_GetHaptics(Count: Plongint): PSDL_HapticID; cdecl; external libSDL3;
function SDL_GetHapticNameForID(instance_id: TSDL_HapticID): pansichar; cdecl; external libSDL3;
function SDL_OpenHaptic(instance_id: TSDL_HapticID): PSDL_Haptic; cdecl; external libSDL3;
function SDL_GetHapticFromID(instance_id: TSDL_HapticID): PSDL_Haptic; cdecl; external libSDL3;
function SDL_GetHapticID(haptic: PSDL_Haptic): TSDL_HapticID; cdecl; external libSDL3;
function SDL_GetHapticName(haptic: PSDL_Haptic): pansichar; cdecl; external libSDL3;
function SDL_IsMouseHaptic: Tbool; cdecl; external libSDL3;
function SDL_OpenHapticFromMouse: PSDL_Haptic; cdecl; external libSDL3;
function SDL_IsJoystickHaptic(joystick: PSDL_Joystick): Tbool; cdecl; external libSDL3;
function SDL_OpenHapticFromJoystick(joystick: PSDL_Joystick): PSDL_Haptic; cdecl; external libSDL3;
procedure SDL_CloseHaptic(haptic: PSDL_Haptic); cdecl; external libSDL3;
function SDL_GetMaxHapticEffects(haptic: PSDL_Haptic): longint; cdecl; external libSDL3;
function SDL_GetMaxHapticEffectsPlaying(haptic: PSDL_Haptic): longint; cdecl; external libSDL3;
function SDL_GetHapticFeatures(haptic: PSDL_Haptic): TUint32; cdecl; external libSDL3;
function SDL_GetNumHapticAxes(haptic: PSDL_Haptic): longint; cdecl; external libSDL3;
function SDL_HapticEffectSupported(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): Tbool; cdecl; external libSDL3;
function SDL_CreateHapticEffect(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): longint; cdecl; external libSDL3;
function SDL_UpdateHapticEffect(haptic: PSDL_Haptic; effect: longint; Data: PSDL_HapticEffect): Tbool; cdecl; external libSDL3;
function SDL_RunHapticEffect(haptic: PSDL_Haptic; effect: longint; iterations: TUint32): Tbool; cdecl; external libSDL3;
function SDL_StopHapticEffect(haptic: PSDL_Haptic; effect: longint): Tbool; cdecl; external libSDL3;
procedure SDL_DestroyHapticEffect(haptic: PSDL_Haptic; effect: longint); cdecl; external libSDL3;
function SDL_GetHapticEffectStatus(haptic: PSDL_Haptic; effect: longint): Tbool; cdecl; external libSDL3;
function SDL_SetHapticGain(haptic: PSDL_Haptic; gain: longint): Tbool; cdecl; external libSDL3;
function SDL_SetHapticAutocenter(haptic: PSDL_Haptic; autocenter: longint): Tbool; cdecl; external libSDL3;
function SDL_PauseHaptic(haptic: PSDL_Haptic): Tbool; cdecl; external libSDL3;
function SDL_ResumeHaptic(haptic: PSDL_Haptic): Tbool; cdecl; external libSDL3;
function SDL_StopHapticEffects(haptic: PSDL_Haptic): Tbool; cdecl; external libSDL3;
function SDL_HapticRumbleSupported(haptic: PSDL_Haptic): Tbool; cdecl; external libSDL3;
function SDL_InitHapticRumble(haptic: PSDL_Haptic): Tbool; cdecl; external libSDL3;
function SDL_PlayHapticRumble(haptic: PSDL_Haptic; strength: single; length: TUint32): Tbool; cdecl; external libSDL3;
function SDL_StopHapticRumble(haptic: PSDL_Haptic): Tbool; cdecl; external libSDL3;

implementation


end.

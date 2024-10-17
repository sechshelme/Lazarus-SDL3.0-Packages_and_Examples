/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

const
  SDL_RELEASED = 0;
  SDL_PRESSED = 1;

type
  PSDL_EventType = ^TSDL_EventType;
  TSDL_EventType = longint;

const
  SDL_EVENT_FIRST = 0;
  SDL_EVENT_QUIT = $100;
  SDL_EVENT_TERMINATING = 257;
  SDL_EVENT_LOW_MEMORY = 258;
  SDL_EVENT_WILL_ENTER_BACKGROUND = 259;
  SDL_EVENT_DID_ENTER_BACKGROUND = 260;
  SDL_EVENT_WILL_ENTER_FOREGROUND = 261;
  SDL_EVENT_DID_ENTER_FOREGROUND = 262;
  SDL_EVENT_LOCALE_CHANGED = 263;
  SDL_EVENT_SYSTEM_THEME_CHANGED = 264;
  SDL_EVENT_DISPLAY_ORIENTATION = $151;
  SDL_EVENT_DISPLAY_ADDED = 338;
  SDL_EVENT_DISPLAY_REMOVED = 339;
  SDL_EVENT_DISPLAY_MOVED = 340;
  SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED = 341;
  SDL_EVENT_DISPLAY_FIRST = SDL_EVENT_DISPLAY_ORIENTATION;
  SDL_EVENT_DISPLAY_LAST = SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED;
  SDL_EVENT_WINDOW_SHOWN = $202;
  SDL_EVENT_WINDOW_HIDDEN = 515;
  SDL_EVENT_WINDOW_EXPOSED = 516;
  SDL_EVENT_WINDOW_MOVED = 517;
  SDL_EVENT_WINDOW_RESIZED = 518;
  SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED = 519;
  SDL_EVENT_WINDOW_MINIMIZED = 520;
  SDL_EVENT_WINDOW_MAXIMIZED = 521;
  SDL_EVENT_WINDOW_RESTORED = 522;
  SDL_EVENT_WINDOW_MOUSE_ENTER = 523;
  SDL_EVENT_WINDOW_MOUSE_LEAVE = 524;
  SDL_EVENT_WINDOW_FOCUS_GAINED = 525;
  SDL_EVENT_WINDOW_FOCUS_LOST = 526;
  SDL_EVENT_WINDOW_CLOSE_REQUESTED = 527;
  SDL_EVENT_WINDOW_TAKE_FOCUS = 528;
  SDL_EVENT_WINDOW_HIT_TEST = 529;
  SDL_EVENT_WINDOW_ICCPROF_CHANGED = 530;
  SDL_EVENT_WINDOW_DISPLAY_CHANGED = 531;
  SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED = 532;
  SDL_EVENT_WINDOW_OCCLUDED = 533;
  SDL_EVENT_WINDOW_ENTER_FULLSCREEN = 534;
  SDL_EVENT_WINDOW_LEAVE_FULLSCREEN = 535;
  SDL_EVENT_WINDOW_DESTROYED = 536;
  SDL_EVENT_WINDOW_PEN_ENTER = 537;
  SDL_EVENT_WINDOW_PEN_LEAVE = 538;
  SDL_EVENT_WINDOW_HDR_STATE_CHANGED = 539;
  SDL_EVENT_WINDOW_FIRST = SDL_EVENT_WINDOW_SHOWN;
  SDL_EVENT_WINDOW_LAST = SDL_EVENT_WINDOW_PEN_LEAVE;
  SDL_EVENT_KEY_DOWN = $300;
  SDL_EVENT_KEY_UP = 769;
  SDL_EVENT_TEXT_EDITING = 770;
  SDL_EVENT_TEXT_INPUT = 771;
  SDL_EVENT_KEYMAP_CHANGED = 772;
  SDL_EVENT_KEYBOARD_ADDED = 773;
  SDL_EVENT_KEYBOARD_REMOVED = 774;
  SDL_EVENT_MOUSE_MOTION = $400;
  SDL_EVENT_MOUSE_BUTTON_DOWN = 1025;
  SDL_EVENT_MOUSE_BUTTON_UP = 1026;
  SDL_EVENT_MOUSE_WHEEL = 1027;
  SDL_EVENT_MOUSE_ADDED = 1028;
  SDL_EVENT_MOUSE_REMOVED = 1029;
  SDL_EVENT_JOYSTICK_AXIS_MOTION = $600;
  SDL_EVENT_JOYSTICK_BALL_MOTION = 1537;
  SDL_EVENT_JOYSTICK_HAT_MOTION = 1538;
  SDL_EVENT_JOYSTICK_BUTTON_DOWN = 1539;
  SDL_EVENT_JOYSTICK_BUTTON_UP = 1540;
  SDL_EVENT_JOYSTICK_ADDED = 1541;
  SDL_EVENT_JOYSTICK_REMOVED = 1542;
  SDL_EVENT_JOYSTICK_BATTERY_UPDATED = 1543;
  SDL_EVENT_JOYSTICK_UPDATE_COMPLETE = 1544;
  SDL_EVENT_GAMEPAD_AXIS_MOTION = $650;
  SDL_EVENT_GAMEPAD_BUTTON_DOWN = 1617;
  SDL_EVENT_GAMEPAD_BUTTON_UP = 1618;
  SDL_EVENT_GAMEPAD_ADDED = 1619;
  SDL_EVENT_GAMEPAD_REMOVED = 1620;
  SDL_EVENT_GAMEPAD_REMAPPED = 1621;
  SDL_EVENT_GAMEPAD_TOUCHPAD_DOWN = 1622;
  SDL_EVENT_GAMEPAD_TOUCHPAD_MOTION = 1623;
  SDL_EVENT_GAMEPAD_TOUCHPAD_UP = 1624;
  SDL_EVENT_GAMEPAD_SENSOR_UPDATE = 1625;
  SDL_EVENT_GAMEPAD_UPDATE_COMPLETE = 1626;
  SDL_EVENT_GAMEPAD_STEAM_HANDLE_UPDATED = 1627;
  SDL_EVENT_FINGER_DOWN = $700;
  SDL_EVENT_FINGER_UP = 1793;
  SDL_EVENT_FINGER_MOTION = 1794;
  SDL_EVENT_CLIPBOARD_UPDATE = $900;
  SDL_EVENT_DROP_FILE = $1000;
  SDL_EVENT_DROP_TEXT = 4097;
  SDL_EVENT_DROP_BEGIN = 4098;
  SDL_EVENT_DROP_COMPLETE = 4099;
  SDL_EVENT_DROP_POSITION = 4100;
  SDL_EVENT_AUDIO_DEVICE_ADDED = $1100;
  SDL_EVENT_AUDIO_DEVICE_REMOVED = 4353;
  SDL_EVENT_AUDIO_DEVICE_FORMAT_CHANGED = 4354;
  SDL_EVENT_SENSOR_UPDATE = $1200;
  SDL_EVENT_PEN_DOWN = $1300;
  SDL_EVENT_PEN_UP = 4865;
  SDL_EVENT_PEN_MOTION = 4866;
  SDL_EVENT_PEN_BUTTON_DOWN = 4867;
  SDL_EVENT_PEN_BUTTON_UP = 4868;
  SDL_EVENT_CAMERA_DEVICE_ADDED = $1400;
  SDL_EVENT_CAMERA_DEVICE_REMOVED = 5121;
  SDL_EVENT_CAMERA_DEVICE_APPROVED = 5122;
  SDL_EVENT_CAMERA_DEVICE_DENIED = 5123;
  SDL_EVENT_RENDER_TARGETS_RESET = $2000;
  SDL_EVENT_RENDER_DEVICE_RESET = 8193;
  SDL_EVENT_POLL_SENTINEL = $7F00;
  SDL_EVENT_USER = $8000;
  SDL_EVENT_LAST = $FFFF;
  SDL_EVENT_ENUM_PADDING = $7FFFFFFF;

type
  PSDL_CommonEvent = ^TSDL_CommonEvent;

  TSDL_CommonEvent = record
    _type: TUint32;
    reserved: TUint32;
    timestamp: TUint64;
  end;

  PSDL_DisplayEvent = ^TSDL_DisplayEvent;

  TSDL_DisplayEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    displayID: TSDL_DisplayID;
    data1: TSint32;
  end;

  PSDL_WindowEvent = ^TSDL_WindowEvent;

  TSDL_WindowEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    data1: TSint32;
    data2: TSint32;
  end;

  PSDL_KeyboardDeviceEvent = ^TSDL_KeyboardDeviceEvent;

  TSDL_KeyboardDeviceEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_KeyboardID;
  end;

  PSDL_KeyboardEvent = ^TSDL_KeyboardEvent;

  TSDL_KeyboardEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    which: TSDL_KeyboardID;
    scancode: TSDL_Scancode;
    key: TSDL_Keycode;
    mod_: TSDL_Keymod;
    raw: TUint16;
    state: TUint8;
    _repeat: TUint8;
  end;

  PSDL_TextEditingEvent = ^TSDL_TextEditingEvent;

  TSDL_TextEditingEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    Text: PChar;
    start: TSint32;
    length: TSint32;
  end;

  PSDL_TextInputEvent = ^TSDL_TextInputEvent;

  TSDL_TextInputEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    Text: PChar;
  end;

  PSDL_MouseDeviceEvent = ^TSDL_MouseDeviceEvent;

  TSDL_MouseDeviceEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_MouseID;
  end;

  PSDL_MouseMotionEvent = ^TSDL_MouseMotionEvent;

  TSDL_MouseMotionEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    which: TSDL_MouseID;
    state: TSDL_MouseButtonFlags;
    x: single;
    y: single;
    xrel: single;
    yrel: single;
  end;

  PSDL_MouseButtonEvent = ^TSDL_MouseButtonEvent;

  TSDL_MouseButtonEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    which: TSDL_MouseID;
    button: TUint8;
    state: TUint8;
    clicks: TUint8;
    padding: TUint8;
    x: single;
    y: single;
  end;

  PSDL_MouseWheelEvent = ^TSDL_MouseWheelEvent;

  TSDL_MouseWheelEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    which: TSDL_MouseID;
    x: single;
    y: single;
    direction: TSDL_MouseWheelDirection;
    mouse_x: single;
    mouse_y: single;
  end;

  PSDL_JoyAxisEvent = ^TSDL_JoyAxisEvent;

  TSDL_JoyAxisEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
    axis: TUint8;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
    Value: TSint16;
    padding4: TUint16;
  end;

  PSDL_JoyBallEvent = ^TSDL_JoyBallEvent;

  TSDL_JoyBallEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
    ball: TUint8;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
    xrel: TSint16;
    yrel: TSint16;
  end;

  PSDL_JoyHatEvent = ^TSDL_JoyHatEvent;

  TSDL_JoyHatEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
    hat: TUint8;
    Value: TUint8;
    padding1: TUint8;
    padding2: TUint8;
  end;

  PSDL_JoyButtonEvent = ^TSDL_JoyButtonEvent;

  TSDL_JoyButtonEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
    button: TUint8;
    state: TUint8;
    padding1: TUint8;
    padding2: TUint8;
  end;

  PSDL_JoyDeviceEvent = ^TSDL_JoyDeviceEvent;

  TSDL_JoyDeviceEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
  end;

  PSDL_JoyBatteryEvent = ^TSDL_JoyBatteryEvent;

  TSDL_JoyBatteryEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
    state: TSDL_PowerState;
    percent: longint;
  end;

  PSDL_GamepadAxisEvent = ^TSDL_GamepadAxisEvent;

  TSDL_GamepadAxisEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
    axis: TUint8;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
    Value: TSint16;
    padding4: TUint16;
  end;

  PSDL_GamepadButtonEvent = ^TSDL_GamepadButtonEvent;

  TSDL_GamepadButtonEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
    button: TUint8;
    state: TUint8;
    padding1: TUint8;
    padding2: TUint8;
  end;

  PSDL_GamepadDeviceEvent = ^TSDL_GamepadDeviceEvent;

  TSDL_GamepadDeviceEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
  end;

  PSDL_GamepadTouchpadEvent = ^TSDL_GamepadTouchpadEvent;

  TSDL_GamepadTouchpadEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
    touchpad: TSint32;
    finger: TSint32;
    x: single;
    y: single;
    pressure: single;
  end;

  PSDL_GamepadSensorEvent = ^TSDL_GamepadSensorEvent;

  TSDL_GamepadSensorEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_JoystickID;
    sensor: TSint32;
    Data: array[0..2] of single;
    sensor_timestamp: TUint64;
  end;

  PSDL_AudioDeviceEvent = ^TSDL_AudioDeviceEvent;

  TSDL_AudioDeviceEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_AudioDeviceID;
    recording: TUint8;
    padding1: TUint8;
    padding2: TUint8;
    padding3: TUint8;
  end;

  PSDL_CameraDeviceEvent = ^TSDL_CameraDeviceEvent;

  TSDL_CameraDeviceEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_CameraDeviceID;
  end;

  PSDL_TouchFingerEvent = ^TSDL_TouchFingerEvent;

  TSDL_TouchFingerEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    touchID: TSDL_TouchID;
    fingerID: TSDL_FingerID;
    x: single;
    y: single;
    dx: single;
    dy: single;
    pressure: single;
    windowID: TSDL_WindowID;
  end;

  PSDL_PenTipEvent = ^TSDL_PenTipEvent;

  TSDL_PenTipEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    which: TSDL_PenID;
    tip: TUint8;
    state: TUint8;
    pen_state: TUint16;
    x: single;
    y: single;
    axes: array[0..(SDL_PEN_NUM_AXES) - 1] of single;
  end;

  PSDL_PenMotionEvent = ^TSDL_PenMotionEvent;

  TSDL_PenMotionEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    which: TSDL_PenID;
    padding1: TUint8;
    padding2: TUint8;
    pen_state: TUint16;
    x: single;
    y: single;
    axes: array[0..(SDL_PEN_NUM_AXES) - 1] of single;
  end;

  PSDL_PenButtonEvent = ^TSDL_PenButtonEvent;

  TSDL_PenButtonEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    which: TSDL_PenID;
    button: TUint8;
    state: TUint8;
    pen_state: TUint16;
    x: single;
    y: single;
    axes: array[0..(SDL_PEN_NUM_AXES) - 1] of single;
  end;

  PSDL_DropEvent = ^TSDL_DropEvent;

  TSDL_DropEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    x: single;
    y: single;
    Source: PChar;
    Data: PChar;
  end;

  PSDL_ClipboardEvent = ^TSDL_ClipboardEvent;

  TSDL_ClipboardEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
  end;

  PSDL_SensorEvent = ^TSDL_SensorEvent;

  TSDL_SensorEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
    which: TSDL_SensorID;
    Data: array[0..5] of single;
    sensor_timestamp: TUint64;
  end;

  PSDL_QuitEvent = ^TSDL_QuitEvent;

  TSDL_QuitEvent = record
    _type: TSDL_EventType;
    reserved: TUint32;
    timestamp: TUint64;
  end;

  PSDL_UserEvent = ^TSDL_UserEvent;

  TSDL_UserEvent = record
    _type: TUint32;
    reserved: TUint32;
    timestamp: TUint64;
    windowID: TSDL_WindowID;
    code: TSint32;
    data1: pointer;
    data2: pointer;
  end;

  PSDL_Event = ^TSDL_Event;
  TSDL_Event = record
    case longint of
      0: (_type: TUint32);
      1: (common: TSDL_CommonEvent);
      2: (display: TSDL_DisplayEvent);
      3: (window: TSDL_WindowEvent);
      4: (kdevice: TSDL_KeyboardDeviceEvent);
      5: (key: TSDL_KeyboardEvent);
      6: (edit: TSDL_TextEditingEvent);
      7: (Text: TSDL_TextInputEvent);
      8: (mdevice: TSDL_MouseDeviceEvent);
      9: (motion: TSDL_MouseMotionEvent);
      10: (button: TSDL_MouseButtonEvent);
      11: (wheel: TSDL_MouseWheelEvent);
      12: (jdevice: TSDL_JoyDeviceEvent);
      13: (jaxis: TSDL_JoyAxisEvent);
      14: (jball: TSDL_JoyBallEvent);
      15: (jhat: TSDL_JoyHatEvent);
      16: (jbutton: TSDL_JoyButtonEvent);
      17: (jbattery: TSDL_JoyBatteryEvent);
      18: (gdevice: TSDL_GamepadDeviceEvent);
      19: (gaxis: TSDL_GamepadAxisEvent);
      20: (gbutton: TSDL_GamepadButtonEvent);
      21: (gtouchpad: TSDL_GamepadTouchpadEvent);
      22: (gsensor: TSDL_GamepadSensorEvent);
      23: (adevice: TSDL_AudioDeviceEvent);
      24: (cdevice: TSDL_CameraDeviceEvent);
      25: (sensor: TSDL_SensorEvent);
      26: (quit: TSDL_QuitEvent);
      27: (user: TSDL_UserEvent);
      28: (tfinger: TSDL_TouchFingerEvent);
      29: (ptip: TSDL_PenTipEvent);
      30: (pmotion: TSDL_PenMotionEvent);
      31: (pbutton: TSDL_PenButtonEvent);
      32: (drop: TSDL_DropEvent);
      33: (clipboard: TSDL_ClipboardEvent);
      34: (padding: array[0..127] of TUint8);
  end;

procedure SDL_PumpEvents; cdecl; external sdl3_lib;

type
  PSDL_EventAction = ^TSDL_EventAction;
  TSDL_EventAction = longint;

const
  SDL_ADDEVENT = 0;
  SDL_PEEKEVENT = 1;
  SDL_GETEVENT = 2;


function SDL_PeepEvents(events: PSDL_Event; numevents: longint; action: TSDL_EventAction; minType: TUint32; maxType: TUint32): longint; cdecl; external sdl3_lib;
function SDL_HasEvent(_type: TUint32): TSDL_bool; cdecl; external sdl3_lib;
function SDL_HasEvents(minType: TUint32; maxType: TUint32): TSDL_bool; cdecl; external sdl3_lib;
procedure SDL_FlushEvent(_type: TUint32); cdecl; external sdl3_lib;
procedure SDL_FlushEvents(minType: TUint32; maxType: TUint32); cdecl; external sdl3_lib;
function SDL_PollEvent(event: PSDL_Event): TSDL_bool; cdecl; external sdl3_lib;
function SDL_WaitEvent(event: PSDL_Event): TSDL_bool; cdecl; external sdl3_lib;
function SDL_WaitEventTimeout(event: PSDL_Event; timeoutMS: TSint32): TSDL_bool; cdecl; external sdl3_lib;
function SDL_PushEvent(event: PSDL_Event): longint; cdecl; external sdl3_lib;

type
  TSDL_EventFilter = function(userdata: pointer; event: PSDL_Event): longint; cdecl;
  PSDL_EventFilter = ^TSDL_EventFilter;

procedure SDL_SetEventFilter(filter: TSDL_EventFilter; userdata: pointer); cdecl; external sdl3_lib;
function SDL_GetEventFilter(filter: PSDL_EventFilter; userdata: Ppointer): TSDL_bool; cdecl; external sdl3_lib;
function SDL_AddEventWatch(filter: TSDL_EventFilter; userdata: pointer): longint; cdecl; external sdl3_lib;
procedure SDL_DelEventWatch(filter: TSDL_EventFilter; userdata: pointer); cdecl; external sdl3_lib;
procedure SDL_FilterEvents(filter: TSDL_EventFilter; userdata: pointer); cdecl; external sdl3_lib;
procedure SDL_SetEventEnabled(_type: TUint32; Enabled: TSDL_bool); cdecl; external sdl3_lib;
function SDL_EventEnabled(_type: TUint32): TSDL_bool; cdecl; external sdl3_lib;
function SDL_RegisterEvents(numevents: longint): TUint32; cdecl; external sdl3_lib;
function SDL_AllocateEventMemory(size: Tsize_t): pointer; cdecl; external sdl3_lib;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

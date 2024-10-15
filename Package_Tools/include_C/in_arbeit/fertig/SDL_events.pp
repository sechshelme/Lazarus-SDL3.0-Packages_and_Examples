
unit SDL_events;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_events.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_events.h
}

Type
PSDL_EventFilter = ^TSDL_EventFilter;
PSDL_Window = ^TSDL_Window;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


{
  Simple DirectMedia Layer
  Copyright (C) 1997-2024 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
 }
{*
 * # CategoryEvents
 *
 * Event queue management.
  }
{$ifndef SDL_events_h_}
{$define SDL_events_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_audio.h>}
{$include <SDL3/SDL_camera.h>}
{$include <SDL3/SDL_error.h>}
{$include <SDL3/SDL_gamepad.h>}
{$include <SDL3/SDL_joystick.h>}
{$include <SDL3/SDL_keyboard.h>}
{$include <SDL3/SDL_keycode.h>}
{$include <SDL3/SDL_mouse.h>}
{$include <SDL3/SDL_pen.h>}
{$include <SDL3/SDL_power.h>}
{$include <SDL3/SDL_sensor.h>}
{$include <SDL3/SDL_scancode.h>}
{$include <SDL3/SDL_touch.h>}
{$include <SDL3/SDL_video.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
{ General keyboard/mouse/pen state definitions  }
{*
 * The types of events that can be delivered.
 *
 * \since This enum is available since SDL 3.0.0.
  }
{*< Unused (do not remove)  }
{ Application events  }
{*< User-requested quit  }
{ These application events have special meaning on iOS and Android, see README-ios.md and README-android.md for details  }
{*< The application is being terminated by the OS. This event must be handled in a callback set with SDL_AddEventWatch().
                                     Called on iOS in applicationWillTerminate()
                                     Called on Android in onDestroy()
                                 }
{*< The application is low on memory, free memory if possible. This event must be handled in a callback set with SDL_AddEventWatch().
                                     Called on iOS in applicationDidReceiveMemoryWarning()
                                     Called on Android in onTrimMemory()
                                 }
{*< The application is about to enter the background. This event must be handled in a callback set with SDL_AddEventWatch().
                                     Called on iOS in applicationWillResignActive()
                                     Called on Android in onPause()
                                 }
{*< The application did enter the background and may not get CPU for some time. This event must be handled in a callback set with SDL_AddEventWatch().
                                     Called on iOS in applicationDidEnterBackground()
                                     Called on Android in onPause()
                                 }
{*< The application is about to enter the foreground. This event must be handled in a callback set with SDL_AddEventWatch().
                                     Called on iOS in applicationWillEnterForeground()
                                     Called on Android in onResume()
                                 }
{*< The application is now interactive. This event must be handled in a callback set with SDL_AddEventWatch().
                                     Called on iOS in applicationDidBecomeActive()
                                     Called on Android in onResume()
                                 }
{*< The user's locale preferences have changed.  }
{*< The system theme changed  }
{ Display events  }
{ 0x150 was SDL_DISPLAYEVENT, reserve the number for sdl2-compat  }
{*< Display orientation has changed to data1  }
{*< Display has been added to the system  }
{*< Display has been removed from the system  }
{*< Display has changed position  }
{*< Display has changed desktop mode  }
{*< Display has changed current mode  }
{*< Display has changed content scale  }
{ Window events  }
{ 0x200 was SDL_WINDOWEVENT, reserve the number for sdl2-compat  }
{ 0x201 was SDL_EVENT_SYSWM, reserve the number for sdl2-compat  }
{*< Window has been shown  }
{*< Window has been hidden  }
{*< Window has been exposed and should be redrawn, and can be redrawn directly from event watchers for this event  }
{*< Window has been moved to data1, data2  }
{*< Window has been resized to data1xdata2  }
{*< The pixel size of the window has changed to data1xdata2  }
{*< The pixel size of a Metal view associated with the window has changed  }
{*< Window has been minimized  }
{*< Window has been maximized  }
{*< Window has been restored to normal size and position  }
{*< Window has gained mouse focus  }
{*< Window has lost mouse focus  }
{*< Window has gained keyboard focus  }
{*< Window has lost keyboard focus  }
{*< The window manager requests that the window be closed  }
{*< Window had a hit test that wasn't SDL_HITTEST_NORMAL  }
{*< The ICC profile of the window's display has changed  }
{*< Window has been moved to display data1  }
{*< Window display scale has been changed  }
{*< The window safe area has been changed  }
{*< The window has been occluded  }
{*< The window has entered fullscreen mode  }
{*< The window has left fullscreen mode  }
{*< The window with the associated ID is being or has been destroyed. If this message is being handled
                                             in an event watcher, the window handle is still valid and can still be used to retrieve any userdata
                                             associated with the window. Otherwise, the handle has already been destroyed and all resources
                                             associated with it are invalid  }
{*< Window HDR properties have changed  }
{ Keyboard events  }
{*< Key pressed  }
{*< Key released  }
{*< Keyboard text editing (composition)  }
{*< Keyboard text input  }
{*< Keymap changed due to a system event such as an
                                            input language or keyboard layout change.  }
{*< A new keyboard has been inserted into the system  }
{*< A keyboard has been removed  }
{*< Keyboard text editing candidates  }
{ Mouse events  }
{*< Mouse moved  }
{*< Mouse button pressed  }
{*< Mouse button released  }
{*< Mouse wheel motion  }
{*< A new mouse has been inserted into the system  }
{*< A mouse has been removed  }
{ Joystick events  }
{*< Joystick axis motion  }
{*< Joystick trackball motion  }
{*< Joystick hat position change  }
{*< Joystick button pressed  }
{*< Joystick button released  }
{*< A new joystick has been inserted into the system  }
{*< An opened joystick has been removed  }
{*< Joystick battery level change  }
{*< Joystick update is complete  }
{ Gamepad events  }
{*< Gamepad axis motion  }
{*< Gamepad button pressed  }
{*< Gamepad button released  }
{*< A new gamepad has been inserted into the system  }
{*< A gamepad has been removed  }
{*< The gamepad mapping was updated  }
{*< Gamepad touchpad was touched  }
{*< Gamepad touchpad finger was moved  }
{*< Gamepad touchpad finger was lifted  }
{*< Gamepad sensor was updated  }
{*< Gamepad update is complete  }
{*< Gamepad Steam handle has changed  }
{ Touch events  }
{ 0x800, 0x801, and 0x802 were the Gesture events from SDL2. Do not reuse these values! sdl2-compat needs them!  }
{ Clipboard events  }
{*< The clipboard or primary selection changed  }
{ Drag and drop events  }
{*< The system requests a file open  }
{*< text/plain drag-and-drop event  }
{*< A new set of drops is beginning (NULL filename)  }
{*< Current set of drops is now complete (NULL filename)  }
{*< Position while moving over the window  }
{ Audio hotplug events  }
{*< A new audio device is available  }
{*< An audio device has been removed.  }
{*< An audio device's format has been changed by the system.  }
{ Sensor events  }
{*< A sensor was updated  }
{ Pressure-sensitive pen events  }
{*< Pressure-sensitive pen has become available  }
{*< Pressure-sensitive pen has become unavailable  }
{*< Pressure-sensitive pen touched drawing surface  }
{*< Pressure-sensitive pen stopped touching drawing surface  }
{*< Pressure-sensitive pen button pressed  }
{*< Pressure-sensitive pen button released  }
{*< Pressure-sensitive pen is moving on the tablet  }
{*< Pressure-sensitive pen angle/pressure/etc changed  }
{ Camera hotplug events  }
{*< A new camera device is available  }
{*< A camera device has been removed.  }
{*< A camera device has been approved for use by the user.  }
{*< A camera device has been denied for use by the user.  }
{ Render events  }
{*< The render targets have been reset and their contents need to be updated  }
{*< The device has been reset and all textures need to be recreated  }
{ Internal events  }
{*< Signals the end of an event poll cycle  }
{* Events SDL_EVENT_USER through SDL_EVENT_LAST are for your use,
     *  and should be allocated with SDL_RegisterEvents()
      }
{*
     *  This last event is only for bounding internal arrays
      }
{ This just makes sure the enum is the size of Uint32  }
type
  PSDL_EventType = ^TSDL_EventType;
  TSDL_EventType =  Longint;
  Const
    SDL_EVENT_FIRST = &;
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
    SDL_EVENT_DISPLAY_DESKTOP_MODE_CHANGED = 341;
    SDL_EVENT_DISPLAY_CURRENT_MODE_CHANGED = 342;
    SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED = 343;
    SDL_EVENT_DISPLAY_FIRST = SDL_EVENT_DISPLAY_ORIENTATION;
    SDL_EVENT_DISPLAY_LAST = SDL_EVENT_DISPLAY_CONTENT_SCALE_CHANGED;
    SDL_EVENT_WINDOW_SHOWN = $202;
    SDL_EVENT_WINDOW_HIDDEN = 515;
    SDL_EVENT_WINDOW_EXPOSED = 516;
    SDL_EVENT_WINDOW_MOVED = 517;
    SDL_EVENT_WINDOW_RESIZED = 518;
    SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED = 519;
    SDL_EVENT_WINDOW_METAL_VIEW_RESIZED = 520;
    SDL_EVENT_WINDOW_MINIMIZED = 521;
    SDL_EVENT_WINDOW_MAXIMIZED = 522;
    SDL_EVENT_WINDOW_RESTORED = 523;
    SDL_EVENT_WINDOW_MOUSE_ENTER = 524;
    SDL_EVENT_WINDOW_MOUSE_LEAVE = 525;
    SDL_EVENT_WINDOW_FOCUS_GAINED = 526;
    SDL_EVENT_WINDOW_FOCUS_LOST = 527;
    SDL_EVENT_WINDOW_CLOSE_REQUESTED = 528;
    SDL_EVENT_WINDOW_HIT_TEST = 529;
    SDL_EVENT_WINDOW_ICCPROF_CHANGED = 530;
    SDL_EVENT_WINDOW_DISPLAY_CHANGED = 531;
    SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED = 532;
    SDL_EVENT_WINDOW_SAFE_AREA_CHANGED = 533;
    SDL_EVENT_WINDOW_OCCLUDED = 534;
    SDL_EVENT_WINDOW_ENTER_FULLSCREEN = 535;
    SDL_EVENT_WINDOW_LEAVE_FULLSCREEN = 536;
    SDL_EVENT_WINDOW_DESTROYED = 537;
    SDL_EVENT_WINDOW_HDR_STATE_CHANGED = 538;
    SDL_EVENT_WINDOW_FIRST = SDL_EVENT_WINDOW_SHOWN;
    SDL_EVENT_WINDOW_LAST = SDL_EVENT_WINDOW_HDR_STATE_CHANGED;
    SDL_EVENT_KEY_DOWN = $300;
    SDL_EVENT_KEY_UP = 769;
    SDL_EVENT_TEXT_EDITING = 770;
    SDL_EVENT_TEXT_INPUT = 771;
    SDL_EVENT_KEYMAP_CHANGED = 772;
    SDL_EVENT_KEYBOARD_ADDED = 773;
    SDL_EVENT_KEYBOARD_REMOVED = 774;
    SDL_EVENT_TEXT_EDITING_CANDIDATES = 775;
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
    SDL_EVENT_PEN_PROXIMITY_IN = $1300;
    SDL_EVENT_PEN_PROXIMITY_OUT = 4865;
    SDL_EVENT_PEN_DOWN = 4866;
    SDL_EVENT_PEN_UP = 4867;
    SDL_EVENT_PEN_BUTTON_DOWN = 4868;
    SDL_EVENT_PEN_BUTTON_UP = 4869;
    SDL_EVENT_PEN_MOTION = 4870;
    SDL_EVENT_PEN_AXIS = 4871;
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
;
{*
 * Fields shared by every event
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< Event type, shared with all events, Uint32 to cover user events which are not in the SDL_EventType enumeration  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
type
  PSDL_CommonEvent = ^TSDL_CommonEvent;
  TSDL_CommonEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
    end;
{*
 * Display state change event data (event.display.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_DISPLAYEVENT_*  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The associated display  }
{*< event dependent data  }
{*< event dependent data  }

  PSDL_DisplayEvent = ^TSDL_DisplayEvent;
  TSDL_DisplayEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      displayID : TSDL_DisplayID;
      data1 : TSint32;
      data2 : TSint32;
    end;
{*
 * Window state change event data (event.window.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_WINDOW_*  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The associated window  }
{*< event dependent data  }
{*< event dependent data  }

  PSDL_WindowEvent = ^TSDL_WindowEvent;
  TSDL_WindowEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      data1 : TSint32;
      data2 : TSint32;
    end;
{*
 * Keyboard device event structure (event.kdevice.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_KEYBOARD_ADDED or SDL_EVENT_KEYBOARD_REMOVED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The keyboard instance id  }

  PSDL_KeyboardDeviceEvent = ^TSDL_KeyboardDeviceEvent;
  TSDL_KeyboardDeviceEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_KeyboardID;
    end;
{*
 * Keyboard button event structure (event.key.*)
 *
 * The `key` is the base SDL_Keycode generated by pressing the `scancode`
 * using the current keyboard layout, applying any options specified in
 * SDL_HINT_KEYCODE_OPTIONS. You can get the SDL_Keycode corresponding to the
 * event scancode and modifiers directly from the keyboard layout, bypassing
 * SDL_HINT_KEYCODE_OPTIONS, by calling SDL_GetKeyFromScancode().
 *
 * \since This struct is available since SDL 3.0.0.
 *
 * \sa SDL_GetKeyFromScancode
 * \sa SDL_HINT_KEYCODE_OPTIONS
  }
{*< SDL_EVENT_KEY_DOWN or SDL_EVENT_KEY_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with keyboard focus, if any  }
{*< The keyboard instance id, or 0 if unknown or virtual  }
{*< SDL physical key code  }
{*< SDL virtual key code  }
{*< current key modifiers  }
{*< The platform dependent scancode for this event  }
{*< true if the key is pressed  }
{*< true if this is a key repeat  }

  PSDL_KeyboardEvent = ^TSDL_KeyboardEvent;
  TSDL_KeyboardEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_KeyboardID;
      scancode : TSDL_Scancode;
      key : TSDL_Keycode;
      mod : TSDL_Keymod;
      raw : TUint16;
      down : Tbool;
      _repeat : Tbool;
    end;
{*
 * Keyboard text editing event structure (event.edit.*)
 *
 * The start cursor is the position, in UTF-8 characters, where new typing
 * will be inserted into the editing text. The length is the number of UTF-8
 * characters that will be replaced by new typing.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_TEXT_EDITING  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with keyboard focus, if any  }
(* Const before declarator ignored *)
{*< The editing text  }
{*< The start cursor of selected editing text, or -1 if not set  }
{*< The length of selected editing text, or -1 if not set  }

  PSDL_TextEditingEvent = ^TSDL_TextEditingEvent;
  TSDL_TextEditingEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      text : Pansichar;
      start : TSint32;
      length : TSint32;
    end;
{*
 * Keyboard IME candidates event structure (event.edit_candidates.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_TEXT_EDITING_CANDIDATES  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with keyboard focus, if any  }
(* Const before declarator ignored *)
(* Const before declarator ignored *)
{*< The list of candidates, or NULL if there are no candidates available  }
{*< The number of strings in `candidates`  }
{*< The index of the selected candidate, or -1 if no candidate is selected  }
{*< true if the list is horizontal, false if it's vertical  }

  PSDL_TextEditingCandidatesEvent = ^TSDL_TextEditingCandidatesEvent;
  TSDL_TextEditingCandidatesEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      candidates : ^Pansichar;
      num_candidates : TSint32;
      selected_candidate : TSint32;
      horizontal : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{*
 * Keyboard text input event structure (event.text.*)
 *
 * This event will never be delivered unless text input is enabled by calling
 * SDL_StartTextInput(). Text input is disabled by default!
 *
 * \since This struct is available since SDL 3.0.0.
 *
 * \sa SDL_StartTextInput
 * \sa SDL_StopTextInput
  }
{*< SDL_EVENT_TEXT_INPUT  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with keyboard focus, if any  }
(* Const before declarator ignored *)
{*< The input text, UTF-8 encoded  }

  PSDL_TextInputEvent = ^TSDL_TextInputEvent;
  TSDL_TextInputEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      text : Pansichar;
    end;
{*
 * Mouse device event structure (event.mdevice.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_MOUSE_ADDED or SDL_EVENT_MOUSE_REMOVED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The mouse instance id  }

  PSDL_MouseDeviceEvent = ^TSDL_MouseDeviceEvent;
  TSDL_MouseDeviceEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_MouseID;
    end;
{*
 * Mouse motion event structure (event.motion.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_MOUSE_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with mouse focus, if any  }
{*< The mouse instance id or SDL_TOUCH_MOUSEID  }
{*< The current button state  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }
{*< The relative motion in the X direction  }
{*< The relative motion in the Y direction  }

  PSDL_MouseMotionEvent = ^TSDL_MouseMotionEvent;
  TSDL_MouseMotionEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_MouseID;
      state : TSDL_MouseButtonFlags;
      x : single;
      y : single;
      xrel : single;
      yrel : single;
    end;
{*
 * Mouse button event structure (event.button.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_MOUSE_BUTTON_DOWN or SDL_EVENT_MOUSE_BUTTON_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with mouse focus, if any  }
{*< The mouse instance id, SDL_TOUCH_MOUSEID  }
{*< The mouse button index  }
{*< true if the button is pressed  }
{*< 1 for single-click, 2 for double-click, etc.  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }

  PSDL_MouseButtonEvent = ^TSDL_MouseButtonEvent;
  TSDL_MouseButtonEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_MouseID;
      button : TUint8;
      down : Tbool;
      clicks : TUint8;
      padding : TUint8;
      x : single;
      y : single;
    end;
{*
 * Mouse wheel event structure (event.wheel.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_MOUSE_WHEEL  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with mouse focus, if any  }
{*< The mouse instance id, SDL_TOUCH_MOUSEID  }
{*< The amount scrolled horizontally, positive to the right and negative to the left  }
{*< The amount scrolled vertically, positive away from the user and negative toward the user  }
{*< Set to one of the SDL_MOUSEWHEEL_* defines. When FLIPPED the values in X and Y will be opposite. Multiply by -1 to change them back  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }

  PSDL_MouseWheelEvent = ^TSDL_MouseWheelEvent;
  TSDL_MouseWheelEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_MouseID;
      x : single;
      y : single;
      direction : TSDL_MouseWheelDirection;
      mouse_x : single;
      mouse_y : single;
    end;
{*
 * Joystick axis motion event structure (event.jaxis.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_JOYSTICK_AXIS_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The joystick axis index  }
{*< The axis value (range: -32768 to 32767)  }

  PSDL_JoyAxisEvent = ^TSDL_JoyAxisEvent;
  TSDL_JoyAxisEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      axis : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
      value : TSint16;
      padding4 : TUint16;
    end;
{*
 * Joystick trackball motion event structure (event.jball.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_JOYSTICK_BALL_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The joystick trackball index  }
{*< The relative motion in the X direction  }
{*< The relative motion in the Y direction  }

  PSDL_JoyBallEvent = ^TSDL_JoyBallEvent;
  TSDL_JoyBallEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      ball : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
      xrel : TSint16;
      yrel : TSint16;
    end;
{*
 * Joystick hat position change event structure (event.jhat.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_JOYSTICK_HAT_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The joystick hat index  }
{*< The hat position value.
                         *   \sa SDL_HAT_LEFTUP SDL_HAT_UP SDL_HAT_RIGHTUP
                         *   \sa SDL_HAT_LEFT SDL_HAT_CENTERED SDL_HAT_RIGHT
                         *   \sa SDL_HAT_LEFTDOWN SDL_HAT_DOWN SDL_HAT_RIGHTDOWN
                         *
                         *   Note that zero means the POV is centered.
                          }

  PSDL_JoyHatEvent = ^TSDL_JoyHatEvent;
  TSDL_JoyHatEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      hat : TUint8;
      value : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 * Joystick button event structure (event.jbutton.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_JOYSTICK_BUTTON_DOWN or SDL_EVENT_JOYSTICK_BUTTON_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The joystick button index  }
{*< true if the button is pressed  }

  PSDL_JoyButtonEvent = ^TSDL_JoyButtonEvent;
  TSDL_JoyButtonEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      button : TUint8;
      down : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 * Joystick device event structure (event.jdevice.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_JOYSTICK_ADDED or SDL_EVENT_JOYSTICK_REMOVED or SDL_EVENT_JOYSTICK_UPDATE_COMPLETE  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }

  PSDL_JoyDeviceEvent = ^TSDL_JoyDeviceEvent;
  TSDL_JoyDeviceEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
    end;
{*
 * Joysick battery level change event structure (event.jbattery.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_JOYSTICK_BATTERY_UPDATED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The joystick battery state  }
{*< The joystick battery percent charge remaining  }

  PSDL_JoyBatteryEvent = ^TSDL_JoyBatteryEvent;
  TSDL_JoyBatteryEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      state : TSDL_PowerState;
      percent : longint;
    end;
{*
 * Gamepad axis motion event structure (event.gaxis.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_GAMEPAD_AXIS_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The gamepad axis (SDL_GamepadAxis)  }
{*< The axis value (range: -32768 to 32767)  }

  PSDL_GamepadAxisEvent = ^TSDL_GamepadAxisEvent;
  TSDL_GamepadAxisEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      axis : TUint8;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
      value : TSint16;
      padding4 : TUint16;
    end;
{*
 * Gamepad button event structure (event.gbutton.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_GAMEPAD_BUTTON_DOWN or SDL_EVENT_GAMEPAD_BUTTON_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The gamepad button (SDL_GamepadButton)  }
{*< true if the button is pressed  }

  PSDL_GamepadButtonEvent = ^TSDL_GamepadButtonEvent;
  TSDL_GamepadButtonEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      button : TUint8;
      down : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
    end;
{*
 * Gamepad device event structure (event.gdevice.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_GAMEPAD_ADDED, SDL_EVENT_GAMEPAD_REMOVED, or SDL_EVENT_GAMEPAD_REMAPPED, SDL_EVENT_GAMEPAD_UPDATE_COMPLETE or SDL_EVENT_GAMEPAD_STEAM_HANDLE_UPDATED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }

  PSDL_GamepadDeviceEvent = ^TSDL_GamepadDeviceEvent;
  TSDL_GamepadDeviceEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
    end;
{*
 * Gamepad touchpad event structure (event.gtouchpad.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_GAMEPAD_TOUCHPAD_DOWN or SDL_EVENT_GAMEPAD_TOUCHPAD_MOTION or SDL_EVENT_GAMEPAD_TOUCHPAD_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The index of the touchpad  }
{*< The index of the finger on the touchpad  }
{*< Normalized in the range 0...1 with 0 being on the left  }
{*< Normalized in the range 0...1 with 0 being at the top  }
{*< Normalized in the range 0...1  }

  PSDL_GamepadTouchpadEvent = ^TSDL_GamepadTouchpadEvent;
  TSDL_GamepadTouchpadEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      touchpad : TSint32;
      finger : TSint32;
      x : single;
      y : single;
      pressure : single;
    end;
{*
 * Gamepad sensor event structure (event.gsensor.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_GAMEPAD_SENSOR_UPDATE  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The joystick instance id  }
{*< The type of the sensor, one of the values of SDL_SensorType  }
{*< Up to 3 values from the sensor, as defined in SDL_sensor.h  }
{*< The timestamp of the sensor reading in nanoseconds, not necessarily synchronized with the system clock  }

  PSDL_GamepadSensorEvent = ^TSDL_GamepadSensorEvent;
  TSDL_GamepadSensorEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_JoystickID;
      sensor : TSint32;
      data : array[0..2] of single;
      sensor_timestamp : TUint64;
    end;
{*
 * Audio device event structure (event.adevice.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_AUDIO_DEVICE_ADDED, or SDL_EVENT_AUDIO_DEVICE_REMOVED, or SDL_EVENT_AUDIO_DEVICE_FORMAT_CHANGED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< SDL_AudioDeviceID for the device being added or removed or changing  }
{*< false if a playback device, true if a recording device.  }

  PSDL_AudioDeviceEvent = ^TSDL_AudioDeviceEvent;
  TSDL_AudioDeviceEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_AudioDeviceID;
      recording : Tbool;
      padding1 : TUint8;
      padding2 : TUint8;
      padding3 : TUint8;
    end;
{*
 * Camera device event structure (event.cdevice.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_CAMERA_DEVICE_ADDED, SDL_EVENT_CAMERA_DEVICE_REMOVED, SDL_EVENT_CAMERA_DEVICE_APPROVED, SDL_EVENT_CAMERA_DEVICE_DENIED  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< SDL_CameraID for the device being added or removed or changing  }

  PSDL_CameraDeviceEvent = ^TSDL_CameraDeviceEvent;
  TSDL_CameraDeviceEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_CameraID;
    end;
{*
 * Touch finger event structure (event.tfinger.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_FINGER_MOTION or SDL_EVENT_FINGER_DOWN or SDL_EVENT_FINGER_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The touch device id  }
{*< Normalized in the range 0...1  }
{*< Normalized in the range 0...1  }
{*< Normalized in the range -1...1  }
{*< Normalized in the range -1...1  }
{*< Normalized in the range 0...1  }
{*< The window underneath the finger, if any  }

  PSDL_TouchFingerEvent = ^TSDL_TouchFingerEvent;
  TSDL_TouchFingerEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      touchID : TSDL_TouchID;
      fingerID : TSDL_FingerID;
      x : single;
      y : single;
      dx : single;
      dy : single;
      pressure : single;
      windowID : TSDL_WindowID;
    end;
{*
 * Pressure-sensitive pen proximity event structure (event.pmotion.*)
 *
 * When a pen becomes visible to the system (it is close enough to a tablet,
 * etc), SDL will send an SDL_EVENT_PEN_PROXIMITY_IN event with the new pen's
 * ID. This ID is valid until the pen leaves proximity again (has been removed
 * from the tablet's area, the tablet has been unplugged, etc). If the same
 * pen reenters proximity again, it will be given a new ID.
 *
 * Note that "proximity" means "close enough for the tablet to know the tool
 * is there." The pen touching and lifting off from the tablet while not
 * leaving the area are handled by SDL_EVENT_PEN_DOWN and SDL_EVENT_PEN_UP.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_PEN_PROXIMITY_IN or SDL_EVENT_PEN_PROXIMITY_OUT  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with mouse focus, if any  }
{*< The pen instance id  }

  PSDL_PenProximityEvent = ^TSDL_PenProximityEvent;
  TSDL_PenProximityEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_PenID;
    end;
{*
 * Pressure-sensitive pen motion event structure (event.pmotion.*)
 *
 * Depending on the hardware, you may get motion events when the pen is not
 * touching a tablet, for tracking a pen even when it isn't drawing. You
 * should listen for SDL_EVENT_PEN_DOWN and SDL_EVENT_PEN_UP events, or check
 * `pen_state & SDL_PEN_INPUT_DOWN` to decide if a pen is "drawing" when
 * dealing with pen motion.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_PEN_MOTION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with mouse focus, if any  }
{*< The pen instance id  }
{*< Complete pen input state at time of event  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }

  PSDL_PenMotionEvent = ^TSDL_PenMotionEvent;
  TSDL_PenMotionEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_PenID;
      pen_state : TSDL_PenInputFlags;
      x : single;
      y : single;
    end;
{*
 * Pressure-sensitive pen touched event structure (event.ptouch.*)
 *
 * These events come when a pen touches a surface (a tablet, etc), or lifts
 * off from one.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_PEN_DOWN or SDL_EVENT_PEN_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with pen focus, if any  }
{*< The pen instance id  }
{*< Complete pen input state at time of event  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }
{*< true if eraser end is used (not all pens support this).  }
{*< true if the pen is touching or false if the pen is lifted off  }

  PSDL_PenTouchEvent = ^TSDL_PenTouchEvent;
  TSDL_PenTouchEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_PenID;
      pen_state : TSDL_PenInputFlags;
      x : single;
      y : single;
      eraser : Tbool;
      down : Tbool;
    end;
{*
 * Pressure-sensitive pen button event structure (event.pbutton.*)
 *
 * This is for buttons on the pen itself that the user might click. The pen
 * itself pressing down to draw triggers a SDL_EVENT_PEN_DOWN event instead.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_PEN_BUTTON_DOWN or SDL_EVENT_PEN_BUTTON_UP  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with mouse focus, if any  }
{*< The pen instance id  }
{*< Complete pen input state at time of event  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }
{*< The pen button index (first button is 1).  }
{*< true if the button is pressed  }

  PSDL_PenButtonEvent = ^TSDL_PenButtonEvent;
  TSDL_PenButtonEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_PenID;
      pen_state : TSDL_PenInputFlags;
      x : single;
      y : single;
      button : TUint8;
      down : Tbool;
    end;
{*
 * Pressure-sensitive pen pressure / angle event structure (event.paxis.*)
 *
 * You might get some of these events even if the pen isn't touching the
 * tablet.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_PEN_AXIS  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window with pen focus, if any  }
{*< The pen instance id  }
{*< Complete pen input state at time of event  }
{*< X coordinate, relative to window  }
{*< Y coordinate, relative to window  }
{*< Axis that has changed  }
{*< New value of axis  }

  PSDL_PenAxisEvent = ^TSDL_PenAxisEvent;
  TSDL_PenAxisEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      which : TSDL_PenID;
      pen_state : TSDL_PenInputFlags;
      x : single;
      y : single;
      axis : TSDL_PenAxis;
      value : single;
    end;
{*
 * An event used to drop text or request a file open by the system
 * (event.drop.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_DROP_BEGIN or SDL_EVENT_DROP_FILE or SDL_EVENT_DROP_TEXT or SDL_EVENT_DROP_COMPLETE or SDL_EVENT_DROP_POSITION  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The window that was dropped on, if any  }
{*< X coordinate, relative to window (not on begin)  }
{*< Y coordinate, relative to window (not on begin)  }
(* Const before declarator ignored *)
{*< The source app that sent this drop event, or NULL if that isn't available  }
(* Const before declarator ignored *)
{*< The text for SDL_EVENT_DROP_TEXT and the file name for SDL_EVENT_DROP_FILE, NULL for other events  }

  PSDL_DropEvent = ^TSDL_DropEvent;
  TSDL_DropEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      x : single;
      y : single;
      source : Pansichar;
      data : Pansichar;
    end;
{*
 * An event triggered when the clipboard contents have changed
 * (event.clipboard.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_CLIPBOARD_UPDATE  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }

  PSDL_ClipboardEvent = ^TSDL_ClipboardEvent;
  TSDL_ClipboardEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
    end;
{*
 * Sensor event structure (event.sensor.*)
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_SENSOR_UPDATE  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The instance ID of the sensor  }
{*< Up to 6 values from the sensor - additional values can be queried using SDL_GetSensorData()  }
{*< The timestamp of the sensor reading in nanoseconds, not necessarily synchronized with the system clock  }

  PSDL_SensorEvent = ^TSDL_SensorEvent;
  TSDL_SensorEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
      which : TSDL_SensorID;
      data : array[0..5] of single;
      sensor_timestamp : TUint64;
    end;
{*
 * The "quit requested" event
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_QUIT  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }

  PSDL_QuitEvent = ^TSDL_QuitEvent;
  TSDL_QuitEvent = record
      _type : TSDL_EventType;
      reserved : TUint32;
      timestamp : TUint64;
    end;
{*
 * A user-defined event type (event.user.*)
 *
 * This event is unique; it is never created by SDL, but only by the
 * application. The event can be pushed onto the event queue using
 * SDL_PushEvent(). The contents of the structure members are completely up to
 * the programmer; the only requirement is that '''type''' is a value obtained
 * from SDL_RegisterEvents().
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< SDL_EVENT_USER through SDL_EVENT_LAST-1, Uint32 because these are not in the SDL_EventType enumeration  }
{*< In nanoseconds, populated using SDL_GetTicksNS()  }
{*< The associated window if any  }
{*< User defined event code  }
{*< User defined data pointer  }
{*< User defined data pointer  }

  PSDL_UserEvent = ^TSDL_UserEvent;
  TSDL_UserEvent = record
      _type : TUint32;
      reserved : TUint32;
      timestamp : TUint64;
      windowID : TSDL_WindowID;
      code : TSint32;
      data1 : pointer;
      data2 : pointer;
    end;
{*
 * The structure for all events in SDL.
 *
 * \since This struct is available since SDL 3.0.0.
  }
{*< Event type, shared with all events, Uint32 to cover user events which are not in the SDL_EventType enumeration  }
{*< Common event data  }
{*< Display event data  }
{*< Window event data  }
{*< Keyboard device change event data  }
{*< Keyboard event data  }
{*< Text editing event data  }
{*< Text editing candidates event data  }
{*< Text input event data  }
{*< Mouse device change event data  }
{*< Mouse motion event data  }
{*< Mouse button event data  }
{*< Mouse wheel event data  }
{*< Joystick device change event data  }
{*< Joystick axis event data  }
{*< Joystick ball event data  }
{*< Joystick hat event data  }
{*< Joystick button event data  }
{*< Joystick battery event data  }
{*< Gamepad device event data  }
{*< Gamepad axis event data  }
{*< Gamepad button event data  }
{*< Gamepad touchpad event data  }
{*< Gamepad sensor event data  }
{*< Audio device event data  }
{*< Camera device event data  }
{*< Sensor event data  }
{*< Quit request event data  }
{*< Custom event data  }
{*< Touch finger event data  }
{*< Pen proximity event data  }
{*< Pen tip touching event data  }
{*< Pen motion event data  }
{*< Pen button event data  }
{*< Pen axis event data  }
{*< Drag and drop event data  }
{*< Clipboard event data  }
{ This is necessary for ABI compatibility between Visual C++ and GCC.
       Visual C++ will respect the push pack pragma and use 52 bytes (size of
       SDL_TextEditingEvent, the largest structure for 32-bit and 64-bit
       architectures) for this union, and GCC will use the alignment of the
       largest datatype within the union, which is 8 bytes on 64-bit
       architectures.

       So... we'll add padding to force the size to be the same for both.

       On architectures where pointers are 16 bytes, this needs rounding up to
       the next multiple of 16, 64, and on architectures where pointers are
       even larger the size of SDL_UserEvent will dominate as being 3 pointers.
     }

  PSDL_Event = ^TSDL_Event;
  TSDL_Event = record
      case longint of
        0 : ( _type : TUint32 );
        1 : ( common : TSDL_CommonEvent );
        2 : ( display : TSDL_DisplayEvent );
        3 : ( window : TSDL_WindowEvent );
        4 : ( kdevice : TSDL_KeyboardDeviceEvent );
        5 : ( key : TSDL_KeyboardEvent );
        6 : ( edit : TSDL_TextEditingEvent );
        7 : ( edit_candidates : TSDL_TextEditingCandidatesEvent );
        8 : ( text : TSDL_TextInputEvent );
        9 : ( mdevice : TSDL_MouseDeviceEvent );
        10 : ( motion : TSDL_MouseMotionEvent );
        11 : ( button : TSDL_MouseButtonEvent );
        12 : ( wheel : TSDL_MouseWheelEvent );
        13 : ( jdevice : TSDL_JoyDeviceEvent );
        14 : ( jaxis : TSDL_JoyAxisEvent );
        15 : ( jball : TSDL_JoyBallEvent );
        16 : ( jhat : TSDL_JoyHatEvent );
        17 : ( jbutton : TSDL_JoyButtonEvent );
        18 : ( jbattery : TSDL_JoyBatteryEvent );
        19 : ( gdevice : TSDL_GamepadDeviceEvent );
        20 : ( gaxis : TSDL_GamepadAxisEvent );
        21 : ( gbutton : TSDL_GamepadButtonEvent );
        22 : ( gtouchpad : TSDL_GamepadTouchpadEvent );
        23 : ( gsensor : TSDL_GamepadSensorEvent );
        24 : ( adevice : TSDL_AudioDeviceEvent );
        25 : ( cdevice : TSDL_CameraDeviceEvent );
        26 : ( sensor : TSDL_SensorEvent );
        27 : ( quit : TSDL_QuitEvent );
        28 : ( user : TSDL_UserEvent );
        29 : ( tfinger : TSDL_TouchFingerEvent );
        30 : ( pproximity : TSDL_PenProximityEvent );
        31 : ( ptouch : TSDL_PenTouchEvent );
        32 : ( pmotion : TSDL_PenMotionEvent );
        33 : ( pbutton : TSDL_PenButtonEvent );
        34 : ( paxis : TSDL_PenAxisEvent );
        35 : ( drop : TSDL_DropEvent );
        36 : ( clipboard : TSDL_ClipboardEvent );
        37 : ( padding : array[0..127] of TUint8 );
      end;
{ Make sure we haven't broken binary compatibility  }
{SDL_COMPILE_TIME_ASSERT(SDL_Event, sizeof(SDL_Event) == sizeof(((SDL_Event *)NULL)->padding)); }
{ Function prototypes  }
{*
 * Pump the event loop, gathering events from the input devices.
 *
 * This function updates the event queue and internal input device state.
 *
 * **WARNING**: This should only be run in the thread that initialized the
 * video subsystem, and for extra safety, you should consider only doing those
 * things on the main thread in any case.
 *
 * SDL_PumpEvents() gathers all the pending input information from devices and
 * places it in the event queue. Without calls to SDL_PumpEvents() no events
 * would ever be placed on the queue. Often the need for calls to
 * SDL_PumpEvents() is hidden from the user since SDL_PollEvent() and
 * SDL_WaitEvent() implicitly call SDL_PumpEvents(). However, if you are not
 * polling or waiting for events (e.g. you are filtering them), then you must
 * call SDL_PumpEvents() to force an event queue update.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PollEvent
 * \sa SDL_WaitEvent
  }

procedure SDL_PumpEvents;cdecl;external;
{ @  }
{*
 * The type of action to request from SDL_PeepEvents().
 *
 * \since This enum is available since SDL 3.0.0.
  }
{*< Add events to the back of the queue.  }
{*< Check but don't remove events from the queue front.  }
{*< Retrieve/remove events from the front of the queue.  }
type
  PSDL_EventAction = ^TSDL_EventAction;
  TSDL_EventAction =  Longint;
  Const
    SDL_ADDEVENT = 0;
    SDL_PEEKEVENT = 1;
    SDL_GETEVENT = 2;
;
{*
 * Check the event queue for messages and optionally return them.
 *
 * `action` may be any of the following:
 *
 * - `SDL_ADDEVENT`: up to `numevents` events will be added to the back of the
 *   event queue.
 * - `SDL_PEEKEVENT`: `numevents` events at the front of the event queue,
 *   within the specified minimum and maximum type, will be returned to the
 *   caller and will _not_ be removed from the queue. If you pass NULL for
 *   `events`, then `numevents` is ignored and the total number of matching
 *   events will be returned.
 * - `SDL_GETEVENT`: up to `numevents` events at the front of the event queue,
 *   within the specified minimum and maximum type, will be returned to the
 *   caller and will be removed from the queue.
 *
 * You may have to call SDL_PumpEvents() before calling this function.
 * Otherwise, the events may not be ready to be filtered when you call
 * SDL_PeepEvents().
 *
 * This function is thread-safe.
 *
 * \param events destination buffer for the retrieved events, may be NULL to
 *               leave the events in the queue and return the number of events
 *               that would have been stored.
 * \param numevents if action is SDL_ADDEVENT, the number of events to add
 *                  back to the event queue; if action is SDL_PEEKEVENT or
 *                  SDL_GETEVENT, the maximum number of events to retrieve.
 * \param action action to take; see [[#action|Remarks]] for details.
 * \param minType minimum value of the event type to be considered;
 *                SDL_EVENT_FIRST is a safe choice.
 * \param maxType maximum value of the event type to be considered;
 *                SDL_EVENT_LAST is a safe choice.
 * \returns the number of events actually stored or -1 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PollEvent
 * \sa SDL_PumpEvents
 * \sa SDL_PushEvent
  }

function SDL_PeepEvents(events:PSDL_Event; numevents:longint; action:TSDL_EventAction; minType:TUint32; maxType:TUint32):longint;cdecl;external;
{ @  }
{*
 * Check for the existence of a certain event type in the event queue.
 *
 * If you need to check for a range of event types, use SDL_HasEvents()
 * instead.
 *
 * \param type the type of event to be queried; see SDL_EventType for details.
 * \returns true if events matching `type` are present, or false if events
 *          matching `type` are not present.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasEvents
  }
function SDL_HasEvent(_type:TUint32):Tbool;cdecl;external;
{*
 * Check for the existence of certain event types in the event queue.
 *
 * If you need to check for a single event type, use SDL_HasEvent() instead.
 *
 * \param minType the low end of event type to be queried, inclusive; see
 *                SDL_EventType for details.
 * \param maxType the high end of event type to be queried, inclusive; see
 *                SDL_EventType for details.
 * \returns true if events with type >= `minType` and <= `maxType` are
 *          present, or false if not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_HasEvents
  }
function SDL_HasEvents(minType:TUint32; maxType:TUint32):Tbool;cdecl;external;
{*
 * Clear events of a specific type from the event queue.
 *
 * This will unconditionally remove any events from the queue that match
 * `type`. If you need to remove a range of event types, use SDL_FlushEvents()
 * instead.
 *
 * It's also normal to just ignore events you don't care about in your event
 * loop without calling this function.
 *
 * This function only affects currently queued events. If you want to make
 * sure that all pending OS events are flushed, you can call SDL_PumpEvents()
 * on the main thread immediately before the flush call.
 *
 * If you have user events with custom data that needs to be freed, you should
 * use SDL_PeepEvents() to remove and clean up those events before calling
 * this function.
 *
 * \param type the type of event to be cleared; see SDL_EventType for details.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_FlushEvents
  }
procedure SDL_FlushEvent(_type:TUint32);cdecl;external;
{*
 * Clear events of a range of types from the event queue.
 *
 * This will unconditionally remove any events from the queue that are in the
 * range of `minType` to `maxType`, inclusive. If you need to remove a single
 * event type, use SDL_FlushEvent() instead.
 *
 * It's also normal to just ignore events you don't care about in your event
 * loop without calling this function.
 *
 * This function only affects currently queued events. If you want to make
 * sure that all pending OS events are flushed, you can call SDL_PumpEvents()
 * on the main thread immediately before the flush call.
 *
 * \param minType the low end of event type to be cleared, inclusive; see
 *                SDL_EventType for details.
 * \param maxType the high end of event type to be cleared, inclusive; see
 *                SDL_EventType for details.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_FlushEvent
  }
procedure SDL_FlushEvents(minType:TUint32; maxType:TUint32);cdecl;external;
{*
 * Poll for currently pending events.
 *
 * If `event` is not NULL, the next event is removed from the queue and stored
 * in the SDL_Event structure pointed to by `event`. The 1 returned refers to
 * this event, immediately stored in the SDL Event structure -- not an event
 * to follow.
 *
 * If `event` is NULL, it simply returns 1 if there is an event in the queue,
 * but will not remove it from the queue.
 *
 * As this function may implicitly call SDL_PumpEvents(), you can only call
 * this function in the thread that set the video mode.
 *
 * SDL_PollEvent() is the favored way of receiving system events since it can
 * be done from the main loop and does not suspend the main loop while waiting
 * on an event to be posted.
 *
 * The common practice is to fully process the event queue once every frame,
 * usually as a first step before updating the game's state:
 *
 * ```c
 * while (game_is_still_running) 
 *     SDL_Event event;
 *     while (SDL_PollEvent(&event))   // poll until all events are handled!
 *         // decide what to do with this event.
 *     
 *
 *     // update game state, draw the current frame
 * 
 * ```
 *
 * \param event the SDL_Event structure to be filled with the next event from
 *              the queue, or NULL.
 * \returns true if this got an event or false if there are none available.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PushEvent
 * \sa SDL_WaitEvent
 * \sa SDL_WaitEventTimeout
  }
function SDL_PollEvent(event:PSDL_Event):Tbool;cdecl;external;
{*
 * Wait indefinitely for the next available event.
 *
 * If `event` is not NULL, the next event is removed from the queue and stored
 * in the SDL_Event structure pointed to by `event`.
 *
 * As this function may implicitly call SDL_PumpEvents(), you can only call
 * this function in the thread that initialized the video subsystem.
 *
 * \param event the SDL_Event structure to be filled in with the next event
 *              from the queue, or NULL.
 * \returns true on success or false if there was an error while waiting for
 *          events; call SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PollEvent
 * \sa SDL_PushEvent
 * \sa SDL_WaitEventTimeout
  }
function SDL_WaitEvent(event:PSDL_Event):Tbool;cdecl;external;
{*
 * Wait until the specified timeout (in milliseconds) for the next available
 * event.
 *
 * If `event` is not NULL, the next event is removed from the queue and stored
 * in the SDL_Event structure pointed to by `event`.
 *
 * As this function may implicitly call SDL_PumpEvents(), you can only call
 * this function in the thread that initialized the video subsystem.
 *
 * The timeout is not guaranteed, the actual wait time could be longer due to
 * system scheduling.
 *
 * \param event the SDL_Event structure to be filled in with the next event
 *              from the queue, or NULL.
 * \param timeoutMS the maximum number of milliseconds to wait for the next
 *                  available event.
 * \returns true if this got an event or false if the timeout elapsed without
 *          any events available.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PollEvent
 * \sa SDL_PushEvent
 * \sa SDL_WaitEvent
  }
function SDL_WaitEventTimeout(event:PSDL_Event; timeoutMS:TSint32):Tbool;cdecl;external;
{*
 * Add an event to the event queue.
 *
 * The event queue can actually be used as a two way communication channel.
 * Not only can events be read from the queue, but the user can also push
 * their own events onto it. `event` is a pointer to the event structure you
 * wish to push onto the queue. The event is copied into the queue, and the
 * caller may dispose of the memory pointed to after SDL_PushEvent() returns.
 *
 * Note: Pushing device input events onto the queue doesn't modify the state
 * of the device within SDL.
 *
 * This function is thread-safe, and can be called from other threads safely.
 *
 * Note: Events pushed onto the queue with SDL_PushEvent() get passed through
 * the event filter but events added with SDL_PeepEvents() do not.
 *
 * For pushing application-specific events, please use SDL_RegisterEvents() to
 * get an event type that does not conflict with other code that also wants
 * its own custom event types.
 *
 * \param event the SDL_Event to be added to the queue.
 * \returns true on success, false if the event was filtered or on failure;
 *          call SDL_GetError() for more information. A common reason for
 *          error is the event queue being full.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PeepEvents
 * \sa SDL_PollEvent
 * \sa SDL_RegisterEvents
  }
function SDL_PushEvent(event:PSDL_Event):Tbool;cdecl;external;
{*
 * A function pointer used for callbacks that watch the event queue.
 *
 * \param userdata what was passed as `userdata` to SDL_SetEventFilter() or
 *                 SDL_AddEventWatch, etc.
 * \param event the event that triggered the callback.
 * \returns true to permit event to be added to the queue, and false to
 *          disallow it. When used with SDL_AddEventWatch, the return value is
 *          ignored.
 *
 * \threadsafety SDL may call this callback at any time from any thread; the
 *               application is responsible for locking resources the callback
 *               touches that need to be protected.
 *
 * \since This datatype is available since SDL 3.0.0.
 *
 * \sa SDL_SetEventFilter
 * \sa SDL_AddEventWatch
  }
type

  TSDL_EventFilter = function (userdata:pointer; event:PSDL_Event):Tbool;cdecl;
{*
 * Set up a filter to process all events before they change internal state and
 * are posted to the internal event queue.
 *
 * If the filter function returns true when called, then the event will be
 * added to the internal queue. If it returns false, then the event will be
 * dropped from the queue, but the internal state will still be updated. This
 * allows selective filtering of dynamically arriving events.
 *
 * **WARNING**: Be very careful of what you do in the event filter function,
 * as it may run in a different thread!
 *
 * On platforms that support it, if the quit event is generated by an
 * interrupt signal (e.g. pressing Ctrl-C), it will be delivered to the
 * application at the next event poll.
 *
 * There is one caveat when dealing with the SDL_QuitEvent event type. The
 * event filter is only called when the window manager desires to close the
 * application window. If the event filter returns 1, then the window will be
 * closed, otherwise the window will remain open if possible.
 *
 * Note: Disabled events never make it to the event filter function; see
 * SDL_SetEventEnabled().
 *
 * Note: If you just want to inspect events without filtering, you should use
 * SDL_AddEventWatch() instead.
 *
 * Note: Events pushed onto the queue with SDL_PushEvent() get passed through
 * the event filter, but events pushed onto the queue with SDL_PeepEvents() do
 * not.
 *
 * \param filter an SDL_EventFilter function to call when an event happens.
 * \param userdata a pointer that is passed to `filter`.
 *
 * \threadsafety SDL may call the filter callback at any time from any thread;
 *               the application is responsible for locking resources the
 *               callback touches that need to be protected.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_AddEventWatch
 * \sa SDL_SetEventEnabled
 * \sa SDL_GetEventFilter
 * \sa SDL_PeepEvents
 * \sa SDL_PushEvent
  }

procedure SDL_SetEventFilter(filter:TSDL_EventFilter; userdata:pointer);cdecl;external;
{*
 * Query the current event filter.
 *
 * This function can be used to "chain" filters, by saving the existing filter
 * before replacing it with a function that will call that saved filter.
 *
 * \param filter the current callback function will be stored here.
 * \param userdata the pointer that is passed to the current event filter will
 *                 be stored here.
 * \returns true on success or false if there is no event filter set.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetEventFilter
  }
function SDL_GetEventFilter(filter:PSDL_EventFilter; userdata:Ppointer):Tbool;cdecl;external;
{*
 * Add a callback to be triggered when an event is added to the event queue.
 *
 * `filter` will be called when an event happens, and its return value is
 * ignored.
 *
 * **WARNING**: Be very careful of what you do in the event filter function,
 * as it may run in a different thread!
 *
 * If the quit event is generated by a signal (e.g. SIGINT), it will bypass
 * the internal queue and be delivered to the watch callback immediately, and
 * arrive at the next event poll.
 *
 * Note: the callback is called for events posted by the user through
 * SDL_PushEvent(), but not for disabled events, nor for events by a filter
 * callback set with SDL_SetEventFilter(), nor for events posted by the user
 * through SDL_PeepEvents().
 *
 * \param filter an SDL_EventFilter function to call when an event happens.
 * \param userdata a pointer that is passed to `filter`.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \threadsafety It is safe to call this function from any thread.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_RemoveEventWatch
 * \sa SDL_SetEventFilter
  }
function SDL_AddEventWatch(filter:TSDL_EventFilter; userdata:pointer):Tbool;cdecl;external;
{*
 * Remove an event watch callback added with SDL_AddEventWatch().
 *
 * This function takes the same input as SDL_AddEventWatch() to identify and
 * delete the corresponding callback.
 *
 * \param filter the function originally passed to SDL_AddEventWatch().
 * \param userdata the pointer originally passed to SDL_AddEventWatch().
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_AddEventWatch
  }
procedure SDL_RemoveEventWatch(filter:TSDL_EventFilter; userdata:pointer);cdecl;external;
{*
 * Run a specific filter function on the current event queue, removing any
 * events for which the filter returns false.
 *
 * See SDL_SetEventFilter() for more information. Unlike SDL_SetEventFilter(),
 * this function does not change the filter permanently, it only uses the
 * supplied filter until this function returns.
 *
 * \param filter the SDL_EventFilter function to call when an event happens.
 * \param userdata a pointer that is passed to `filter`.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_GetEventFilter
 * \sa SDL_SetEventFilter
  }
procedure SDL_FilterEvents(filter:TSDL_EventFilter; userdata:pointer);cdecl;external;
{*
 * Set the state of processing events by type.
 *
 * \param type the type of event; see SDL_EventType for details.
 * \param enabled whether to process the event or not.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_EventEnabled
  }
procedure SDL_SetEventEnabled(_type:TUint32; enabled:Tbool);cdecl;external;
{*
 * Query the state of processing events by type.
 *
 * \param type the type of event; see SDL_EventType for details.
 * \returns true if the event is being processed, false otherwise.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_SetEventEnabled
  }
function SDL_EventEnabled(_type:TUint32):Tbool;cdecl;external;
{*
 * Allocate a set of user-defined events, and return the beginning event
 * number for that set of events.
 *
 * \param numevents the number of events to be allocated.
 * \returns the beginning event number, or 0 if numevents is invalid or if
 *          there are not enough user-defined events left.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PushEvent
  }
function SDL_RegisterEvents(numevents:longint):TUint32;cdecl;external;
{*
 * Get window associated with an event.
 *
 * \param event an event containing a `windowID`.
 * \returns the associated window on success or NULL if there is none.
 *
 * \since This function is available since SDL 3.0.0.
 *
 * \sa SDL_PollEvent
 * \sa SDL_WaitEvent
 * \sa SDL_WaitEventTimeout
  }
(* Const before declarator ignored *)
function SDL_GetWindowFromEvent(event:PSDL_Event):PSDL_Window;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_events_h_  }

implementation


end.


unit SDL_touch;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_touch.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_touch.h
}


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
 * # CategoryTouch
 *
 * SDL touch management.
  }
{$ifndef SDL_touch_h_}
{$define SDL_touch_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_error.h>}
{$include <SDL3/SDL_mouse.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
{ C++ extern C conditionnal removed }
type
  PSDL_TouchID = ^TSDL_TouchID;
  TSDL_TouchID = TUint64;

  PSDL_FingerID = ^TSDL_FingerID;
  TSDL_FingerID = TUint64;
{ touch screen with window-relative coordinates  }
{ trackpad with absolute device coordinates  }
{ trackpad with screen cursor-relative coordinates  }

  PSDL_TouchDeviceType = ^TSDL_TouchDeviceType;
  TSDL_TouchDeviceType =  Longint;
  Const
    SDL_TOUCH_DEVICE_INVALID = -(1);
    SDL_TOUCH_DEVICE_DIRECT = (-(1))+1;
    SDL_TOUCH_DEVICE_INDIRECT_ABSOLUTE = (-(1))+2;
    SDL_TOUCH_DEVICE_INDIRECT_RELATIVE = (-(1))+3;
;
{*
 * Data about a single finger in a multitouch event.
 *
 * Each touch even is a collection of fingers that are simultaneously in
 * contact with the touch device (so a "touch" can be a "multitouch," in
 * reality), and this struct reports details of the specific fingers.
 *
 * \since This struct is available since SDL 3.0.0.
 *
 * \sa SDL_GetTouchFingers
  }
{*< the finger ID  }
{*< the x-axis location of the touch event, normalized (0...1)  }
{*< the y-axis location of the touch event, normalized (0...1)  }
{*< the quantity of pressure applied, normalized (0...1)  }
type
  PSDL_Finger = ^TSDL_Finger;
  TSDL_Finger = record
      id : TSDL_FingerID;
      x : single;
      y : single;
      pressure : single;
    end;
{ Used as the device ID for mouse events simulated with touch input  }

{ was #define dname def_expr }
function SDL_TOUCH_MOUSEID : TSDL_MouseID;  

{ Used as the SDL_TouchID for touch events simulated with mouse input  }
{ was #define dname def_expr }
function SDL_MOUSE_TOUCHID : TSDL_TouchID;  

{*
 * Get a list of registered touch devices.
 *
 * On some platforms SDL first sees the touch device if it was actually used.
 * Therefore the returned list might be empty, although devices are available.
 * After using all devices at least once the number will be correct.
 *
 * \param count a pointer filled in with the number of devices returned, may
 *              be NULL.
 * \returns a 0 terminated array of touch device IDs or NULL on failure; call
 *          SDL_GetError() for more information. This should be freed with
 *          SDL_free() when it is no longer needed.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetTouchDevices(count:Plongint):PSDL_TouchID;cdecl;external;
{*
 * Get the touch device name as reported from the driver.
 *
 * \param touchID the touch device instance ID.
 * \returns touch device name, or NULL on failure; call SDL_GetError() for
 *          more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before declarator ignored *)
function SDL_GetTouchDeviceName(touchID:TSDL_TouchID):Pansichar;cdecl;external;
{*
 * Get the type of the given touch device.
 *
 * \param touchID the ID of a touch device.
 * \returns touch device type.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetTouchDeviceType(touchID:TSDL_TouchID):TSDL_TouchDeviceType;cdecl;external;
{*
 * Get a list of active fingers for a given touch device.
 *
 * \param touchID the ID of a touch device.
 * \param count a pointer filled in with the number of fingers returned, can
 *              be NULL.
 * \returns a NULL terminated array of SDL_Finger pointers or NULL on failure;
 *          call SDL_GetError() for more information. This is a single
 *          allocation that should be freed with SDL_free() when it is no
 *          longer needed.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetTouchFingers(touchID:TSDL_TouchID; count:Plongint):^PSDL_Finger;cdecl;external;
{ Ends C function definitions when using C++  }
{ C++ end of extern C conditionnal removed }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_touch_h_  }

implementation

{ was #define dname def_expr }
function SDL_TOUCH_MOUSEID : TSDL_MouseID;
  begin
    SDL_TOUCH_MOUSEID:=TSDL_MouseID(-(1));
  end;

{ was #define dname def_expr }
function SDL_MOUSE_TOUCHID : TSDL_TouchID;
  begin
    SDL_MOUSE_TOUCHID:=TSDL_TouchID(-(1));
  end;


end.

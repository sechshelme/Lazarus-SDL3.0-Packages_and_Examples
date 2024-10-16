
unit SDL_sensor;
interface

{
  Automatically converted by H2Pas 0.99.16 from SDL_sensor.h
  The following command line parameters were used:
    -p
    -T
    -d
    -c
    -e
    SDL_sensor.h
}

Type
PSDL_Sensor = ^TSDL_Sensor;
Psingle = ^Tsingle;

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
 * # CategorySensor
 *
 * SDL sensor management.
 *
 * In order to use these functions, SDL_Init() must have been called with the
 * SDL_INIT_SENSOR flag. This causes SDL to scan the system for sensors, and
 * load appropriate drivers.
  }
{$ifndef SDL_sensor_h_}
{$define SDL_sensor_h_}
{$include <SDL3/SDL_stdinc.h>}
{$include <SDL3/SDL_error.h>}
{$include <SDL3/SDL_properties.h>}
{$include <SDL3/SDL_begin_code.h>}
{ Set up for C function definitions, even when using C++  }
type
{*
 * This is a unique ID for a sensor for the time it is connected to the
 * system, and is never reused for the lifetime of the application.
 *
 * The value 0 is an invalid ID.
 *
 * \since This datatype is available since SDL 3.0.0.
  }

  PSDL_SensorID = ^TSDL_SensorID;
  TSDL_SensorID = TUint32;
{*
 * A constant to represent standard gravity for accelerometer sensors.
 *
 * The accelerometer returns the current acceleration in SI meters per second
 * squared. This measurement includes the force of gravity, so a device at
 * rest will have an value of SDL_STANDARD_GRAVITY away from the center of the
 * earth, which is a positive Y value.
 *
 * \since This macro is available since SDL 3.0.0.
  }

const
  SDL_STANDARD_GRAVITY = 9.80665;  
{*
 * The different sensors defined by SDL.
 *
 * Additional sensors may be available, using platform dependent semantics.
 *
 * Here are the additional Android sensors:
 *
 * https://developer.android.com/reference/android/hardware/SensorEvent.html#values
 *
 * Accelerometer sensor notes:
 *
 * The accelerometer returns the current acceleration in SI meters per second
 * squared. This measurement includes the force of gravity, so a device at
 * rest will have an value of SDL_STANDARD_GRAVITY away from the center of the
 * earth, which is a positive Y value.
 *
 * - `values[0]`: Acceleration on the x axis
 * - `values[1]`: Acceleration on the y axis
 * - `values[2]`: Acceleration on the z axis
 *
 * For phones and tablets held in natural orientation and game controllers
 * held in front of you, the axes are defined as follows:
 *
 * - -X ... +X : left ... right
 * - -Y ... +Y : bottom ... top
 * - -Z ... +Z : farther ... closer
 *
 * The accelerometer axis data is not changed when the device is rotated.
 *
 * Gyroscope sensor notes:
 *
 * The gyroscope returns the current rate of rotation in radians per second.
 * The rotation is positive in the counter-clockwise direction. That is, an
 * observer looking from a positive location on one of the axes would see
 * positive rotation on that axis when it appeared to be rotating
 * counter-clockwise.
 *
 * - `values[0]`: Angular speed around the x axis (pitch)
 * - `values[1]`: Angular speed around the y axis (yaw)
 * - `values[2]`: Angular speed around the z axis (roll)
 *
 * For phones and tablets held in natural orientation and game controllers
 * held in front of you, the axes are defined as follows:
 *
 * - -X ... +X : left ... right
 * - -Y ... +Y : bottom ... top
 * - -Z ... +Z : farther ... closer
 *
 * The gyroscope axis data is not changed when the device is rotated.
 *
 * \since This enum is available since SDL 3.0.0.
 *
 * \sa SDL_GetCurrentDisplayOrientation
  }
{*< Returned for an invalid sensor  }
{*< Unknown sensor type  }
{*< Accelerometer  }
{*< Gyroscope  }
{*< Accelerometer for left Joy-Con controller and Wii nunchuk  }
{*< Gyroscope for left Joy-Con controller  }
{*< Accelerometer for right Joy-Con controller  }
{*< Gyroscope for right Joy-Con controller  }
type
  PSDL_SensorType = ^TSDL_SensorType;
  TSDL_SensorType =  Longint;
  Const
    SDL_SENSOR_INVALID = -(1);
    SDL_SENSOR_UNKNOWN = (-(1))+1;
    SDL_SENSOR_ACCEL = (-(1))+2;
    SDL_SENSOR_GYRO = (-(1))+3;
    SDL_SENSOR_ACCEL_L = (-(1))+4;
    SDL_SENSOR_GYRO_L = (-(1))+5;
    SDL_SENSOR_ACCEL_R = (-(1))+6;
    SDL_SENSOR_GYRO_R = (-(1))+7;
;
{ Function prototypes  }
{*
 * Get a list of currently connected sensors.
 *
 * \param count a pointer filled in with the number of sensors returned, may
 *              be NULL.
 * \returns a 0 terminated array of sensor instance IDs or NULL on failure;
 *          call SDL_GetError() for more information. This should be freed
 *          with SDL_free() when it is no longer needed.
 *
 * \since This function is available since SDL 3.0.0.
  }

function SDL_GetSensors(count:Plongint):PSDL_SensorID;cdecl;external;
{*
 * Get the implementation dependent name of a sensor.
 *
 * This can be called before any sensors are opened.
 *
 * \param instance_id the sensor instance ID.
 * \returns the sensor name, or NULL if `instance_id` is not valid.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before declarator ignored *)
function SDL_GetSensorNameForID(instance_id:TSDL_SensorID):Pansichar;cdecl;external;
{*
 * Get the type of a sensor.
 *
 * This can be called before any sensors are opened.
 *
 * \param instance_id the sensor instance ID.
 * \returns the SDL_SensorType, or `SDL_SENSOR_INVALID` if `instance_id` is
 *          not valid.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorTypeForID(instance_id:TSDL_SensorID):TSDL_SensorType;cdecl;external;
{*
 * Get the platform dependent type of a sensor.
 *
 * This can be called before any sensors are opened.
 *
 * \param instance_id the sensor instance ID.
 * \returns the sensor platform dependent type, or -1 if `instance_id` is not
 *          valid.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorNonPortableTypeForID(instance_id:TSDL_SensorID):longint;cdecl;external;
{*
 * Open a sensor for use.
 *
 * \param instance_id the sensor instance ID.
 * \returns an SDL_Sensor object or NULL on failure; call SDL_GetError() for
 *          more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_OpenSensor(instance_id:TSDL_SensorID):PSDL_Sensor;cdecl;external;
{*
 * Return the SDL_Sensor associated with an instance ID.
 *
 * \param instance_id the sensor instance ID.
 * \returns an SDL_Sensor object or NULL on failure; call SDL_GetError() for
 *          more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorFromID(instance_id:TSDL_SensorID):PSDL_Sensor;cdecl;external;
{*
 * Get the properties associated with a sensor.
 *
 * \param sensor the SDL_Sensor object.
 * \returns a valid property ID on success or 0 on failure; call
 *          SDL_GetError() for more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorProperties(sensor:PSDL_Sensor):TSDL_PropertiesID;cdecl;external;
{*
 * Get the implementation dependent name of a sensor.
 *
 * \param sensor the SDL_Sensor object.
 * \returns the sensor name or NULL on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
  }
(* Const before declarator ignored *)
function SDL_GetSensorName(sensor:PSDL_Sensor):Pansichar;cdecl;external;
{*
 * Get the type of a sensor.
 *
 * \param sensor the SDL_Sensor object to inspect.
 * \returns the SDL_SensorType type, or `SDL_SENSOR_INVALID` if `sensor` is
 *          NULL.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorType(sensor:PSDL_Sensor):TSDL_SensorType;cdecl;external;
{*
 * Get the platform dependent type of a sensor.
 *
 * \param sensor the SDL_Sensor object to inspect.
 * \returns the sensor platform dependent type, or -1 if `sensor` is NULL.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorNonPortableType(sensor:PSDL_Sensor):longint;cdecl;external;
{*
 * Get the instance ID of a sensor.
 *
 * \param sensor the SDL_Sensor object to inspect.
 * \returns the sensor instance ID, or 0 on failure; call SDL_GetError() for
 *          more information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorID(sensor:PSDL_Sensor):TSDL_SensorID;cdecl;external;
{*
 * Get the current state of an opened sensor.
 *
 * The number of values and interpretation of the data is sensor dependent.
 *
 * \param sensor the SDL_Sensor object to query.
 * \param data a pointer filled with the current sensor state.
 * \param num_values the number of values to write to data.
 * \returns true on success or false on failure; call SDL_GetError() for more
 *          information.
 *
 * \since This function is available since SDL 3.0.0.
  }
function SDL_GetSensorData(sensor:PSDL_Sensor; data:Psingle; num_values:longint):Tbool;cdecl;external;
{*
 * Close a sensor previously opened with SDL_OpenSensor().
 *
 * \param sensor the SDL_Sensor object to close.
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_CloseSensor(sensor:PSDL_Sensor);cdecl;external;
{*
 * Update the current state of the open sensors.
 *
 * This is called automatically by the event loop if sensor events are
 * enabled.
 *
 * This needs to be called from the thread that initialized the sensor
 * subsystem.
 *
 * \since This function is available since SDL 3.0.0.
  }
procedure SDL_UpdateSensors;cdecl;external;
{ Ends C function definitions when using C++  }
{$include <SDL3/SDL_close_code.h>}
{$endif}
{ SDL_sensor_h_  }

implementation


end.

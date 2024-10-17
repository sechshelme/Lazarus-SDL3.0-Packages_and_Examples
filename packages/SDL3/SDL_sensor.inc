/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   19:37                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_SensorID = ^TSDL_SensorID;
  TSDL_SensorID = TUint32;

const
  SDL_STANDARD_GRAVITY = 9.80665;

type
  PSDL_SensorType = ^TSDL_SensorType;
  TSDL_SensorType = longint;

const
  SDL_SENSOR_INVALID = -(1);
  SDL_SENSOR_UNKNOWN = (-(1)) + 1;
  SDL_SENSOR_ACCEL = (-(1)) + 2;
  SDL_SENSOR_GYRO = (-(1)) + 3;
  SDL_SENSOR_ACCEL_L = (-(1)) + 4;
  SDL_SENSOR_GYRO_L = (-(1)) + 5;
  SDL_SENSOR_ACCEL_R = (-(1)) + 6;
  SDL_SENSOR_GYRO_R = (-(1)) + 7;

type
  TSDL_Sensor = record
  end;
  PSDL_Sensor = ^TSDL_Sensor;

function SDL_GetSensors(Count: Plongint): PSDL_SensorID; cdecl; external libSDL3;
function SDL_GetSensorNameForID(instance_id: TSDL_SensorID): pansichar; cdecl; external libSDL3;
function SDL_GetSensorTypeForID(instance_id: TSDL_SensorID): TSDL_SensorType; cdecl; external libSDL3;
function SDL_GetSensorNonPortableTypeForID(instance_id: TSDL_SensorID): longint; cdecl; external libSDL3;
function SDL_OpenSensor(instance_id: TSDL_SensorID): PSDL_Sensor; cdecl; external libSDL3;
function SDL_GetSensorFromID(instance_id: TSDL_SensorID): PSDL_Sensor; cdecl; external libSDL3;
function SDL_GetSensorProperties(sensor: PSDL_Sensor): TSDL_PropertiesID; cdecl; external libSDL3;
function SDL_GetSensorType(sensor: PSDL_Sensor): TSDL_SensorType; cdecl; external libSDL3;
function SDL_GetSensorNonPortableType(sensor: PSDL_Sensor): longint; cdecl; external libSDL3;
function SDL_GetSensorID(sensor: PSDL_Sensor): TSDL_SensorID; cdecl; external libSDL3;
function SDL_GetSensorData(sensor: PSDL_Sensor; Data: Psingle; num_values: longint): Tbool; cdecl; external libSDL3;
procedure SDL_CloseSensor(sensor: PSDL_Sensor); cdecl; external libSDL3;
procedure SDL_UpdateSensors; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}

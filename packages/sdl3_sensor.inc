/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_Sensor = ^TSDL_Sensor;
  TSDL_Sensor = Pointer;      {undefined structure}

  PSDL_SensorID = ^TSDL_SensorID;
  TSDL_SensorID = Uint32;

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

  SDL_STANDARD_GRAVITY =cfloat( 9.80665);

function SDL_GetSensors(count:Plongint):PSDL_SensorID;cdecl;external;
function SDL_GetSensorInstanceName(instance_id:TSDL_SensorID):Pchar;cdecl;external;
function SDL_GetSensorInstanceType(instance_id:TSDL_SensorID):TSDL_SensorType;cdecl;external;
function SDL_GetSensorInstanceNonPortableType(instance_id:TSDL_SensorID):longint;cdecl;external;
function SDL_OpenSensor(instance_id:TSDL_SensorID):PSDL_Sensor;cdecl;external;
function SDL_GetSensorFromInstanceID(instance_id:TSDL_SensorID):PSDL_Sensor;cdecl;external;
function SDL_GetSensorProperties(sensor:PSDL_Sensor):TSDL_PropertiesID;cdecl;external;
function SDL_GetSensorName(sensor:PSDL_Sensor):Pchar;cdecl;external;
function SDL_GetSensorType(sensor:PSDL_Sensor):TSDL_SensorType;cdecl;external;
function SDL_GetSensorNonPortableType(sensor:PSDL_Sensor):longint;cdecl;external;
function SDL_GetSensorInstanceID(sensor:PSDL_Sensor):TSDL_SensorID;cdecl;external;
function SDL_GetSensorData(sensor:PSDL_Sensor; data:Psingle; num_values:longint):longint;cdecl;external;
procedure SDL_CloseSensor(sensor:PSDL_Sensor);cdecl;external;
procedure SDL_UpdateSensors;cdecl;external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

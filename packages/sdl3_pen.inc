/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_PenID = ^TSDL_PenID;
  TSDL_PenID = uint32;

const
  SDL_PEN_INVALID = uint32(0);
  SDL_PEN_MOUSEID = uint32(-2);
  SDL_PEN_INFO_UNKNOWN = -(1);

type
  PSDL_PenAxis = ^TSDL_PenAxis;
  TSDL_PenAxis = longint;

const
  SDL_PEN_AXIS_PRESSURE = 0;
  SDL_PEN_AXIS_XTILT = 1;
  SDL_PEN_AXIS_YTILT = 2;
  SDL_PEN_AXIS_DISTANCE = 3;
  SDL_PEN_AXIS_ROTATION = 4;
  SDL_PEN_AXIS_SLIDER = 5;
  SDL_PEN_NUM_AXES = 6;
  SDL_PEN_AXIS_LAST = SDL_PEN_NUM_AXES - 1;

  SDL_PEN_FLAG_DOWN_BIT_INDEX = 13;
  SDL_PEN_FLAG_INK_BIT_INDEX = 14;
  SDL_PEN_FLAG_ERASER_BIT_INDEX = 15;
  SDL_PEN_FLAG_AXIS_BIT_OFFSET = 16;

function SDL_PEN_CAPABILITY(capbit: culong): culong;
function SDL_PEN_AXIS_CAPABILITY(axis: culong): culong;

const
  SDL_PEN_TIP_INK = SDL_PEN_FLAG_INK_BIT_INDEX;
  SDL_PEN_TIP_ERASER = SDL_PEN_FLAG_ERASER_BIT_INDEX;

function SDL_PEN_DOWN_MASK: culong;
function SDL_PEN_INK_MASK: culong;
function SDL_PEN_ERASER_MASK: culong;
function SDL_PEN_AXIS_PRESSURE_MASK: culong;
function SDL_PEN_AXIS_XTILT_MASK: culong;
function SDL_PEN_AXIS_YTILT_MASK: culong;
function SDL_PEN_AXIS_DISTANCE_MASK: culong;
function SDL_PEN_AXIS_ROTATION_MASK: culong;
function SDL_PEN_AXIS_SLIDER_MASK: culong;
function SDL_PEN_AXIS_BIDIRECTIONAL_MASKS: culong;

type
  PSDL_PenSubtype = ^TSDL_PenSubtype;
  TSDL_PenSubtype = longint;

const
  SDL_PEN_TYPE_UNKNOWN = 0;
  SDL_PEN_TYPE_ERASER = 1;
  SDL_PEN_TYPE_PEN = 2;
  SDL_PEN_TYPE_PENCIL = 3;
  SDL_PEN_TYPE_BRUSH = 4;
  SDL_PEN_TYPE_AIRBRUSH = 5;
  SDL_PEN_TYPE_LAST = SDL_PEN_TYPE_AIRBRUSH;

function SDL_GetPens(Count: Plongint): PSDL_PenID; cdecl; external;
function SDL_GetPenStatus(instance_id: TSDL_PenID; x: Psingle; y: Psingle; axes: Psingle; num_axes: Tsize_t): Uint32; cdecl; external;
function SDL_GetPenFromGUID(guid: TSDL_GUID): TSDL_PenID; cdecl; external;
function SDL_GetPenGUID(instance_id: TSDL_PenID): TSDL_GUID; cdecl; external;
function SDL_PenConnected(instance_id: TSDL_PenID): TSDL_bool; cdecl; external;
function SDL_GetPenName(instance_id: TSDL_PenID): PChar; cdecl; external;

type
  PSDL_PenCapabilityInfo = ^TSDL_PenCapabilityInfo;

  TSDL_PenCapabilityInfo = record
    max_tilt: single;
    wacom_id: Uint32;
    num_buttons: int8;
  end;

function SDL_GetPenCapabilities(instance_id: TSDL_PenID; capabilities: PSDL_PenCapabilityInfo): int32; cdecl; external;
function SDL_GetPenType(instance_id: TSDL_PenID): TSDL_PenSubtype; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_PEN_CAPABILITY(capbit: culong): culong;
begin
  SDL_PEN_CAPABILITY := culong(1) shl capbit;
end;

function SDL_PEN_AXIS_CAPABILITY(axis: culong): culong;
begin
  SDL_PEN_AXIS_CAPABILITY := SDL_PEN_CAPABILITY(axis + SDL_PEN_FLAG_AXIS_BIT_OFFSET);
end;

function SDL_PEN_DOWN_MASK: culong;
begin
  SDL_PEN_DOWN_MASK := SDL_PEN_CAPABILITY(SDL_PEN_FLAG_DOWN_BIT_INDEX);
end;

function SDL_PEN_INK_MASK: culong;
begin
  SDL_PEN_INK_MASK := SDL_PEN_CAPABILITY(SDL_PEN_FLAG_INK_BIT_INDEX);
end;

function SDL_PEN_ERASER_MASK: culong;
begin
  SDL_PEN_ERASER_MASK := SDL_PEN_CAPABILITY(SDL_PEN_FLAG_ERASER_BIT_INDEX);
end;

function SDL_PEN_AXIS_PRESSURE_MASK: culong;
begin
  SDL_PEN_AXIS_PRESSURE_MASK := SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_PRESSURE);
end;

function SDL_PEN_AXIS_XTILT_MASK: culong;
begin
  SDL_PEN_AXIS_XTILT_MASK := SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_XTILT);
end;

function SDL_PEN_AXIS_YTILT_MASK: culong;
begin
  SDL_PEN_AXIS_YTILT_MASK := SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_YTILT);
end;

function SDL_PEN_AXIS_DISTANCE_MASK: culong;
begin
  SDL_PEN_AXIS_DISTANCE_MASK := SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_DISTANCE);
end;

function SDL_PEN_AXIS_ROTATION_MASK: culong;
begin
  SDL_PEN_AXIS_ROTATION_MASK := SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_ROTATION);
end;

function SDL_PEN_AXIS_SLIDER_MASK: culong;
begin
  SDL_PEN_AXIS_SLIDER_MASK := SDL_PEN_AXIS_CAPABILITY(SDL_PEN_AXIS_SLIDER);
end;

function SDL_PEN_AXIS_BIDIRECTIONAL_MASKS: culong;
begin
  Result := SDL_PEN_AXIS_XTILT_MASK or SDL_PEN_AXIS_YTILT_MASK;
end;


{$ENDIF read_implementation}

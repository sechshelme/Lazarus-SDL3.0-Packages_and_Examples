/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_PowerState = ^TSDL_PowerState;
  TSDL_PowerState = longint;

const
  SDL_POWERSTATE_UNKNOWN = 0;
  SDL_POWERSTATE_ON_BATTERY = 1;
  SDL_POWERSTATE_NO_BATTERY = 2;
  SDL_POWERSTATE_CHARGING = 3;
  SDL_POWERSTATE_CHARGED = 4;

function SDL_GetPowerInfo(seconds: Plongint; percent: Plongint): TSDL_PowerState; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

{$ENDIF read_implementation}

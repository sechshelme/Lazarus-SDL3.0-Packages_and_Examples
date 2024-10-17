/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

type
  PSDL_PowerState = ^TSDL_PowerState;
  TSDL_PowerState = longint;

const
  SDL_POWERSTATE_ERROR = -(1);
  SDL_POWERSTATE_UNKNOWN = (-(1)) + 1;
  SDL_POWERSTATE_ON_BATTERY = (-(1)) + 2;
  SDL_POWERSTATE_NO_BATTERY = (-(1)) + 3;
  SDL_POWERSTATE_CHARGING = (-(1)) + 4;
  SDL_POWERSTATE_CHARGED = (-(1)) + 5;

function SDL_GetPowerInfo(seconds: Plongint; percent: Plongint): TSDL_PowerState; cdecl; external libSDL3;

{$ENDIF read_interface}


{$IFDEF read_implementation}


{$ENDIF read_implementation}

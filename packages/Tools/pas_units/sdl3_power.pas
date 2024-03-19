unit SDL3_power;

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

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

implementation

end.

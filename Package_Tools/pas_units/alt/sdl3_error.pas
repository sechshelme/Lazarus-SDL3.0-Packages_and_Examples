unit SDL3_error;

interface

uses
  SDL3_stdinc;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

function SDL_SetError(fmt: PChar): longint; varargs; cdecl; external sdl3_lib;
function SDL_OutOfMemory: longint; cdecl; external sdl3_lib;
function SDL_GetError: PChar; cdecl; external sdl3_lib;
procedure SDL_ClearError; cdecl; external sdl3_lib;


function SDL_Unsupported: longint;
function SDL_InvalidParamError(param: PChar): longint;

type
  PSDL_errorcode = ^TSDL_errorcode;
  TSDL_errorcode = longint;

implementation

function SDL_Unsupported: longint;
begin
  SDL_Unsupported := SDL_SetError('That operation is not supported');
end;

function SDL_InvalidParamError(param: PChar): longint;
begin
  SDL_InvalidParamError := SDL_SetError('Parameter ''%s'' is invalid', param);
end;

end.

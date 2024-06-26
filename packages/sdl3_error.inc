/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 26-06-2024   19:19                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_SetError(fmt: PChar): longint; varargs; cdecl; external sdl3_lib;
function SDL_OutOfMemory: longint; cdecl; external sdl3_lib;
function SDL_GetError: PChar; cdecl; external sdl3_lib;
procedure SDL_ClearError; cdecl; external sdl3_lib;


function SDL_Unsupported: longint;
function SDL_InvalidParamError(param: PChar): longint;

type
  PSDL_errorcode = ^TSDL_errorcode;
  TSDL_errorcode = longint;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_Unsupported: longint;
begin
  SDL_Unsupported := SDL_SetError('That operation is not supported');
end;

function SDL_InvalidParamError(param: PChar): longint;
begin
  SDL_InvalidParamError := SDL_SetError('Parameter ''%s'' is invalid', param);
end;

{$ENDIF read_implementation}

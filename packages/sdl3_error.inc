/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_SetError(fmt: PChar): longint; varargs; cdecl; external;
//function SDL_SetError(fmt:Pchar):longint;cdecl;external;
function SDL_GetError: PChar; cdecl; external;
procedure SDL_ClearError; cdecl; external;

function SDL_OutOfMemory: longint;
function SDL_Unsupported: longint;
function SDL_InvalidParamError(param: longint): longint;

type
  PSDL_errorcode = ^TSDL_errorcode;
  TSDL_errorcode = longint;

const
  SDL_ENOMEM = 0;
  SDL_EFREAD = 1;
  SDL_EFWRITE = 2;
  SDL_EFSEEK = 3;
  SDL_UNSUPPORTED_ = 4;
  SDL_LASTERROR = 5;

function SDL_Error(code: TSDL_errorcode): longint; cdecl; external;

{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_OutOfMemory: longint;
begin
  SDL_OutOfMemory := SDL_Error(SDL_ENOMEM);
end;

function SDL_Unsupported: longint;
begin
  SDL_Unsupported := SDL_Error(SDL_UNSUPPORTED_);
end;

function SDL_InvalidParamError(param: longint): longint;
begin
  SDL_InvalidParamError := SDL_SetError('Parameter ''%s'' is invalid', param);
end;

{$ENDIF read_implementation}

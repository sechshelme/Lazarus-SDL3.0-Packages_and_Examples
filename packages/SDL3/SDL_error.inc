/////////////////////////////////////////////////////////////
//                                                         //
//   WARNUNG, dies Datei wird automatisch durch            //
//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //
//   Erzeugt am: 17-10-2024   17:58                        //
//                                                         //
/////////////////////////////////////////////////////////////

{$IFDEF read_interface}

function SDL_SetError(fmt: pansichar): Tbool; varargs cdecl; external libSDL3;
function SDL_OutOfMemory: Tbool; cdecl; external libSDL3;
function SDL_GetError: pansichar; cdecl; external libSDL3;
function SDL_ClearError: Tbool; cdecl; external libSDL3;

function SDL_Unsupported: boolean;
function SDL_InvalidParamError(param: longint): boolean;


{$ENDIF read_interface}


{$IFDEF read_implementation}

function SDL_Unsupported: boolean;
begin
  SDL_Unsupported := SDL_SetError('That operation is not supported');
end;

function SDL_InvalidParamError(param: longint): boolean;
begin
  SDL_InvalidParamError := SDL_SetError('Parameter ''%s'' is invalid', param);
end;


{$ENDIF read_implementation}

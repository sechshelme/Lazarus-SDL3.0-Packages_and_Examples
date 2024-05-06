program Project1;

uses
  sdl3;

  function CreateObject: Pointer;
  begin
    Result := nil;  // Als Test;
    if Result = nil then  begin
      SDL_SetError('Couldn''t build !');
    end;
  end;

var
  obj: Pointer;
begin
  obj := CreateObject;
  if obj = nil then  begin
    SDL_Log('Error: %s', SDL_GetError());
  end;
end.

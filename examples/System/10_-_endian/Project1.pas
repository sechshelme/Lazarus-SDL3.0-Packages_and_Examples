program Project1;

uses
  SDL3,
  ctypes;

var
  i32: uint32;


begin
  WriteLn(SDL_log(12.45));
  SDL_Log('%ix%i', 123, 456);
  SDL_Log('log 1');
  SDL_Log('Mathe log: %f', SDL_log(12.45));

  i32 := 1234;
  SDL_Log('int32: %i', i32);
  i32 := SDL_Swap32(i32);
  SDL_Log('int32: %i', i32);
  i32 := SDL_Swap32(i32);
  SDL_Log('int32: %i', i32);
end.

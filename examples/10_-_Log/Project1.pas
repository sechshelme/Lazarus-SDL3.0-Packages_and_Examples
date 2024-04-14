program Project1;

uses
  SDL3,
  ctypes;

begin
  WriteLn(SDL_log(12.45));
  SDL_Log('%ix%i', 123, 456);
  SDL_Log('log 1');
  SDL_Log('Mathe log: %f', SDL_log(12.45));
end.

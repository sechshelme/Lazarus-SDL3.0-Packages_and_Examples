program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  //  ctypes,
  SDL3;


begin
  SDL_init(SDL_INIT_VIDEO);

  SDL_Log('%ix%i', 123, 456);
  SDL_Log('%ix%i', 123, 456);
  SDL_Log('%ix%i', 123, 456);

  SDL_LogCritical(0, 'blabla 1');
  SDL_LogCritical(0, 'blabla 2');
  SDL_Log('log 1');
  SDL_Log('log 2');

  SDL_Quit;
end.

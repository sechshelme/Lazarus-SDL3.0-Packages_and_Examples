program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
    SDL3,
  ctypes;

  {$LinkLib 'SDL3'}

  // Mathe Funktion

//  function SDL_log_m(x: cdouble): cdouble; cdecl; external name 'SDL_log';



  // Log Info Ausgabe
//  procedure SDL_Log(fmt: PChar); varargs; cdecl; external;


begin
  WriteLn(SDL_log(12.45));
  SDL_Log('%ix%i', 123, 456);
  SDL_Log('log 1');
  SDL_Log('Mathe log: %f',SDL_log(12.45));

  SDL_cos(123);
//  SDL_cos('abc');
//  SDL_cos(PChar('abc'));
//  SDL_cos('%ix%i', 123, 456);
end.

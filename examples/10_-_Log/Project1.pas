program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  //  SDL3,
  ctypes;

  {$LinkLib 'SDL3'}

  // Mathe Funktion
  function SDL_log(x: cdouble): cdouble; cdecl; external;
  function SDL_cos(x: cdouble): cdouble; cdecl; external;


  // Log Info Ausgabe
  procedure SDL_Log(fmt: PChar); varargs; cdecl; external;


begin
  WriteLn(SDL_log(12.45));
  SDL_Log('%ix%i', 123, 456);
  SDL_Log('log 1');
  WriteLn(SDL_log(12.45));

  SDL_cos(123);
//  SDL_cos('abc');
//  SDL_cos(PChar('abc'));
//  SDL_cos('%ix%i', 123, 456);
end.

program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3,

  SDL_test_fuzzer;

  {$linklib libSDL3_test.a}



  //function SDLTest_RandomUint8: UInt8; cdecl; external 'libSDL3_test.a';

  //function SDLTest_RandomUint8: UInt8; cdecl; external;

var
  i: integer;
begin
  SDL_init(SDL_INIT_VIDEO);

  for i := 0 to 20 do begin
    WriteLn(SDLTest_RandomUint8);
  end;


  SDL_Quit;
end.

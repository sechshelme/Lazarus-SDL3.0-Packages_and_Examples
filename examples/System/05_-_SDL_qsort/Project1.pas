program Project1;

// https://wiki.libsdl.org/SDL3/SDL_AddTimer

uses
  sdl3;

var
  Data: array of DWord = nil;
  i: integer;

  function sort_func(para1: pointer; para2: pointer): longint;
  var
    i1, i2: DWord;
  begin
    i1 := PDWord(para1)^;
    i2 := PDWord(para2)^;
    Result := i1 - i2;
  end;

begin
  SetLength(Data, 8000);
  Randomize;
  for i := 0 to Length(Data) - 1 do begin
    Data[i] := SDL_rand;
    //    Data[i] := Random(2000);
  end;

  SDL_qsort(Pointer(Data), Length(Data), SizeOf(DWord), @sort_func);

  for i := 0 to Length(Data) - 1 do begin
    SDL_Log('data: %u', Data[i]);
  end;
end.

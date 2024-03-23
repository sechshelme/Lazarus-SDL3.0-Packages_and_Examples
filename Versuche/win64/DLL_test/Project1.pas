program Project1;

uses unit1;

var
  a:array of char;

begin
  SetLength(a, 1000);
  SDL_GetVersion(@a);

end.

program Project1;

uses
  SDL3,
  ctypes;

var
  stream: PSDL_IOStream;
  size: TSint64;
  ch: byte;
  i: integer;
  datasize: Tsize_t;
  pc: PChar;
begin
  stream := SDL_IOFromFile('test.txt', 'w');
  if stream = nil then begin
    SDL_Log('Write Fehler');
  end;
  SDL_IOprintf(stream, 'Hello world !'#10);
  SDL_IOprintf(stream, 'Hallo Welt !'#10);
  SDL_CloseIO(stream);

  stream := SDL_IOFromFile('test.txt', 'r');
  if stream = nil then begin
    SDL_Log('Read Fehler');
  end;
  size := SDL_GetIOSize(stream);

  for i := 0 to size - 1 do begin
    SDL_ReadU8(stream, @ch);
    Write(char(ch));
  end;
  SDL_CloseIO(stream);

  pc := SDL_LoadFile('test.txt', @datasize);
  Write(pc);

end.

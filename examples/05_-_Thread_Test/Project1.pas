program Project1;

uses
  ctypes,
  SDL3;

  // https://sdl.elynx.fr/SDL_CreateThread/

var
  thread: PSDL_Thread;
  threadReturnValue: longint;

  function TestThread(Data: pointer): longint; cdecl;
  var
    i: integer;
  begin
    for i := 0 to 10 do begin
      WriteLn(i, ' ');
      SDL_Delay(50);
    end;
    Result := i;
  end;

begin
  WriteLn('Simple SDL_CreateThread test:');
  thread := SDL_CreateThread(@TestThread, 'TestThread', nil);

  if thread = nil then  begin
    WriteLn('SDL_CreateThread failed: ', SDL_GetError);
  end else begin
    SDL_WaitThread(thread, @threadReturnValue);
    WriteLn('Thread returned value: ', threadReturnValue);
  end;
end.

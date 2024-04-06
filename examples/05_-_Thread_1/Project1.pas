program Project1;

uses
  {$IFDEF linux}
    cthreads,
  {$ENDIF}
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
      SDL_Log('%i', i);
      SDL_Delay(50);
    end;
    Result := i;
  end;

begin
  WriteLn('Simple SDL_CreateThread test:');
  thread := SDL_CreateThread(@TestThread, 'TestThread', nil);

  if thread = nil then  begin
    SDL_Log('SDL_CreateThread failed: %i', SDL_GetError);
  end else begin
    SDL_WaitThread(thread, @threadReturnValue);
    SDL_Log('Thread returned value: %i', threadReturnValue);
  end;
end.

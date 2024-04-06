program Project1;

uses
  {$IFDEF linux}
  //  cthreads,
  {$ENDIF}
  SDL3;

  // https://sdl.elynx.fr/SDL_CreateThread/

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

var
  thread: array [0..7] of PSDL_Thread;
  threadReturnValue: longint;
  i: integer;
begin
  WriteLn('Simple SDL_CreateThread test:');
  for i := 0 to Length(thread) - 1 do begin
    thread[i] := SDL_CreateThread(@TestThread, 'TestThread', nil);
    if thread[i] = nil then  begin
      SDL_Log('SDL_CreateThread failed: %i', SDL_GetError);
      halt;
    end;
  end;


  for i := 0 to Length(thread) - 1 do begin
    SDL_WaitThread(thread[i], @threadReturnValue);
    SDL_Log('Thread returned value: %i', threadReturnValue);
  end;
end.

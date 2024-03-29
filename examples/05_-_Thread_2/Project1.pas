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
    Index: PtrInt;
  begin
    Index := PtrInt(Data);

    for i := 0 to 10 do begin
      SDL_Log('Index: %i    Counter: %i', Index, i);
      SDL_Delay(50);
    end;
    Result := Index;
  end;

var
  thread: array of PSDL_Thread;
  threadReturnValue: longint;
  i: integer;
begin
  SDL_Log('Simple SDL_CreateThread test:');
  SetLength(thread, 8);
  for i := 0 to Length(thread) - 1 do begin
    thread[i] := SDL_CreateThread(@TestThread, 'TestThread', pointer(i));

    if thread[i] = nil then  begin
      SDL_LogError(0, 'SDL_CreateThread failed: %i', SDL_GetError);
    end;
  end;

  for i := 0 to Length(thread) - 1 do begin
    SDL_WaitThread(thread[i], @threadReturnValue);
    SDL_Log('Thread returned value: %i', threadReturnValue);
  end;
end.

program Project1;

uses
  {$IFDEF linux}
  cthreads,
  {$ENDIF}
  sdl3;

  // https://sdl.elynx.fr/SDL_CreateThread/

const
  StrNormal = #27'[0m';

  function GetCol(i: integer): string;
  begin
    Result := #27'[9' + char(i mod 8 + byte('0')) + 'm';
  end;


  function TestThread(Data: pointer): longint; cdecl;
  var
    i: integer;
    Index: PtrInt;
  begin
    Index := PtrInt(Data);

    for i := 0 to 10 do begin
      SDL_Log(PChar(GetCol(Index) + 'Index: %i    Counter: %i' + StrNormal), Index, i);
            SDL_Delay(100);
      //      SDL_Delay(random(100));
    end;
    Result := Index;
  end;

var
  thread: array of PSDL_Thread;
  threadReturnValue: longint;
  i: integer;
begin
  SDL_SetError('dsfsdfsfsd');
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

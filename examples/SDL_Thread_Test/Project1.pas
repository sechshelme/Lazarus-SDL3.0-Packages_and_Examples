program Project1;

uses
  ctypes,
  SDL3;

  // https://sdl.elynx.fr/SDL_CreateThread/

var
  ver: TSDL_Version;
  window: PSDL_Window;
  e: TSDL_Event;
  quit: boolean = False;
  image, screen: PSDL_Surface;
  dstrect: TSDL_Rect = (x: 100; y: 100; w: 200; h: 200);
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

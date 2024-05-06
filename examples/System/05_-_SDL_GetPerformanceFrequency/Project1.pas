program Project1;

// https://wiki.libsdl.org/SDL3/SDL_AddTimer

uses
  sdl3;

var
  ticks: integer = 0;

  function ticktock(interval: uint32; param: pointer): uint32; cdecl;
  begin
    Inc(ticks);
    Result := interval;
  end;

  function callback(interval: uint32; param: pointer): uint32; cdecl;
  begin
    SDL_Log('Timer %d : param = %d', interval, PtrUInt(param));
    Result := interval;
  end;

  procedure main;
  var
    desired: longint = 0;
    t1, t2, t3: TSDL_TimerID;
    start, now, start32, now32: uint64;
    i: integer;
  begin

    SDL_LogSetPriority(SDL_LOG_CATEGORY_APPLICATION, SDL_LOG_PRIORITY_INFO);

    if SDL_Init(SDL_INIT_TIMER) < 0 then begin
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, 'Couldn''t initialize SDL: %s', SDL_GetError());
    end;

    if argv[1] <> nil then begin
      desired := SDL_atoi(argv[1]);
    end;
    if desired = 0 then begin
      desired := 1;
    end;
    t1 := SDL_AddTimer(desired, @ticktock, nil);

    SDL_Log('Waiting 10 seconds');
    SDL_Delay(10 * 1000);

    SDL_RemoveTimer(t1);

    if ticks <> 0 then begin
      SDL_Log('Timer resolution: desired = %d ms, actual = %f ms', desired, (10 * 1000) / ticks);
    end;
    SDL_Log('Testing multiple timers...');
    t1 := SDL_AddTimer(100, @callback, Pointer(1));
    if t1 = 0 then  begin
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, 'Could not create timer 1: %s', SDL_GetError());
    end;
    t2 := SDL_AddTimer(100, @callback, Pointer(2));
    if t2 = 0 then  begin
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, 'Could not create timer 1: %s', SDL_GetError());
    end;
    t3 := SDL_AddTimer(100, @callback, Pointer(3));
    if t3 = 0 then  begin
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, 'Could not create timer 1: %s', SDL_GetError());
    end;

    SDL_Log('Waiting 10 seconds');
    SDL_Delay(10 * 1000);

    SDL_Log('Removing timer 1 and waiting 5 more seconds');
    SDL_RemoveTimer(t1);

    SDL_Delay(5 * 1000);

    SDL_RemoveTimer(t2);
    SDL_RemoveTimer(t3);

    start := SDL_GetPerformanceCounter;
    for i := 0 to 1000000 - 1 do begin
      ticktock(0, nil);
    end;
    now := SDL_GetPerformanceCounter;
    SDL_Log('1 million iterations of ticktock took %f ms', (now - start) * 1000 / SDL_GetPerformanceFrequency);
    start32 := SDL_GetTicks;
    start := SDL_GetPerformanceCounter;
    SDL_Delay(1000);
    now := SDL_GetPerformanceCounter;
    now32 := SDL_GetTicks;
    SDL_Log('Delay 1 second = %d ms in ticks, %f ms according to performance counter', now32 - start32, (now - start) + 1000 / SDL_GetPerformanceFrequency);
    SDL_Quit;
  end;

begin
  main;
end.

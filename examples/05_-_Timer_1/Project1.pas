program Project1;

// https://wiki.libsdl.org/SDL3/SDL_AddTimer

uses
  sdl3;

var
  timer_id1, timer_id2: TSDL_TimerID;

  function my_callbackfunc(interval: uint32; param: pointer): uint32; cdecl;
  begin
    SDL_Log('Timer    Name: %s', param);
    Result := interval;
  end;

begin
  SDL_Init(SDL_INIT_TIMER);
  SDL_Log('Simple SDL_AddTimer test:');
  timer_id1 := SDL_AddTimer(500, @my_callbackfunc, PChar('Timer 1'));
  timer_id2 := SDL_AddTimer(300, @my_callbackfunc, PChar('Timer 2'));
  SDL_Delay(3000);
  SDL_Log('Remove Timer 1');
  SDL_RemoveTimer(timer_id1);
  SDL_Delay(3000);
  SDL_Log('Remove Timer 2');
  SDL_RemoveTimer(timer_id2);
  SDL_Delay(3000);
  SDL_Log('Ende');
  SDL_Quit;
end.

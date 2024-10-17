program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3;

const
  Width = 800;
  heigt = 600;
  TimerCount = 800;
  RectSize = 20;

type
  TPointrect = record
    index: integer;
    stepx, stepy: single;
    rect: TSDL_FRect;
  end;
  PPointrect = ^TPointrect;

var
  window: PSDL_Window;
  renderer: PSDL_Renderer;

  procedure Run;
  var
    e: TSDL_Event;
    quit: boolean = False;
    rDest: array[0..TimerCount - 1] of TSDL_FRect;
    time: extended;
    pr: TPointrect;
    i: integer;
    red, green, blue: single;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e._type of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.key of

              SDLK_ESCAPE: begin
                quit := True;
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
          SDL_EVENT_USER: begin
            pr := PPointrect(e.user.data1)^;
            rDest[pr.index] := pr.rect;
          end;
          SDL_EVENT_WINDOW_RESIZED: begin
            SDL_Log('X: %i   Y: %i', e.window.data1, e.window.data2);
          end;
        end;
      end;

      time := SDL_GetTicks / 1000;
      red := (SDL_sinf(time) + 1) / 2.0;
      green := (SDL_sinf(time / 2) + 1) / 2.0;
      blue := (SDL_sinf(time / 3) + 1) / 2.0;

      SDL_SetRenderDrawColorFloat(renderer, red, green, blue, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);

      SDL_SetRenderDrawColorFloat(renderer, 1 - red, 1 - green, 1 - blue, SDL_ALPHA_OPAQUE);
      for i := 0 to TimerCount - 1 do begin
        SDL_RenderFillRect(renderer, @rDest[i]);
      end;

      SDL_RenderPresent(renderer);
    end;
  end;

  function PointMoveProc(userdata: pointer; timerID: TSDL_TimerID; interval: TUint32): TUint32; cdecl;
  var
    event: TSDL_Event;
    userevent: TSDL_UserEvent;
    rect: PPointrect absolute userdata;
  begin
    rect^.rect.x += rect^.stepx;
    if rect^.rect.x > Width then begin
      rect^.rect.x := -RectSize;
    end;
    if rect^.rect.x < -RectSize then begin
      rect^.rect.x := Width;
    end;

    rect^.rect.y += rect^.stepy;
    if rect^.rect.y > heigt then begin
      rect^.rect.y := -RectSize;
    end;
    if rect^.rect.y < -RectSize then begin
      rect^.rect.y := heigt;
    end;

    userevent._type := SDL_EVENT_USER;
    userevent.code := 0;
    userevent.data1 := userdata;
    userevent.data2 := nil;

    event._type := SDL_EVENT_USER;
    event.user := userevent;

    SDL_PushEvent(@event);
    Result := interval;
  end;

  procedure main;
  var
    Pointrect: array [0..TimerCount - 1] of TPointrect;
    i: integer;
  begin
    SDL_init(SDL_INIT_VIDEO);

    for i := 0 to TimerCount - 1 do begin
      Pointrect[i].rect.x := Random(RectSize) + RectSize;
      Pointrect[i].rect.y := Random(RectSize) + RectSize;
      Pointrect[i].rect.w := Random(RectSize) + RectSize;
      Pointrect[i].rect.h := Random(RectSize) + RectSize;
      Pointrect[i].index := i;
      Pointrect[i].stepx := (Random - 0.5) * 2;
      Pointrect[i].stepy := (Random - 0.5) * 2;

      SDL_AddTimer(10, @PointMoveProc, @Pointrect[i]);
    end;

    window := SDL_CreateWindow('SDL3 Window', Width, heigt, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Kann kein SDL-Fenster erzeugen !');
    end;

    renderer := SDL_CreateRenderer(window, nil);
    if renderer = nil then begin
      SDL_Log('Kann kein SDL-Renderer erzeugen !');
    end;

    Run;

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.

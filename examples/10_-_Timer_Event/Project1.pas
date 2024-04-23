program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3;

const
  Width = 800;
  heigt = 600;
  TimerCount = 800;

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
  bitmapTex: PSDL_Texture;

  function CreateTexture: PSDL_Texture;
  const
    size = 64;
  var
    r: TSDL_Rect;
    surface: PSDL_Surface;
  begin
    surface := SDL_CreateSurface(size, size, SDL_PIXELFORMAT_RGBA32);
    if surface = nil then begin
      SDL_Log('Kann kein Surface erzeugen !');
    end;
    r.x := 0;
    r.y := 0;
    r.w := size;
    r.h := size;
    SDL_FillSurfaceRect(surface, @r, $8888FFFF);

    Result := SDL_CreateTextureFromSurface(renderer, surface);
    if Result = nil then begin
      SDL_Log('Kann keine Texture erzeugen !');
    end;

    SDL_DestroySurface(surface);
  end;

  procedure Run;
  const
    step = 0.01;
  var
    e: TSDL_Event;
    quit: boolean = False;
    rDest: array[0..TimerCount - 1] of TSDL_FRect;
    keyStat: PUInt8;
    red, green, blue: int64;
    time: extended;
    pr: TPointrect;
    i: integer;
    r: TSDL_FRect;
  begin
    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      if keyStat[SDL_SCANCODE_SPACE] <> 0 then begin
      end;

      while SDL_PollEvent(@e) do begin
        case e.type_ of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.keysym.sym of

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
        end;
      end;

      time := SDL_GetTicks / 1000;
      red := Trunc((SDL_sinf(time) + 1) / 2.0 * 255);
      green := Trunc((SDL_sinf(time / 2) + 1) / 2.0 * 255);
      blue := Trunc((SDL_sinf(time / 3) + 1) / 2.0 * 255);

      SDL_SetRenderDrawColor(renderer, red, green, blue, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);

      for i := 0 to TimerCount - 1 do begin
        SDL_RenderTexture(renderer, bitmapTex, nil, @rDest[i]);
      end;

      r.x:=150;
      r.y:=150;
      r.w:=50;
      r.h:=50;

      SDL_SetRenderDrawColorFloat(renderer, 0.5, 1.0, 0.0, SDL_ALPHA_OPAQUE);
      SDL_RenderFillRect(renderer, @r);

      SDL_RenderPresent(renderer);
    end;
  end;

  function PointMoveProc(interval: uint32; param: pointer): uint32; cdecl;
  var
    event: TSDL_Event;
    userevent: TSDL_UserEvent;
    rect: PPointrect absolute param;
  begin
    rect^.rect.x += rect^.stepx;
    if rect^.rect.x > Width then begin
      rect^.rect.x := 0;
    end;

    rect^.rect.y += rect^.stepy;
    if rect^.rect.y > heigt then begin
      rect^.rect.y := 0;
    end;

    userevent._type := SDL_EVENT_USER;
    userevent.code := 0;
    userevent.data1 := param;
    userevent.data2 := nil;

    event.type_ := SDL_EVENT_USER;
    event.user := userevent;

    SDL_PushEvent(@event);
    Result := interval;
  end;

  procedure main;
  var
    Pointrect: array [0..TimerCount - 1] of TPointrect;
    i: integer;
    //    Pointrect: TPointrect;
  begin
    SDL_init(SDL_INIT_VIDEO or SDL_INIT_TIMER);

    for i := 0 to TimerCount - 1 do begin

      Pointrect[i].rect.x := Random(50) + 50;
      Pointrect[i].rect.y := Random(50) + 50;
      Pointrect[i].rect.w := Random(50) + 50;
      Pointrect[i].rect.h := Random(50) + 50;
      Pointrect[i].index := i;
      Pointrect[i].stepx := Random();
      Pointrect[i].stepy := Random();

      SDL_AddTimer(10, @PointMoveProc, @Pointrect[i]);
    end;

    window := SDL_CreateWindow('SDL3 Window', Width, heigt, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      SDL_Log('Kann kein SDL-Fenster erzeugen !');
    end;

    renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_ACCELERATED);
    if renderer = nil then begin
      SDL_Log('Kann kein SDL-Renderer erzeugen !');
    end;

    bitmapTex := CreateTexture;

    Run;

    SDL_DestroyTexture(bitmapTex);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.

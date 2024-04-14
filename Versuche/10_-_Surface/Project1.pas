program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3;

var
  window: PSDL_Window;
  bitmapSurface, surface, winSurface: PSDL_Surface;
  distrect: TSDL_Rect = (x: 100; y: 100; w: 200; h: 200);
  Width, Height, bbwidth, bbheight: longint;

  procedure SDLFail(const err: string);
  begin
    SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, PChar('Fehler: ' + err));
    Halt(1);
  end;


  procedure Run;
  var
    event: TSDL_Event;
    quit: boolean = False;
    time: single;
    red, green, blue: byte;
    keyStat: PUInt8;
    cnt: integer = 0;
  begin

    while not quit do begin
      keyStat := SDL_GetKeyboardState(nil);
      if keyStat[SDL_SCANCODE_SPACE] <> 0 then begin
        SDL_Log('Space is pressed   %i',cnt);
        inc( cnt);
      end;
      if keyStat[SDL_SCANCODE_LEFT] <> 0 then begin
        SDL_Log('Left is pressed   %i',cnt);
        inc( cnt);
      end;

      while SDL_PollEvent(@event) do begin
        case event.type_ of
          SDL_EVENT_KEY_DOWN: begin
            SDL_Log('key: %i', event.key.keysym.sym); // neu

            case event.key.keysym.sym of
              SDLK_ESCAPE: begin
                quit := True;
              end;
              SDLK_m: begin
//                SwitchMouseButton;
              end;

            end;
          end;
          SDL_EVENT_MOUSE_BUTTON_DOWN:SDL_Log('Mouse down');
          SDL_EVENT_MOUSE_BUTTON_UP:SDL_Log('Mouse up');
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;


      time := SDL_GetTicks / 1000;
      red := Trunc((SDL_sinf(time) + 1) / 2.0 * 255);
      green := Trunc((SDL_sinf(time / 2) + 1) / 2.0 * 255);
      blue := Trunc((SDL_sinf(time / 3) + 1) / 2.0 * 255);

      SDL_BlitSurface(surface,nil,winSurface,nil);
    SDL_FlipSurface(surface,SDL_FLIP_HORIZONTAL);

    SDL_FillSurfaceRect(winSurface,@distrect,$FF);

      SDL_UpdateWindowSurface(window);

    end;
  end;

function CreateSurface:PSDL_Surface;   const size=256;
var pixels:array[0..size-1,0..size-1]of uint32;
  i, j: Integer;
begin
  for i:=0 to size-1 do for j:=0 to size-1 do pixels[i,j]:=Random($FF);
  Result:=SDL_CreateSurfaceFrom(@pixels,size,size,0,SDL_PIXELFORMAT_RGBA8888);
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDLFail('Kann kein SDL-Fenster erzeugen !');
  end;

  winSurface:=SDL_GetWindowSurface(window);
  winsurface :=SDL_CreateSurface(100,100, SDL_PIXELFORMAT_RGBA8888);
  if winsurface = nil then begin
    SDLFail('Kann winSurface nicherzeugen !');
  end;

  surface:=CreateSurface;

  SDL_ShowWindow(window);

  SDL_GetWindowSize(window, @Width, @Height);
  SDL_GetWindowSizeInPixels(window, @bbwidth, @bbheight);
  SDL_LogCritical(0, 'Window size: %ix%i', bbwidth, bbheight);
  SDL_LogCritical(0, 'blabla');
  SDL_Log('Window size: %ix%i', bbwidth, bbheight);
  SDL_Log('log');
  SDL_LogWarn(0, 'warn');
  WriteLn('Window size: ', bbwidth, 'x', bbheight);

  if Width <> bbwidth then  begin
    SDL_Log('This is a highdpi environment.');
  end;

  Run;

  SDL_DestroyWindow(window);

  SDL_Quit;
  SDL_Log('Application quit successfully!');
end.

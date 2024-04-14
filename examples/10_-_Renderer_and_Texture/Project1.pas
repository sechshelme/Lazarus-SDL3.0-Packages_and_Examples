program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3;

var
  window: PSDL_Window;
  bitmapSurface: PSDL_Surface;
  renderer: PSDL_Renderer;
  bitmapTex: PSDL_Texture;

  function CreateSurface: PSDL_Surface;
  var
    r: TSDL_Rect;
  begin
    Result := SDL_LoadBMP('mauer.bmp');
    if Result = nil then begin
      SDL_Log('Kann keine textur erzeugen !');
    end;
    r.x := 1;
    r.y := 1;
    r.w := 2;
    r.h := 2;
    SDL_FillSurfaceRect(Result, @r, $FFFF);
  end;

  procedure SDLMain;
  var
    e: TSDL_Event;
    quit: boolean = False;
    rSrc, rDest: TSDL_FRect;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) do begin
        WriteLn('event: ', e.type_); // neu
        case e.type_ of
          SDL_EVENT_KEY_DOWN: begin
            WriteLn('key: ', e.key.keysym.sym); // neu
            case e.key.keysym.sym of

              SDLK_ESCAPE: begin
                quit := True;
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

      SDL_RenderClear(renderer);
      //   SDL_RenderTexture(renderer, bitmapTex, nil, @distrect);
      rSrc.x := 0;
      rSrc.y := 0;
      rSrc.w := 400;
      rSrc.h := 400;

      rDest.x := 0;
      rDest.y := 0;
      rDest.w := 100;
      rDest.h := 100;


      SDL_RenderTexture(renderer, bitmapTex, @rSrc, @rDest);
      SDL_RenderPresent(renderer);
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  window := SDL_CreateWindow('SDL3 Window', 320, 200, SDL_WINDOW_RESIZABLE);
  if window = nil then begin
    SDL_Log('Kann kein SDL-Fenster erzeugen !');
  end;
  renderer := SDL_CreateRenderer(window, nil, SDL_RENDERER_ACCELERATED);
  if renderer = nil then begin
    SDL_Log('Kann kein SDL-Renderer erzeugen !');
  end;

  bitmapSurface := CreateSurface;

  bitmapTex := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
  if bitmapSurface = nil then begin
    SDL_Log('Kann bmp nicht laden !');
  end;

  SDL_DestroySurface(bitmapSurface);

  SDLMain;

  SDL_DestroyTexture(bitmapTex);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(window);

  SDL_Quit;
end.

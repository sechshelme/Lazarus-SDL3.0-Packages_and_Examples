program Project1;

// https://wiki.libsdl.org/SDL3/SDL_GetWindowSurface

uses
  SDL3;

var
  window: PSDL_Window;
  surface, image: PSDL_Surface;
 rSrc, rDest:TSDL_Rect;
begin
  if SDL_init(SDL_INIT_VIDEO) < 0 then begin
    SDL_Log('Konnte SDL-VIDEO nicht laden!:  %s', SDL_GetError);
  end;

  window := SDL_CreateWindow('Surface Example', 640, 480, 0);
  if window = nil then begin
    SDL_Log('Konnte kein Windows erzeugen!:  %s', SDL_GetError);
  end;

  surface := SDL_GetWindowSurface(window);
  if surface = nil then begin
    SDL_Log('Konnte kein Surface erzeugen!:  %s', SDL_GetError);
  end;

  image := SDL_LoadBMP('mauer.bmp');
  if image = nil then begin
    SDL_Log('Konnte BMP nicht laden!:  %s', SDL_GetError);
  end;

  rDest.items:=[100,100, 200,200];
  rSrc.items:=[0,0, 20,20];
  SDL_BlitSurfaceScaled(image, nil, surface, @rDest, SDL_SCALEMODE_NEAREST);
  SDL_BlitSurface(image, nil, surface, nil);
  SDL_UpdateWindowSurface(window);

  SDL_Delay(5000);

  SDL_DestroySurface(image);
  SDL_DestroyWindow(window);
  SDL_Quit;
end.

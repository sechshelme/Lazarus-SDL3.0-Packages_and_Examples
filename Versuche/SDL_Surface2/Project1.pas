program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

// https://stackoverflow.com/questions/37978149/sdl1-sdl2-resolution-list-building-with-a-custom-screen-mode-class

uses
  SDL3,
  unixtype;

//Widht: 3440 Height: 1440
//Widht: 2560 Height: 1440
//Widht: 2560 Height: 1080
//Widht: 1920 Height: 1200
//Widht: 1920 Height: 1080
//Widht: 1680 Height: 1050
//Widht: 1600 Height: 1200
//Widht: 1600 Height:  900
//Widht: 1440 Height:  900
//Widht: 1366 Height:  768
//Widht: 1280 Height: 1024
//Widht: 1280 Height:  800
//Widht: 1280 Height:  720
//Widht: 1152 Height:  864
//Widht: 1024 Height:  768
//Widht:  800 Height:  600
//Widht:  720 Height:  480
//Widht:  640 Height:  480

var
  sdlWindow1: PSDL_Window;
  sdlRenderer: PSDL_Renderer;
  sdlsurface: PSDL_Surface;
  r: TSDL_Rect;

begin

  //initilization of video subsystem
  if SDL_Init(SDL_INIT_VIDEO) < 0 then begin
    Halt;
  end;

  // full set up
  sdlWindow1 := SDL_CreateWindow('Window1',500, 500, SDL_WINDOW_RESIZABLE);
  if sdlWindow1 = nil then begin
    Halt;
  end;
  //
  //  sdlRenderer := SDL_CreateRenderer(sdlWindow1, -1, 0);
  //  if sdlRenderer = nil then begin
  //    Halt;
  //  end;

  sdlsurface := SDL_GetWindowSurface(sdlWindow1);

  // quick set up
  {
  if SDL_CreateWindowAndRenderer(500, 500, SDL_WINDOW_SHOWN, @sdlWindow1, @sdlRenderer) <> 0
    then Halt;
  }
  r.x := 10;
  r.y := 10;
  r.h := 100;
  r.w := 100;


//  SDL_FillSurfaceRect(sdlsurface, @r, $FFFFFFFF);
  SDL_FillSurfaceRect(sdlsurface, @r, $FFFFFFFF);
  SDL_UpdateWindowSurface(sdlWindow1);


  //  SDL_RenderPresent(sdlRenderer);
  // render to window for 2 seconds
  SDL_Delay(4000);

  // clear memory
  SDL_DestroyRenderer(sdlRenderer);
  SDL_DestroyWindow(sdlWindow1);

  //closing SDL2
  SDL_Quit;

end.

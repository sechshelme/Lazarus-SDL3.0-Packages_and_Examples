program Project1;

uses
  heaptrc,
  ctypes,
  SDL3, Node, FillCol;

procedure main;
var
  win: PSDL_Window;
  screen: PSDL_Renderer;
begin
  SDL_Init(SDL_INIT_VIDEO);
  win:=SDL_CreateWindow('Knoten',1000,1000,SDL_WINDOW_RESIZABLE);
  screen:=SDL_CreateRenderer(win,nil,SDL_RENDERER_PRESENTVSYNC);
end;

begin
  main;
end.


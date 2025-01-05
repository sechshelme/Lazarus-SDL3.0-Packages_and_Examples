program Project1;

// https://github.com/PascalGameDevelopment/SDL2-for-Pascal

// SDL2 - https://wiki.libsdl.org/SDL2/SDL_RenderGeometry
// SDL3 - https://wiki.libsdl.org/SDL3/SDL_RenderGeometry
// https://discourse.libsdl.org/t/am-i-using-sdl-rendergeometry-correctly/32995

uses
  SDL3;

  procedure Triangle(renderer: PSDL_Renderer);
  const
    vert: array of TSDL_Vertex = (
      (position: (x: 200; y: 150); color: (r: 1.0; g: 0.0; b: 0.0; a: 1.0); tex_coord: (x: 0.5; y: 1.0)),
      (position: (x: 100; y: 450); color: (r: 0.0; g: 0.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 0.0)),
      (position: (x: 300; y: 450); color: (r: 0.0; g: 1.0; b: 0.0; a: 1.0); tex_coord: (x: 1.0; y: 0.0)));
  begin
    SDL_RenderGeometry(renderer, nil, PSDL_Vertex(vert), Length(vert), nil, 0);
  end;

  procedure Quad(renderer: PSDL_Renderer);
  const
    vert: array of TSDL_Vertex = (
      (position: (x: 500; y: 150); color: (r: 1.0; g: 0.0; b: 0.0; a: 1.0); tex_coord: (x: 0.5; y: 1.0)),
      (position: (x: 700; y: 150); color: (r: 0.0; g: 0.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 0.0)),
      (position: (x: 700; y: 250); color: (r: 0.0; g: 0.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 0.0)),
      (position: (x: 500; y: 250); color: (r: 1.0; g: 1.0; b: 0.0; a: 1.0); tex_coord: (x: 1.0; y: 0.0)));
    indices: array of integer = (0, 1, 2, 2, 3, 0);
  begin
    SDL_RenderGeometry(renderer, nil, PSDL_Vertex(vert), Length(vert), PInteger(indices), Length(indices));
  end;

  function CreateTexture(renderer: PSDL_Renderer): PSDL_Texture;
  var
    bitmapSurface: PSDL_Surface;
  begin
    bitmapSurface := SDL_LoadBMP('mauer.bmp');
    if bitmapSurface = nil then  begin
      SDL_Log('Kann keine textur erzeugen !');
    end;

    Result := SDL_CreateTextureFromSurface(renderer, bitmapSurface);
    if Result = nil then begin
      SDL_Log('Kann Textur nicht laden ezeugen !');
    end;
    SDL_DestroySurface(bitmapSurface);
  end;

  procedure TexturQuad(renderer: PSDL_Renderer);
  const
    vert: array of TSDL_Vertex = (
      (position: (x: 500; y: 350); color: (r: 1.0; g: 1.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 0.0)),
      (position: (x: 700; y: 350); color: (r: 1.0; g: 1.0; b: 1.0; a: 1.0); tex_coord: (x: 1.0; y: 0.0)),
      (position: (x: 700; y: 450); color: (r: 1.0; g: 1.0; b: 1.0; a: 1.0); tex_coord: (x: 1.0; y: 1.0)),
      (position: (x: 500; y: 450); color: (r: 1.0; g: 1.0; b: 1.0; a: 1.0); tex_coord: (x: 0.0; y: 1.0)));
    indices: array of integer = (0, 1, 2, 2, 3, 0);
  var
    texture: PSDL_Texture;
  begin
    texture := CreateTexture(renderer);
    SDL_RenderGeometry(renderer, texture, PSDL_Vertex(vert), Length(vert), PInteger(indices), Length(indices));
    SDL_DestroyTexture(texture);
  end;



  procedure main;
  var
    window: PSDL_Window;
    renderer: PSDL_Renderer;

    quit: boolean = False;
    e: TSDL_Event;

  begin
    if not SDL_Init(SDL_INIT_VIDEO) then begin
      Halt;
    end;

    window := SDL_CreateWindow('RenderGeometrie Example', 800, 600, SDL_WINDOW_RESIZABLE);
    if window = nil then begin
      Halt;
    end;

    renderer := SDL_CreateRenderer(window, nil);
    if renderer = nil then begin
      Halt;
    end;

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
        end;
      end;

      SDL_SetRenderDrawColor(renderer, $00, $00, $00, $FF);
      SDL_RenderClear(renderer);

      Triangle(renderer);
      Quad(renderer);
      TexturQuad(renderer);

      SDL_RenderPresent(renderer);
    end;

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit;

  end;

begin
  main;
end.

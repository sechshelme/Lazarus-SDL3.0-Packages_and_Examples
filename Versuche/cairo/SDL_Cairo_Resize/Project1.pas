
program cardioid3;

uses
  SysUtils,
  SDL3,
  Cairo;

  procedure CairoDraw(Texture: PSDL_Texture; w, h: integer);
  var
    Surface: pcairo_surface_t;
    context: pcairo_t;
    Pixels: pointer;
    Pitch: integer;
  begin
    SDL_LockTexture(Texture, nil, @Pixels, @Pitch);
    Surface := cairo_image_surface_create_for_data(Pixels, CAIRO_FORMAT_ARGB32, w, h, Pitch);
    context := cairo_create(Surface);

    cairo_set_source_rgba(context, Random, Random, Random, Random);
    cairo_arc(context, w / 2, h / 2, w / 4, 0, 2 * SDL_PI_D);
    cairo_stroke(context);

    cairo_destroy(context);
    cairo_surface_destroy(Surface);
    SDL_UnlockTexture(Texture);
  end;

var
  win: PSDL_Window;
  renderer: PSDL_Renderer;
  texture: PSDL_Texture=nil;
  e: TSDL_Event;

  Width: integer = 320;
  Height: integer = 200;
  quit: boolean = False;

  function CreateTexture:PSDL_Texture;
  begin
    Result := SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, Width, Height);
  end;

begin
  SDL_Init(SDL_INIT_VIDEO);
  win := SDL_CreateWindow('Example SDL3 Cairo', Width, Height, SDL_WINDOW_RESIZABLE);
  renderer := SDL_CreateRenderer(win, nil);

  texture := CreateTexture;

  while not quit do begin
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
        SDL_EVENT_WINDOW_RESIZED: begin
          begin
            SDL_GetWindowSize(win, @Width, @Height);
            SDL_DestroyTexture(texture);
            texture := CreateTexture;
          end;
        end;
      end;
    end;

    //    SDL_SetRenderDrawColor(renderer, $FF, $FF, $FF, $FF);
    //  SDL_RenderClear(renderer);

    CairoDraw(texture, Width, Height);

    SDL_RenderTexture(renderer, texture, nil, nil);
    SDL_RenderPresent(renderer);
  end;

  SDL_DestroyTexture(texture);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(win);
  SDL_Quit;
end.

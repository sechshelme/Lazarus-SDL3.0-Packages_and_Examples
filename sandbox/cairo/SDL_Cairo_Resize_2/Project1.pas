
program cardioid3;

uses
  SysUtils,
  SDL3,
  Cairo;

  procedure CairoDraw(const ATexture: PSDL_Texture; const AWidth, AHeight: integer);
  var
    LSurface: pcairo_surface_t;
    LContext: pcairo_t;
    LPixels: pointer;
    LPitch: integer;
  const
    R = 0.15;
    X = -R;
    Y = 0;
  var
    a, xx, yy, rr: double;
  begin
    SDL_LockTexture(ATexture, nil, @LPixels, @LPitch);

    LSurface := cairo_image_surface_create_for_data(LPixels, CAIRO_FORMAT_ARGB32, AWidth, AHeight, LPitch);

    LContext := cairo_create(LSurface);

    cairo_set_source_rgb(LContext, 0.2, 0.3, 0.1);
    cairo_paint(LContext);
    cairo_scale(LContext, AWidth, AHeight);
    cairo_translate(LContext, 0.4, 0.5);
    cairo_set_line_width(LContext, 0.001);
    cairo_set_source_rgba(LContext, 0.8, 0.8, 0, 0.5);

    { Dessin d'une cardioïde par la méthode de l'enveloppe : https://mathimages.swarthmore.edu/index.php/Cardioid }

    a := 0;
    while a < 2 * PI - PI / 72 do begin
      xx := R * Cos(a);
      yy := R * Sin(a);
      rr := Sqrt((xx - X) * (xx - X) + (yy - Y) * (yy - Y));
      cairo_arc(LContext, xx, yy, rr, 0, 2 * PI);
      cairo_stroke(LContext);
      a := a + PI / 36;
    end;

    cairo_destroy(LContext);
    cairo_surface_destroy(LSurface);

    SDL_UnlockTexture(ATexture);
  end;

const
  SURFACE_WIDTH = 320;
  SURFACE_HEIGHT = 200;

var
  win: PSDL_Window;
  renderer: PSDL_Renderer;
  texture: PSDL_Texture;
  e: TSDL_Event;

  RendererWidth: integer = SURFACE_WIDTH;
  RendererHeight: integer = SURFACE_HEIGHT;
  quit: boolean = False;

begin
  SDL_Init(SDL_INIT_VIDEO);
  win := SDL_CreateWindow('Example SDL3 Cairo', SURFACE_WIDTH, SURFACE_HEIGHT, SDL_WINDOW_RESIZABLE);
  renderer := SDL_CreateRenderer(win, nil);

  SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
  SDL_RenderClear(renderer);

  texture := SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, RendererWidth, RendererHeight);

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
        SDL_EVENT_WINDOW_RESIZED: begin
          begin
            WriteLn('W. resized');
            SDL_DestroyTexture(texture);
            SDL_GetWindowSize(win, @RendererWidth, @RendererHeight);
            texture := SDL_CreateTexture(renderer, SDL_PIXELFORMAT_ARGB8888, SDL_TEXTUREACCESS_STREAMING, RendererWidth, RendererHeight);
          end;
        end;
      end;
    end;

    SDL_SetRenderDrawColor(renderer, $FF, $FF, $FF, 255);
    SDL_RenderClear(renderer);

    CairoDraw(texture, RendererWidth, RendererHeight);

    SDL_RenderTexture(renderer, texture, nil, nil);
    SDL_RenderPresent(renderer);
  end;

  SDL_DestroyTexture(texture);
  SDL_DestroyRenderer(renderer);
  SDL_DestroyWindow(win);
  SDL_Quit;
end.

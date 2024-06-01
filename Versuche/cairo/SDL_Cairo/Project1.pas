program Project1;

uses
  Cairo,
  SDL3;

  // Ist bei Lazarus nicht dabei !
  procedure cairo_surface_set_device_scale(cr: Pcairo_surface_t; x, y: double); cdecl; external LIB_CAIRO;

var
  window: PSDL_Window;
  sd_surface: PSDL_Surface;
  quit: boolean = False;
  e: TSDL_Event;
  renderer: PSDL_Renderer;
  texture: PSDL_Texture;

  window_height, window_width: integer;

  procedure Create_Cairo(surface: PSDL_Surface);
  var
    cairo_x_multiplier, cairo_y_multiplier: integer;
    cr_surface: Pcairo_surface_t;
    cr: Pcairo_t;
    xc: double = 320;
    yc: double = 240;
    radius: double = 200;
    angele1: double = 45 * (pi / 180);
    angele2: double = 180 * (pi / 180);

  begin
    cairo_x_multiplier := 1;
    cairo_y_multiplier := 1;

    cr_surface := cairo_image_surface_create_for_data(surface^.pixels, CAIRO_FORMAT_RGB24, surface^.w, surface^.h, surface^.pitch);

    cairo_surface_set_device_scale(cr_surface, cairo_x_multiplier, cairo_y_multiplier);

    cr := cairo_create(cr_surface);

    SDL_SetRenderDrawColor(renderer, 0, 0, 0, 0);
    SDL_RenderClear(renderer);

    cairo_set_source_rgba(cr, 1, 1, 1, 1.0);
    cairo_rectangle(cr, 0, 0, 640, 480);
    cairo_fill(cr);

    cairo_set_source_rgba(cr, 0, 0, 0, 1.0);
    cairo_set_line_width(cr, 10.0);
    cairo_arc(cr, xc, yc, radius, angele1, angele2);
    cairo_stroke(cr);

    cairo_set_source_rgba(cr, 1, 0.2, 0.2, 0.6);
    cairo_set_line_width(cr, 6.0);

    cairo_arc(cr, xc, yc, 10.0, 0, 2 * pi);
    cairo_fill(cr);

    cairo_arc(cr, xc, yc, radius, angele1, angele1);
    cairo_line_to(cr, xc, yc);
    cairo_arc(cr, xc, yc, radius, angele2, angele2);
    cairo_line_to(cr, xc, yc);
    cairo_stroke(cr);
  end;

begin
  SDL_Init(SDL_INIT_VIDEO);
  window := SDL_CreateWindow('An SDL3 window', 640, 480, SDL_WINDOW_RESIZABLE);
  renderer := SDL_CreateRenderer(window, nil);
  SDL_GetWindowSize(window, @window_width, @window_height);

  //      sd_surface := SDL_CreateRGBSurface(0, renderer_width, renderer_height, 32, $FF0000, $00FF00, $0000FF, 0);
  sd_surface := SDL_CreateSurface(640, 480, SDL_PIXELFORMAT_BGRA32);

  Create_Cairo(sd_surface);
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
      end;
    end;
    texture := SDL_CreateTextureFromSurface(renderer, sd_surface);

    SDL_RenderTexture(renderer, texture, nil, nil);
    SDL_RenderPresent(renderer);
  end;
  SDL_DestroySurface(sd_surface);
  SDL_DestroyWindow(window);
  SDL_Quit();
end.

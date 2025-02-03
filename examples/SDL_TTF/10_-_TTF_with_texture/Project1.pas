program Project1;

uses
  SDL3,
  SDL3_ttf;

const
  fontName = '/usr/share/fonts/truetype/ubuntu/Ubuntu-MI.ttf';

  function LoadTextTexture(rendere: PSDL_Renderer): PSDL_Texture;
  const
    hello = 'Hello World !';
  var
    font: PTTF_Font;
    fg: TSDL_Color;
    surface: PSDL_Surface;
  begin
    font := TTF_OpenFont(fontName, 20);
    fg.items := [$FF, $FF, $00, $FF];
    surface := TTF_RenderText_Blended(font, hello, Length(hello), fg);
    TTF_CloseFont(font);

    Result := SDL_CreateTextureFromSurface(rendere, surface);
    SDL_DestroySurface(surface);
  end;

  procedure main;
  var
    window: PSDL_Window;
    destRect: TSDL_FRect;
    quit: boolean = False;
    event: TSDL_Event;
    renderer: PSDL_Renderer;
    FontTexture: PSDL_Texture;
  begin
    if not SDL_init(SDL_INIT_VIDEO) then begin
      SDL_Log('Konnte SDL-VIDEO nicht laden!:  %s', SDL_GetError);
    end;

    window := SDL_CreateWindow('TTF Example', 640, 480, 0);
    renderer := SDL_CreateRenderer(window, nil);
    TTF_Init;

    FontTexture := LoadTextTexture(renderer);

    destRect.items := [0, 200, FontTexture^.w * 2, FontTexture^.h * 2];
    SDL_Log('Textsize: %4.2f x %4.2fg', destRect.w, destRect.h);

    while not quit do begin
      while SDL_PollEvent(@event) do begin
        case event._type of
          SDL_EVENT_KEY_DOWN: begin
            case event.key.key of
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

      destRect.x := SDL_sinf(SDL_GetTicks / 300) * 200 + 200;

      SDL_SetRenderDrawColorFloat(renderer, 0.2, 0.0, 0.1, SDL_ALPHA_OPAQUE);
      SDL_RenderClear(renderer);
      SDL_RenderTexture(renderer, FontTexture, nil, @destRect);
      SDL_RenderPresent(renderer);
    end;

    SDL_DestroyTexture(FontTexture);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit;
  end;

begin
  main;
end.

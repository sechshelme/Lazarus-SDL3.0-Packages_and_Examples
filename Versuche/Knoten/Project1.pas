program Project1;

{$modeswitch arrayoperators on}

uses
  heaptrc,
  ctypes,
  SDL3,
  Node,
  FillCol,
  Attribute;

  // https://discourse.libsdl.org/t/node-graph-ui-example/50993/3

  procedure main;
  var
    Attributes: array of TAttribute = nil;
    win: PSDL_Window;
    screen: PSDL_Renderer;
    i: integer;
    quit: boolean = False;
    event: TSDL_Event;
  begin
    SDL_Init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('Knoten', 1000, 1000, SDL_WINDOW_RESIZABLE);
    screen := SDL_CreateRenderer(win, nil, SDL_RENDERER_PRESENTVSYNC);


    for i := 0 to 8 do begin
      Attributes += [TAttribute.Create(screen)];
    end;

    while not quit do begin
      while SDL_PollEvent(@event) do begin
        for i := 0 to Length(Attributes) - 1 do begin
          Attributes[i].handleEvent(@event);
        end;

        case event.type_ of
          SDL_EVENT_KEY_DOWN: begin
            case event.key.keysym.sym of
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

      SDL_SetRenderDrawColor(screen, 95, 95, 125, 255);
      SDL_RenderClear(screen);

      for i := 0 to Length(Attributes) - 1 do begin
        Attributes[i].draw;
      end;

      SDL_RenderPresent(screen);
    end;

    for i := 0 to Length(Attributes) - 1 do begin
      Attributes[i].Free;
    end;

    SDL_Quit;
  end;


begin
  main;
end.

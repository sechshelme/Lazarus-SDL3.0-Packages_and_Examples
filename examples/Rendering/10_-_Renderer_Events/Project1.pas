program Project1;

uses
  SDL3, SurfaceWindow, RenderWindow, RenderGeometrieWindow;

var
  surWin:TSurfaceWindow;
  rendWin:TRenderWindow;
  GeometrieWin:TRenderGeometrieWindow;

  procedure SDLMain;
  var
    e: TSDL_Event;
    quit: boolean = False;
  begin
    while not quit do begin
      surWin.LoopHandle;
      rendWin.LoopHandle;
      GeometrieWin.LoopHandle;
      while SDL_PollEvent(@e) do begin
        surWin.EventHandle(e);
        rendWin.EventHandle(e);
        GeometrieWin.EventHandle(e);

        case e._type of
          SDL_EVENT_KEY_DOWN: begin
            case e.key.key of
              SDLK_ESCAPE: begin
                quit := True;
              end;
            end;
          end;
         SDL_EVENT_WINDOW_CLOSE_REQUESTED: begin
    //      SDL_EVENT_QUIT: begin
            quit := True;
            WriteLn('quit');
          end;
        end;
      end;

      surWin.paint;
      rendWin.paint;
      GeometrieWin.paint;
    end;
  end;

begin
  SDL_init(SDL_INIT_VIDEO);

  surWin:=TSurfaceWindow.Create;
  rendWin:=TRenderWindow.Create;
  GeometrieWin:=TRenderGeometrieWindow.Create;

  SDLMain;

  surWin.Free;
  rendWin.Free;
  GeometrieWin.Free;

  SDL_Quit;
end.

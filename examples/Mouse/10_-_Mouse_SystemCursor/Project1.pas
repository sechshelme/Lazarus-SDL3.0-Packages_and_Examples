program Project1;

// https://github.com/libsdl-org/SDL/issues/9496

uses
  SDL3,
  ctypes;

var
  renderer: PSDL_Renderer;
  customBWCursor,  customColorCursor: PSDL_Cursor;
  cursors: array of PSDL_Cursor;

  procedure LoadCursors;
  const
    size = 132;
  var
    Data, mask: array[0..size*size * 4 - 1] of TUint8;
    i: integer;
  begin
    for i := 0 to Length(Data) - 1 do begin
      Data[i] := $3;
      mask[i] := $bF;
    end;
    customBWCursor := SDL_CreateCursor(Data, mask, size, size, 0, 0);
customColorCursor:=    SDL_CreateColorCursor(nil,0,0);

    SetLength(cursors, SDL_NUM_SYSTEM_CURSORS);
    for i := 0 to Length(cursors) - 1 do begin
      cursors[i] := SDL_CreateSystemCursor(i);
    end;
  end;

  procedure Init;
  var
    win: PSDL_Window;

  begin
    SDL_Init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('Cursor', 640, 480, SDL_WINDOW_RESIZABLE);
    if win = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Konnte Fenster nicht erzeugen !');
      Halt(-1);
    end;

    renderer := SDL_CreateRenderer(win, nil);
    if renderer = nil then begin
      SDL_LogError(SDL_LOG_CATEGORY_CUSTOM, 'Kann kein SDL-Renderer erzeugen !');
      Halt(-1);
    end;

    LoadCursors;
  end;

  procedure Render;
  begin
    SDL_SetRenderDrawColor(renderer, $99, $99, $9, 255);
    SDL_RenderClear(renderer);

    SDL_RenderPresent(renderer);
  end;

  procedure Event;
  var
    quit: boolean = False;
    e: TSDL_Event;
    sym: TSDL_KeyCode;
    cursorIndex: integer = 0;
  begin
    while not quit do begin
      while SDL_PollEvent(@e) do begin
        case e.type_ of
          SDL_EVENT_MOUSE_BUTTON_DOWN: begin
            case e.button.button of
              SDL_BUTTON_LEFT: begin
                Inc(cursorIndex);
                if cursorIndex >= SDL_NUM_SYSTEM_CURSORS then begin
                  cursorIndex := 0;
                end;

                SDL_SetCursor(cursors[cursorIndex]);
              end;
              SDL_BUTTON_RIGHT: begin
                SDL_SetCursor(customBWCursor);
              end;
            end;
          end;
          SDL_EVENT_KEY_DOWN: begin
            sym := e.key.keysym.sym;
            case sym of
              SDLK_ESCAPE, SDLK_AC_BACK: begin
                quit := True;
              end;
            end;
          end;
          SDL_EVENT_QUIT: begin
            quit := True;
          end;
        end;
      end;

      Render;
    end;
  end;

  procedure Destroy;
  var
    i: integer;
  begin
    for i := 0 to Length(cursors) - 1 do begin
      SDL_DestroyCursor(cursors[i]);
    end;
    SDL_DestroyCursor(customBWCursor);
    SDL_DestroyCursor(customColorCursor);
    SDL_Quit;
  end;

begin
  Init;
  Event;
  Destroy;
end.

program Project1;

// https://github.com/Ravbug/sdl3-sample/blob/main/src/main.cpp

uses
  ctypes,
  SDL3,

  SDL_test,
  SDL_test_assert,
  SDL_test_common,
  SDL_test_compare,
  SDL_test_crc32,
  SDL_test_font,
  SDL_test_harness,
  SDL_test_log,
  SDL_test_md5,
  SDL_test_memory,
  SDL_test_fuzzer;

  {$linklib libSDL3_test.a}

var
  i: integer;
  testwin: PSDLTest_TextWindow;
  window: PSDL_Window;
  renderer: PSDL_Renderer;
  quit: boolean = False;
  e: TSDL_Event;
begin
  SDL_init(SDL_INIT_VIDEO);

  SDLTest_Log('error');
  SDLTest_LogError('error');

  WriteLn('fontheigt: ', FONT_CHARACTER_SIZE);

  for i := 0 to 20 do begin
    WriteLn(SDLTest_RandomUint8);
  end;

  SDL_CreateWindowAndRenderer('titel', 640, 480, 0, @window, @renderer);
  SDL_ShowWindow(window);

  testwin := SDLTest_TextWindowCreate(100, 100, 300, 300);

  repeat
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

      SDL_SetRenderDrawColorFloat(renderer, 0.5, 0.0, 1.0, SDL_ALPHA_OPAQUE_FLOAT);
      SDL_RenderClear(renderer);
      SDL_SetRenderDrawColorFloat(renderer, 0.5, 1.0, 1.0, SDL_ALPHA_OPAQUE_FLOAT);

      SDLTest_TextWindowAddText(testwin, 'Hello World !');
      SDLTest_TextWindowAddText(testwin, 'Hallo Welt !');
      SDLTest_TextWindowDisplay(testwin, renderer);

      SDL_RenderPresent(renderer);

    end;
  until quit;




  SDL_Quit;
end.

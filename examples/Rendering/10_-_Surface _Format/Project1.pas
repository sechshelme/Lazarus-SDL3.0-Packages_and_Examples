program Project1;

uses
  SysUtils,
  SDL3;

  procedure printSurface(sur: PSDL_Surface);
  var
    ch: pbyte;
  begin
    ch := sur^.pixels;
    SDL_Log('Pixel: %2X %2X %2X %2X ', ch[0], ch[1], ch[2], ch[3]);
    SDL_Log('format: %u', sur^.format^.format);
    SDL_Log('bit per pixel: %u    bytes per Pixel: %u', sur^.format^.bits_per_pixel, sur^.format^.bytes_per_pixel);
    SDL_Log('Rmask:  %08X   Gmask:  %08X   Bmask:  %08X   Amask:  %08X   ', sur^.format^.Rmask, sur^.format^.Gmask, sur^.format^.Bmask, sur^.format^.Amask);
    SDL_Log('Rshift: %u   Gshift: %u   Bshift: %u   Ashift: %u   ', sur^.format^.Rshift, sur^.format^.Gshift, sur^.format^.Bshift, sur^.format^.Ashift);
    SDL_Log('Rloss: %u   Gloss: %u   Bloss: %u   Aloss: %u'#10#10, sur^.format^.Rloss, sur^.format^.Gloss, sur^.format^.Bloss, sur^.format^.Aloss);
  end;

  procedure main;
  const
    pixels_1: array [0..3] of byte = ($FF, $FF, $00, $FF);
    pixels_2: array [0..0] of DWord = ($FFFF00FF);
  var
    win: PSDL_Window;
    winSurface: PSDL_Surface;
    Surface: array of PSDL_Surface = nil;
    r: TSDL_Rect;
    i: integer;
  begin
    SDL_Init(SDL_INIT_VIDEO);
    win := SDL_CreateWindow('Big / Little-Endian', 320, 200, SDL_WINDOW_RESIZABLE);
    winSurface := SDL_GetWindowSurface(win);

    SetLength(Surface, 3);

    // io.
    Surface[0] := SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA32);
    SDL_memcpy(Surface[0]^.pixels, @pixels_1, sizeof(pixels_1));

    // warped
    Surface[1] := SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA32);
    SDL_memcpy(Surface[1]^.pixels, @pixels_2, sizeof(pixels_2));

    // io.
    Surface[2] := SDL_CreateSurface(1, 1, SDL_PIXELFORMAT_RGBA8888);
    SDL_memcpy(Surface[2]^.pixels, @pixels_2, sizeof(pixels_2));

    for i := 0 to Length(Surface) - 1 do begin
      printSurface(Surface[i]);
      r.items := [10 + i * 40, 10, 30, 30];
      SDL_BlitSurfaceScaled(Surface[i], nil, winSurface, @r, SDL_SCALEMODE_NEAREST);
    end;

    SDL_UpdateWindowSurface(win);

    SDL_Delay(5000);

    SDL_DestroyWindow(win);
    SDL_DestroySurface(winSurface);

    for i := 0 to Length(Surface) - 1 do begin
      SDL_DestroySurface(Surface[i]);
    end;

    SDL_Quit();
  end;

begin
  main;
end.

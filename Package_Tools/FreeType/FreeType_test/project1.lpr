program project1;

// https://freetype.org/freetype2/docs/tutorial/step1.html
// https://chromium.googlesource.com/chromium/src/third_party/freetype2/+/VER-2-BETA2/include/fterrors.h

// https://freetype.org/freetype2/docs/tutorial/example2.cpp


//{$linklib freetype.so}
// {$linklib /usr/local/lib/x86_64-linux-gnu/libfreetype.so.6.20.1}
{$linklib libfreetype.so}




uses
  ctypes,
  freetype,
  fttypes;

procedure WriteGlyphAsTGA;
begin

end;


var
  library_: TFT_Library;
  error: TFT_Error;
  face: TFT_Face;
begin
  error := FT_Init_FreeType(@library_);
  if error <> 0 then begin
    WriteLn('Fehler: ',error);
  end;


  error := FT_New_Face(library_, '/usr/share/wine/fonts/courier.ttf', 0, @face);
  if error <> 0 then begin
    WriteLn('Fehler: ',error);
  end;

  error := FT_Set_Char_Size(nil, 0,15*64,300,400);
  if error <> 0 then begin
    WriteLn('Fehler: Set_Char_Size   ',error);
  end;

end.

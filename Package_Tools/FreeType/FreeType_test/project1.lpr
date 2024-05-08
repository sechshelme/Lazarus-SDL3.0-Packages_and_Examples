program project1;

// https://freetype.org/freetype2/docs/tutorial/step1.html

uses
  ctypes,
  freetype,
  fttypes;

  {$linklib freetype}


var
  library_: TFT_Library;
  error: TFT_Error;
  face: TFT_Face;
//face:array[0..10000] of byte;
begin
  error := FT_Init_FreeType(@library_);
  if error <> 0 then begin
    WriteLn('Fehler');
  end;

WriteLn(  PtrUInt(@face));
  error := FT_New_Face(library_, '/usr/share/wine/fonts/courier.ttf', 0, @face);

  WriteLn(  PtrUInt(@face));
  WriteLn('error: ', error);

end.

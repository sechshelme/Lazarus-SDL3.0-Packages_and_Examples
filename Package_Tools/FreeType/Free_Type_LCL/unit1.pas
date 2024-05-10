unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ctypes,
  freetype,
  ftimage,
  fttypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    bitmap:TBitmap;
    procedure Init_FreeType;
    procedure show_image;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  ImgWidth = 1280;
  ImgHeight = 960;
var
  image: array [0..ImgHeight - 1, 0..ImgWidth] of char;


procedure TForm1.FormCreate(Sender: TObject);
begin
  bitmap:=TBitmap.Create;
  bitmap.SetSize(ImgWidth,ImgHeight);
  Init_FreeType;
  ClientWidth:=ImgWidth;
  ClientHeight:=ImgHeight;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bitmap.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  show_image;
  canvas.Draw(00,00,bitmap);
end;

procedure draw_bitmap(bit: PFT_Bitmap; x: TFT_Int; y: TFT_Int);
var
  x_max, y_max, i, j, p, q: TFT_Int;
  ch: char;
begin
  x_max := x + bit^.Width;
  y_max := y + bit^.rows;

  i := x;
  p := 0;
  while (i < x_max) do begin
    Inc(i);
    Inc(p);

    j := y;
    q := 0;
    while (j < y_max) do begin
      Inc(j);
      Inc(q);

      if (i < 0) or (j < 0) or (i >= ImgWidth) or (j >= ImgHeight) then begin
        Continue;
      end;

      image[j, i] := char(bit^.buffer[q * bit^.Width + p]);
    end;
  end;
end;

procedure TForm1.Init_FreeType;
  const
    fileName = '/usr/share/fonts/truetype/freefont/FreeMono.ttf';
    //    fileName2 = '/usr/share/wine/fonts/courier.ttf';
    HelloText: PChar = 'Hello world !'+LineEnding+'Hallo Welt !';
  var
    library_: TFT_Library;
    face: TFT_Face;
    slot: TFT_GlyphSlot;

    pen: TFT_Vector;
    matrix: TFT_Matrix;

    error: TFT_Error;

    angle: double;
    target_height, n: integer;

  begin
    angle := (120.0 / 360) * 3.14159 * 2;
    target_height := ImgHeight;

    error := FT_Init_FreeType(@library_);
    if error <> 0 then begin
      WriteLn('Fehler: ', error);
    end;

    error := FT_New_Face(library_, fileName, 0, @face);
    if error <> 0 then begin
      WriteLn('Fehler: ', error);
    end;

    error := FT_Set_Char_Size(face, 50 * 15, 0, 150, 0);
    if error <> 0 then begin
      WriteLn('Fehler: Set_Char_Size   ', error);
    end;

    slot := face^.glyph;

    matrix.xx := Round(Cos(angle) * $10000);
    matrix.xy := Round(-Sin(angle) * $10000);
    matrix.yx := Round(Sin(angle) * $10000);
    matrix.yy := Round(Cos(angle) * $10000);

    pen.x :=  40000;
    pen.y := 20000;

    for n := 0 to Length(HelloText) - 1 do begin
      FT_Set_Transform(face, @matrix, @pen);

      error := FT_Load_Char(face, TFT_ULong(HelloText[n]), FT_LOAD_RENDER);
      if error <> 0 then begin
        WriteLn('Fehler: Load_Char   ', error);
      end;

      draw_bitmap(@slot^.bitmap, slot^.bitmap_left, target_height - slot^.bitmap_top);

      pen.x += slot^.advance.x;
      pen.y += slot^.advance.y;
    end;

    pen.x :=  40000;
    pen.y := 30000;

    for n := 0 to Length(HelloText) - 1 do begin
      FT_Set_Transform(face, @matrix, @pen);

      error := FT_Load_Char(face, TFT_ULong(HelloText[n]), FT_LOAD_RENDER);
      if error <> 0 then begin
        WriteLn('Fehler: Load_Char   ', error);
      end;

      draw_bitmap(@slot^.bitmap, slot^.bitmap_left, target_height - slot^.bitmap_top);

      pen.x += slot^.advance.x;
      pen.y += slot^.advance.y;
    end;

    FT_Done_Face(face);
    FT_Done_FreeType(library_);
  end;

procedure TForm1.show_image;
  var
    i, j: Integer;
    ch: Char;
begin
  for i := 0 to ImgHeight - 1 do begin
    for j := 0 to ImgWidth - 1 do begin
     bitmap.Canvas.Pixels[i,j]:=TColor( image[i,j]) or $888800;
    end;
  end;
end;

end.


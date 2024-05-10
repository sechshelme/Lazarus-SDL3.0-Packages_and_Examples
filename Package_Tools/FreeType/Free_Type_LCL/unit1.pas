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
    procedure FormPaint(Sender: TObject);
  private
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
  Init_FreeType;
  ClientWidth:=ImgWidth;
  ClientHeight:=ImgHeight;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  show_image;
end;

procedure draw_bitmap(bitmap: PFT_Bitmap; x: TFT_Int; y: TFT_Int);
var
  x_max, y_max, i, j, p, q: TFT_Int;
  ch: char;
begin
  x_max := x + bitmap^.Width;
  y_max := y + bitmap^.rows;


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

      image[j, i] := char(bitmap^.buffer[q * bitmap^.Width + p]);
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

    error := FT_Set_Char_Size(face, 150 * 15, 0, 150, 0);
    if error <> 0 then begin
      WriteLn('Fehler: Set_Char_Size   ', error);
    end;

    slot := face^.glyph;

    matrix.xx := Round(Cos(angle) * $10000);
    matrix.xy := Round(-Sin(angle) * $10000);
    matrix.yx := Round(Sin(angle) * $10000);
    matrix.yy := Round(Cos(angle) * $10000);

    pen.x :=  20000;
    pen.y := 20000;

    //WriteLn('angle:', angle:10:4);
    //WriteLn('size: matrix: ', SizeOf(matrix));
    //WriteLn('size: pen:    ', SizeOf(pen));
    //WriteLn('pen.x:    ', pen.x);
    //WriteLn('pen.y:    ', pen.y);
    //WriteLn('matrix.xx:    ', matrix.xx);
    //WriteLn('matrix.xy:    ', matrix.xy);
    //WriteLn('matrix.yx:    ', matrix.yx);
    //WriteLn('matrix.yy:    ', matrix.yy);

    for n := 0 to Length(HelloText) - 1 do begin
      FT_Set_Transform(face, @matrix, @pen);

      error := FT_Load_Char(face, TFT_ULong(HelloText[n]), FT_LOAD_RENDER);
      if error <> 0 then begin
        WriteLn('Fehler: Load_Char   ', error);
      end;

      //      Write('n: ',n,'   ');
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
      Canvas.Pixels[i,j]:=TColor( image[i,j]) or $888800;
    end;
  end;
  //for i := 0 to ImgHeight - 1 do begin
  //  for j := 0 to ImgWidth - 1 do begin
  //    case image[i,j] of
  //    #0 :ch:= ' ';
  //    #1..#127 :ch:= '+';
  //    #128..#255 :ch:= '*';
  //    end;
  //    Write(ch);
  //  end;
  //end;

end;

end.


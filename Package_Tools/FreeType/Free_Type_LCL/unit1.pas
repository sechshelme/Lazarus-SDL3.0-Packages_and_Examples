unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  OpenGLContext,
  gl,
  ctypes, freetype, ftimage, fttypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    library_: TFT_Library;
    face: TFT_Face;

    procedure Face_To_Image(angle: single);
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
  image: array [0..ImgHeight - 1, 0..ImgWidth - 1] of char;


procedure TForm1.FormCreate(Sender: TObject);
const
  fileName = '/usr/share/fonts/truetype/freefont/FreeMono.ttf';
  //    fileName2 = '/usr/share/wine/fonts/courier.ttf';
var
  error: TFT_Error;
begin
  Timer1.Enabled := False;
  Timer1.Interval := 100;
  ClientWidth := ImgWidth + 50;
  ClientHeight := ImgHeight + 50;

  OpenGLControl1.MakeCurrent();
  glClearColor(0, 0, 0, 0);

  error := FT_Init_FreeType(@library_);
  if error <> 0 then begin
    WriteLn('Fehler: ', error);
  end;

  error := FT_New_Face(library_, fileName, 0, @face);
  if error <> 0 then begin
    WriteLn('Fehler: ', error);
  end;

  error := FT_Set_Char_Size(face, 100 * 15, 0, 150, 0);
  if error <> 0 then begin
    WriteLn('Fehler: Set_Char_Size   ', error);
  end;

  Timer1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FT_Done_Face(face);
  FT_Done_FreeType(library_);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  angle: single = 0.0;
begin
  angle += 0.1;
  Face_To_Image(angle);

glDrawPixels(ImgWidth,ImgHeight,GL_LUMINANCE,GL_UNSIGNED_BYTE,@image);
OpenGLControl1.SwapBuffers;
end;

procedure draw_bitmap(bit: PFT_Bitmap; x: TFT_Int; y: TFT_Int);
var
  x_max, y_max, i, j, p, q: TFT_Int;
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

      image[j, i] :=char(byte(image[j, i])or byte(bit^.buffer[q * bit^.Width + p]));
    end;
  end;
end;

procedure TForm1.Face_To_Image(angle: single);
const
//  HelloText: PChar = 'Hello world !   Hallo Welt !';
  HelloText: PChar = 'Computer sind dumm';
var
  error: TFT_Error;
  pen: TFT_Vector;
  matrix: TFT_Matrix;
  slot: TFT_GlyphSlot;

  n: integer;
begin
  slot := face^.glyph;

  angle+=pi / 7 ;
  WriteLn(angle:4:2);
//  angle:=pi;

  matrix.xx := Round(Cos(angle) * $10000);
  matrix.xy := Round(-Sin(angle) * $10000);
  matrix.yx := Round(Sin(angle) * $10000);
  matrix.yy := Round(Cos(angle) * $10000);

  pen.x := 40000;
  pen.y := 30000;

  for n := 0 to Length(HelloText) - 1 do begin
    FT_Set_Transform(face, @matrix, @pen);

    error := FT_Load_Char(face, TFT_ULong(HelloText[n]), FT_LOAD_RENDER);
    if error <> 0 then begin
      WriteLn('Fehler: Load_Char   ', error);
    end;

    draw_bitmap(@slot^.bitmap, slot^.bitmap_left, ImgHeight - slot^.bitmap_top);

    pen.x += slot^.advance.x;
    pen.y += slot^.advance.y;
  end;
end;

end.

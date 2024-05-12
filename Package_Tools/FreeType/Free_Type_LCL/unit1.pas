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
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    library_: TFT_Library;
    face: TFT_Face;

    procedure draw_bitmap(bit: PFT_Bitmap; x: TFT_Int; y: TFT_Int);
    procedure Face_To_Image(angle: single);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

var
  image: array of byte = nil;

procedure TForm1.FormCreate(Sender: TObject);
const
//  fileName = '/usr/share/fonts/truetype/freefont/FreeMono.ttf';
//  fileName = '/usr/share/fonts/truetype/noto/NotoSansMono-Bold.ttf';
      fileName = '/usr/share/fonts/truetype/ubuntu/Ubuntu-MI.ttf';
var
  error: TFT_Error;
begin
  Timer1.Enabled := False;
  Timer1.Interval := 100;
  ClientWidth := 1600;
  ClientHeight := 1200;

  OpenGLControl1.Align := alClient;
  OpenGLControl1.AutoResizeViewport:=False;
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

  error := FT_Set_Char_Size(face, 5000, 00, 0, 350);
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

procedure TForm1.FormResize(Sender: TObject);
begin
  Timer1.Enabled := False;
  SetLength(image, OpenGLControl1.Width * OpenGLControl1.Height);
  WriteLn('OpenGlControl: ', OpenGLControl1.Width, ' x ', OpenGLControl1.Height, '     Form: ', Width, ' x ', Height);
  FillChar(image[0], Length(image), 0);
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  angle: single = 0.0;
begin
  angle += 0.3;
  Face_To_Image(angle);

  glDrawPixels(OpenGLControl1.Width, OpenGLControl1.Height, GL_LUMINANCE, GL_UNSIGNED_BYTE, Pointer(image));
  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.draw_bitmap(bit: PFT_Bitmap; x: TFT_Int; y: TFT_Int);
var
  x_max, y_max, i, j, p, q: TFT_Int;
begin
  x_max := x + bit^.Width;
  y_max := y + bit^.rows;

  i := x;
  p := -1;
  while (i < x_max) do begin
    Inc(i);
    Inc(p);

    j := y;
    q := -1;
    while (j < y_max) do begin
      Inc(j);
      Inc(q);

      if (i < 0) or (j < 0) or (i >= OpenGLControl1.Width) or (j >= OpenGLControl1.Height) then begin
        Continue;
      end;

      image[j * OpenGLControl1.Width + i] := image[j * OpenGLControl1.Width + i] or bit^.buffer[q * bit^.Width + p];
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

  matrix.xx := Round(Cos(angle) * 10000);
  matrix.xy := -Round(-Sin(angle) * 10000);
  matrix.yx := Round(Sin(angle) * 10000);
  matrix.yy := -Round(Cos(angle) * 10000);

  pen.x := 40000;
  pen.y := 50000;

  for n := 0 to Length(HelloText) - 1 do begin
    FT_Set_Transform(face, @matrix, @pen);

    error := FT_Load_Char(face, TFT_ULong(HelloText[n]), FT_LOAD_RENDER);
    if error <> 0 then begin
      WriteLn('Fehler: Load_Char   ', error);
    end;

    draw_bitmap(@slot^.bitmap, slot^.bitmap_left, OpenGLControl1.Height - slot^.bitmap_top);

    pen.x += slot^.advance.x;
    pen.y += slot^.advance.y;
  end;
end;

end.

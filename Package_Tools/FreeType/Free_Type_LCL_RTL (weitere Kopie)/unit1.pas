unit Unit1;

//{$mode objfpc}{$H+}

interface

uses
  {$IFDEF Windows}
  Windows,
  {$ENDIF}
  ctypes, dynlibs,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  OpenGLContext, gl,
  freetype, freetypehdyn,
  LazUTF8;

const
  {$IFDEF Linux}
  libc = 'c';
  {$ENDIF}

  {$IFDEF Windows}
  libc = 'msvcrt.dll';
  {$ENDIF}

// https://cplusplus.com/reference/cstdlib/mbstowcs/
//function mbstowcs(dest:PDWord; src:PChar; max :SizeInt): SizeInt; cdecl; external libc;
function mbstowcs(dest: Pointer; src: PChar; max: SizeInt): SizeInt; cdecl; external libc;
function mblen(pmb: PDWord; max: SizeInt): cint; cdecl; external libc;

function setlocale(catogory: cint; locale: PChar): PChar; cdecl; external libc;

// https://cplusplus.com/reference/cuchar/mbrtoc32/
// size_t mbrtoc32 ( char32_t * pc32, const char * pmb, size_t max, mbstate_t * ps);
function mbrtoc32(dest: PDWord; src: PChar; max: SizeInt; state: Pointer): SizeInt; cdecl; external libc;


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
    imageWidht, imageHeight: integer;
    library_: PFT_Library;
    face: PFT_Face;

    procedure draw_bitmap(var bit: FT_Bitmap; x: FT_Int; y: FT_Int);
    procedure Face_To_Image(angle: single);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

var
  image: array of byte = nil;


type
  PXChar2b = ^TXChar2b;
  TXChar2b = record
       byte1 : cuchar;
       byte2 : cuchar;
    end;

  TXChar2BArray = array of TXChar2b;
procedure UTF8toXChar2b(var output: TXChar2BArray; const s: string);
var
  StrLen: IntPtr;
  StrPtr: pbyte;
  C2BPtr: PXChar2b;
  c: byte;

begin
  StrLen := Length(s);
  SetLength(output, StrLen);
  StrPtr := @s[1];
  C2BPtr := @output[0];
  while ((PtrUInt(StrPtr) - PtrUInt(@s[1])) div SizeOf(char) < StrLen) do begin
    c := StrPtr^;
    if c < 128 then  begin
      C2BPtr^.byte1 := 0;
      C2BPtr^.byte2 := c;
      Inc(C2BPtr);
    end else if StrPtr^ < $C0 then begin
      Continue;
    end else begin
      case StrPtr^ and $F0 of
        $C0, $D0: begin
          C2BPtr^.byte1 := (c and $1C) shr 2;
          Inc(StrPtr);
          C2BPtr^.byte2 := ((c and $03) shl 6) + (StrPtr^ and $3F);
          Inc(C2BPtr);
        end;
        $E0: begin
          Inc(StrPtr);
          C2BPtr^.byte1 := ((c and $0F) shl 4) + ((StrPtr^ and $3C) shr 2);
          c := StrPtr^;
          Inc(StrPtr);
          C2BPtr^.byte2 := ((c and $03) shl 6) + (StrPtr^ and $3F);
          Inc(C2BPtr);
        end;
      end;
    end;
    Inc(StrPtr);
  end;
  SetLength(output, (PtrUInt(C2BPtr) - PtrUInt(@output[0])) div SizeOf(TXChar2b));
end;



procedure TForm1.FormCreate(Sender: TObject);
const
  //  fileName = '/usr/share/fonts/truetype/freefont/FreeMono.ttf';
  //  fileName = '/usr/share/fonts/truetype/noto/NotoSansMono-Bold.ttf';
  fileName = '/usr/share/fonts/truetype/ubuntu/Ubuntu-MI.ttf';
var
  error: FT_Error;
begin
  {$ifdef windows}
  InitializeFreetype('libfreetype-6.dll');
  {$else}
  InitializeFreetype('');
  {$endif}
  Timer1.Enabled := False;
  Timer1.Interval := 100;
  ClientWidth := 1600;
  ClientHeight := 1200;

  OpenGLControl1.Align := alClient;
  OpenGLControl1.AutoResizeViewport := False;
  OpenGLControl1.MakeCurrent();
  glClearColor(0, 0, 0, 0);

  error := FT_Init_FreeType(library_);
  if error <> 0 then begin
    WriteLn('Fehler: ', error);
  end;

  error := FT_New_Face(library_, fileName, 0, face);
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
  imageWidht := ClientWidth and not %11;
  imageHeight := ClientHeight;
  SetLength(image, imageWidht * imageHeight);
  FillDWord(image[0], Length(image) div 4, 0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
const
  angle: single = 0.0;
begin
  angle += 0.0;
  Face_To_Image(angle);

  glDrawPixels(imageWidht, imageHeight, GL_LUMINANCE, GL_UNSIGNED_BYTE, Pointer(image));
  OpenGLControl1.SwapBuffers;
end;

procedure TForm1.draw_bitmap(var bit: FT_Bitmap; x: FT_Int; y: FT_Int);
var
  x_max, y_max, ofs, i, j, p, q: FT_Int;
  buf: pbyte;
begin
  x_max := x + bit.Width;
  y_max := y + bit.rows;

  i := x;
  p := 0;
  while (i < x_max) do begin

    j := y;
    q := 0;
    while (j < y_max) do begin

      if (i >= 0) and (j >= 0) and (i < imageWidht) and (j < imageHeight) then begin
        ofs := j * imageWidht + i;
        buf := bit.buffer;
        image[ofs] := image[ofs] or buf[q * bit.Width + p];
      end;

      Inc(j);
      Inc(q);
    end;
    Inc(i);
    Inc(p);
  end;
end;

// https://onlinetools.com/utf8/convert-utf8-to-utf32

procedure TForm1.Face_To_Image(angle: single);
const
  //    HelloText: PChar = 'Hello world !  öäü ÄÖÜ ÿï ŸÏ!';
  //  HelloText: PChar = 'Computer sind dumm';
  HelloText: PChar = 'ŸAÄÖÜ';
  //  HelloText:PChar='AäÄ';
  //  HelloText:PChar=#$41#$C3#$A4#$C3#$84;
var
  error: FT_Error;
  pen: FT_Vector;
  matrix: FT_Matrix;
  slot: PFT_GlyphSlot;
  n, i: integer;

  str32: TXChar2BArray = nil;
  len: SizeInt = 0;

  //  Char2BString:TChar2BString;
  Char2BString: unicodestring;

  // https://gist.github.com/jiaoyk/c9ba7fed2a086c73aecb3edee83af0f6

begin
  Timer1.Enabled := False;

  slot := face^.glyph;

  matrix.xx := Round(Cos(angle) * 10000);
  matrix.xy := -Round(-Sin(angle) * 10000);
  matrix.yx := Round(Sin(angle) * 10000);
  matrix.yy := -Round(Cos(angle) * 10000);

  pen.x := 40000;
  pen.y := 40000;

  Char2BString := UTF8ToUTF16(HelloText);
  WriteLn(#10'Length Char2BString: ', Length(Char2BString));
  for n := 1 to Length(Char2BString) do begin
    FT_Set_Transform(face, @matrix, @pen);
    error := FT_Load_Char(face, FT_ULong(Char2BString[n]), FT_LOAD_RENDER);
    if error <> 0 then begin
      WriteLn('Fehler: Load_Char   ', error);
    end;
    draw_bitmap(slot^.bitmap, slot^.bitmap_left, OpenGLControl1.Height - slot^.bitmap_top);

    pen.x += slot^.advance.x;
    pen.y += slot^.advance.y;
  end;

  // =================

  UTF8toXChar2b(str32,HelloText);
//
//
//  len := mbstowcs(nil, HelloText, Length(str32));
//  SetLength(str32, len);
//  WriteLn('len_UTF8: ', Length(HelloText));
//  WriteLn('len_UTF32: ', len);
//  mbstowcs(PDWord(str32), HelloText, len);

  WriteLn('Input len: ', Length(HelloText));
  for i := 0 to Length(HelloText) - 1 do begin
    Write('0x', IntToHex(byte(HelloText[i]), 2), '    -    ');
  end;
  WriteLn();
  WriteLn('Output len: ', len);
  for i := 0 to Length(str32) - 1 do begin
    Write('0x', IntToHex(DWORD(str32[n].byte1 shl 8 + str32[n].byte2), 8), ' - ');
  end;

  pen.x := 40000;
  pen.y := 50000;

  for n := 0 to Length(str32) - 1 do begin
    FT_Set_Transform(face, @matrix, @pen);
    error := FT_Load_Char(face, FT_ULong(str32[n].byte1 shl 8 + str32[n].byte2), FT_LOAD_RENDER);
    if error <> 0 then begin
      WriteLn('Fehler: Load_Char   ', error);
    end;
    draw_bitmap(slot^.bitmap, slot^.bitmap_left, OpenGLControl1.Height - slot^.bitmap_top);

    pen.x += slot^.advance.x;
    pen.y += slot^.advance.y;
  end;

end;

end.

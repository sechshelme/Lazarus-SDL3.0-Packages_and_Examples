unit Unit1;

{$mode objfpc}{$H+}

interface

uses
//    SDL_pixels,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}


//SDL_COLORSPACE_SRGB = SDL_DEFINE_COLORSPACE(SDL_COLOR_TYPE_RGB,SDL_COLOR_RANGE_FULL,SDL_COLOR_PRIMARIES_BT709,SDL_TRANSFER_CHARACTERISTICS_SRGB,SDL_MATRIX_COEFFICIENTS_IDENTITY,SDL_CHROMA_LOCATION_NONE);
//#define SDL_DEFINE_COLORSPACE(type, range, primaries, transfer, matrix, chroma) \
//    (((Uint32)(type) << 28) | ((Uint32)(range) << 24) | ((Uint32)(chroma) << 20) | \
//    ((Uint32)(primaries) << 10) | ((Uint32)(transfer) << 5) | ((Uint32)(matrix) << 0))

{ TForm1 }

// #define SDL_DEFINE_PIXELFORMAT(type, order, layout, bits, bytes) ((1 << 28) | ((type) << 24) | ((order) << 20) | ((layout) << 16) |  ((bits) << 8) | ((bytes) << 0))
// SDL_PIXELFORMAT_INDEX1LSB = SDL_DEFINE_PIXELFORMAT(SDL_PIXELTYPE_INDEX1,SDL_BITMAPORDER_4321,0,1,0);
// SDL_PIXELFORMAT_INDEX1LSB = (1 shl 28) or (SDL_PIXELTYPE_INDEX1 shl 24) or (SDL_BITMAPORDER_4321 shl 20) or (0 shl 16) or (1 shl 8) or (0 shl 0);

procedure TForm1.Button1Click(Sender: TObject);
var
  slPixelPAS: TStringList;
  s: string;
  i, p: SizeInt;
  sa: TAnsiStringArray;
begin

  Memo1.Clear;
  slPixelPAS := TStringList.Create;
  slPixelPAS.LoadFromFile('SDL_pixels.pp');

  slPixelPAS[0] := 'unit SDL3_pixels;';
  for i := 0 to slPixelPAS.Count - 1 do begin
    s := slPixelPAS[i];

    // --- SDL_DEFINE_PIXELFORMAT
    p := pos('SDL_DEFINE_PIXELFORMAT', s);
    if p > 20 then  begin
      s := StringReplace(s, 'SDL_DEFINE_PIXELFORMAT', '', []);
      sa := s.Split('(,)');
      s :=
        sa[0] + '(1 shl 28) or (' +
        sa[1] + ' shl 24) or (' +
        sa[2] + ' shl 20) or (' +
        sa[3] + ' shl 16) or (' +
        sa[4] + ' shl 8) or (' +
        sa[5] + ' shl 0);';
      Memo1.Lines.Add(s);
    end;

    // --- SDL_DEFINE_COLORSPACE
    p := pos('SDL_DEFINE_COLORSPACE', s);
    if p > 20 then  begin
      s := StringReplace(s, 'SDL_DEFINE_COLORSPACE', '', []);
      sa := s.Split('(,)');
      s :=
        sa[0] + '(uint32(' +
        sa[1] + ') shl 28) or (uint32(' +
        sa[2] + ') shl 24) or (uint32(' +
        sa[3] + ') shl 20) or (uint32(' +
        sa[4] + ') shl 16) or (uint32(' +
        sa[5] + ') shl 8) or (uint32(' +
        sa[6] + ') shl 0);';
      Memo1.Lines.Add(s);
    end;

    // ---- SDL_DEFINE_PIXELFOURCC;
    p := pos('SDL_DEFINE_PIXELFOURCC', s);
    if p > 20 then  begin
      s := StringReplace(s, 'SDL_DEFINE_PIXELFOURCC', '', []);
      sa := s.Split('(,)');
      s :=
        sa[0] + '(byte(' +
        sa[1] + ') shl 0) or (byte(' +
        sa[2] + ') shl 8) or (byte(' +
        sa[3] + ') shl 16) or (byte(' +
        sa[4] + ') shl 24);';
      Memo1.Lines.Add(s);
    end;

    slPixelPAS[i] := s;
  end;
  slPixelPAS.SaveToFile('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/sdl3/packages/Tools/pas_units/sdl3_pixels.pas');
  slPixelPAS.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 1000;
  Width := 1000;
end;

end.

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
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

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
//  path='/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/Package_Tools/include_C/sdl.diff';
  path='/home/tux/Schreibtisch/von_Git/GTK4_stable/test.diff';
var
  slHeader: TStringList;
  i, j: integer;
  s: string;
begin
  Memo1.Clear;

  slHeader := TStringList.Create;
  slHeader.LoadFromFile(path);
  for j := slHeader.Count - 1 downto 0 do begin
    s := slHeader[j];
    if (pos('>  *', s) = 1) or (pos('<  *', s) = 1) or (pos('> /*', s) = 1) or (pos('< /*', s) = 1) then begin
      slHeader.Delete(j);
    end;
  end;
  slHeader.SaveToFile(path);
  slHeader.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 1000;
  Width := 1000;
end;

end.

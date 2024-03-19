unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  gtk2,
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
var
  slFile, unit_source, inc_dest: TStringList;
  i, j: integer;
  s, path: string;
  p: SizeInt;
begin
  Memo1.Clear;
  slFile := FindAllFiles('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/sdl3/packages/Tools/pas_units', '*.pas');

  for i := 0 to slFile.Count - 1 do begin
    unit_source := TStringList.Create;
    inc_dest := TStringList.Create;
    inc_dest.Add('/////////////////////////////////////////////////////////////');
    inc_dest.Add('//                                                         //');
    inc_dest.Add('//   WARNUNG, dies Datei wird automatisch durch            //');
    inc_dest.Add('//   das Program "Convert_Unit_to_pas_includes" erzeugt !  //');
    inc_dest.Add('//                                                         //');
    inc_dest.Add('/////////////////////////////////////////////////////////////');
    inc_dest.Add('');
    unit_source.LoadFromFile(slFile[i]);

    j := 0;

    // $PACKRECORDS C suchen
    repeat
      s := unit_source[j];
      p := pos('{$PACKRECORDS C}', s);
      if p <> 0 then begin
        Inc(j);
      end;
      Inc(j);
      if j >= unit_source.Count then begin
        WriteLn('Überlauf  {$PACKRECORDS C}');
        halt;
      end;
    until p <> 0;

    inc_dest.Add('{$IFDEF read_interface}');

    // implementation C suchen
    repeat
      s := unit_source[j];
      p := pos('implementation', s);
      if p = 1 then begin
        Inc(j);
      end else begin
        inc_dest.Add(unit_source[j]);
      end;
      Inc(j);
      if j >= unit_source.Count then begin
        WriteLn('Überlauf  implementation');
        halt;
      end;
    until p = 1;

    inc_dest.Add('{$ENDIF read_interface}');
    inc_dest.Add('');
    inc_dest.Add('');
    inc_dest.Add('{$IFDEF read_implementation}');
    inc_dest.Add('');

    // end. C suchen
    repeat
      s := unit_source[j];
      p := pos('end.', s);
      if p = 1 then begin
      end else begin
        inc_dest.Add(unit_source[j]);
      end;

      Inc(j);
      if (j >= unit_source.Count) and (p <> 1) then begin
        WriteLn(slFile[i], '    Überlauf  end.');
        //        halt;
      end;
    until p = 1;

    inc_dest.Add('{$ENDIF read_implementation}');


    path := ExtractFileName(slFile[i]);
    path := ChangeFileExt(path, '.inc');
    //    path := '/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/sdl3/packages/pas_includes/' + path;
    path := '/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/sdl3/packages/' + path;
    Memo1.Lines.Add(path);

    inc_dest.SaveToFile(path);

    inc_dest.Free;
    unit_source.Free;
  end;

  slFile.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 1000;
  Width := 1000;
end;

end.

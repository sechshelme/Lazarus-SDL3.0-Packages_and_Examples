unit AddSongs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, StdCtrls,FileUtil;

type

  { TAddSoundForm }

  TAddSoundForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
        ListBox:TListBox;
  public

  end;

var
  AddSoundForm: TAddSoundForm;

implementation

{$R *.lfm}

{ TAddSoundForm }

procedure TAddSoundForm.FormCreate(Sender: TObject);
var
  sl: TStringList;
begin
  ListBox:=TListBox.Create(Self);
  ListBox.Parent:=Self;
  ListBox.Left:=10;
  ListBox.Top:=10;
  ListBox.Height:=Height-20;
  ListBox.Width:=Width-20;
  ListBox.Anchors:=[akLeft,akTop,akBottom];

  sl:=FindAllDirectories('/n4800/Multimedia/Music',False);
          ListBox.Items.AddStrings(sl);
  sl.Free;
  sl:=FindAllFiles('/n4800/Multimedia/Music','*',False,faDirectory);
  ListBox.Items.AddStrings(sl);
  sl.Free;
end;

end.


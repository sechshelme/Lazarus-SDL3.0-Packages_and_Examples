unit AddSongs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, StdCtrls,
 Buttons, ComCtrls, FileUtil;

type

  { TAddSoundForm }

  TAddSoundForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    btn: TBitBtn;
        ListBox:TListBox;

        procedure btnClick(Sender: TObject);
        procedure btnCloseClick(Sender: TObject);
        function FindFile(const path:string):TStringList;
  public

  end;

var
  AddSoundForm: TAddSoundForm;

implementation

uses
  Unit1;

{$R *.lfm}

{ TAddSoundForm }

procedure TAddSoundForm.FormCreate(Sender: TObject);
var
  sl: TStringList;
begin
btn:=   TBitBtn.Create(self);
btn.Parent:=Self;
btn.Anchors:=[akBottom,akRight];
btn.Left:=ClientWidth-90;
btn.Top:=ClientHeight-100;
btn.OnClick:=@btnClick;
btn.Caption:='Add';

btn:=   TBitBtn.Create(self);
btn.Parent:=Self;
btn.Anchors:=[akBottom,akRight];
btn.Left:=ClientWidth-90;
btn.Top:=ClientHeight-50;
btn.OnClick:=@btnCloseClick;
btn.Caption:='Close';

  ListBox:=TListBox.Create(Self);
  ListBox.Parent:=Self;
  ListBox.Left:=10;
  ListBox.Top:=10;
  ListBox.Height:=ClientHeight-20;
  ListBox.Width:=ClientWidth-110;
  ListBox.Anchors:=[akLeft,akTop,akBottom,akRight];

  sl:=FindFile('/n4800/Multimedia/Music/Disco/Boney M/1981 - Boonoonoonoos');
  ListBox.Clear;
  ListBox.Items.AddStrings(sl);
  sl.Free;
end;

function TAddSoundForm.FindFile(const path: string): TStringList;
var
  Info: TRawByteSearchRec;
begin
  Result:=TStringList.Create;
if  FindFirst(path+'/*.flac',faAnyFile,Info)=0 then begin
  repeat
        Result.Add(path+'/'+ Info.Name);
  until FindNext(Info)<>0;
end;
end;

procedure TAddSoundForm.btnClick(Sender: TObject);
begin
 form1.ListBoxSongs.Items:=ListBox.Items;
end;

procedure TAddSoundForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.


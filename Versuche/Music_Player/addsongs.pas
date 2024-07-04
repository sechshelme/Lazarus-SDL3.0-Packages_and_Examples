unit AddSongs;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, FileUtil,
  SoundListBox;

type

  { TAddSoundForm }

  TAddSoundForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    btn: TBitBtn;
    FSongListBox: TSoundListBox;
    ListBoxDirectory, ListBoxMusic: TListBox;
    procedure btnClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure DownDirProc(Sender: TObject);
    procedure OpenDirProc(Sender: TObject);
  public
    property SongListBox: TSoundListBox write FSongListBox;
    procedure FindNewFile(const path:String);
  end;

var
  SoundAddForm: TAddSoundForm;

implementation

{$R *.lfm}

procedure TAddSoundForm.FormCreate(Sender: TObject);
var
  panel: TPanel;
  spliter: TSplitter;
begin
  Width:=Screen.Width * 2 div 3;
  Height:=Screen.Height * 2 div 3;

  btn := TBitBtn.Create(self);
  btn.Parent := Self;
  btn.Anchors := [akBottom, akRight];
  btn.Left := ClientWidth - 90;
  btn.Top := ClientHeight - 100;
  btn.OnClick := @btnClick;
  btn.Caption := 'Add';

  btn := TBitBtn.Create(self);
  btn.Parent := Self;
  btn.Anchors := [akBottom, akRight];
  btn.Left := ClientWidth - 90;
  btn.Top := ClientHeight - 50;
  btn.OnClick := @btnCloseClick;
  btn.Caption := 'Close';

  panel := TPanel.Create(self);
  panel.Parent := Self;
  panel.Left := 10;
  panel.Top := 10;
  panel.Height := ClientHeight - 60;
  panel.Width := ClientWidth - 110;
  panel.Anchors := [akLeft, akTop, akBottom, akRight];

  spliter := TSplitter.Create(panel);
  spliter.Parent := panel;
  spliter.Align := alLeft;
  spliter.MinSize := 50;

  ListBoxDirectory := TListBox.Create(panel);
  ListBoxDirectory.Parent := panel;
  ListBoxDirectory.Width := panel.Width div 2;
  ListBoxDirectory.Align := alLeft;
  ListBoxDirectory.Constraints.MinWidth:=50;

  ListBoxMusic := TListBox.Create(panel);
  ListBoxMusic.Parent := panel;
  ListBoxMusic.Align := alClient;

  btn := TBitBtn.Create(self);
  btn.Parent := Self;
  btn.Anchors := [akBottom, akLeft];
  btn.Left := 5;
  btn.Top := ClientHeight - 35;
  btn.OnClick := @OpenDirProc;
  btn.Caption := 'Open';

  btn := TBitBtn.Create(self);
  btn.Parent := Self;
  btn.Anchors := [akBottom, akLeft];
  btn.Left := 5+80;
  btn.Top := ClientHeight - 35;
  btn.OnClick := @DownDirProc;
  btn.Caption := 'Up';

  FindNewFile('/n4800/Multimedia/Music/Disco/Boney M');
end;

procedure TAddSoundForm.btnClick(Sender: TObject);
begin
  if FSongListBox <> nil then begin
    FSongListBox.Items.AddStrings( ListBoxMusic.Items);
  end;
end;

procedure TAddSoundForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TAddSoundForm.OpenDirProc(Sender: TObject);
var
  index: Integer;
begin
   index:=ListBoxDirectory.ItemIndex;
   if index>=0 then FindNewFile(ListBoxDirectory.Items[index]);
end;

procedure TAddSoundForm.DownDirProc(Sender: TObject);


function ExtractParentPath(const fullPath: string): string;
var
  lastSlashPos: Integer;
begin
  lastSlashPos := LastDelimiter('/', fullPath);
  Result := Copy(fullPath, 1, lastSlashPos - 1);
  lastSlashPos := LastDelimiter('/', Result);
  Result := Copy(Result, 1, lastSlashPos);
end;

var
  s:String='/n4800/Multimedia/Music/Disco/Boney M';
begin
  WriteLn(s);
  s:=ExtractParentPath(s);
  WriteLn(s);
  s:=ExtractParentPath(s);
  WriteLn(s);
  s:=ExtractParentPath(s);
  WriteLn(s);
  s:=ExtractParentPath(s);
  WriteLn(s);
  s:=ExtractParentPath(s);
  WriteLn(s);
  s:=ExtractParentPath(s);
  WriteLn(s);
  s:=ExtractParentPath(s);
  WriteLn(s);


end;



procedure TAddSoundForm.FindNewFile(const path: String);
var
  sl: TStringList;
begin
//  sl := FindAllDirectories('/n4800/Multimedia/Music/Disco/Boney M', False);
  sl := FindAllDirectories(path, False);
  ListBoxDirectory.Clear;
  ListBoxDirectory.Items.AddStrings(sl);
  sl.Free;

//  sl := FindAllFiles('/n4800/Multimedia/Music/Disco/Boney M/1981 - Boonoonoonoos', '*.flac', False);
  sl := FindAllFiles(path, '*.flac', False);
  ListBoxMusic.Clear;
  ListBoxMusic.Items.AddStrings(sl);
  sl.Free;
end;

end.

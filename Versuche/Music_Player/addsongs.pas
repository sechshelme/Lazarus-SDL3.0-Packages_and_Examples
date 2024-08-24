unit AddSongs;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, FileUtil, StrUtils,
  Common,
  SoundListBox;

type

  { TAddSoundForm }

  TAddSoundForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    TitelLabel: TLabel;
    FSongListBox: TSoundListBox;
    ListBoxDirectory, ListBoxMusic: TListBox;
    procedure AddSongsProc(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure DownDirProc(Sender: TObject);
    procedure OpenDirProc(Sender: TObject);
    procedure SelectAllProc(Sender: TObject);
  public
    property SongListBox: TSoundListBox write FSongListBox;
    procedure FindNewFile(const path: string);
  end;

var
  SoundAddForm: TAddSoundForm;

implementation

{$R *.lfm}

procedure TAddSoundForm.FormCreate(Sender: TObject);
var
  btn: TBitBtn;
  panel: TPanel;
  spliter: TSplitter;
begin
  Width := Screen.Width * 2 div 3;
  Height := Screen.Height * 2 div 3;

  btn := TBitBtn.Create(self);
  btn.Parent := Self;
  btn.Anchors := [akBottom, akRight];
  btn.Left := ClientWidth - 90;
  btn.Top := ClientHeight - 150;
  btn.OnClick := @SelectAllProc;
  btn.Caption := 'Select All';

  btn := TBitBtn.Create(self);
  btn.Parent := Self;
  btn.Anchors := [akBottom, akRight];
  btn.Left := ClientWidth - 90;
  btn.Top := ClientHeight - 100;
  btn.OnClick := @AddSongsProc;
  btn.Caption := 'Add';

  btn := TBitBtn.Create(self);
  btn.Parent := Self;
  btn.Anchors := [akBottom, akRight];
  btn.Left := ClientWidth - 90;
  btn.Top := ClientHeight - 50;
  btn.OnClick := @btnCloseClick;
  btn.Caption := 'Close';

  TitelLabel:=TLabel.Create(Self);
  TitelLabel.Parent:=Self;
  TitelLabel.Caption:='titel';

  panel := TPanel.Create(self);
  panel.Parent := Self;
  panel.Left := 10;
  panel.Top := TitelLabel.Height+10;
  panel.Height := ClientHeight - TitelLabel.Height - 60;
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
  ListBoxDirectory.Constraints.MinWidth := 50;
  ListBoxDirectory.OnDblClick := @OpenDirProc;

  ListBoxMusic := TListBox.Create(panel);
  ListBoxMusic.Parent := panel;
  ListBoxMusic.MultiSelect := True;
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
  btn.Left := 5 + 80;
  btn.Top := ClientHeight - 35;
  btn.OnClick := @DownDirProc;
  btn.Caption := 'Up';

  FindNewFile(MusicDir);
end;

procedure TAddSoundForm.AddSongsProc(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to ListBoxMusic.Count - 1 do begin
    if ListBoxMusic.Selected[i] then begin
      FSongListBox.Add(ListBoxMusic.Items[i]);
    end;
  end;
end;

procedure TAddSoundForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TAddSoundForm.OpenDirProc(Sender: TObject);
var
  index: integer;
  Dir: string;
begin
  index := ListBoxDirectory.ItemIndex;
  if index >= 0 then begin
    Dir := ListBoxDirectory.Items[index];
    if Dir = '..' then begin
      DownDirProc(Sender);
    end else begin
      MusicDir:=Dir;
      FindNewFile(MusicDir);
    end;
  end;
end;

procedure TAddSoundForm.SelectAllProc(Sender: TObject);
begin
  ListBoxMusic.SelectAll;
end;

procedure TAddSoundForm.DownDirProc(Sender: TObject);


  function ParentPath(const Path: string): string;
  begin
    Result := ExpandFileName(IncludeTrailingPathDelimiter(Path) + '..');

    //Result := ExtractFilePath(ExcludeTrailingPathDelimiter(Path));
    //if Result = '' then begin
    //  Result := Path;
    //end;
  end;

begin
  MusicDir := ParentPath(MusicDir);
  FindNewFile(MusicDir);
end;



procedure TAddSoundForm.FindNewFile(const path: string);
var
  sl: TStringList;
begin
  TitelLabel.Caption:=MusicDir;

  //  sl := FindAllDirectories('/n4800/Multimedia/Music/Disco/Boney M', False);
  sl := FindAllDirectories(path, False);
  ListBoxDirectory.Clear;
  ListBoxDirectory.Items.AddStrings(sl);
  ListBoxDirectory.Items.Insert(0, '..');
  ListBoxDirectory.Sorted:=True;
  sl.Free;

  //  sl := FindAllFiles('/n4800/Multimedia/Music/Disco/Boney M/1981 - Boonoonoonoos', '*.flac', False);
  sl := FindAllFiles(path, '*.flac;*.wav;*.mp3', False);
  ListBoxMusic.Clear;
  ListBoxMusic.Items.AddStrings(sl);
  ListBoxMusic.Sorted:=True;
  sl.Free;
end;

end.

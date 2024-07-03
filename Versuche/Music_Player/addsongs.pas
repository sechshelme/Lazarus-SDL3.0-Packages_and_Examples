unit AddSongs;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls, StdCtrls,
  Buttons, ComCtrls, ExtCtrls, FileUtil,
  SoundListBox;

type
  TAddSoundForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    btn: TBitBtn;
    FSongListBox: TSoundListBox;
    ListBoxDirectory, ListBoxMusic: TListBox;
    procedure btnClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  public
    property SongListBox: TSoundListBox write FSongListBox;
  end;

var
  SoundAddForm: TAddSoundForm;

implementation

{$R *.lfm}

procedure TAddSoundForm.FormCreate(Sender: TObject);
var
  sl: TStringList;
  panel: TPanel;
  spliter: TSplitter;
begin
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
  panel.Height := ClientHeight - 20;
  panel.Width := ClientWidth - 110;
  panel.Anchors := [akLeft, akTop, akBottom, akRight];

  spliter := TSplitter.Create(panel);
  spliter.Parent := panel;
  spliter.Align := alLeft;
  spliter.MinSize := 1;

  ListBoxDirectory := TListBox.Create(panel);
  ListBoxDirectory.Parent := panel;
  ListBoxDirectory.Width := panel.Width div 2;
  ListBoxDirectory.Align := alLeft;

  ListBoxMusic := TListBox.Create(panel);
  ListBoxMusic.Parent := panel;
  ListBoxMusic.Align := alClient;


  sl := FindAllDirectories('/n4800/Multimedia/Music/Disco/Boney M', False);
  ListBoxDirectory.Clear;
  ListBoxDirectory.Items.AddStrings(sl);
  sl.Free;

  sl := FindAllFiles('/n4800/Multimedia/Music/Disco/Boney M/1981 - Boonoonoonoos', '*.flac', False);
  ListBoxMusic.Clear;
  ListBoxMusic.Items.AddStrings(sl);
  sl.Free;
end;

procedure TAddSoundForm.btnClick(Sender: TObject);
begin
  if FSongListBox <> nil then begin
    FSongListBox.Items := ListBoxMusic.Items;
  end;
end;

procedure TAddSoundForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.

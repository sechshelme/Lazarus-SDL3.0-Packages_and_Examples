unit PlayBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Buttons,
  Common;

type
  TPlayBoxEvent = procedure(cmd: Tcommand) of object;

  { TPlayBox }

  TPlayBox = class(TPanel)
  private
    FOnPlayBoxEvent: TPlayBoxEvent;
    procedure BtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property OnPlayBoxEvent: TPlayBoxEvent read FOnPlayBoxEvent write FOnPlayBoxEvent;
  private
    BtnPlay, BtnNext: TBitBtn;
  end;

implementation

{ TPlayBox }

procedure TPlayBox.BtnClick(Sender: TObject);
begin
  if OnPlayBoxEvent <> nil then  begin
    OnPlayBoxEvent(Tcommand(TBitBtn(Sender).Tag));
  end;
end;

// ▶️ ⏸⏯ ⏹⏺ ⏭   ⏮   ⏩⏪

constructor TPlayBox.Create(AOwner: TComponent);
var
  i: Integer;
  Btn: TBitBtn;
begin
  inherited Create(AOwner);

  Width := 200;
  Height := 290;

  for i := 0 to Length(PlayCmdProp) - 1 do begin
    Btn := TBitBtn.Create(Self);
    Btn.Caption := PlayCmdProp[i].Caption;
    Btn.Tag := PtrInt(PlayCmdProp[i].cmd);
    Btn.OnClick := @BtnClick;
    Btn.Top:=i*30;

    Btn.Parent := Self;
  end;

  //BtnPlay := TBitBtn.Create(Self);
  //BtnPlay.Parent := Self;
  //BtnPlay.Caption := '▶️';
  //BtnPlay.Tag := PtrInt(cmPlay);
  //BtnPlay.OnClick := @BtnClick;
  //
  //BtnNext := TBitBtn.Create(Self);
  //BtnNext.Top := 30;
  //BtnNext.Parent := Self;
  //BtnNext.Caption := '⏭️';
  //BtnNext.Tag := PtrInt(cmNext);
  //BtnNext.OnClick := @BtnClick;
end;

end.

unit PlayBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Buttons,
  Common;

type
  TPlayBoxEvent = procedure(cmd: Tcommand) of object;
  TBox = class(TPanel)
  private
    FOnPlayBoxEvent: TPlayBoxEvent;
    procedure BtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property OnPlayBoxEvent: TPlayBoxEvent read FOnPlayBoxEvent write FOnPlayBoxEvent;
  private
    procedure LoadButtons(const props: TcmdProps);
  end;


  TEditBox=class(TBox)
    constructor Create(AOwner: TComponent); override;
  end;

  TPlayBox=class(TBox)
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TBox }

procedure TBox.BtnClick(Sender: TObject);
begin
  if OnPlayBoxEvent <> nil then  begin
    OnPlayBoxEvent(Tcommand(TBitBtn(Sender).Tag));
  end;
end;

// 🐑🐑🐑🐑🐑🐑🐑▶️ ⏸ ⏯ ⏹ ⏺ ⏭ ⏮ ⏩ ⏪
// ▶️ ⏸⏯ ⏹⏺ ⏭⏮ ⏩⏪
// ▶️ ⏸⏯ ⏹⏺ ⏭   ⏮   ⏩⏪

constructor TBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TBox.LoadButtons(const props: TcmdProps);
var
  i: Integer;
  Btn: TBitBtn;
  propsCount: SizeInt;
begin
  propsCount:=Length(props);

  Width := 100;
  Height :=  propsCount*30+20;
  Left:=-100;
  Left:=TControl( Owner).Width-Width;

  for i := 0 to propsCount - 1 do begin
    Btn := TBitBtn.Create(Self);
    Btn.Caption := props[i].Caption;
    Btn.Tag := PtrInt(props[i].cmd);
    Btn.OnClick := @BtnClick;
    Btn.Left:=10;
    Btn.Top:=i*30+10;
    Btn.Parent := Self;
  end;
end;

{ TEditBox }

constructor TEditBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Anchors:=[akTop,akRight];
  LoadButtons(EditCmdProb);
end;

{ TPlayBox }

constructor TPlayBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Anchors:=[akBottom,akRight];
  LoadButtons(PlayCmdProp);
  Top:=TControl(Owner).Height-Height;
end;

end.

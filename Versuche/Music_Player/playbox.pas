unit PlayBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls,Buttons;

type
  TPlayBoxEvent = procedure(AName: string) of object;

  { TPlayBox }

  TPlayBox = class(TPanel)
  private
    FOnPlayBoxEvent: TPlayBoxEvent;
    procedure BtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property OnPlayBoxEvent: TPlayBoxEvent read FOnPlayBoxEvent write FOnPlayBoxEvent;
    private
      BtnPlay,BtnNext:TBitBtn;
  end;

implementation

{ TPlayBox }

procedure TPlayBox.BtnClick(Sender: TObject);
begin
if OnPlayBoxEvent<>nil then  OnPlayBoxEvent(TBitBtn(Sender).Name);
end;

// ▶️⏸⏯⏹⏺⏭⏮⏩⏪

constructor TPlayBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width:=200;
  Height:=90;

  BtnPlay:=TBitBtn.Create(Self);
  BtnPlay.Parent:=Self;
  BtnPlay.Caption:='▶️';
  BtnPlay.Name:='play';
  BtnPlay.OnClick:=@BtnClick;

  BtnNext:=TBitBtn.Create(Self);
  BtnNext.Top:=30;
  BtnNext.Parent:=Self;
  BtnNext.Caption:='⏭️';
  BtnNext.Name:='next';
  BtnNext.OnClick:=@BtnClick;
end;

end.

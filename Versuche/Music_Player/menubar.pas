unit MenuBar;

interface

uses  Classes, Menus,
  Common;

type
//  TMenuBarEvent = procedure(AName: string) of object;
  TMenuBarEvent = procedure(cmd: Tcommand) of object;

  { TMenuBar }

  TMenuBar = class(TMainMenu)
  private
    FOnMenuBarEvent: TMenuBarEvent;
    procedure MenuBarClick(Sender: TObject);
    procedure AddMenuFromProps(ACaption: string; props: TcmdProps);
  public
    property OnMenuBarEvent: TMenuBarEvent read FOnMenuBarEvent write FOnMenuBarEvent;
    constructor Create(AOwner: TComponent); override;
  end;


implementation

{ TMenuBar }

constructor TMenuBar.Create(AOwner: TComponent);
var
  mmi, smi: TMenuItem;
begin
  inherited Create(AOwner);

  // --- Datei
  mmi := TMenuItem.Create(self);
  mmi.Caption := 'Datei';
  Items.Add(mmi);

  smi := TMenuItem.Create(self);
  smi.Caption := 'Neu';
  mmi.Add(smi);

  smi := TMenuItem.Create(self);
  smi.Caption := '-';
  mmi.Add(smi);

  smi := TMenuItem.Create(self);
  smi.Caption := 'Speichern';
  mmi.Add(smi);

  smi := TMenuItem.Create(self);
  smi.Caption := 'Öffnen';
  mmi.Add(smi);

  smi := TMenuItem.Create(self);
  smi.Caption := '-';
  mmi.Add(smi);

  smi := TMenuItem.Create(self);
  smi.Caption := 'Beenden';
  smi.Name := 'beenden';
  smi.OnClick := @MenuBarClick;
  mmi.Add(smi);

  // --- Edit
  AddMenuFromProps('Edit', EditCmdProb);

  // --- Play
  AddMenuFromProps('Play', PlayCmdProp);


  // --- Optionen
  mmi := TMenuItem.Create(self);
  mmi.Caption := 'Optionen';
  Items.Add(mmi);

  // --- Hilfe
  mmi := TMenuItem.Create(self);
  mmi.Caption := 'Hilfe';
  Items.Add(mmi);

  smi := TMenuItem.Create(self);
  smi.Caption := 'About...';
  smi.Name := 'about';
  smi.OnClick := @MenuBarClick;
  mmi.Add(smi);

end;

procedure TMenuBar.MenuBarClick(Sender: TObject);
begin
  if OnMenuBarEvent <> nil then  begin
    OnMenuBarEvent(Tcommand(TMainMenu(Sender).Tag));
  end;
end;

procedure TMenuBar.AddMenuFromProps(ACaption: string; props: TcmdProps);
var
  mmi, smi: TMenuItem;
  i: Integer;
begin
  mmi := TMenuItem.Create(Self);
  mmi.Caption := ACaption;
  Items.Add(mmi);

  for i := 0 to Length(props) - 1 do begin
    smi := TMenuItem.Create(self);
    smi.Caption := props[i].Caption;
    smi.Tag:=PtrInt(props[i].cmd);
    smi.OnClick := @MenuBarClick;
    mmi.Add(smi);
  end;
end;

end.

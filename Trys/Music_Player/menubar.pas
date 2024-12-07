unit MenuBar;

interface

uses  Classes, Menus,
  Common;

type
  TMenuBarEvent = procedure(cmd: Tcommand) of object;

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

constructor TMenuBar.Create(AOwner: TComponent);
var
  mmi, smi: TMenuItem;
begin
  inherited Create(AOwner);

  // --- Datei
  AddMenuFromProps('Datei', FileCmdProb);

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
  i: integer;
begin
  mmi := TMenuItem.Create(Self);
  mmi.Caption := ACaption;
  Items.Add(mmi);

  for i := 0 to Length(props) - 1 do begin
    smi := TMenuItem.Create(self);
    smi.Caption := props[i].Caption;
    smi.Tag := PtrInt(props[i].cmd);
    smi.OnClick := @MenuBarClick;
    mmi.Add(smi);
  end;
end;

end.

unit MenuBar;

interface

uses  Classes,
  Menus;

type

  TMenuBarEvent = procedure(AName: string) of object;
  { TMenuBar }

  TMenuBar = class(TMainMenu)

  private
    FOnMenuBarEvent: TMenuBarEvent;
    procedure MenuBarClick(Sender: TObject);
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

  // --- Play
  mmi := TMenuItem.Create(self);
  mmi.Caption := 'Play';
  Items.Add(mmi);

  smi := TMenuItem.Create(self);
  smi.Caption := 'Play';
  smi.Name := 'play';
  smi.OnClick := @MenuBarClick;
  mmi.Add(smi);


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
    OnMenuBarEvent(TMainMenu(Sender).Name);
  end;
end;

end.

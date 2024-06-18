unit SoundListBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Dialogs;

type

  { TSoundListBox }

  TSoundListBox = class(TListBox)
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Add;
    procedure Remove;
    procedure Down;
    procedure Up;
    function Next: boolean;
    function Prev: boolean;
    function GetTitle: string;
  end;

implementation

constructor TSoundListBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Anchors := [akTop, akLeft, akBottom, akRight];
  Top := 50;
  Left := 16;
end;

procedure TSoundListBox.Add;
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  OpenDialog.Options := OpenDialog.Options + [ofAllowMultiSelect];
  if OpenDialog.Execute then begin
    Items.AddStrings(OpenDialog.Files);
  end;
  OpenDialog.Free;
end;

procedure TSoundListBox.Remove;
var
  index: integer;
begin
  index := ItemIndex;
  if (index > 0) and (index < Count) then  begin
    Items.Delete(index);
  end;
end;

procedure TSoundListBox.Down;
var
  index: integer;
begin
  index := ItemIndex;
  if index = -1 then begin
    Exit;
  end;
  if index < Count - 1 then  begin
    Items.Move(index, index + 1);
    ItemIndex := index + 1;
  end;
end;

procedure TSoundListBox.Up;
var
  index: integer;
begin
  index := ItemIndex;
  if index = -1 then begin
    Exit;
  end;
  if index > 0 then  begin
    Items.Move(index, index - 1);
    ItemIndex := index - 1;
  end;
end;

function TSoundListBox.Next: boolean;
var
  index: integer;
begin
  if Count <= 0 then begin
    Result := False;
  end else begin
    Result := True;
  end;
  index := ItemIndex;
  Inc(index);
  if index >= Items.Count then begin
    index := 0;
  end;
  ItemIndex := index;
end;

function TSoundListBox.Prev: boolean;
begin

end;

function TSoundListBox.GetTitle: string;
begin
  if ItemIndex >= 0 then begin
    Result := Items[ItemIndex];
  end else begin
    Result := '';
  end;
end;

end.

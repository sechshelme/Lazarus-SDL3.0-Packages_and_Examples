unit SoundListBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, Controls, Dialogs, ComCtrls,
  SDL3, SDL3_mixer;

type

  { TSoundListBox }

  TSoundListBox = class(TListBox)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add;
    procedure Remove;
    procedure Down;
    procedure Up;
    function Next: boolean;
    function Prev: boolean;

    procedure Play;

    procedure SetTrackBar(ATrackBar: TTrackBar);
    function getMusic:PMix_Music;

    function GetTitle: string;
    procedure LoadNewMusic(const titel: string);
  private
    music: PMix_Music;
    FTrackBar: TTrackBar;
  end;

implementation

constructor TSoundListBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Anchors := [akTop, akLeft, akBottom, akRight];
  Top := 50;
  Left := 16;

  music := nil;
end;

destructor TSoundListBox.Destroy;
begin

  inherited Destroy;
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
    Exit;
  end else begin
    Result := True;
  end;
  index := ItemIndex;
  Inc(index);
  if index >= Items.Count then begin
    index := 0;
  end;
  ItemIndex := index;
  if Result then begin
    LoadNewMusic(GetTitle);
  end;
end;

//function TSoundListBox.Prev(music: PMix_Music): boolean;
function TSoundListBox.Prev: boolean;
var
  musicPos: double;
begin
  if Count <= 0 then begin
    Result := False;
    Exit;
  end else begin
    Result := True;
  end;
  musicPos := Mix_GetMusicPosition(music);
  if musicPos > 1.0 then begin
    Mix_SetMusicPosition(0.1);
    Result := False;
  end else begin
    if ItemIndex = 0 then begin
      ItemIndex := Count - 1;
    end else begin
      ItemIndex := ItemIndex - 1;
    end;
  end;
  if Result then begin
    LoadNewMusic(GetTitle);
  end;
end;

procedure TSoundListBox.Play;
var
  index: integer;
  s: string;
begin
  if music <> nil then begin
    Mix_FreeMusic(music);
    music := nil;
  end;
  index := ItemIndex;
  if Count > 0 then begin
    if index < 0 then begin
      index := 0;
      ItemIndex := index;
    end;
    s := Items[index];
    LoadNewMusic(s);
  end;
end;

procedure TSoundListBox.SetTrackBar(ATrackBar: TTrackBar);
begin
  FTrackBar := ATrackBar;
end;

function TSoundListBox.getMusic: PMix_Music;
begin
 Result:=music;
end;

function TSoundListBox.GetTitle: string;
begin
  if ItemIndex >= 0 then begin
    Result := Items[ItemIndex];
  end else begin
    Result := '';
  end;
end;

procedure TSoundListBox.LoadNewMusic(const titel: string);
begin
  if music <> nil then begin
    Mix_FreeMusic(music);
  end;
  music := Mix_LoadMUS(PChar(titel));
  if music = nil then begin
    SDL_Log('Konnte Musik nicht laden: %s', Mix_GetError);
  end;


  //  Mix_PlayMusic(music, 1);
  Mix_FadeInMusic(music, 1, 3000);
  if FTrackBar <> nil then begin
  FTrackBar.Max := Trunc(Mix_MusicDuration(music) * 1000);
  FTrackBar.Position := 0;
  end;
end;

end.

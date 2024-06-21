unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SDL3, SDL3_mixer,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, LCLType,
  Buttons, ExtCtrls, ComCtrls, Types, FileUtil,
  SoundListBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtnAdd: TBitBtn;
    BitBtnPlay: TBitBtn;
    BitBtnNext: TBitBtn;
    BitBtnPrev: TBitBtn;
    BitBtnRemove: TBitBtn;
    BitBtnUp: TBitBtn;
    BitBtnDown: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    procedure BitBtnAddClick(Sender: TObject);
    procedure BitBtnDownClick(Sender: TObject);
    procedure BitBtnNextClick(Sender: TObject);
    procedure BitBtnPlayClick(Sender: TObject);
    procedure BitBtnPrevClick(Sender: TObject);
    procedure BitBtnRemoveClick(Sender: TObject);
    procedure BitBtnUpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    ListBox: TSoundListBox;
    music: PMix_Music;
    procedure LoadNewMusic(const titel: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  sl: TStringList;
begin
  SDL_Init(SDL_INIT_AUDIO);
  Mix_OpenAudio(0, nil);
  music := nil;

  ListBox := TSoundListBox.Create(self);
  ListBox.Anchors := [akTop, akLeft, akBottom, akRight];
  ListBox.Width := ClientWidth - 150;
  ListBox.Height := ClientHeight - 70;
  ListBox.Parent := self;

  sl := FindAllFiles('/n4800/Multimedia/Music/Disco/C.C. Catch/1986 - Catch The Catch', '*.flac');
  ListBox.Items.AddStrings(sl);
  sl.Free;
  ListBox.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_1.wav');
  ListBox.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_2.wav');
  ListBox.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_3.wav');
  ListBox.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_4.wav');
  ListBox.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_5.wav');
  ListBox.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_6.wav');
  ListBox.Items.Add('/n4800/Multimedia/Music/Disco/Boney M/1981 - Boonoonoonoos/01 - Boonoonoonoos.flac');

  Timer1.Interval := 100;
  TrackBar1.TickStyle := tsNone;
  Width := 1024;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Mix_FreeMusic(music);
  Mix_CloseAudio;
  SDL_Quit;
end;

procedure TForm1.LoadNewMusic(const titel: string);
begin
  if music <> nil then begin
    Mix_FreeMusic(music);
  end;
  music := Mix_LoadMUS(PChar(titel));
  if music = nil then begin
    SDL_Log('Konnte Musik nicht laden: %s', Mix_GetError);
  end;


//  Mix_PlayMusic(music, 1);
  Mix_FadeInMusic(music,1,3000);
  TrackBar1.Max := Trunc(Mix_MusicDuration(music) * 1000);
  TrackBar1.Position := 0;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Mix_SetMusicPosition(TrackBar1.Position / 1000);
  WriteLn('change');
end;



procedure TForm1.Timer1Timer(Sender: TObject);
var
  t_length, t_pos: double;
  index: integer;
  s: string;
  ChangeProc: TNotifyEvent;
begin
  if ListBox.Count > 0 then begin
    if music <> nil then begin
      t_length := Mix_MusicDuration(music);
      WriteStr(s, t_length: 6: 1);
      Label1.Caption := s;
      t_pos := Mix_GetMusicPosition(music);
      WriteStr(s, t_pos: 6: 1);
      Label3.Caption := s;
      ChangeProc := TrackBar1.OnChange;
      TrackBar1.OnChange := nil;
      TrackBar1.Position := Trunc(Mix_GetMusicPosition(music) * 1000);
      TrackBar1.OnChange := ChangeProc;

      if t_pos >= t_length then begin
        if ListBox.Next then  begin
          LoadNewMusic(ListBox.GetTitle);
        end;
      end;
    end;
  end;
end;

procedure TForm1.BitBtnAddClick(Sender: TObject);
begin
  ListBox.Add;
end;

procedure TForm1.BitBtnRemoveClick(Sender: TObject);
begin
  ListBox.Remove;
end;

procedure TForm1.BitBtnUpClick(Sender: TObject);
begin
  ListBox.Up;
end;

procedure TForm1.BitBtnDownClick(Sender: TObject);
begin
  ListBox.Down;
end;

procedure TForm1.BitBtnPlayClick(Sender: TObject);
var
  index: integer;
  s: string;
begin
  if music <> nil then begin
    Mix_FreeMusic(music);
    music := nil;
  end;
  index := ListBox.ItemIndex;
  if index >= 0 then  begin
    s := ListBox.Items[index];
    LoadNewMusic(s);
  end;
end;

procedure TForm1.BitBtnNextClick(Sender: TObject);
begin
  if ListBox.Next then  begin
    LoadNewMusic(ListBox.GetTitle);
  end;
end;

procedure TForm1.BitBtnPrevClick(Sender: TObject);
begin
  if ListBox.Prev(music) then begin
    LoadNewMusic(ListBox.GetTitle);
  end;
end;

end.

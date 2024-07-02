unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SDL3, SDL3_mixer,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, LCLType,
  Buttons, ExtCtrls, ComCtrls, Menus, Types, FileUtil,
Common,  MenuBar, SoundListBox, PlayBox;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    MainMenu: TMenuBar;
    EditBox:TEditBox;
    PlayBox: TPlayBox;
    ListBox: TSoundListBox;
    procedure MainMenuMenuBarEvent(AName: string);
    procedure BoxEventProc(cmd:Tcommand);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.MainMenuMenuBarEvent(AName: string);
begin
  case AName of
    'beenden': begin
      Close;
    end;
    'play': begin
    end;

    'about': begin
      ShowMessage('About');
    end;
  end;
end;

procedure TForm1.BoxEventProc(cmd: Tcommand);
var
  index: Integer;
  s: String;
begin
  case cmd of
    cmAdd: begin
      ListBox.Add;
    end;
    cmRemove: begin
      ListBox.Remove;
    end;
    cmUp: begin
      ListBox.Up;
    end;
    cmDown: begin
      ListBox.Down;
    end;

    cmPlay: begin
      ListBox.Play;
    end;
    cmNext: begin
      if ListBox.Next then  begin
//        LoadNewMusic(ListBox.GetTitle);
      end;
    end;
    cmPrev: begin
//      if ListBox.Prev(music) then begin
        if ListBox.Prev then begin
//        LoadNewMusic(ListBox.GetTitle);
      end;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  sl: TStringList;
begin
  SDL_Init(SDL_INIT_AUDIO);
  Mix_OpenAudio(0, nil);
//  music := nil;

  MainMenu := TMenuBar.Create(Self);
  MainMenu.OnMenuBarEvent := @MainMenuMenuBarEvent;
  Menu := MainMenu;

  ListBox := TSoundListBox.Create(self);
  ListBox.Anchors := [akTop, akLeft, akBottom, akRight];
  ListBox.Width := ClientWidth - 150;
  ListBox.Height := ClientHeight - 70;
  ListBox.Parent := self;
  ListBox.SetTrackBar(TrackBar1);

  EditBox := TEditBox.Create(Self);
  EditBox.Parent := Self;
  EditBox.OnPlayBoxEvent := @BoxEventProc;

  PlayBox := TPlayBox.Create(Self);
  PlayBox.Parent := Self;
  PlayBox.OnPlayBoxEvent := @BoxEventProc;

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
  ListBox.Free;

//  Mix_FreeMusic(music);
  Mix_CloseAudio;
  SDL_Quit;
end;

//procedure TForm1.LoadNewMusic(const titel: string);
//begin
//  if music <> nil then begin
//    Mix_FreeMusic(music);
//  end;
//  music := Mix_LoadMUS(PChar(titel));
//  if music = nil then begin
//    SDL_Log('Konnte Musik nicht laden: %s', Mix_GetError);
//  end;
//
//
//  //  Mix_PlayMusic(music, 1);
//  Mix_FadeInMusic(music, 1, 3000);
//  TrackBar1.Max := Trunc(Mix_MusicDuration(music) * 1000);
//  TrackBar1.Position := 0;
//end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  Mix_SetMusicPosition(TrackBar1.Position / 1000);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  t_length, t_pos: double;
  s: string;
  ChangeProc: TNotifyEvent;
  music: PMix_Music;
begin
  music:=ListBox.getMusic;

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
          ListBox.LoadNewMusic(ListBox.GetTitle);
        end;
      end;
    end;
  end;
end;

end.

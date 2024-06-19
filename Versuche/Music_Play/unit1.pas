unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SDL3, SDL3_mixer,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, LCLType,
  Buttons, ExtCtrls, ComCtrls, Types, FileUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    music1: PMix_Music;
    music2: PMix_Music;
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

  music1 := Mix_LoadMUS('/n4800/Multimedia/Music/Disco/C.C. Catch/1988 - Diamonds/05  - Heartbreak Hotel.flac');
  if music1 = nil then begin
    SDL_Log('Konnte music1 nicht laden!  %s', Mix_GetError);
  end;
  Mix_PlayMusic(music1, 1);

  music2 := Mix_LoadMUS('/n4800/Multimedia/Music/Disco/C.C. Catch/1988 - Diamonds/06  - Soul Survivor.flac');
  if music2 = nil then begin
    SDL_Log('Konnte music2 nicht laden!  %s', Mix_GetError);
  end;
  Mix_PlayMusic(music2, 1);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Mix_ResumeMusic
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Mix_FreeMusic(music1);
  Mix_FreeMusic(music2);
  Mix_CloseAudio;
  SDL_Quit;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
var
  t_length, t_pos: double;
  s: string;
begin
  WriteLn('timer');
  if music1 <> nil then begin
    t_length := Mix_MusicDuration(music1);
    WriteStr(s, t_length: 6: 1);
    Label1.Caption := s;
    t_pos := Mix_GetMusicPosition(music1);
    WriteStr(s, t_pos: 6: 1);
    Label2.Caption := s;
  end;
  if music2 <> nil then begin
    t_length := Mix_MusicDuration(music2);
    WriteStr(s, t_length: 6: 1);
    Label3.Caption := s;
    t_pos := Mix_GetMusicPosition(music2);
    WriteStr(s, t_pos: 6: 1);
    Label4.Caption := s;
  end;
end;

end.

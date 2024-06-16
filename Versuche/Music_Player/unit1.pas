unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SDL3, SDL3_mixer,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls,
  Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtnAdd: TBitBtn;
    BitBtnPlay: TBitBtn;
    BitBtnRemove: TBitBtn;
    BitBtnUp: TBitBtn;
    BitBtnDown: TBitBtn;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    procedure BitBtnAddClick(Sender: TObject);
    procedure BitBtnDownClick(Sender: TObject);
    procedure BitBtnPlayClick(Sender: TObject);
    procedure BitBtnRemoveClick(Sender: TObject);
    procedure BitBtnUpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    music: PMix_Music;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  audio_spec: TSDL_AudioSpec;
begin
  SDL_Init(SDL_INIT_AUDIO);
  Mix_OpenAudio(0, nil);



  music := Mix_LoadMUS('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_1.wav');
  if music = nil then begin
    WriteLn('WAV nicht gefunden !  ', Mix_GetError);
  end;

  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_1.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_2.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_3.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_4.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_5.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_6.wav');

  Width := 1024;
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Mix_FreeMusic(music);
  Mix_CloseAudio;
  SDL_Quit;
end;

procedure TForm1.BitBtnAddClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    ListBox1.Items.AddStrings(OpenDialog1.Files);
  end;
end;

procedure TForm1.BitBtnRemoveClick(Sender: TObject);
var
  index: integer;
begin
  index := ListBox1.ItemIndex;
  WriteLn(index);
  if (index > 0) and (index < ListBox1.Count) then  begin
    ListBox1.Items.Delete(index);
  end;
end;

procedure TForm1.BitBtnUpClick(Sender: TObject);
var
  index: integer;
begin
  index := ListBox1.ItemIndex;
  if index > 0 then  begin
    ListBox1.Items.Move(index, index - 1);
    ListBox1.ItemIndex := index - 1;
  end;
end;

procedure TForm1.BitBtnDownClick(Sender: TObject);
var
  index: integer;
begin
  index := ListBox1.ItemIndex;
  if index < ListBox1.Count - 1 then  begin
    ListBox1.Items.Move(index, index + 1);
    ListBox1.ItemIndex := index + 1;
  end;
end;

procedure TForm1.BitBtnPlayClick(Sender: TObject);
var
  index: integer;
  s: String;
begin
  if music <> nil then begin
    Mix_FreeMusic(music);
    music:=nil;
  end;
  index := ListBox1.ItemIndex;
  if index >= 0 then  begin
    s:=ListBox1.Items[index];
    music := Mix_LoadMUS(PChar(s));
  end;
  Mix_PlayMusic(music, 2);
end;

end.

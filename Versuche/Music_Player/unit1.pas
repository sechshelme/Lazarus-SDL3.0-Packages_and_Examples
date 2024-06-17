unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SDL3, SDL3_mixer,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, LCLType,
  Buttons, ExtCtrls, ComCtrls, Types;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtnAdd: TBitBtn;
    BitBtnPlay: TBitBtn;
    BitBtnRemove: TBitBtn;
    BitBtnUp: TBitBtn;
    BitBtnDown: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    TrackBar1: TTrackBar;
    procedure BitBtnAddClick(Sender: TObject);
    procedure BitBtnDownClick(Sender: TObject);
    procedure BitBtnPlayClick(Sender: TObject);
    procedure BitBtnRemoveClick(Sender: TObject);
    procedure BitBtnUpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
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
  audio_spec: TSDL_AudioSpec;
begin
  SDL_Init(SDL_INIT_AUDIO);
  Mix_OpenAudio(0, nil);

  music := nil;

  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_1.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_2.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_3.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_4.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_5.wav');
  ListBox1.Items.Add('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-3/examples/Audio/20_-_SDL_LoadWav_and_Button/Boing_6.wav');
  ListBox1.Items.Add('/n4800/Multimedia/Music/Disco/Boney M/1981 - Boonoonoonoos/01 - Boonoonoonoos.flac');

  Timer1.Interval := 100;
  TrackBar1.TickStyle:=tsNone;
  Width := 1024;
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Mix_FreeMusic(music);
  Mix_CloseAudio;
  SDL_Quit;
end;

procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: integer; ARect: TRect; State: TOwnerDrawState);
begin
  ListBox1.Canvas.Pen.Color := ListBox1.Canvas.Brush.Color;
  ListBox1.Canvas.Rectangle(ARect);

  if odSelected in State then  begin
    ListBox1.Canvas.Font.Color := clRed;
  end else begin
    ListBox1.Canvas.Font.Color := clGreen;
  end;
  ListBox1.Canvas.TextOut(ARect.Left, ARect.Top, ListBox1.Items[Index]);
end;

procedure TForm1.LoadNewMusic(const titel: string);
begin
  if music <> nil then begin
    Mix_FreeMusic(music);
  end;
  music := Mix_LoadMUS(PChar(titel));
  if music=nil then SDL_Log('Konnte Musik nicht laden: %s',Mix_GetError) ;
  Mix_PlayMusic(music, 1);
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
  if ListBox1.Count > 0 then begin
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
        index := ListBox1.ItemIndex;
        Inc(index);
        if index >= ListBox1.Items.Count then begin
          index := 0;
        end;
        //        TOwnerDrawState;
        //        ListBox1.;
        //        if ListBox1.CanFocus then ListBox1.Focused;:=True;
        ListBox1.ItemIndex := index;
        LoadNewMusic(ListBox1.Items[index]);
      end;
    end;
  end;
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
  s: string;
begin
  if music <> nil then begin
    Mix_FreeMusic(music);
    music := nil;
  end;
  index := ListBox1.ItemIndex;
  if index >= 0 then  begin
    s := ListBox1.Items[index];
    LoadNewMusic(s);
  end;
end;

end.

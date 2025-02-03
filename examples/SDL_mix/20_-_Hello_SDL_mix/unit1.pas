unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SDL3, SDL3_mixer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    music1: PMix_Music;
    music2: PMix_Music;
    chunk1, chunk2, chunk3: PMix_Chunk;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function GetChunkLength(chunk: PMix_Chunk): double;
var
  numSamples: longint;
begin
  numSamples := chunk^.allocated div (SizeOf(uint16) + chunk^.volume);
  WriteLn(chunk^.allocated);
  WriteLn(chunk^.volume);
  WriteLn('sam: ',numSamples);
  Result := numSamples / 44100;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SDL_Init(SDL_INIT_AUDIO);
WriteLn(  Mix_OpenAudio(0, nil));
  music1 := Mix_LoadMUS('/n4800/Multimedia/Music/Disco/Various/Dance Classics/CD 1/11 - I''m on fire - 5000 volts.flac');
  if music1 = nil then begin
    WriteLn('error music1: ', Mix_GetError);
  end;
  music2 := Mix_LoadMUS('/n4800/Multimedia/Music/Disco/Various/Dance Classics/CD 1/10 - Fly Robin fly - Silver convention.flac');
  if music2 = nil then begin
    WriteLn('error music2', Mix_GetError);
  end;

//  chunk1 := Mix_LoadWAV('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/Altes_SDL-1.2/lazyfoo.net/11_-_Playing_Sounds/doom.mid');
  chunk1 := Mix_LoadWAV('/n4800/Multimedia/Music/Disco/Various/Dance Classics/CD 1/11 - I''m on fire - 5000 volts.flac');
  if chunk1 = nil then begin
    WriteLn('error music1', Mix_GetError);
  end;
  chunk2 := Mix_LoadWAV('/n4800/Multimedia/Music/Disco/Various/Dance Classics/CD 1/10 - Fly Robin fly - Silver convention.flac');
  if chunk2 = nil then begin
    WriteLn('error music2', Mix_GetError);
  end;
  chunk3 := Mix_LoadWAV('/n4800/Multimedia/Music/Diverses/Test CDs/The High-end Test Record/Swiss HiFi Show/01 - Testsignale - The High-end Test Record.flac');
  if chunk3 = nil then begin
    WriteLn('error music3', Mix_GetError);
  end;
  WriteLn(GetChunkLength(chunk1): 10: 5);
  WriteLn(GetChunkLength(chunk2): 10: 5);
  WriteLn(GetChunkLength(chunk3): 10: 5);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  Mix_FadeInMusic(music1, 1, 3000);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Mix_FadeInMusic(music2, 1, 3000);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Mix_FadeInChannel(1, chunk1, 1, 3000);
  Mix_FadeOutChannel(1, 3000);
  WriteLn(GetChunkLength(chunk1): 10: 5);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Mix_FadeInChannel(2, chunk2, 1, 3000);
  Mix_FadeOutChannel(2, 3000);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  Mix_FadeInChannel(3, chunk3, 1, 3000);
  Mix_FadeOutChannel(3, 3000);
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Mix_SetPosition(1, 180, 255);

end;

end.

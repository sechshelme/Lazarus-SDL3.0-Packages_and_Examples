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
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    music: PMix_Music;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function FileToStr(const path: string): ansistring;
var
  f: file of byte;
  size: int64;
begin
  Result := '';
  if FileExists(path) then begin
    Assign(f, path);
    Reset(f);
    size := FileSize(f);
    SetLength(Result, size);
    BlockRead(f, Result[1], size);
    Close(f);
  end else begin
    WriteLn('FEHLER: Kann Datei ' + path + ' nicht finden');
  end;
end;

var
  buffer: string;

procedure TForm1.FormCreate(Sender: TObject);
var
  stream: PSDL_IOStream;
begin
  buffer := FileToStr('/home/tux/Schreibtisch/sound/test.mp3');

  stream := SDL_IOFromConstMem(Pointer(buffer), Length(buffer));
  if stream = nil then begin
    WriteLn('stream error: ', SDL_GetError);
  end;

  SDL_Init(SDL_INIT_AUDIO);
  WriteLn(Mix_OpenAudio(0, nil));

  music := Mix_LoadMUS_IO(stream, True);
  if music = nil then begin
    WriteLn('error music: ', Mix_GetError);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Mix_FadeInMusic(music, 1, 3000);
end;

end.

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, openal;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure list_audio_devices(devices:PALchar);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  aldevice: PALCdevice;
begin
  aldevice := alcOpenDevice(nil);
  if aldevice = nil then begin
    WriteLn('error');
  end else begin
    WriteLn('io.');
    list_audio_devices(aldevice);
  end;


end;

end.

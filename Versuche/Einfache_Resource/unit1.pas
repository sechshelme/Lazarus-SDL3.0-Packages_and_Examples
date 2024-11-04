unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

function ResourceToByteBuffer(Resource: string): TCharArray;
var
  rs: TResourceStream;
begin
  Result := nil;
  rs := TResourceStream.Create(HINSTANCE, Resource, RT_RCDATA);
  SetLength(Result, rs.Size);
  rs.Read(PChar(Result)^, rs.Size);
  rs.Free;
end;

var
  buffer: TCharArray;

procedure TForm1.FormCreate(Sender: TObject);
begin
  buffer := ResourceToByteBuffer('UNIT1');
  Memo1.Text := string(buffer);
end;

end.

unit AudioBar;

interface

uses
  SDL3;

type

  { TSoundBar }

  TSoundBar=class(TObject)
    constructor Create(const Titel:String;x,y:Integer);
    destructor Destroy; override;
  end;

implementation

{ TSoundBar }

constructor TSoundBar.Create(const Titel: String; x, y: Integer);
begin

end;

destructor TSoundBar.Destroy;
begin
  inherited Destroy;
end;

end.


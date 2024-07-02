unit Common;

interface

type
  Tcommand = (cmAdd, cmRemove, cmUp, cmDown, cmPlay,cmStop, cmNext, cmPrev);

  TcmdProp = record
    cmd: Tcommand;
    Caption: string;
  end;
  TcmdProps = array of TcmdProp;

const
  PlayCmdProp: TcmdProps = (
    (cmd: cmPlay; Caption: '▶️'),
    (cmd: cmStop; Caption: '⏹️'),
    (cmd: cmNext; Caption: '⏭️'),
    (cmd: cmPrev; Caption: '⏮'));

  EditCmdProb: TcmdProps = (
    (cmd: cmAdd; Caption: 'Add'),
    (cmd: cmRemove; Caption: 'Remove'),
    (cmd: cmUp; Caption: 'Up'),
    (cmd: cmDown; Caption: 'Down'));

implementation

end.

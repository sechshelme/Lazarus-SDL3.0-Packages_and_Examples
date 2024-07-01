unit Common;

interface

type
  Tcommand = (cmAdd, cmRemove, cmUp, cmDown, cmPlay, cmNext, cmPrev);

  TcmdProb = record
    cmd: Tcommand;
    Caption: string;
  end;
  TcmdProps = array of TcmdProb;

const
  PlayCmdProp: TcmdProps = (
    (cmd: cmPlay; Caption: '▶️'),
    (cmd: cmNext; Caption: '⏭️'),
    (cmd: cmPrev; Caption: '⏮'));

  EditCmdProb: TcmdProps = (
    (cmd: cmAdd; Caption: 'Add'),
    (cmd: cmRemove; Caption: 'Remove'),
    (cmd: cmUp; Caption: 'Up'),
    (cmd: cmDown; Caption: 'Down'));

implementation

end.

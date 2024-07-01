unit Common;

interface

type
  Tcommand = (cmPlay := 1000, cmNext, cmPrev);

  tcmdProb = record
    cmd: Tcommand;
    Caption: string;
  end;
  tcmdProps = array of tcmdProb;

const
  PlayCmdProp: tcmdProps = (
    (cmd: cmPlay; Caption: '▶️'),
    (cmd: cmNext; Caption: '⏭️'),
    (cmd: cmPrev; Caption: '⏮' ));

implementation

end.

unit Common;

interface

const
TrackBarDivider=1000;
  MusicDir:String = '/n4800/Multimedia/Music/Disco/Boney M';

type
  Tcommand = (cmNone, cmNew, cmSave, cmOpen, cmClose, cmAdd, cmRemove,cmRemoveAll, cmUp, cmDown, cmPlay, cmStop, cmNext, cmPrev);

  TcmdProp = record
    cmd: Tcommand;
    Caption: string;
  end;
  TcmdProps = array of TcmdProp;

const
  FileCmdProb: TcmdProps = (
    (cmd: cmNew; Caption: 'Neu'),
    (cmd: cmNone; Caption: '-'),
    (cmd: cmSave; Caption: 'Speichern'),
    (cmd: cmOpen; Caption: 'Öffnen'),
    (cmd: cmNone; Caption: '-'),
    (cmd: cmClose; Caption: 'Beenden'));

  EditCmdProb: TcmdProps = (
    (cmd: cmAdd; Caption: 'Add'),
    (cmd: cmRemove; Caption: 'Remove'),
    (cmd: cmRemoveAll; Caption: 'Remove All'),
    (cmd: cmUp; Caption: 'Up'),
    (cmd: cmDown; Caption: 'Down'));

  PlayCmdProp: TcmdProps = (
    (cmd: cmPlay; Caption: '▶️'),
    (cmd: cmStop; Caption: '⏹️'),
    (cmd: cmNext; Caption: '⏭️'),
    (cmd: cmPrev; Caption: '⏮'));

implementation

end.

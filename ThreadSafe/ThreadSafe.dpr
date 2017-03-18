program ThreadSafe;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form1},
  uThreadDict in 'uThreadDict.pas',
  uThreadList in 'uThreadList.pas',
  uOrderedDict in 'uOrderedDict.pas',
  uThreadOrderedDict in 'uThreadOrderedDict.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

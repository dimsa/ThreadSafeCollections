program ThreadSafe;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form1},
  uThreadDict in 'uThreadDict.pas',
  uThreadList in 'uThreadList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program ThreadSafe;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form1},
  uThreadDict in 'ThreadSafeCollections\uThreadDict.pas',
  uThreadList in 'ThreadSafeCollections\uThreadList.pas',
  uThreadOrderedDict in 'ThreadSafeCollections\uThreadOrderedDict.pas',
  uOrderedDict in 'BaseCollections\uOrderedDict.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program ThreadSafe;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form1},
  uThreadDict in 'ThreadSafeCollections\uThreadDict.pas',
  uThreadList in 'ThreadSafeCollections\uThreadList.pas',
  uThreadOrderedDict in 'ThreadSafeCollections\uThreadOrderedDict.pas',
  uOrderedDict in 'BaseCollections\uOrderedDict.pas',
  uUniqueList in 'BaseCollections\uUniqueList.pas',
  uThreadUniqueList in 'ThreadSafeCollections\uThreadUniqueList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

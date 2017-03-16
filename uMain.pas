unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Threading, uThreadDict, System.SyncObjs,
  System.Generics.Collections, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    btn1: TButton;
    mmo1: TMemo;
    btn2: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    FDict: TThreadDict<string, Integer>;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btn1Click(Sender: TObject);
var
  i: Integer;
  vThreadIterator: integer;
  vTaskWrite, vTaskRead: ITask;
  vLock: TObject;
  vList: TList<string>;
begin
  vLock := TObject.Create;
  vThreadIterator := 0;
  for i := 0 to 999 do
  begin
    vTaskWrite := TTask.Create (procedure()
    begin
      Sleep(Random(50));
      TMonitor.Enter(vLock);
      TInterlocked.Add(vThreadIterator, 1);
      TMonitor.Exit(vLock);
      FDict.Add('Key' + vThreadIterator.ToString, vThreadIterator);
    end);
    vTaskWrite.Start;
   end;
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  vDict: TDictionary<string, Integer>;
  s: string;
begin
  try
  vDict := FDict.Items;
  mmo1.Lines.Add(IntToStr(vDict.Count));
  for s in vDict.Keys do
    mmo1.Lines.Add(s + ' = ' + IntToStr(vDict[s]));
  finally
    FDict.Unlock;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDict := TThreadDict<string, Integer>.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDict.Free;
end;

end.

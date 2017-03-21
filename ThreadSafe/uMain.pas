unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Threading, uThreadDict, uThreadOrderedDict, uOrderedDict,
  System.SyncObjs, System.Generics.Collections, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    btn1: TButton;
    mmo1: TMemo;
    btn2: TButton;
    btn3: TButton;
    Button1: TButton;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FDict: TThreadDict<string, Integer>;
    FOrderedDict: TThreadOrderedDict<string, Integer>;
    procedure FillDict(const ACount: Integer);
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
  vTaskWrite: ITask;
  vLock: TObject;
begin
  vLock := TObject.Create;
  vThreadIterator := 0;
  for i := 0 to 999 do
  begin
    vTaskWrite := TTask.Create (procedure()
    begin
      TMonitor.Enter(vLock);
      TInterlocked.Add(vThreadIterator, 1);
      TMonitor.Exit(vLock);
      FOrderedDict.Add('Key' + vThreadIterator.ToString, vThreadIterator);
    end);
    vTaskWrite.Start;
   end;
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  vDict: TOrderedDict<string, Integer>;
  i: Integer;
begin
  vDict := FOrderedDict.LockPointer;
  mmo1.Lines.Add('==========' + IntToStr(vDict.Count) + '==========');

  for i := 0 to vDict.Count - 1 do
    if vDict.ContainsKey('Key' + IntToStr(i + 1)) then
      mmo1.Lines.Add(vDict.KeyByIndex[i] + ' = ' + IntToStr(vDict[vDict.KeyByIndex[i]]));

  FOrderedDict.UnlockPointer;
end;

procedure TForm1.btn3Click(Sender: TObject);
var
  i: Integer;
  vThreadIterator1, vThreadIterator2, vThreadIterator3: integer;
  vTaskWrite, vTaskRead: ITask;
  vLock: TObject;
  vRes: Integer;
  vStartWriteTime, vEndWriteTime, vSumTime: Integer;
  vStartReadTime, vEndReadTime, vSumReadTime: Integer;
  vA: Integer;
begin
  vLock := TObject.Create;
  vThreadIterator1 := 0;
  vThreadIterator2 := 0;
  vThreadIterator3 := 0;
  vSumTime := 0;
  vSumReadTime := 0;
  vA := 0;

  FOrderedDict.Clear;

  for i := 0 to 999999 do
  begin
    vTaskWrite := TTask.Create (procedure()
    begin
      vStartWriteTime := TThread.GetTickCount;

      TMonitor.Enter(vLock);
      TInterlocked.Add(vThreadIterator1, 1);
      TMonitor.Exit(vLock);
      FOrderedDict.Add('Key' + vThreadIterator1.ToString, vThreadIterator1);

      vEndWriteTime := TThread.GetTickCount;
      TInterlocked.Add(vSumTime, vEndWriteTime - vStartWriteTime);
    end);
    vTaskWrite.Start;
    end;

  Sleep(3000);
  ShowMessage(IntToStr(vSumTime));

  for i := 0 to 999999 do
  begin
    vTaskRead := TTask.Create (procedure()
    begin
      vStartReadTime := TThread.GetTickCount;

      TMonitor.Enter(vLock);
      TInterlocked.Add(vThreadIterator2, 1);
      TMonitor.Exit(vLock);
        if FOrderedDict.TryGetValueByKey('Key' + IntToStr(vThreadIterator2), vRes) then
          vA := vA + 1;

      vEndReadTime := TThread.GetTickCount;
      TInterlocked.Add(vSumReadTime, vEndReadTime - vStartReadTime);
    end);
    vTaskRead.Start;
   end;

    Sleep(3000);
    ShowMessage('Read=' + IntToStr(vSumReadTime) + ' iterations ' + IntToStr(vA));
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  vThreadIterator2, vThreadIterator3: integer;
  vTaskWrite, vTaskRead, vTaskDelete: ITask;
  vLock: TObject;
  vRes: Integer;
begin
  vLock := TObject.Create;
  vThreadIterator2 := 0;
  vThreadIterator3 := 0;

  FillDict(999);
  Sleep(3000);

  for i := 0 to 999 do
  begin
    vTaskDelete := TTask.Create (procedure()
    begin
      Sleep(Random(5));
      TMonitor.Enter(vLock);
      TInterlocked.Add(vThreadIterator3, 1);
      TMonitor.Exit(vLock);

      if FOrderedDict.ContainsKey('Key' + IntToStr(vThreadIterator3)) then
         FOrderedDict.Remove('Key' + IntToStr(vThreadIterator3));
    end);
    vTaskDelete.Start;

    vTaskRead := TTask.Create (procedure()
    begin
      Sleep(Random(5));
      TMonitor.Enter(vLock);
      TInterlocked.Add(vThreadIterator2, 1);
      TMonitor.Exit(vLock);
        if FOrderedDict.TryGetValueByKey('Key' + IntToStr(vThreadIterator2), vRes) then
        begin
          TMonitor.Enter(vLock);
          mmo1.Lines.Add(IntToStr(vRes));
          TMonitor.Exit(vLock);
        end;
    end);
    vTaskRead.Start;
   end;
end;

procedure TForm1.FillDict(const ACount: Integer);
var
  i: Integer;
  vThreadIterator: integer;
  vTaskWrite: ITask;
  vLock: TObject;
begin
  FOrderedDict.Clear;
  vLock := TObject.Create;
  for i := 0 to ACount - 1 do
  begin
    vTaskWrite := TTask.Create (procedure()
    begin
      TMonitor.Enter(vLock);
      TInterlocked.Add(vThreadIterator, 1);
      TMonitor.Exit(vLock);
      FOrderedDict.Add('Key' + vThreadIterator.ToString, vThreadIterator);
    end);
    vTaskWrite.Start;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDict := TThreadDict<string, Integer>.Create;
  FOrderedDict := TThreadOrderedDict<string, Integer>.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDict.Free;
  FOrderedDict.Free;
end;

end.

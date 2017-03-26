unit TestThreadList;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit 
  being tested.

}

interface

uses
  TestFramework, System.Generics.Collections, uThreadList, System.SysUtils,
  System.Threading, System.SyncObjs, System.Classes;

type
  // Test methods for class TThreadList

  TestTThreadList = class(TTestCase)
  strict private
    FList: TThreadList<string>;
    procedure CheckAllHere(ACount: Integer);
    procedure AsyncAdd(ACount: Integer);
    procedure AsyncAddAndInsert(ACount: Integer);
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CheckOrderedList;
  published
    procedure TestAsyncAdd;
    procedure TestAsyncInsert;
    procedure TestAsyncAddAndInsert;
    procedure TestAsyncSwap;

    procedure TestSyncAdd;
    procedure TestSyncInsert;
    procedure TestSyncRemove;
    procedure TestSyncDelete;
    procedure TestSyncSwap;
    procedure TestSyncClear;
    procedure TestSyncMove;
    procedure TestSyncContains;
    procedure TestSyncIndexOf;
    procedure TestSyncCount;
  end;

implementation

procedure TestTThreadList.CheckOrderedList;
var
  i: Integer;
  vResList: TList<string>;
  vRes: string;
begin
  CheckEquals(5, FList.Count);
  vResList := TList<string>.Create;
  for i := 0 to 4 do
  begin
    if (FList.TryGetValue(i, vRes)) then
      vResList.Add(vRes);
  end;

  for i := 0 to 4 do
    CheckEquals('Item' + IntToStr(I), vResList[i]);
end;

procedure TestTThreadList.SetUp;
begin
  FList := TThreadList<string>.Create;
end;

procedure TestTThreadList.TearDown;
begin
  FList.Free;
  FList := nil;
end;

procedure TestTThreadList.TestAsyncAdd;
begin
  AsyncAdd(1000);
  CheckAllHere(1000);
end;

procedure TestTThreadList.TestAsyncAddAndInsert;
begin
  AsyncAddAndInsert(1000);
  CheckAllHere(1000);
end;

procedure TestTThreadList.TestAsyncInsert;
var
  i: Integer;
  vThreadIterator: Integer;
  vTasks: array of ITask;
begin
  SetLength(vTasks, 900);

  vThreadIterator := 0;
  for i := 0 to 99 do
  begin
    FList.Add('Item' + IntToStr(i));
  end;

  vThreadIterator := 99;
  for i := 100 to 999 do
  begin
    vTasks[i - 100] :=
      TTask.Create (procedure()
      begin
        Sleep(Random(1));
        TInterlocked.Add(vThreadIterator, 1);
        FList.Insert(Random(5), 'Item' + IntToStr(vThreadIterator));
      end);
    vTasks[i - 100].Start;
  end;

  TTask.WaitForAll(vTasks);

  CheckAllHere(1000);
end;

procedure TestTThreadList.TestAsyncSwap;
var
  i: Integer;
  vTasks: array of ITask;
begin
  SetLength(vTasks, 900);
  for i := 0 to 99 do
  begin
    FList.Add('Item' + IntToStr(i));
  end;

  for i := 0 to 899 do
  begin
    vTasks[i] :=
      TTask.Create (procedure()
      begin
        Sleep(Random(2));
        FList.Swap(Random(100), Random(100));
      end);
    vTasks[i].Start;
  end;

  TTask.WaitForAll(vTasks);
  CheckAllHere(100);
end;

procedure TestTThreadList.TestSyncAdd;
var
  i: Integer;
begin
  for i := 0 to 4 do
    FList.Add('Item' + IntToStr(I));

  CheckOrderedList;
end;

procedure TestTThreadList.TestSyncInsert;
begin
  FList.Insert(0, 'Item1');
  FList.Insert(0, 'Item0');
  FList.Insert(2, 'Item3');
  FList.Insert(3, 'Item4');
  FList.Insert(2, 'Item2');

  CheckOrderedList;
end;

procedure TestTThreadList.TestSyncRemove;
var
  i: Integer;
  vList: TList<string>;
begin
  for i := 0 to 4 do
    FList.Add('Item' + IntToStr(I));

  FList.Remove('Item3');

  vList := FList.LockPointer;

  CheckEquals(4, vList.Count);
  CheckEquals(False, vList.Contains('Item3'));

  CheckEquals('Item0', vList[0]);
  CheckEquals('Item1', vList[1]);
  CheckEquals('Item2', vList[2]);
  CheckEquals('Item4', vList[3]);

  FList.UnlockPointer;
end;

procedure TestTThreadList.TestSyncDelete;
var
  i: Integer;
  vList: TList<string>;
begin
  for i := 0 to 4 do
    FList.Add('Item' + IntToStr(I));

  FList.Delete(3);

  vList := FList.LockPointer;

  CheckEquals(4, vList.Count);
  CheckEquals(False, vList.Contains('Item3'));

  CheckEquals('Item0', vList[0]);
  CheckEquals('Item1', vList[1]);
  CheckEquals('Item2', vList[2]);
  CheckEquals('Item4', vList[3]);

  FList.UnlockPointer;
end;

procedure TestTThreadList.TestSyncSwap;
var
  i: Integer;
  vList: TList<string>;
begin
  for i := 0 to 4 do
    FList.Add('Item' + IntToStr(I));

  FList.Swap(2, 3);

  vList := FList.LockPointer;

  CheckEquals('Item0', vList[0]);
  CheckEquals('Item1', vList[1]);
  CheckEquals('Item3', vList[2]);
  CheckEquals('Item2', vList[3]);
  CheckEquals('Item4', vList[4]);

  FList.UnlockPointer;
end;

procedure TestTThreadList.TestSyncClear;
var
  i: Integer;
begin
  for i := 0 to 4 do
    FList.Add('Item' + IntToStr(I));

  FList.Clear;

  CheckEquals(0, FList.Count);
end;

procedure TestTThreadList.TestSyncMove;
var
  i: Integer;
  vList: TList<string>;
begin
  for i := 0 to 4 do
    FList.Add('Item' + IntToStr(I));

  FList.Move(3, 1);

  vList := FList.LockPointer;

  CheckEquals('Item0', vList[0]);
  CheckEquals('Item3', vList[1]);
  CheckEquals('Item1', vList[2]);
  CheckEquals('Item2', vList[3]);
  CheckEquals('Item4', vList[4]);

  FList.UnlockPointer;
end;

procedure TestTThreadList.TestSyncContains;
var
  i: Integer;
begin
  for i := 0 to 4 do
    FList.Add('Item' + IntToStr(I));

  FList.Delete(2);
  FList.Remove('Item3');

  CheckEquals(True, FList.Contains('Item0'));
  CheckEquals(True, FList.Contains('Item1'));
  CheckEquals(False, FList.Contains('Item2'));
  CheckEquals(False, FList.Contains('Item3'));
  CheckEquals(True, FList.Contains('Item4'));
  CheckEquals(False, FList.Contains('Item5'));
end;

procedure TestTThreadList.TestSyncIndexOf;
var
  i: Integer;
begin
  for i := 0 to 4 do
    FList.Add('Item' + IntToStr(I));

  FList.Swap(2, 3);

  CheckEquals(0, FList.IndexOf('Item0'));
  CheckEquals(1, FList.IndexOf('Item1'));
  CheckEquals(3, FList.IndexOf('Item2'));
  CheckEquals(2, FList.IndexOf('Item3'));
  CheckEquals(4, FList.IndexOf('Item4'));
end;

procedure TestTThreadList.TestSyncCount;
var
  i: Integer;
begin
  for i := 0 to 4 do
    FList.Add('Item' + IntToStr(I));

  FList.Delete(3);

  CheckEquals(4, FList.Count);
end;

procedure TestTThreadList.AsyncAdd(ACount: Integer);
var
  i: Integer;
  vThreadIterator1, vThreadIterator2: Integer;
  vLock1, vLock2: TObject;
  vTasks: array of ITask;
begin
  vLock1 := TObject.Create;
  vLock2 := TObject.Create;

  SetLength(vTasks, ACount);
  vThreadIterator1 := 0;
  vThreadIterator2 := ACount div 2;

  for i := 0 to ACount - 1 do
  begin
    if i mod 2 = 0 then
    begin
      vTasks[i] :=
        TTask.Create (procedure()
        begin
          TThread.Yield;
          TThread.Sleep(Random(2));
          TMonitor.Enter(vLock1);
          TThread.Yield;
          FList.Add('Item' + IntToStr(vThreadIterator1));
          TThread.Yield;
          TInterlocked.Add(vThreadIterator1, 1);
          TThread.Yield;
          TMonitor.Exit(vLock1);
          TThread.Yield;
        end);
      vTasks[i].Start;
    end else
    begin
       vTasks[i] :=
         TTask.Create (procedure()
         begin
           TThread.Yield;
           TThread.Sleep(Random(2));
           TMonitor.Enter(vLock2);
           TThread.Yield;
           FList.Add('Item' + IntToStr(vThreadIterator2));
           TThread.Yield;
           TInterlocked.Add(vThreadIterator2, 1);
           TThread.Yield;
           TMonitor.Exit(vLock2);
           TThread.Yield;
         end);
       vTasks[i].Start;
    end;
  end;
  TTask.WaitForAll(vTasks);
end;

procedure TestTThreadList.AsyncAddAndInsert(ACount: Integer);
var
  i: Integer;
  vThreadIterator1, vThreadIterator2: Integer;
  vLock1, vLock2: TObject;
  vTasks: array of ITask;
begin
  vLock1 := TObject.Create;
  vLock2 := TObject.Create;

  SetLength(vTasks, ACount);
  vThreadIterator1 := 0;
  vThreadIterator2 := ACount div 2;

  for i := 0 to ACount - 1 do
  begin
    if i mod 2 = 0 then
    begin
      vTasks[i] :=
        TTask.Create (procedure()
        begin
          TThread.Yield;
          TThread.Sleep(Random(2));
          TMonitor.Enter(vLock1);
          TThread.Yield;
          FList.Add('Item' + IntToStr(vThreadIterator1));
          TThread.Yield;
          TInterlocked.Add(vThreadIterator1, 1);
          TThread.Yield;
          TMonitor.Exit(vLock1);
          TThread.Yield;
        end);
      vTasks[i].Start;
    end else
    begin
       vTasks[i] :=
         TTask.Create (procedure()
         begin
           TThread.Yield;
           TThread.Sleep(Random(2));
           TMonitor.Enter(vLock2);
           TThread.Yield;
           FList.Insert(0, 'Item' + IntToStr(vThreadIterator2));
           TThread.Yield;
           TInterlocked.Add(vThreadIterator2, 1);
           TThread.Yield;
           TMonitor.Exit(vLock2);
           TThread.Yield;
         end);
       vTasks[i].Start;
    end;
  end;
  TTask.WaitForAll(vTasks);
end;

procedure TestTThreadList.CheckAllHere(ACount: Integer);
var
  i: Integer;
  vList: TList<string>;
begin
  try
    vList := FList.LockPointer;
    CheckEquals(ACount, vList.Count);
    for i := 0 to ACount - 1 do
      CheckEquals(True, vList.Contains('Item' + IntToStr(i)));

  finally
    FList.UnlockPointer;
  end;
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTThreadList.Suite);
end.

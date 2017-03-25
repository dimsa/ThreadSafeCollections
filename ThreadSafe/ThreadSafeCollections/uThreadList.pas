unit uThreadList;

interface

uses
  System.Generics.Collections;

type
  TThreadList<TValue> = class
  private
    FList: TList<TValue>;
    FLock: TObject;
    procedure Lock;
    procedure Unlock;
  public
    procedure Add(const AValue: TValue);
    procedure Remove(const AValue: TValue);
    procedure Delete(const AIndex: Integer);
    procedure Clear;
    procedure Swap(const AIndex1, AIndex2: Integer); overload;
    procedure Move(const ACurIndex, ANewIndex: Integer);
    procedure SetValue(const AIndex: Integer; AValue: TValue);

    // vvv Non-blocking methods vvv
    function TryGetValue(const AIndex: Integer; out AValue: TValue): Boolean;
    function Contains(const AItem: TValue): Boolean;
    function IndexOf(const AItem: TValue): Integer;
    function Count: Integer;
    // ^^^ Non-blocking methods ^^^

    function ItemsCopy: TList<TValue>;

    // It is not threadsave if you use pointer after Unlocking
    function LockPointer: TList<TValue>;
    procedure UnlockPointer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadList<TValue> }

procedure TThreadList<TValue>.Add(const AValue: TValue);
begin
  Lock;
  try
    FList.Add(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadList<TValue>.Clear;
begin
  Lock;
  try
    FList.Clear;
  finally
    Unlock;
  end;
end;

function TThreadList<TValue>.Contains(const AItem: TValue): Boolean;
begin
  Result := FList.Contains(AItem);
end;

function TThreadList<TValue>.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TThreadList<TValue>.Create;
begin
  FLock := TObject.Create;
  FList := TList<TValue>.Create;
end;

procedure TThreadList<TValue>.Delete(const AIndex: Integer);
begin
  Lock;
  try
    FList.Delete(AIndex);
  finally
    Unlock;
  end;
end;

destructor TThreadList<TValue>.Destroy;
begin
  Lock;
  try
    FList.Free;
    inherited Destroy;
  finally
    Unlock;
    FLock.Free;
  end;
end;

function TThreadList<TValue>.IndexOf(const AItem: TValue): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

function TThreadList<TValue>.ItemsCopy: TList<TValue>;
begin
  Lock;
  try
    Result := TList<TValue>.Create(FList);
  finally
    Unlock;
  end;

end;

procedure TThreadList<TValue>.Lock;
begin
  TMonitor.Enter(FLock);
end;

function TThreadList<TValue>.LockPointer: TList<TValue>;
begin
  Lock;
  Result := FList;
end;

procedure TThreadList<TValue>.Move(const ACurIndex, ANewIndex: Integer);
begin
  Lock;
  try
    FList.Move(ACurIndex, ANewIndex);
  finally
    Unlock;
  end;
end;

procedure TThreadList<TValue>.Remove(const AValue: TValue);
begin
  Lock;
  try
    FList.Remove(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadList<TValue>.SetValue(const AIndex: Integer; AValue: TValue);
begin
  Lock;
  try
    FList[AIndex] := AValue;
  finally
    Unlock;
  end;
end;

procedure TThreadList<TValue>.Swap(const AIndex1, AIndex2: Integer);
var
  vTmpItem: TValue;
begin
  Lock;
  try
    vTmpItem := FList[AIndex1];
    FList[AIndex1] := FList[AIndex2];
    FList[AIndex2] := vTmpItem;
  finally
    Unlock;
  end;
end;

function TThreadList<TValue>.TryGetValue(const AIndex: Integer; out AValue: TValue): Boolean;
begin
  try
    AValue := FList[AIndex];
    Exit(True);
  except
    Exit(False);
  end;
end;

procedure TThreadList<TValue>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

procedure TThreadList<TValue>.UnlockPointer;
begin
  Unlock;
end;

end.

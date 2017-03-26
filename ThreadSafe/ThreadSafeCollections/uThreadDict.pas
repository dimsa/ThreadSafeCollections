unit uThreadDict;

interface

uses
  System.Generics.Collections;

type
  TThreadDict<TKey, TValue> = class
  private
    FDict: TDictionary<TKey, TValue>;
    FLock: TObject;

    procedure Lock;
    procedure Unlock;
  public
    procedure Add(const AKey: TKey; const AValue: TValue);
    procedure Remove(const AKey: TKey);
    procedure Clear;
    procedure SetValue(const AKey: TKey; const AValue: TValue);

    // vvv Non-blocking methods vvv
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    function ContainsKey(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean;
    function Count: Integer;
    // ^^^ Non-blocking methods ^^^

    function ItemsCopy: TDictionary<TKey, TValue>;

    // It is not threadsave if you use pointer after Unlocking
    function LockPointer: TDictionary<TKey, TValue>;
    procedure UnlockPointer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadDict<TKey, TValue> }

procedure TThreadDict<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  Lock;
  try
    FDict.Add(AKey, AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TKey, TValue>.Clear;
begin
  Lock;
  try
    FDict.Clear;
  finally
    Unlock;
  end;
end;

function TThreadDict<TKey, TValue>.ContainsKey(const AKey: TKey): Boolean;
begin
  Result := FDict.ContainsKey(AKey);
end;

function TThreadDict<TKey, TValue>.ContainsValue(const AValue: TValue): Boolean;
begin
  Result := FDict.ContainsValue(AValue);
end;

function TThreadDict<TKey, TValue>.Count: Integer;
begin
  Result := FDict.Count;
end;

constructor TThreadDict<TKey, TValue>.Create;
begin
  FLock := TObject.Create;
  FDict := TDictionary<TKey, TValue>.Create;
end;

destructor TThreadDict<TKey, TValue>.Destroy;
begin
  Lock;
  try
    FDict.Free;
    inherited Destroy;
  finally
    Unlock;
    FLock.Free;
  end;
end;

function TThreadDict<TKey, TValue>.ItemsCopy: TDictionary<TKey, TValue>;
begin
  Lock;
  try
    Result := TDictionary<TKey, TValue>.Create(FDict);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TKey, TValue>.Lock;
begin
  TMonitor.Enter(FLock);
end;

function TThreadDict<TKey, TValue>.LockPointer: TDictionary<TKey, TValue>;
begin
  Lock;
  Result := FDict;
end;

procedure TThreadDict<TKey, TValue>.Remove(const AKey: TKey);
begin
  Lock;
  try
    FDict.Remove(AKey);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TKey, TValue>.SetValue(const AKey: TKey;
  const AValue: TValue);
begin
  Lock;
  try
    FDict[AKey] := AValue;
  finally
    Unlock;
  end;
end;

function TThreadDict<TKey, TValue>.TryGetValue(const AKey: TKey;
  out AValue: TValue): Boolean;
begin
  try
    AValue := FDict[AKey];
    Exit(True);
  except
    Exit(False);
  end;
end;

procedure TThreadDict<TKey, TValue>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

procedure TThreadDict<TKey, TValue>.UnlockPointer;
begin
  Unlock;
end;

end.

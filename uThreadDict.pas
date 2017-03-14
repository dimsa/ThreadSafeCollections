unit uThreadDict;

interface

uses
  System.Generics.Collections;

type
  TThreadDict<TKey, TValue> = class
  private
    FDict: TDictionary<TKey, TValue>;
    FLock: TObject;
  protected
    function PointerUnsafeLock: TDictionary<TKey, TValue>;
  public
    procedure Add(const AKey: TKey; const AValue: TValue);
    procedure Remove(const AKey: TKey);
    procedure Clear;

    function Lock: TDictionary<TKey, TValue>;
    procedure Unlock;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadDict<TKey, TValue> }

procedure TThreadDict<TKey, TValue>.Add(const AKey: TKey; const AValue: TValue);
begin
  PointerUnsafeLock;
  try
    FDict.Add(AKey, AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TKey, TValue>.Clear;
begin
  PointerUnsafeLock;
  try
    FDict.Clear;
  finally
    Unlock;
  end;
end;

constructor TThreadDict<TKey, TValue>.Create;
begin
  FLock := TObject.Create;
  FDict := TDictionary<TKey, TValue>.Create;
end;

destructor TThreadDict<TKey, TValue>.Destroy;
begin
  PointerUnsafeLock;
  try
    FDict.Free;
    inherited Destroy;
  finally
    Unlock;
    FLock.Free;
  end;
  inherited;
end;

function TThreadDict<TKey, TValue>.Lock: TDictionary<TKey, TValue>;
begin
  Result := TDictionary<TKey, TValue>.Create(PointerUnsafeLock);
end;

function TThreadDict<TKey, TValue>.PointerUnsafeLock: TDictionary<TKey, TValue>;
begin
  TMonitor.Enter(FLock);
  Result := FDict;
end;

procedure TThreadDict<TKey, TValue>.Remove(const AKey: TKey);
begin
  PointerUnsafeLock;
  try
    FDict.Remove(AKey);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TKey, TValue>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

end.

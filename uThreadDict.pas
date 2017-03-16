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
    function PointerUnsafeCopy: TDictionary<TKey, TValue>;
  public
    procedure Add(const AKey: TKey; const AValue: TValue);
    procedure Remove(const AKey: TKey);
    procedure Clear;

    function ItemsCopy: TDictionary<TKey, TValue>;

    procedure Lock;
    procedure Unlock;
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
  inherited;
end;

function TThreadDict<TKey, TValue>.ItemsCopy: TDictionary<TKey, TValue>;
begin
  Lock;
  try
    Result := TDictionary<TKey, TValue>.Create(PointerUnsafeCopy);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TKey, TValue>.Lock;
begin
  TMonitor.Enter(FLock);
end;

function TThreadDict<TKey, TValue>.PointerUnsafeCopy: TDictionary<TKey, TValue>;
begin
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

procedure TThreadDict<TKey, TValue>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

end.

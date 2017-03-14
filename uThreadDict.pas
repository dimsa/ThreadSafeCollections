unit uThreadDict;

interface

uses
  System.Generics.Collections;

type
  TThreadDict<TKey, TValue> = class
  private
    FDict: TDictionary<TKey, TValue>;
    FLock: TObject;
  public
    procedure Add(const AKey: TKey; const AValue: TValue);
    procedure Clear;
    procedure Remove(const AKey: TKey);

    function Lock: TDictionary<TKey, TValue>;
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

function TThreadDict<TKey, TValue>.Lock: TDictionary<TKey, TValue>;
begin
  TMonitor.Enter(FLock);
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

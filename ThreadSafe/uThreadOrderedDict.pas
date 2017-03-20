unit uThreadOrderedDict;

interface

uses
  uOrderedDict;

{$X+}
type
  TThreadOrderedDict<TKey, TValue> = class
  private
    FDict: TOrderedDict<TKey, TValue>;
    FLock: TObject;
    procedure Lock;
    procedure Unlock;
  public
    procedure Add(AKey: TKey; AValue: TValue);
    procedure Insert(AIndex: Integer; AKey: TKey; AValue: TValue);
    procedure Remove(AKey: TKey);
    procedure RemoveAllValues(AValue: TValue);
    procedure Delete(AIndex: Integer);
    procedure Clear;

    function ContainsKey(AKey: TKey): Boolean;
    function ContainsValue(AValue: TValue): Boolean;
    function Count: Integer;

    function LockPointer: TOrderedDict<TKey,TValue>;
    procedure UnlockPointer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadOrderedDict<TKey, TValue> }

procedure TThreadOrderedDict<TKey, TValue>.Add(AKey: TKey; AValue: TValue);
begin
  Lock;
  try
    FDict.Add(AKey, AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.Clear;
begin
  Lock;
  try
    FDict.Clear;
  finally
    Unlock;
  end;
end;

function TThreadOrderedDict<TKey, TValue>.ContainsKey(AKey: TKey): Boolean;
begin
  Lock;
  try
    Result := FDict.ContainsKey(AKey);
  finally
    Unlock;
  end;
end;

function TThreadOrderedDict<TKey, TValue>.ContainsValue(
  AValue: TValue): Boolean;
begin
  Lock;
  try
    Result := FDict.ContainsValue(AValue);
  finally
    Unlock;
  end;
end;

function TThreadOrderedDict<TKey, TValue>.Count: Integer;
begin
  Lock;
  try
    Result := FDict.Count;
  finally
    Unlock;
  end;
end;

constructor TThreadOrderedDict<TKey, TValue>.Create;
begin
  FLock := TObject.Create;
  FDict := TOrderedDict<TKey, TValue>.Create;
end;

procedure TThreadOrderedDict<TKey, TValue>.Delete(AIndex: Integer);
begin
  Lock;
  try
    FDict.Delete(AIndex);
  finally
    Unlock;
  end;
end;

destructor TThreadOrderedDict<TKey, TValue>.Destroy;
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

procedure TThreadOrderedDict<TKey, TValue>.Insert(AIndex: Integer; AKey: TKey; AValue: TValue);
begin
  Lock;
  try
    FDict.Insert(AIndex, AKey, AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.Lock;
begin
  TMonitor.Enter(FLock);
end;

function TThreadOrderedDict<TKey, TValue>.LockPointer: TOrderedDict<TKey, TValue>;
begin
  Lock;
  Result := FDict;
end;

procedure TThreadOrderedDict<TKey, TValue>.Remove(AKey: TKey);
begin
  Lock;
  try
    FDict.Remove(AKey);
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.RemoveAllValues(AValue: TValue);
begin
  Lock;
  try
    FDict.RemoveAllValues(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.Unlock;
begin
  TMonitor.Exit(FLock);

end;

procedure TThreadOrderedDict<TKey, TValue>.UnlockPointer;
begin
  Unlock;
end;

end.

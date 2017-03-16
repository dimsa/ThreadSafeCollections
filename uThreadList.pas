unit uThreadList;

interface

uses
  System.Generics.Collections;

type
  TThreadDict<TValue> = class
  private
    FList: TList<TValue>;
    FLock: TObject;
  protected
    function PointerUnsafeLock: TList<TValue>;
  public
    procedure Add(const AValue: TValue);
    procedure Remove(const AValue: TValue);
    procedure Clear;

    function Lock: TList<TValue>;
    procedure Unlock;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadDict<TKey, TValue> }

procedure TThreadDict<TValue>.Add(const AValue: TValue);
begin
  PointerUnsafeLock;
  try
    FList.Add(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TValue>.Clear;
begin
  PointerUnsafeLock;
  try
    FList.Clear;
  finally
    Unlock;
  end;
end;

constructor TThreadDict<TValue>.Create;
begin
  FLock := TObject.Create;
  FList := TList<TValue>.Create;
end;

destructor TThreadDict<TValue>.Destroy;
begin
  PointerUnsafeLock;
  try
    FList.Free;
    inherited Destroy;
  finally
    Unlock;
    FLock.Free;
  end;
  inherited;
end;

function TThreadDict<TValue>.Lock: TList<TValue>;
begin
  Result := TList<TValue>.Create(PointerUnsafeLock);
end;

function TThreadDict<TValue>.PointerUnsafeLock: TList<TValue>;
begin
  TMonitor.Enter(FLock);
  Result := FList;
end;

procedure TThreadDict<TValue>.Remove(const AValue: TValue);
begin
  PointerUnsafeLock;
  try
    FList.Remove(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadDict<TValue>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

end.

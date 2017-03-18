unit uThreadList;

interface

uses
  System.Generics.Collections;

type
  TThreadList<TValue> = class
  private
    FList: TList<TValue>;
    FLock: TObject;
  protected
    function PointerUnsafeCopy: TList<TValue>;
  public
    procedure Add(const AValue: TValue);
    procedure Remove(const AValue: TValue);
    procedure Delete(const AIndex: Integer);
    procedure Clear;

    function ItemsCopy: TList<TValue>;

    procedure Lock;
    procedure Unlock;
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
  inherited;
end;

function TThreadList<TValue>.ItemsCopy: TList<TValue>;
begin
  Lock;
  try
    Result := TList<TValue>.Create(PointerUnsafeCopy);
  finally
    Unlock;
  end;

end;

procedure TThreadList<TValue>.Lock;
begin
  TMonitor.Enter(FLock);
end;

function TThreadList<TValue>.PointerUnsafeCopy: TList<TValue>;
begin
  Result := FList;
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

procedure TThreadList<TValue>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

end.

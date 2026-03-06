unit ASTTest.Ops.OverloadCmpGeneric1;

interface

type
  TMyInt<T> = record
    class operator Equal(ALeft: TMyInt<T>; ARight: Integer): Boolean; overload;
    class operator Equal(ALeft: Integer; ARight: TMyInt<T>): Boolean; overload;

    class operator NotEqual(ALeft: TMyInt<T>; ARight: Integer): Boolean; overload;
    class operator NotEqual(ALeft: Integer; ARight: TMyInt<T>): Boolean; overload;

    class operator GreaterThan(ALeft: TMyInt<T>; ARight: Integer): Boolean; overload;
    class operator GreaterThan(ALeft: Integer; ARight: TMyInt<T>): Boolean; overload;

    class operator GreaterThanOrEqual(ALeft: TMyInt<T>; ARight: Integer): Boolean; overload;
    class operator GreaterThanOrEqual(ALeft: Integer; ARight: TMyInt<T>): Boolean; overload;

    class operator LessThan(ALeft: TMyInt<T>; ARight: Integer): Boolean; overload;
    class operator LessThan(ALeft: Integer; ARight: TMyInt<T>): Boolean; overload;

    class operator LessThanOrEqual(ALeft: TMyInt<T>; ARight: Integer): Boolean; overload;
    class operator LessThanOrEqual(ALeft: Integer; ARight: TMyInt<T>): Boolean; overload;
  end;

implementation

var
  MyInt: TMyInt<Byte>;

procedure Main;
begin
  if MyInt = 1 then;
  if MyInt <> 1 then;
  if MyInt > 1 then;
  if MyInt >= 1 then;
  if MyInt < 1 then;
  if MyInt <= 1 then;

  if 1 = MyInt then;
  if 1 <> MyInt then;
  if 1 > MyInt then;
  if 1 >= MyInt then;
  if 1 < MyInt then;
  if 1 <= MyInt then;
end;

{ TMyInt<T> }

class operator TMyInt<T>.Equal(ALeft: TMyInt<T>; ARight: Integer): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.Equal(ALeft: Integer; ARight: TMyInt<T>): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.GreaterThan(ALeft: Integer; ARight: TMyInt<T>): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.GreaterThan(ALeft: TMyInt<T>; ARight: Integer): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.GreaterThanOrEqual(ALeft: TMyInt<T>; ARight: Integer): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.GreaterThanOrEqual(ALeft: Integer; ARight: TMyInt<T>): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.LessThan(ALeft: Integer; ARight: TMyInt<T>): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.LessThanOrEqual(ALeft: Integer; ARight: TMyInt<T>): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.LessThanOrEqual(ALeft: TMyInt<T>; ARight: Integer): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.NotEqual(ALeft: TMyInt<T>; ARight: Integer): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.NotEqual(ALeft: Integer; ARight: TMyInt<T>): Boolean;
begin
  Result := False;
end;

class operator TMyInt<T>.LessThan(ALeft: TMyInt<T>; ARight: Integer): Boolean;
begin
  Result := False;
end;

end.
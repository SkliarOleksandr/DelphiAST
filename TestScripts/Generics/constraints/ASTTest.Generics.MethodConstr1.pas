unit ASTTest.Generics.MethodConstr1;

interface

{$HINTS OFF}

type
  TMyObject = class
    class function Check<T: class>(AObj: T): Boolean;  static; inline;
    class function Compare<TLeft, TRight: class>(ALeft: TLeft; ARight: TRight): Boolean; static; inline;
    class function Cast<TSrc, TDst: class>(const ASource: TSrc): TDst; static; inline;
  end;

implementation

{ TMyObject }

class function TMyObject.Cast<TSrc, TDst>(const ASource: TSrc): TDst;
begin
  Result := nil;
  if ASource is TDst then;
end;

class function TMyObject.Check<T>(AObj: T): Boolean;
begin
  Result := AObj is T;
end;

class function TMyObject.Compare<TLeft, TRight>(ALeft: TLeft; ARight: TRight): Boolean;
begin
  Result := (ALeft is TRight) or (ARight is TLeft);
end;

end.
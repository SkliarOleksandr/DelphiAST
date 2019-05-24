unit AST.Delphi.SysOperators;

interface

uses AST.Delphi.Classes, AST.Delphi.Contexts;

type

  TIDInternalOperator = class(TIDOperator)
  public
    constructor CreateAsIntOp; reintroduce;
  end;

  TIDInternalOpImplicit = class(TIDInternalOperator)
  public
    constructor CreateInternal(ResultType: TIDType); reintroduce;
    function Check(const Src, Dst: TIDType): Boolean; overload; virtual; abstract;
    function Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration; overload; virtual;
    function Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression; virtual;
  end;

  TIDInternalOpExplisit = class(TIDInternalOpImplicit)

  end;

  {sys explicit operator Enum -> Any}
  TSysExplicitEnumToAny = class(TIDInternalOpImplicit)
  private
    class var fInstance: TIDInternalOpImplicit;
  public
    function Check(const Src: TIDType; const Dst: TIDType): Boolean; override;
    class function Instance: TIDInternalOperator;
  end;

implementation

{ TIDInternalOperator }

constructor TIDInternalOperator.CreateAsIntOp;
begin
  CreateFromPool;
end;


{ TIDInternalOpImplicit }

function TIDInternalOpImplicit.Check(const Src: TIDExpression; const Dst: TIDType): TIDDeclaration;
begin
  if Check(Src.DataType, Dst) then
    Result := Dst
  else
    Result := nil;
end;

constructor TIDInternalOpImplicit.CreateInternal(ResultType: TIDType);
begin
  CreateFromPool;
  ItemType := itProcedure;
  Self.DataType := ResultType;
end;

function TIDInternalOpImplicit.Match(const SContext: PSContext; const Src: TIDExpression; const Dst: TIDType): TIDExpression;
begin
  if Check(Src.DataType, Dst) then
    Result := Src
  else
    Result := nil;
end;

{ TSysExplicitEnumToAny }

function TSysExplicitEnumToAny.Check(const Src, Dst: TIDType): Boolean;
begin
  Result := Dst.Ordinal;
end;

class function TSysExplicitEnumToAny.Instance: TIDInternalOperator;
begin
  if not Assigned(fInstance) then
    fInstance := TSysExplicitEnumToAny.CreateAsIntOp();
  Result := fInstance;
end;

end.

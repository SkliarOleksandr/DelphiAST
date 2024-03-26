unit AST.Delphi.SysTypes;

interface

uses
  AST.Classes,
  AST.Delphi.Intf,
  AST.Delphi.Operators,
  AST.Delphi.Classes;

type

  TBuiltin_IntType = class(TIDOrdinal)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); virtual; abstract;
  end;

  TBuiltin_FltType = class(TIDType)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); virtual; abstract;
    function BinarOperator(Op: TOperatorID; Right: TIDType): TIDType; override;
  end;

  TBuiltin_StrType = class(TIDString)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); virtual; abstract;
  end;

//  TST_Byte = class(TSys_IntType)
//  public
//    constructor CreateAsBuiltin(ASystem: TASTModule); override;
//    procedure CreateStandardOperators; override;
//  end;
//
//  TST_ShortInt = class(TSys_IntType)
//  public
//    constructor CreateAsBuiltin(ASystem: TASTModule); override;
//    procedure CreateStandardOperators; override;
//  end;

  TBuiltin_Extended = class(TBuiltin_FltType)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); override;
  end;

  TBuiltin_Currency = class(TBuiltin_FltType)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); override;
  end;

  TBuiltin_Comp = class(TBuiltin_FltType)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); override;
  end;

  TBuiltin_OpenString = class(TBuiltin_StrType)
  public
    procedure SetupOperators(ADecls: PDelphiSystemDeclarations); override;
  end;

  TBuiltin_AnsiString = class(TBuiltin_StrType)
  public
    function MatchImplicitTo(ADst: TIDType): Boolean;
  end;


implementation

uses
  AST.Parser.Utils,
  AST.Delphi.Parser,
  AST.Delphi.DataTypes,
  AST.Delphi.System;

{ TST_Byte }

//constructor TST_Byte.CreateAsBuiltin(ASystem: TASTModule);
//begin
//  inherited CreateAsSystem(TASTDelphiUnit(ASystem).IntfScope, 'Byte');
//  DataTypeID := dtUInt8;
//  LowBound := 0;
//  HighBound := MaxUInt8;
//end;
//
//procedure TST_Byte.CreateStandardOperators;
//begin
//  inherited;
//
//end;

{ TST_ShortInt }
//
//constructor TST_ShortInt.CreateAsBuiltin(ASystem: TASTModule);
//begin
//  inherited CreateAsSystem(TASTDelphiUnit(ASystem).IntfScope, 'Byte');
//  DataTypeID := dtInt8;
//  LowBound := MinInt8;
//  HighBound := MaxInt8;
//end;
//
//procedure TST_ShortInt.CreateStandardOperators;
//begin
//  inherited;
//
//end;

{ TBuiltin_Comp }

procedure TBuiltin_Comp.SetupOperators(ADecls: PDelphiSystemDeclarations);
begin
  // Comparisons
  OverloadAllCmpOperators;
  // Implicits
//  OverloadImplicitTo(ADecls._Float32);
//  OverloadImplicitTo(ADecls._Float64);
//  OverloadImplicitTo(ADecls._Float80);
//  OverloadImplicitTo(ADecls._Currency);



 // OverloadImplicitTo(ADecls._Variant, Operators.ImplicitVariantFromAny);

end;

{ TBuiltin_Extended }

procedure TBuiltin_Extended.SetupOperators(ADecls: PDelphiSystemDeclarations);
begin
  OverloadBinarOperator2(opDivide, Self, Self);
end;

{ TBuiltin_Currency }

procedure TBuiltin_Currency.SetupOperators(ADecls: PDelphiSystemDeclarations);
begin
  // Comparisons
  OverloadAllCmpOperators;

end;

{ TBuiltin_OpenString }

procedure TBuiltin_OpenString.SetupOperators(ADecls: PDelphiSystemDeclarations);
begin
  // Comparisons
  OverloadAllCmpOperators;

end;

{ TBuiltin_AnsiString }

function TBuiltin_AnsiString.MatchImplicitTo(ADst: TIDType): Boolean;
begin
  Result := (ADSt.DataTypeID = dtString) or
            ((ADst.DataTypeID = dtStaticArray) and (TIDStaticArray(ADst).ElementDataType.DataTypeID = dtAnsiChar));
end;


{ TBuiltin_FltType }

function TBuiltin_FltType.BinarOperator(Op: TOperatorID; Right: TIDType): TIDType;
begin
  case Op of
    opAdd: Result := Self;
    opSubtract: Result := Self;
    opMultiply: Result := Self;
    opDivide: Result := Self;
  else
    Result := inherited;
  end;
end;

end.

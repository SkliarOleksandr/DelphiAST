unit NPCompiler.Evaluater;

interface

uses NPCompiler.Classes;

type
  INPCEvaluater = interface
    ['{6339DA72-C482-4F64-9F6A-E9175005A699}']
    function Evaluate(const Proc: TIDProcedure): TIDConstant;
  end;

  TNPCEvaluater = class(TInterfacedObject, INPCEvaluater)
  protected
    //FT: TILTranslator;
  public
    function Evaluate(const Proc: TIDProcedure): TIDConstant; virtual;
  end;


implementation

{ TNPCEvaluater }

function TNPCEvaluater.Evaluate(const Proc: TIDProcedure): TIDConstant;
begin

end;

end.

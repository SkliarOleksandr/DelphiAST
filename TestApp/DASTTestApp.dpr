program DASTTestApp;

uses
  Vcl.Forms,
  TestAppMain in 'TestAppMain.pas' {frmTestAppMain},
  ARC in '..\Source\ARC.pas',
  AVL in '..\Source\AVL.pas',
  IL.Instructions in '..\Source\IL.Instructions.pas',
  IL.TypeInfo in '..\Source\IL.TypeInfo.pas',
  IL.Types in '..\Source\IL.Types.pas',
  NPCompiler.Classes in '..\Source\NPCompiler.Classes.pas',
  NPCompiler.ConstCalculator in '..\Source\NPCompiler.ConstCalculator.pas',
  NPCompiler.Contexts in '..\Source\NPCompiler.Contexts.pas',
  NPCompiler.DataTypes in '..\Source\NPCompiler.DataTypes.pas',
  NPCompiler.Errors in '..\Source\NPCompiler.Errors.pas',
  NPCompiler.Evaluater in '..\Source\NPCompiler.Evaluater.pas',
  NPCompiler.Evaluater.VM in '..\Source\NPCompiler.Evaluater.VM.pas',
  NPCompiler.ExpressionContext in '..\Source\NPCompiler.ExpressionContext.pas',
  NPCompiler.Intf in '..\Source\NPCompiler.Intf.pas',
  NPCompiler.Messages in '..\Source\NPCompiler.Messages.pas',
  NPCompiler.Operators in '..\Source\NPCompiler.Operators.pas',
  NPCompiler.Options in '..\Source\NPCompiler.Options.pas',
  NPCompiler.Package in '..\Source\NPCompiler.Package.pas',
  NPCompiler.SysFunctions in '..\Source\NPCompiler.SysFunctions.pas',
  NPCompiler.Utils in '..\Source\NPCompiler.Utils.pas',
  NPLCompiler.Targets in '..\Source\NPLCompiler.Targets.pas',
  OPCompiler in '..\Source\OPCompiler.pas',
  SystemUnit in '..\Source\SystemUnit.pas',
  iDStringParser in '..\Source\Parser\iDStringParser.pas',
  OPCompiler.Parser in '..\Source\Parser\OPCompiler.Parser.pas',
  AST.Classes in '..\Source\AST.Classes.pas',
  AST.Delphi.Parser in '..\Source\AST.Delphi.Parser.pas',
  AST.Project in '..\Source\AST.Project.pas',
  AST.Delphi.Project in '..\Source\AST.Delphi.Project.pas',
  AST.Writer in '..\Source\AST.Writer.pas',
  AST.Parser.Contexts in '..\Source\AST.Parser.Contexts.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestAppMain, frmTestAppMain);
  Application.Run;
end.

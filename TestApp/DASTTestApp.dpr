program DASTTestApp;

uses
  Vcl.Forms,
  TestAppMain in 'TestAppMain.pas' {frmTestAppMain},
  AVL in '..\Source\AVL.pas',
  AST.Delphi.Classes in '..\Source\AST.Delphi.Classes.pas',
  AST.Pascal.ConstCalculator in '..\Source\AST.Pascal.ConstCalculator.pas',
  NPCompiler.Contexts in '..\Source\NPCompiler.Contexts.pas',
  NPCompiler.DataTypes in '..\Source\NPCompiler.DataTypes.pas',
  NPCompiler.Errors in '..\Source\NPCompiler.Errors.pas',
  NPCompiler.Intf in '..\Source\NPCompiler.Intf.pas',
  NPCompiler.Messages in '..\Source\NPCompiler.Messages.pas',
  AST.Delphi.Operators in '..\Source\AST.Delphi.Operators.pas',
  NPCompiler.Options in '..\Source\NPCompiler.Options.pas',
  NPCompiler.Package in '..\Source\NPCompiler.Package.pas',
  AST.Delphi.SysFunctions in '..\Source\AST.Delphi.SysFunctions.pas',
  NPCompiler.Utils in '..\Source\NPCompiler.Utils.pas',
  NPLCompiler.Targets in '..\Source\NPLCompiler.Targets.pas',
  AST.Pascal.Parser in '..\Source\AST.Pascal.Parser.pas',
  AST.Delphi.System in '..\Source\AST.Delphi.System.pas',
  AST.Lexer.Delphi in '..\Source\Lexers\AST.Lexer.Delphi.pas',
  AST.Classes in '..\Source\AST.Classes.pas',
  AST.Delphi.Parser in '..\Source\AST.Delphi.Parser.pas',
  AST.Project in '..\Source\AST.Project.pas',
  AST.Delphi.Project in '..\Source\AST.Delphi.Project.pas',
  AST.Writer in '..\Source\AST.Writer.pas',
  AST.Parser.Contexts in '..\Source\AST.Parser.Contexts.pas',
  AST.Delphi.SysOperators in '..\Source\AST.Delphi.SysOperators.pas',
  AST.Delphi.Contexts in '..\Source\AST.Delphi.Contexts.pas',
  AST.Delphi.Errors in '..\Source\AST.Delphi.Errors.pas',
  AST.Parser.Errors in '..\Source\AST.Parser.Errors.pas',
  AST.Lexer in '..\Source\Lexers\AST.Lexer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestAppMain, frmTestAppMain);
  Application.Run;
end.

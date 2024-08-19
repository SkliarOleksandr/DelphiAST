program DASTTestApp;


{.$define FullDebugMode}

uses
  {$IFDEF FullDebugMode} FastMM4, {$ENDIF}
  Vcl.Forms,
  TestAppMain in 'TestAppMain.pas' {frmTestAppMain},
  AVL in '..\Source\AVL.pas',
  AST.Delphi.Classes in '..\Source\AST.Delphi.Classes.pas',
  AST.Pascal.ConstCalculator in '..\Source\AST.Pascal.ConstCalculator.pas',
  AST.Delphi.DataTypes in '..\Source\AST.Delphi.DataTypes.pas',
  AST.Parser.Messages in '..\Source\AST.Parser.Messages.pas',
  AST.Delphi.Operators in '..\Source\AST.Delphi.Operators.pas',
  AST.Parser.Options in '..\Source\AST.Parser.Options.pas',
  AST.Pascal.Project in '..\Source\AST.Pascal.Project.pas',
  AST.Delphi.SysFunctions in '..\Source\AST.Delphi.SysFunctions.pas',
  AST.Parser.Utils in '..\Source\AST.Parser.Utils.pas',
  AST.Targets in '..\Source\AST.Targets.pas',
  AST.Pascal.Parser in '..\Source\AST.Pascal.Parser.pas',
  AST.Delphi.System in '..\Source\AST.Delphi.System.pas',
  AST.Lexer.Delphi in '..\Source\Lexers\AST.Lexer.Delphi.pas',
  AST.Classes in '..\Source\AST.Classes.pas',
  AST.Delphi.Parser in '..\Source\AST.Delphi.Parser.pas',
  AST.Intf in '..\Source\AST.Intf.pas',
  AST.Delphi.Project in '..\Source\AST.Delphi.Project.pas',
  AST.Writer in '..\Source\AST.Writer.pas',
  AST.Parser.Contexts in '..\Source\AST.Parser.Contexts.pas',
  AST.Delphi.SysOperators in '..\Source\AST.Delphi.SysOperators.pas',
  AST.Delphi.Contexts in '..\Source\AST.Delphi.Contexts.pas',
  AST.Delphi.Errors in '..\Source\AST.Delphi.Errors.pas',
  AST.Parser.Errors in '..\Source\AST.Parser.Errors.pas',
  AST.Lexer in '..\Source\Lexers\AST.Lexer.pas',
  AST.Parser.ProcessStatuses in '..\Source\AST.Parser.ProcessStatuses.pas',
  AST.Delphi.Options in '..\Source\AST.Delphi.Options.pas',
  AST.Delphi.Intf in '..\Source\AST.Delphi.Intf.pas',
  AST.Pascal.Intf in '..\Source\AST.Pascal.Intf.pas',
  AST.Delphi.Operators.Signatures in '..\Source\AST.Delphi.Operators.Signatures.pas',
  AST.Parser.Log in '..\Source\AST.Parser.Log.pas',
  AST.Delphi.SysTypes in '..\Source\AST.Delphi.SysTypes.pas',
  AST.Delphi.Declarations in '..\Source\AST.Delphi.Declarations.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestAppMain, frmTestAppMain);
  Application.Run;
end.

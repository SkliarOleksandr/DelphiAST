program DASTTestApp;


{.$define FullDebugMode}

uses
  {$IFDEF FullDebugMode}
  FastMM4,
  {$ENDIF }
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
  AST.Delphi.Declarations in '..\Source\AST.Delphi.Declarations.pas',
  ASTTest.Implicit.Pointers in '..\TestScripts\Implict\ASTTest.Implicit.Pointers.pas',
  ASTTest.Cmp.Strings in '..\TestScripts\Operators\Compare\ASTTest.Cmp.Strings.pas',
  ASTTest.Cmp.PChars in '..\TestScripts\Operators\Compare\ASTTest.Cmp.PChars.pas',
  ASTTest.Sub.Integer in '..\TestScripts\Operators\Substruct\ASTTest.Sub.Integer.pas',
  ASTTest.Add.Pointers in '..\TestScripts\Operators\Add\ASTTest.Add.Pointers.pas',
  ASTTest.Cmp.Pointers in '..\TestScripts\Operators\Compare\ASTTest.Cmp.Pointers.pas',
  ASTTest.Ops.OverloadWithImplicitCast in '..\TestScripts\Operators\Overload\ASTTest.Ops.OverloadWithImplicitCast.pas',
  AST.Utils.CmdLineParser in 'AST.Utils.CmdLineParser.pas',
  ASTTest.Generics.Class1 in '..\TestScripts\Generics\ASTTest.Generics.Class1.pas',
  ASTTest.Generics.GenResult in '..\TestScripts\Generics\ASTTest.Generics.GenResult.pas',
  ASTTest.Generics.Methods in '..\TestScripts\Generics\ASTTest.Generics.Methods.pas',
  ASTTest.Generics.Mixed in '..\TestScripts\Generics\ASTTest.Generics.Mixed.pas',
  TASTTest.Classes.Inherited1 in '..\TestScripts\Classes\TASTTest.Classes.Inherited1.pas',
  TASTTest.Classes.Inherited2 in '..\TestScripts\Classes\TASTTest.Classes.Inherited2.pas',
  ASTTest.Ordian.Enums.Init in '..\TestScripts\Ordinals\ASTTest.Ordian.Enums.Init.pas',
  ASTTest.Procs.External in '..\TestScripts\Procedures\ASTTest.Procs.External.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestAppMain, frmTestAppMain);
  Application.Run;
end.

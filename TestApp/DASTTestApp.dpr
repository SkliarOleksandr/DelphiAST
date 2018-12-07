program DASTTestApp;

uses
  Vcl.Forms,
  TestAppMain in 'TestAppMain.pas' {frmTestAppMain},
  ARC in '..\DCA\DelphiAST\ARC.pas',
  AVL in '..\DCA\DelphiAST\AVL.pas',
  IL.Instructions in '..\DCA\DelphiAST\IL.Instructions.pas',
  IL.TypeInfo in '..\DCA\DelphiAST\IL.TypeInfo.pas',
  IL.Types in '..\DCA\DelphiAST\IL.Types.pas',
  NPCompiler.Classes in '..\DCA\DelphiAST\NPCompiler.Classes.pas',
  NPCompiler.ConstCalculator in '..\DCA\DelphiAST\NPCompiler.ConstCalculator.pas',
  NPCompiler.DataTypes in '..\DCA\DelphiAST\NPCompiler.DataTypes.pas',
  NPCompiler.Errors in '..\DCA\DelphiAST\NPCompiler.Errors.pas',
  NPCompiler.Evaluater in '..\DCA\DelphiAST\NPCompiler.Evaluater.pas',
  NPCompiler.Evaluater.VM in '..\DCA\DelphiAST\NPCompiler.Evaluater.VM.pas',
  NPCompiler.Intf in '..\DCA\DelphiAST\NPCompiler.Intf.pas',
  NPCompiler.Messages in '..\DCA\DelphiAST\NPCompiler.Messages.pas',
  NPCompiler.Operators in '..\DCA\DelphiAST\NPCompiler.Operators.pas',
  NPCompiler.Options in '..\DCA\DelphiAST\NPCompiler.Options.pas',
  NPCompiler.Package in '..\DCA\DelphiAST\NPCompiler.Package.pas',
  OPCompiler in '..\DCA\DelphiAST\OPCompiler.pas',
  NPCompiler.SysFunctions in '..\DCA\DelphiAST\NPCompiler.SysFunctions.pas',
  NPCompiler.Utils in '..\DCA\DelphiAST\NPCompiler.Utils.pas',
  SystemUnit in '..\DCA\DelphiAST\SystemUnit.pas',
  iDStringParser in '..\DCA\DelphiAST\Parser\iDStringParser.pas',
  OPCompiler.Parser in '..\DCA\DelphiAST\Parser\OPCompiler.Parser.pas',
  NPLCompiler.Targets in '..\DCA\DelphiAST\NPLCompiler.Targets.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTestAppMain, frmTestAppMain);
  Application.Run;
end.

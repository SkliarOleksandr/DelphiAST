unit AST.Delphi.Options;

interface

uses AST.Parser.Options;

type
  TWarnOption = class(TOption)
  public
    class function ArgsCount: Integer; override;
  end;

  TDelphiOptions = class(TOptions)
  private
    fOptSCOPEDENUMS: TBoolOption;
    fOptPOINTERMATH: TBoolOption;
    function GetSCOPEDENUMSValue: Boolean;
    function GetPOINTERMATHValue: Boolean;
  public
    constructor Create(Parent: TOptions); override;
    property SCOPEDENUMS: Boolean read GetSCOPEDENUMSValue;
    property POINTERMATH: Boolean read GetPOINTERMATHValue;
  end;


implementation

{ TWarnOption }

class function TWarnOption.ArgsCount: Integer;
begin
  Result := 2;
end;

{ TDelphiOptions }

constructor TDelphiOptions.Create(Parent: TOptions);
begin
  inherited;
  fOptSCOPEDENUMS := AddBoolOption('SCOPEDENUMS');
  fOptPOINTERMATH := AddBoolOption('POINTERMATH');
//// Align fields (Delphi)
//{$A},{$ALIGN}
//// Application type (Delphi)
//{$APPTYPE}
//// Assert directives (Delphi)
//{$C},{$ASSERTIONS}
//// Boolean short-circuit evaluation (Delphi compiler directive)
//{$B},{$BOOLEVAL}
//// Code align (Delphi)
//{$CODEALIGN}
//// Debug information (Delphi)
//{$D},{$DEBUGINFO}
//// DENYPACKAGEUNIT directive (Delphi)
//{$DENYPACKAGEUNIT}
//// Description (Delphi)
//{$D},{$DESCRIPTION}
//// DESIGNONLY directive (Delphi)
//{$DESIGNONLY}
//// Executable extension (Delphi)
//{$E},{$EXTENSION}
//// Export symbols (Delphi)
//{$ObjExportAll}
//// Extended syntax (Delphi)
//{$X},{$EXTENDEDSYNTAX}
//// Extended type compatibility (Delphi)
//{$EXTENDEDCOMPATIBILITY}
//// External Symbols (Delphi)
//{$EXTERNALSYM [ 'typeNameInHpp' [ 'typeNameInHppUnion' ]]}
//// Floating point precision control (Delphi for x64)
//{$EXCESSPRECISION}
//// HIGHCHARUNICODE directive (Delphi)
//{$HIGHCHARUNICODE}
//// Hints (Delphi)
//{$HINTS}
//// HPP emit (Delphi)
//{$HPPEMIT}
//// Image base address
//{$IMAGEBASE}
//// Implicit Build (Delphi)
//{$IMPLICITBUILD}
//// Imported data
//{$G},{$IMPORTEDDATA}
//// Include file (Delphi)
//{$I},{$INCLUDE}
//// Input output checking (Delphi)
//{$I},{$IOCHECKS}
//// Compiler directives for libraries or shared objects (Delphi)
//{$LIBPREFIX}, {$LIBSUFFIX}, {$LIBVERSION}
//// Link object file (Delphi)
//{$L file},{$LINK file}
//// Local symbol information (Delphi)
//{$L+},{$LOCALSYMBOLS}
//// Long strings (Delphi)
//{$H},{$LONGSTRINGS}
//// Memory allocation sizes (Delphi)
//{$M},{$MINSTACKSIZE},{$MAXSTACKSIZE}
//// METHODINFO directive (Delphi)
//{$METHODINFO}
//// Minimum enumeration size (Delphi)
//{$Z1},{$Z2},{$Z4},{$MINENUMSIZE 1},{$MINENUMSIZE 2},{$MINENUMSIZE 4}
//// NOINCLUDE (Delphi)
//{$NOINCLUDE}
//// OBJTYPENAME directive (Delphi)
//{$OBJTYPENAME typeIdent ['{B|N}typeNameInObj']}
//// Old type layout (Delphi)
//{$OLDTYPELAYOUT ON}
//// Open String Parameters (Delphi)
//{$P},{$OPENSTRINGS}
//// Optimization (Delphi)
//{$O},{$OPTIMIZATION}
//// Overflow checking (Delphi)
//{$Q},{$OVERFLOWCHECKS}
//// PE (portable executable) header flags (Delphi)
//{$SetPEFlags},{$SetPEOptFlags}
//// PE header operating system version
//{$SETPEOSVERSION}
//// PE header subsystem version
//{$SETPESUBSYSVERSION}
//// PE header user version
//{$SETPEUSERVERSION}
//// Pentium-safe FDIV operations (Delphi)
//{$U},{$SAFEDIVIDE}
//// Pointer Math (Delphi)
//{$POINTERMATH}
//// Range checking
//{$R},{$RANGECHECKS}
//// Real48 compatibility (Delphi)
//{$REALCOMPATIBILITY}
//// Regions
//{$REGION},{$ENDREGION}
//// Reserved address space for resources (Delphi, Linux)
//{$M},{$RESOURCERESERVE}
//// Resource file (Delphi)
//{$R},{$RESOURCE}
//// RTTI directive (Delphi)
//{$RTTI INHERIT|EXPLICIT}
//// RUNONLY directive (Delphi)
//{$RUNONLY}
//// Run-Time Type Information (Delphi)
//{$M},{$TYPEINFO}
//// Scoped Enums (Delphi)
//{$SCOPEDENUMS}
//// Stack frames (Delphi)
//{$W},{$STACKFRAMES}
//// Strong link types (Delphi)
//{$STRONGLINKTYPES}
//// Symbol declaration and cross-reference information (Delphi)
//{$Y},{$REFERENCEINFO},{DEFINITIONINFO}
//// Type-checked pointers (Delphi)
//{$T},{$TYPEDADDRESS}
//// Var-string checking (Delphi)
//{$V},{$VARSTRINGCHECKS}
//// Warning messages (Delphi)
//{$WARN}
//// Warnings (Delphi)
//{$WARNINGS}
//// Weak packaging
//{$WEAKPACKAGEUNIT}
//// WEAKLINKRTTI directive (Delphi)
//{$WEAKLINKRTTI}
//// Writeable typed constants (Delphi)
//{$J},{$WRITEABLECONST}
//// Zero-based strings (Delphi)
//{$ZEROBASEDSTRINGS}
end;

function TDelphiOptions.GetPOINTERMATHValue: Boolean;
begin
  Result := fOptPOINTERMATH.Value;
end;

function TDelphiOptions.GetSCOPEDENUMSValue: Boolean;
begin
  Result := fOptSCOPEDENUMS.Value;
end;

end.

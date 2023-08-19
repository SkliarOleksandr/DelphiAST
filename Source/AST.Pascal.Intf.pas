unit AST.Pascal.Intf;

interface

uses
  System.SysUtils, System.Classes, System.Types, Generics.Collections,
  AST.Intf,
  AST.Classes,
  AST.Parser.Utils,
  AST.Parser.Messages,
  AST.Parser.Options;

type
  TUnits = TList<TASTModule>;
  TEnumDeclProc = procedure (Module: TASTModule; Decl: TASTDeclaration);


  IPascalProjectSettings = interface(IASTProjectSettings)

  end;

  IASTPascalProject = interface(IASTProject)
    ['{32BDB0E9-59FD-4116-9B7F-6B2DEAB26F59}']
    function GetStringConstant(const Value: string): Integer; overload;
    function GetIncludeDebugInfo: Boolean;
    function GetUnitsCount: Integer;
    function GetUnit(Index: Integer): TASTModule; overload;
    function GetSearchPathes: TStrings;
    function GetOptions: TPackageOptions;
    function GetTarget: string;
    function GetDefines: TDefines;
    function FindUnitFile(const UnitName: string): string;
    function GetUnit(const UnitName: string): TObject; overload;
    function UsesUnit(const UnitName: string; AfterUnit: TASTModule): TASTModule;
    function GetSysUnit: TASTModule;
    function GetStopCompileIfError: Boolean;
    function GetCompileAll: Boolean;
    procedure SetStopCompileIfError(const Value: Boolean);
    procedure SetCompileAll(const Value: Boolean);
    procedure SetIncludeDebugInfo(const Value: Boolean);
    procedure SetRTTICharset(const Value: TRTTICharset);
    procedure SetTarget(const Value: string);
    procedure SaveToStream(Stream: TStream);
    procedure AddUnit(aUnit, AfterUnit: TASTModule); overload;
    procedure AddUnit(const FileName: string); overload;
    procedure AddUnitSource(const Source: string); overload;
    procedure AddUnitSearchPath(const Path: string; IncludeSubDirectories: Boolean = True);
    procedure Clear;
    procedure EnumIntfDeclarations(const EnumProc: TEnumASTDeclProc);
    procedure EnumAllDeclarations(const EnumProc: TEnumASTDeclProc);
    procedure DoBeforeCompileUnit(AUnit: TASTModule);
    procedure DoFinishCompileUnit(AUnit: TASTModule; AIntfOnly: Boolean);
    procedure PutMessage(const Message: TCompilerMessage); overload;
    function GetMessages: ICompilerMessages;
    function GetRTTICharset: TRTTICharset;
    function RefCount: Integer;
    function Compile: TCompilerResult;
    function CompileInterfacesOnly: TCompilerResult;
    function GetPointerSize: Integer;
    function GetNativeIntSize: Integer;
    property Messages: ICompilerMessages read GetMessages;
    property RTTICharset: TRTTICharset read GetRTTICharset write SetRTTICharset;
    property IncludeDebugInfo: Boolean read GetIncludeDebugInfo write SetIncludeDebugInfo;
    property StopCompileIfError: Boolean read GetStopCompileIfError write SetStopCompileIfError;
    property CompileAll: Boolean read GetCompileAll write SetCompileAll;
    property UnitsCount: Integer read GetUnitsCount;
    property Units[Index: Integer]: TASTModule read GetUnit;
    property SearchPathes: TStrings read GetSearchPathes;
    property Options: TPackageOptions read GetOptions;
    property Target: string read GetTarget write SetTarget;
    property Defines: TDefines read GetDefines;
    property PointerSize: Integer read GetPointerSize;
    property NativeIntSize: Integer read GetNativeIntSize;
    property SysUnit: TASTModule read GetSysUnit;
  end;


implementation

end.

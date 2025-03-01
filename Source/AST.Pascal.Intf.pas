unit AST.Pascal.Intf;

interface

uses
  System.SysUtils, System.Classes, System.Types, Generics.Collections,
  AST.Intf,
  AST.Classes,
  AST.Targets,
  AST.Parser.Utils,
  AST.Parser.Messages,
  AST.Parser.Options;

type
  IASTPascalUnit = interface(IASTModule)
    ['{C846B6A1-A57A-42B3-8BA0-8DD0931F3D35}']
    function Defined(const ADefine: string): Boolean;
  end;

  IPascalProjectSettings = interface(IASTProjectSettings)
    ['{354555F9-A1EE-408C-B6A3-2BE412D07077}']
  end;

  IASTPascalProject = interface(IASTProject)
    ['{32BDB0E9-59FD-4116-9B7F-6B2DEAB26F59}']
    function GetStringConstant(const Value: string): Integer; overload;
    function GetIncludeDebugInfo: Boolean;
    function GetUnitsCount: Integer;
    function GetUnit(Index: Integer): IASTPascalUnit; overload;
    function GetAllUnitsCount: Integer;
    function GetAllUnit(AIndex: Integer): IASTPascalUnit;
    function GetSearchPathes: TStrings;
    function GetOptions: TPackageOptions;
    function GetTarget: TASTTargetClass;
    function GetDefines: TDefines;
    function FindUnitFile(const AUnitName: string; const AFileExt: string = ''): string;
    function FindParsedUnit(const AUnitName: string): IASTPascalUnit;
    function GetUnit(const AUnitName: string): TObject; overload;
    function UsesUnit(const AUnitName: string; const AUnitPath: string): IASTPascalUnit;
    function GetSysUnit: IASTPascalUnit;
    function GetCompileAll: Boolean;
    procedure SetCompileAll(const Value: Boolean);
    procedure SetIncludeDebugInfo(const Value: Boolean);
    procedure SetRTTICharset(const Value: TRTTICharset);
    procedure SetTarget(const Value: TASTTargetClass);
    procedure SaveToStream(Stream: TStream);
    procedure AddUnit(const AUnit, BeforeUnit: IASTPascalUnit); overload;
    procedure AddUnit(const FileName: string); overload;
    procedure AddUnitSource(const Source: string); overload;
    procedure AddUnitSearchPath(const APath: string; AIncludeSubDirs: Boolean = True);
    procedure RemoveUnitSearchPath(const APath: string);
    procedure Clear(AClearImplicitUnits: Boolean);
    procedure EnumDeclarations(const AEnumProc: TEnumASTDeclProc; AUnitScope: TUnitScopeKind);
    procedure DoBeforeCompileUnit(AUnit: TASTModule);
    procedure DoFinishCompileUnit(AUnit: TASTModule; AIntfOnly: Boolean);
    procedure SetUnitScopeNames(const Value: string);
    procedure SetParseSystemUnit(AValue: Boolean);
    procedure SetProjectFileName(const Value: string);
    function GetMessages: ICompilerMessages;
    function GetRTTICharset: TRTTICharset;
    function RefCount: Integer;
    function Compile: TCompilerResult;
    function CompileInterfacesOnly: TCompilerResult;
    function GetPointerSize: Integer;
    function GetNativeIntSize: Integer;
    function GetVariantSize: Integer;
    function GetUnitScopeNames: string;
    function GetParseSystemUnit: Boolean;
    function GetProjectFileName: string;
    property Messages: ICompilerMessages read GetMessages;
    property RTTICharset: TRTTICharset read GetRTTICharset write SetRTTICharset;
    property IncludeDebugInfo: Boolean read GetIncludeDebugInfo write SetIncludeDebugInfo;
    property CompileAll: Boolean read GetCompileAll write SetCompileAll;
    property UnitsCount: Integer read GetUnitsCount;
    property Units[Index: Integer]: IASTPascalUnit read GetUnit;
    property AllUnitsCount: Integer read GetAllUnitsCount;
    property AllUnits[AIndex: Integer]: IASTPascalUnit read GetAllUnit;
    property SearchPathes: TStrings read GetSearchPathes;
    property Options: TPackageOptions read GetOptions;
    property Target: TASTTargetClass read GetTarget write SetTarget;
    property Defines: TDefines read GetDefines;
    property PointerSize: Integer read GetPointerSize;
    property NativeIntSize: Integer read GetNativeIntSize;
    property VariantSize: Integer read GetVariantSize;
    property SysUnit: IASTPascalUnit read GetSysUnit;
    property UnitScopeNames: string read GetUnitScopeNames write SetUnitScopeNames;
    property ParseSystemUnit: Boolean read GetParseSystemUnit write SetParseSystemUnit;
    property ProjectFileName: string read GetProjectFileName write SetProjectFileName;
  end;


implementation

end.

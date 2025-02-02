unit AST.Intf;

interface

uses
  System.SysUtils,
  AST.Lexer,
  AST.Parser.ProcessStatuses,
  AST.JsonSchema;

type

  TCompilerResult = (
    CompileNone,
    CompileInProgress,
    CompileSuccess,
    CompileFail,
    CompileSkip
  );

  TUnitScopeKind = (
    scopeBoth,
    scopeInterface,
    scopeImplementation
  );

  IASTModule = interface;
  IASTProject = interface;
  IASTDeclaration = interface;


  TEnumASTDeclProc = reference to procedure (const Module: IASTModule; const Decl: IASTDeclaration);

  IASTModule = interface
    ['{1E3A5748-1671-41E8-BAAD-4BBB2B363BF4}']
    function GetModuleName: string;
    function GetTotalLinesParsed: Integer;
    property Name: string read GetModuleName;
    property TotalLinesParsed: Integer read GetTotalLinesParsed;

    procedure PutError(const AMessage: string; const ATextPosition: TTextPosition; ACritical: Boolean = False); overload;
    procedure PutError(const AMessage: string; const AParams: array of const; const ATextPosition: TTextPosition; ACritical: Boolean = False); overload;

    procedure EnumDeclarations(const AEnumProc: TEnumASTDeclProc; AUnitScope: TUnitScopeKind);

    function Compile(ACompileIntfOnly: Boolean; RunPostCompile: Boolean = True): TCompilerResult;

    function Lexer_Line: Integer;
    function Lexer_Position: TTextPosition;

    function ToJson: TJsonASTDeclaration;
  end;

  TCompilerMessageType = (cmtHint, cmtWarning, cmtError, cmtInteranlError);

  TASTProgressEvent = reference to procedure (const Module: IASTModule; Status: TASTProcessStatusClass; AElapsedTime: Int64);
  TASTRrojectConsoleWriteEvent = reference to procedure (const Module: IASTModule; Line: Integer; const Message: string);

  IASTParserMessage = interface
    ['{50649C47-C46C-45B6-B87D-D9779484E227}']
    function GetModuleSource: string;
    function GetModuleName: string;
    function GetModule: IASTModule;
    procedure SetModule(const Value: IASTModule);
    procedure SetModuleName(const Value: string);
    function GetMessageType: TCompilerMessageType;
    function GetMessageTypeName: string;
    function GetMessageText: string;
    function GetCol: Integer;
    function GetRow: Integer;

    property Module: IASTModule read GetModule write SetModule;
    property ModuleName: string read GetModuleName write SetModuleName;
    property ModuleSource: string read GetModuleSource;
    property MessageType: TCompilerMessageType read GetMessageType;
    property MessageTypeName: string read GetMessageTypeName;
    property MessageText: string read GetMessageText;
    property Row: Integer read GetRow;
    property Col: Integer read GetCol;
    function AsString: string;
  end;

  IASTProject = interface
    ['{AE77D75A-4F7F-445B-ADF9-47CF5C2F0A14}']
    procedure SetName(const Value: string);
    procedure SetOnProgress(const Value: TASTProgressEvent);
    procedure SetOnConsoleWrite(const Value: TASTRrojectConsoleWriteEvent);
    procedure SetStopCompileIfError(const Value: Boolean);
    function GetOnProgress: TASTProgressEvent;
    function GetOnConsoleWrite: TASTRrojectConsoleWriteEvent;

    function GetName: string;
    function GetPointerSize: Integer;
    function GetNativeIntSize: Integer;
    function GetTotalLinesParsed: Integer;
    function GetTotalUnitsParsed: Integer;
    function GetTotalUnitsIntfOnlyParsed: Integer;
    function GetStopCompileIfError: Boolean;
    function InPogress: Boolean;

    property Name: string read GetName write SetName;
    property PointerSize: Integer read GetPointerSize;
    property NativeIntSize: Integer read GetNativeIntSize;
    property TotalLinesParsed: Integer read GetTotalLinesParsed;
    property TotalUnitsParsed: Integer read GetTotalUnitsParsed;
    property TotalUnitsIntfOnlyParsed: Integer read GetTotalUnitsIntfOnlyParsed;
    property StopCompileIfError: Boolean read GetStopCompileIfError write SetStopCompileIfError;
    property OnProgress: TASTProgressEvent read GetOnProgress write SetOnProgress;
    property OnConsoleWrite: TASTRrojectConsoleWriteEvent read GetOnConsoleWrite write SetOnConsoleWrite;

    procedure CosoleWrite(const Module: IASTModule; Line: Integer; const Message: string);

    procedure PutMessage(const AMessage: IASTParserMessage); overload;
    procedure PutMessage(const AModule: IASTModule; AMsgType: TCompilerMessageType; const AMessage: string;
                         const ATextPostition: TTextPosition); overload;

    function ToJson: TJsonASTDeclaration;
  end;

  IASTProjectSettings = interface
    ['{F0A54AD9-2588-4CC9-9B8E-0010BD9E06DC}']
  end;

  IASTDeclaration = interface
    ['{9405C64A-EF83-4EA4-AA27-1DCBCBA7DF11}']
    function GetID: TIdentifier;
    function GetName: string;
    function GetSrcPos: TTextPosition;
    function GetModule: IASTModule;
    function GetDisplayName: string;
    function Get_Obj: TObject;

    procedure SetID(const AValue: TIdentifier);
    procedure SetName(const AValue: string);
    procedure SetSrcPos(const AValue: TTextPosition);

    property ID: TIdentifier read GetID write SetID;
    property Name: string read GetName write SetName;
    property SrcPos: TTextPosition read GetSrcPos write SetSrcPos;
    property Module: IASTModule read GetModule;
    property DisplayName: string read GetDisplayName;
    property _Obj: TObject read Get_Obj;

    function Decl2Str(AFullName: Boolean = False): string; overload;
    procedure Decl2Str(ABuilder: TStringBuilder;
                       ANestedLevel: Integer = 0;
                       AAppendName: Boolean = True); overload;

    function ToJson: TJsonASTDeclaration;
  end;

implementation

end.

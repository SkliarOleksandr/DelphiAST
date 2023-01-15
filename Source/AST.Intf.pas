unit AST.Intf;

interface

uses AST.Parser.ProcessStatuses;

type

  IASTModule = interface
    ['{1E3A5748-1671-41E8-BAAD-4BBB2B363BF4}']
    function GetModuleName: string;
    function GetTotalLinesParsed: Integer;
    property Name: string read GetModuleName;
    property TotalLinesParsed: Integer read GetTotalLinesParsed;
  end;

  TASTProgressEvent = reference to procedure (const Module: IASTModule; Status: TASTProcessStatusClass);
  TASTRrojectConsoleWriteEvent = reference to procedure (const Module: IASTModule; Line: Integer; const Message: string);

  IASTProject = interface
    ['{AE77D75A-4F7F-445B-ADF9-47CF5C2F0A14}']
    procedure SetOnProgress(const Value: TASTProgressEvent);
    procedure SetOnConsoleWrite(const Value: TASTRrojectConsoleWriteEvent);
    function GetOnProgress: TASTProgressEvent;
    function GetOnConsoleWrite: TASTRrojectConsoleWriteEvent;
    property OnProgress: TASTProgressEvent read GetOnProgress write SetOnProgress;
    property OnConsoleWrite: TASTRrojectConsoleWriteEvent read GetOnConsoleWrite write SetOnConsoleWrite;

    function GetPointerSize: Integer;
    function GetNativeIntSize: Integer;
    function GetTotalLinesParsed: Integer;
    function GetTotalUnitsParsed: Integer;
    function GetTotalUnitsIntfOnlyParsed: Integer;

    property PointerSize: Integer read GetPointerSize;
    property NativeIntSize: Integer read GetNativeIntSize;
    property TotalLinesParsed: Integer read GetTotalLinesParsed;
    property TotalUnitsParsed: Integer read GetTotalUnitsParsed;
    property TotalUnitsIntfOnlyParsed: Integer read GetTotalUnitsIntfOnlyParsed;

    procedure CosoleWrite(const Module: IASTModule; Line: Integer; const Message: string);
  end;

  IASTProjectSettings = interface
    ['{F0A54AD9-2588-4CC9-9B8E-0010BD9E06DC}']
  end;

implementation

end.

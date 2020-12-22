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

  IASTProject = interface
    ['{AE77D75A-4F7F-445B-ADF9-47CF5C2F0A14}']
    procedure SetOnProgress(const Value: TASTProgressEvent);
    function GetOnProgress: TASTProgressEvent;
    property OnProgress: TASTProgressEvent read GetOnProgress write SetOnProgress;

    function GetPointerSize: Integer;
    function GetNativeIntSize: Integer;
    function GetTotalLinesParsed: Integer;
    property PointerSize: Integer read GetPointerSize;
    property NativeIntSize: Integer read GetNativeIntSize;
    property TotalLinesParsed: Integer read GetTotalLinesParsed;
  end;

  IASTProjectSettings = interface
    ['{F0A54AD9-2588-4CC9-9B8E-0010BD9E06DC}']
  end;

implementation

end.

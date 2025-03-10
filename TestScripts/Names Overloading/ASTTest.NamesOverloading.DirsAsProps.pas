unit ASTTest.NamesOverloading.DirsAsProps;

interface

type
  TMyClass = class
  private
    FData: string;
  public
    property on: string read FData;
    property protected: string read FData;
    property sealed: string read FData;
    property abstract: string read FData;
    property readonly: string read FData;
    property writeonly: string read FData;
    property dispid: string read FData;
    property default: string read FData;
    property stored: string read FData;
    property index: string read FData;
    property register: string read FData;
    property safecall: string read FData;
    property stdcall: string read FData;
    property cdecl: string read FData;
    property assembler: string read FData;
    property export: string read FData;
    property helper: string read FData;
    property forward: string read FData;
    property virtual: string read FData;
    property override: string read FData;
    property varargs: string read FData;
  end;

implementation

end.
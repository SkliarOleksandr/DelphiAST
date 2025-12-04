unit AST.JsonSchema;

interface

uses
  System.SysUtils, REST.Json.Types;

type

  TJsonBase = class
  public
    function ToJsonText: string;
  end;

  TASTHandle = NativeUInt;

  TJsonASTDeclaration = class(TJsonBase)
    kind: string;
    name: string;
    handle: TASTHandle;
    srcRow: Integer;
    srcCol: Integer;
    genericInstances: TArray<TJsonASTDeclaration>;
  end;

  TASTTypeDeclDetails = class(TJsonBase)

  end;

  TASTJsonDeclClass = class of TJsonASTDeclaration;

  TASTJsonType = class(TJsonASTDeclaration)
    typeKind: string;
    typeDecl: TASTTypeDeclDetails;
  end;

  TASTJsonStruct = class(TASTJsonType)
    ansestorName: string;
    ansestorHandle: TASTHandle;
    members: TArray<TJsonASTDeclaration>;
  end;

  TASTJsonVariable = class(TJsonASTDeclaration)
    dataTypeName: string;
    dataTypeHandle: Integer;
  end;

  TASTJsonParam = class(TASTJsonVariable)
    modifier: string;
  end;

  TASTJsonFunctionBody = class(TJsonBase)

  end;

  TASTJsonFunction = class(TJsonASTDeclaration)
    params: TArray<TASTJsonParam>;
    resultTypeName: string;
    resultTypeHandle: Integer;
    isVirtual: Boolean;
    isOverload: Boolean;
    prevOverload: TASTHandle;
    body: TASTJsonFunctionBody;
  end;

  TJsonASTProject = class(TJsonASTDeclaration)
    [JSONName('fileName')]
    fileName: string;
    modules: TArray<TJsonASTDeclaration>;
  end;

  TJsonASTUnit = class(TJsonASTDeclaration)
    [JSONName('fileName')]
    fileName: string;
    intfDecls: TArray<TJsonASTDeclaration>;
    implDecls: TArray<TJsonASTDeclaration>;
  end;

implementation

uses
  REST.Json;

{ TJsonBase }

function TJsonBase.ToJsonText: string;
begin
  Result := TJson.ObjectToJsonString(Self);
end;

end.

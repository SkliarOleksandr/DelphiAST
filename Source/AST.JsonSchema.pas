unit AST.JsonSchema;

interface

uses
  System.SysUtils, REST.Json.Types;

type

  TJsonBase = class

  end;

  TASTHandle = NativeUInt;

  TJsonASTDeclaration = class(TJsonBase)
    kind: string;
    name: string;
    handle: TASTHandle;
    srcRow: Integer;
    srcCol: Integer;
  end;

  TASTTypeDeclDetails = class(TJsonBase)

  end;

  TASTJsonDeclClass = class of TJsonASTDeclaration;

  TASTJsonType = class (TJsonASTDeclaration)
    typeKind: string;
    typeDecl: TASTTypeDeclDetails;
  end;

  TASTJsonVariable = class(TJsonASTDeclaration)
    dataTypeName: string;
    dataTypeHandle: Integer;
  end;

  TASTJsonParam = class(TASTJsonVariable)

  end;

  TASTJsonFunction = class(TJsonASTDeclaration)
    params: TArray<TASTJsonParam>;
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

end.

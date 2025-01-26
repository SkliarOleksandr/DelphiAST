unit AST.Delphi.JsonSchema;

interface

uses
  System.SysUtils, System.Classes,
  AST.JsonSchema;

type
  TASTJsonDelphiEnum = class(TASTJsonType)
  type
    TItem = class
      name: string;
      value: Integer;
      explicitValue: Boolean;
    end;
  var
    items: TArray<TItem>;
  end;

  TASTJsonDelphiSet = class(TASTJsonType)
    baseTypeSpec: string;
  end;

  TASTJsonDelphiAlias = class(TASTJsonType)
    sourceTypeSpec: string;
    newType: Boolean;
  end;

  TASTJsonDelphiRange = class(TASTJsonType)
    baseTypeName: string;
    lowValue: string;
    HighValue: string;
  end;

implementation

end.

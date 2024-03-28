unit AST.Delphi.Declarations;

interface

uses
  AST.Intf;

type

  TIDItemType = (
    itUnknown,         // неизвестный тип
    itVar,             // переменная
    itConst,           // константа
    itProcedure,       // процедуры/функция
    itSysOperator,     // system operator
    itMacroFunction,   // функция времени компиляции
    itProperty,        // свойство
    itAlias,           // алиас
    itType,            // тип
    itUnit,            // модуль
    itLabel            // label
  );

  IASTDelphiDeclaration = interface(IASTDeclaration)
    ['{07010A8E-2274-494F-8D70-435E882E55AC}']
    function Get_Unit: IASTModule;
    property _Unit: IASTModule read Get_Unit;
  end;

  IASTDelphiType = interface(IASTDelphiDeclaration)
    ['{0C08CB5D-3C9D-4637-8001-E952239CB0C7}']
  end;

  IDelphiBuiltInTypes = interface
  ['{5BDBE355-E105-49E7-A9D2-0683390F6ED8}']
    function Get_Int8: IASTDelphiType;
    function Get_Int16: IASTDelphiType;
    function Get_Int32: IASTDelphiType;
    function Get_Int64: IASTDelphiType;
    function Get_UInt8: IASTDelphiType;
    function Get_UInt16: IASTDelphiType;
    function Get_UInt32: IASTDelphiType;
    function Get_UInt64: IASTDelphiType;
    function Get_NativeInt: IASTDelphiType;
    function Get_NativeUInt: IASTDelphiType;
    // floating point types
    function Get_Float32: IASTDelphiType;
    function Get_Float64: IASTDelphiType;
    function Get_Float80: IASTDelphiType;
    function Get_Currency: IASTDelphiType;
    function Get_Comp: IASTDelphiType;
    function Get_Real: IASTDelphiType;
    // other
    function Get_Boolean: IASTDelphiType;
    function Get_AnsiChar: IASTDelphiType;
    function Get_WideChar: IASTDelphiType;
    function Get_AnsiString: IASTDelphiType;
    function Get_OpenString: IASTDelphiType;
    function Get_UnicodeString: IASTDelphiType;
    function Get_ShortString: IASTDelphiType;
    function Get_WideString: IASTDelphiType;
    function Get_Variant: IASTDelphiType;
    function Get_NullPtrType: IASTDelphiType;
    function Get_PointerType: IASTDelphiType;
    function Get_UntypedReference: IASTDelphiType;
    function Get_Untyped: IASTDelphiType;
    function Get_MetaType: IASTDelphiType;
    function Get_Void: IASTDelphiType;
    function Get_GuidType: IASTDelphiType;
    function Get_PAnsiChar: IASTDelphiType;
    function Get_PWideChar: IASTDelphiType;
    function Get_OrdinalType: IASTDelphiType;
    function Get_TObject: IASTDelphiType;
    function Get_Exception: IASTDelphiType;
    function Get_EAssertClass: IASTDelphiType;
    function Get_TTypeKind: IASTDelphiType;
    function Get_DateTimeType: IASTDelphiType;
    function Get_DateType: IASTDelphiType;
    function Get_TimeType: IASTDelphiType;
    function Get_EmptySetType: IASTDelphiType;
    function Get_ResStringRecord: IASTDelphiType;
    function Get_TVarRec: IASTDelphiType;

    property _Int8: IASTDelphiType read Get_Int8;
    property _Int16: IASTDelphiType read Get_Int16;
    property _Int32: IASTDelphiType read Get_Int32;
    property _Int64: IASTDelphiType read Get_Int64;
    property _UInt8: IASTDelphiType read Get_UInt8;
    property _UInt16: IASTDelphiType read Get_UInt16;
    property _UInt32: IASTDelphiType read Get_UInt32;
    property _UInt64: IASTDelphiType read Get_UInt64;
    property _NativeInt: IASTDelphiType read Get_NativeInt;
    property _NativeUInt: IASTDelphiType read Get_NativeUInt;
    // floating point types
    property _Float32: IASTDelphiType read Get_Float32;
    property _Float64: IASTDelphiType read Get_Float64;
    property _Float80: IASTDelphiType read Get_Float80;
    property _Currency: IASTDelphiType read Get_Currency;
    property _Comp: IASTDelphiType read Get_Comp;
    property _Real: IASTDelphiType read Get_Real;
    // other
    property _Boolean: IASTDelphiType read Get_Boolean;
    property _AnsiChar: IASTDelphiType read Get_AnsiChar;
    property _WideChar: IASTDelphiType read Get_WideChar;
    property _AnsiString: IASTDelphiType read Get_AnsiString;
    property _OpenString: IASTDelphiType read Get_OpenString;
    property _UnicodeString: IASTDelphiType read Get_UnicodeString;
    property _ShortString: IASTDelphiType read Get_ShortString;
    property _WideString: IASTDelphiType read Get_WideString;
    property _Variant: IASTDelphiType read Get_Variant;
    property _NullPtrType: IASTDelphiType read Get_NullPtrType;
    property _PointerType: IASTDelphiType read Get_PointerType;
    property _UntypedReference: IASTDelphiType read Get_UntypedReference;
    property _Untyped: IASTDelphiType read Get_Untyped;
    property _MetaType: IASTDelphiType read Get_MetaType;
    property _Void: IASTDelphiType read Get_Void;
    property _GuidType: IASTDelphiType read Get_GuidType;
    property _PAnsiChar: IASTDelphiType read Get_PAnsiChar;
    property _PWideChar: IASTDelphiType read Get_PWideChar;
    property _OrdinalType: IASTDelphiType read Get_OrdinalType;
    property _TObject: IASTDelphiType read Get_TObject;
    property _Exception: IASTDelphiType read Get_Exception;
    property _EAssertClass: IASTDelphiType read Get_EAssertClass;
    property _TTypeKind: IASTDelphiType read Get_TTypeKind;
    property _DateTimeType: IASTDelphiType read Get_DateTimeType;
    property _DateType: IASTDelphiType read Get_DateType;
    property _TimeType: IASTDelphiType read Get_TimeType;
    property _EmptySetType: IASTDelphiType read Get_EmptySetType;
    property _ResStringRecord: IASTDelphiType read Get_ResStringRecord;
    property _TVarRec: IASTDelphiType read Get_TVarRec;
  end;


implementation

end.

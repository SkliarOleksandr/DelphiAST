unit ASTTest.Implicit.Pointers2;

interface

{$HINTS OFF}

type
  IIntf = interface
  end;

  TObj = class
  end;

  TObjClass = class of TObj;

  TIntArray = array of Integer;

  PByte = ^Byte;

var
  Ptr: Pointer;
  BytePtr: PByte;

  DArr: TIntArray;
  Intf: IIntf;
  Obj: TObj;
  ObjClass: TObjClass;

implementation

procedure Main;
begin
  Ptr := BytePtr;
  Ptr := DArr;
  Ptr := Intf;
  Ptr := Obj;
  Ptr := ObjClass;
end;


end.
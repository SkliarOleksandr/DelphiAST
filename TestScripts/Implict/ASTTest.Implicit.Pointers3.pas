unit ASTTest.Implicit.Pointers3;

interface

{$HINTS OFF}

type
  IIntf = interface
  end;

  TObj = class
  end;

  TObjClass = class of TObj;

  TIntArray = array of Integer;

  PByte1 = ^Byte;
  PByte2 = ^Byte;

  TProc = procedure;
  PProc = ^TProc;

var
  Ptr: Pointer;

  BytePtr1: PByte1;
  BytePtr2: PByte2;
  DArr: TIntArray;
  Intf: IIntf;
  Obj: TObj;
  ObjClass: TObjClass;
  Proc: TProc;

implementation

procedure Main;
begin
  BytePtr1 := Ptr;
  // BytePtr1 := BytePtr2;  // E2010 Incompatible types
  // DArr := Ptr;           // E2010 Incompatible types
  // Intf := Ptr;           // E2010 Incompatible types
  Obj := Ptr;
  ObjClass := Ptr;
  Proc := Ptr;
end;


end.
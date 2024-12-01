unit ASTTest.Implicit.Pointers;

interface

implementation

type
  TRecord = record
    a, b: Integer;
  end;
  PRecord = ^TRecord;

  IIntf = interface

  end;
  PIntf = ^IIntf;

  TObj = class

  end;
  PObj = ^TObj;
  
  PInteger = ^Integer;
  
  {$POINTERMATH ON}
  PByte = ^Byte;
  {$POINTERMATH OFF}

procedure Test;
var
  LPtr: Pointer;
  LPByte: PByte;
  LPInt: PInteger;
  PRec: PRecord;
  LPAChr: PAnsiChar;
  LPWChr: PWideChar;
  LPIntf: PIntf;
  LPObj: PObj;
begin
  LPtr := nil;
  LPByte := LPtr;
  LPByte := LPtr;
  PRec := LPtr;
  LPAChr := LPtr;
  LPWChr := LPtr;
  LPInt := LPtr;
  LPObj := LPtr;
end;

end.

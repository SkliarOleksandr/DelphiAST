unit ASTTest.Implicit.Pointers1;

interface

{$HINTS OFF}

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
  
  PByte = ^Byte;

  TProc = procedure;
  PProc = ^TProc;

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
  LPProc: PProc;
begin
  LPtr := nil;
  LPByte := LPtr;
  LPByte := LPtr;
  PRec := LPtr;
  LPAChr := LPtr;
  LPWChr := LPtr;
  LPInt := LPtr;
  LPObj := LPtr;
  LPProc := LPtr;
end;

end.

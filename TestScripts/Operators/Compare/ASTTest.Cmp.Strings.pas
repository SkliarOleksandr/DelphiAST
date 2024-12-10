unit ASTTest.Cmp.Strings;

interface

{$HINTS OFF}

implementation

procedure CmpAnsiStr;
var
  LAStr1: AnsiString;
  LAStr2: AnsiString;
begin
  if LAStr1 = LAStr1 then;
  if LAStr2 <> LAStr2 then;
end;

procedure CmpUnicodeStr;
var
  LUStr1: string;
  LUStr2: string;
begin
  if LUStr1 = LUStr2 then;
  if LUStr1 <> LUStr2 then;
end;

procedure CmpWideStr;
var
  LWStr1: WideString;
  LWStr2: WideString;
begin
  if LWStr1 = LWStr2 then;
  if LWStr1 <> LWStr2 then;
end;

procedure CmpShortStr;
var
  LSStr1: ShortString;
  LSStr2: ShortString;  
  LSStr3: string[10];
  LSStr4: string[20];  
begin
  if LSStr1 = LSStr2 then;
  if LSStr1 <> LSStr2 then;
  
  if LSStr3 = LSStr4 then;
  if LSStr3 <> LSStr4 then;
  
  if LSStr1 = LSStr3 then;  
  if LSStr2 <> LSStr4 then;    
end;
  
end.
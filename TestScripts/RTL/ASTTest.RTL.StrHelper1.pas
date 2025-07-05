unit ASTTest.RTL.StrHelper1;

interface

{$HINTS OFF}

implementation

uses System.SysUtils;

procedure Main;
var
  Buffer: array [0..255] of Char;
  LBuffer: PChar;
begin
  LBuffer := @Buffer[0];
  var LLen := LBuffer - @Buffer[0];
  var LResult := string.Create(Buffer, 0, LLen);
end;

end.
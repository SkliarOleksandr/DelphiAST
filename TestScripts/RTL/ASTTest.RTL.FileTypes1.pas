unit ASTTest.RTL.FileTypes1;

interface

implementation

procedure Test1;
var
  F: File;
  S: string;
  Buffer: array[0..255] of Char;
begin
  AssignFile(F, 'testfile.dat');
  Rewrite(F, 1);
  S := 'Hello';
  BlockWrite(F, S[1], Length(S));

  CloseFile(F);

  AssignFile(F, 'testfile.dat');
  Reset(F, 1);
  FillChar(Buffer, SizeOf(Buffer), 0);
  BlockRead(F, Buffer, Length(S));
  CloseFile(F);
end;

procedure Test2;
var
  F: TextFile;
  S, ReadS: string;
begin
  AssignFile(F, 'test.txt');
  Rewrite(F);
  S := 'Hello';
  Writeln(F, S);
  CloseFile(F);

  AssignFile(F, 'test.txt');
  Reset(F);
  Readln(F, ReadS);
  CloseFile(F);
end;

end.

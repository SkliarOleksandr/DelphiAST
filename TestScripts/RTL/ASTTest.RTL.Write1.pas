unit ASTTest.RTL.Write1;

interface

implementation

procedure Main;
begin
  Write(1);
  Write('1');
  Write(1.2);
  Write(1, 2, 3);
    
  Writeln(1);
  Writeln('1');
  Writeln(1.2);
  Writeln(1, 2, 3);      
end;

end.
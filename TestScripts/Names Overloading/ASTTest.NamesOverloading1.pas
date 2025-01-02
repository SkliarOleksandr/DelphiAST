unit ASTTest.NamesOverloading1;

interface

{$WARNINGS OFF} {$HINTS OFF}

type
  TGeneric<T> = class
    procedure Test;
  end;

  TMyObj = class
  type
    TNested = class
      procedure Execute;
    end;
    class var FDefault: string;
    class property Default: string read FDefault;
    class property Default2: string read FDefault;
    procedure Test;
  end;

implementation

{ TGeneric<T> }

procedure TGeneric<T>.Test;
begin
  var X := Default(T); // removing this line fixes error below!
end;

{ TMyObj }

procedure TMyObj.TNested.Execute;
begin
  // unexpected, E2029 '(' expected but ';' found
  // var A := Default;

  // unexpected, bur works!
  var B := Default(Integer);

  // expected, works
  var C := Default2;
end;

procedure TMyObj.Test;
begin
  // expected, E2066 Missing operator or semicolon
  // var A := Default(Integer);

  // expected
  //var B := Default;
  var C := Default2;
end;

end.


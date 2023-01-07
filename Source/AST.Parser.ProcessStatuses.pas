unit AST.Parser.ProcessStatuses;

interface

type

  // Base AST process class. Should be expanded for derived parsing/compiling/build processes
  TASTProcessStatus = class
  public
    class function Name: string; virtual; abstract;
  end;

  TASTProcessStatusClass = class of TASTProcessStatus;

  TASTStatusParseBegin = class(TASTProcessStatus)
  public
    class function Name: string; override;
  end;

  TASTStatusParseIntfSucess = class(TASTProcessStatus)
  public
    class function Name: string; override;
  end;

  TASTStatusParseSuccess = class(TASTProcessStatus)
  public
    class function Name: string; override;
  end;

  TASTStatusParseFail = class(TASTProcessStatus)
  public
    class function Name: string; override;
  end;

implementation

{ TASTStatusParseBegin }

class function TASTStatusParseBegin.Name: string;
begin
  Result := 'parse begin';
end;

{ TASTStatusParseSuccess }

class function TASTStatusParseSuccess.Name: string;
begin
  Result := 'parse success';
end;

{ TASTStatusParseFail }

class function TASTStatusParseFail.Name: string;
begin
  Result := 'parse fail';
end;

{ TASTStatusParseIntfSucess }

class function TASTStatusParseIntfSucess.Name: string;
begin
  Result := 'parse intf success';
end;

end.

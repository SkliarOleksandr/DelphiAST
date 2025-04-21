unit ASTTest.Intf.MethodDelegation1;

interface

type
 
  IMyIntf = interface
    function MyFunc: Integer;
  end;  

  TMyObject = class(TInterfacedObject, IMyIntf)
  private   
    function GetInt: Integer; virtual; abstract;
    // delegation: 
    function IMyIntf.MyFunc = GetInt;    
  end;     

implementation

end.
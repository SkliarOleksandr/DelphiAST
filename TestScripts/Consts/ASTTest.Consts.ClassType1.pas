unit ASTTest.Consts.ClassType1;

interface
 
type
  TObject = class
  end;
   
  TClass = class of TObject;
  
const
   CClass1: TClass = TObject;

implementation

end.
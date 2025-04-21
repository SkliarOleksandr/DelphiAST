unit ASTTest.Intf.MethodDelegation2;

interface

type

  TMyObject = class(TObject, IUnknown)
  private
    { IUnknown }
    function IUnknown.QueryInterface = ObjQueryInterface;
    function IUnknown._AddRef = ObjAddRef;
    function IUnknown._Release = ObjRelease;

    function ObjAddRef: Integer; virtual; stdcall; abstract;
    function ObjQueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall; abstract;
    function ObjRelease: Integer; virtual; stdcall; abstract;
  end;

implementation


end.

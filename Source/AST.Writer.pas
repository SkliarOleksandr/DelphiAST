unit AST.Writer;

interface

uses AST.Classes;

type
  TASTWriter<TDoc, TNode> = record
  public
    type TWriteNodeProc = reference to procedure (const Node: TNode; const AST: TASTItem);
    type TGetNodeProc = reference to function (const Container: TDoc;
                                               const RootNode: TNode; const NodeText: string): TNode;
    class procedure Write(const Doc: TDoc;
                          const RootNode: TNode;
                          const Module: TASTModule;
                          const GetNodeProc: TGetNodeProc;
                          const WriteNodeProc: TWriteNodeProc); static;
  end;

implementation

{ TASTWriter<TDest> }

class procedure TASTWriter<TDoc, TNode>.Write(const Doc: TDoc;
                                              const RootNode: TNode;
                                              const Module: TASTModule;
                                              const GetNodeProc: TGetNodeProc;
                                              const WriteNodeProc: TWriteNodeProc);
var
  Node: TNode;
begin
  Node := GetNodeProc(Doc, RootNode, Module.Name);
end;

end.

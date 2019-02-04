unit AST.Writer;

interface

uses AST.Classes, AST.Delphi.Parser;

type
  TASTWriter<TDoc, TNode> = class
  private
  type
    TWriteNodeProc = reference to procedure (const Node: TNode; const AST: TASTItem);
    TGetNodeProc = reference to function (const Container: TDoc;
                                               const RootNode: TNode; const NodeText: string): TNode;
  var
    fVarsSectionName: string;
    fConstsSectionName: string;
    fTypesSectionName: string;
    fFuncsSectionName: string;
    fDoc: TDoc;
    fModule: TASTModule;
    fGetNodeProc: TGetNodeProc;
    fWriteNodeProc: TWriteNodeProc;
  protected
    procedure WriteVars(RootNode: TNode);
    procedure WriteConsts(RootNode: TNode);
    procedure WriteTypes(RootNode: TNode);
    procedure WriteFuncs(RootNode: TNode);
    procedure WriteBody(RootNode: TNode; Body: TASTBody);
    procedure WriteKW_If(RootNode: TNode; KW: TASTKWIF);
    procedure WriteKW_Loop(RootNode: TNode; KW: TASTKWLoop);
    procedure WriteKW_With(RootNode: TNode; KW: TASTKWWith);

  public
    constructor Create(const Doc: TDoc;
                       const Module: TASTModule;
                       const GetNodeProc: TGetNodeProc;
                       const WriteNodeProc: TWriteNodeProc);

    procedure Write(const RootNode: TNode);
  end;

implementation

{ TASTWriter<TDest> }

uses NPCompiler.Classes;

constructor TASTWriter<TDoc, TNode>.Create(const Doc: TDoc;
                       const Module: TASTModule;
                       const GetNodeProc: TGetNodeProc;
                       const WriteNodeProc: TWriteNodeProc);
begin
  fVarsSectionName := 'vars';
  fConstsSectionName := 'consts';
  fTypesSectionName := 'types';
  fFuncsSectionName := 'funcs';
  fDoc := Doc;
  fModule := Module;
  fGetNodeProc := GetNodeProc;
  fWriteNodeProc := WriteNodeProc;
end;

procedure TASTWriter<TDoc, TNode>.Write(const RootNode: TNode);
var
  Node: TNode;
begin
  Node := fGetNodeProc(fDoc, RootNode, fModule.Name);
  WriteVars(Node);
  WriteConsts(Node);
  WriteTypes(Node);
  WriteFuncs(Node);
end;

procedure TASTWriter<TDoc, TNode>.WriteConsts(RootNode: TNode);
begin
  RootNode := fGetNodeProc(fDoc, RootNode, fConstsSectionName);
end;

procedure TASTWriter<TDoc, TNode>.WriteBody(RootNode: TNode; Body: TASTBody);
var
  Item: TASTItem;
  CNode: TNode;
begin
  if not Assigned(Body) then
    Exit;
  Item := Body.FirstChild;
  while Assigned(Item) do
  begin
    CNode := fGetNodeProc(fDoc, RootNode, Item.DisplayName);

    if Item is TASTKWIF then
       WriteKW_If(CNode, TASTKWIF(Item));

    if Item is TASTKWWhile then
       WriteKW_Loop(CNode, TASTKWLoop(Item));

    if Item is TASTKWRepeat then
       WriteKW_Loop(CNode, TASTKWLoop(Item));

    if Item is TASTKWFor then
       WriteKW_Loop(CNode, TASTKWLoop(Item));

    if Item is TASTKWWith then
       WriteKW_With(CNode, TASTKWWith(Item));

    Item := Item.Next;
  end;
end;

procedure TASTWriter<TDoc, TNode>.WriteFuncs(RootNode: TNode);
var
  Func: TASTDelphiProc;
  CNode: TNode;
begin
  RootNode := fGetNodeProc(fDoc, RootNode, fFuncsSectionName);
  Func := fModule.GetFirstFunc() as TASTDelphiProc;
  while Assigned(Func) do
  begin
    CNode := fGetNodeProc(fDoc, RootNode, Func.Name);
    WriteBody(CNode, Func.Body);
    Func := Func.NextItem as TASTDelphiProc;
  end;
end;

procedure TASTWriter<TDoc, TNode>.WriteKW_If(RootNode: TNode; KW: TASTKWIF);
var
  CNode: TNode;
begin
  CNode := fGetNodeProc(fDoc, RootNode, 'then');
  WriteBody(CNode, KW.ThenBody);
  if Assigned(KW.ElseBody) then
  begin
    CNode := fGetNodeProc(fDoc, RootNode, 'else');
    WriteBody(CNode, KW.ElseBody);
  end;
end;

procedure TASTWriter<TDoc, TNode>.WriteKW_Loop(RootNode: TNode; KW: TASTKWLoop);
begin
  WriteBody(RootNode, KW.Body);
end;

procedure TASTWriter<TDoc, TNode>.WriteKW_With(RootNode: TNode; KW: TASTKWWith);
begin
  WriteBody(RootNode, KW.Body);
end;

procedure TASTWriter<TDoc, TNode>.WriteTypes(RootNode: TNode);
begin
  RootNode := fGetNodeProc(fDoc, RootNode, fTypesSectionName);
end;

procedure TASTWriter<TDoc, TNode>.WriteVars(RootNode: TNode);
begin
  RootNode := fGetNodeProc(fDoc, RootNode, fVarsSectionName);
end;

end.

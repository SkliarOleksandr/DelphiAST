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
    procedure WriteBody(RootNode: TNode; Body: TASTBlock);
    procedure WriteKW_If(RootNode: TNode; KW: TASTKWIF);
    procedure WriteKW_Loop(RootNode: TNode; KW: TASTKWLoop);
    procedure WriteKW_With(RootNode: TNode; KW: TASTKWWith);
    procedure WriteKW_Case(RootNode: TNode; KW: TASTKWCase);
    procedure WriteKW_DeclSections(RootNode: TNode; KW: TASTKWDeclSection);
    procedure WriteKW_TryBlock(RootNode: TNode; KW: TASTKWTryBlock);
  public
    constructor Create(const Doc: TDoc;
                       const Module: TASTModule;
                       const GetNodeProc: TGetNodeProc;
                       const WriteNodeProc: TWriteNodeProc);

    procedure Write(const RootNode: TNode);
  end;

implementation

{ TASTWriter<TDest> }

uses AST.Delphi.Classes;

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

procedure TASTWriter<TDoc, TNode>.WriteBody(RootNode: TNode; Body: TASTBlock);
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
       WriteKW_If(CNode, TASTKWIF(Item))
    else
    if Item is TASTKWLoop then
       WriteKW_Loop(CNode, TASTKWLoop(Item))
    else
    if Item is TASTKWWith then
       WriteKW_With(CNode, TASTKWWith(Item))
    else
    if Item is TASTKWCase then
       WriteKW_Case(CNode, TASTKWCase(Item))
    else
    if Item is TASTKWDeclSection then
       WriteKW_DeclSections(CNode, TASTKWDeclSection(Item));

    if Item is TASTKWTryBlock then
       WriteKW_TryBlock(CNode, TASTKWTryBlock(Item));

    Item := Item.Next;
  end;
end;

procedure TASTWriter<TDoc, TNode>.WriteFuncs(RootNode: TNode);
//var
//  Func: TASTDelphiProc;
//  CNode: TNode;
begin
// todo:
//  RootNode := fGetNodeProc(fDoc, RootNode, fFuncsSectionName);
//  Func := fModule.GetFirstFunc() as TASTDelphiProc;
//  while Assigned(Func) do
//  begin
//    CNode := fGetNodeProc(fDoc, RootNode, Func.Name);
//    WriteBody(CNode, Func.Body);
//    Func := Func.NextItem as TASTDelphiProc;
//  end;
end;

procedure TASTWriter<TDoc, TNode>.WriteKW_Case(RootNode: TNode; KW: TASTKWCase);
var
  CNode: TNode;
  Item: TASTExpBlockItem;
begin
  Item := KW.FirstItem;
  while Assigned(Item) do
  begin
    CNode := fGetNodeProc(fDoc, RootNode, Item.Expression.DisplayName + ':');
    WriteBody(CNode, Item.Body);
    Item := Item.Next as TASTExpBlockItem;
  end;
  if Assigned(KW.ElseBody) then
  begin
    CNode := fGetNodeProc(fDoc, RootNode, 'else');
    WriteBody(CNode, KW.ElseBody);
  end;
end;

procedure TASTWriter<TDoc, TNode>.WriteKW_DeclSections(RootNode: TNode; KW: TASTKWDeclSection);
begin
  for var Decl in KW.Decls do
    fGetNodeProc(fDoc, RootNode, Decl.DisplayName);
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

procedure TASTWriter<TDoc, TNode>.WriteKW_TryBlock(RootNode: TNode; KW: TASTKWTryBlock);
var
  CNode: TNode;
  Item: TASTExpBlockItem;
begin
  WriteBody(RootNode, KW.Body);
  Item := KW.FirstExceptBlock;
  if Assigned(Item) then
  begin
    CNode := fGetNodeProc(fDoc, RootNode, 'except');
    while Assigned(Item) do
    begin
      var OnNode: TNode := fGetNodeProc(fDoc, CNode, 'on ' + Item.DisplayName);
      WriteBody(OnNode, Item.Body);
      Item := Item.Next as TASTExpBlockItem;
    end;
  end;
  if Assigned(KW.FinallyBody) then
  begin
    CNode := fGetNodeProc(fDoc, RootNode, 'finally');
    WriteBody(CNode, KW.FinallyBody);
  end;
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

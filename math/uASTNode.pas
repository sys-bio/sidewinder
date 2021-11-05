unit uASTNode;

// Used for storing equations (rate law, etc

interface

type
 {
T_ASTNode = class
  private
  valueStr: string; // store numbers and species, params as string
  valueType: integer; // from astnodetype_t const list
  public
  constructor create();
  function getNumChildren(): integer;
  function getChild(index: integer): T_ASTNode;
  function getLeftChild(): T_ASTNode;
  function getRightChild(): T_AstNode;
  function getType(): integer; // the type of this ASTNode
  function getValue(): double;  // ? can double handle NaN ?
  procedure addChild( addNode: T_ASTNode );

end;
       }
implementation

end.

with Bullfrog.Access_Types.Advanced_Smart_Access;
with Bullfrog.Access_Types.Advanced_Smart_Access.Debug;
with Bullfrog.Access_Types.Advanced_Smart_Access_Traits;
with Bullfrog.Access_Types.Advanced_Smart_Access.Make;

package Bullfrog.Tests.Smart_Access_Node is

   type Node;
   type Node_Access is access Node;

   package Node_Traits is new Bullfrog.Access_Types.Advanced_Smart_Access_Traits
      (Element_Type => Node);

   package Node_Smart_Access is new Bullfrog.Access_Types.Advanced_Smart_Access
      (Traits           => Node_Traits,
       Atomic_Increment => True);

   package Node_Debug is new Node_Smart_Access.Debug;

   type Node is record
      Value  : Integer := 0;
      Height : Integer := 1;
      Parent : Node_Smart_Access.Weak_Access;
      Left   : Node_Smart_Access.Shared_Access;
      Right  : Node_Smart_Access.Shared_Access;
   end record;

   package Make_Node is new Node_Smart_Access.Make
      (Element_Type   => Node,
       Element_Access => Node_Access,
       Traits         => Node_Traits);

   type Tree is tagged record
      Root : Node_Smart_Access.Shared_Access;
   end record;

   procedure Insert(Object : in out Tree; Value : Integer);
   procedure Print(Object : in Tree);

end Bullfrog.Tests.Smart_Access_Node;

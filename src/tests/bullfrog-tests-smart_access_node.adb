with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with System.Address_Image;

package body Bullfrog.Tests.Smart_Access_Node is

   function To_String(Node : Node_Smart_Access.Shared_Access) return String is
      function Value_To_String
         (Node : Node_Smart_Access.Shared_Access)
          return String
      is
      begin
         if Node.Is_Null then
            return "-1";
         else
            return Integer'Image(Node.Reference.Value);
         end if;
      end Value_To_String;
   begin
      if Node.Is_Null then
         return "(-1, 0,-1,-1,-1)";
      end if;

      return
         "("
         & Integer'Image(Node.Reference.Value)
         & ","
         & Integer'Image(Node.Reference.Height)
         & ","
         & Value_To_String(Node_Smart_Access.Make.Shared_Access
                          (Node.Reference.Parent))
         & ","
         & Value_To_String(Node.Reference.Left)
         & ","
         & Value_To_String(Node.Reference.Right)
         & ")";

   end To_String;

   procedure Print(Node : Node_Smart_Access.Shared_Access) is
   begin
      if Node.Is_Null then
         Put_Line
            ("N=>"
             & To_String(Node)
             & ",P=>"
             & To_String(Node)
             & ",L=>"
             & To_String(Node)
             & ",R=>"
             & To_String(Node));
      else
         Put_Line
            ("N=>"
             & To_String(Node)
             & ",P=>"
             & To_String
                (Node_Smart_Access.Make.Shared_Access(Node.Reference.Parent))
             & ",L=>"
             & To_String(Node.Reference.Left)
             & ",R=>"
             & To_String(Node.Reference.Right));
      end if;
   end Print;


   procedure Finalize(Memory : in out Node_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Node,
          Name   => Node_Access);
   begin
      Put_Line
         ("Finalize: "
          & System.Address_Image(Memory.all'Address)
          & " => "
          & Integer'Image(Memory.Value)
          & ","
          & Integer'Image(Memory.Height));
      Free(Memory);
   end Finalize;

   function Make_Node(Value : Integer) return Node_Access is
      Temp : Node_Access := new Node'(Value => Value, others => <>);
   begin
      Put_Line
         ("Initialize: "
          & System.Address_Image(Temp.all'Address)
          & " => "
          & Integer'Image(Temp.Value));
      return Temp;
   end Make_Node;

   function Max_Height
      (Left,Right : Node_Smart_Access.Shared_Access)
       return Integer
   is
   begin
      if Left.Is_Null then
         if Right.Is_Null then
            return 0;
         else
            return Right.Reference.Height;
         end if;
      elsif Right.Is_Null then
         return Left.Reference.Height;
      elsif Left.Reference.Height > Right.Reference.Height then
         return Left.Reference.Height;
      else
         return Right.Reference.Height;
      end if;
   end Max_Height;

   function New_Max_Height
      (Left, Right : Node_Smart_Access.Shared_Access)
       return Integer
   is
   begin
      return Max_Height(Left,Right) + 1;
   end New_Max_Height;

   function Height_Difference
      (Left,Right  : Node_Smart_Access.Shared_Access)
       return Integer
   is
      Left_Height  : Integer
         := (if Left.Is_Null  then 0 else Left.Reference.Height);
      Right_Height : Integer
         := (if Right.Is_Null then 0 else Right.Reference.Height);
   begin
      return Left_Height - Right_Height;
   end Height_Difference;

   procedure Rotate_Right(Root : in out Node_Smart_Access.Shared_Access) is
      Left  : Node_Smart_Access.Shared_Access := Root.Reference.Left;
   begin

      -- Make new parent connections
      if Left.Reference.Right.Not_Null then
         Left.Reference.Right.Reference.Parent.Swap(Left.Reference.Parent);
         Left.Reference.Parent.Swap(Root.Reference.Parent);
      else
         Left.Reference.Parent.Swap(Root.Reference.Parent);
         Root.Reference.Parent := Node_Smart_Access.Make.Weak_Access(Left);
      end if;


      -- Break potential circular reference first
      Root.Reference.Left := Left.Reference.Right;

      -- Move the root
      Left.Reference.Right := Root;

      -- Adjust height for old root so later calculation on new root
      -- is correct
      Root.Reference.Height := Max_Height
         (Left  => Root.Reference.Left,
          Right => Root.Reference.Right) + 1;

      -- Make Left the new root.  This will ensure the correct
      -- connections because Root was passed in by refence.
      Root.Swap(Left);

   end Rotate_Right;

   procedure Rotate_Left(Root : in out Node_Smart_Access.Shared_Access) is
      Right  : Node_Smart_Access.Shared_Access := Root.Reference.Right;
   begin

      -- Make new parent connections
      if Right.Reference.Left.Not_Null then
         Right.Reference.Left.Reference.Parent.Swap(Right.Reference.Parent);
         Right.Reference.Parent.Swap(Root.Reference.Parent);
      else
         Right.Reference.Parent.Swap(Root.Reference.Parent);
         Root.Reference.Parent := Node_Smart_Access.Make.Weak_Access(Right);
      end if;

      -- Break potential circular reference first
      Root.Reference.Right := Right.Reference.Left;

      -- Move the root
      Right.Reference.Left := Root;

      -- Adjust height for old root so later calculation on new root
      -- is correct
      Root.Reference.Height := Max_Height
         (Left  => Root.Reference.Left,
          Right => Root.Reference.Right) + 1;

      -- Make Right the new root.  This will ensure the correct
      -- connections because Root was passed in by refence.
      Root.Swap(Right);

   end Rotate_Left;

   procedure Balance(Root : in out Node_Smart_Access.Shared_Access) is
      Balance_Factor : Integer := Height_Difference
         (Left  => Root.Reference.Left,
          Right => Root.Reference.Right);
   begin

      -- Need a balance factor of at least 2 (or -2) before a rotate
      -- is required
      if Balance_Factor > 1 then
         -- Left heavy

         -- Calculate child height to check for double rotate
         Balance_Factor := Height_Difference
            (Left  => Root.Reference.Left.Reference.Left,
             Right => Root.Reference.Left.Reference.Right);

         if Balance_Factor < 0 then
            -- Child is right heavy so a double rotate will be needed

            -- Make child left heavy
            Rotate_Left(Root.Reference.Left);

            -- Set new child height so later calcs are correct
            Root.Reference.Left.Reference.Height := Max_Height
               (Left  => Root.Reference.Left.Reference.Left,
                Right => Root.Reference.Left.Reference.Right) + 1;
         end if;

         -- Balance the node
         Rotate_Right(Root);

      elsif Balance_Factor < -1 then
         -- Right heavy

         -- Calculate child height to check for double rotate
         Balance_Factor := Height_Difference
            (Left  => Root.Reference.Right.Reference.Left,
             Right => Root.Reference.Right.Reference.Right);

         if Balance_Factor > 0 then
            -- Child is left heavy so a double rotate will be needed

            -- Make child right heavy
            Rotate_Right(Root.Reference.Right);

            -- Set new child height so later calcs are correct
            Root.Reference.Right.Reference.Height := Max_Height
               (Left  => Root.Reference.Right.Reference.Left,
                Right => Root.Reference.Right.Reference.Right) + 1;
         end if;

         -- Balance the node
         Rotate_Left(Root);

      end if;

   end Balance;

   procedure Insert_Impl
      (Root  : in out Node_Smart_Access.Shared_Access;
       Value : Integer)
   is
      -- Be careful using these after a balancing operation.
      -- They are frozen to the specific node, so when root
      -- updates, they will still link to the original values
      Left  : Node_Smart_Access.Shared_Access renames Root.Reference.Left;
      Right : Node_Smart_Access.Shared_Access renames Root.Reference.Right;
   begin


      if Value < Root.Reference.Value then
         if Left.Is_Null then
            Left := Node_Smart_Access.Make.Shared_Access(Make_Node(Value));
            Left.Reference.Parent := Node_Smart_Access.Make.Weak_Access(Root);
         else
            Insert_Impl(Left,Value);
            Balance(Root);
         end if;

         Root.Reference.Height := Max_Height
            (Left  => Root.Reference.Left,
             Right => Root.Reference.Right) + 1;


      elsif Value = Root.Reference.Value then
         Root.Reference.Value := Value;  -- Update if equal


      else
         if Right.Is_Null then
            Right := Node_Smart_Access.Make.Shared_Access(Make_Node(Value));
            Right.Reference.Parent := Node_Smart_Access.Make.Weak_Access(Root);
         else
            Insert_Impl(Right,Value);
            Balance(Root);
         end if;

         Root.Reference.Height := Max_Height
            (Left  => Root.Reference.Left,
             Right => Root.Reference.Right) + 1;
      end if;


   end Insert_Impl;

   procedure Insert(Object : in out Tree; Value : Integer) is
   begin
      if Object.Root.Not_Null then
         Insert_Impl(Object.Root,Value);
      else
         Object.Root := Node_Smart_Access.Make.Shared_Access
            (Make_Node(Value));
      end if;
   end Insert;

   procedure Search(Node : Node_Smart_Access.Shared_Access) is
   begin

      if Node.Not_Null then
         Search(Node.Reference.Left);
         Search(Node.Reference.Right);
         Put_Line(To_String(Node));
      end if;

   end Search;

   procedure Print(Object : in Tree) is
      Levels : Integer := (if Object.Root.Is_Null then 0 else Object.Root.Reference.Height);
   begin
      Search(Object.Root);
      New_Line;
   end Print;

end Bullfrog.Tests.Smart_Access_Node;

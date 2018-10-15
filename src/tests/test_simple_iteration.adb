with Bullfrog.Containers.Simple_Iterators;
with Bullfrog.Access_Types.References;
with Ada.Iterator_Interfaces;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Simple_Iteration is
   package Container_Type is

      type Instance is tagged limited private
         with
            Constant_Indexing => Constant_Reference,
            Iterator_Element  => Integer,
            Default_Iterator  => Iterate;

      function Capacity(Self : Instance) return Positive;
      procedure Append(Self : in out Instance; Value : Integer);

      type Cursor_Type is private;
      function Has_Element(Cursor : Cursor_Type) return Boolean;

      package References is new Bullfrog.Access_Types.References
         (Element_Type => Integer);

      function Constant_Reference
         (Container : aliased Instance;
          Position  : Cursor_Type)
          return References.Not_Null_Constant_Reference;

      package Iterator_Interfaces is new Ada.Iterator_Interfaces
         (Cursor       => Cursor_Type,
          Has_Element  => Has_Element);

      function Iterate
         (Container : Instance)
          return Iterator_Interfaces.Reversible_Iterator'Class;

   private

      type Cursor_Type is record
         Index : Natural;
      end record;

      type Integer_Array is array (1..5) of aliased Integer;

      type Instance is tagged limited record
         Data : Integer_Array := (others => 0);
         Size : Natural       := 0;
      end record;
      function First(Self : Instance) return Cursor_Type;
      function Next
         (Self     : Instance;
          Position : Cursor_Type)
          return Cursor_Type;
      function Last(Self : Instance) return Cursor_Type;
      function Previous
         (Self     : Instance;
          Position : Cursor_Type)
          return Cursor_Type;

      package Iterators is new Bullfrog.Containers.Simple_Iterators.Reversible
         (Container_Type => Instance,
          Cursor_Type    => Cursor_Type,
          Has_Element    => Has_Element,
          First          => First,
          Next           => Next,
          Previous       => Previous,
          Last           => Last,
          Iterator_Interfaces => Iterator_Interfaces);


   end Container_Type;

   package body Container_Type is

      function Capacity(Self : Instance) return Positive is
         (Self.Data'Length);

      procedure Append(Self : in out Instance; Value : Integer) is
      begin
         if Self.Size = Self.Data'Length then
            raise Constraint_Error;
         end if;
         Self.Size := Self.Size + 1;
         Self.Data(Self.Size) := Value;
      end Append;

      function Has_Element(Cursor : Cursor_Type) return Boolean
      is (Cursor.Index /= 0);

      function Constant_Reference
         (Container : aliased Instance;
          Position  : Cursor_Type)
          return References.Not_Null_Constant_Reference
      is begin
         return (Element => Container.Data(Position.Index)'Access);
      end Constant_Reference;

      function First(Self : Instance) return Cursor_Type is
      begin
         if Self.Size = 0 then
            return (Index => 0);
         else
            return (Index => 1);
         end if;
      end First;

      function Next
         (Self     : Instance;
          Position : Cursor_Type)
          return Cursor_Type
      is begin
         if Self.Size = 0 or Position.Index = 0 then
            return (Index => 0);
         elsif Position.Index = Self.Data'Last then
            return (Index => 0);
         else
            return (Index => Position.Index + 1);
         end if;
      end Next;

      function Last(Self : Instance) return Cursor_Type is
      begin
         if Self.Size = 0 then
            return (Index => 0);
         else
            return (Index => Self.Data'Last);
         end if;
      end Last;

      function Previous
         (Self     : Instance;
          Position : Cursor_Type)
          return Cursor_Type
      is begin
         if Self.Size = 0 or Position.Index = 0 then
            return (Index => 0);
         elsif Position.Index = 1 then
            return (Index => 0);
         else
            return (Index => Position.Index - 1);
         end if;
      end Previous;

      function Iterate
         (Container : Instance)
          return Iterator_Interfaces.Reversible_Iterator'Class
      is begin
         return Iterators.Reversible_Iterate(Container);
      end Iterate;

   end Container_Type;

   Container : Container_Type.Instance;
begin
   for Count in 1 .. Container.Capacity loop
      Container.Append(Count);
   end loop;

   Put_Line("Forward Iteration");
   for Element of Container loop
      Put_Line(Integer'Image(Element));
   end loop;

   Put_Line("Reverse Iteration");
   for Element of reverse Container loop
      Put_Line(Integer'Image(Element));
   end loop;

end Test_Simple_Iteration;

with Bullfrog.Containers.Predefined_Iterable_Wrappers;
with Bullfrog.Containers.Iterable_Wrappers;
with Bullfrog.Access_Types.References;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Multiway_Trees;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Bounded_Vectors;
with Ada.Containers.Bounded_Doubly_Linked_Lists;
with Ada.Containers.Bounded_Hashed_Maps;
with Ada.Containers.Bounded_Ordered_Maps;
with Ada.Containers.Bounded_Hashed_Sets;
with Ada.Containers.Bounded_Ordered_Sets;
with Ada.Containers.Bounded_Multiway_Trees;


procedure Test_Generic_Iteration is

   Test_Size : constant := 5;

   generic
      with package Forward
         is new Bullfrog.Containers.Iterable_Wrappers.Forward(<>);
      with function To_Integer
         (Source : Forward.Element_Type)
          return Integer is <>;
      with procedure Add_Integer
         (Container : in out Forward.Container_Type;
          Element   : in     Integer) is <>;
   package Test_Forward is
      procedure Run(Container : aliased in out Forward.Container_Type);
   end Test_Forward;
   package body Test_Forward is
      procedure Run(Container : aliased in out Forward.Container_Type) is
      begin
         for i in 1..Test_Size loop
            Add_Integer(Container,i);
         end loop;
         Put("Forward:   ");
         for E of Forward.Iterable(Container) loop
            Put(" " & Integer'Image(To_Integer(E)));
         end loop;
         New_Line;
      end Run;
   end Test_Forward;

   generic
      with package Reversible
         is new Bullfrog.Containers.Iterable_Wrappers.Reversible(<>);
      with function To_Integer
         (Source : Reversible.Element_Type)
          return Integer is <>;
      with procedure Add_Integer
         (Container : in out Reversible.Container_Type;
          Element   : in     Integer) is <>;
   package Test_Reversible is
      procedure Run(Container : aliased in out Reversible.Container_Type);
   end Test_Reversible;
   package body Test_Reversible is
      procedure Run(Container : aliased in out Reversible.Container_Type) is
      begin
         for i in 1..Test_Size loop
            Add_Integer(Container,i);
         end loop;
         Put("Reversible:");
         for E of Reversible.Iterable(Container) loop
            Put(" " & Integer'Image(To_Integer(E)));
         end loop;
         Put("  &");
         for E of reverse Reversible.Iterable(Container) loop
            Put(" " & Integer'Image(To_Integer(E)));
         end loop;
         New_Line;
      end Run;
   end Test_Reversible;


   package Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Integer);
   package Doubly_Linked_Lists is new Ada.Containers.Doubly_Linked_Lists
      (Element_Type => Integer);
   package Ordered_Maps is new Ada.Containers.Ordered_Maps
      (Key_Type        => Integer,
       Element_Type    => Integer);
   package Ordered_Sets is new Ada.Containers.Ordered_Sets
      (Element_Type => Integer);


   use Bullfrog.Containers;
   package Iterable_Vectors is
      new Predefined_Iterable_Wrappers.Vectors
         (Container_Pkg => Vectors);
   package Iterable_Doubly_Linked_Lists is
      new Predefined_Iterable_Wrappers.Doubly_Linked_Lists
         (Container_Pkg => Doubly_Linked_Lists);
   package Iterable_Ordered_Maps is
      new Predefined_Iterable_Wrappers.Ordered_Maps
         (Container_Pkg => Ordered_Maps);
   package Iterable_Ordered_Sets is
      new Predefined_Iterable_Wrappers.Ordered_Sets
         (Container_Pkg => Ordered_Sets);


   procedure Add_Integer
      (Container : in out Vectors.Vector;
       Element   :                Integer)
   is begin
      Container.Append(Element);
   end Add_Integer;
   procedure Add_Integer
      (Container : in out Doubly_Linked_Lists.List;
       Element   :                Integer)
   is begin
      Container.Append(Element);
   end Add_Integer;
   procedure Add_Integer
      (Container : in out Ordered_Maps.Map;
       Element   :                Integer)
   is begin
      Container.Insert(Element,Element);
   end Add_Integer;
   procedure Add_Integer
      (Container : in out Ordered_Sets.Set;
       Element   :                Integer)
   is begin
      Container.Insert(Element);
   end Add_Integer;

   function To_Integer(i : Integer) return Integer is
   begin
      return i;
   end To_Integer;

   package Forward_Vectors is new Test_Forward
      (Iterable_Vectors.Forward,
       To_Integer,
       Add_Integer);
   package Forward_Doubly_Linked_Lists is new Test_Forward
      (Iterable_Doubly_Linked_Lists.Forward,
       To_Integer,
       Add_Integer);
   package Forward_Ordered_Maps is new Test_Forward
      (Iterable_Ordered_Maps.Forward,
       To_Integer,
       Add_Integer);
   package Forward_Ordered_Sets is new Test_Forward
      (Iterable_Ordered_Sets.Forward,
       To_Integer,
       Add_Integer);

   package Reversible_Vectors is new Test_Reversible
      (Iterable_Vectors.Reversible,
       To_Integer,
       Add_Integer);
   package Reversible_Doubly_Linked_Lists is new Test_Reversible
      (Iterable_Doubly_Linked_Lists.Reversible,
       To_Integer,
       Add_Integer);
   package Reversible_Ordered_Maps is new Test_Reversible
      (Iterable_Ordered_Maps.Reversible,
       To_Integer,
       Add_Integer);
   package Reversible_Ordered_Sets is new Test_Reversible
      (Iterable_Ordered_Sets.Reversible,
       To_Integer,
       Add_Integer);

   type Composite is record
      Value : Integer := 0;
   end record;

   package C_Vectors is new Ada.Containers.Vectors(Natural,Composite);
   package Iter_C_Vectors is
      new Bullfrog.Containers.Predefined_Iterable_Wrappers.Vectors
         (Container_Pkg => C_Vectors);
   procedure Add_Integer
      (C : in out C_Vectors.Vector;
       I : Integer)
   is begin
      C.Append(New_Item => (Value => I));
   end Add_Integer;
   function To_Integer
      (E : Composite)
       return Integer
   is begin
      return E.Value;
   end To_Integer;

   package Test_FC is new Test_Forward
      (Iter_C_Vectors.Forward,
       To_Integer,
       Add_Integer);
   package Test_RC is new Test_Reversible
      (Iter_C_Vectors.Reversible,
       To_Integer,
       Add_Integer);

begin
   Put_Line("Starting...");
   declare
      Vector : Vectors.Vector;
      List   : Doubly_Linked_Lists.List;
      Map    : Ordered_Maps.Map;
      Set    : Ordered_Sets.Set;
   begin
      Put("Vectors - ");  Forward_Vectors.Run(Vector);
      Put("Lists   - ");  Forward_Doubly_Linked_Lists.Run(List);
      Put("Maps    - ");  Forward_Ordered_Maps.Run(Map);
      Put("Sets    - ");  Forward_Ordered_Sets.Run(Set);
   end;
   declare
      Vector : Vectors.Vector;
      List   : Doubly_Linked_Lists.List;
      Map    : Ordered_Maps.Map;
      Set    : Ordered_Sets.Set;
   begin
      Put("Vectors - ");  Reversible_Vectors.Run(Vector);
      Put("Lists   - ");  Reversible_Doubly_Linked_Lists.Run(List);
      Put("Maps    - ");  Reversible_Ordered_Maps.Run(Map);
      Put("Sets    - ");  Reversible_Ordered_Sets.Run(Set);
   end;
   declare
      Vector : C_Vectors.Vector;
   begin
      Put("Composite 1 - "); Test_FC.Run(Vector);
   end;
   declare
      Vector : C_Vectors.Vector;
   begin
      Put("Composite 1 - "); Test_RC.Run(Vector);
   end;

end Test_Generic_Iteration;

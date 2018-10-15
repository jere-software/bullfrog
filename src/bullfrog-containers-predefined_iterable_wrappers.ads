------------------------------------------------------------------------------
--                      Copyright (C) 2018 - Present                        --
--                            Jeremiah Breeden                              --
--                           All Rights Reserved                            --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file might be   --
--  covered by the GNU Public License.                                      --
------------------------------------------------------------------------------

with Bullfrog.Containers.Iterable_Wrappers;
with Bullfrog.Access_Types.References;
with Ada.Assertions;
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



-- This package provides predefined versions of the Iterable_Wrappers
-- package for each of the Ada standard containers.  These wrappers, by
-- nature of the container designs, are not as efficient as manually
-- iterating through them.  They are just provided as a convenience if
-- using the Iterable_Wrappers package as a generic formal.
package Bullfrog.Containers.Predefined_Iterable_Wrappers is

   -- Vectors implementation
   generic
      with package Container_Pkg is new Ada.Containers.Vectors(<>);
   package Vectors is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Vector;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Vector_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Reversible is
         new Containers.Iterable_Wrappers.Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Vectors;

   -- Doubly_Linked_Lists implementation
   generic
      with package Container_Pkg is new Ada.Containers.Doubly_Linked_Lists(<>);
   package Doubly_Linked_Lists is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.List;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.List_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Reversible is
         new Containers.Iterable_Wrappers.Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Doubly_Linked_Lists;

   -- Hashed_Maps implementation
   generic
      with package Container_Pkg is new Ada.Containers.Hashed_Maps(<>);
   package Hashed_Maps is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Map;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Map_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Hashed_Maps;

   -- Ordered_Maps implementation
   generic
      with package Container_Pkg is new Ada.Containers.Ordered_Maps(<>);
   package Ordered_Maps is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Map;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Map_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Reversible is
         new Containers.Iterable_Wrappers.Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Ordered_Maps;

   -- Hashed_Sets implementation
   generic
      with package Container_Pkg is new Ada.Containers.Hashed_Sets(<>);
   package Hashed_Sets is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Set;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Set_Iterator_Interfaces;

      -- Proxy functions needed
      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      -- The actual predefined packages
      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Hashed_Sets;

   -- Ordered_Sets implementation
   generic
      with package Container_Pkg is new Ada.Containers.Ordered_Sets(<>);
   package Ordered_Sets is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Set;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Set_Iterator_Interfaces;

      -- Proxy functions needed
      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Ordered_Sets;

   -- Multiway_Trees implementation
   generic
      with package Container_Pkg is new Ada.Containers.Multiway_Trees(<>);
   package Multiway_Trees is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Tree;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Tree_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Multiway_Trees;

   -- Indefinite_Vectors implementation
   generic
      with package Container_Pkg is new Ada.Containers.Indefinite_Vectors(<>);
   package Indefinite_Vectors is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Vector;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Vector_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Reversible is
         new Containers.Iterable_Wrappers.Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Indefinite_Vectors;

   -- Indefinite_Doubly_Linked_Lists implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Indefinite_Doubly_Linked_Lists(<>);
   package Indefinite_Doubly_Linked_Lists is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.List;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.List_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Reversible is
         new Containers.Iterable_Wrappers.Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Indefinite_Doubly_Linked_Lists;

   -- Indefinite_Hashed_Maps implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Indefinite_Hashed_Maps(<>);
   package Indefinite_Hashed_Maps is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Map;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Map_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Indefinite_Hashed_Maps;

   -- Ordered_Maps implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Indefinite_Ordered_Maps(<>);
   package Indefinite_Ordered_Maps is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Map;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Map_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Reversible is
         new Containers.Iterable_Wrappers.Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Indefinite_Ordered_Maps;

   -- Indefinite_Hashed_Sets implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Indefinite_Hashed_Sets(<>);
   package Indefinite_Hashed_Sets is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Set;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Set_Iterator_Interfaces;

      -- Proxy functions needed
      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      -- The actual predefined packages
      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Indefinite_Hashed_Sets;

   -- Indefinite_Ordered_Sets implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Indefinite_Ordered_Sets(<>);
   package Indefinite_Ordered_Sets is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Set;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Set_Iterator_Interfaces;

      -- Proxy functions needed
      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Indefinite_Ordered_Sets;

   -- Indefinite_Multiway_Trees implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Indefinite_Multiway_Trees(<>);
   package Indefinite_Multiway_Trees is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Tree;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Tree_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Indefinite_Multiway_Trees;

   -- Bounded_Vectors implementation
   generic
      with package Container_Pkg is new Ada.Containers.Bounded_Vectors(<>);
   package Bounded_Vectors is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Vector;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Vector_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Reversible is
         new Containers.Iterable_Wrappers.Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Bounded_Vectors;

   -- Bounded_Doubly_Linked_Lists implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Bounded_Doubly_Linked_Lists(<>);
   package Bounded_Doubly_Linked_Lists is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.List;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.List_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Reversible is
         new Containers.Iterable_Wrappers.Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Bounded_Doubly_Linked_Lists;

   -- Bounded_Hashed_Maps implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Bounded_Hashed_Maps(<>);
   package Bounded_Hashed_Maps is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Map;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Map_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Bounded_Hashed_Maps;

   -- Ordered_Maps implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Bounded_Ordered_Maps(<>);
   package Bounded_Ordered_Maps is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Map;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Map_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Reversible is
         new Containers.Iterable_Wrappers.Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Bounded_Ordered_Maps;

   -- Bounded_Hashed_Sets implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Bounded_Hashed_Sets(<>);
   package Bounded_Hashed_Sets is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Set;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Set_Iterator_Interfaces;

      -- Proxy functions needed
      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      -- The actual predefined packages
      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Bounded_Hashed_Sets;

   -- Bounded_Ordered_Sets implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Bounded_Ordered_Sets(<>);
   package Bounded_Ordered_Sets is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Set;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Set_Iterator_Interfaces;

      -- Proxy functions needed
      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
      is (Container.Iterate)
      with Inline;

      -- The actual predefined packages
      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Iterate);

      package Constant_Reversible is
         new Containers.Iterable_Wrappers.Constant_Reversible
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Bounded_Ordered_Sets;

   -- Bounded_Multiway_Trees implementation
   generic
      with package Container_Pkg
         is new Ada.Containers.Bounded_Multiway_Trees(<>);
   package Bounded_Multiway_Trees is

      -- type renamings for ease of reading
      subtype Container_Type is Container_Pkg.Tree;
      subtype Cursor_Type    is Container_Pkg.Cursor;

      -- Proxy packages needed
      package References is new Access_Types.References
         (Element_Type => Container_Pkg.Element_Type);
      package Iterators renames
         Container_Pkg.Tree_Iterator_Interfaces;

      -- Proxy functions needed
      function Reference
         (Container : aliased in out Container_Type;
          Cursor    :                Cursor_Type)
          return References.Not_Null_Reference
      is (Element =>
             Container_Pkg.Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    :         Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Element =>
             Container_Pkg.Constant_Reference
                (Container => Container,
                 Position  => Cursor).Element)
      with Inline;

      -- The actual predefined packages
      package Forward is
         new Containers.Iterable_Wrappers.Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Reference          => Reference,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

      package Constant_Forward is
         new Containers.Iterable_Wrappers.Constant_Forward
            (Container_Type     => Container_Type,
             Cursor_Type        => Cursor_Type,
             References         => References,
             Constant_Reference => Constant_Reference,
             Iterators          => Iterators,
             Iterate            => Container_Pkg.Iterate);

   end Bounded_Multiway_Trees;

end Bullfrog.Containers.Predefined_Iterable_Wrappers;

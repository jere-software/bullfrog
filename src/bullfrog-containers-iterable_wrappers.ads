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

with Bullfrog.Access_Types.References;
with Ada.Iterator_Interfaces;

-- This package provides a means to create formal iterable types for generic
-- programming.  A separate package, Predefined_Iterable_Wrappers, provides
-- package proxies to the standard ada containers.  See them for examples
-- of how to create proxy packages for other containers from 3rd party
-- libraries.
package Bullfrog.Containers.Iterable_Wrappers is

   -- This provides the formal type specification needed to do forward
   -- iteration of a container in a generic.  The cost of using this package
   -- includes:
   --    1. Use of 2 proxy packages to setup
   --    2. Possible extra assignments of access variables
   --    3. Dereferencing of a container access varable for each element
   --       usage.
   generic

      -- This is the container to be iterated over.
      type Container_Type(<>) is limited private;

      -- This is the needed Cursor type used to access elements in a
      -- container.
      type Cursor_Type(<>) is limited private;

      -- This provides formal references types, which is needed in
      -- a generic formal parameter.
      with package References is new Access_Types.References(<>);

      -- This opreration must be supplied to provide element access for
      -- iteration.  For containers that are already iterable, this is the
      -- operation defined for the attribute Variable_Indexing
      with function Reference
         (Container : aliased in out Container_Type;
          Cursor    : Cursor_Type)
          return References.Not_Null_Reference
         is <>;

      -- This operation must be supplied to provide constant element access for
      -- iteration.  For containers that are already iterable, this is the
      -- operation defined for the attribute Constant_Indexing
      with function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    : Cursor_Type)
          return References.Not_Null_Constant_Reference
         is <>;

      -- This provides formal iterator interface types, which is needed
      -- in a generic formal parameter.  It must use the same Cursor type
      -- as specified above.
      with package Iterators is new Ada.Iterator_Interfaces
         (Cursor => Cursor_Type,
          others => <>);

      -- This operation must be supplied to provide a default iterator
      -- from a container.  For containers that are already iterable,
      -- this is the operation defined for the attribute Default_Iterator
      with function Iterate
         (Container : Container_Type)
          return Iterators.Forward_Iterator'Class
         is <>;

   package Forward is

      -- This is the type of Element housed in the Container_Type
      subtype Element_Type is References.Item_Type;

      -- This type provides an iterable wrapper for a container in a
      -- generic.  It must be created using the function Iterable
      type Wrapper(<>) is tagged limited private
         with
            Default_Iterator  => Default_Iterator,
            Constant_Indexing => Constant_Indexing,
            Variable_Indexing => Variable_Indexing,
            Iterator_Element  => Element_Type;

      -- This function provides a means to get the starting iterator from
      -- the container.  It is assigned to the Default_Iterator attribute.
      function Default_Iterator
         (Container : Wrapper)
          return Iterators.Forward_Iterator'Class
         with Inline;

      -- This function provides a constant reference to a given element
      -- It is assigned to the Constant_Indexing attribute.
      function Constant_Indexing
         (Container : aliased Wrapper;
          Cursor    : Cursor_Type)
          return References.Not_Null_Constant_Reference
         with Inline;

      -- This function provides a reference to a given element
      -- It is assigned to the Variable_Indexing attribute.
      function Variable_Indexing
         (Container : aliased in out Wrapper;
          Cursor    : Cursor_Type)
          return References.Not_Null_Reference
         with Inline;

      -- This function is used to create an iterable wrapper for a container.
      -- Common Usage:
      --    for E of Iterable(Container_Object) loop
      --       E.Do_Things;
      --    end loop;
      function Iterable
         (Container : aliased in out Container_Type)
          return Wrapper
         with Inline;

   private

      type Wrapper
         (Container_Access : not null access Container_Type)
      is tagged limited null record;

      function Default_Iterator
         (Container : Wrapper)
          return Iterators.Forward_Iterator'Class
      is (Iterate(Container.Container_Access.all));

      function Constant_Indexing
         (Container : aliased Wrapper;
          Cursor    : Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Raw_Access =>
             Constant_Reference
                (Container => Container.Container_Access.all,
                 Cursor    => Cursor).Raw_Access);

      function Variable_Indexing
         (Container : aliased in out Wrapper;
          Cursor    : Cursor_Type)
          return References.Not_Null_Reference
      is (Raw_Access =>
             Constant_Reference
                (Container => Container.Container_Access.all,
                 Cursor    => Cursor).Raw_Access);

      function Iterable
         (Container : aliased in out Container_Type)
          return Wrapper
      is (Container_Access => Container'Access);

   end Forward;

   -- This provides the formal type specification needed to do reversible
   -- iteration of a container in a generic.  The cost of using this package
   -- includes:
   --    1. Use of 2 proxy packages to setup
   --    2. Possible extra assignments of access variables
   --    3. Dereferencing of a container access varable for each element
   --       usage.
   generic

      -- This is the container to be iterated over.
      type Container_Type(<>) is limited private;

      -- This is the needed Cursor type used to access elements in a
      -- container.
      type Cursor_Type(<>) is limited private;

      -- This provides formal references types, which is needed in
      -- a generic formal parameter.
      with package References is new Access_Types.References(<>);

      -- This opreration must be supplied to provide element access for
      -- iteration.  For containers that are already iterable, this is the
      -- operation defined for the attribute Variable_Indexing
      with function Reference
         (Container : aliased in out Container_Type;
          Cursor    : Cursor_Type)
          return References.Not_Null_Reference
         is <>;

      -- This operation must be supplied to provide constant element access for
      -- iteration.  For containers that are already iterable, this is the
      -- operation defined for the attribute Constant_Indexing
      with function Constant_Reference
         (Container : aliased Container_Type;
          Cursor    : Cursor_Type)
          return References.Not_Null_Constant_Reference
         is <>;

      -- This provides formal iterator interface types, which is needed
      -- in a generic formal parameter.  It must use the same Cursor type
      -- as specified above.
      with package Iterators is new Ada.Iterator_Interfaces
         (Cursor => Cursor_Type,
          others => <>);

      -- This operation must be supplied to provide a default iterator
      -- from a container.  For containers that are already iterable,
      -- this is the operation defined for the attribute Default_Iterator
      with function Iterate
         (Container : Container_Type)
          return Iterators.Reversible_Iterator'Class
         is <>;

   package Reversible is

      -- This is the type of Element housed in the Container_Type
      subtype Element_Type is References.Item_Type;

      -- This type provides an iterable wrapper for a container in a
      -- generic.  It must be created using the function Iterable
      type Wrapper(<>) is tagged limited private
         with
            Default_Iterator  => Default_Iterator,
            Constant_Indexing => Constant_Indexing,
            Variable_Indexing => Variable_Indexing,
            Iterator_Element  => Element_Type;

      -- This function provides a means to get the starting iterator from
      -- the container.  It is assigned to the Default_Iterator attribute.
      function Default_Iterator
         (Container : Wrapper)
          return Iterators.Reversible_Iterator'Class
         with Inline;

      -- This function provides a constant reference to a given element
      -- It is assigned to the Constant_Indexing attribute.
      function Constant_Indexing
         (Container : aliased Wrapper;
          Cursor    : Cursor_Type)
          return References.Not_Null_Constant_Reference
         with Inline;

      -- This function provides a reference to a given element
      -- It is assigned to the Variable_Indexing attribute.
      function Variable_Indexing
         (Container : aliased in out Wrapper;
          Cursor    : Cursor_Type)
          return References.Not_Null_Reference
         with Inline;

      -- This function is used to create an iterable wrapper for a container.
      -- Common Usage:
      --    for E of Iterable(Container_Object) loop
      --       E.Do_Things;
      --    end loop;
      function Iterable
         (Container : aliased in out Container_Type)
          return Wrapper
         with Inline;

   private

      type Wrapper
         (Container_Access : not null access Container_Type)
      is tagged limited null record;

      function Default_Iterator
         (Container : Wrapper)
          return Iterators.Reversible_Iterator'Class
      is (Iterate(Container.Container_Access.all));

      function Constant_Indexing
         (Container : aliased Wrapper;
          Cursor    : Cursor_Type)
          return References.Not_Null_Constant_Reference
      is (Raw_Access =>
             Constant_Reference
                (Container => Container.Container_Access.all,
                 Cursor    => Cursor).Raw_Access);

      function Variable_Indexing
         (Container : aliased in out Wrapper;
          Cursor    : Cursor_Type)
          return References.Not_Null_Reference
      is (Raw_Access =>
             Reference
                (Container => Container.Container_Access.all,
                 Cursor    => Cursor).Raw_Access);

      function Iterable
         (Container : aliased in out Container_Type)
          return Wrapper
      is (Container_Access => Container'Access);

   end Reversible;

end Bullfrog.Containers.Iterable_Wrappers;

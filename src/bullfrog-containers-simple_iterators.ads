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

with Ada.Iterator_Interfaces;

-- Provides a very basic contract for iteration.  It does not enforce
-- tampering checks or supply ranged iteration.
package Bullfrog.Containers.Simple_Iterators is

   pragma Pure;

   -- Contract for Forward Iteration only
   generic

      -- The container needing iteration
      type Container_Type(<>) is tagged limited private;

      -- The type providing element access.  It should provide
      -- default initialization where Has_Element returns False
      type Cursor_Type is private;

      -- Indicates if the Cursor has an element
      with function Has_Element(Cursor : Cursor_Type) return Boolean is <>;

      -- Returns the location of the first element in a container
      with function First
         (Container : Container_Type)
          return Cursor_Type is <>;

      -- Provides the location of the next element in a container
      with function Next
         (Container : Container_Type;
          Cursor    : Cursor_Type)
          return Cursor_Type is <>;

      -- Needs to be externally declared to make the Iterate functions
      -- usable
      with package Iterator_Interfaces is new Ada.Iterator_Interfaces
         (Cursor       => Cursor_Type,
          Has_Element  => Has_Element);

   package Forward is

      -- This function can be assigned to the Default_Iterator aspect
      function Forward_Iterate
         (Container : Container_Type)
          return Iterator_Interfaces.Forward_Iterator'Class;

   private

      -- Internal iterator type
      type Forward_Iterator
      is limited new Iterator_Interfaces.Forward_Iterator with record

         -- Access to the container
         Container : not null access constant Container_Type;

      end record;

      -- Returns the location of the first element in the container
      overriding function First
         (Iterator : Forward_Iterator)
          return Cursor_Type;

      -- Returns the location of the next element in the container
      overriding function Next
         (Iterator : Forward_Iterator;
          Cursor   : Cursor_Type)
          return Cursor_Type;

   end Forward;

   -- Contract for Forward and Reversible Iteration
   generic

      -- The container needing iteration
      type Container_Type(<>) is tagged limited private;

      -- The type providing element access.  It should provide
      -- default initialization where Has_Element returns False
      type Cursor_Type is private;

      -- Indicates if the Cursor has an element
      with function Has_Element(Cursor : Cursor_Type) return Boolean is <>;

      -- Returns the location of the first element in a container
      with function First
         (Container : Container_Type)
          return Cursor_Type is <>;

      -- Provides the location of the next element in a container
      with function Next
         (Container : Container_Type;
          Cursor    : Cursor_Type)
          return Cursor_Type is <>;

      -- Provides the location of the previous element in a container
      with function Previous
         (Container : Container_Type;
          Cursor    : Cursor_Type)
          return Cursor_Type is <>;

      -- Provides the location of the last element in a container
      with function Last
         (Container : Container_Type)
          return Cursor_Type is <>;

      -- Needs to be externally declared to make the Iterate functions
      -- usable
      with package Iterator_Interfaces is new Ada.Iterator_Interfaces
         (Cursor       => Cursor_Type,
          Has_Element  => Has_Element);

   package Reversible is

      -- This function can be assigned to the Default_Iterator aspect.
      -- Allows for forward iterators
      function Forward_Iterate
         (Container : Container_Type)
          return Iterator_Interfaces.Forward_Iterator'Class;

      -- This function can be assigned to the Default_Iterator aspect.
      -- Allows for reversible iterators
      function Reversible_Iterate
         (Container : Container_Type)
          return Iterator_Interfaces.Reversible_Iterator'Class;

   private

      -- Internal iterator type
      type Reversible_Iterator
      is limited new Iterator_Interfaces.Reversible_Iterator with record

         -- Access to the container
         Container : not null access constant Container_Type;

      end record;

      -- Returns the location of the first element in the container
      overriding function First
         (Iterator : Reversible_Iterator)
          return Cursor_Type;

      -- Returns the location of the next element in the container
      overriding function Next
         (Iterator : Reversible_Iterator;
          Cursor   : Cursor_Type)
          return Cursor_Type;

      -- Returns the location of the last element in the container
      overriding function Last
         (Iterator : Reversible_Iterator)
          return Cursor_Type;

      -- Returns the location of the previous element in the container
      overriding function Previous
         (Iterator : Reversible_Iterator;
          Cursor   : Cursor_Type)
          return Cursor_Type;

   end Reversible;


end Bullfrog.Containers.Simple_Iterators;

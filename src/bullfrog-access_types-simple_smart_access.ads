------------------------------------------------------------------------------
--                      Copyright (C) 2016 - Present                        --
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

with Bullfrog.Access_Types.Smart_Access;
with Ada.Unchecked_Deallocation;

-- This package provides simple smart access types.  They are suitable for
-- most user defined types (in particular not for incomplete types).
generic

   -- The basic type held by a Smart_Access type
   type Item_Type(<>) is limited private;

   -- This specifies whether or not to use atomic increment (a count wrapped
   -- in a protected object).  For single task applications, this should
   -- be False.  When True, it does not guarantee task saftey on the
   -- Smart_Access types, but it does guarantee that seperate Smart_Access
   -- variables in separate tasks can safely manage the same resource.  If one
   -- needs multiple tasks to access the same Smart_Access variable, however,
   -- it will need to be wrapped in some sort of synchronization primitive.
   -- It also does not guarantee task safety on the resource itself.
   Atomic_Increment : Boolean := True;

package Bullfrog.Access_Types.Simple_Smart_Access is

   -----------------------------------------------------------------------------
   -- Package Types
   -----------------------------------------------------------------------------

   -- Core implementation.  Do not use directly
   type Item_Access is access Item_Type;
   procedure Do_Not_Use_Finalize is new Ada.Unchecked_Deallocation
      (Object => Item_Type,
       Name   => Item_Access);
   package Core is new Access_Types.Smart_Access
      (Item_Type        => Item_Type,
       Item_Access      => Item_Access,
       Finalize         => Do_Not_Use_Finalize,
       Atomic_Increment => Atomic_Increment);

   -- This type provides read only access to the resource
   subtype Constant_Item_Access is Core.Constant_Item_Access;

   -- Basic counting type for the underlying reference count
   subtype Basic_Count is Core.Basic_Count;

   -- This Smart_Access type provides reference counting semantics.  It can
   -- be copied and will automatically handle deallocating the held access
   -- variable.
   subtype Shared_Access is Core.Shared_Access;

   -- This Smart_Access type is the "goto" access manager for most situations.
   -- Only one Unique_Access variable can hold a reference at a time.
   -- Ownership can be transferred to other Unique_Access variables when
   -- desired.
   subtype Unique_Access is Core.Unique_Access;



   -----------------------------------------------------------------------------
   -- Shared_Access type
   -----------------------------------------------------------------------------

   -- Releases the Shared_Access object's control of the reference.  The
   -- Shared_Access object is set to null.  If it was the last Shared_Access
   -- object in control of the reference, the reference is finalized.
   procedure Set_Null
      (Self : in out Shared_Access)
      with
         Inline => True;

   -- Returns a dereferenced view of the Share_Access object's reference
   -- NOTE:  In the future (when GNAT is fixed) this will return a
   --        variable of type References.Reference:
   --           package References is new Access_Types.References(Item_Type);
   --           function Reference
   --              (Self : in Shared_Access)
   --               return References.Reference;
   --
   --        Then all calls of Object.Reference.all will need to be replaced
   --        with just Object.Reference
   function Reference
      (Self : in Shared_Access)
       return Item_Access
      with
         Inline => True;

   -- Returns a dereferenced read-only view of the Share_Access object's
   -- reference
   -- NOTE:  In the future (when GNAT is fixed) this will return a
   --        variable of type References.Constant_Reference:
   --           package References is new Access_Types.References(Item_Type);
   --           function Constant_Reference
   --              (Self : in Shared_Access)
   --               return References.Constant_Reference;
   --
   --        Then all calls of Object.Constant_Reference.all will need to be
   --        replaced with just Object.Constant_Reference
   function Constant_Reference
      (Self : in Shared_Access)
       return Constant_Item_Access
      with
         Inline => True;

   -- Compares two Shared_Access objects
   function "="
      (Left,Right : Shared_Access)
       return Boolean
      with
         Inline => True;

   -- Returns True if the object holds a null reference
   function Is_Null
      (Self : in Shared_Access)
       return Boolean
      with
         Inline => True;

   -- Returns True is the object does not hold a null reference
   function Not_Null
      (Self : in Shared_Access)
       return Boolean
      with
         Inline => True;

   -- Swaps two Shared_Access objects.  This is generally faster
   -- than simple assignement as it doesn't require any calls to
   -- Finalize or Adjust, and it doesn't require any protected
   -- operations.
   procedure Swap (Left, Right : in out Shared_Access) with Inline => True;

   -- Override for Ada.Finalization.Controlled
   procedure Adjust  (Self : in out Shared_Access) with Inline => True;
   procedure Finalize(Self : in out Shared_Access) with Inline => True;


   -----------------------------------------------------------------------------
   -- Unique_Access type
   -----------------------------------------------------------------------------

   -- Releases the Unique_Access object's control of the reference.  The
   -- reference is finalized.
   procedure Set_Null
      (Self : in out Unique_Access)
      with
         Inline => True;

   -- Returns a dereferenced view of the Unique_Access object's reference
   function Reference
      (Self : in Unique_Access)
       return Item_Access
      with
         Inline => True;

   -- Returns a dereferenced read-only view of the Unique_Access object's
   -- reference
   function Constant_Reference
      (Self : in Unique_Access)
       return Constant_Item_Access
      with
         Inline => True;

   -- Returns True if the object holds a null reference
   function Is_Null
      (Self : in Unique_Access)
       return Boolean
      with
         Inline => True;

   -- Returns True if the object does not hold a null reference
   function Not_Null
      (Self : in Unique_Access)
       return Boolean
      with
         Inline => True;

   -- Swaps two Unique_Access objects
   procedure Swap(Left, Right : in out Unique_Access) with Inline => True;

   -- Override for Ada.Finalization.Limited_Controlled
   procedure Finalize(Self : in out Unique_Access) with Inline => True;



   -- This package provides all the constructing operations for Smart_Acess
   -- types.
   package Make is

      -- Constructs a Shared_Access object
      procedure Shared_Access
         (Target : in out Simple_Smart_Access.Shared_Access;
          Source : in     not null Item_Access)
         with
            Inline => True;
      procedure Shared_Access
         (Target : in out Simple_Smart_Access.Shared_Access;
          Source : in     Simple_Smart_Access.Shared_Access)
         with
            Inline => True;
      procedure Shared_Access
         (Target : in out Simple_Smart_Access.Shared_Access;
          Source : in out Simple_Smart_Access.Unique_Access)
         with
            Inline => True;
      function Shared_Access
         (Source : in not null Item_Access)
          return Simple_Smart_Access.Shared_Access
         with
            Inline => True;
      function Shared_Access
         (Source : in out Simple_Smart_Access.Unique_Access)
          return Simple_Smart_Access.Shared_Access
         with
            Inline => True;

      -- Constructs a Unique_Access object
      procedure Unique_Access
         (Target : in out Simple_Smart_Access.Unique_Access;
          Source : in     not null Item_Access)
         with
            Inline => True;
      procedure Unique_Access
         (Target : in out Simple_Smart_Access.Unique_Access;
          Source : in out Simple_Smart_Access.Unique_Access)
         with
            Inline => True;
      function Unique_Access
         (Source : in not null Item_Access)
          return Simple_Smart_Access.Unique_Access
         with
            Inline => True;
      function Unique_Access
         (Source : in out Simple_Smart_Access.Unique_Access)
          return Simple_Smart_Access.Unique_Access
         with
            Inline => True;

   private

      -- Shared_Access operations
      procedure Shared_Access
         (Target : in out Simple_Smart_Access.Shared_Access;
          Source : in     not null Item_Access)
          renames Core.Make.Shared_Access;
      procedure Shared_Access
         (Target : in out Simple_Smart_Access.Shared_Access;
          Source : in     Simple_Smart_Access.Shared_Access)
          renames Core.Make.Shared_Access;
      procedure Shared_Access
         (Target : in out Simple_Smart_Access.Shared_Access;
          Source : in out Simple_Smart_Access.Unique_Access)
          renames Core.Make.Shared_Access;
      function Shared_Access
         (Source : in not null Item_Access)
          return Simple_Smart_Access.Shared_Access
          renames Core.Make.Shared_Access;
      function Shared_Access
         (Source : in out Simple_Smart_Access.Unique_Access)
          return Simple_Smart_Access.Shared_Access
          renames Core.Make.Shared_Access;

      -- Unique_Access operations
      procedure Unique_Access
         (Target : in out Simple_Smart_Access.Unique_Access;
          Source : in     not null Item_Access)
          renames Core.Make.Unique_Access;
      procedure Unique_Access
         (Target : in out Simple_Smart_Access.Unique_Access;
          Source : in out Simple_Smart_Access.Unique_Access)
          renames Core.Make.Unique_Access;
      function Unique_Access
         (Source : in not null Item_Access)
          return Simple_Smart_Access.Unique_Access
          renames Core.Make.Unique_Access;
      function Unique_Access
         (Source : in out Simple_Smart_Access.Unique_Access)
          return Simple_Smart_Access.Unique_Access
          renames Core.Make.Unique_Access;

   end Make;

   -- This package provides special purpose non-primitive operations for
   -- Smart_Access types.
   package Utilities is

      -- Returns the number of Shared_Access objects that manage this
      -- reference.
      function Use_Count
         (Self : in Shared_Access)
          return Basic_Count
         with
            Inline => True;

   private

      function Use_Count
         (Self : in Shared_Access)
          return Basic_Count
          renames Core.Utilities.Use_Count;

   end Utilities;

private

   -- Shared_Access operations
   procedure Set_Null
      (Self : in out Shared_Access)
       renames Core.Set_Null;
   function Reference
      (Self : in Shared_Access)
       return Item_Access
       renames Core.Reference;
   function Constant_Reference
      (Self : in Shared_Access)
       return Constant_Item_Access
       renames Core.Constant_Reference;
   function "="
      (Left,Right : Shared_Access)
       return Boolean
       renames Core."=";
   function Is_Null
      (Self : in Shared_Access)
       return Boolean
       renames Core.Is_Null;
   function Not_Null
      (Self : in Shared_Access)
       return Boolean
       renames Core.Not_Null;
   procedure Swap (Left, Right : in out Shared_Access) renames Core.Swap;
   procedure Adjust  (Self : in out Shared_Access) renames Core.Adjust;
   procedure Finalize(Self : in out Shared_Access) renames Core.Finalize;

   -- Unique_Access operations
   procedure Set_Null
      (Self : in out Unique_Access)
       renames Core.Set_Null;
   function Reference
      (Self : in Unique_Access)
       return Item_Access
       renames Core.Reference;
   function Constant_Reference
      (Self : in Unique_Access)
       return Constant_Item_Access
       renames Core.Constant_Reference;
   function Is_Null
      (Self : in Unique_Access)
       return Boolean
       renames Core.Is_Null;
   function Not_Null
      (Self : in Unique_Access)
       return Boolean
       renames Core.Not_Null;
   procedure Swap(Left, Right : in out Unique_Access) renames Core.Swap;
   procedure Finalize(Self : in out Unique_Access) renames Core.Finalize;

end Bullfrog.Access_Types.Simple_Smart_Access;

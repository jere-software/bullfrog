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


with Ada.Finalization;
with Bullfrog.Access_Types.Reference_Counts;

-- This package provides general smart access types.  They are suitable for
-- almost any ada type.  In addition, one can specify the named access type to
-- allow for custom storage pools if desired.  Finally, a finalization
-- procedure can be specified for the named access type.  This is typically an
-- Unchecked_Deallocation, but can be specified any way the user desires.
generic

   -- The basic type held by a Smart_Access type
   type Element_Type(<>);

   -- The desired named access type that pairs with Element_Type
   type Element_Access is access Element_Type;

   -- A finalization procedure to be called when all
   -- references to the Element_Access variable are gone.
   -- The client should never manually call this procedure.
   with procedure Finalize(Memory : in out Element_Access);

   -- This specifies whether or not to use atomic increment (a count wrapped
   -- in a protected object).  For single task applications, this should
   -- be False.  When True, it does not guarantee task saftey on the
   -- Smart_Access types, but it does guarantee that seperate Smart_Access
   -- variables in separate tasks can safely manage the same resource.  If one
   -- needs multiple tasks to access the same Smart_Access variable, however,
   -- it will need to be wrapped in some sort of synchronization primitive.
   -- It also does not guarantee task safety on the resource itself.
   Atomic_Increment : Boolean := False;

package Bullfrog.Access_Types.Smart_Access is

   pragma Pure;

   -----------------------------------------------------------------------------
   -- Package Types
   -----------------------------------------------------------------------------

   -- This type provides read only access to the resource
   type Constant_Element_Access is access constant Element_Type;
   for Constant_Element_Access'Storage_Pool use Element_Access'Storage_Pool;

   -- Basic counting type for the underlying reference count
   subtype Basic_Count is Bullfrog.Access_Types.Reference_Counts.Basic_Count;

   -- This Smart_Access type provides reference counting semantics.  It can
   -- be copied and will automatically handle finalizing the held resource.
   -- Do not create circular references with this type.  Instead use
   -- Weak_Access types to break any such connections.
   type Shared_Access is new Ada.Finalization.Controlled with private;

   -- This Smart_Access type provides indirect access to a resource. By
   -- itself, it cannot view or modify the resource, but must be promoted to
   -- a Shared_Access object (via Make.Shared_Access) in order to do so.
   -- When a Shared_Access object is not null and is used to create a
   -- Weak_Access object (via Make.Weak_Access), the newly created Weak_Access
   -- object is considered "assigned" to that Shared_Access object's resource.
   -- It remains "assigned" even if all of the Shared_Access objects managing
   -- the resource are finalized (which finalizes the resource as well).
   -- Objects of type Weak_Access are used to create handles or break circular
   -- references for Shared_Access objects.
   type Weak_Access is new Ada.Finalization.Controlled with private;

   -- This Smart_Access type is the "goto" access manager for most situations.
   -- Only one Unique_Access variable can hold a resource at a time.
   -- Ownership can be transferred to other Unique_Access variables when
   -- desired.
   type Unique_Access is new Ada.Finalization.Limited_Controlled with private;

   -- Provides a "by reference" type for the smart_access ojbect that supplies
   -- a mutable view of the primary object.
   type Reference_Holder
      (Element : not null access Element_Type)
   is limited null record
      with
         Implicit_Dereference => Element;

   -- Provides a "by reference" type for the smart_access ojbect that supplies
   -- a constant view of the primary object.
   type Constant_Reference_Holder
      (Element : not null access constant Element_Type)
   is limited null record
      with
         Implicit_Dereference => Element;


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

   -- Provides modifiable reference to the data.  Return value acts
   -- as if it is of Element_Type
   function Reference
      (Self : in Shared_Access)
       return Reference_Holder
      with
         Inline => True;

   -- Provides a read-only reference to the data.  Return value acts
   -- as if it is of constant Element_Type
   function Constant_Reference
      (Self : in Shared_Access)
       return Constant_Reference_Holder
      with
         Inline => True;

   -- Compares two Shared_Access objects
   overriding
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
   procedure Swap (Left, Right : in out Shared_Access);

   -- Moves one Shared_Access object into another, destroying the
   -- source object in the process.  This requires a call to
   -- Finalize but not to Adjust.
   procedure Move (Target, Source : in out Shared_Access);

   -- Override for Ada.Finalization.Controlled
   overriding procedure Adjust  (Self : in out Shared_Access);
   overriding procedure Finalize(Self : in out Shared_Access);


   -----------------------------------------------------------------------------
   -- Weak_Access type
   -----------------------------------------------------------------------------

   -- Releases the Weak_Access object's assignment to a Shared_Access resource.
   -- If it was the last Weak_Access object assigned to a Shared_Access
   -- object's resource and there are no Shared_Access objects currently
   -- managing the resource, the internal reference counts are finalized.
   procedure Remove_Assignment
      (Self : in out Weak_Access)
      with
         Inline => True;

   -- Compares two Weak_Access objects.  Returns True if they are assigned to
   -- the same Shared_Access object's resource or are both unassigned.
   overriding
   function "="
      (Left,Right : Weak_Access)
       return Boolean
      with
         Inline => True;

   -- Returns True if the object is assigned to a Shared_Access object's
   -- resource.
   function Is_Assigned
      (Self : in Weak_Access)
       return Boolean
      with
         Inline => True;

   -- Returns True if the object is not assigned to a Shared_Access object's
   -- resource.
   function Not_Assigned
      (Self : in Weak_Access)
       return Boolean
      with
         Inline => True;

   -- Swaps two Weak_Access objects.  This is generally faster
   -- than simple assignement as it doesn't require any calls to
   -- Finalize or Adjust, and it doesn't require any protected
   -- operations.
   procedure Swap(Left, Right : in out Weak_Access);

   -- Moves one Weak_Access object into another, destroying the
   -- source object in the process.  This requires a call to
   -- Finalize but not to Adjust.
   procedure Move (Target, Source : in out Weak_Access);

   -- Override for Ada.Finalization.Controlled
   overriding procedure Adjust  (Self : in out Weak_Access);
   overriding procedure Finalize(Self : in out Weak_Access);


   -----------------------------------------------------------------------------
   -- Unique_Access type
   -----------------------------------------------------------------------------

   -- Releases the Unique_Access object's control of the reference.  The
   -- reference is finalized.
   procedure Set_Null
      (Self : in out Unique_Access)
      with
         Inline => True;

   -- Provides modifiable reference to the data.  Return value acts
   -- as if it is of Element_Type
   function Reference
      (Self : in Unique_Access)
       return Reference_Holder
      with
         Inline => True;

   -- Provides modifiable reference to the data.  Return value acts
   -- as if it is of constant Element_Type
   function Constant_Reference
      (Self : in Unique_Access)
       return Constant_Reference_Holder
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
   procedure Swap(Left, Right : in out Unique_Access);

   -- Moves one Shared_Access object into another, destroying the
   -- source object in the process.
   procedure Move (Target, Source : in out Unique_Access);

   -- Override for Ada.Finalization.Limited_Controlled
   overriding procedure Finalize(Self : in out Unique_Access);



   -- This package provides all the constructing operations for Smart_Access
   -- types.
   package Make is

      -- Constructs a Shared_Access object
      procedure Shared_Access
         (Target : in out Smart_Access.Shared_Access;
          Source : in     not null Element_Access);
      procedure Shared_Access
         (Target : in out Smart_Access.Shared_Access;
          Source : in     Smart_Access.Shared_Access);
      procedure Shared_Access
         (Target : in out Smart_Access.Shared_Access;
          Source : in     Smart_Access.Weak_Access);
      procedure Shared_Access
         (Target : in out Smart_Access.Shared_Access;
          Source : in out Smart_Access.Unique_Access);
      function Shared_Access
         (Source : in not null Element_Access)
          return Smart_Access.Shared_Access;
      function Shared_Access
         (Source : in Smart_Access.Weak_Access)
          return Smart_Access.Shared_Access;
      function Shared_Access
         (Source : in out Smart_Access.Unique_Access)
          return Smart_Access.Shared_Access;

      -- Constructs a Weak_Access object
      procedure Weak_Access
         (Target : in out Smart_Access.Weak_Access;
          Source : in     Smart_Access.Weak_Access);
      procedure Weak_Access
         (Target : in out Smart_Access.Weak_Access;
          Source : in     Smart_Access.Shared_Access);
      function Weak_Access
         (Source : in Smart_Access.Shared_Access)
          return Smart_Access.Weak_Access;

      -- Constructs a Unique_Access object
      procedure Unique_Access
         (Target : in out Smart_Access.Unique_Access;
          Source : in     not null Element_Access);
      procedure Unique_Access
         (Target : in out Smart_Access.Unique_Access;
          Source : in out Smart_Access.Unique_Access);
      function Unique_Access
         (Source : in not null Element_Access)
          return Smart_Access.Unique_Access;
      function Unique_Access
         (Source : in out Smart_Access.Unique_Access)
          return Smart_Access.Unique_Access;

   end Make;

   -- This package provides special purpose non-primitive operations for
   -- Smart_Access types.
   package Utilities is

      -- Returns the number of Shared_Access objects that manage this
      -- reference.
      function Use_Count
         (Self : in Shared_Access)
          return Basic_Count;

      -- Returns the number of Weak_Access objects that manage this
      -- reference.  If any Shared_Access objects manage this resource,
      -- a value of 1 is added to the count.
      function Weak_Count
         (Self : in Shared_Access)
          return Basic_Count;

      -- Provides dangerous unprotected access to the item type.  This should
      -- only be used when developing wrappers for this type or when needing
      -- to pass a pointer to an existing library operation.
      function Raw_Access
         (Self : in Shared_Access)
          return Element_Access
         with
            Inline => True;

      -- Provides dangerous unprotected access to a constant view of the item
      -- type.  This should only be used when developing wrappers for this
      -- type or when needing to pass a pointer to an existing library
      -- operation.
      function Raw_Constant_Access
         (Self : in Shared_Access)
          return Constant_Element_Access
         with
            Inline => True;

      -- Returns the number of Shared_Access objects that manage this
      -- reference.
      function Use_Count
         (Self : in Weak_Access)
          return Basic_Count;

      -- Returns the number of Weak_Access objects that manage this
      -- reference.  If any Shared_Access objects manage this resource,
      -- a value of 1 is added to the count.
      function Weak_Count
         (Self : in Weak_Access)
          return Basic_Count;

      -- Returns True if the Weak_Access object is assigned to the
      -- Shared_Access object's resource.
      function Is_Assigned_To
         (Weak   : in Weak_Access;
          Shared : in Shared_Access)
          return Boolean
         with
            Inline => True;

      -- Returns True if the Weak_Access object is not assigned to the
      -- Shared_Access object's resource.
      function Not_Assigned_To
         (Weak   : in Weak_Access;
          Shared : in Shared_Access)
          return Boolean
         with
            Inline => True;

      -- Provides dangerous unprotected access to the item type.  This should
      -- only be used when developing wrappers for this type or when needing
      -- to pass a pointer to an existing library operation.
      function Raw_Access
         (Self : in Unique_Access)
          return Element_Access
         with
            Inline => True;

      -- Provides dangerous unprotected access to a constant view of the item
      -- type.  This should only be used when developing wrappers for this
      -- type or when needing to pass a pointer to an existing library
      -- operation.
      function Raw_Constant_Access
         (Self : in Unique_Access)
          return Constant_Element_Access
         with
            Inline => True;

   end Utilities;

private

   -- Container for the two reference counts
   type Counts is record
      Strong : Reference_Counts.Reference_Count(Atomic_Increment);
      Weak   : Reference_Counts.Reference_Count(Atomic_Increment);
   end record;

   -- Access type for the counts container
   type Counts_Access is access Counts;


   -----------------------------------------------------------------------------
   -- Shared_Access type
   -----------------------------------------------------------------------------

   type Shared_Access is new Ada.Finalization.Controlled with
      record
         Item_Reference   : Element_Access := null;
         Counts_Reference : Counts_Access  := null;
      end record;


   -----------------------------------------------------------------------------
   -- Weak_Access type
   -----------------------------------------------------------------------------

   type Weak_Access is new Ada.Finalization.Controlled with
      record
         Item_Reference   : Element_Access := null;
         Counts_Reference : Counts_Access  := null;
      end record;


   -----------------------------------------------------------------------------
   -- Unique_Access type
   -----------------------------------------------------------------------------

   type Unique_Access is new Ada.Finalization.Limited_Controlled with
      record
         Item_Reference : Element_Access := null;
      end record;

end Bullfrog.Access_Types.Smart_Access;

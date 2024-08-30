------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

with Ada.Finalization;
with Bullfrog.Access_Types.Reference_Counts;
with Bullfrog.Access_Types.Advanced_Smart_Access_Traits;

-- Example incomplete type declaration setup:
--
--    use Bullfrog.Access_Types;
--
--    type My_Element_Type;
--    type My_Element_Access is access My_Element_Type;
--
--    package Core_Traits is new Advanced_Smart_Access_Traits
--       (Element_Type => My_Element_Type);
--    package Core is new Advanced_Smart_Access
--       (Traits => Core_Traits);
--
--    type My_Element_Type is record
--       <Stuff>
--    end record;
--
--    package Core_Make is new Core.Make
--       (Element_Type   => My_Element_Type,
--        Element_Access => My_Element_Access,
--        Traits         => Core_Traits);

-- Example custom storage type declaration setup:
--
--    use Bullfrog.Access_Types;
--
--    Pool : My_Storage_Pool_Type;
--
--    type My_Element_Type is record
--       <Stuff>
--    end record;
--
--    type My_Element_Access is access My_Element_Type
--       with Storage_Pool => Pool;
--
--    package Core_Traits is new Advanced_Smart_Access_Traits
--       (Element_Type => My_Element_Type);
--    package Core is new Advanced_Smart_Access
--       (Traits => Core_Traits);
--    package Core_Make is new Core.Make
--       (Element_Type   => My_Element_Type,
--        Element_Access => My_Element_Access,
--        Traits         => Core_Traits);

-- This package provides general smart access types.  They are suitable for
-- almost any ada type.
generic

   -- This package contains the element type to make Smart_Access types for.
   with package Traits is new Advanced_Smart_Access_Traits(<>);

package Bullfrog.Access_Types.Advanced_Smart_Access is

   -----------------------------------------------------------------------------
   -- Package Types
   -----------------------------------------------------------------------------

   -- This type provides variable access to the resource
   type Element_Access is access Traits.Element_Type;

   -- This type provides read only access to the resource
   type Constant_Element_Access is access constant Traits.Element_Type;

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
   -- NOTE:  Do not create objects of this type explicitly.  This type is
   --        only meant to be used as a temporary return object for the
   --        Reference function.  Explicitly creating objects of this type
   --        can cause erroneous memory access.
   type Reference_Holder
      (Element : not null access Traits.Element_Type)
   is limited null record
      with
         Implicit_Dereference => Element;

   -- Provides a "by reference" type for the smart_access ojbect that supplies
   -- a constant view of the primary object.
   -- NOTE:  Do not create objects of this type explicitly.  This type is
   --        only meant to be used as a temporary return object for the
   --        Constant_Reference function.  Explicitly creating objects of this
   --        type can cause erroneous memory access.
   type Constant_Reference_Holder
      (Element : not null access constant Traits.Element_Type)
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
   -- NOTE:  Do not save the return value of this function to any objects.
   --        Doing so can cause erroneous memory access.  This function is
   --        meant to only return temporary reference objects.
   --        Intended usage is:
   --
   --        Shared_Access_Object.Reference.Element_Method_Or_Field
   function Reference
      (Self : in Shared_Access)
       return Reference_Holder
      with
         Inline => True;

   -- Provides a read-only reference to the data.  Return value acts
   -- as if it is of constant Element_Type
   -- NOTE:  Do not save the return value of this function to any objects.
   --        Doing so can cause erroneous memory access.  This function is
   --        meant to only return temporary constnat reference objects.
   --        Intended usage is:
   --
   --        Shared_Access_Object.Constant_Reference.Element_Method_Or_Field
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
      Strong : Reference_Counts.Reference_Count(Traits.Atomic_Increment);
      Weak   : Reference_Counts.Reference_Count(Traits.Atomic_Increment);
   end record;

   -- Access type for the counts container
   type Counts_Access is access Counts;

   -----------------------------------------------------------------------------
   -- Element_Type finalization
   -----------------------------------------------------------------------------

   -- Access to deallocation procedure for Element_Type objects
   type Deallocation is access procedure (Memory : in out Element_Access);

   -- Actaul deallocation procedure holder (assigned by Make package)
   Deallocate : Deallocation := null;


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

end Bullfrog.Access_Types.Advanced_Smart_Access;

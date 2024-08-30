------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

-- This package provides all the constructing operations for Smart_Access
-- types.  See the package Advanced_Smart_Access for ane sample on how to use 
-- this package.  It must be declared at the same accessibility level as
-- the parent instantiation of Advanced_Smart_Access
generic
   
   -- The complete type that corresponds to Element_Type used
   -- in the parent package.
   type Element_Type(<>) is limited private;
   
   -- This type provides variable access to the resource
   type Element_Access is access Element_Type;

   -- This package contains the element type to make Smart_Access types for.
   -- This is used to validate that the two Element_Types used here
   -- and in the parent package actually match.
   with package Traits is new Advanced_Smart_Access_Traits
      (Element_Type     => Element_Type,
       Atomic_Increment => <>);
   
   -- Provide a custom deallocation procedure.  Providing this will 
   -- make the packange not call Unchecked_Deallocation internally.  The
   -- supplied procedure here needs to handle deallocation if needed
   Custom_Deallocator : access procedure (Element : in out Element_Access) 
      := null;
   
package Bullfrog.Access_Types.Advanced_Smart_Access.Make is
   
   -- Constructs a Shared_Access object
   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in     not null Element_Access);
   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in     Advanced_Smart_Access.Shared_Access);
   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in     Advanced_Smart_Access.Weak_Access);
   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in out Advanced_Smart_Access.Unique_Access);
   function Shared_Access
      (Source : in not null Element_Access)
       return Advanced_Smart_Access.Shared_Access;
   function Shared_Access
      (Source : in Advanced_Smart_Access.Weak_Access)
       return Advanced_Smart_Access.Shared_Access;
   function Shared_Access
      (Source : in out Advanced_Smart_Access.Unique_Access)
       return Advanced_Smart_Access.Shared_Access;

   -- Constructs a Weak_Access object
   procedure Weak_Access
      (Target : in out Advanced_Smart_Access.Weak_Access;
       Source : in     Advanced_Smart_Access.Weak_Access);
   procedure Weak_Access
      (Target : in out Advanced_Smart_Access.Weak_Access;
       Source : in     Advanced_Smart_Access.Shared_Access);
   function Weak_Access
      (Source : in Advanced_Smart_Access.Shared_Access)
       return Advanced_Smart_Access.Weak_Access;

   -- Constructs a Unique_Access object
   procedure Unique_Access
      (Target : in out Advanced_Smart_Access.Unique_Access;
       Source : in     not null Element_Access);
   procedure Unique_Access
      (Target : in out Advanced_Smart_Access.Unique_Access;
       Source : in out Advanced_Smart_Access.Unique_Access);
   function Unique_Access
      (Source : in not null Element_Access)
          return Advanced_Smart_Access.Unique_Access;
   function Unique_Access
      (Source : in out Advanced_Smart_Access.Unique_Access)
       return Advanced_Smart_Access.Unique_Access;
   
private
   
   -- This procedure is used as a means to ensure this package isn't 
   -- instantiated at an accessibility level lower than the parent
   -- package
   procedure Accessibility_Check
      (Memory : in out Advanced_Smart_Access.Element_Access) 
   is null;
   
   -- Ada rules require that the Access to Accessibiity_Check only be used
   -- in the package spec of a generic package.  This variable is a 
   -- temporary holding spot for it.
   Deallocate_Check : constant Deallocation := Accessibility_Check'Access;

end Bullfrog.Access_Types.Advanced_Smart_Access.Make;

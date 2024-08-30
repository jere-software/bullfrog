------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

-- This package provides a set of traits needed to handle incomplete types
-- for Smart_Access based packages.  See the package Advanced_Smart_Access
-- for ane sample on how to use this package.
generic
   
   -- The basic type held by a Smart_Access type
   type Element_Type(<>);
   
   -- This specifies whether or not to use atomic increment (a count wrapped
   -- in a protected object).  For single task applications, this should
   -- be False.  When True, it does not guarantee task saftey on the
   -- Smart_Access types, but it does guarantee that seperate Smart_Access
   -- variables in separate tasks can safely manage the same resource.  If one
   -- needs multiple tasks to access the same Smart_Access variable, however,
   -- it will need to be wrapped in some sort of synchronization primitive.
   -- It also does not guarantee task safety on the resource itself.
   Atomic_Increment : Boolean := False;
   
package Bullfrog.Access_Types.Advanced_Smart_Access_Traits is

   -- Returns the ID for this package instantiation.  The ID is unique only
   -- while the package is in scope.
   function Get_Package_ID return Trait_Package_ID;
   
private
   
   -- Dummy variable type needed
   type Dummy is null record;
   
   -- Dummy variable
   Placeholder : constant Dummy := (null record);
   
   function Get_Package_ID return Trait_Package_ID is 
      (Trait_Package_ID
         (System.Storage_Elements.To_Integer (Placeholder'Address)));
   
end Bullfrog.Access_Types.Advanced_Smart_Access_Traits;

------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

with Bullfrog.Containers.Limited_Indefinite_Holders;

-- Provides a holder for limited definite types
generic

   -- Element type to hold
   type Element_Type is limited private;

   -- Indicates if reference types are checked or
   -- unchecked.  Default is checked.
   Unchecked_References : Boolean := False;

package Bullfrog.Containers.Limited_Definite_Holders is

   -- This is just an extension to the indefinite version
   -- that adds a couple of useability functions
   package Core is new Limited_Indefinite_Holders
      (Element_Type         => Element_Type, 
       Unchecked_References => Unchecked_References);

   -- Primary holder type
   type Holder is new Core.Holder with private;

   -- Equality
   function "="(L,R : Holder) return Boolean
      with Inline;

   -- Returns a holder containing a defaulted element
   function Default_Element return Holder
      with Inline;

   -- Replaces the current element with a defaulted one.
   procedure Default_Element(Self : in out Holder)
      with Inline;

private

   type Holder is new Core.Holder with null record;

end Bullfrog.Containers.Limited_Definite_Holders;
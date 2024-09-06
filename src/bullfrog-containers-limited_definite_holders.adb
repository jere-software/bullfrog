------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

package body Bullfrog.Containers.Limited_Definite_Holders is

   function Make return Element_Type
      with Inline;
   function Make return Element_Type is 
   begin
      return Result : Element_Type;
   end Make;

   use type Core.Holder;

   function "="(L,R : Holder) return Boolean
      is (Core.Holder(L) = Core.Holder(R));

   function Default_Element return Holder is 
      (To_Holder(Make'Access));

   procedure Default_Element(Self : in out Holder) is
   begin
      Self.Replace_Element(Make'Access);
   end Default_Element;

end Bullfrog.Containers.Limited_Definite_Holders;
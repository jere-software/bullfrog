------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

with System.Address_Image;
with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

package body Bullfrog.Access_Types.Advanced_Smart_Access.Debug is

   function To_Integer is new Ada.Unchecked_Conversion
      (Source => Element_Access,
       Target => Integer_Address);

    function To_Integer is new Ada.Unchecked_Conversion
      (Source => Counts_Access,
       Target => Integer_Address);

   function To_String(Object : Shared_Access) return String is
   begin
      return
         "("
         & System.Address_Image(Object'Address)
         & ","
         & Integer_Address'Image(To_Integer(Object.Item_Reference))
         & ","
         & Integer_Address'Image(To_Integer(Object.Counts_Reference))
         & ","
         & Reference_Counts.Basic_Count'Image(Utilities.Use_Count(Object))
         & ","
         & Reference_Counts.Basic_Count'Image(Utilities.Weak_Count(Object))
         & ")";
   end To_String;
   function To_String(Object : Weak_Access) return String is
   begin
      return
         "("
         & System.Address_Image(Object'Address)
         & ","
         & Integer_Address'Image(To_Integer(Object.Item_Reference))
         & ","
         & Integer_Address'Image(To_Integer(Object.Counts_Reference))
         & ","
         & Reference_Counts.Basic_Count'Image(Utilities.Use_Count(Object))
         & ","
         & Reference_Counts.Basic_Count'Image(Utilities.Weak_Count(Object))
         & ")";
   end To_String;
   function To_String(Object : Unique_Access) return String is
   begin
      return
         "("
         & System.Address_Image(Object'Address)
         & ","
         & Integer_Address'Image(To_Integer(Object.Item_Reference))
         & ")";
   end To_String;
end Bullfrog.Access_Types.Advanced_Smart_Access.Debug;

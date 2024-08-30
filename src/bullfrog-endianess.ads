------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

private with Bullfrog.Modular_To_Array_Conversions;

-- This package provides the ability to detect Endianess for your
-- system
package Bullfrog.Endianess is

   -- Defines different types of Byte Order supported
   type Byte_Order is (Little_Endian, Middle_Endian, Big_Endian);

   -- The current systems current byte order at time the program is run.
   -- This is elaborated at run time which makes this package ineligible to
   -- be pure.  If the system is not big or little Endian, it is assumed to
   -- to be middle endian.
   System_Byte_Order : constant Byte_Order;
   
private
   
   use Modular_To_Array_Conversions;
   use Modular_To_Array_Conversions.Native_Arrays;
   use all type Unsigned_8;

   -- Value used to determine Endianess
   Test_Value : constant := 16#01020304#;

   -- Determine the endianess of the system by compareing a
   -- 32 bit value to various expected array values
   System_Byte_Order : constant Byte_Order   
      := (if To_Unsigned_8x4(Test_Value) = (1,2,3,4) then
             Big_Endian
          elsif To_Unsigned_8x4(Test_Value) = (4,3,2,1) then
             Little_Endian
          else
             Middle_Endian);

end Bullfrog.Endianess;

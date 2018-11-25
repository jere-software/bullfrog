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

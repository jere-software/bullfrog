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

with Interfaces;

-- Provides utilities for converting modular types to
-- and from arrays with a specific Endianess.  Modular types
-- used are subtypes of those from the standard package Interfaces.
package Bullfrog.Modular_To_Array_Conversions with Pure is

   -- Type renamed from package Interfaces
   subtype Unsigned_8  is Interfaces.Unsigned_8;
   subtype Unsigned_16 is Interfaces.Unsigned_16;
   subtype Unsigned_32 is Interfaces.Unsigned_32;
   subtype Unsigned_64 is Interfaces.Unsigned_64;

   -- Array of 8 Unsigned_8 values
   type Unsigned_8x8 is array(1..8) of Unsigned_8 with Pack, Size => 64;

   -- Array of 4 Unsigned_8 values
   type Unsigned_8x4 is array(1..4) of Unsigned_8 with Pack, Size => 32;

   -- Array of 2 Unsigned_8 values
   type Unsigned_8x2 is array(1..2) of Unsigned_8 with Pack, Size => 16;

   -- Array of 4 Unsigned_16 values
   type Unsigned_16x4 is array(1..4) of Unsigned_16 with Pack, Size => 64;

   -- Array of 2 Unsigned_16 values
   type Unsigned_16x2 is array(1..2) of Unsigned_16 with Pack, Size => 32;

   -- Array of 2 Unsigned_32 values
   type Unsigned_32x2 is array(1..2) of Unsigned_32 with Pack, Size => 64;

   -- Conversions to/from Native type from/to native array representations
   package Native_Arrays is

      -- Converts from a modular type to an array
      function To_Unsigned_8x8 (Source : Unsigned_64) return Unsigned_8x8
         with Inline;
      function To_Unsigned_8x4 (Source : Unsigned_32) return Unsigned_8x4
         with Inline;
      function To_Unsigned_8x2 (Source : Unsigned_16) return Unsigned_8x2
         with Inline;
      function To_Unsigned_16x4(Source : Unsigned_64) return Unsigned_16x4
         with Inline;
      function To_Unsigned_16x2(Source : Unsigned_32) return Unsigned_16x2
         with Inline;
      function To_Unsigned_32x2(Source : Unsigned_64) return Unsigned_32x2
         with Inline;

      -- Converts to a modular type from an array
      function To_Unsigned_64(Source : Unsigned_8x8)  return Unsigned_64
         with Inline;
      function To_Unsigned_64(Source : Unsigned_16x4) return Unsigned_64
         with Inline;
      function To_Unsigned_64(Source : Unsigned_32x2) return Unsigned_64
         with Inline;
      function To_Unsigned_32(Source : Unsigned_8x4)  return Unsigned_32
         with Inline;
      function To_Unsigned_32(Source : Unsigned_16x2) return Unsigned_32
         with Inline;
      function To_Unsigned_16(Source : Unsigned_8x2)  return Unsigned_16
         with Inline;

   end Native_Arrays;

   -- Conversions to/from Native type from/to array representations where
   -- bytes are in reverse order.  Useful for converting Little_Endian to
   -- Big_Endian and vice versa.
   package Byte_Swapped_Arrays is

      -- Converts from a modular type to an array
      function To_Unsigned_8x8 (Source : Unsigned_64) return Unsigned_8x8
         with Inline;
      function To_Unsigned_8x4 (Source : Unsigned_32) return Unsigned_8x4
         with Inline;
      function To_Unsigned_8x2 (Source : Unsigned_16) return Unsigned_8x2
         with Inline;
      function To_Unsigned_16x4(Source : Unsigned_64) return Unsigned_16x4
         with Inline;
      function To_Unsigned_16x2(Source : Unsigned_32) return Unsigned_16x2
         with Inline;
      function To_Unsigned_32x2(Source : Unsigned_64) return Unsigned_32x2
         with Inline;

      -- Converts to a modular type from an array
      function To_Unsigned_64(Source : Unsigned_8x8)  return Unsigned_64
         with Inline;
      function To_Unsigned_64(Source : Unsigned_16x4) return Unsigned_64
         with Inline;
      function To_Unsigned_64(Source : Unsigned_32x2) return Unsigned_64
         with Inline;
      function To_Unsigned_32(Source : Unsigned_8x4)  return Unsigned_32
         with Inline;
      function To_Unsigned_32(Source : Unsigned_16x2) return Unsigned_32
         with Inline;
      function To_Unsigned_16(Source : Unsigned_8x2)  return Unsigned_16
         with Inline;

   end Byte_Swapped_Arrays;

   -- Conversions to/from Native type from/to Big_Endian array representations
   package Big_Endian_Arrays is

      -- Converts from a modular type to an array
      function To_Unsigned_8x8 (Source : Unsigned_64) return Unsigned_8x8
         with Inline;
      function To_Unsigned_8x4 (Source : Unsigned_32) return Unsigned_8x4
         with Inline;
      function To_Unsigned_8x2 (Source : Unsigned_16) return Unsigned_8x2
         with Inline;
      function To_Unsigned_16x4(Source : Unsigned_64) return Unsigned_16x4
         with Inline;
      function To_Unsigned_16x2(Source : Unsigned_32) return Unsigned_16x2
         with Inline;
      function To_Unsigned_32x2(Source : Unsigned_64) return Unsigned_32x2
         with Inline;

      -- Converts to a modular type from an array
      function To_Unsigned_64(Source : Unsigned_8x8)  return Unsigned_64
         with Inline;
      function To_Unsigned_64(Source : Unsigned_16x4) return Unsigned_64
         with Inline;
      function To_Unsigned_64(Source : Unsigned_32x2) return Unsigned_64
         with Inline;
      function To_Unsigned_32(Source : Unsigned_8x4)  return Unsigned_32
         with Inline;
      function To_Unsigned_32(Source : Unsigned_16x2) return Unsigned_32
         with Inline;
      function To_Unsigned_16(Source : Unsigned_8x2)  return Unsigned_16
         with Inline;

   end Big_Endian_Arrays;

   -- Conversions to/from Native type from/to Little_Endian array
   -- representations
   package Little_Endian_Arrays is

      -- Converts from a modular type to an array
      function To_Unsigned_8x8 (Source : Unsigned_64) return Unsigned_8x8
         with Inline;
      function To_Unsigned_8x4 (Source : Unsigned_32) return Unsigned_8x4
         with Inline;
      function To_Unsigned_8x2 (Source : Unsigned_16) return Unsigned_8x2
         with Inline;
      function To_Unsigned_16x4(Source : Unsigned_64) return Unsigned_16x4
         with Inline;
      function To_Unsigned_16x2(Source : Unsigned_32) return Unsigned_16x2
         with Inline;
      function To_Unsigned_32x2(Source : Unsigned_64) return Unsigned_32x2
         with Inline;

      -- Converts to a modular type from an array
      function To_Unsigned_64(Source : Unsigned_8x8)  return Unsigned_64
         with Inline;
      function To_Unsigned_64(Source : Unsigned_16x4) return Unsigned_64
         with Inline;
      function To_Unsigned_64(Source : Unsigned_32x2) return Unsigned_64
         with Inline;
      function To_Unsigned_32(Source : Unsigned_8x4)  return Unsigned_32
         with Inline;
      function To_Unsigned_32(Source : Unsigned_16x2) return Unsigned_32
         with Inline;
      function To_Unsigned_16(Source : Unsigned_8x2)  return Unsigned_16
         with Inline;

   end Little_Endian_Arrays;

end Bullfrog.Modular_To_Array_Conversions;

------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
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

   -- Conversions to/from Native type from/to native array representations
   package Native_Arrays is

      -- Converts from a modular type to an array
      function To_Unsigned_8x8 (Source : Unsigned_64) return Unsigned_8x8
         with Inline;
      function To_Unsigned_8x4 (Source : Unsigned_32) return Unsigned_8x4
         with Inline;
      function To_Unsigned_8x2 (Source : Unsigned_16) return Unsigned_8x2
         with Inline;

      -- Converts to a modular type from an array
      function To_Unsigned_64(Source : Unsigned_8x8)  return Unsigned_64
         with Inline;
      function To_Unsigned_32(Source : Unsigned_8x4)  return Unsigned_32
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

      -- Converts to a modular type from an array
      function To_Unsigned_64(Source : Unsigned_8x8)  return Unsigned_64
         with Inline;
      function To_Unsigned_32(Source : Unsigned_8x4)  return Unsigned_32
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

      -- Converts to a modular type from an array
      function To_Unsigned_64(Source : Unsigned_8x8)  return Unsigned_64
         with Inline;
      function To_Unsigned_32(Source : Unsigned_8x4)  return Unsigned_32
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

      -- Converts to a modular type from an array
      function To_Unsigned_64(Source : Unsigned_8x8)  return Unsigned_64
         with Inline;
      function To_Unsigned_32(Source : Unsigned_8x4)  return Unsigned_32
         with Inline;
      function To_Unsigned_16(Source : Unsigned_8x2)  return Unsigned_16
         with Inline;

   end Little_Endian_Arrays;

end Bullfrog.Modular_To_Array_Conversions;

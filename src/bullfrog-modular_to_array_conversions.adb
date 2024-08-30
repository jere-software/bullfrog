------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body Bullfrog.Modular_To_Array_Conversions is

    -- Quick conversion from modular type to array
   function To_8x8 is new Ada.Unchecked_Conversion
      (Source => Unsigned_64,
       Target => Unsigned_8x8) with Inline;
   function To_8x4 is new Ada.Unchecked_Conversion
      (Source => Unsigned_32,
       Target => Unsigned_8x4) with Inline;
   function To_8x2 is new Ada.Unchecked_Conversion
      (Source => Unsigned_16,
       Target => Unsigned_8x2) with Inline;


   -- Quick conversion from array to modular type
   function To_64 is new Ada.Unchecked_Conversion
      (Source => Unsigned_8x8,
       Target => Unsigned_64) with Inline;
   function To_32 is new Ada.Unchecked_Conversion
      (Source => Unsigned_8x4,
       Target => Unsigned_32) with Inline;
   function To_16 is new Ada.Unchecked_Conversion
      (Source => Unsigned_8x2,
       Target => Unsigned_16) with Inline;

   -- Conversions to/from Native type from/to native array representations
   package body Native_Arrays is

      function To_Unsigned_8x8
         (Source : Unsigned_64)
          return Unsigned_8x8
      is (To_8x8(Source));

      function To_Unsigned_8x4
         (Source : Unsigned_32)
          return Unsigned_8x4
      is (To_8x4(Source));

      function To_Unsigned_8x2
         (Source : Unsigned_16)
          return Unsigned_8x2
      is (To_8x2(Source));

      function To_Unsigned_64
         (Source : Unsigned_8x8)
          return Unsigned_64
      is (To_64(Source));

      function To_Unsigned_32
         (Source : Unsigned_8x4)
          return Unsigned_32
      is (To_32(Source));

      function To_Unsigned_16
         (Source : Unsigned_8x2)
          return Unsigned_16
      is (To_16(Source));

   end Native_Arrays;

   package body Byte_Swapped_Arrays is

      function To_Unsigned_8x8
         (Source : Unsigned_64)
          return Unsigned_8x8
      is (1 => To_8x8(Source)(8),
          2 => To_8x8(Source)(7),
          3 => To_8x8(Source)(6),
          4 => To_8x8(Source)(5),
          5 => To_8x8(Source)(4),
          6 => To_8x8(Source)(3),
          7 => To_8x8(Source)(2),
          8 => To_8x8(Source)(1));

      function To_Unsigned_8x4
         (Source : Unsigned_32)
          return Unsigned_8x4
      is (1 => To_8x4(Source)(4),
          2 => To_8x4(Source)(3),
          3 => To_8x4(Source)(2),
          4 => To_8x4(Source)(1));

      function To_Unsigned_8x2
         (Source : Unsigned_16)
          return Unsigned_8x2
      is (1 => To_8x2(Source)(2),
          2 => To_8x2(Source)(1));

      function To_Unsigned_64
         (Source : Unsigned_8x8)
          return Unsigned_64
      is (To_64(Unsigned_8x8'
             (1 => Source(8),
              2 => Source(7),
              3 => Source(6),
              4 => Source(5),
              5 => Source(4),
              6 => Source(3),
              7 => Source(2),
              8 => Source(1))));

      function To_Unsigned_32
         (Source : Unsigned_8x4)
          return Unsigned_32
      is (To_32(Unsigned_8x4'
             (1 => Source(4),
              2 => Source(3),
              3 => Source(2),
              4 => Source(1))));

      function To_Unsigned_16
         (Source : Unsigned_8x2)
          return Unsigned_16
      is (To_16(Unsigned_8x2'
             (1 => Source(2),
              2 => Source(1))));

   end Byte_Swapped_Arrays;

   package body Big_Endian_Arrays is

      function To_Unsigned_8x8(Source : Unsigned_64) return Unsigned_8x8 is
         pragma Suppress(All_Checks);
         use all type Unsigned_64;
      begin
         return
            (1 => Unsigned_8(Shift_Right(Source,56) and 16#0FF#),
             2 => Unsigned_8(Shift_Right(Source,48) and 16#0FF#),
             3 => Unsigned_8(Shift_Right(Source,40) and 16#0FF#),
             4 => Unsigned_8(Shift_Right(Source,32) and 16#0FF#),
             5 => Unsigned_8(Shift_Right(Source,24) and 16#0FF#),
             6 => Unsigned_8(Shift_Right(Source,16) and 16#0FF#),
             7 => Unsigned_8(Shift_Right(Source, 8) and 16#0FF#),
             8 => Unsigned_8(Source                 and 16#0FF#));
      end To_Unsigned_8x8;

      function To_Unsigned_8x4(Source : Unsigned_32) return Unsigned_8x4 is
         pragma Suppress(All_Checks);
         use all type Unsigned_32;
      begin
         return
            (1 => Unsigned_8(Shift_Right(Source,24) and 16#0FF#),
             2 => Unsigned_8(Shift_Right(Source,16) and 16#0FF#),
             3 => Unsigned_8(Shift_Right(Source, 8) and 16#0FF#),
             4 => Unsigned_8(Source                 and 16#0FF#));
      end To_Unsigned_8x4;

      function To_Unsigned_8x2(Source : Unsigned_16) return Unsigned_8x2 is
         pragma Suppress(All_Checks);
         use all type Unsigned_16;
      begin
         return
            (1 => Unsigned_8(Shift_Right(Source, 8) and 16#0FF#),
             2 => Unsigned_8(Source                 and 16#0FF#));
      end To_Unsigned_8x2;

      function To_Unsigned_64(Source : Unsigned_8x8) return Unsigned_64 is
         Pragma Suppress(All_Checks);
         use all type Unsigned_64;
      begin
         return (Shift_Left(   Unsigned_64(Source(1)),56)
                 or Shift_Left(Unsigned_64(Source(2)),48)
                 or Shift_Left(Unsigned_64(Source(3)),40)
                 or Shift_Left(Unsigned_64(Source(4)),32)
                 or Shift_Left(Unsigned_64(Source(5)),24)
                 or Shift_Left(Unsigned_64(Source(6)),16)
                 or Shift_Left(Unsigned_64(Source(7)), 8)
                 or            Unsigned_64(Source(8)));
      end To_Unsigned_64;

      function To_Unsigned_32(Source : Unsigned_8x4) return Unsigned_32 is
         Pragma Suppress(All_Checks);
         use all type Unsigned_32;
      begin
         return (Shift_Left(   Unsigned_32(Source(1)),24)
                 or Shift_Left(Unsigned_32(Source(2)),16)
                 or Shift_Left(Unsigned_32(Source(3)),8)
                 or            Unsigned_32(Source(4)));
      end To_Unsigned_32;

      function To_Unsigned_16(Source : Unsigned_8x2) return Unsigned_16 is
         Pragma Suppress(All_Checks);
         use all type Unsigned_16;
      begin
         return (Shift_Left(   Unsigned_16(Source(1)),8)
                 or            Unsigned_16(Source(2)));
      end To_Unsigned_16;

--        function To_Unsigned_16(Source : Unsigned_8x2)  return Unsigned_16 is
--        begin
--           case System_Byte_Order is
--              when Little_Endian =>
--                 return Byte_Swapped_Arrays.To_Unsigned_16(Source);
--              when Middle_Endian =>
--                 return Portable_To_16(Source);
--              when Big_Endian =>
--                 return Native_Arrays.To_Unsigned_16(Source);
--           end case;
--        end To_Unsigned_16;

   end Big_Endian_Arrays;

   package body Little_Endian_Arrays is

      function To_Unsigned_8x8(Source : Unsigned_64) return Unsigned_8x8 is
         pragma Suppress(All_Checks);
         use all type Unsigned_64;
      begin
         return
            (8 => Unsigned_8(Shift_Right(Source,56) and 16#0FF#),
             7 => Unsigned_8(Shift_Right(Source,48) and 16#0FF#),
             6 => Unsigned_8(Shift_Right(Source,40) and 16#0FF#),
             5 => Unsigned_8(Shift_Right(Source,32) and 16#0FF#),
             4 => Unsigned_8(Shift_Right(Source,24) and 16#0FF#),
             3 => Unsigned_8(Shift_Right(Source,16) and 16#0FF#),
             2 => Unsigned_8(Shift_Right(Source, 8) and 16#0FF#),
             1 => Unsigned_8(Source                 and 16#0FF#));
      end To_Unsigned_8x8;

      function To_Unsigned_8x4(Source : Unsigned_32) return Unsigned_8x4 is
         pragma Suppress(All_Checks);
         use all type Unsigned_32;
      begin
         return
            (4 => Unsigned_8(Shift_Right(Source,24) and 16#0FF#),
             3 => Unsigned_8(Shift_Right(Source,16) and 16#0FF#),
             2 => Unsigned_8(Shift_Right(Source, 8) and 16#0FF#),
             1 => Unsigned_8(Source                 and 16#0FF#));
      end To_Unsigned_8x4;

      function To_Unsigned_8x2(Source : Unsigned_16) return Unsigned_8x2 is
         pragma Suppress(All_Checks);
         use all type Unsigned_16;
      begin
         return
            (2 => Unsigned_8(Shift_Right(Source, 8) and 16#0FF#),
             1 => Unsigned_8(Source                 and 16#0FF#));
      end To_Unsigned_8x2;

      function To_Unsigned_64(Source : Unsigned_8x8) return Unsigned_64 is
         Pragma Suppress(All_Checks);
         use all type Unsigned_64;
      begin
         return (Shift_Left(   Unsigned_64(Source(8)),56)
                 or Shift_Left(Unsigned_64(Source(7)),48)
                 or Shift_Left(Unsigned_64(Source(6)),40)
                 or Shift_Left(Unsigned_64(Source(5)),32)
                 or Shift_Left(Unsigned_64(Source(4)),24)
                 or Shift_Left(Unsigned_64(Source(3)),16)
                 or Shift_Left(Unsigned_64(Source(2)), 8)
                 or            Unsigned_64(Source(1)));
      end To_Unsigned_64;

      function To_Unsigned_32(Source : Unsigned_8x4) return Unsigned_32 is
         Pragma Suppress(All_Checks);
         use all type Unsigned_32;
      begin
         return (Shift_Left(   Unsigned_32(Source(4)),24)
                 or Shift_Left(Unsigned_32(Source(3)),16)
                 or Shift_Left(Unsigned_32(Source(2)),8)
                 or            Unsigned_32(Source(1)));
      end To_Unsigned_32;

      function To_Unsigned_16(Source : Unsigned_8x2) return Unsigned_16 is
         Pragma Suppress(All_Checks);
         use all type Unsigned_16;
      begin
         return (Shift_Left(   Unsigned_16(Source(2)),8)
                 or            Unsigned_16(Source(1)));
      end To_Unsigned_16;

   end Little_Endian_Arrays;

end Bullfrog.Modular_To_Array_Conversions;

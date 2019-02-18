with Bullfrog.Modular_To_Array_Conversions;
use Bullfrog.Modular_To_Array_Conversions;
with Ada.Text_IO; use Ada.Text_IO;
with Bullfrog.Endianess; use Bullfrog.Endianess;

-- This procedure tests both Bullfrog.Modular_to_Array_Conversions and
-- Bullfrog.Endianess
procedure Bullfrog.Tests.Modular_To_Array_Conversions is
   
   -- Modular Test Values
   Value_16 : Unsigned_16
      := 16#F1E2#;
   Value_32 : Unsigned_32
      := 16#F1E2D3C4#;
   Value_64 : Unsigned_64
      := 16#F1E2D3C4B5A69788#;
   
   -- Little Endian Byte Swapped Array Test Values
   LE_Swap_8x2  : Unsigned_8x2  := (16#F1#,16#E2#);
   LE_Swap_8x4  : Unsigned_8x4  := (16#F1#,16#E2#,16#D3#,16#C4#);
   LE_Swap_8x8  : Unsigned_8x8  := (16#F1#,16#E2#,16#D3#,16#C4#,
                                    16#B5#,16#A6#,16#97#,16#88#);
   LE_Swap_16x2 : Unsigned_16x2 := (16#E2F1#,16#C4D3#);
   LE_Swap_16x4 : Unsigned_16x4 := (16#E2F1#,16#C4D3#,16#A6B5#,16#8897#);
   LE_Swap_32x2 : Unsigned_32x2 := (16#C4D3E2F1#,16#8897A6B5#);
   
   -- Little Endian Native Array Test Values
   LE_Same_8x2  : Unsigned_8x2  := (16#E2#,16#F1#);
   LE_Same_8x4  : Unsigned_8x4  := (16#C4#,16#D3#,16#E2#,16#F1#);
   LE_Same_8x8  : Unsigned_8x8  := (16#88#,16#97#,16#A6#,16#B5#,
                                    16#C4#,16#D3#,16#E2#,16#F1#);
   LE_Same_16x2 : Unsigned_16x2 := (16#D3C4#,16#F1E2#);
   LE_Same_16x4 : Unsigned_16x4 := (16#9788#,16#B5A6#,16#D3C4#,16#F1E2#);
   LE_Same_32x2 : Unsigned_32x2 := (16#B5A69788#,16#F1E2D3C4#);
   
   -- Bit Endian Byte Swapped Array Test Values
   BE_Swap_8x2  : Unsigned_8x2  := (16#E2#,16#F1#);
   BE_Swap_8x4  : Unsigned_8x4  := (16#C4#,16#D3#,16#E2#,16#F1#);
   BE_Swap_8x8  : Unsigned_8x8  := (16#88#,16#97#,16#A6#,16#B5#,
                                    16#C4#,16#D3#,16#E2#,16#F1#);
   BE_Swap_16x2 : Unsigned_16x2 := (16#C4D3#,16#E2F1#);
   BE_Swap_16x4 : Unsigned_16x4 := (16#8897#,16#A6B5#,16#C4D3#,16#E2F1#);
   BE_Swap_32x2 : Unsigned_32x2 := (16#8897A6B5#,16#C4D3E2F1#);
   
   -- Big Endian Native Array Test Values
   BE_Same_8x2  : Unsigned_8x2  := (16#F1#,16#E2#);
   BE_Same_8x4  : Unsigned_8x4  := (16#F1#,16#E2#,16#D3#,16#C4#);
   BE_Same_8x8  : Unsigned_8x8  := (16#F1#,16#E2#,16#D3#,16#C4#,
                                    16#B5#,16#A6#,16#97#,16#88#);
   BE_Same_16x2 : Unsigned_16x2 := (16#F1E2#,16#D3C4#);
   BE_Same_16x4 : Unsigned_16x4 := (16#F1E2#,16#D3C4#,16#B5A6#,16#9788#);
   BE_Same_32x2 : Unsigned_32x2 := (16#F1E2D3C4#,16#B5A69788#);
   
   generic
      type Item_Type is limited private;
      with function "="(L,R : Item_Type) return Boolean is <>;
   procedure Test(L,R : Item_Type; Comment : String := "");
   
   procedure Test(L,R : Item_Type; Comment : String := "") is 
   begin
      if L = R then
         Put_Line("Passed: " & Comment);
      else
         Put_Line("***Failed: " & Comment);
      end if;
   end Test;
   
   use all type Unsigned_16;
   use all type Unsigned_32;
   use all type Unsigned_64;
   procedure Test_8x2  is new Test(Item_Type => Unsigned_8x2);
   procedure Test_8x4  is new Test(Item_Type => Unsigned_8x4);
   procedure Test_8x8  is new Test(Item_Type => Unsigned_8x8);
   procedure Test_16x2 is new Test(Item_Type => Unsigned_16x2);
   procedure Test_16x4 is new Test(Item_Type => Unsigned_16x4);
   procedure Test_32x2 is new Test(Item_Type => Unsigned_32x2);
   procedure Test_16 is new Test(Item_Type => Unsigned_16);
   procedure Test_32 is new Test(Item_Type => Unsigned_32);
   procedure Test_64 is new Test(Item_Type => Unsigned_64);
   
   
begin
   
   -- Print out system endianess
   Put_Line("Endianess for this machine is " 
            & Byte_Order'Image(System_Byte_Order));
   
   if System_Byte_Order = Little_Endian then
      
      Test_8x2
         (Native_Arrays.To_Unsigned_8x2(Value_16),
          LE_Same_8x2,
          "16 => 8x2  Native");
      Test_8x2
         (Byte_Swapped_Arrays.To_Unsigned_8x2(Value_16),
          LE_Swap_8x2,
          "16 => 8x2  Byte_Swapped");
      Test_8x4
         (Native_Arrays.To_Unsigned_8x4(Value_32),
          LE_Same_8x4,
          "32 => 8x4  Native");
      Test_8x4
         (Byte_Swapped_Arrays.To_Unsigned_8x4(Value_32),
          LE_Swap_8x4,
          "32 => 8x4  Byte_Swapped");
      Test_8x8
         (Native_Arrays.To_Unsigned_8x8(Value_64),
          LE_Same_8x8,
          "64 => 8x8  Native");
      Test_8x8
         (Byte_Swapped_Arrays.To_Unsigned_8x8(Value_64),
          LE_Swap_8x8,
          "64 => 8x8  Byte_Swapped");
      Test_16x2
         (Native_Arrays.To_Unsigned_16x2(Value_32),
          LE_Same_16x2,
          "32 => 16x2 Native");
      Test_16x2
         (Byte_Swapped_Arrays.To_Unsigned_16x2(Value_32),
          LE_Swap_16x2,
          "32 => 16x2 Byte_Swapped");
      Test_16x4
         (Native_Arrays.To_Unsigned_16x4(Value_64),
          LE_Same_16x4,
          "64 => 16x4 Native");
      Test_16x4
         (Byte_Swapped_Arrays.To_Unsigned_16x4(Value_64),
          LE_Swap_16x4,
          "64 => 16x4 Byte_Swapped");
      Test_32x2
         (Native_Arrays.To_Unsigned_32x2(Value_64),
          LE_Same_32x2,
          "64 => 32x2 Native");
      Test_32x2
         (Byte_Swapped_Arrays.To_Unsigned_32x2(Value_64),
          LE_Swap_32x2,
          "64 => 32x2 Byte_Swapped");
      
      Test_16
         (Native_Arrays.To_Unsigned_16(LE_Same_8x2),
          Value_16,
          "8x2  => 16 Native");
      Test_16
         (Byte_Swapped_Arrays.To_Unsigned_16(LE_Swap_8x2),
          Value_16,
          "8x2  => 16 Byte_Swapped");
      Test_32
         (Native_Arrays.To_Unsigned_32(LE_Same_8x4),
          Value_32,
          "8x4  => 32 Native");
      Test_32
         (Byte_Swapped_Arrays.To_Unsigned_32(LE_Swap_8x4),
          Value_32,
          "8x4  => 32 Byte_Swapped");
      Test_64
         (Native_Arrays.To_Unsigned_64(LE_Same_8x8),
          Value_64,
          "8x8  => 64 Native");
      Test_64
         (Byte_Swapped_Arrays.To_Unsigned_64(LE_Swap_8x8),
          Value_64,
          "8x8  => 64 Byte_Swapped");
      Test_32
         (Native_Arrays.To_Unsigned_32(LE_Same_16x2),
          Value_32,
          "16x2 => 32 Native");
      Test_32
         (Byte_Swapped_Arrays.To_Unsigned_32(LE_Swap_16x2),
          Value_32,
          "16x2 => 32 Byte_Swapped");
      Test_64
         (Native_Arrays.To_Unsigned_64(LE_Same_16x4),
          Value_64,
          "16x4 => 64 Native");
      Test_64
         (Byte_Swapped_Arrays.To_Unsigned_64(LE_Swap_16x4),
          Value_64,
          "16x4 => 64 Byte_Swapped");
      Test_64
         (Native_Arrays.To_Unsigned_64(LE_Same_32x2),
          Value_64,
          "32x2 => 64 Native");
      Test_64
         (Byte_Swapped_Arrays.To_Unsigned_64(LE_Swap_32x2),
          Value_64,
          "32x2 => 64 Byte_Swapped");
      
      Test_8x2
         (Big_Endian_Arrays.To_Unsigned_8x2(Value_16),
          LE_Swap_8x2,
          "16 => 8x2  Big_Endian");
      Test_8x2
         (Little_Endian_Arrays.To_Unsigned_8x2(Value_16),
          LE_Same_8x2,
          "16 => 8x2  Little_Endian");
      Test_8x4
         (Big_Endian_Arrays.To_Unsigned_8x4(Value_32),
          LE_Swap_8x4,
          "32 => 8x4  Big_Endian");
      Test_8x4
         (Little_Endian_Arrays.To_Unsigned_8x4(Value_32),
          LE_Same_8x4,
          "32 => 8x4  Little_Endian");
      Test_8x8
         (Big_Endian_Arrays.To_Unsigned_8x8(Value_64),
          LE_Swap_8x8,
          "64 => 8x8  Big_Endian");
      Test_8x8
         (Little_Endian_Arrays.To_Unsigned_8x8(Value_64),
          LE_Same_8x8,
          "64 => 8x8  Little_Endian");
      Test_16x2
         (Big_Endian_Arrays.To_Unsigned_16x2(Value_32),
          LE_Swap_16x2,
          "32 => 16x2 Big_Endian");
      Test_16x2
         (Little_Endian_Arrays.To_Unsigned_16x2(Value_32),
          LE_Same_16x2,
          "32 => 16x2 Little_Endian");
      Test_16x4
         (Big_Endian_Arrays.To_Unsigned_16x4(Value_64),
          LE_Swap_16x4,
          "64 => 16x4 Big_Endian");
      Test_16x4
         (Little_Endian_Arrays.To_Unsigned_16x4(Value_64),
          LE_Same_16x4,
          "64 => 16x4 Little_Endian");
      Test_32x2
         (Big_Endian_Arrays.To_Unsigned_32x2(Value_64),
          LE_Swap_32x2,
          "64 => 32x2 Big_Endian");
      Test_32x2
         (Little_Endian_Arrays.To_Unsigned_32x2(Value_64),
          LE_Same_32x2,
          "64 => 32x2 Little_Endian");
      
      Test_16
         (Big_Endian_Arrays.To_Unsigned_16(LE_Swap_8x2),
          Value_16,
          "8x2  => 16 Big_Endian");
      Test_16
         (Little_Endian_Arrays.To_Unsigned_16(LE_Same_8x2),
          Value_16,
          "8x2  => 16 Little_Endian");
      Test_32
         (Big_Endian_Arrays.To_Unsigned_32(LE_Swap_8x4),
          Value_32,
          "8x4  => 32 Big_Endian");
      Test_32
         (Little_Endian_Arrays.To_Unsigned_32(LE_Same_8x4),
          Value_32,
          "8x4  => 32 Little_Endian");
      Test_64
         (Big_Endian_Arrays.To_Unsigned_64(LE_Swap_8x8),
          Value_64,
          "8x8  => 64 Big_Endian");
      Test_64
         (Little_Endian_Arrays.To_Unsigned_64(LE_Same_8x8),
          Value_64,
          "8x8  => 64 Little_Endian");
      Test_32
         (Big_Endian_Arrays.To_Unsigned_32(LE_Swap_16x2),
          Value_32,
          "16x2 => 32 Big_Endian");
      Test_32
         (Little_Endian_Arrays.To_Unsigned_32(LE_Same_16x2),
          Value_32,
          "16x2 => 32 Little_Endian");
      Test_64
         (Big_Endian_Arrays.To_Unsigned_64(LE_Swap_16x4),
          Value_64,
          "16x4 => 64 Big_Endian");
      Test_64
         (Little_Endian_Arrays.To_Unsigned_64(LE_Same_16x4),
          Value_64,
          "16x4 => 64 Little_Endian");
      Test_64
         (Big_Endian_Arrays.To_Unsigned_64(LE_Swap_32x2),
          Value_64,
          "32x2 => 64 Big_Endian");
      Test_64
         (Little_Endian_Arrays.To_Unsigned_64(LE_Same_32x2),
          Value_64,
          "32x2 => 64 Little_Endian");
      
   elsif System_Byte_Order = Big_Endian then
      
      Test_8x2
         (Native_Arrays.To_Unsigned_8x2(Value_16),
          BE_Same_8x2,
          "16 => 8x2  Native");
      Test_8x2
         (Byte_Swapped_Arrays.To_Unsigned_8x2(Value_16),
          BE_Swap_8x2,
          "16 => 8x2  Byte_Swapped");
      Test_8x4
         (Native_Arrays.To_Unsigned_8x4(Value_32),
          BE_Same_8x4,
          "32 => 8x4  Native");
      Test_8x4
         (Byte_Swapped_Arrays.To_Unsigned_8x4(Value_32),
          BE_Swap_8x4,
          "32 => 8x4  Byte_Swapped");
      Test_8x8
         (Native_Arrays.To_Unsigned_8x8(Value_64),
          BE_Same_8x8,
          "64 => 8x8  Native");
      Test_8x8
         (Byte_Swapped_Arrays.To_Unsigned_8x8(Value_64),
          BE_Swap_8x8,
          "64 => 8x8  Byte_Swapped");
      Test_16x2
         (Native_Arrays.To_Unsigned_16x2(Value_32),
          BE_Same_16x2,
          "32 => 16x2 Native");
      Test_16x2
         (Byte_Swapped_Arrays.To_Unsigned_16x2(Value_32),
          BE_Swap_16x2,
          "32 => 16x2 Byte_Swapped");
      Test_16x4
         (Native_Arrays.To_Unsigned_16x4(Value_64),
          BE_Same_16x4,
          "64 => 16x4 Native");
      Test_16x4
         (Byte_Swapped_Arrays.To_Unsigned_16x4(Value_64),
          BE_Swap_16x4,
          "64 => 16x4 Byte_Swapped");
      Test_32x2
         (Native_Arrays.To_Unsigned_32x2(Value_64),
          BE_Same_32x2,
          "64 => 32x2 Native");
      Test_32x2
         (Byte_Swapped_Arrays.To_Unsigned_32x2(Value_64),
          BE_Swap_32x2,
          "64 => 32x2 Byte_Swapped");
      
      Test_16
         (Native_Arrays.To_Unsigned_16(BE_Same_8x2),
          Value_16,
          "8x2  => 16 Native");
      Test_16
         (Byte_Swapped_Arrays.To_Unsigned_16(BE_Swap_8x2),
          Value_16,
          "8x2  => 16 Byte_Swapped");
      Test_32
         (Native_Arrays.To_Unsigned_32(BE_Same_8x4),
          Value_32,
          "8x4  => 32 Native");
      Test_32
         (Byte_Swapped_Arrays.To_Unsigned_32(BE_Swap_8x4),
          Value_32,
          "8x4  => 32 Byte_Swapped");
      Test_64
         (Native_Arrays.To_Unsigned_64(BE_Same_8x8),
          Value_64,
          "8x8  => 64 Native");
      Test_64
         (Byte_Swapped_Arrays.To_Unsigned_64(BE_Swap_8x8),
          Value_64,
          "8x8  => 64 Byte_Swapped");
      Test_32
         (Native_Arrays.To_Unsigned_32(BE_Same_16x2),
          Value_32,
          "16x2 => 32 Native");
      Test_32
         (Byte_Swapped_Arrays.To_Unsigned_32(BE_Swap_16x2),
          Value_32,
          "16x2 => 32 Byte_Swapped");
      Test_64
         (Native_Arrays.To_Unsigned_64(BE_Same_16x4),
          Value_64,
          "16x4 => 64 Native");
      Test_64
         (Byte_Swapped_Arrays.To_Unsigned_64(BE_Swap_16x4),
          Value_64,
          "16x4 => 64 Byte_Swapped");
      Test_64
         (Native_Arrays.To_Unsigned_64(BE_Same_32x2),
          Value_64,
          "32x2 => 64 Native");
      Test_64
         (Byte_Swapped_Arrays.To_Unsigned_64(BE_Swap_32x2),
          Value_64,
          "32x2 => 64 Byte_Swapped");
      
      Test_8x2
         (Big_Endian_Arrays.To_Unsigned_8x2(Value_16),
          BE_Same_8x2,
          "16 => 8x2  Big_Endian");
      Test_8x2
         (Little_Endian_Arrays.To_Unsigned_8x2(Value_16),
          BE_Swap_8x2,
          "16 => 8x2  Little_Endian");
      Test_8x4
         (Big_Endian_Arrays.To_Unsigned_8x4(Value_32),
          BE_Same_8x4,
          "32 => 8x4  Big_Endian");
      Test_8x4
         (Little_Endian_Arrays.To_Unsigned_8x4(Value_32),
          BE_Swap_8x4,
          "32 => 8x4  Little_Endian");
      Test_8x8
         (Big_Endian_Arrays.To_Unsigned_8x8(Value_64),
          BE_Same_8x8,
          "64 => 8x8  Big_Endian");
      Test_8x8
         (Little_Endian_Arrays.To_Unsigned_8x8(Value_64),
          BE_Swap_8x8,
          "64 => 8x8  Little_Endian");
      Test_16x2
         (Big_Endian_Arrays.To_Unsigned_16x2(Value_32),
          BE_Same_16x2,
          "32 => 16x2 Big_Endian");
      Test_16x2
         (Little_Endian_Arrays.To_Unsigned_16x2(Value_32),
          BE_Swap_16x2,
          "32 => 16x2 Little_Endian");
      Test_16x4
         (Big_Endian_Arrays.To_Unsigned_16x4(Value_64),
          BE_Same_16x4,
          "64 => 16x4 Big_Endian");
      Test_16x4
         (Little_Endian_Arrays.To_Unsigned_16x4(Value_64),
          BE_Swap_16x4,
          "64 => 16x4 Little_Endian");
      Test_32x2
         (Big_Endian_Arrays.To_Unsigned_32x2(Value_64),
          BE_Same_32x2,
          "64 => 32x2 Big_Endian");
      Test_32x2
         (Little_Endian_Arrays.To_Unsigned_32x2(Value_64),
          BE_Swap_32x2,
          "64 => 32x2 Little_Endian");
      
      Test_16
         (Big_Endian_Arrays.To_Unsigned_16(BE_Same_8x2),
          Value_16,
          "8x2  => 16 Big_Endian");
      Test_16
         (Little_Endian_Arrays.To_Unsigned_16(BE_Swap_8x2),
          Value_16,
          "8x2  => 16 Little_Endian");
      Test_32
         (Big_Endian_Arrays.To_Unsigned_32(BE_Same_8x4),
          Value_32,
          "8x4  => 32 Big_Endian");
      Test_32
         (Little_Endian_Arrays.To_Unsigned_32(BE_Swap_8x4),
          Value_32,
          "8x4  => 32 Little_Endian");
      Test_64
         (Big_Endian_Arrays.To_Unsigned_64(BE_Same_8x8),
          Value_64,
          "8x8  => 64 Big_Endian");
      Test_64
         (Little_Endian_Arrays.To_Unsigned_64(BE_Swap_8x8),
          Value_64,
          "8x8  => 64 Little_Endian");
      Test_32
         (Big_Endian_Arrays.To_Unsigned_32(BE_Same_16x2),
          Value_32,
          "16x2 => 32 Big_Endian");
      Test_32
         (Little_Endian_Arrays.To_Unsigned_32(BE_Swap_16x2),
          Value_32,
          "16x2 => 32 Little_Endian");
      Test_64
         (Big_Endian_Arrays.To_Unsigned_64(BE_Same_16x4),
          Value_64,
          "16x4 => 64 Big_Endian");
      Test_64
         (Little_Endian_Arrays.To_Unsigned_64(BE_Swap_16x4),
          Value_64,
          "16x4 => 64 Little_Endian");
      Test_64
         (Big_Endian_Arrays.To_Unsigned_64(BE_Same_32x2),
          Value_64,
          "32x2 => 64 Big_Endian");
      Test_64
         (Little_Endian_Arrays.To_Unsigned_64(BE_Swap_32x2),
          Value_64,
          "32x2 => 64 Little_Endian");
      
   else
      
      Put_Line("***Native and Byte Swapped Tests not supported***");
      
   end if;
   
   
   
end Bullfrog.Tests.Modular_To_Array_Conversions;

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
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

package body Bullfrog.Access_Types.Reference_Counts is

   -- Implementation for incrementing the value
   procedure Increment(Count : in out Count_Type) with Inline;
   procedure Increment(Count : in out Count_Type) is
   begin
      if Count = Count_Type'Last then
         raise Count_Overflow;
      end if;
      if Count /= 0 then
         Count := Count + 1;
      end if;
   end Increment;

   -- Implementation for decrementing the value
   procedure Decrement(Count : in out Count_Type) with Inline;
   procedure Decrement(Count : in out Count_Type) is
   begin
      if Count = Count_Type'First then
         raise Count_Underflow;
      end if;

      Count := Count - 1;
   end Decrement;

   function Get(Count : Reference_Count) return Count_Type is
   begin
      case Count.Atomic is
         when True =>  return Count.Protected_Impl.Get;
         when False => return Count.Impl;
      end case;
   end Get;

   procedure Increment(Count : in out Reference_Count) is
   begin
      case Count.Atomic is
         when True =>  Count.Protected_Impl.Increment;
         when False => Increment(Count.Impl);
      end case;
   end Increment;

   function Post_Increment(Count : in out Reference_Count) return Count_Type is
   begin
      case Count.Atomic is
         when True =>
            return Value : Count_Type do
               Count.Protected_Impl.Post_Increment(Value);
            end return;
         when False =>
            return Value : Count_Type do
               Value := Count.Impl;
               Increment(Count.Impl);
            end return;
      end case;
   end Post_Increment;

   function Pre_Increment(Count : in out Reference_Count) return Count_Type is
   begin
      case Count.Atomic is
         when True =>
            return Value : Count_Type do
               Count.Protected_Impl.Pre_Increment(Value);
            end return;
         when False =>
            Increment(Count.Impl);
            return Count.Impl;
      end case;
   end Pre_Increment;

   procedure Decrement(Count : in out Reference_Count) is
   begin
      case Count.Atomic is
         when True =>  Count.Protected_Impl.Decrement;
         when False => Decrement(Count.Impl);
      end case;
   end Decrement;

   function Post_Decrement(Count : in out Reference_Count) return Count_Type is
   begin
      case Count.Atomic is
         when True =>
            return Value : Count_Type do
               Count.Protected_Impl.Post_Decrement(Value);
            end return;
         when False =>
            return Value : Count_Type do
               Value := Count.Impl;
               Decrement(Count.Impl);
            end return;
      end case;
   end Post_Decrement;

   function Pre_Decrement(Count : in out Reference_Count) return Count_Type is
   begin
      case Count.Atomic is
         when True =>
            return Value : Count_Type do
               Count.Protected_Impl.Pre_Decrement(Value);
            end return;
         when False =>
            Decrement(Count.Impl);
            return Count.Impl;
      end case;
   end Pre_Decrement;

   protected body Atomic_Reference_Count is

      function Get return Count_Type is
      begin
         return Count;
      end Get;

      procedure Increment is
      begin
         Increment(Count);
      end Increment;

      procedure Post_Increment(Value : out Count_Type) is
      begin
         Value := Count;
         Increment(Count);
      end Post_Increment;

      procedure Pre_Increment(Value : out Count_Type) is
      begin
         Increment(Count);
         Value := Count;
      end Pre_Increment;

      procedure Decrement is
      begin
         Decrement(Count);
      end Decrement;

      procedure Post_Decrement(Value : out Count_Type) is
      begin
         Value := Count;
         Decrement(Count);
      end Post_Decrement;

      procedure Pre_Decrement(Value : out Count_Type) is
      begin
         Decrement(Count);
         Value := Count;
      end Pre_Decrement;

   end Atomic_Reference_Count;

end Bullfrog.Access_Types.Reference_Counts;

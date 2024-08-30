------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

package body Bullfrog.Access_Types.Reference_Counts is

   -- Implementation for incrementing the value
   procedure Increment(Count : in out Basic_Count) with Inline;
   procedure Increment(Count : in out Basic_Count) is
   begin
      if Count = Basic_Count'Last then
         raise Count_Overflow;
      end if;
      if Count /= 0 then
         Count := Count + 1;
      end if;
   end Increment;

   -- Implementation for decrementing the value
   procedure Decrement(Count : in out Basic_Count) with Inline;
   procedure Decrement(Count : in out Basic_Count) is
   begin
      if Count = Basic_Count'First then
         raise Count_Underflow;
      end if;

      Count := Count - 1;
   end Decrement;

   function Get(Count : Reference_Count) return Basic_Count is
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

   function Post_Increment(Count : in out Reference_Count) return Basic_Count is
   begin
      case Count.Atomic is
         when True =>
            return Value : Basic_Count do
               Count.Protected_Impl.Post_Increment(Value);
            end return;
         when False =>
            return Value : Basic_Count do
               Value := Count.Impl;
               Increment(Count.Impl);
            end return;
      end case;
   end Post_Increment;

   function Pre_Increment(Count : in out Reference_Count) return Basic_Count is
   begin
      case Count.Atomic is
         when True =>
            return Value : Basic_Count do
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

   function Post_Decrement(Count : in out Reference_Count) return Basic_Count is
   begin
      case Count.Atomic is
         when True =>
            return Value : Basic_Count do
               Count.Protected_Impl.Post_Decrement(Value);
            end return;
         when False =>
            return Value : Basic_Count do
               Value := Count.Impl;
               Decrement(Count.Impl);
            end return;
      end case;
   end Post_Decrement;

   function Pre_Decrement(Count : in out Reference_Count) return Basic_Count is
   begin
      case Count.Atomic is
         when True =>
            return Value : Basic_Count do
               Count.Protected_Impl.Pre_Decrement(Value);
            end return;
         when False =>
            Decrement(Count.Impl);
            return Count.Impl;
      end case;
   end Pre_Decrement;

   protected body Atomic_Reference_Count is

      function Get return Basic_Count is
      begin
         return Count;
      end Get;

      procedure Increment is
      begin
         Increment(Count);
      end Increment;

      procedure Post_Increment(Value : out Basic_Count) is
      begin
         Value := Count;
         Increment(Count);
      end Post_Increment;

      procedure Pre_Increment(Value : out Basic_Count) is
      begin
         Increment(Count);
         Value := Count;
      end Pre_Increment;

      procedure Decrement is
      begin
         Decrement(Count);
      end Decrement;

      procedure Post_Decrement(Value : out Basic_Count) is
      begin
         Value := Count;
         Decrement(Count);
      end Post_Decrement;

      procedure Pre_Decrement(Value : out Basic_Count) is
      begin
         Decrement(Count);
         Value := Count;
      end Pre_Decrement;

   end Atomic_Reference_Count;

end Bullfrog.Access_Types.Reference_Counts;

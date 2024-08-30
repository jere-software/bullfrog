------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

-- This package provides reference count types needed for smart access types
package Bullfrog.Access_Types.Reference_Counts with Pure is

   -- Basic count type for reference counting
   type Basic_Count is mod 2**32;

   -- Occurs if trying to decrement a Basic_Count variable past 'First
   Count_Underflow : exception;

   -- Occurs if trying to increment a Basic_Count variable past 'Last
   Count_Overflow  : exception;

   -- Main reference count type of the package
   type Reference_Count(Atomic : Boolean) is limited private;

   -- Gets the current value of the reference count
   function Get(Count : Reference_Count) return Basic_Count
      with Inline;

   -- Increment the value by 1
   -- Does not increment if the value is Basic_Count'First
   procedure Increment(Count : in out Reference_Count)
      with Inline;

   -- Increment the value by 1 and return the original value
   -- Does not increment if the value is Basic_Count'First
   function Post_Increment(Count : in out Reference_Count) return Basic_Count
      with Inline;

   -- Increment the value by 1 and return the final value
   -- Does not increment if the value is Basic_Count'First
   function Pre_Increment(Count : in out Reference_Count) return Basic_Count
      with Inline;

   -- Decrement the value by 1
   procedure Decrement(Count : in out Reference_Count)
      with Inline;

   -- Decrement the value by 1 and return the original value
   function Post_Decrement(Count : in out Reference_Count) return Basic_Count
      with Inline;

   -- Decrement the value by 1 and return the final value
   function Pre_Decrement(Count : in out Reference_Count) return Basic_Count
      with Inline;

private

   -- Protected implementation provides atomic access to the count
   protected type Atomic_Reference_Count is

      -- Gets the current value of the reference count
      function Get return Basic_Count;

      -- Increment the value by 1
      -- Does not increment if the value is Basic_Count'First
      procedure Increment;

      -- Increment the value by 1 and return the original value
      -- Does not increment if the value is Basic_Count'First
      procedure Post_Increment(Value : out Basic_Count);

      -- Increment the value by 1 and return the final value
      -- Does not increment if the value is Basic_Count'First
      procedure Pre_Increment(Value : out Basic_Count);

      -- Decrement the value by 1
      procedure Decrement;

      -- Decrement the value by 1 and return the original value
      procedure Post_Decrement(Value : out Basic_Count);

      -- Decrement the value by 1 and return the final value
      procedure Pre_Decrement(Value : out Basic_Count);

   private

      Count : Basic_Count := Basic_Count'First + 1;

   end Atomic_Reference_Count;



   type Reference_Count(Atomic : Boolean) is limited record
      case Atomic is
         when True =>
            Protected_Impl : Atomic_Reference_Count;
         when False =>
            Impl           : Basic_Count := Basic_Count'First + 1;
      end case;
   end record;

end Bullfrog.Access_Types.Reference_Counts;

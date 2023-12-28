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

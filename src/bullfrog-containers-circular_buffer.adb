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

package body Bullfrog.Containers.Circular_Buffer is

   -----------------------------------------------------------------------------
   -- Buffer Implementation
   -----------------------------------------------------------------------------

   function Is_Empty (Self : Buffer) return Boolean is
   begin
      return Self.Get_Index = Self.Put_Index;
   end Is_Empty;

   function Not_Empty(Self : Buffer) return Boolean is
   begin
      return Self.Get_Index /= Self.Put_Index;
   end Not_Empty;

   function Is_Full  (Self : Buffer) return Boolean is
      Get_Index : Buffer_Index := Self.Get_Index;
   begin
      if Get_Index = 0 then
         return Self.Put_Index = Self.Max_Size;
      else
         return (Get_Index - Self.Put_Index) = 1;
      end if;
   end Is_Full;

   function Not_Full (Self : Buffer) return Boolean is
      Get_Index : Buffer_Index := Self.Get_Index;
   begin
      if Get_Index = 0 then
         return Self.Put_Index /= Self.Max_Size;
      else
         return (Get_Index - Self.Put_Index) /= 1;
      end if;
   end Not_Full;



   -----------------------------------------------------------------------------
   -- Consumer Implementation
   -----------------------------------------------------------------------------

   package body Consumer is

      function  Get(Source : in out Buffer) return Item_Type is
         Value : Item_Type;
      begin
         if Get(Source,Value) then
            return Value;
         else
            raise Container_Empty;
         end if;
      end Get;

      function  Get
         (Source : in out Buffer;
          Value  :    out Item_Type)
          return Boolean
      is
         Get_Index : Buffer_Index := Source.Get_Index;
      begin

         if Get_Index = Source.Put_Index then
            return False;
         end if;

         Value := Source.Data(Get_Index);

         if Get_Index = Source.Max_Size then
            Get_Index := 0;
         else
            Get_Index := Get_Index + 1;
         end if;

         Source.Get_Index := Get_Index;

         return True;

      end Get;

      procedure Get(Source : in out Buffer; Value : out Item_Type) is
      begin
         if not Get(Source,Value) then
            raise Container_Empty;
         end if;
      end Get;

      procedure Reset(Target : in out Buffer) is
      begin
         Target.Get_Index := Target.Put_Index;
      end Reset;

   end Consumer;

   -----------------------------------------------------------------------------
   -- Producer Implementation
   -----------------------------------------------------------------------------

   package body Producer is

      function Put
         (Target : in out Buffer;
          Value  : in     Item_Type)
          return Boolean
      is
         Put_Index : Buffer_Index := Target.Put_Index;
      begin

         Target.Data(Put_Index) := Value;

         if Put_Index = Target.Max_Size then
            Put_Index := 0;
         else
            Put_Index := Put_Index + 1;
         end if;

         if Put_Index = Target.Get_Index then
            --  Full so don't update index
            return False;
         else
            Target.Put_Index := Put_Index;
            return True;
         end if;
      end Put;


      procedure Put
         (Target : in out Buffer;
          Value  : in     Item_Type)
      is
      begin
         if Put(Target,Value) = False then
            raise Container_Full;
         end if;
      end Put;

   end Producer;

end Bullfrog.Containers.Circular_Buffer;

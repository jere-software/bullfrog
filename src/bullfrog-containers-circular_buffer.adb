------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
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

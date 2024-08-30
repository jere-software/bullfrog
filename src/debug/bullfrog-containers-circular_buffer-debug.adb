------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

package body Bullfrog.Containers.Circular_Buffer.Debug is

   function Put_Index(Source : Buffer) return Buffer_Index is
   begin
      return Source.Put_Index;
   end Put_Index;
   function Get_Index(Source : Buffer) return Buffer_Index is
   begin
      return Source.Get_Index;
   end Get_Index;

   function Current_Element_Status
      (Source : Buffer;
       Index  : Buffer_Index)
          return Element_Status
   is
   begin
      if Index > Source.Max_Size then
         return Out_Of_Bounds;
      elsif Index = Source.Get_Index then
         if Index = Source.Put_Index then
            return Both_Indexes;
         else
            return Get_Index;
         end if;
      elsif Index = Source.Put_Index then
         return Put_Index;
      elsif Source.Get_Index <= Source.Put_Index then
         if Index < Source.Get_Index or Index > Source.Put_Index then
            return No_Element;
         else
            return Some_Element;
         end if;
      else
         if Index < Source.Get_Index and Index > Source.Put_Index then
            return No_Element;
         else
            return Some_Element;
         end if;
      end if;
   end Current_Element_Status;

   function View
      (Source : Buffer;
       Index  : Buffer_Index)
          return Item_Type
   is
   begin
      if Index > Source.Max_Size then
         raise Index_Out_Of_Bounds;
      end if;
      return Source.Data(Index);
   end View;

end Bullfrog.Containers.Circular_Buffer.Debug;

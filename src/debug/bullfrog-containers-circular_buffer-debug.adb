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

package body Bullfrog.Containers.Circular_Buffer.Debug is

   function Put_Index(Buffer : Buffer_Type) return Buffer_Index_Type is
   begin
      return Buffer.Put_Index;
   end Put_Index;
   function Get_Index(Buffer : Buffer_Type) return Buffer_Index_Type is
   begin
      return Buffer.Get_Index;
   end Get_Index;

   function Element_Status
      (Buffer : Buffer_Type;
       Index  : Buffer_Index_Type)
          return Element_Status_Type
   is
   begin
      if Index > Buffer.Buffer_Size then
         return Out_Of_Bounds;
      elsif Index = Buffer.Get_Index then
         if Index = Buffer.Put_Index then
            return Both_Indexes;
         else
            return Get_Index;
         end if;
      elsif Index = Buffer.Put_Index then
         return Put_Index;
      elsif Buffer.Get_Index <= Buffer.Put_Index then
         if Index < Buffer.Get_Index or Index > Buffer.Put_Index then
            return No_Element;
         else
            return Some_Element;
         end if;
      else
         if Index < Buffer.Get_Index and Index > Buffer.Put_Index then
            return No_Element;
         else
            return Some_Element;
         end if;
      end if;
   end Element_Status;

   function View
      (Buffer : Buffer_Type;
       Index  : Buffer_Index_Type)
          return Item_Type
   is
   begin
      if Index > Buffer.Buffer_Size then
         raise Index_Out_Of_Bounds;
      end if;
      return Buffer.Data(Index);
   end View;

end Bullfrog.Containers.Circular_Buffer.Debug;

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

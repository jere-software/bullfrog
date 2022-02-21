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

with System.Address_Image;
with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

package body Bullfrog.Access_Types.Advanced_Smart_Access.Debug is

   function To_Integer is new Ada.Unchecked_Conversion
      (Source => Element_Access,
       Target => Integer_Address);

    function To_Integer is new Ada.Unchecked_Conversion
      (Source => Counts_Access,
       Target => Integer_Address);

   function To_String(Object : Shared_Access) return String is
   begin
      return
         "("
         & System.Address_Image(Object'Address)
         & ","
         & Integer_Address'Image(To_Integer(Object.Item_Reference))
         & ","
         & Integer_Address'Image(To_Integer(Object.Counts_Reference))
         & ","
         & Reference_Counts.Basic_Count'Image(Utilities.Use_Count(Object))
         & ","
         & Reference_Counts.Basic_Count'Image(Utilities.Weak_Count(Object))
         & ")";
   end To_String;
   function To_String(Object : Weak_Access) return String is
   begin
      return
         "("
         & System.Address_Image(Object'Address)
         & ","
         & Integer_Address'Image(To_Integer(Object.Item_Reference))
         & ","
         & Integer_Address'Image(To_Integer(Object.Counts_Reference))
         & ","
         & Reference_Counts.Basic_Count'Image(Utilities.Use_Count(Object))
         & ","
         & Reference_Counts.Basic_Count'Image(Utilities.Weak_Count(Object))
         & ")";
   end To_String;
   function To_String(Object : Unique_Access) return String is
   begin
      return
         "("
         & System.Address_Image(Object'Address)
         & ","
         & Integer_Address'Image(To_Integer(Object.Item_Reference))
         & ")";
   end To_String;
end Bullfrog.Access_Types.Advanced_Smart_Access.Debug;

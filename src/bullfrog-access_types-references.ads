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

-- This package provides types for references and constant references
generic

   -- This is the type that the references will reflect
   type Item_Type(<>);

package Bullfrog.Access_Types.References is

   pragma Pure;

   -- Provides a reference holder that is safer than just a simple access
   -- type.  For most purposes, a variable of type Reference is equivalant
   -- to a variable of type Item_Type.
   type Reference
      (Raw_Access : access Item_Type)
   is limited null record
      with
         Implicit_Dereference => Raw_Access;

   -- Provides a reference holder that is safer than just a simple access
   -- type.  For most purposes, a variable of type Reference is equivalant
   -- to a variable of type Item_Type.  This version provides read-only
   -- access.
   type Constant_Reference
      (Raw_Access : access constant Item_Type)
   is limited null record
      with
         Implicit_Dereference => Raw_Access;

   -- Provides a reference holder that is safer than just a simple access
   -- type.  For most purposes, a variable of type Reference is equivalant
   -- to a variable of type Item_Type.  This version ensures that the
   -- reference is not null.
   type Not_Null_Reference
      (Raw_Access : not null access Item_Type)
   is limited null record
      with
         Implicit_Dereference => Raw_Access;

   -- Provides a reference holder that is safer than just a simple access
   -- type.  For most purposes, a variable of type Reference is equivalant
   -- to a variable of type Item_Type.  This version ensures that the
   -- reference is not null and provides read-only access.
   type Not_Null_Constant_Reference
      (Raw_Access : not null access constant Item_Type)
   is limited null record
      with
         Implicit_Dereference => Raw_Access;

end Bullfrog.Access_Types.References;

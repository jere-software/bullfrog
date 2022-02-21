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

-- This package provides a set of traits needed to handle incomplete types
-- for Smart_Access based packages.  See the package Advanced_Smart_Access
-- for ane sample on how to use this package.
generic
   
   -- The basic type held by a Smart_Access type
   type Element_Type(<>);
   
package Bullfrog.Access_Types.Advanced_Smart_Access_Traits is

   -- Returns the ID for this package instantiation.  The ID is unique only
   -- while the package is in scope.
   function Get_Package_ID return Trait_Package_ID;
   
private
   
   -- Dummy variable type needed
   type Dummy is null record;
   
   -- Dummy variable
   Placeholder : constant Dummy := (null record);
   
   function Get_Package_ID return Trait_Package_ID is 
      (Trait_Package_ID(Placeholder'Address));
   
end Bullfrog.Access_Types.Advanced_Smart_Access_Traits;

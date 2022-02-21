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

with Bullfrog.Access_Types.Advanced_Smart_Access;

-- This package provides all the constructing operations for Smart_Access
-- types.  See the package Advanced_Smart_Access for ane sample on how to use 
-- this package.
generic
   
   -- The complete type that corresponds to Element_Type used
   -- in the parent package.
   type Element_Type(<>) is limited private;
   
   -- This type provides variable access to the resource
   type Element_Access is access Element_Type;

   -- This package contains the element type to make Smart_Access types for.
   -- This is used to validate that the two Element_Types used here
   -- and in the parent package actually match.
   with package Traits is new Advanced_Smart_Access_Traits(Element_Type);
   
package Bullfrog.Access_Types.Advanced_Smart_Access.Make is
   
   -- Constructs a Shared_Access object
   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in     not null Element_Access);
   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in     Advanced_Smart_Access.Shared_Access);
   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in     Advanced_Smart_Access.Weak_Access);
   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in out Advanced_Smart_Access.Unique_Access);
   function Shared_Access
      (Source : in not null Element_Access)
       return Advanced_Smart_Access.Shared_Access;
   function Shared_Access
      (Source : in Advanced_Smart_Access.Weak_Access)
       return Advanced_Smart_Access.Shared_Access;
   function Shared_Access
      (Source : in out Advanced_Smart_Access.Unique_Access)
       return Advanced_Smart_Access.Shared_Access;

   -- Constructs a Weak_Access object
   procedure Weak_Access
      (Target : in out Advanced_Smart_Access.Weak_Access;
       Source : in     Advanced_Smart_Access.Weak_Access);
   procedure Weak_Access
      (Target : in out Advanced_Smart_Access.Weak_Access;
       Source : in     Advanced_Smart_Access.Shared_Access);
   function Weak_Access
      (Source : in Advanced_Smart_Access.Shared_Access)
       return Advanced_Smart_Access.Weak_Access;

   -- Constructs a Unique_Access object
   procedure Unique_Access
      (Target : in out Advanced_Smart_Access.Unique_Access;
       Source : in     not null Element_Access);
   procedure Unique_Access
      (Target : in out Advanced_Smart_Access.Unique_Access;
       Source : in out Advanced_Smart_Access.Unique_Access);
   function Unique_Access
      (Source : in not null Element_Access)
          return Advanced_Smart_Access.Unique_Access;
   function Unique_Access
      (Source : in out Advanced_Smart_Access.Unique_Access)
       return Advanced_Smart_Access.Unique_Access;
   
private
   
   -- This procedure is used as a means to ensure this package isn't 
   -- instantiated at an accessibility level lower than the parent
   -- package
   procedure Accessibility_Check
      (Memory : in out Advanced_Smart_Access.Element_Access) 
   is null;
   
   -- Ada rules require that the Access to Accessibiity_Check only be used
   -- in the package spec of a generic package.  This variable is a 
   -- temporary holding spot for it.
   Deallocate_Check : constant Deallocation := Accessibility_Check'Access;

end Bullfrog.Access_Types.Advanced_Smart_Access.Make;

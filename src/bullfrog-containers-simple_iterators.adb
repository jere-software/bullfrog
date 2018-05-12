------------------------------------------------------------------------------
--                      Copyright (C) 2018 - Present                        --
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

package body Bullfrog.Containers.Simple_Iterators is

   package body Forward is

      function First
         (Iterator : Forward_Iterator)
          return Cursor_Type
      is begin
         return First(Iterator.Container.all);
      end First;

      function Next
         (Iterator : Forward_Iterator;
          Cursor   : Cursor_Type)
          return Cursor_Type
      is begin
         return Next(Iterator.Container.all,Cursor);
      end Next;

      function Forward_Iterate
         (Container : Container_Type)
          return Iterator_Interfaces.Forward_Iterator'Class
      is begin
         return Forward_Iterator'
            (Container => Container'Unchecked_Access);
      end Forward_Iterate;

   end Forward;

   package body Reversible is

      function First
         (Iterator : Reversible_Iterator)
          return Cursor_Type
      is begin
         return First(Iterator.Container.all);
      end First;

      function Next
         (Iterator : Reversible_Iterator;
          Cursor   : Cursor_Type)
          return Cursor_Type
      is begin
         return Next(Iterator.Container.all,Cursor);
      end Next;

      function Last
         (Iterator : Reversible_Iterator)
          return Cursor_Type
      is begin
         return Last(Iterator.Container.all);
      end Last;

      function Previous
         (Iterator : Reversible_Iterator;
          Cursor   : Cursor_Type)
          return Cursor_Type
      is begin
         return Previous(Iterator.Container.all,Cursor);
      end Previous;

      function Forward_Iterate
         (Container : Container_Type)
          return Iterator_Interfaces.Forward_Iterator'Class
      is begin
         return Reversible_Iterator'
            (Container => Container'Unchecked_Access);
      end Forward_Iterate;

      function Reversible_Iterate
         (Container : Container_Type)
          return Iterator_Interfaces.Reversible_Iterator'Class
      is begin
         return Reversible_Iterator'
            (Container => Container'Unchecked_Access);
      end Reversible_Iterate;

   end Reversible;


end Bullfrog.Containers.Simple_Iterators;

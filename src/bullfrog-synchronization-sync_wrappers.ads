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

with Bullfrog.Synchronization.Mutexes;
with Ada.Finalization;

-- Provides a quick protected interface for complex already defined types.
-- If making your own types, it is much more efficient to create a custom
-- protected type.  This package only allows access to the data via a
-- scoped mutex.  The mutex automatically locks when accessing the variable
-- through the Lock function and unlocks when it goes out of scope.
generic

   -- type of Element to protect.  It must be constrained
   type Element_Type is limited private;


package Bullfrog.Synchronization.Sync_Wrappers is

   -- This is the primary way to access the variable.  It must be created
   -- with a call to the procedure "Lock".  If it is default initializes,
   -- it raises the Unsynchronized_Data exception.  Upon creation it locks
   -- a mutex.  The mutex is unlocked when the Scoped_Lock object goes out
   -- of scope.
   type Scoped_Lock(Element : not null access Element_Type)
   is limited private
      with Implicit_Dereference => Element;

   -- This is the type that holds the actual copy of the data.  It must live
   -- longer than any Scoped_Lock objects.
   type Wrapper is tagged limited private;

   -- This function provides access to the internal data by creating a
   -- scoped lock.  Do not call this at library level or the declarative
   -- region of a operation where you plan to further call the Lock operation.
   function Lock(Self : aliased in out Wrapper) return Scoped_Lock;

   -- Optional default constructor
   generic
      with function Default return Element_Type;
   function Default return Wrapper with Inline;

   -- Constructor for 1 parameter
   generic
      type Item_Type(<>) is limited private;
      with function Constructor(Value : Item_Type) return Element_Type;
   function Make(Value : Item_Type) return Wrapper with Inline;

private

   -- This type provides finalization/initialization to control locking and
   -- unlocking the mutex;
   type Finalizer(Mutex : access Mutexes.Basic_Mutex := null) is
      new Ada.Finalization.Limited_Controlled with null record;

   -- Shouldn't be called if package is used correctly.  Raises the
   -- Unsynchronized_Data exception if called
   overriding procedure Initialize(Self : in out Finalizer);

   -- Unlocks the mutex
   overriding procedure Finalize(Self : in out Finalizer);

   type Scoped_Lock(Element : not null access Element_Type) is limited record
      Impl : Finalizer;
   end record;

   type Wrapper is new Ada.Finalization.Limited_Controlled with record
      Mutex   : aliased Mutexes.Basic_Mutex;
      Element : aliased Element_Type;
   end record;

   -- Can only finish if no Scoped_Locks are active
   overriding procedure Finalize(Self : in out Wrapper);

end Bullfrog.Synchronization.Sync_Wrappers;

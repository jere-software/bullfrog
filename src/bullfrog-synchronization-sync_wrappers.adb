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

package body Bullfrog.Synchronization.Sync_Wrappers is

   function Lock (Self : aliased in out Wrapper) return Scoped_Lock is
   begin
      Self.Mutex.Lock;
      return (Element => Self.Element'Access,
              Impl    =>
                 (Ada.Finalization.Limited_Controlled with
                     Mutex => Self.Mutex'Access));
   end Lock;

   procedure Initialize (Self : in out Finalizer) is
   begin

      -- This procedure should never be called
      raise Unsynchronized_Data;
   end Initialize;

   procedure Finalize (Self : in out Finalizer) is
   begin

      -- Unlock mutex when going out of scope
      Self.Mutex.Unlock;
   end Finalize;

   function Default return Wrapper is
   begin
      return (Ada.Finalization.Limited_Controlled with
                 Element => Default, others => <>);
   end Default;

   -- After bug fix will be:
   --     function Make(Value : Item_Type) return Wrapper is
   --     begin
   --        return (Element => Constructor(Value), others => <>);
   --     end Make;
   package body Constructors is
      function Make(Value : Item_Type) return Wrapper is
      begin
         return (Ada.Finalization.Limited_Controlled with
                    Element => Constructor(Value), others => <>);
      end Make;
   end Constructors;

   procedure Finalize(Self : in out Wrapper) is
   begin
      -- Prevents finalization until all scoped locks are finalized
      Self.Mutex.Lock;
   end Finalize;

end Bullfrog.Synchronization.Sync_Wrappers;

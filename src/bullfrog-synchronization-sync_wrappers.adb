------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
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

   function Make(Value : Item_Type) return Wrapper is
   begin
      return (Ada.Finalization.Limited_Controlled with
                 Element => Constructor(Value), others => <>);
   end Make;

   procedure Finalize(Self : in out Wrapper) is
   begin
      -- Prevents finalization until all scoped locks are finalized
      Self.Mutex.Lock;
   end Finalize;

end Bullfrog.Synchronization.Sync_Wrappers;

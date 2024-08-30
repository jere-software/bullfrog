------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

package body Bullfrog.Synchronization.Mutexes is

   procedure Lock (Self : in out Basic_Mutex) is
   begin
      Self.Impl.Lock;
   end Lock;

   procedure Unlock (Self : in out Basic_Mutex) is
   begin
      Self.Impl.Unlock;
   end Unlock;

   function Try_Lock (Self : in out Basic_Mutex) return Boolean is
   begin
      return Result : Boolean do
         Self.Impl.Try_Lock(Result);
      end return;
   end Try_Lock;

   function Is_Locked (Self : Basic_Mutex) return Boolean is
   begin
      return Self.Impl.Is_Locked;
   end Is_Locked;

   function Current_Owner (Self : Basic_Mutex) return Owner is
   begin
      return Self.Impl.Current_Owner;
   end Current_Owner;

   protected body Basic_Impl is

      entry Lock when not Locked is
      begin
         Current := Basic_Impl.Lock'Caller;
         Locked  := True;
      end Lock;

      procedure Unlock is
      begin
         if Locked then
            Current := Null_Task_Id;
            Locked  := False;
         else
            raise Mutex_Use_Error;
         end if;
      end Unlock;

      procedure Try_Lock(Success : out Boolean) is
      begin
         if Locked then
            Success := False;
         else
            Current := Current_Task;
            Locked  := True;
            Success := True;
         end if;
      end Try_Lock;

      function Is_Locked return Boolean is
      begin
         return Locked;
      end Is_Locked;

      function Current_Owner return Owner is
      begin
         return Current;
      end Current_Owner;

   end Basic_Impl;

   procedure Lock (Self : in out Recursive_Mutex) is
   begin
      Self.Impl.Lock;
   end Lock;

   procedure Unlock (Self : in out Recursive_Mutex) is
   begin
      Self.Impl.Unlock;
   end Unlock;

   function Try_Lock (Self : in out Recursive_Mutex) return Boolean is
   begin
      return Result : Boolean do
         Self.Impl.Try_Lock(Result);
      end return;
   end Try_Lock;

   function Is_Locked (Self : Recursive_Mutex) return Boolean is
   begin
      return Self.Impl.Is_Locked;
   end Is_Locked;

   function Current_Owner (Self : Recursive_Mutex) return Owner is
   begin
      return Self.Impl.Current_Owner;
   end Current_Owner;

   protected body Recursive_Impl is

      entry Lock when True is
      begin
         if Current = Null_Task_Id then

            -- If no owner, then take possession
            Current := Recursive_Impl.Lock'Caller;
            Count := 1;

         elsif Current = Recursive_Impl.Lock'Caller then

            -- If already the owner, then just increment the count
            Count := Count + 1;

         else

            -- It is owned, but not the owner, so wait
            requeue Actual_Lock;

         end if;
      end Lock;

      procedure Unlock is
      begin
         if Current = Current_Task then

            if Count > 0 then

               -- Decrementing count
               Count := Count - 1;

               if Count = 0 then

                  -- releasing ownership since no longer in recursion
                  Current := Null_Task_Id;

               end if;

            else

               -- Something went horribly wrong here
               raise Mutex_Use_Error;

            end if;

         else

            -- Can only unlock if the owner
            raise Mutex_Use_Error;

         end if;
      end Unlock;

      procedure Try_Lock (Success : out Boolean) is
      begin
         if Current = Null_Task_Id then

            -- Not owned, so take ownership
            Current := Current_Task;
            Count   := 1;
            Success := True;

         elsif Current = Current_Task then

            -- Already own it so update recursion count
            Count   := Count + 1;
            Success := True;

         else

            -- Someone else owns it already
            Success := False;

         end if;
      end Try_Lock;

      function Is_Locked return Boolean is
      begin
         return Count > 0;
      end Is_Locked;

      function Current_Owner return Owner is
      begin
         return Current;
      end Current_Owner;

      entry Actual_Lock when Count = 0 is
      begin

         -- After blocking for a while, now can take ownership
         Count   := 1;
         Current := Actual_Lock'Caller;

      end Actual_Lock;

   end Recursive_Impl;

end Bullfrog.Synchronization.Mutexes;

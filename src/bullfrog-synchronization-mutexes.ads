------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

with Ada.Task_Identification;

-- Provides mutex implementations
package Bullfrog.Synchronization.Mutexes is

   -----------------------------------------------------------------------------
   -- Support types
   -----------------------------------------------------------------------------

   -- Type to represent the owner
   subtype Owner is Ada.Task_Identification.Task_Id;

   -----------------------------------------------------------------------------
   -- Basic Mutex Implementation
   -----------------------------------------------------------------------------

   -- Basic functionality standard mutex
   type Basic_Mutex is tagged limited private;

   -- Acquires the mutex on a mutex or blocks until it can.  Raises
   -- Constraint_Error if recursively called too deep
   procedure Lock(Self : in out Basic_Mutex)
      with Inline;

   -- Unlocks the mutex.  Raises Mutex_Use_Error if not the owning task or
   -- not locked.
   procedure Unlock(Self : in out Basic_Mutex)
      with Inline;

   -- Acquires the mutex if available.  Returns True if successful.  Raises
   -- Constraint_Error if recursively called too deep
   function Try_Lock(Self : in out Basic_Mutex) return Boolean
      with Inline;

   -- Indicates if the mutex is currently locked
   function Is_Locked(Self : Basic_Mutex) return Boolean
      with Inline;

   -- Returns an identifier unique to the owning task.  Use
   -- Ada.Task_Identification facilities to leverage this value
   function Current_Owner(Self : Basic_Mutex) return Owner
      with Inline;


   -----------------------------------------------------------------------------
   -- Recursive Mutex Implementation
   -----------------------------------------------------------------------------

   -- Mutex that can be relocked by the thread already owning the mutex, 
   -- so that it can be locked recursively. It must be unlocked recursively
   -- as well to prevent deadlock
   type Recursive_Mutex is tagged limited private;

   -- Acquires the mutex on a mutex or blocks until it can.  Raises
   -- Constraint_Error if recursively called too deep
   procedure Lock(Self : in out Recursive_Mutex)
      with Inline;

   -- Unlocks the mutex.  Raises Mutex_Use_Error if not the owning task or
   -- not locked.
   procedure Unlock(Self : in out Recursive_Mutex)
      with Inline;

   -- Acquires the mutex if available.  Returns True if successful.  Raises
   -- Constraint_Error if recursively called too deep
   function Try_Lock(Self : in out Recursive_Mutex) return Boolean
      with Inline;

   -- Indicates if the mutex is currently locked
   function Is_Locked(Self : Recursive_Mutex) return Boolean
      with Inline;

   -- Returns an identifier unique to the owning task.  Use
   -- Ada.Task_Identification facilities to leverage this value
   function Current_Owner(Self : Recursive_Mutex) return Owner
      with Inline;

private

   use Ada.Task_Identification;

   -- Protected Implementation of the basic mutex
   protected type Basic_Impl is

      -- Acquires the lock on a mutex or blocks until it can.
      entry Lock;

      -- Unlocks the mutex.  Raises Mutex_Use_Error if not the owning task or
      -- not locked.
      procedure Unlock;

      -- Acquires the mutex if available.  Returns True if successful.
      procedure Try_Lock(Success : out Boolean);

      -- Indicates if the mutex is currently locked
      function Is_Locked return Boolean;

      -- Returns an identifier unique to the owning task.  Use
      -- Ada.Task_Identification facilities to leverage this value
      function Current_Owner return Owner;

   private

      -- Holds the owner of the mutex
      Current : Owner := Null_Task_Id;

      -- Lock state of the mutex
      Locked  : Boolean := False;

   end Basic_Impl;

   type Basic_Mutex is tagged limited record
      Impl : Basic_Impl;
   end record;

   -- Protected implementation of the recursive mutex
   protected type Recursive_Impl is

      -- Acquires the lock on a mutex or blocks until it can.  Raises
      -- Constraint_Error if recursively called too deep.  Requeues to
      -- Actual_Lock if not the current owner (and there is an owner)
      entry Lock;

      -- Unlocks the mutex.  Raises Mutex_Use_Error if not the owning task or
      -- not locked.
      procedure Unlock;

      -- Acquires the mutex if available.  Returns True if successful.  Raises
      -- Constraint_Error if recursively called too deep
      procedure Try_Lock(Success : out Boolean);

      -- Indicates if the mutex is currently locked
      function Is_Locked return Boolean;

      -- Returns an identifier unique to the owning task.  Use
      -- Ada.Task_Identification facilities to leverage this value
      function Current_Owner return Owner;

   private

      -- Requeue entry for the situation where there is an owning task, but
      -- it isn't the current task
      entry Actual_Lock;

      -- Holds the owner of the mutex
      Current : Owner := Null_Task_Id;

      -- Recursive count
      Count   : Natural := 0;

   end Recursive_Impl;

   type Recursive_Mutex is tagged limited record
      Impl : Recursive_Impl;
   end record;

end Bullfrog.Synchronization.Mutexes;

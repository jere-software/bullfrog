------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

-- Base package for synchronization utilities
package Bullfrog.Synchronization with Pure is

   -- Generated when attempting to use data that should be synchronized
   -- but is not
   Unsynchronized_Data : exception;

   -- Generated when incorrectly using a mutex, such as trying to unlock
   -- an already unlocked mutex or when not the owner.
   Mutex_Use_Error     : exception;

end Bullfrog.Synchronization;

------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

generic
package Bullfrog.Access_Types.Advanced_Smart_Access.Debug is
   function To_String(Object : Shared_Access) return String;
   function To_String(Object : Weak_Access)   return String;
   function To_String(Object : Unique_Access) return String;
end Bullfrog.Access_Types.Advanced_Smart_Access.Debug;

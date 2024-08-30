------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

-- Top level package for the Bullfrog containers library
package Bullfrog.Containers with Pure is

   -- This exception is raised if a Put is issued on a full container
   Container_Full  : exception;

   -- This exception is raised if a Get is issued on an empty container
   Container_Empty : exception;

end Bullfrog.Containers;

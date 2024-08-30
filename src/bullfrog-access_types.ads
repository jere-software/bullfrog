------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

private with System.Storage_Elements;

-- Base package for special access types
package Bullfrog.Access_Types with Pure is

   -- An ID type used to validate that trait packages are correctly referenced
   type Trait_Package_ID is private;

private

   type Trait_Package_ID is new System.Storage_Elements.Integer_Address;

end Bullfrog.Access_Types;

------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

-- Package used to debug a buffer's state.  This package is not task safe.
generic
package Bullfrog.Containers.Circular_Buffer.Debug is



   function Put_Index(Source : Buffer) return Buffer_Index;
   function Get_Index(Source : Buffer) return Buffer_Index;

   type Element_Status is
      (Both_Indexes,
       Put_Index,
       Get_Index,
       Some_Element,
       No_Element,
       Out_Of_Bounds);

   function Current_Element_Status
      (Source : Buffer;
       Index  : Buffer_Index)
       return Element_Status;

   Index_Out_Of_Bounds : exception;

   function View
      (Source : Buffer;
       Index  : Buffer_Index)
       return Item_Type;


end Bullfrog.Containers.Circular_Buffer.Debug;

------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------


with Ada.Unchecked_Deallocation;

package body Bullfrog.Access_Types.Advanced_Smart_Access is

   use type Bullfrog.Access_Types.Reference_Counts.Basic_Count;

   -----------------------------------------------------------------------------
   -- Shared_Access type
   -----------------------------------------------------------------------------

   procedure Set_Null
      (Self : in out Shared_Access)
       renames Finalize;

   function Reference
      (Self : in Shared_Access)
       return Reference_Holder
   is (Element => Self.Item_Reference);

   function Constant_Reference
      (Self : in Shared_Access)
       return Constant_Reference_Holder
   is (Element => Self.Item_Reference);

   overriding
   function "="
      (Left,Right : Shared_Access)
       return Boolean
   is
   begin
      return Left.Item_Reference = Right.Item_Reference;
   end "=";

   function Is_Null
      (Self : in Shared_Access)
       return Boolean
   is
   begin
      return Self.Item_Reference = null;
   end Is_Null;

   function Not_Null
      (Self : in Shared_Access)
       return Boolean
   is
   begin
      return Self.Item_Reference /= null;
   end Not_Null;

   procedure Swap
      (Left, Right : in out Shared_Access)
   is
      Temp_Reference : Element_Access   := Left.Item_Reference;
      Temp_Counts    : Counts_Access := Left.Counts_Reference;
   begin
      Left.Item_Reference    := Right.Item_Reference;
      Left.Counts_Reference  := Right.Counts_Reference;
      Right.Item_Reference   := Temp_Reference;
      Right.Counts_Reference := Temp_Counts;
   end Swap;

   procedure Move (Target, Source : in out Shared_Access) is
   begin
      Target.Finalize;
      Target.Item_Reference   := Source.Item_Reference;
      Target.Counts_Reference := Source.Counts_Reference;
      Source.Item_Reference   := null;
      Source.Counts_Reference := null;
   end Move;

   overriding
   procedure Adjust
      (Self : in out Shared_Access)
   is
   begin
      if Self.Item_Reference /= null then
         Reference_Counts.Increment(Self.Counts_Reference.Strong);
      end if;
   end Adjust;

   overriding
   procedure Finalize
      (Self : in out Shared_Access)
   is

      use Access_Types.Reference_Counts;

      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Counts,
          Name   => Counts_Access);

   begin
      if Self.Item_Reference /= null then
         if Pre_Decrement(Self.Counts_Reference.Strong) = 0 then
            if Pre_Decrement(Self.Counts_Reference.Weak) = 0 then
               Free(Self.Counts_Reference);
            end if;
            Deallocate(Self.Item_Reference);
         end if;
         Self.Item_Reference   := null;
         Self.Counts_Reference := null;
      end if;
   end Finalize;

   -----------------------------------------------------------------------------
   -- Weak_Access type
   -----------------------------------------------------------------------------

   procedure Remove_Assignment
      (Self : in out Weak_Access)
       renames Finalize;

   overriding
   function "="
      (Left,Right : Weak_Access)
       return Boolean
   is
   begin
      return Left.Counts_Reference = Right.Counts_Reference;
   end "=";

   function Is_Assigned
      (Self : in Weak_Access)
       return Boolean
   is
   begin
      return Self.Counts_Reference /= null;
   end Is_Assigned;

   function Not_Assigned
      (Self : in Weak_Access)
       return Boolean
   is
   begin
      return
         Self.Counts_Reference = null;
   end Not_Assigned;

   procedure Swap
      (Left, Right : in out Weak_Access)
   is
      Temp_Reference : Element_Access   := Left.Item_Reference;
      Temp_Counts    : Counts_Access := Left.Counts_Reference;
   begin
      Left.Item_Reference    := Right.Item_Reference;
      Left.Counts_Reference  := Right.Counts_Reference;
      Right.Item_Reference   := Temp_Reference;
      Right.Counts_Reference := Temp_Counts;
   end Swap;

   procedure Move (Target, Source : in out Weak_Access) is
   begin
      Target.Finalize;
      Target.Item_Reference   := Source.Item_Reference;
      Target.Counts_Reference := Source.Counts_Reference;
      Source.Item_Reference   := null;
      Source.Counts_Reference := null;
   end Move;

   overriding
   procedure Adjust
      (Self : in out Weak_Access)
   is
   begin
      if Self.Counts_Reference /= null then
         Reference_Counts.Increment(Self.Counts_Reference.Weak);
      end if;
   end Adjust;

   overriding
   procedure Finalize
      (Self : in out Weak_Access)
   is

      use Access_Types.Reference_Counts;
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Counts,
          Name   => Counts_Access);

   begin
      if Self.Counts_Reference /= null then
         if Pre_Decrement(Self.Counts_Reference.Weak) = 0 then
            Free(Self.Counts_Reference);
         end if;
         Self.Item_Reference   := null;
         Self.Counts_Reference := null;
      end if;
   end Finalize;


   -----------------------------------------------------------------------------
   -- Unique_Access type
   -----------------------------------------------------------------------------

   procedure Set_Null
      (Self : in out Unique_Access)
       renames Finalize;

   function Reference
      (Self : in Unique_Access)
       return Reference_Holder
   is (Element => Self.Item_Reference);

   function Constant_Reference
      (Self : in Unique_Access)
       return Constant_Reference_Holder
   is (Element => Self.Item_Reference);

   function Is_Null
      (Self : in Unique_Access)
       return Boolean
   is
   begin
      return Self.Item_Reference = null;
   end Is_Null;

   function Not_Null
      (Self : in Unique_Access)
       return Boolean
   is
   begin
      return Self.Item_Reference /= null;
   end Not_Null;

   procedure Swap
      (Left, Right : in out Unique_Access)
   is
      Temp_Reference : Element_Access := Left.Item_Reference;
   begin
      Left.Item_Reference  := Right.Item_Reference;
      Right.Item_Reference := Temp_Reference;
   end Swap;

   procedure Move (Target, Source : in out Unique_Access) is
   begin
      Target.Finalize;
      Target.Item_Reference   := Source.Item_Reference;
      Source.Item_Reference   := null;
   end Move;

   overriding
   procedure Finalize
      (Self : in out Unique_Access)
   is
   begin
      if Self.Item_Reference /= null then
         Deallocate(Self.Item_Reference);
         Self.Item_Reference := null;
      end if;
   end Finalize;



   package body Utilities is

      function Use_Count
         (Self : in Shared_Access)
          return Basic_Count
      is
      begin
         if Self.Counts_Reference = null then
            return Basic_Count'First;
         else
            return Reference_Counts.Get(Self.Counts_Reference.Strong);
         end if;
      end Use_Count;

      function Weak_Count
         (Self : in Shared_Access)
          return Basic_Count
      is
      begin
         if Self.Counts_Reference = null then
            return Basic_Count'First;
         else
            return Reference_Counts.Get(Self.Counts_Reference.Weak);
         end if;
      end Weak_Count;

      function Raw_Access
         (Self : in Shared_Access)
          return Element_Access
      is (Self.Item_Reference);

      function Raw_Constant_Access
         (Self : in Shared_Access)
          return Constant_Element_Access
      is (Constant_Element_Access(Self.Item_Reference));


      function Use_Count
         (Self : in Weak_Access)
          return Basic_Count
      is
      begin
         if Self.Counts_Reference = null then
            return Basic_Count'First;
         else
            return Reference_Counts.Get(Self.Counts_Reference.Strong);
         end if;
      end Use_Count;

      function Weak_Count
         (Self : in Weak_Access)
          return Basic_Count
      is
      begin
         if Self.Counts_Reference = null then
            return Basic_Count'First;
         else
            return Reference_Counts.Get(Self.Counts_Reference.Weak);
         end if;
      end Weak_Count;


      function Is_Assigned_To
         (Weak   : in Weak_Access;
          Shared : in Shared_Access)
          return Boolean
      is
      begin
         return Weak.Counts_Reference /= null
            and Weak.Counts_Reference =  Shared.Counts_Reference;
      end Is_Assigned_To;

      function Not_Assigned_To
         (Weak   : in Weak_Access;
          Shared : in Shared_Access)
          return Boolean
      is
      begin
         return Weak.Counts_Reference =  null
            or  Weak.Counts_Reference /= Shared.Counts_Reference;
      end Not_Assigned_To;

      function Raw_Access
         (Self : in Unique_Access)
          return Element_Access
      is (Self.Item_Reference);

      function Raw_Constant_Access
         (Self : in Unique_Access)
          return Constant_Element_Access
      is (Constant_Element_Access(Self.Item_Reference));

   end Utilities;

end Bullfrog.Access_Types.Advanced_Smart_Access;

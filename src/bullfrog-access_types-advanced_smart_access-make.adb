------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Bullfrog.Access_Types.Advanced_Smart_Access.Make is
   
   -- Access type to local deallocation method
   type Deallocation is access procedure (Memory : in out Element_Access);
   
   -- Thie procedure frees the Element_Type object
   procedure Deallocate is new Ada.Unchecked_Deallocation
      (Object => Element_Type,
       Name   => Element_Access);
   
   -- Wrapper procedure needed for main package
   procedure Deallocate_Wrapper(Memory : in out Element_Access) is
   begin
      Deallocate(Memory);
   end Deallocate_Wrapper;
   
   -- This function converts Element_Access between packages
   function Convert is new Ada.Unchecked_Conversion
      (Source => Element_Access,
       Target => Advanced_Smart_Access.Element_Access);
   
   -- This function converts Deallocation objects between pacakges
   function Convert is new Ada.Unchecked_Conversion
      (Source => Deallocation,
       Target => Advanced_Smart_Access.Deallocation);
   
   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in     not null Element_Access)
   is
   begin
      Target.Finalize;
      Target.Counts_Reference := new Counts;
      Target.Item_Reference   := Convert(Source);
   exception
      when others =>
         declare
            Temp : Element_Access := Source;
         begin
            Deallocate(Temp);
            raise;
         end;
   end Shared_Access;

   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in     Advanced_Smart_Access.Shared_Access)
   is
   begin
      if Target = Source then
         return;
      end if;

      Target.Finalize;
      if Source.Item_Reference /= null then
         Reference_Counts.Increment(Source.Counts_Reference.Strong);
         Target.Item_Reference   := Source.Item_Reference;
         Target.Counts_Reference := Source.Counts_Reference;
      end if;

   end Shared_Access;

   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in     Advanced_Smart_Access.Weak_Access)
   is
      use Access_Types.Reference_Counts;
   begin
      if Utilities.Is_Assigned_To(Source, Target) then
         return;
      end if;
      Target.Finalize;
      if Source.Counts_Reference /= null then
         if Pre_Increment(Source.Counts_Reference.Strong) /= 0 then
            Target.Item_Reference    := Source.Item_Reference;
            Target.Counts_Reference  := Source.Counts_Reference;
         end if;
      end if;
   end Shared_Access;

   procedure Shared_Access
      (Target : in out Advanced_Smart_Access.Shared_Access;
       Source : in out Advanced_Smart_Access.Unique_Access)
   is
   begin
      Target.Finalize;
      if Source.Item_Reference /= null then
         Target.Counts_Reference := new Counts;
         Target.Item_Reference   := Source.Item_Reference;
         Source.Item_Reference   := null;
      end if;
   end Shared_Access;

   function Shared_Access
      (Source : in not null Element_Access)
       return Advanced_Smart_Access.Shared_Access
   is
   begin
      return (Ada.Finalization.Controlled with
                 Item_Reference   => Convert(Source),
              Counts_Reference => new Counts);
   exception
      when others =>
         declare
            Temp : Element_Access := Source;
         begin
            Deallocate(Temp);
            raise;
         end;
   end Shared_Access;

   function Shared_Access
      (Source : in Advanced_Smart_Access.Weak_Access)
       return Advanced_Smart_Access.Shared_Access
   is
      use Access_Types.Reference_Counts;
   begin
      if Source.Counts_Reference = null then
         return (Ada.Finalization.Controlled with
                 Item_Reference   => null,
                 Counts_Reference => null);
      else
         if Pre_Increment(Source.Counts_Reference.Strong) = 0 then
            return (Ada.Finalization.Controlled with
                    Item_Reference   => null,
                    Counts_Reference => null);
         else
            return (Ada.Finalization.Controlled with
                    Item_Reference   => Source.Item_Reference,
                    Counts_Reference => Source.Counts_Reference);
         end if;
      end if;
   end Shared_Access;

   function Shared_Access
      (Source : in out Advanced_Smart_Access.Unique_Access)
       return Advanced_Smart_Access.Shared_Access
   is
   begin
      if Source.Item_Reference = null then
         return (Ada.Finalization.Controlled with
                 Item_Reference   => null,
                 Counts_Reference => null);
      else
         return Target : Advanced_Smart_Access.Shared_Access do
            Target.Counts_Reference := new Counts;
            Target.Item_Reference   := Source.Item_Reference;
            Source.Item_Reference   := null;
         end return;
      end if;
   end Shared_Access;

   procedure Weak_Access
      (Target : in out Advanced_Smart_Access.Weak_Access;
       Source : in     Advanced_Smart_Access.Weak_Access)
   is
   begin
      Target.Finalize;
      if Source.Counts_Reference /= null then
         Reference_Counts.Increment(Source.Counts_Reference.Weak);
         Target.Item_Reference   := Source.Item_Reference;
         Target.Counts_Reference := Source.Counts_Reference;
      end if;
   end Weak_Access;

   procedure Weak_Access
      (Target : in out Advanced_Smart_Access.Weak_Access;
       Source : in     Advanced_Smart_Access.Shared_Access)
   is
   begin
      Target.Finalize;
      if Source.Counts_Reference /= null then
         Reference_Counts.Increment(Source.Counts_Reference.Weak);
         Target.Item_Reference   := Source.Item_Reference;
         Target.Counts_Reference := Source.Counts_Reference;
      end if;
   end Weak_Access;

   function Weak_Access
      (Source : in Advanced_Smart_Access.Shared_Access)
       return Advanced_Smart_Access.Weak_Access
   is
   begin
      if Source.Counts_Reference = null then
         return (Ada.Finalization.Controlled with
                 Item_Reference   => null,
                 Counts_Reference => null);
      else
         Reference_Counts.Increment(Source.Counts_Reference.Weak);
         return (Ada.Finalization.Controlled with
                 Item_Reference   => Source.Item_Reference,
                 Counts_Reference => Source.Counts_Reference);
      end if;
   end Weak_Access;

   procedure Unique_Access
      (Target : in out Advanced_Smart_Access.Unique_Access;
       Source : in     not null Element_Access)
   is
   begin
      Target.Finalize;
      Target.Item_Reference := Convert(Source);
   end Unique_Access;

   procedure Unique_Access
      (Target : in out Advanced_Smart_Access.Unique_Access;
       Source : in out Advanced_Smart_Access.Unique_Access)
   is
   begin
      Target.Finalize;
      Target.Item_Reference := Source.Item_Reference;
      Source.Item_Reference := null;
   end Unique_Access;

   function Unique_Access
      (Source : in not null Element_Access)
       return Advanced_Smart_Access.Unique_Access
   is
   begin
      return (Ada.Finalization.Limited_Controlled with
                 Item_Reference => Convert(Source));
   end Unique_Access;

   function Unique_Access
      (Source : in out Advanced_Smart_Access.Unique_Access)
       return Advanced_Smart_Access.Unique_Access
   is
   begin
      if Source.Item_Reference = null then
         return (Ada.Finalization.Limited_Controlled with
                 Item_Reference => null);
      else
         return Target : Advanced_Smart_Access.Unique_Access do
            Target.Item_Reference := Source.Item_Reference;
            Source.Item_Reference := null;
         end return;
      end if;
   end Unique_Access;
   
begin
   
   -- This validates that the same Traits package is used for both packages
   -- to ensure that the unchecked conversions are correct
   if Traits.Get_Package_ID /= Advanced_Smart_Access.Traits.Get_Package_ID then
      raise Program_Error with "Incompatable traits package used in Make";
   end if;
   
   -- Do the accessibility check first.
   Advanced_Smart_Access.Deallocate := Deallocate_Check;
   
   -- Assign the actual deallocation routine
   Advanced_Smart_Access.Deallocate := Convert
      (if Custom_Deallocator = null then
          Deallocate_Wrapper'Access
       else
          Custom_Deallocator);

end Bullfrog.Access_Types.Advanced_Smart_Access.Make;

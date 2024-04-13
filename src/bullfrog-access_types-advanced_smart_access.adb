------------------------------------------------------------------------------
--                      Copyright (C) 2016 - Present                        --
--                            Jeremiah Breeden                              --
--                           All Rights Reserved                            --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file might be   --
--  covered by the GNU Public License.                                      --
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
            Finalize(Self.Item_Reference);
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
         Finalize(Self.Item_Reference);
         Self.Item_Reference := null;
      end if;
   end Finalize;



   package body Make is

      procedure Shared_Access
         (Target : in out Advanced_Smart_Access.Shared_Access;
          Source : in     not null Element_Access)
      is
      begin
         Target.Finalize;
         Target.Counts_Reference := new Counts;
         Target.Item_Reference   := Source;
      exception
         when others =>
            declare
               Temp : Element_Access := Source;
            begin
               Finalize(Temp);
               raise;
            end;
      end Shared_Access;

      procedure Shared_Access
         (Target : in out Advanced_Smart_Access.Shared_Access;
          Source : in     Advanced_Smart_Access.Shared_Access)
      is
      begin
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
                 Item_Reference   => Source,
                 Counts_Reference => new Counts);
      exception
         when others =>
            declare
               Temp : Element_Access := Source;
            begin
               Finalize(Temp);
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
         Target.Item_Reference := Source;
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
                 Item_Reference => Source);
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

   end Make;



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

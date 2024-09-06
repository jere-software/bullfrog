------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

package body Bullfrog.Containers.Limited_Indefinite_Holders is

   -- Bring in operators
   use type Shared_Access;

   function Is_Empty(Self : Holder) return Boolean is
      (Self.Object.Is_Null);

   function Not_Empty(Self : Holder) return Boolean is
      (Self.Object.Not_Null);

   function "="(L,R : Holder) return Boolean is (L.Object = R.Object);

   -- Local package rename
   package Utilities renames Smart_Access.Utilities;

   -- Local package rename
   package Make renames Smart_Access.Make;

   function To_Holder
      (Element : not null access function return Element_Type) 
       return Holder
   is (Object => Make.Shared_Access(new Element_Type'(Element.all)));

   function Empty return Holder is (others => <>);

   procedure Clear(Self : in out Holder) is 
   begin
      Self.Object.Set_Null;
   end Clear;

   procedure Replace_Element
      (Self    : in out Holder; 
       Element : not null access function return Element_Type)
   is begin
      Make.Shared_Access
         (Target => Self.Object,
          Source => new Element_Type'(Element.all));
   end Replace_Element;

   procedure Query_Element
      (Self    : Holder;
       Process : not null access procedure(Element : Element_Type))
   is begin
      Process(Self.Unchecked_Access.all);
   end Query_Element;
   
   procedure Update_Element
      (Self    : in out Holder;
       Process : not null access procedure(Element : in out Element_Type))
   is begin
      Process(Self.Unchecked_Access.all);
   end Update_Element;

   procedure Move(Target, Source : in out Holder) is
   begin
      Target.Object.Move(Source.Object);
   end Move;

   procedure Swap(Target, Source : in out Holder) is
   begin
      Target.Object.Swap(Source.Object);
   end Swap;

   -- Safety enabled type
   subtype Enabled_Persistance is Base_Persistence(Disable => False);

   -- Constructor for a persistance object that has safety turned on.
   -- This is a slight optimization over direct assignment since
   -- limited types are build in place
   function Shared_Object(Self : Holder) return Enabled_Persistance is
   begin
      return Result : Enabled_Persistance do
         Make.Shared_Access
            (Target => Result.Object,
             Source => Self.Object);
      end return;
   end Shared_Object;

   function Constant_Reference(Self : Holder) return Constant_Reference_Type is 
      (if Unchecked_References then 
         (Element => Self.Unchecked_Access, 
          Shared  => (Disable => True))
       else
         (Element => Self.Unchecked_Access, 
          Shared  => Shared_Object(Self)));

   function Reference(Self : in out Holder) return Reference_Type is 
      (if Unchecked_References then 
         (Element => Self.Unchecked_Access, 
          Shared  => (Disable => True))
       else
         (Element => Self.Unchecked_Access, 
          Shared  => Shared_Object(Self)));

   function Unchecked_Access(Self : Holder) return Accessor is 
      (Accessor(Utilities.Raw_Access(Self.Object)));

end Bullfrog.Containers.Limited_Indefinite_Holders;
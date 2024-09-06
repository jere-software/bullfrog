------------------------------------------------------------------------------
-- Copyright (C) 2016 - 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.
------------------------------------------------------------------------------

private with Bullfrog.Access_Types.Smart_Access;

-- Provides a holder for limited indefinite types
generic

   -- Element type to hold
   type Element_Type(<>) is limited private;

   -- Indicates if reference types are checked or
   -- unchecked.  Default is checked.
   Unchecked_References : Boolean := False;

package Bullfrog.Containers.Limited_Indefinite_Holders is

   -- Primary holder type
   type Holder is tagged limited private;

   -- Indicates if the holder is empty
   function Is_Empty(Self : Holder) return Boolean;

   -- Indicates if the holder is not empty
   function Not_Empty(Self : Holder) return Boolean;

   -- Equality
   function "="(L,R : Holder) return Boolean;

   -- Constructing function
   function To_Holder
      (Element : not null access function return Element_Type) 
       return Holder;

   -- Creates an empty holder
   function Empty return Holder;

   -- An empty holder
   Empty_Holder : constant Holder;

   -- Removes element from holder
   procedure Clear(Self : in out Holder);

   -- Replaces the existing element with a new one
   procedure Replace_Element
      (Self    : in out Holder; 
       Element : not null access function return Element_Type);

   -- Allows one to directly view an element
   procedure Query_Element
      (Self    : Holder;
       Process : not null access procedure(Element : Element_Type))
   with Pre => Self.Not_Empty;

   -- Allows one to directly update an element
   procedure Update_Element
      (Self    : in out Holder;
       Process : not null access procedure(Element : in out Element_Type))
   with Pre => Self.Not_Empty;

   -- Moves one holder into another, clearing any existing holder
   procedure Move(Target, Source : in out Holder);

   -- Swaps two holders around
   procedure Swap(Target, Source : in out Holder);

   -----------------------------------------------
   -- Element References
   -----------------------------------------------

   -- Returns a "constant reference" to the object.  By default this
   -- ensures the reference is safe to use.  Setting Unchecked_References
   -- to True can improve performance but allows the reference to
   -- potentially dangle
   type Constant_Reference_Type
      (Element : not null access constant Element_Type)
   is limited private
      with Implicit_Dereference => Element;

   -- Returns a "variable reference" to the object.  By default this
   -- ensures the reference is safe to use.  Setting Unchecked_References
   -- to True can improve performance but allows the reference to
   -- potentially dangle
   type Reference_Type
      (Element : not null access Element_Type)
   is limited private
      with Implicit_Dereference => Element;

   -- Returns a constant reference to the object held by
   -- the holder
   function Constant_Reference(Self : Holder) return Constant_Reference_Type
      with Pre => Self.Not_Empty;

   -- Returns a variable reference to the object held by
   -- the holder
   function Reference(Self : in out Holder) return Reference_Type
      with Pre => Self.Not_Empty;

   -----------------------------------------------
   -- Advanced Utility
   -----------------------------------------------

   -- Special access type for unchecked programming
   type Accessor is not null access all Element_Type 
      with Storage_Size => 0;

   -- Returns unchecked local access to the element.  This
   -- is meant purely for advanced users
   function Unchecked_Access(Self : Holder) return Accessor
      with Pre => Self.Not_Empty;

private

   -- Package for Shared_Access type.  This does not
   -- need to be thread safe at all
   package Smart_Access is new Access_Types.Smart_Access
      (Element_Type     => Element_Type,
       Atomic_Increment => False);

   -- Local rename of Shared_Access type
   subtype Shared_Access is Smart_Access.Shared_Access;

   type Holder is tagged limited record

      -- Use a shared access type to hold the item
      Object : Shared_Access;  -- defaults to null

   end record;

   Empty_Holder : constant Holder := (others => <>);

   -- This ensures that refences always point to 
   -- valid memory.  They actually contain a shared
   -- access objet to the element so it cannot be deleted 
   -- by the holder while the reference exists
   type Base_Persistence(Disable : Boolean) is limited record
      case Disable is
         when False => Object : Shared_Access;
         when True  => null;
      end case;
   end record;

   -- Local subtype based on the package settings
   subtype Persistence is Base_Persistence(Unchecked_References);

   type Constant_Reference_Type
      (Element : not null access constant Element_Type)
   is limited record
      Shared : Persistence;
   end record;

   type Reference_Type
      (Element : not null access Element_Type)
   is limited record
      Shared : Persistence;
   end record;

end Bullfrog.Containers.Limited_Indefinite_Holders;
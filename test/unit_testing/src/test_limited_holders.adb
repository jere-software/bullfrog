with Bullfrog.Containers.Limited_Indefinite_Holders;
with Bullfrog.Containers.Limited_Definite_Holders;

with Ada.Finalization; use Ada.Finalization;
with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Limited_Holders is 

   package UUT_Base is
      type UUT is new Limited_Controlled with record
         Value : Integer := 0;
      end record;
      overriding procedure Initialize(Self : in out UUT);
      overriding procedure Finalize(Self : in out UUT);
      function Make return UUT;
   end UUT_Base;

   package body UUT_Base is
      procedure Initialize(Self : in out UUT) is 
      begin
         Put_Line("Initialize " & Self'Address'Image);
      end Initialize;

      procedure Finalize(Self : in out UUT) is 
      begin
         Put_Line("Finalize   " & Self'Address'Image & ", Value =>" & Self.Value'Image);
      end Finalize;

      function Make return UUT is (Limited_Controlled with Value => 12345);
   end UUT_Base; use UUT_Base;
   
   package Definite_Holders is new Bullfrog.Containers.Limited_Definite_Holders(UUT);

   Holder : Definite_Holders.Holder := Definite_Holders.Default_Element;

   subtype Reference_Type is Definite_Holders.Core.Reference_Type;

   Ref : Reference_Type := Holder.Reference;

   procedure Test_Dangling is
      Other : Definite_Holders.Holder := Definite_Holders.To_Holder(Make'Access);

      function Lifetime return Reference_Type is
         Bad_Guy : Definite_Holders.Holder := Definite_Holders.Default_Element;
      begin
         Put("Testing holder lifetime: ... ");
         Bad_Guy.Reference.Value := 67890;
         return Bad_Guy.Reference;
      end Lifetime;
      
   begin

      begin
         Put("Holder.Clear: ... ");  
         Holder.Clear;
         Put_Line("Failed");
      exception
         when Program_Error => Put_Line("Passed");
      end;

      begin
         Put("Holder.Replace_Element: ... "); 
         Holder.Replace_Element(Make'Access);
         Put_Line("Failed");
      exception
         when Program_Error => Put_Line("Passed");
      end;

      begin
         Put("Holder.Move: ... ");
         Holder.Move(Other);
         Put_Line("Failed");
      exception
         when Program_Error => Put_Line("Passed");
      end;

      begin
         Put("Other.Move: ... ");
         Other.Move(Holder);
         Put_Line("Failed");
      exception
         when Program_Error => Put_Line("Passed");
      end;

      begin declare
         Ref : Reference_Type := Lifetime;
      begin
         Put_Line("Failed");
      end; exception
         when Program_Error => Put_Line("Passed");
      end;

   end Test_Dangling;

   procedure Test_Null is 
      Null_Holder : Definite_Holders.Holder := Definite_Holders.Empty;
      procedure Query(Value : UUT) is
      begin
         Put_Line("Failed");
      end Query;
      procedure Update(Value : in out UUT) is
      begin
         Value.Value := 1000;
         Put_Line("Failed");
      end Update;
   begin

      Put("Testing Is_Empty: ... ");
      if Null_Holder.Is_Empty then
         Put_Line("Passed");
      else 
         Put_Line("Failed");
      end if;

      Put("Testing Not_Empty: ... ");
      if Null_Holder.Not_Empty then
         Put_Line("Failed");
      else 
         Put_Line("Passed");
      end if;

      Put("Testing Unchecked_Access when null: ... ");
      begin declare
         Thing : constant Definite_Holders.Core.Accessor := Null_Holder.Unchecked_Access;
      begin
         Put_Line("Failed");
      end; exception
         when others => Put_Line("Passed");

      end;Put("Testing Reference when null: ... ");
      begin declare
         Thing : constant Reference_Type := Null_Holder.Reference;
      begin
         Put_Line("Failed");
      end; exception
         when others => Put_Line("Passed");
      end;

      begin
         Put("Testing Query_Element when null: ... ");
         Null_Holder.Query_Element(Query'Access);
      exception
         when others => Put_Line("Passed");
      end;

      begin
         Put("Testing Update_Element when null: ... ");
         Null_Holder.Update_Element(Update'Access);
      exception
         when others => Put_Line("Passed");
      end;
      
   end Test_Null;

begin 


   Put_Line(Holder.Reference.Value'Image);
   Ref.Value := 100;
   Put_Line(Holder.Reference.Value'Image);

   Test_Dangling;
   Test_Null;

end Test_Limited_Holders;
with Ada.Text_IO; use Ada.Text_IO;
with Bullfrog.Synchronization.Sync_Wrappers;
with Ada.Exceptions;

procedure Test_Sync_Wrappers is
   use Bullfrog.Synchronization;

   package Int_Wrappers is new Sync_Wrappers(Integer);

   function Make_Impl(Value : Integer) return Integer is (Value);

   function Make is new Int_Wrappers.Make
      (Item_Type   => Integer,
       Constructor => Make_Impl);

   v1 : Int_Wrappers.Wrapper := Make(50);

   procedure Print(Wrapper : in out Int_Wrappers.Wrapper) is
   begin
      Put_Line("Value is " & Integer'Image(Wrapper.Lock));
   end Print;

begin

   Put_Line("Starting Test");
   -- Test general operations
   Print(v1);
   v1.Lock := 23;
   print(v1);

   -- Test bad initialization
   declare
      i : aliased Integer := 75;
      g : Int_Wrappers.Scoped_Lock(I'Access);
   begin
      Put_Line("Failed Initialization Test for Guard");
   end;

exception
   when Error : Unsynchronized_Data =>
      Put_Line(Ada.Exceptions.Exception_Information(Error));
      Put_Line("Passed Initialization Test for Guard");
end Test_Sync_Wrappers;

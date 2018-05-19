with Ada.Text_IO; use Ada.Text_IO;
with Bullfrog.Synchronization.Sync_Wrappers;
with Ada.Exceptions;

procedure Test_Sync_Wrappers is
   use Bullfrog.Synchronization;

   package Int_Wrappers is new Sync_Wrappers(Integer);

   function Make(Value : Integer) return Integer is (Value);

   package Make_Int_Wrappers is new Int_Wrappers.Constructors
      (Item_Type   => Integer,
       Constructor => Make);
   function Make
      (Value : Integer)
       return Int_Wrappers.Wrapper
       renames Make_Int_Wrappers.Make;

   v : Int_Wrappers.Wrapper := Make(50);
begin
   -- Test general operations
   Put_Line("Value is " & Integer'Image(v.Lock));
   v.Lock := 23;
   Put_Line("Value is " & Integer'Image(v.Lock));

   -- Test recursion
   declare
      g : Int_Wrappers.Scoped_Lock := v.Lock;
   begin
      g := 30;
      Put_Line("Value is " & Integer'Image(g));
      v.Lock := 99;
      Put_Line("Value is " & Integer'Image(v.Lock));
   end;

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

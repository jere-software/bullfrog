with Bullfrog.Access_Types.Smart_Access;
with Bullfrog.Access_Types.Smart_Access.Debug;

package Bullfrog.Tests.Smart_Access is

   type Test_Type;

   type Test_Access is access all Test_Type;
   type Test_Class_Access is access all Test_Type'Class;

   procedure Finalize(Memory : in out Test_Access);
   procedure Finalize(Memory : in out Test_Class_Access) is null;

   package TT is new Bullfrog.Access_Types.Smart_Access
      (Item_Type        => Test_Type,
       Item_Access      => Test_Access,
       Finalize         => Finalize,
       Atomic_Increment => False);

   package TC is new Bullfrog.Access_Types.Smart_Access
      (Item_Type        => Test_Type'Class,
       Item_Access      => Test_Class_Access,
       Finalize         => Finalize,
       Atomic_Increment => False);

   type Test_Type(Value : Integer) is tagged limited record
      Other : Integer := 0;
   end record;

   package TT_Debug is new TT.Debug;

   function Make_Test_Type return Test_Access;


   function To_String(Obj : TT.Shared_Access) return String;
   function To_String(Obj : TT.Weak_Access)   return String;
   function To_String(Obj : TT.Unique_Access) return String;

end Bullfrog.Tests.Smart_Access;

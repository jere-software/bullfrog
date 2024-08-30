with Bullfrog.Access_Types.Advanced_Smart_Access_Traits;
with Bullfrog.Access_Types.Advanced_Smart_Access;
with Bullfrog.Access_Types.Advanced_Smart_Access.Make;
with Bullfrog.Access_Types.Advanced_Smart_Access.Debug;

package Bullfrog.Tests.Advanced_Smart_Access is

   type Test_Type;
   type Test_Access is access Test_Type;

   package TT_Traits is new Bullfrog.Access_Types.Advanced_Smart_Access_Traits
      (Element_Type     => Test_Type,
       Atomic_Increment => False);

   package TT is new Bullfrog.Access_Types.Advanced_Smart_Access
      (Traits => TT_Traits);

   type Test_Class_Access is access all Test_Type'Class;

   package TC_Traits is new Bullfrog.Access_Types.Advanced_Smart_Access_Traits
      (Element_Type     => Test_Type'Class,
       Atomic_Increment => False);

   package TC is new Bullfrog.Access_Types.Advanced_Smart_Access
      (Traits => TT_Traits);

   type Test_Type(Value : Integer) is tagged limited record
      Other : Integer := 0;
   end record;

   package TT_Make is new TT.Make
      (Element_Type   => Test_Type,
       Element_Access => Test_Access,
       Traits         => TT_Traits);

   package TT_Debug is new TT.Debug;

   function Make_Test_Type return Test_Access;


   function To_String(Obj : TT.Shared_Access) return String;
   function To_String(Obj : TT.Weak_Access)   return String;
   function To_String(Obj : TT.Unique_Access) return String;

end Bullfrog.Tests.Advanced_Smart_Access;

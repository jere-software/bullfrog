with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with System.Address_Image;
with System.Storage_Elements; use System.Storage_Elements;

package body Bullfrog.Tests.Custom_Smart_Access is

   ID : Integer := 0;

   function Make_Test_Type return Test_Access is
      Temp : Test_Access := new Test_Type(ID);
   begin
      ID := ID + 1;
      Put_Line("Initialize(Test_Access): "
               & Integer_Address'Image(To_Integer(Temp.all'Address))
               & " => "
               & Integer'Image(Temp.Value));
      return Temp;
   end Make_Test_Type;

   procedure Finalize(Memory : in out Test_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
         (Object => Test_Type,
          Name   => Test_Access);
   begin
      Put_Line("Finalize(Test_Access): "
               & Integer_Address'Image(To_Integer(Memory.all'Address))
               & " => "
               & Integer'Image(Memory.Value));
      Free(Memory);
   end Finalize;

   function To_String(Obj : TT.Shared_Access) return String is
      Temp : String := (if
                           Obj.Is_Null
                        then
                           ""
                        else
                           Integer'Image(Obj.Reference.Value));
   begin
      return ("Shared: " & TT_Debug.To_String(Obj) & " => " & Temp);
   end To_String;
   function To_String(Obj : TT.Weak_Access)   return String is
      Shared : TT.Shared_Access := TT_Make.Shared_Access(Obj);
      Temp   : String           := (if
                                       Shared.Is_Null
                                    then
                                       ""
                                    else
                                       Integer'Image(Shared.Reference.Value));
   begin
      return ("Weak:   " & TT_Debug.To_String(Obj) & " => " & Temp);
   end To_String;
   function To_String(Obj : TT.Unique_Access) return String is
      Temp : String := (if
                           Obj.Is_Null
                        then
                           ""
                        else
                           Integer'Image(Obj.Reference.Value));
   begin
      return ("Unique: " & TT_Debug.To_String(Obj) & " => " & Temp);
   end To_String;

end Bullfrog.Tests.Custom_Smart_Access;

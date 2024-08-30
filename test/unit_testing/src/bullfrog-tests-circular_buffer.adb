with Ada.Text_IO;
with Bullfrog.Containers.Circular_Buffer.Debug;

package body Bullfrog.Tests.Circular_Buffer is

   Data_Size : constant := 5;
   package Integer_Buffer is new Bullfrog.Containers.Circular_Buffer(Integer);
   package Integer_Buffer_Debug is new Integer_Buffer.Debug;
   type Int_Array_Type is array(Integer range <>) of Integer;

   Ints_Array  : Int_Array_Type(1..Data_Size);
   Ints_Buffer : Integer_Buffer.Buffer(Data_Size);

   subtype Buffer_Type is Integer_Buffer.Buffer;

   use type Integer_Buffer.Buffer_Index;

   procedure Debug_Print
      (Buffer : Buffer_Type;
       Elements_Per_Row : Integer := -1)
   is
      Full_Str  : constant String :=
         (if Integer_Buffer.Is_Full(Buffer)  then
                "F"
          else
             "N");
      Empty_Str : constant String :=
         (if Integer_Buffer.Is_Empty(Buffer) then
                "E"
          else
             "N");
      Count : constant Integer_Buffer.Buffer_Index :=
         (if Elements_Per_Row <= 0 then
             Buffer.Max_Size
          else
             Integer_Buffer.Buffer_Index(Elements_Per_Row)-1);
      Index : Integer_Buffer.Buffer_Index := 0;

      function Print_Element_State return String is
         Empty_Buffer : constant String := "* ";
         Put_Element  : constant String := "P ";
         Get_Element  : constant String := "G ";
         No_Element   : constant String := "x ";
         Some_Element : constant String := "e ";

         subtype Status_Type is Integer_Buffer_Debug.Element_Status;
         Element_States : constant array(Status_Type) of String(1..2) :=
            (Integer_Buffer_Debug.Both_Indexes  => "B ",
             Integer_Buffer_Debug.Put_Index     => "P ",
             Integer_Buffer_Debug.Get_Index     => "G ",
             Integer_Buffer_Debug.Some_Element  => "e ",
             Integer_Buffer_Debug.No_Element    => "x ",
             Integer_Buffer_Debug.Out_Of_Bounds => "* ");
      begin
         return Element_States
            (Integer_Buffer_Debug.Current_Element_Status(Buffer,Index));
      end Print_Element_State;


      package Index_IO is new Ada.Text_IO.Modular_IO
         (Integer_Buffer.Buffer_Index);
      Biggest_Num_String : constant String
         := Integer_Buffer.Buffer_Index'Image(Buffer.Max_Size);
      Get_Str : String(1..Biggest_Num_String'Length) := (others => ' ');
      Put_Str : String(1..Biggest_Num_String'Length) := (others => ' ');
      Tabbed_Str : constant String(1 .. (Biggest_Num_String'Length*2 + 12))
         := (others => ' ');

   begin
      Index_IO.Put(Item  => Integer_Buffer_Debug.Get_Index(Buffer),
                   To    => Get_Str);
      Index_IO.Put(Item  => Integer_Buffer_Debug.Put_Index(Buffer),
                   To    => Put_Str);
      Ada.Text_IO.Put
         (Get_Str     & ", "
          & Put_Str   & ": "
          & Empty_Str & ", "
          & Full_Str  & " => ");

      if Elements_Per_Row /= 0 then
         while Index <= Buffer.Max_Size loop
            for I in 0..Count loop
               exit when Index > Buffer.Max_Size;
               Ada.Text_IO.Put(Print_Element_State);
               Index := Index + 1;
            end loop;
            Ada.Text_IO.New_Line;
            if Index <= Buffer.Max_Size then
               Ada.Text_IO.Put(Tabbed_Str);
            end if;
         end loop;
         null;
      else
         Ada.Text_IO.New_Line;
      end if;
      null;
   end Debug_Print;



   procedure Init_Array is
   begin
      for I in Ints_Array'Range loop
         Ints_Array(I) := I;
      end loop;
   end Init_Array;

   procedure Test_Empty is
   begin
      Ada.Text_IO.Put("Empty Testing...");
      if Integer_Buffer.Is_Empty(Ints_Buffer) = True then
         Ada.Text_IO.Put("Passed ");
      else
         Ada.Text_IO.Put("Failed ");
      end if;
      if Integer_Buffer.Not_Empty(Ints_Buffer) = False then
         Ada.Text_IO.Put("Passed ");
      else
         Ada.Text_IO.Put("Failed ");
      end if;

      if Integer_Buffer.Is_Full(Ints_Buffer) = False then
         Ada.Text_IO.Put("Passed ");
      else
         Ada.Text_IO.Put("Failed ");
      end if;
      if Integer_Buffer.Not_Full(Ints_Buffer) = True then
         Ada.Text_IO.Put("Passed ");
      else
         Ada.Text_IO.Put("Failed ");
      end if;
      declare
         Value : Integer;
      begin
         if Integer_Buffer.Consumer.Get(Ints_Buffer,Value) then
            Ada.Text_IO.Put("Failed ");
         else
            Ada.Text_IO.Put("Passed ");
         end if;
         Value := Integer_Buffer.Consumer.Get(Ints_Buffer);
         Ada.Text_IO.Put("Failed ");
      exception
         when others => Ada.Text_IO.Put("Passed ");
      end;
      declare
         Value : Integer;
      begin
         Integer_Buffer.Consumer.Get(Ints_Buffer,Value);
         Ada.Text_IO.Put_Line("Failed");
      exception
         when others => Ada.Text_IO.Put_Line("Passed");
      end;
   end Test_Empty;
   procedure Test_Full is
   begin
      Ada.Text_IO.Put("Full  Testing...");
      if Integer_Buffer.Is_Empty(Ints_Buffer) = False then
         Ada.Text_IO.Put("Passed ");
      else
         Ada.Text_IO.Put("Failed ");
      end if;
      if Integer_Buffer.Not_Empty(Ints_Buffer) = True then
         Ada.Text_IO.Put("Passed ");
      else
         Ada.Text_IO.Put("Failed ");
      end if;

      if Integer_Buffer.Is_Full(Ints_Buffer) = True then
         Ada.Text_IO.Put("Passed ");
      else
         Ada.Text_IO.Put("Failed ");
      end if;
      if Integer_Buffer.Not_Full(Ints_Buffer) = False then
         Ada.Text_IO.Put("Passed ");
      else
         Ada.Text_IO.Put("Failed ");
      end if;
      declare
         Value : Integer := 100;
      begin
         if Integer_Buffer.Producer.Put(Ints_Buffer,Value) then
            Ada.Text_IO.Put("Failed ");
         else
            Ada.Text_IO.Put("Passed ");
         end if;
         Integer_Buffer.Producer.Put(Ints_Buffer,Value);
         Ada.Text_IO.Put_Line("Failed");
      exception
         when others => Ada.Text_IO.Put_Line("Passed");
      end;
   end Test_Full;

   procedure Run_Test is
   begin
      Init_Array;

      Test_Empty;
      Debug_Print(Ints_Buffer);


      for E of Ints_Array loop
         Integer_Buffer.Producer.Put(Ints_Buffer,E);
         Debug_Print(Ints_Buffer);
      end loop;
      Test_Full;


      for E of Ints_Array loop
         if Integer_Buffer.Consumer.Get(Ints_Buffer) = E then
            Debug_Print(Ints_Buffer);
         else
            Ada.Text_IO.Put_Line("Failed");
            exit;
         end if;
      end loop;
      Test_Empty;


      for E of Ints_Array loop
         Integer_Buffer.Producer.Put(Ints_Buffer,E);
         Debug_Print(Ints_Buffer);
      end loop;
      Test_Full;


      for E of Ints_Array loop
         if Integer_Buffer.Consumer.Get(Ints_Buffer) = E then
            Debug_Print(Ints_Buffer);
         else
            Ada.Text_IO.Put_Line("Failed");
            exit;
         end if;
      end loop;
      Test_Empty;


      for E of Ints_Array loop
         Integer_Buffer.Producer.Put(Ints_Buffer,E);
         Debug_Print(Ints_Buffer);
      end loop;
      Test_Full;


      Integer_Buffer.Consumer.Reset(Ints_Buffer);
      Test_Empty;
   end Run_Test;
end Bullfrog.Tests.Circular_Buffer;

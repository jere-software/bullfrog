with Bullfrog.Tests.Smart_Access;  use Bullfrog.Tests.Smart_Access;
with Ada.Text_IO; use Ada.Text_IO;
with Bullfrog.Access_Types.Smart_Access.Debug;
with Bullfrog.Tests.Smart_Access_Node;

package body Bullfrog.Tests.Smart_Access.Main is
   procedure Run is
      -- Test initialization
      tts1 : aliased TT.Shared_Access := TT.Make.Shared_Access(Make_Test_Type);
      ttw1 : aliased TT.Weak_Access   := TT.Make.Weak_Access(tts1);
      ttu1 : aliased TT.Unique_Access := TT.Make.Unique_Access(Make_Test_Type);
      ttu2 : aliased TT.Unique_Access := TT.Make.Unique_Access(Make_Test_Type);
      ttu3 : aliased TT.Unique_Access := TT.Make.Unique_Access(Make_Test_Type);
      ttu4 : aliased TT.Unique_Access := TT.Make.Unique_Access(Make_Test_Type);

      tts2 : aliased TT.Shared_Access := tts1;
      tts3 : aliased TT.Shared_Access := TT.Make.Shared_Access(ttw1);
      tts4 : aliased TT.Shared_Access := TT.Make.Shared_Access(ttu1);
      tts5 : aliased TT.Shared_Access;
      tts6 : aliased TT.Shared_Access;
      tts7 : aliased TT.Shared_Access;
      tts8 : aliased TT.Shared_Access;

      ttw2 : aliased TT.Weak_Access := ttw1;
      ttw3 : aliased TT.Weak_Access;
      ttw4 : aliased TT.Weak_Access;

      ttu5 : aliased TT.Unique_Access := TT.Make.Unique_Access(ttu2);
      ttu6 : aliased TT.Unique_Access;
      ttu7 : aliased TT.Unique_Access;

      -- Create iterative arrays for ease of testing
      tts : constant array(Integer range <>) of access TT.Shared_Access :=
         (1 => tts1'Access,
          2 => tts2'Access,
          3 => tts3'Access,
          4 => tts4'Access,
          5 => tts5'Access,
          6 => tts6'Access,
          7 => tts7'Access,
          8 => tts8'Access);

      ttw : constant array(Integer range <>) of access TT.Weak_Access :=
         (1 => ttw1'Access,
          2 => ttw2'Access,
          3 => ttw3'Access,
          4 => ttw4'Access);

      ttu : constant array(Integer range <>) of access TT.Unique_Access :=
         (1 => ttu1'Access,
          2 => ttu2'Access,
          3 => ttu3'Access,
          4 => ttu4'Access,
          5 => ttu5'Access,
          6 => ttu6'Access,
          7 => ttu7'Access);

   begin

      -- Test runtime construction
      TT.Make.Shared_Access
         (Target => tts5,
          Source => Make_Test_Type);
      TT.Make.Shared_Access
         (Target => tts6,
          Source => tts1);
      TT.Make.Shared_Access
         (Target => tts7,
          Source => ttw1);
      TT.Make.Shared_Access
         (Target => tts8,
          Source => ttu3);

      TT.Make.Weak_Access
         (Target => ttw3,
          Source => ttw1);
      TT.Make.Weak_Access
         (Target => ttw4,
          Source => tts1);

      TT.Make.Unique_Access
         (Target => ttu6,
          Source => Make_Test_Type);
      TT.Make.Unique_Access
         (Target => ttu7,
          Source => ttu4);

      -- Print current states
      for Index in tts'Range loop
         Put_Line(To_String(tts(Index).all));
      end loop;
      for Index in ttw'Range loop
         Put_Line(To_String(ttw(Index).all));
      end loop;
      for Index in ttu'Range loop
         Put_Line(To_String(ttu(Index).all));
      end loop;
   end Run;
end Bullfrog.Tests.Smart_Access.Main;

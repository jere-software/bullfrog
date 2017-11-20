with Bullfrog.Tests.Smart_Access.Main;
with Bullfrog.Tests.Smart_Access_Node;

procedure Test_Smart_Access is
   Tree : Bullfrog.Tests.Smart_Access_Node.Tree;
begin
   for Index in 10 .. 13 loop
      Tree.Insert(Index);
   end loop;
   for Index in 18 .. 21 loop
      Tree.Insert(Index);
   end loop;
   for Index in 14 .. 16 loop
      Tree.Insert(Index);
   end loop;

   Tree.Print;
end Test_Smart_Access;

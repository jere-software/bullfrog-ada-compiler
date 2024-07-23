with Ada.Text_IO;
with Test_1;
with Test_2;
with Test_3;

-- Program entry point
procedure Main is
   Lexer : Test_1.Lexer;
begin
   Ada.Text_IO.Put_Line("---------------Starting Tests---------------");
   --Test_1.Tokenize_Directory(Lexer, "./src");
   --Test_1.Tokenize_Directory(Lexer, "../src");

   -- Test 2 runs Test 1 implicitly
   --Test_2.Tokenize_Directory(Lexer, "./src");
   --Test_2.Tokenize_Directory(Lexer, "../src");
   --Test_2.Tokenize_Directory;
   Test_3.Tokenize_String(Lexer, "' Hello World", False);
end Main;
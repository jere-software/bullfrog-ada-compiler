with Compiler.Lexer;
with Compiler.Strings;

package Test_3 is

   -- Local redeclaration
   subtype Lexer is Compiler.Lexer.Instance;
   subtype String is Compiler.Strings.String;

   type Test_Result is (Unexpected, Fail, Pass);

   -- Manual input test
   procedure Tokenize_String(Lexer : in out Test_3.Lexer; Value : String; Expect_Pass : Boolean := True);
   
   -- Command line functionality
   procedure Tokenize_String(Lexer : in out Test_3.Lexer);

end Test_3;
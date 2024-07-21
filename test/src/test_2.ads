with Compiler.Lexer;

package Test_2 is

   subtype Lexer is Compiler.Lexer.Instance;

   procedure Tokenize_File(Lexer : in out Test_2.Lexer; Filename : String);

   procedure Tokenize_Directory(Lexer : in out Test_2.Lexer; Directory_Name : String);

end Test_2;
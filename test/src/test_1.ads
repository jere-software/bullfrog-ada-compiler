with Compiler.Lexer;

package Test_1 is

   subtype Lexer is Compiler.Lexer.Instance;

   function  Tokenize_File(Lexer : in out Test_1.Lexer; Filename : String) return Boolean;
   procedure Tokenize_File(Lexer : in out Test_1.Lexer; Filename : String);

   procedure Tokenize_Directory(Lexer : in out Test_1.Lexer; Directory_Name : String);

end Test_1;
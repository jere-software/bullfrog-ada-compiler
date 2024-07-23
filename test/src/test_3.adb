with Object_Streams;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Test_2;

package body Test_3 is

   use Compiler.Strings;

   function Tokenize_String
      (Lexer       : in out Test_3.Lexer; 
       Value       :        String; 
       Expect_Pass :        Boolean := True) 
       return Boolean 
   is
      Stream : aliased Object_Streams.Stream;
   begin
      
      Text_IO.Put("Tokenizing String: " & Value & " => ");

      String'Write(Stream'Access, Value);

      Lexer.Run(Stream'Access);

      if Expect_Pass then
         Text_IO.Put_Line("PASS");
         return True;
      else
         Text_IO.Put_Line("FAIL - Expected a lexical error but found none");
         return False;
      end if;

   exception
      when E : Compiler.Lexer.Lexical_Error => 
         if Expect_Pass then
            Put_Line("FAIL - Lexical error not expected - " & Exception_Message(E));
            return False;
         else
            Text_IO.Put_Line("PASS - Lexical_Error - " & Exception_Message(E));
            return True;
         end if;
      when E : others => 
         Put_Line("FAIL - Unexpected exception - " & Exception_Name(E) & " - " & Exception_Message(E));
         return False;
   end Tokenize_String;

   procedure Tokenize_String(Lexer : in out Test_3.Lexer; Value : String; Expect_Pass : Boolean := True) is 
      Result : constant Boolean := Tokenize_String(Lexer, Value, Expect_Pass);
   begin
      null;
   end Tokenize_String;
   
   procedure Tokenize_String(Lexer : in out Test_3.Lexer) is
      use Ada.Command_Line;
      use Ada.Text_IO;
      procedure Help is
      begin
         Put_Line("Command usage error: ");
         Put_Line("   main [directory name]");
         Put_Line("   main [-p string]");
         Put_Line("   main [-f string]");
      end Help;
   begin
      case Argument_Count is
         when 0 =>
            null;
         when 1 =>
            Test_2.Tokenize_Directory(Lexer, Argument(1));
         when 2 =>
            if Argument(1) = "-p" then
               Tokenize_String(Lexer, Argument(2), True);
            elsif Argument(1) = "-f" then
               Tokenize_String(Lexer, Argument(2), False);
            else  
               Help;
            end if;
         when others =>
            Help;
      end case;
   end Tokenize_String;

end Test_3;
with Test_1;
with Interfaces.C;
with Compiler.Tokens;
with Compiler.Strings;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;

package body Test_2 is

   use Compiler.Strings.Text_IO;

   procedure System(Command : String) is
      procedure System(Command : Interfaces.C.char_array)
         with Import, Convention => C, External_Name => "system";
   begin
      System(Interfaces.C.To_C(Command));
   end System;

   -- Puts double quotes back in for each quote
   function Unescape_String(Value : Compiler.Strings.String) return Compiler.Strings.String is
      use Compiler.Strings;
   begin
      for Index in Value'Range loop
         if Value(Index) in Compiler.Strings.Quote then
            return Value(Value'First .. Index) & Quote & Unescape_String(Value(Index+1 .. Value'Last));
         end if;
      end loop;
      return Value;
   end Unescape_String;

   procedure Remove_File(Filename : String) is
   begin
      if Exists(Filename) then
         System("rm " & Filename);
      end if;
   end Remove_File;

   procedure Tokenize_File(Lexer : in out Test_2.Lexer; Filename : String) is 
      Line  : Positive := 1;
      First : Positive := 1;

      File : File_Type;

      function Output_Filename return String is
         ("./workspace/" & Simple_Name(Filename));
   begin
      Test_1.Tokenize_File(Lexer, Filename);

      Put("Validating File: " & Filename & " => ");
      if Lexer.Is_Running then
         Ada.Text_IO.Put_Line("FAIL - Tokenization step incomplete");
         return;
      end if;

      Remove_File(Output_Filename);

      Create 
         (File => File,
          Name => Output_Filename,
          Mode => Out_File);

      
      for Token of Lexer.All_Tokens.all loop
         --Compiler.Lexer.Debug(Token);

         -- Handle space and new lines
         if Token.Line > Line then
            New_Line(File, Positive_Count(Token.Line-Line));
            First := 1;
         end if;
         Line := Token.Line;
         if Token.First > First then
            Put(File, Compiler.Strings.Space);
         end if;
         First := Token.Last + 1;

         case Token.Kind is
            when Compiler.Tokens.Comment           => Put(File, "--" & Token.Value.Get);
            when Compiler.Tokens.String_Literal    => Put(File, """" & Unescape_String(Token.Value.Get) & """");
            when Compiler.Tokens.Character_Literal => Put(File, "'" & Token.Value.Get & "'");
            when others                            => Put(File, Token.Value.Get);
         end case;

      end loop;
      Close(File);

      System("diff -iw " & Filename  
         & " "   & Output_Filename 
         & " > " & Output_Filename & ".diff");

      if Size(Output_Filename & ".diff") in 0 then
         Put_Line("PASS");
      else
         Put_Line("FAIL - Diff file has content");
      end if;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line("FAIL - Unexpected exception - " & Exception_Name(E) & " - " & Exception_Message(E));
         if Is_Open(File) then
            Close(File);
         end if;
   end Tokenize_File;

   procedure Tokenize_Directory(Lexer : in out Test_2.Lexer; Directory_Name : String) is 
      procedure Tokenize_File(File : Directory_Entry_Type) is
      begin
         Tokenize_File(Lexer, Full_Name(File));
      end Tokenize_File;
   begin

      Put("Tokenizing Directory: " & Directory_Name & " => ");
      if not Exists(Directory_Name) or else Kind(Directory_Name) /= Directory then
         Ada.Text_IO.Put("FAIL - Invalid directory name");
      end if;
      New_Line;

      Search(Directory_Name, "", (Ordinary_File => True, others => False), Tokenize_File'Access);

   end Tokenize_Directory;

end Test_2;
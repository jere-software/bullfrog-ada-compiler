with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;

package body Test_1 is

   use type File_Kind;
   
   procedure Pass is
   begin
      Put_Line("PASS");
   end Pass;

   procedure Fail(Message : String) is
   begin
      Put_Line("FAIL - " & Message);
   end Fail;

   procedure Tokenize_File(Lexer : in out Test_1.Lexer; Filename : String) is 
   begin
      Put("Tokenizing File: " & Filename & " => ");
      if not Exists(Filename) or else Kind(Filename) /= Ordinary_File then
         Fail("Invalid filename");
      elsif  Filename(Filename'Last - 3 .. Filename'Last) /= ".ads"
         and Filename(Filename'Last - 3 .. Filename'Last) /= ".adb"
      then
         Fail("Invalid file type");
      end if;
      Lexer.Run(Filename);
      Pass;
   exception
      when E : Compiler.Lexer.Lexical_Error => Fail("Lexical error not expected - " & Exception_Message(E));
      when E : others => Fail("Unexpected exception - " & Exception_Name(E) & " - " & Exception_Message(E));
   end Tokenize_File;

   procedure Tokenize_Directory(Lexer : in out Test_1.Lexer; Directory_Name : String) is
      procedure Tokenize_File(File : Directory_Entry_Type) is
      begin
         Tokenize_File(Lexer, Full_Name(File));
      end Tokenize_File;
   begin

      Put("Tokenizing Directory: " & Directory_Name & " => ");
      if not Exists(Directory_Name) or else Kind(Directory_Name) /= Directory then
         Fail("Invalid directory name");
      end if;
      New_Line;

      Search(Directory_Name, "", (Ordinary_File => True, others => False), Tokenize_File'Access);

   end Tokenize_Directory;

end Test_1;
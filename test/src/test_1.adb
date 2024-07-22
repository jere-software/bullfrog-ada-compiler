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

   function Tokenize_File(Lexer : in out Test_1.Lexer; Filename : String) return Boolean is 
   begin
      Put("Tokenizing File: " & Filename & " => ");
      if not Exists(Filename) or else Kind(Filename) /= Ordinary_File then
         Fail("Invalid filename");
         return False;
      end if;
      Lexer.Run(Filename);
      Pass;
      return True;
   exception
      when E : Compiler.Lexer.Lexical_Error => 
         Fail("Lexical error not expected - " & Exception_Message(E));
         return False;
      when E : others => 
         Fail("Unexpected exception - " & Exception_Name(E) & " - " & Exception_Message(E));
         return False;
   end Tokenize_File;

   procedure Tokenize_File(Lexer : in out Test_1.Lexer; Filename : String) is
      Result : constant Boolean := Tokenize_File(Lexer, Filename);
      pragma Unreferenced(Result);
   begin
      null;
   end Tokenize_File;

   procedure Tokenize_Directory(Lexer : in out Test_1.Lexer; Directory_Name : String) is

      Test_Failure : exception;

      procedure Tokenize_File(File : Directory_Entry_Type) is
         Filename : constant String := Full_Name(File);
      begin
         if Kind(Filename) /= Ordinary_File 
            or else (         Filename(Filename'Last - 3 .. Filename'Last) /= ".ads"
                     and then Filename(Filename'Last - 3 .. Filename'Last) /= ".adb")
         then 
            Put_Line("Skipping " & Filename);
            return; -- Skip if not what we are looking for or done
         end if;

         if not Tokenize_File(Lexer, Filename) then
            raise Test_Failure;
         end if;
      end Tokenize_File;
      
   begin

      Put("Tokenizing Directory: " & Directory_Name & " => ");
      if not Exists(Directory_Name) or else Kind(Directory_Name) /= Directory then
         Fail("Invalid directory name");
      end if;
      New_Line;

      Search(Directory_Name, "", (Ordinary_File => True, others => False), Tokenize_File'Access);

   exception
      when Test_Failure => null; -- Just end if a test failed
   end Tokenize_Directory;

end Test_1;
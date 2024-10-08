-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
with Ada.Exceptions;
with Compiler.Lexer;
with Compiler.Parser;
with Ada.Command_Line;
with Ada.IO_Exceptions;

-- Program entry point
procedure Main is
   Lexer : Compiler.Lexer.Instance;
   use Ada.Exceptions;
   use Ada.Command_Line;

   procedure Lex(Filename : String) is
   begin
      Ada.Text_IO.Put("Lexing " & Filename & "... ");
      Lexer.Run(Filename);
      Ada.Text_IO.Put_Line("Tokens:");
      Ada.Text_IO.Put_Line("---------------------------------");
      
      for Token of Lexer.All_Tokens.all loop
         Compiler.Lexer.Debug(Token);
      end loop;
      
      Ada.Text_IO.New_Line(2);
   exception
      when E : Compiler.Lexer.Lexical_Error =>
         Ada.Text_IO.Put("LEXICAL ERROR: ");
         Ada.Text_IO.Put_Line(Exception_Message(E));
      when E : Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line("Invalid filename");
      when E : others => 
         Ada.Text_IO.Put_Line("Unexpected exception occurred: " & Exception_Name(E));
         raise;
   end Lex;

begin
   case Argument_Count is
      when 0 =>
         Ada.Text_IO.Put_Line("Usage: bfgada filename [..filename]");
         return;
      when others => 
         null;
   end case;

   for Number in 1 .. Argument_Count loop
      Lex(Argument(Number));
   end loop;
end Main;
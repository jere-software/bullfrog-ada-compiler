-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
with Ada.Exceptions;
with Compiler.Lexer;
with Ada.Command_Line;
with Ada.IO_Exceptions;

-- Program entry point
procedure Main is
   Lexer : Compiler.Lexer.Instance;
   use Ada.Exceptions;
   use Ada.Command_Line;
begin
   case Argument_Count is
      when 0 => Lexer.Run("input.txt");
      when 1 => Lexer.Run(Argument(1));
      when others => 
         Ada.Text_IO.Put_Line("Invalid command line arguments");
         return;
   end case;

   Ada.Text_IO.Put_Line("Tokens:");
   Ada.Text_IO.Put_Line("---------------------------------");
   
   for Token of Lexer.All_Tokens.all loop
       Compiler.Lexer.Debug(Token);
   end loop;
   
   Ada.Text_IO.Put_Line("---------------------------------");
exception
   when E : Compiler.Lexer.Lexical_Error =>
      Ada.Text_IO.Put("LEXICAL ERROR: ");
      Ada.Text_IO.Put_Line(Exception_Message(E));
   when E : Ada.IO_Exceptions.Name_Error =>
      Ada.Text_IO.Put("Invalid filename: " & Argument(1));
   when E : others => 
      Ada.Text_IO.Put("Unexpected exception occurred: " & Exception_Name(E));
      raise;
end Main;
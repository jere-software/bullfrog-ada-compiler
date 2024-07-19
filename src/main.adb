-- Copyright (C) 2024
-- Jeremiah Breeden      
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
with Ada.Exceptions;
with Compiler.Lexer;
with Compiler.Strings;

-- Program entry point
procedure Main is
   Lexer : Compiler.Lexer.Instance;
   use Compiler.Strings; 
   use Ada.Exceptions;
begin
   Lexer.Run("input.txt");
   Ada.Text_IO.Put_Line("Tokens:");
   Ada.Text_IO.Put_Line("---------------------------------");
   
   for Token of Lexer.All_Tokens.all loop
       Compiler.Lexer.Debug(Token);
   end loop;
   
   Ada.Text_IO.Put_Line("---------------------------------");
exception
   when E : Compiler.Lexer.Lexical_Error =>
      Text_IO.Put("LEXICAL ERROR: ");
      Text_IO.Put_Line(To_String(Exception_Message(E)));
   when E : others => 
      Text_IO.Put("Unexpected exception occurred: " & To_String(Exception_Name(E)));
      raise;
end Main;
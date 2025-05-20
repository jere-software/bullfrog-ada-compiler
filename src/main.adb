-- Copyright (C) 2024 - 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
with Ada.Exceptions;
with Compiler.Lexer;
with Compiler.Parser;
with Compiler.AST;
with Compiler.Strings;
with Ada.Command_Line;
with Ada.IO_Exceptions;

-- Program entry point
procedure Main is
   use Ada.Exceptions;
   use Ada.Command_Line;

   procedure Execute(Filename : String) is
      Parser : Compiler.Parser.Instance;
   begin
      Ada.Text_IO.Put("Parsing " & Filename & "... ");

      declare
         Tree : Compiler.AST.Tree := Parser.Run(Filename);
      begin
         Ada.Text_IO.Put_Line("Tokens:");
         Ada.Text_IO.Put_Line("---------------------------------");
         
         for Token of Parser.Lexer.All_Tokens.all loop
            Compiler.Lexer.Debug(Token);
         end loop;
      end;
      
   exception
      when E : Compiler.Lexer.Lexical_Error =>
         Ada.Text_IO.Put_Line(Exception_Message(E));
      when E : Compiler.Parser.Parsing_Error =>
         Ada.Text_IO.Put_Line(Exception_Message(E));
      when E : Ada.IO_Exceptions.Name_Error =>
         Ada.Text_IO.Put_Line("Invalid filename");
      when E : others => 
         Ada.Text_IO.Put_Line
            ("Unexpected exception occurred: " 
             & Exception_Information(E));
         raise;
   end Execute;

   procedure Execute_Interactive is
      Heading : constant String := "Expression: ";
      function Print_Columns(Line : String) return Boolean is
         Padding : constant String := (1..Heading'Length => ' ');
         type Digit is mod 10;
         Index : Digit := 1;
         Tens  : Digit := 1;

         package Text_IO renames Compiler.Strings.Text_IO;
      begin

         Text_IO.Put(Padding);
         for Count in Line'Range loop
            Text_IO.Put(Compiler.Strings.Image(Integer(Index)));
            Index := Index + 1;
         end loop;
         Text_IO.New_Line;
         Index := 1;

         Text_IO.Put(Padding);
         for Count in Line'Range loop
            if Index = 0 then
               Text_IO.Put(Compiler.Strings.Image(Integer(Tens)));
               Tens := Tens + 1;
            else
               Text_IO.Put(" ");
            end if;
            Index := Index + 1;
         end loop;
         Text_IO.New_Line;

         return True;
         
      end Print_Columns;
   begin
      loop
         Compiler.Strings.Text_IO.Put(Heading);
         declare
            Line   : constant String  := Compiler.Strings.Text_IO.Get_Line;
            Dummy  : constant Boolean := Print_Columns(Line);
            Stream : aliased constant Compiler.Strings.Read_Only_Stream := Compiler.Strings.Make(Line);
            Parser : Compiler.Parser.Instance;
            Tree   : Compiler.AST.Tree := Parser.Run(Stream'Access);
         begin
            Ada.Text_IO.Put_Line("Tokens:");
            Ada.Text_IO.Put_Line("---------------------------------");
            
            for Token of Parser.Lexer.All_Tokens.all loop
               Compiler.Lexer.Debug(Token);
            end loop;

            Compiler.Strings.Text_IO.New_Line;
         end;
      end loop;
   exception
      when E : Compiler.Lexer.Lexical_Error =>
         Ada.Text_IO.Put_Line(Exception_Message(E));
      when E : Compiler.Parser.Parsing_Error =>
         Ada.Text_IO.Put_Line(Exception_Message(E));
      when E : others => 
         Ada.Text_IO.Put_Line
            ("Unexpected exception occurred: " 
             & Exception_Information(E));
         raise;
   end Execute_Interactive;

begin
   case Argument_Count is
      when 0 =>
         Ada.Text_IO.Put_Line("Usage:");
         Ada.Text_IO.Put_Line("   bfgada filename [..filename]");
         Ada.Text_IO.Put_Line("   bfgada -i");
         return;
      when others => 
         null;
   end case;

   if Argument_Count = 1 and then Argument(1) = "-i" then
      Execute_Interactive;
   else
      for Number in 1 .. Argument_Count loop
         Execute(Argument(Number));
      end loop;
   end if;
end Main;
-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
with Test_1;

-- Program entry point
procedure Main is
   Lexer : Test_1.Lexer;
begin
   Ada.Text_IO.Put_Line("Hello World");
   Test_1.Tokenize_Directory(Lexer, "./src");
   Test_1.Tokenize_Directory(Lexer, "../src");
end Main;
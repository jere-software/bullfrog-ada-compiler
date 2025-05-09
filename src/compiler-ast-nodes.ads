-- Copyright (C) 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

limited with Compiler.AST.Visitors;

with Compiler.Lexer;

-- Primary list of AST nodes
package Compiler.AST.Nodes is

   -----------------------------------------------
   -- Literals
   -----------------------------------------------

   -- Literals (Integer, Real, String, Character)
   type Literal is new Node with record
      Token : Lexer.Token;
   end record;

   -- Overrides for the abstract node base class
   overriding procedure Visit(Self :        Literal; Visitor : in out Visitors.Read_Only'Class)   with Inline;
   overriding procedure Visit(Self : in out Literal; Visitor : in out Visitors.Full_Access'Class) with Inline;

end Compiler.AST.Nodes;
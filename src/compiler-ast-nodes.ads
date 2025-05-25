-- Copyright (C) 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

limited with Compiler.AST.Visitors;

-- Primary list of AST nodes
package Compiler.AST.Nodes is

   -----------------------------------------------
   -- Literals
   -----------------------------------------------

   -- Literals (Integer, Real, String, Character)
   type Literal is new Node with null record;

   -- Overrides for the abstract node base class
   overriding procedure Visit(Self :        Literal; Visitor : in out Visitors.Read_Only'Class)   with Inline;
   overriding procedure Visit(Self : in out Literal; Visitor : in out Visitors.Full_Access'Class) with Inline;

   -- Named objects
   type Name is new Node with null record;

   -- Overrides for the abstract node base class
   overriding procedure Visit(Self :        Name; Visitor : in out Visitors.Read_Only'Class)   with Inline;
   overriding procedure Visit(Self : in out Name; Visitor : in out Visitors.Full_Access'Class) with Inline;

   -- `null` keyword
   type Null_Statement is new Node with null record;

   -- Overrides for the abstract node base class
   overriding procedure Visit(Self :        Null_Statement; Visitor : in out Visitors.Read_Only'Class)   with Inline;
   overriding procedure Visit(Self : in out Null_Statement; Visitor : in out Visitors.Full_Access'Class) with Inline;

   -- One argument operations keyword
   type Unary_Operation is new Node with record
      Expression : Any_Node;
   end record;

   -- Overrides for the abstract node base class
   overriding procedure Visit(Self :        Unary_Operation; Visitor : in out Visitors.Read_Only'Class)   with Inline;
   overriding procedure Visit(Self : in out Unary_Operation; Visitor : in out Visitors.Full_Access'Class) with Inline;

   -- Two argument operations keyword
   type Binary_Operation is new Node with record
      Left, Right   : Any_Node;
      Short_Circuit : Boolean := False;
   end record;

   -- Overrides for the abstract node base class
   overriding procedure Visit(Self :        Binary_Operation; Visitor : in out Visitors.Read_Only'Class)   with Inline;
   overriding procedure Visit(Self : in out Binary_Operation; Visitor : in out Visitors.Full_Access'Class) with Inline;

end Compiler.AST.Nodes;
-- Copyright (C) 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.AST.Visitors;

package body Compiler.AST.Nodes is

   -----------------------------------------------
   -- Utility items
   -----------------------------------------------

   -- Generic used to generate the linkage between a node and a visitor
   generic
      type Node_Type is limited private;
      type Read_Only   is abstract limited new Visitors.Read_Only   with private;
      type Full_Access is abstract limited new Visitors.Full_Access with private;
      with procedure Visit(Visitor : in out Read_Only;   Node :        Node_Type) is abstract <>;
      with procedure Visit(Visitor : in out Full_Access; Node : in out Node_Type) is abstract <>;
   package Node_Template is

      -- Visit procedures designed to be used for renames for each node type's primitives.
      -- These mimic the primitive operations of the abstrace base node type
      procedure Visit(Node :        Node_Type; Visitor : in out Read_Only'Class)   with Inline;
      procedure Visit(Node : in out Node_Type; Visitor : in out Full_Access'Class) with Inline;

   end Node_Template;

   -- Implementation of the generic
   package body Node_Template is

      procedure Visit(Node : Node_Type; Visitor : in out Read_Only'Class) is
      begin
         Visit(Visitor, Node);
      end Visit;

      procedure Visit(Node : in out Node_Type; Visitor : in out Full_Access'Class) is
      begin
         Visit(Visitor, Node);
      end Visit;

   end Node_Template;

   -----------------------------------------------
   -- Templates for all nodes
   -----------------------------------------------

   -- Generates the linkage for each node type.  Using a
   -- nested package so the generic instances can be named
   -- the same as the node types (for easier copy/paste)
   package Templates is

      use all type Visitors.Read_Only;
      use all type Visitors.Full_Access;

      -- NOTE: Add a package for each new node type
      package Literal is new Node_Template
         (Nodes.Literal, Visitors.Read_Only, Visitors.Full_Access);

   end Templates;

   -----------------------------------------------
   -- Node primitive operation renamings
   -----------------------------------------------

   -- NOTE: Add primitives for each node type here

   procedure Visit
      (Self    :        Literal; 
       Visitor : in out Visitors.Read_Only'Class)
   renames Templates.Literal.Visit;

   procedure Visit
      (Self    : in out Literal; 
       Visitor : in out Visitors.Full_Access'Class)
   renames Templates.Literal.Visit;

end Compiler.AST.Nodes;
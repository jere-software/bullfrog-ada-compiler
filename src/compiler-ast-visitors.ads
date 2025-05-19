-- Copyright (C) 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.AST.Nodes;

-- Primary interface for all AST visitor objects.  These are intended
-- to be used to walk the AST tree for the various compiler stages
-- after the parser
package Compiler.AST.Visitors is

   -- Provides read-only access to the AST
   type Read_Only is limited interface;

   -- Provides modify access to the AST
   type Full_Access is limited interface;

   -- Visits the node (both read-only and modify access options for each node)
   procedure Visit(Visitor : in out Read_Only;   Node :        Nodes.Literal) is abstract;
   procedure Visit(Visitor : in out Full_Access; Node : in out Nodes.Literal) is abstract;
   procedure Visit(Visitor : in out Read_Only;   Node :        Nodes.Name) is abstract;
   procedure Visit(Visitor : in out Full_Access; Node : in out Nodes.Name) is abstract;
   procedure Visit(Visitor : in out Read_Only;   Node :        Nodes.Null_Statement) is abstract;
   procedure Visit(Visitor : in out Full_Access; Node : in out Nodes.Null_Statement) is abstract;
   procedure Visit(Visitor : in out Read_Only;   Node :        Nodes.Unary_Operation) is abstract;
   procedure Visit(Visitor : in out Full_Access; Node : in out Nodes.Unary_Operation) is abstract;
   procedure Visit(Visitor : in out Read_Only;   Node :        Nodes.Binary_Operation) is abstract;
   procedure Visit(Visitor : in out Full_Access; Node : in out Nodes.Binary_Operation) is abstract;

end Compiler.AST.Visitors;

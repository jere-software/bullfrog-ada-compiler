-- Copyright (C) 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.AST.Visitors;

package body Compiler.AST.Nodes is

   -- NOTE: Add primitives for each node type here

   procedure Visit
      (Self    :        Literal; 
       Visitor : in out Visitors.Read_Only'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

   procedure Visit
      (Self    : in out Literal; 
       Visitor : in out Visitors.Full_Access'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

   procedure Visit
      (Self    :        Name; 
       Visitor : in out Visitors.Read_Only'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

   procedure Visit
      (Self    : in out Name; 
       Visitor : in out Visitors.Full_Access'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

   procedure Visit
      (Self    :        Null_Statement; 
       Visitor : in out Visitors.Read_Only'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

   procedure Visit
      (Self    : in out Null_Statement; 
       Visitor : in out Visitors.Full_Access'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

   procedure Visit
      (Self    :        Unary_Operation; 
       Visitor : in out Visitors.Read_Only'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

   procedure Visit
      (Self    : in out Unary_Operation; 
       Visitor : in out Visitors.Full_Access'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

   procedure Visit
      (Self    :        Binary_Operation; 
       Visitor : in out Visitors.Read_Only'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

   procedure Visit
      (Self    : in out Binary_Operation; 
       Visitor : in out Visitors.Full_Access'Class)
   is begin
      Visitor.Visit(Self);
   end Visit;

end Compiler.AST.Nodes;
-- Copyright (C) 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

package body Compiler.AST is

   -----------------------------------------------
   -- Any_Node interfacae
   -----------------------------------------------

   procedure Visit
      (Self    :        Any_Node;        
       Visitor : in out visitors.Read_Only'Class)
   is begin
      Self.Constant_Reference.Visit(Visitor);
   end Visit;

   procedure Visit
      (Self    : in out Any_Node; 
       Visitor : in out visitors.Full_Access'Class)
   is begin
      Self.Reference.Visit(Visitor);
   end Visit;

   procedure Clear(Self : in out Any_Node) is
   begin
      Holders.Holder(Self).Clear;
   end Clear;

   -----------------------------------------------
   -- Node_List interface
   -----------------------------------------------

   procedure Visit
      (Self    :        Node_List;        
       Visitor : in out visitors.Read_Only'Class)
   is begin
      for Node of Self loop
         Node.Visit(Visitor);
      end loop;
   end Visit;

   procedure Visit
      (Self    : in out Node_List; 
       Visitor : in out visitors.Full_Access'Class)
   is begin
      for Node of Self loop
         Node.Visit(Visitor);
      end loop;
   end Visit;

   procedure Append(Self : in out Node_List; Item : Node'Class) is
   begin
      Vectors.Vector(Self).Append(Item);
   end Append;

   procedure Clear(Self : in out Node_List) is
   begin
      Vectors.Vector(Self).Clear;
   end Clear;

end Compiler.AST;
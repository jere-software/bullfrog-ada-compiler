-- Copyright (C) 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Vectors;

limited with Compiler.AST.Visitors;

with Compiler.Lexer;

-- Provides interface to abstract syntax trees
package Compiler.AST is

   -----------------------------------------------
   -- Abstract node base
   -----------------------------------------------

   -- Primary abstract base for all AST nodes
   type Node is abstract tagged record
      Token : Lexer.Token; -- Primary token referenced by the node
   end record;

   -- Visits the node with read only access
   procedure Visit
      (Self    :        Node; 
       Visitor : in out Visitors.Read_Only'Class)
   is abstract;

   --- Visits the node with modify access
   procedure Visit
      (Self    : in out Node; 
       Visitor : in out Visitors.Full_Access'Class)
   is abstract;

   -- Utility packages
   package Holders is new Ada.Containers.Indefinite_Holders(Node'Class);
   package Vectors is new Ada.Containers.Indefinite_Vectors(Positive, Node'Class);

   -----------------------------------------------
   -- Any_Node interfacae
   -----------------------------------------------

   -- Primary node holder type
   type Any_Node is new Holders.Holder with null record;

   -- Visits the node with read only access
   procedure Visit
      (Self    :        Any_Node;        
       Visitor : in out visitors.Read_Only'Class)
   with Inline;

   --- Visits the node with modify access
   procedure Visit
      (Self    : in out Any_Node; 
       Visitor : in out visitors.Full_Access'Class)
   with Inline;

   -- Constructing function
   function Make(Item : Node'Class) return Any_Node
      renames To_Holder;

   -- Indicates if the holder is empty or not
   function Is_Empty(Self : Any_Node) return Boolean is
      (Holders.Holder(Self).Is_Empty) with Inline;

   -- Indicates if the holder has an element or not
   function Has_Element(Self : Any_Node) return Boolean is
      (not Holders.Holder(Self).Is_Empty) with Inline;

   -- Removes element
   procedure Clear(Self : in out Any_Node)
      with Inline;

   -----------------------------------------------
   -- Node_List interface
   -----------------------------------------------

   -- Primary type to hold groups of nodes
   type Node_List is new Vectors.Vector with null record;

   -- Visits the node with read only access
   procedure Visit
      (Self    :        Node_List;        
       Visitor : in out visitors.Read_Only'Class);

   --- Visits the node with modify access
   procedure Visit
      (Self    : in out Node_List; 
       Visitor : in out visitors.Full_Access'Class);

   -- Adds an item to the list
   procedure Append(Self : in out Node_List; Item : Node'Class)
      with Inline;

   -- Indicates if the list is empty or not
   function Is_Empty(Self : Node_List) return Boolean is
      (Vectors.Vector(Self).Is_Empty) with Inline;

   -- Indicates if the list has an element or not
   function Has_Element(Self : Node_List) return Boolean is
      (not Vectors.Vector(Self).Is_Empty) with Inline;

   -- Removes elements
   procedure Clear(Self : in out Node_List)
      with Inline;

   -----------------------------------------------
   -- AST Tree interface
   -----------------------------------------------

   -- Primary tree type
   type Tree is tagged limited record
      Root : Any_Node;
   end record;

end Compiler.AST;
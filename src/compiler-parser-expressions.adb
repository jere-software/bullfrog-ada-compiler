-- Copyright (C) 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.AST.Nodes;

separate
   (Compiler.Parser)
package body Expressions is

   -- expression ::= 
   --      relation {and relation}  | relation {and then relation}
   --    | relation {or  relation}  | relation {or else  relation}
   --    | relation {xor relation}
   function Expression(Self : in out Instance) return AST.Node'Class is (raise Program_Error);

   -- relation ::= 
   --      simple_expression [relational_operator simple_expression]
   --    | tested_simple_expression [not] in membership_choice_list
   --    | raise_expression
   function Relation(Self : in out Instance) return AST.Node'Class is (raise Program_Error);

   -- simple_expression ::= [unary_adding_operator] term {binary_adding_operator term}
   function Simple_Expression(Self : in out Instance) return AST.Node'Class is (raise Program_Error);

   -- term ::= factor {multiplying_operator factor}
   function Term(Self : in out Instance) return AST.Node'Class is (raise Program_Error);

   -- factor ::= primary [** primary] | abs primary | not primary
   function Factor(Self : in out Instance) return AST.Node'Class is (raise Program_Error);

   -- primary ::= 
   --      numeric_literal  | null | string_literal | aggregate
   --    | name | allocator | (expression)
   --    | (conditional_expression) | (quantified_expression)
   --    | (declare_expression)
   function Primary(Self : in out Instance) return AST.Node'Class is (raise Program_Error);
   
end Expressions;
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

   use Compiler.AST;

   -- expression ::= 
   --      relation {and relation}  | relation {and then relation}
   --    | relation {or  relation}  | relation {or else  relation}
   --    | relation {xor relation}
   function Expression(Self : in out Instance) return AST.Node'Class is 

      -- Get first relation
      Result : Any_Node := Make(Self.Relation);

      -- Get first operator
      Token  : Compiler.Lexer.Token;

      -- Loops through either AND or OR and verifies
      -- that the operator repeats and the short circuit
      -- status is maintained
      procedure Do_Short_Circuit_Loop
         (Operator               : Tokens.Token_Kind;
          Short_Circuit_Operator : Tokens.Token_Kind)
      is begin

         -- Eat the AND / OR
         Self.Eat_Next;
         Token := Self.Token;

         declare
            -- See if it is followed by a short circuit keyword
            Should_Short_Circuit : constant Boolean := Self.Match(Short_Circuit_Operator);
         begin

            -- Loop though looking for repeats on the operator.
            -- Must have consistant short circuit status
            loop

               -- Generate the result
               Result := Make(Nodes.Binary_Operation'
                  (Token         => Token,
                   Left          => Result,
                   Right         => Make(Self.Relation),
                   Short_Circuit => Should_Short_Circuit));

               -- Stop if operators don't match
               exit when not Self.Match(Operator);
               Token := Self.Token;

               -- Verify consistent short circuit operation.  If not,
               -- then send error mentioning parenthesis
               if Should_Short_Circuit /= Self.Match(Short_Circuit_Operator) then 
                  Self.Error("Mixed logical expressions need parenthesis", Token.Line, Token.First);
               end if;

            end loop;
            
         end;
      end Do_Short_Circuit_Loop;

   begin

      case Self.Peek is
         when Tokens.Keyword_And =>
            Do_Short_Circuit_Loop
               (Operator               => Tokens.Keyword_And,
                Short_Circuit_Operator => Tokens.Keyword_Then);
         when Tokens.Keyword_Or =>
            Do_Short_Circuit_Loop
               (Operator               => Tokens.Keyword_Or,
                Short_Circuit_Operator => Tokens.Keyword_Else);
         when Tokens.Keyword_Xor =>
            while Self.Match(Tokens.Keyword_Xor) loop
               Result := Make(Nodes.Binary_Operation'
                  (Token         => Self.Token,
                   Left          => Result,
                   Right         => Make(Self.Relation),
                   Short_Circuit => False));
            end loop;
         when others => 
            null; -- Ignore if correct operator not found
      end case;

      -- Check for mixed logical operators
      case Self.Peek is
         when Tokens.Keyword_And | Tokens.Keyword_Or | Tokens.Keyword_Xor =>
            Self.Eat_Next;
            Self.Error("Mixed logical expressions need parenthesis");
         when others => null;
      end case;

      -- Send back the final result
      return Result.Constant_Reference;

   end Expression;

   -- relation ::= 
   --      simple_expression [relational_operator simple_expression]
   --    | tested_simple_expression [not] in membership_choice_list
   --    | raise_expression
   function Relation(Self : in out Instance) return AST.Node'Class is 
   begin
      -- Search for easy to parse relations first
      case Self.Peek is
         when Tokens.Keyword_Raise => Self.Eat_Next; Self.Error("Raise expression not supported yet");
         when Tokens.Keyword_Not   => Self.Eat_Next; Self.Error("Membership test not supported yet");
         when Tokens.Keyword_In    => Self.Eat_Next; Self.Error("Membership test not supported yet");
         when others => null;
      end case;

      -- Otherwise grab the first simple expression
      declare
         Result : constant Node'Class := Self.Simple_Expression;
      begin
         -- Now check for a relational operator, and generate
         -- a binary operation if found
         case Self.Peek is
            when Tokens.Operator_Equals
               | Tokens.Operator_Not_Equals
               | Tokens.Operator_Less_Than
               | Tokens.Operator_Less_Than_Equals
               | Tokens.Operator_Greater_Than
               | Tokens.Operator_Greater_Than_Equals
            => 
               Self.Eat_Next;
               return Nodes.Binary_Operation'
                  (Token         => Self.Token,
                   Left          => Make(Result),
                   Right         => Make(Self.Simple_Expression),
                   Short_Circuit => False);

            -- If no relational operator, then just return the 
            -- parsed expression
            when others => 
               return Result;
         end case;
      end;
   end Relation;

   -- simple_expression ::= [unary_adding_operator] term {binary_adding_operator term}
   function Simple_Expression(Self : in out Instance) return AST.Node'Class is 
      Result        : Any_Node;
      Short_Circuit : Boolean;
   begin
      -- Check for leading unary operator, and if there 
      -- is one, get it and the following term;
      if         Self.Match(Tokens.Operator_Plus) 
         or else Self.Match(Tokens.Operator_Minus)
      then
         Result := Make(Nodes.Unary_Operation'
            (Token      => Self.Token,
             Expression => Make(Self.Term)));
      
      -- If none found, just get the term.
      else
         Result := Make(Self.Term);
      end if;

      -- Check for any binary adding operators and chain
      -- binary operations until done
      loop
         case Self.Peek is
            when Tokens.Operator_Plus 
               | Tokens.Operator_Minus 
               | Tokens.Operator_Concatenate
            =>
               Self.Eat_Next;
               Result := Make(Nodes.Binary_Operation'
                  (Token         => Self.Token,
                   Left          => Result,
                   Right         => Make(Self.Term),
                   Short_Circuit => False));

            -- If not a binary operation, then just return the current
            -- value back, so exit loop.
            when others =>
               exit;
         end case;
      end loop;

      return Result.Constant_Reference;

   end Simple_Expression;

   -- term ::= factor {multiplying_operator factor}
   function Term(Self : in out Instance) return AST.Node'Class is
      -- Get first factor
      Result : Any_Node := Make(Self.Factor);
   begin
      -- Check for any multiplying operators and chain
      -- binary operations until done
      loop
         case Self.Peek is
            when Tokens.Operator_Multiply | Tokens.Operator_Divide
               | Tokens.Keyword_Mod       | Tokens.Keyword_Rem
            =>
               Self.Eat_Next;
               Result := Make(Nodes.Binary_Operation'
                  (Token         => Self.Token,
                   Left          => Result,
                   Right         => Make(Self.Factor),
                   Short_Circuit => False));

            -- If not a binary operation, then just return the current
            -- value back, so exit loop.
            when others =>
               exit;
         end case;
      end loop;

      return Result.Constant_Reference;

   end Term;

   -- factor ::= primary [** primary] | abs primary | not primary
   function Factor(Self : in out Instance) return AST.Node'Class is
   begin
      case Self.Peek is
         -- Look for unary operations
         when Tokens.Keyword_Abs | Tokens.Keyword_Not =>
            Self.Eat_Next;
            return Nodes.Unary_Operation'
               (Token      => Self.Token, 
                Expression => Make(Self.Primary));

         -- Assume either just a primary or a binary operation
         when others =>
            declare
               Left : constant Node'Class := Self.Primary;
            begin
               -- Binary operation (**)
               if Self.Match(Tokens.Operator_Power) then
                  return Nodes.Binary_Operation'
                     (Token         => Self.Token,
                      Left          => Make(Left),
                      Right         => Make(Self.Primary),
                      Short_Circuit => False);
               
               -- Isolated primary
               else
                  return Left;
               end if;
            end;
      end case;
   end Factor;

   -- primary ::= 
   --      numeric_literal  | null | string_literal | aggregate
   --    | name | allocator | (expression)
   --    | (conditional_expression) | (quantified_expression)
   --    | (declare_expression)
   function Primary(Self : in out Instance) return AST.Node'Class is
   begin

      -- Look at possible primary options
      case Self.Peek is
         when Tokens.Identifier => 
            Self.Eat_Next;
            return Nodes.Name'(Token => Self.Token);
         when Tokens.Character_Literal
            | Tokens.String_Literal 
            | Tokens.Real_Literal
            | Tokens.Integer_Literal
         =>
            Self.Eat_Next;
            return Nodes.Literal'(Token => Self.Token);
         when Tokens.Operator_Open_Parenthesis =>
            Self.Eat_Next;
            return Result : constant Node'Class := Self.Expression do 
               Self.Match(Tokens.Operator_Close_Parenthesis);
            end return;
         when Tokens.Keyword_Null =>
            Self.Eat_Next;
            return Nodes.Null_Statement'(Token => Self.Token);
         when others => null;
      end case;

      -- Error conditions
      case Self.Peek is
         when Tokens.Operator_Plus | Tokens.Operator_Minus =>
            Self.Eat_Next;
            Self.Error("Unary expression needs parenthesis");
         when others =>
            Self.Eat_Next;
            Self.Error("Unexpected token " 
                        & Self.Token_Value
                        & ", a primary token was expected");
      end case;

   end Primary;
   
end Expressions;
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

      -- Loops through either AND or OR and verifies
      -- that the operator repeats and the short circuit
      -- status is maintained
      procedure Do_And_Or_Loop
         (Operator               : Tokens.Token_Kind;
          Short_Circuit_Operator : Tokens.Token_Kind)
      with Pre => 
            (Operator in Tokens.Keyword_And and Short_Circuit_Operator in Tokens.Keyword_Then)
         or (Operator in Tokens.Keyword_Or  and Short_Circuit_Operator in Tokens.Keyword_Else);

      procedure Do_And_Or_Loop
         (Operator               : Tokens.Token_Kind;
          Short_Circuit_Operator : Tokens.Token_Kind)
      is 
         -- Eat the AND / OR
         Token  : Compiler.Lexer.Token := Self.Eat_Next;
      begin

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
                  Self.Error("Mixed logical expressions need parenthesis", Token);
               end if;

            end loop;
            
         end;
      end Do_And_Or_Loop;

   begin

      case Self.Peek is
         when Tokens.Keyword_And =>
            Do_And_Or_Loop
               (Operator               => Tokens.Keyword_And,
                Short_Circuit_Operator => Tokens.Keyword_Then);
         when Tokens.Keyword_Or =>
            Do_And_Or_Loop
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
      if Self.Match(Tokens.Keyword_Raise) then
         return Self.Raise_Expression;
      end if;

      -- Otherwise grab the first simple expression
      declare
         Result : constant Node'Class := Self.Simple_Expression;
      begin
         -- Now check for a relational operator, and generate
         -- a binary operation if found
         case Self.Peek is
            when Tokens.Delimiter_Equals
               | Tokens.Delimiter_Not_Equals
               | Tokens.Delimiter_Less_Than
               | Tokens.Delimiter_Less_Than_Equals
               | Tokens.Delimiter_Greater_Than
               | Tokens.Delimiter_Greater_Than_Equals
            => 
               return Nodes.Binary_Operation'
                  (Token         => Self.Eat_Next,
                   Left          => Make(Result),
                   Right         => Make(Self.Simple_Expression),
                   Short_Circuit => False);

            when Tokens.Keyword_Not   =>
               Self.Eat_Next;
               Self.Match(Tokens.Keyword_In);
               return Nodes.Membership'
                  (Token   => Self.Token,
                   Negate  => True,
                   Source  => Make(Result),
                   Targets => Self.Membership);

            when Tokens.Keyword_In => 
               Self.Eat_Next;
               return Nodes.Membership'
                  (Token   => Self.Token,
                   Negate  => False,
                   Source  => Make(Result),
                   Targets => Self.Membership);

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
      if         Self.Match(Tokens.Delimiter_Plus) 
         or else Self.Match(Tokens.Delimiter_Minus)
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
            when Tokens.Delimiter_Plus 
               | Tokens.Delimiter_Minus 
               | Tokens.Delimiter_Concatenate
            =>
               Result := Make(Nodes.Binary_Operation'
                  (Token         => Self.Eat_Next,
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
            when Tokens.Delimiter_Multiply | Tokens.Delimiter_Divide
               | Tokens.Keyword_Mod       | Tokens.Keyword_Rem
            =>
               Result := Make(Nodes.Binary_Operation'
                  (Token         => Self.Eat_Next,
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
            return Nodes.Unary_Operation'
               (Token      => Self.Eat_Next, 
                Expression => Make(Self.Primary));

         -- Assume either just a primary or a binary operation
         when others =>
            declare
               Left : constant Node'Class := Self.Primary;
            begin
               -- Binary operation (**)
               if Self.Match(Tokens.Delimiter_Exponent) then
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
         when Tokens.String_Literal 
            | Tokens.Real_Literal
            | Tokens.Integer_Literal
         =>
            return Nodes.Literal'(Token => Self.Eat_Next);
         when Tokens.Delimiter_Open_Parenthesis =>
            Self.Eat_Next; -- Munch the open parenthesis
            return Result : constant Node'Class := Self.Expression do 
               -- Munch the close parenthesis
               Self.Match(Tokens.Delimiter_Close_Parenthesis);
            end return;
         when Tokens.Keyword_Null =>
            return Nodes.Null_Expression'(Token => Self.Eat_Next);
         when Tokens.Character_Literal
            | Tokens.Identifier
         => 
            return Nodes.Name'(Token => Self.Eat_Next);
         when others => null;
      end case;

      -- Error conditions
      case Self.Peek is
         when Tokens.Delimiter_Plus | Tokens.Delimiter_Minus =>
            Self.Eat_Next;
            Self.Error("Unary expression needs parenthesis");
         when others =>
            Self.Eat_Next;
            Self.Error("Unexpected token " 
                        & Self.Token_Kind'Image
                        & " found, but a primary token was expected");
      end case;

   end Primary;

   -- membership_choice_list ::= membership_choice {'|' membership_choice}
   function Membership(Self : in out Instance) return AST.Node_List is
   begin
      return Result : Node_List do 
         Result.Append(Self.Membership_Choice);
         while Self.Match(Tokens.Delimiter_Membership) loop
            Result.Append(Self.Membership_Choice);
         end loop;
      end return;
   end Membership;

   -- membership_choice ::= choice_simple_expression | range | subtype_mark
   -- subtype_mark ::= subtype_name
   -- range ::=  
   --   range_attribute_reference
   -- | simple_expression .. simple_expression
   function Membership_Choice(Self : in out Instance) return AST.Node'Class is
      Result : constant Node'Class := Self.Expression;
   begin
      if Self.Match(Tokens.Delimiter_Range) then
         return Nodes.Simple_Range'
            (Token => Self.Token,
             Left  => Make(Result),
             Right => Make(Self.Expression));
      else
         return Result;
      end if;
   end Membership_Choice;

   -- raise_expression ::= raise exception_name [with string_simple_expression]
   function Raise_Expression(Self : in out Instance) return AST.Node'Class is
      Token : Lexer_Token;
   begin
      Self.Match(Tokens.Keyword_Raise);
      Token := Self.Token;
      
      return Result : Nodes.Raise_Expression :=
         (Token  => Token,
          Name   => AST.Nodes.Name(Self.Expression),
          others => <>)
      do 
         if Self.Match(Tokens.Keyword_With) then
            Result.Expression := Make(Self.Expression);
         end if;
      end return;
   exception
      when Parsing_Error => raise;
      when others => Self.Error
         (Message => "Name expected for raise expression",
          Line    => Token.Line,
          Column  => Token.Last + 1);
   end Raise_Expression;
   
end Expressions;
-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Strings;

-- Provides a tokenized breakdown of all lexical elements.
-- Note that while identifiers are broken down into keywords,
-- they are not broken down into attributes, aspects, or
-- pragmas.
package Compiler.Tokens is

   -- Top level token identifier
   type Token_Kind is
      (End_Of_File, -- not a real token, but appended to end of list
       Identifier,
       Attribute,  -- Subcategory of Identifier
       Pragma_ID,  -- Subcategory of Identifier
       String_Literal,
       Character_Literal,
       Integer_Literal,
       Real_Literal,
       Comment, -- Only useful for unit testing, not for production
       Keyword_Begin,
       Keyword_Do,
       Keyword_Goto,
       Keyword_If,
       Keyword_Then,
       Keyword_Elsif,
       Keyword_Else,
       Keyword_Case,
       Keyword_When,
       Keyword_Loop,
       Keyword_While,
       Keyword_For,
       Keyword_Exit,
       Keyword_Delay,
       Keyword_Until,
       Keyword_And,
       Keyword_Or,
       Keyword_Xor,
       Keyword_Not,
       Keyword_With,
       Keyword_Use,
       Keyword_Pragma,
       Keyword_Declare,
       Keyword_Parallel,
       Keyword_Generic,
       Keyword_Separate,
       Keyword_Package,
       Keyword_Overriding,
       Keyword_Procedure,
       Keyword_Function,
       Keyword_Is,
       Keyword_Renames,
       Keyword_Body,
       Keyword_Exception,
       Keyword_Raise,
       Keyword_Return,
       Keyword_Type,
       Keyword_Subtype,
       Keyword_Interface,
       Keyword_Synchronized,
       Keyword_Protected,
       Keyword_Task,
       Keyword_Array,
       Keyword_Record,
       Keyword_Private,
       Keyword_Abstract,
       Keyword_Tagged,
       Keyword_Limited,
       Keyword_Aliased,
       Keyword_Constant,
       Keyword_Access,
       Keyword_All,
       Keyword_Some,
       Keyword_Range,
       Keyword_Delta,
       Keyword_Digits,
       Keyword_Mod,
       Keyword_Rem,
       Keyword_Abs,
       Keyword_New,
       Keyword_Null,
       Keyword_Others,
       Keyword_At,
       Keyword_In,
       Keyword_Out,
       Keyword_Of,
       Keyword_Reverse,
       Keyword_Abort,
       Keyword_Select,
       Keyword_Terminate,
       Keyword_Accept,
       Keyword_Entry,
       Keyword_Requeue,
       Keyword_End,
       Delimiter_Plus,
       Delimiter_Minus,
       Delimiter_Concatenate,
       Delimiter_Multiply,
       Delimiter_Divide,
       Delimiter_Exponent,
       Delimiter_Membership,
       Delimiter_Open_Parenthesis,
       Delimiter_Close_Parenthesis,
       Delimiter_Open_Bracket,
       Delimiter_Close_Bracket,
       Delimiter_Semicolon,
       Delimiter_Colon,
       Delimiter_Comma,
       Delimiter_Apostrophe,
       Delimiter_Dot,
       Delimiter_Range,
       Delimiter_Target,
       Delimiter_Box,
       Delimiter_Left_Label,
       Delimiter_Right_Label,
       Delimiter_Assignment,
       Delimiter_Arrow,
       Delimiter_Equals,
       Delimiter_Not_Equals,
       Delimiter_Less_Than,
       Delimiter_Less_Than_Equals,
       Delimiter_Greater_Than,
       Delimiter_Greater_Than_Equals);

   -- Token subgroups
   subtype Keyword is Token_Kind range
      Keyword_Begin .. Keyword_End;
   subtype Delimiter is Token_Kind range
      Delimiter_Plus .. Delimiter_Greater_Than_Equals;
   
   -- Precedence groups for math and boolean operations
   subtype Logical_Operator is Keyword range
      Keyword_And .. Keyword_Xor;
   subtype Relational_Operator is Delimiter range
      Delimiter_Equals .. Delimiter_Greater_Than_Equals;
   subtype Binary_Adding_Operator is Delimiter range
      Delimiter_Plus .. Delimiter_Concatenate;
   subtype Unary_Adding_Operator is Delimiter range
      Delimiter_Plus .. Delimiter_Minus;
   subtype Multiplying_Operator is Token_Kind 
      with Static_Predicate => Multiplying_Operator in
           Delimiter_Multiply .. Delimiter_Exponent 
         | Keyword_Mod      .. Keyword_Rem;
   subtype Highest_Precedence_Operator is Token_Kind 
      with Static_Predicate => Highest_Precedence_Operator in
           Delimiter_Exponent 
         | Keyword_Abs 
         | Keyword_Not;

-- Specialized numeric types for token information
   type Line_Number is new Positive;
   type Column_Number is new Positive;

   -- Image functions
   function Image(Item : Line_Number) return String is
      (Strings.Image(Integer(Item))) with Inline;
   function Image(Item : Column_Number) return String is
      (Strings.Image(Integer(Item))) with Inline;

   -- Core Token type
   type Token
      (Kind  : Tokens.Token_Kind := Tokens.Comment) 
   is record
      Value  : Strings.Holder := Strings.Empty_Holder;
      Line   : Line_Number    := 1;
      First  : Column_Number  := 1;
      Last   : Column_Number  := 1;
   end record;
      
end Compiler.Tokens;
-- Copyright (C) 2024
-- Jeremiah Breeden      
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

-- Provides a tokenized breakdown of all lexical elements.
-- Note that while identifiers are broken down into keywords,
-- they are not broken down into attributes, aspects, or
-- pragmas.
package Compiler.Tokens with Pure is

    -- Top level token identifier
    type Token_Kind is
        (Identifier,
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
         String_Literal,
         Character_Literal,
         Integer_Literal,
         Real_Literal,
         Comment,
         Operator_Plus,
         Operator_Minus,
         Operator_Concatenate,
         Operator_Multiply,
         Operator_Divide,
         Operator_Power,
         Operator_Membership,
         Operator_Open_Parenthesis,
         Operator_Close_Parenthesis,
         Operator_Open_Bracket,
         Operator_Close_Bracket,
         Operator_Semicolon,
         Operator_Colon,
         Operator_Comma,
         Operator_Apostrophe,
         Operator_Dot,
         Operator_Range,
         Operator_At_Sign,
         Operator_Box,
         Operator_Left_Label,
         Operator_Right_Label,
         Operator_Assignment,
         Operator_Arrow,
         Operator_Equals,
         Operator_Not_Equals,
         Operator_Less_Than,
         Operator_Less_Than_Equals,
         Operator_Greater_Than,
         Operator_Greater_Than_Equals);

    -- Token subgroups
    subtype Keyword is Token_Kind range
        Keyword_Begin .. Keyword_End;
    subtype Operator is Token_Kind range
        Operator_Plus .. Operator_Greater_Than_Equals;
    
    -- Precedence groups for math and boolean operations
    subtype Or_Operation is Keyword range
        Keyword_Or .. Keyword_Xor;
    subtype And_Operation is Keyword range
        Keyword_And .. Keyword_And;
    subtype Logical_Operation is Keyword range
        Keyword_And .. Keyword_Xor;
    subtype Rel_Operation is Operator range
        Operator_Equals .. Operator_Greater_Than_Equals;
    subtype Binary_Add_Operation is Operator range
        Operator_Plus .. Operator_Concatenate;
    subtype Unary_Add_Operation is Operator range
        Operator_Plus .. Operator_Minus;
    subtype Mul_Operation is Token_Kind 
        with Static_Predicate => Mul_Operation in
              Operator_Multiply .. Operator_Power 
            | Keyword_Mod       .. Keyword_Rem;
    subtype Highest_Precedence_Operation is Token_Kind 
        with Static_Predicate => Highest_Precedence_Operation in
              Operator_Power 
            | Keyword_Abs 
            | Keyword_Not;
        
end Compiler.Tokens;
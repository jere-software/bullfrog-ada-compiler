-- Copyright (C) 2024
-- Jeremiah Breeden     
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

-- Parses the stream for an operator or a comment
separate (Compiler.Lexer)
procedure Get_Operator
   (Self   : in out Instance; 
    Stream : not null access Ada.Streams.Root_Stream_Type'Class)
is

   -- Local operator token type declarations
   subtype Operator is Tokens.Token_Kind with
      Static_Predicate =>
            (Operator in Tokens.Comment) -- lump in comment for easier processing
         or (Operator in Tokens.Operator);
   subtype Token_1_Character is Operator
      with Static_Predicate => Token_1_Character
         in  Tokens.Operator_Plus
         |   Tokens.Operator_Concatenate
         |   Tokens.Operator_Membership
         |   Tokens.Operator_Open_Parenthesis
         |   Tokens.Operator_Close_Parenthesis
         |   Tokens.Operator_Open_Bracket
         |   Tokens.Operator_Close_Bracket
         |   Tokens.Operator_Semicolon
         |   Tokens.Operator_Comma
         |   Tokens.Operator_Apostrophe
         |   Tokens.Operator_At_Sign;
   subtype Token_2_Characters is Operator
      with Static_Predicate => Token_2_Characters
         not in Token_1_Character;
   subtype Needs_Next_In is Operator
      with Static_Predicate => Needs_Next_In
         in  Tokens.Comment
         |   Tokens.Operator_Power
         |   Tokens.Operator_Not_Equals
         |   Tokens.Operator_Less_Than_Equals
         |   Tokens.Operator_Greater_Than_Equals
         |   Tokens.Operator_Assignment
         |   Tokens.Operator_Arrow
         |   Tokens.Operator_Range
         |   Tokens.Operator_Box
         |   Tokens.Operator_Left_Label
         |   Tokens.Operator_Right_Label;

   -- Local operator character type declarations
   subtype One_Character is Strings.Operator_1_Character;
   subtype First_Character is Strings.Operator_2_Character;

   -- Character to Token conversion functions for operator parsing
   function To_Token(Character : One_Character) return Token_1_Character is
      (case Character is
         when Strings.Plus              => Tokens.Operator_Plus,
         when Strings.Ampersand         => Tokens.Operator_Concatenate,
         when Strings.Bar               => Tokens.Operator_Membership,
         when Strings.Open_Parenthesis  => Tokens.Operator_Open_Parenthesis,
         when Strings.Close_Parenthesis => Tokens.Operator_Close_Parenthesis,
         when Strings.Open_Bracket      => Tokens.Operator_Open_Bracket,
         when Strings.Close_Bracket     => Tokens.Operator_Close_Bracket,
         when Strings.Semicolon         => Tokens.Operator_Semicolon,
         when Strings.Comma             => Tokens.Operator_Comma,
         when Strings.Apostrophe        => Tokens.Operator_Apostrophe,
         when Strings.At_Sign           => Tokens.Operator_At_Sign);
   function To_Token
      (L : First_Character; 
       R : Character) 
       return Token_2_Characters 
   is  (case L is
         when Strings.Minus => (case R is
            when Strings.Minus => Tokens.Comment,
            when others        => Tokens.Operator_Minus),
         when Strings.Asterisk => (case R is
            when Strings.Asterisk => Tokens.Operator_Power,
            when others           => Tokens.Operator_Multiply),
         when Strings.Forward_Slash => (case R is
            when Strings.Equals => Tokens.Operator_Not_Equals,
            when others         => Tokens.Operator_Divide),
         when Strings.Less_Than => (case R is
            when Strings.Equals       => Tokens.Operator_Less_Than_Equals,
            when Strings.Greater_Than => Tokens.Operator_Box,
            when Strings.Less_Than    => Tokens.Operator_Left_Label,
            when others               => Tokens.Operator_Less_Than),
         when Strings.Greater_Than => (case R is
            when Strings.Equals       => Tokens.Operator_Greater_Than_Equals,
            when Strings.Greater_Than => Tokens.Operator_Right_Label,
            when others               => Tokens.Operator_Greater_Than),
         when Strings.Colon => (case R is
            when Strings.Equals => Tokens.Operator_Assignment,
            when others         => Tokens.Operator_Colon),
         when Strings.Equals => (case R is
            when Strings.Greater_Than => Tokens.Operator_Arrow,
            when others               => Tokens.Operator_Equals),
         when Strings.Period => (case R is
            when Strings.Period => Tokens.Operator_Range,
            when others         => Tokens.Operator_Dot));

   Result : String(1..2);
   Temp  : Character;
   Line  : constant Positive := Self.Line;
   First : constant Positive := Self.Column;
   Kind  : Tokens.Token_Kind;
begin

   -- If an operator with only one Character then set the result
   -- and read in the next Character
   if Self.Next_In in One_Character then
      Self.Add_Token
         (Kind  => To_Token(Self.Next_In),
          Value => "" & Self.Next_In,
          Line  => Line,
          First => First,
          Last  => First);
      Self.Get_Character(Stream);

   -- Otherwise if it's potentially two Characters, then save
   -- the first one, read in the next and process it
   elsif Self.Next_In in First_Character then
      Temp := Self.Next_In;
      Self.Get_Character(Stream);
      Kind := To_Token(Temp, Self.Next_In);

      -- If the next Character was part of a two Character Token, then
      -- set the result and read in the next Character
      if Kind in Needs_Next_In then
         Self.Add_Token
            (Kind  => Kind,
             Value => Temp & Self.Next_In,
             Line  => Line,
             First => First,
             Last  => Self.Column);
         Self.Get_Character(Stream);

      -- Otherwise just set the Token as the first Character
      -- read in and preserve the most recently read Character
      else
         Self.Add_Token
            (Kind  => Kind,
             Value => "" & Temp,
             Line  => Line,
             First => First,
             Last  => First);
      end if;

   -- Not an operator
   else
      Self.Expected("Operator");
   end if;

end Get_Operator;
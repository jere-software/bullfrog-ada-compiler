-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

-- Parses the stream for an operator or a comment
separate (Compiler.Lexer)
procedure Get_Delimiter
   (Self   : in out Instance; 
    Stream : not null access Ada.Streams.Root_Stream_Type'Class)
is

   -- Adds a token for a one character delimiter
   procedure Add_Basic(Kind : Tokens.Token_Kind) is
   begin
      Self.Add_Token
         (Kind  => Kind,
          Value => "" & Self.Next,
          Line  => Self.Line,
          First => Self.Column,
          Last  => Self.Column);
      Self.Advance(Stream);
   end Add_Basic;

   -- Adds a token for a two character delimiter
   procedure Add_Compound(Kind : Tokens.Token_Kind) is
   begin
      Self.Add_Token
         (Kind  => Kind,
          Value => "" & Self.Next & Self.Peek,
          Line  => Self.Line,
          First => Self.Column,
          Last  => Self.Column + 1);
      Self.Advance(Stream);
      Self.Advance(Stream);
   end Add_Compound;

   use Strings;
   use Tokens;

begin

   case Self.Next is
      when Plus              => Add_Basic(Delimiter_Plus);
      when Ampersand         => Add_Basic(Delimiter_Concatenate);
      when Bar               => Add_Basic(Delimiter_Membership);
      when Open_Parenthesis  => Add_Basic(Delimiter_Open_Parenthesis);
      when Close_Parenthesis => Add_Basic(Delimiter_Close_Parenthesis);
      when Open_Bracket      => Add_Basic(Delimiter_Open_Bracket);
      when Close_Bracket     => Add_Basic(Delimiter_Close_Bracket);
      when Semicolon         => Add_Basic(Delimiter_Semicolon);
      when Comma             => Add_Basic(Delimiter_Comma);
      when Apostrophe        => Add_Basic(Delimiter_Apostrophe);
      when At_Sign           => Add_Basic(Delimiter_Target);
      when Minus => 
         case Self.Peek is
            when Minus  => Add_Compound(Tokens.Comment);
            when others => Add_Basic   (Delimiter_Minus);
         end case;
      when Asterisk => 
         case Self.Peek is
            when Asterisk => Add_Compound(Delimiter_Exponent);
            when others   => Add_Basic   (Delimiter_Multiply);
         end case;
      when Forward_Slash => 
         case Self.Peek is
            when Equals => Add_Compound(Delimiter_Not_Equals);
            when others => Add_Basic   (Delimiter_Divide);
         end case;
      when Less_Than => 
         case Self.Peek is
            when Equals       => Add_Compound(Delimiter_Less_Than_Equals);
            when Greater_Than => Add_Compound(Delimiter_Box);
            when Less_Than    => Add_Compound(Delimiter_Left_Label);
            when others       => Add_Basic   (Delimiter_Less_Than);
         end case;
      when Greater_Than => 
         case Self.Peek is
            when Equals       => Add_Compound(Delimiter_Greater_Than_Equals);
            when Greater_Than => Add_Compound(Delimiter_Right_Label);
            when others       => Add_Basic   (Delimiter_Greater_Than);
         end case;
      when Colon => 
         case Self.Peek is
            when Equals => Add_Compound(Delimiter_Assignment);
            when others => Add_Basic   (Delimiter_Colon);
         end case;
      when Equals => 
         case Self.Peek is
            when Greater_Than => Add_Compound(Delimiter_Arrow);
            when others       => Add_Basic   (Delimiter_Equals);
         end case;
      when Period => 
         case Self.Peek is
            when Period => Add_Compound(Delimiter_Range);
            when others => Add_Basic   (Delimiter_Dot);
         end case;
      when others => Self.Error("Operator expected");
   end case;

end Get_Delimiter;
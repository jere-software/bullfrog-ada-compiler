-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

package body Compiler.Parser is

   -- Bring in operators for Token_Kind
   use type Tokens.Token_Kind;

   ------------------------------------------------------
   --------------- Core Parsing Operations --------------
   ------------------------------------------------------

   procedure Run(Self : in out Instance; Filename : Standard.String) is
   begin
      Self.Lexer.Run(Filename);
   end Run;

   procedure Run
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is begin
      Self.Lexer.Run(Stream);
   end Run;

   ------------------------------------------------------
   ----------------- Utility Operations -----------------
   ------------------------------------------------------

   function Token_Kind(Self : Instance; Index : Positive) return Tokens.Token_Kind
      is (Self.Lexer.All_Tokens.all(Index).Kind);
   function Token_Kind(Self : Instance) return Tokens.Token_Kind
      is (Self.Token_Kind(Self.Last));

   function Token_Value(Self : Instance; Index : Positive) return Strings.String
      is (Self.Lexer.All_Tokens.all(Index).Value.Get);
   function Token_Value(Self : Instance) return Strings.String
      is (Self.Token_Value(Self.Last));

   ------------------------------------------------------
   ------------ Low Level Parsing Operations ------------
   ------------------------------------------------------

   procedure Scan(Self : in out Instance) is
   begin
      Self.Last := Self.Next;
      if Self.Next < Self.Lexer.All_Tokens.Last_Index then
         Self.Next := Self.Next + 1;
      end if;
   end Scan;

   procedure Match
      (Self       : in out Instance;
       Identifier :        Strings.String)
   is begin
      Self.Match(Tokens.Identifier);
      if Self.Token_Value /= Identifier then
         Self.Expected(Identifier);
      end if;
   end Match;

   procedure Match
      (Self  : in out Instance; 
       Token :        Tokens.Token_Kind) 
   is begin
      if not Self.Match(Token) then
         Self.Expected(Token'Image);
      end if;
   end Match;

   function Match
      (Self  : in out Instance; 
       Token :        Tokens.Token_Kind) 
       return Boolean
   is begin
      if Self.Token_Kind(Self.Next) = Token then
         Self.Scan;
         return True;
      else
         return False;
      end if;
   end Match;

   ------------------------------------------------------
   ------------- General Parsing Operations -------------
   ------------------------------------------------------

   procedure Attribute_Identifier
      (Self      : in out Instance;
       Info      :    out Attributes.Info)
   is 
      use Attributes;
      ID : Attributes.Attribute_Identifier;
   begin

      Self.Match(Tokens.Attribute);

      ID := Attribute_ID(Self.Token_Value);

      if ID in Valid_Attribute then
         Info := Attribute_Info(ID);
      else
         Self.Expected("Valid attribute");
      end if;

   end Attribute_Identifier;

   procedure Aspect_Identifier
      (Self      : in out Instance;
       Info      :    out Aspects.Info)
   is 
      use Aspects;
      ID : Aspects.Aspect_Identifier;
      Line   : constant Positive 
         := Self.Lexer.All_Tokens.all(Self.Next).Line;
      Column : constant Positive 
         := Self.Lexer.All_Tokens.all(Self.Next).First;
   begin

      -- if an any keywords can be an aspect, then this needs to change to
      -- the following.  Use "or else" to chain multiple keywords.
      --
      --    if not Self.Match(Keyword_XXXX) then
      --       Self.Match(Tokens.Identifier);
      --    end if;

      Self.Match(Tokens.Identifier);

      declare
         Base : constant String := Self.Token_Value;
      begin

         -- See if this is an aspect with an attribute
         -- or just a basic aspect
         if Self.Match(Tokens.Operator_Apostrophe) then
            Self.Match(Tokens.Attribute);
            ID := Aspect_ID(Base, Self.Token_Value);
         else
            ID := Aspect_ID(Base);
         end if;

      end;

      if ID in Valid_Aspect then
         Info := Aspect_Info(ID);
      else
         Self.Expected("Valid Aspect", Line, Column);
      end if;

   end Aspect_Identifier;

   procedure Pragma_Identifier
      (Self      : in out Instance;
       Info      :    out Pragmas.Info)
   is 
      use Pragmas;
      ID : Pragmas.Pragma_Identifier;
   begin

      Self.Match(Tokens.Identifier);

      ID := Pragma_ID(Self.Token_Value);

      if ID in Valid_Pragma then
         Info := Pragma_Info(ID);
      else
         Self.Expected("Valid Pragma");
      end if;

   end Pragma_Identifier;

   ------------------------------------------------------
   -------------- Parser Output Operations --------------
   ------------------------------------------------------

   procedure Halt(Self : Instance; Message : String) is
   begin
      raise Parsing_Error with Message;
   end Halt;

   procedure Expected(Self : Instance; Message : String) is
      Token : Lexer.Token 
         renames Self.Lexer.All_Tokens.all(Self.Last);
   begin
      Self.Expected(Message, Token.Line, Token.First);
   end Expected;

   procedure Expected
      (Self    : Instance; 
       Message : String;
       Token   : Lexer.Token)
   is begin
      Self.Expected(Message, Token.Line, Token.First);
   end Expected;

   procedure Expected(Self : Instance; Message : String; Line, Column : Positive) is
   begin
      Self.Halt
         (Message 
          & " expected at " 
          & Strings.Image(Line) 
          & ":"
          & Strings.Image(Column));
   end Expected;

end Compiler.Parser;
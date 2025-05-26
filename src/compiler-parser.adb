-- Copyright (C) 2024 - 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

package body Compiler.Parser is

   -- Bring in operators for Token_Kind
   use type Tokens.Token_Kind;
   use type Strings.Holder;
   use type Strings.String;

   ------------------------------------------------------
   --------------- Core Parsing Operations --------------
   ------------------------------------------------------

   -- General "Run" operation
   function Run(Self : in out Instance) return AST.Tree is
   begin 
      -- Initialize parser state
      Self.Next    := 1;
      Self.Last    := 1;
      Self.Running := Self.Lexer.All_Tokens.Length not in 0;

      return Result : AST.Tree := (Root => AST.Make(Self.Expression)) do
         Strings.Text_IO.Put_Line("Last Token Parsed is " & Self.Token_Kind'Image
            & " => " & Self.Token_Value);
      end return;
   end Run;

   function Run(Self : in out Instance; Filename : Standard.String) return AST.Tree is
   begin
      Self.Lexer.Run(Filename);
      return Run(Self);
   end Run;

   function Run
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      return AST.Tree
   is begin
      Self.Lexer.Run(Stream);
      return Run(Self);
   end Run;

   function Is_Running(Self : Instance) return Boolean is
      (Self.Running);

   function Lexer(Self : aliased Instance) 
      return not null access constant Compiler.Lexer.Instance is
   (Self.Lexer'Access);

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

   function Token(Self : Instance; Index : Positive) return Compiler.Lexer.Token
      is (Self.Lexer.All_Tokens.all(Index));
   function Token(Self : Instance) return Compiler.Lexer.Token
      is (Self.Token(Self.Last));

   ------------------------------------------------------
   ------------ Low Level Parsing Operations ------------
   ------------------------------------------------------

   procedure Scan(Self : in out Instance) is
   begin
      Self.Last := Self.Next;
      if Self.Next < Self.Lexer.All_Tokens.Last_Index then
         Self.Next := Self.Next + 1;
      else
         Self.Running := False;
      end if;
   end Scan;

   function Mark(Self : Instance) return Positive is (Self.Last);

   procedure Release(Self : in out Instance; Mark : Positive) is
      Last : constant Positive := Self.Lexer.All_Tokens.Last_Index;
   begin
      -- If the supplied mark is not the last, then
      -- Set Last as Mark, and Next as the following
      -- position
      if Mark < Last then
         Self.Last := Mark;
         Self.Next := Mark + 1;
      
      -- Otherwise, set both the last index.  This
      -- bounds the values in the same way that the
      -- Scan operation does.
      else
         Self.Last := Last;
         Self.Next := Last;
      end if;
   end Release;

   function Token_Image(Token : Compiler.Lexer.Token) return Strings.String is
      (if Token.Kind in Tokens.Identifier 
                      | Tokens.Attribute 
                      | Tokens.Pragma_ID 
       then 
         Token.Kind'Image & " (" & Token.Value & ")" 
       else
         Token.Kind'Image);

   function Current_Token_Image(Self  : Instance) return Strings.String is
      (if not Self.Is_Running then
         "End of File"
       else
         Token_Image(Self.Lexer.All_Tokens.all(Self.Next)));

   function Token_Error_Image
      (Self  : Instance;
       Token : Tokens.Token_Kind)
       return Strings.String 
   is ("Expected " & Token'Image & " but found " & Current_Token_Image(Self));

   function Identifier_Error_Image
      (Self       : Instance;
       Identifier : Strings.String)
       return Strings.String
   is ("Expected " & Tokens.Identifier'Image 
       & "(" & Identifier & ") but found " 
       & Current_Token_Image(Self));

   function Peek(Self : Instance) return Tokens.Token_Kind is
      (Self.Lexer.All_Tokens.all(Self.Next).Kind);

   procedure Match
      (Self       : in out Instance;
       Identifier :        Strings.String)
   is begin
      Self.Match(Tokens.Identifier);
      if Self.Token_Value /= Identifier then
         Self.Error(Identifier_Error_Image(Self, Identifier));
      end if;
   end Match;

   procedure Match
      (Self  : in out Instance; 
       Token :        Tokens.Token_Kind) 
   is begin
      if not Self.Match(Token) then
         Self.Error(Token_Error_Image(Self, Token));
      end if;
   end Match;

   function Match
      (Self  : in out Instance; 
       Token :        Tokens.Token_Kind) 
       return Boolean
   is begin
      if Self.Is_Running and then Self.Token_Kind(Self.Next) = Token then
         Self.Scan;
         return True;
      else
         return False;
      end if;
   end Match;

   procedure Eat_Next(Self : in out Instance) is
   begin
      if Self.Is_Running then
         Self.Scan;
      else
         Self.Error("Expected a token", Self.Token.Line, Self.Token.Last + 1);
      end if;
   end Eat_Next;

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
         Self.Error("Expected valid ATTRIBUTE but found " & Self.Token_Value);
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

         if ID in Valid_Aspect then
            Info := Aspect_Info(ID);
         else
            Self.Error("Expected valid ASPECT but found " 
                       & (if Self.Token_Kind = Tokens.Attribute then
                              Base & "'" & Self.Token_Value
                          else
                              Base));
         end if;

      end;

   end Aspect_Identifier;

   procedure Pragma_Identifier
      (Self      : in out Instance;
       Info      :    out Pragmas.Info)
   is 
      use Pragmas;
      ID : Pragmas.Pragma_Identifier;
   begin

      Self.Match(Tokens.Pragma_ID);

      ID := Pragma_ID(Self.Token_Value);

      if ID in Valid_Pragma then
         Info := Pragma_Info(ID);
      else
         Self.Error("Expected valid PRAGMA but found " & Self.Token_Value);
      end if;

   end Pragma_Identifier;

   ------------------------------------------------------
   -------------- Parser Output Operations --------------
   ------------------------------------------------------

   procedure Halt(Self : Instance; Message : String) is
   begin
      raise Parsing_Error with Message;
   end Halt;

   procedure Error(Self : Instance; Message : String) is
      Token : Compiler.Lexer.Token 
         renames Self.Lexer.All_Tokens.all(Self.Last);
   begin
      Self.Error(Message, Token.Line, Token.First);
   end Error;

   procedure Error
      (Self    : Instance; 
       Message : String;
       Token   : Compiler.Lexer.Token)
   is begin
      Self.Error(Message, Token.Line, Token.First);
   end Error;

   procedure Error(Self : Instance; Message : String; Line, Column : Positive) is
   begin
      Self.Halt
         ("Parsing Error @ " 
          & Strings.Image(Line) & ":" & Strings.Image(Column)
          & " => " & Message);
   end Error;

   ------------------------------------------------------
   ------------- Syntax Parsing Operations --------------
   ------------------------------------------------------

   -- Expression parsing
   package Expressions is
      function Expression(Self : in out Instance) return AST.Node'Class;
      function Relation(Self : in out Instance) return AST.Node'Class;
      function Simple_Expression(Self : in out Instance) return AST.Node'Class;
      function Term(Self : in out Instance) return AST.Node'Class;
      function Factor(Self : in out Instance) return AST.Node'Class;
      function Primary(Self : in out Instance) return AST.Node'Class;
   end Expressions;  

   package body Expressions is separate;

   function Expression(Self : in out Instance) return AST.Node'Class renames Expressions.Expression;
   function Relation(Self : in out Instance) return AST.Node'Class renames Expressions.Relation;
   function Simple_Expression(Self : in out Instance) return AST.Node'Class renames Expressions.Simple_Expression;
   function Term(Self : in out Instance) return AST.Node'Class renames Expressions.Term;
   function Factor(Self : in out Instance) return AST.Node'Class renames Expressions.Factor;
   function Primary(Self : in out Instance) return AST.Node'Class renames Expressions.Primary;

end Compiler.Parser;
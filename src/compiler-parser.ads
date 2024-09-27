-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

private with Compiler.Lexer;

with Ada.Streams;
with Compiler.Strings;
with Compiler.Tokens;
with Compiler.Attributes;
with Compiler.Aspects;
with Compiler.Pragmas;


-- Top level package for the parser
package Compiler.Parser is

   -- Core parser type
   type Instance is tagged limited private;

   -- Iterates through the stream and parses the tokens generted
   -- by the internal lexer
   procedure Run(Self : in out Instance; Filename : Standard.String);
   procedure Run
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   -- Error when parsing.  More information in Message field
   Parsing_Error : exception;

private

   type Instance is tagged limited record
      Lexer : Compiler.Lexer.Instance;
      Next  : Positive := 1;
      Last  : Positive := 1;
   end record;

   -- Gets the next token
   procedure Scan(Self : in out Instance);

   -- Determines if the current token matches the specified item
   -- and progresses if so.  Returns a boolean indicating if a match was
   -- found
   procedure Match
      (Self       : in out Instance;
       Identifier :        Strings.String);
   procedure Match
      (Self  : in out Instance; 
       Token :        Tokens.Token_Kind); 
   function Match
      (Self  : in out Instance; 
       Token :        Tokens.Token_Kind) 
       return Boolean;

   -- Matches and validates an identifier.  Retunrs
   -- pertinent info if a valid match, or halts parsing
   -- if not.
   procedure Attribute_Identifier
      (Self      : in out Instance;
       Info      :    out Attributes.Info);
   procedure Aspect_Identifier
      (Self      : in out Instance;
       Info      :    out Aspects.Info);
   procedure Pragma_Identifier
      (Self      : in out Instance;
       Info      :    out Pragmas.Info);

   -- Returns the Token_Kind value for the specified token
   function Token_Kind(Self : Instance; Index : Positive) return Tokens.Token_Kind
      with Inline;
   
   -- Returns the Token_Kind value for the last token matched
   function Token_Kind(Self : Instance) return Tokens.Token_Kind
      with Inline;

   -- Returns the string value for the specified token
   function Token_Value(Self : Instance; Index : Positive) return Strings.String
      with Inline;
   
   -- Returns the string value for the last token matched
   function Token_Value(Self : Instance) return Strings.String
      with Inline;

   -- Low level output operations
   procedure Halt(Self : Instance; Message : String);
   procedure Expected(Self : Instance; Message : String) with Inline;
   procedure Expected(Self : Instance; Message : String; Token : Lexer.Token) with Inline;
   procedure Expected(Self : Instance; Message : String; Line, Column : Positive)
      with Inline;

end Compiler.Parser;
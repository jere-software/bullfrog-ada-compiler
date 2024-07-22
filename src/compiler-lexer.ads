-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

private with Ada.Finalization;

with Ada.Streams;
with Ada.Containers.Vectors;
with Compiler.Tokens;
with Compiler.Strings;

-- Top level package for the lexical scanner
package Compiler.Lexer is

   ------------------------------------------------------
   ----------------- Token Information ------------------
   ------------------------------------------------------

   -- Core Token type
   type Token
      (Kind  : Tokens.Token_Kind := Tokens.Comment) 
   is record
      Value  : Strings.Holder := Strings.Empty_Holder;
      Line   : Positive       := 1;
      First  : Positive       := 1;
      Last   : Positive       := 1;
   end record;

   -- Prints the information for the supplied token to STDOUT
   procedure Debug(Self : Token);

   ------------------------------------------------------
   -------------------- Token Lists ---------------------
   ------------------------------------------------------

   package Vectors is new Ada.Containers.Vectors(Positive, Token);

   -- Core token list type
   subtype Token_List is Vectors.Vector;
   Empty_Token_List : constant Token_List := Vectors.Empty_Vector;

   ------------------------------------------------------
   ------------------ Lexer Operation -------------------
   ------------------------------------------------------

   -- Core lexer type
   type Instance is tagged limited private;

   -- Gets all tokens.  This operation does the following:
   --   Calls Initialize
   --   Iterates through the stream calling Get_Token
   --      until the stream ends
   procedure Run(Self : in out Instance; Filename : Standard.String);
   procedure Run
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   -- Individual operations for getting tokens from a stream one
   -- by one.  Call Initialize first, then use Is_Running and
   -- Get_Token to iterate through tokens
   procedure Initialize(Self : in out Instance); -- Resets parser state
   function Is_Running(Self : Instance) return Boolean with Inline;
   procedure Get_Token
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Is_Running;

   -- Returns the current list of tokens found so far
   function All_Tokens(Self : aliased Instance) 
      return not null access constant Token_List
   with Inline;
   
   -- Last token info
   function Token_Kind(Self : Instance) return Tokens.Token_Kind
      with Inline, 
         Pre => Self.All_Tokens.Length not in 0;
   function Token_Value(Self : Instance) return Strings.String
      with Inline, 
         Pre => Self.All_Tokens.Length not in 0;
   function Token_Line(Self : Instance) return Positive
      with Inline, 
         Pre => Self.All_Tokens.Length not in 0;
   function Token_First(Self : Instance) return Positive
      with Inline, 
         Pre => Self.All_Tokens.Length not in 0;
   function Token_Last(Self : Instance) return Positive
      with Inline, 
         Pre => Self.All_Tokens.Length not in 0;

   -- Debug info on the last token found
   procedure Debug(Self : Instance);

   -- Error when lexing.  More information in Message field
   Lexical_Error : exception;

private

   -- local declaration so all child packages use the same
   -- underlying character type
   subtype Character is Strings.Character; 
   use all type Character;

   -- local declaration so all child packages use the same
   -- underlying string type
   subtype String is Strings.String;
   use all type String;

   -- Lexer state type
   type Status is 
      (Idle, 
       Running);

   type Instance is new Ada.Finalization.Limited_Controlled with record
      Next_In     : Character          := Strings.Space;  -- Next character to process
      Last_In     : Character          := Strings.Space;  -- Last character processed
      State       : Lexer.Status       := Idle;
      Tokens      : aliased Token_List := Empty_Token_List;
      Line        : Positive           := 1;
      Column      : Positive           := 1;
      Next_Line   : Positive           := 1;
      Next_Column : Positive           := 1;
   end record;

   -- Token insertion operations
   procedure Add_Token
      (Self  : in out Instance; 
       Kind  : Tokens.Token_Kind;
       Value : String;
       Line  : Positive;
       First : Positive;
       Last  : Positive)
   with Inline;
   
   -- Last token update operations
   procedure Set_Token_Value(Self : in out Instance; Value : String)
      with Inline,
         Pre => Self.Tokens.Length not in 0;
   procedure Set_Token_Last(Self : in out Instance; Value : Positive)
      with Inline,
         Pre => Self.Tokens.Length not in 0;

   -- Top level scanning operations that generate tokens
   procedure Get_Comment -- Usually called after Get_Operator
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Token_Kind in Tokens.Comment;
   procedure Get_Name
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Strings.Is_Alpha(Self.Next_In);
   procedure Get_Operator -- Can return Comment tokens
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Strings.Is_Operator(Self.Next_In);
   procedure Get_Numeric_Literal -- Can return Operator_Range tokens
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Strings.Is_Digit(Self.Next_In);
   procedure Get_Character_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Next_In in Strings.Apostrophe;
   procedure Get_String_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Next_In in Strings.Quote;

   -- Low level input operations
   procedure Get_Character
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.State /= Idle;
   procedure Skip_Whitespace
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.State /= Idle;

   -- Low level output operations
   procedure Halt(Self : Instance; Message : String);
   procedure Expected(Self : Instance; Message : String) with Inline;
   procedure Expected(Self : Instance; Message : String; Line, Column : Positive)
      with Inline;

end Compiler.Lexer;
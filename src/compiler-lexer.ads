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

   -- Core token types
   subtype Token is Tokens.Token;  
   subtype Token_List is Tokens.Token_List;
   
   -- Import all operators
   use all type Token;
   use all type Token_List;

   -- Prints the information for the supplied token to STDOUT
   procedure Debug(Self : Token);

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

   -- Returns the current list of tokens found so far
   function All_Tokens(Self : aliased Instance) 
      return not null access constant Token_List
   with Inline;

   -- Turn on the ability to save comments as tokens
   procedure Enable_Comments(Self : in out Instance);

   -- Debug info on the last token found
   procedure Debug(Self : Instance);

   -- Error when lexing.  More information in Message field
   Lexical_Error : exception;

private

   --- Intializes the lexer
   procedure Initialize -- Resets lexer state
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class); 

   -- Lexer status
   function Is_Running(Self : Instance) return Boolean with Inline;
   function Not_Running(Self : Instance) return Boolean with Inline;

   -- local declaration so all child packages use the same
   -- underlying character type
   subtype Character is Strings.Character; 
   use all type Character;

   -- local declaration so all child packages use the same
   -- underlying string type
   subtype String is Strings.String;
   use all type String;

   -- local renaming for ease of use
   subtype Line_Number is Tokens.Line_Number;
   subtype Column_Number is Tokens.Column_Number;
   use all type Line_Number;
   use all type Column_Number;

   -- Lexer state type
   type Status is 
      (Off,            -- Finished
       Running,        -- Looking for characters
       End_Of_Stream); -- Last character found

   type Instance is new Ada.Finalization.Limited_Controlled with record
      Next        : Character          := Strings.Nul;  -- Next character to process
      Peek        : Character          := Strings.Nul;  -- Future character to process
      Last_Token  : Tokens.Token_Kind  := Tokens.End_Of_Stream;
      State       : Lexer.Status       := Off;
      Tokens      : aliased Token_List := Compiler.Tokens.Empty_Token_List;
      Line        : Line_Number        := 1;
      Column      : Column_Number      := 1;
      Peek_Line   : Line_Number        := 1;
      Peek_Column : Column_Number      := 1;
      Comments_On : Boolean            := False;
   end record;

   -- Low level input operations
   procedure Advance
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class);
   procedure Skip_Whitespace
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class);
   procedure Skip_Comment
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Next in Strings.Minus and Self.Peek in Strings.Minus;

   -- Low level output operations
   procedure Halt(Self : Instance; Message : String)
      with No_Return;
   procedure Error(Self : Instance; Message : String) 
      with Inline, No_Return;
   procedure Error
      (Self    : Instance; 
       Message : String; 
       Line    : Line_Number; 
       Column  : Column_Number) with Inline, No_Return;

   -- Token creation
   function Make
      (Self  : in out Instance;
       Kind  : Tokens.Token_Kind; 
       Value : String; 
       First : Column_Number) 
       return Token;
   function End_Of_Stream(Self : in out Instance) return Token
      with Pre  =>    Self.Not_Running 
                  and Self.Last_Token not in Tokens.End_Of_Stream,
           Post => Self.Last_Token in Tokens.End_Of_Stream;

   -- Tokenization
   function Get_Next_Token
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
      with Pre  => Self.Last_Token not in Tokens.End_Of_Stream or Self.Is_Running,
           Post => Self.Last_Token not in Tokens.End_Of_Stream or Self.Not_Running
                or Get_Next_Token'Result.Kind in Tokens.Comment;
   function Get_Identifier
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
      with Pre => Self.Is_Running and Strings.Is_Letter(Self.Next);
   function Get_Numeric_Literal
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
      with Pre => Self.Is_Running and Strings.Is_Numeral(Self.Next);
   function Get_String_Literal
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
      with Pre => Self.Is_Running and Self.Next in Strings.Quote;
   function Get_Character_Or_Apostrophe
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
      with Pre => Self.Is_Running and Self.Next = Strings.Apostrophe;
   function Get_Comment
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
      with Pre =>  Self.Is_Running 
               and Self.Next = Strings.Minus 
               and Self.Peek = Strings.Minus;
end Compiler.Lexer;
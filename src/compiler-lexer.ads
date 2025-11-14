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

   -- Specialized numeric types for token information
   subtype Line_Number   is Tokens.Line_Number;
   subtype Column_Number is Tokens.Column_Number;

   -- Core token type
   subtype Token is Tokens.Token;  
   
   -- Import all operators
   use all type Token;
   use all type Line_Number;
   use all type Column_Number;

   -- Image functions
   function Image(Item : Line_Number) return String 
      renames Tokens.Image;
   function Image(Item : Column_Number) return String 
      renames Tokens.Image;

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
   procedure Initialize -- Resets lexer state
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class); 
   function Is_Running(Self : Instance) return Boolean with Inline;
   function Not_Running(Self : Instance) return Boolean with Inline;
   procedure Get_Token
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Is_Running;

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

   -- Last token info
   function Token_Kind(Self : Instance) return Tokens.Token_Kind
      with  Inline, 
            Pre => Self.All_Tokens.Length not in 0;
   function Token_Value(Self : Instance) return Strings.String
      with  Inline, 
            Pre => Self.All_Tokens.Length not in 0;

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
      (Off,          -- Finished
       Running,      -- Looking for characters
       End_Of_File); -- Last character found

   type Instance is new Ada.Finalization.Limited_Controlled with record
      Next        : Character          := Strings.Space;  -- Next character to process
      Peek        : Character          := Strings.Space;  -- Future character to process
      State       : Lexer.Status       := Off;
      Tokens      : aliased Token_List := Empty_Token_List;
      Line        : Line_Number        := 1;
      Column      : Column_Number      := 1;
      Next_Line   : Line_Number        := 1;
      Next_Column : Column_Number      := 1;
      Comments_On : Boolean            := False;
   end record;

   -- Token insertion operations
   procedure Add_Token
      (Self  : in out Instance; 
       Kind  : Tokens.Token_Kind;
       Value : String;
       Line  : Line_Number;
       First : Column_Number;
       Last  : Column_Number)
   with Inline;
   
   -- Last token update operations
   procedure Set_Token_Value(Self : in out Instance; Value : String)
      with Inline,
         Pre => Self.Tokens.Length not in 0;
   procedure Set_Token_Last(Self : in out Instance; Value : Column_Number)
      with Inline,
         Pre => Self.Tokens.Length not in 0;

   -- Top level scanning operations that generate tokens
   procedure Get_Comment -- Usually called after Get_Delimiter
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Token_Kind in Tokens.Comment;
   procedure Skip_Comment -- Usually called after Get_Delimiter
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Token_Kind in Tokens.Comment;
   procedure Get_Identifier -- Can return Attribute or Pragma_ID tokens
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Strings.Is_Letter(Self.Next);
   procedure Get_Delimiter -- Can return Comment tokens
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Strings.Is_Delimiter(Self.Next);
   procedure Get_Numeric_Literal -- Can return Delimiter_Range tokens
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Strings.Is_Numeral(Self.Next);
   procedure Get_Character_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Next in Strings.Apostrophe;
   procedure Get_String_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Next in Strings.Quote;

   -- Low level input operations
   procedure Advance
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class);
   procedure Skip_Whitespace
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      with Pre => Self.Is_Running;

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

end Compiler.Lexer;
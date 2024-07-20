-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Holders;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;

-- Compiler string interface.  This package is meant to be
-- an interface for the whole compiler.  So it can be 
-- replaced with a Wide_ or Wide_Wide_ implementation or
-- another custom implementation
package Compiler.Strings is

   -- Individual character type
   subtype Character is Standard.Character;

   -- Type for a group of characters
   -- NOTE:  String must be a an array type of Characters
   -- with Positive indexing or a custom type with both
   -- indexing aspects defined with a function that takes
   -- a positive index and returns a Character.
   subtype String is Standard.String;

   -- Local type operations
   function "="(L,R : String) return Boolean renames Standard."=";
   function "&"(L,R : String) return String renames Standard."&";
   function "="(L,R : Character) return Boolean renames Standard."=";
   function "&"(L,R : Character) return String renames Standard."&";
   function "&"(L : String; R : Character) return String renames Standard."&";

   -- Text_IO must have the same basic specification as
   -- Ada.Text_IO and interact with type String above
   package Text_IO     renames Ada.Text_IO;
   
   -- Text_Streams must have the same basic specification as
   -- Ada.Text_IO.Text_Streams and interact with type String above
   package Text_Streams renames Text_IO.Text_Streams;

   -- Local types from above packages
   subtype File_Type is Text_IO.File_Type;
   subtype Stream_Access is Text_Streams.Stream_Access;

   -- These packages aren't required outside of this one.
   -- They are only here for ease of use below.
   package Latin_1 renames Ada.Characters.Latin_1;
   package Handling renames Ada.Characters.Handling;

   -- Local redefines
   Space             : constant Character := ' ';
   Tab               : constant Character := Latin_1.HT;
   Carriage_Return   : constant Character := Latin_1.CR;
   New_Line          : constant Character := Latin_1.LF;
   Quote             : constant Character := '"';
   Apostrophe        : constant Character := ''';
   Underscore        : constant Character := '_';
   Plus              : constant Character := '+';
   Minus             : constant Character := '-';
   Ampersand         : constant Character := '&';
   Bar               : constant Character := '|';
   Equals            : constant Character := '=';
   Open_Parenthesis  : constant Character := '(';
   Close_Parenthesis : constant Character := ')';
   Open_Bracket      : constant Character := '[';
   Close_Bracket     : constant Character := ']';
   Semicolon         : constant Character := ';';
   Comma             : constant Character := ',';
   Asterisk          : constant Character := '*';
   Forward_Slash     : constant Character := '/';
   Less_Than         : constant Character := '<';
   Greater_Than      : constant Character := '>';
   Colon             : constant Character := ':';
   Period            : constant Character := '.';
   At_Sign           : constant Character := '@';
   Pound             : constant Character := '#';
   Exponent_Lower    : constant Character := 'e';
   Exponent_Upper    : constant Character := 'E';
   Zero              : constant Character := '0';

   -- Numeric literal types
   subtype Digit is Character range '0' .. '9';
   subtype Extended_Digit is Character with Static_Predicate => 
      Extended_Digit in 'a'..'f' | 'A'..'F';
   subtype Hex_Digit is Character with Static_Predicate =>
      Hex_Digit in Digit | Extended_Digit;

   -- Lexical separator types
   subtype Whitespace    is Character with Static_Predicate =>
      Whitespace in Tab .. New_Line | Space;

   -- Operator detection types
   subtype Operator_1_Character is Character with Static_Predicate =>
      Operator_1_Character in 
           Plus 
         | Ampersand 
         | Bar 
         | Open_Parenthesis 
         | Close_Parenthesis 
         | Open_Bracket 
         | Close_Bracket 
         | Semicolon 
         | Comma
         | Apostrophe
         | At_Sign;
   subtype Operator_2_Character is Character with Static_Predicate =>
      Operator_2_Character in 
           Minus 
         | Asterisk
         | Forward_Slash
         | Less_Than
         | Greater_Than
         | Colon
         | Equals
         | Period;
   
   -- Type conversion operations
   function To_String(Value : Standard.String) return String
      is (Value) with Inline; -- Use Ada.Characters.Conversions for Wide_*
   function Image(Value : Integer) return String is
      (if Value < 0 then 
         Value'Image
       else 
         Ada.Strings.Fixed.Trim(Value'Image, Ada.Strings.Left));
   function Numeric_Value(Value : Hex_Digit) return Natural is
      (case Value is
         when '0'..'9' => Character'Pos(Value) - Character'Pos('0'),
         when 'a'..'f' => Character'Pos(Value) - Character'Pos('a') + 10,
         when 'A'..'F' => Character'Pos(Value) - Character'Pos('A') + 10);
   function Pos(Value : Character) return Natural is
      (Character'Pos(Value));
   function Hash(Value : String) return Ada.Containers.Hash_Type
      renames Ada.Strings.Hash;

   -- Format operations
   function To_Lower(Value : Character) return Character 
      renames Handling.To_Lower;
   function To_Lower(Value : String) return String 
      renames Handling.To_Lower;
   function To_Upper(Value : Character) return Character 
      renames Handling.To_Upper;
   function To_Upper(Value : String) return String 
      renames Handling.To_Upper;

   -- Utility operations for parsing
   function Is_Alpha(Value : Character) return Boolean
      renames Handling.Is_Letter;
   function Is_Digit(Value : Character) return Boolean
      renames Handling.Is_Digit;
   function Is_Hexadecimal_Digit(Value : Character) return Boolean
      renames Handling.Is_Hexadecimal_Digit;
   function Is_Alphanumeric(Character : Strings.Character) return Boolean
      renames Handling.Is_Alphanumeric;
   function Is_Newline(Character : Strings.Character) return Boolean
      renames Handling.Is_Line_Terminator;
   function Is_Graphic(Character : Strings.Character) return Boolean
      renames Handling.Is_Graphic;
   function Is_Name(Character : Strings.Character) return Boolean is
      (Character = Underscore 
       or else Is_Alphanumeric(Character));
   function Is_Integer(Character : Strings.Character) return Boolean is
      (Character in Underscore | Pound | Plus | Minus 
       or else Is_Hexadecimal_Digit(Character));
   function Is_Real(Character : Strings.Character) return Boolean is
      (Character in Underscore | Period | Plus | Minus 
       | Exponent_Lower | Exponent_Upper 
       or else Is_Digit(Character));
   function Is_Space(Character : Strings.Character) return Boolean is
      (Character in Whitespace);
   function Is_Operator(Character : Strings.Character) return Boolean is
      (Character in Operator_1_Character | Operator_2_Character);

   -- Utility operations for getting input data
   function Stream(File : File_Type) return Stream_Access
      renames Text_Streams.Stream;

   -- String holder type, operations, and declarations
   package Holders is new Ada.Containers.Indefinite_Holders(String);
   type Holder is new Holders.Holder with null record;
   
   function New_String(Value : String) return Holder
      renames To_Holder;
   function Get(Self : Holder) return String
      renames Element;
   procedure Set(Self : in out Holder; Value : String)
      renames Replace_Element;

   Empty_Holder : constant Holder := (Holders.Empty_Holder with null record);

end Compiler.Strings;
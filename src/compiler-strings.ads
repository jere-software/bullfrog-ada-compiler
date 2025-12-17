-- Copyright (C) 2024 - 2025
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
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Streams;

-- Compiler string interface.  This package is meant to be
-- an interface for the whole compiler.  So it can be 
-- replaced with a Wide_ or Wide_Wide_ implementation or
-- another custom implementation
package Compiler.Strings is

   -- Individual character type
   subtype Character is Standard.Character;

   -- Type for a group of characters
   -- NOTE:  String must be a an array type of Characters
   -- with Positive indexing.
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
   Nul               : constant Character := Latin_1.Nul;
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
   Lower_A           : constant Character := 'a';
   Lower_E           : constant Character := 'e';
   Upper_A           : constant Character := 'A';
   Upper_E           : constant Character := 'E';
   Zero              : constant Character := '0';

   -- Numeric literal types
   subtype Numeral_Digit is Character range '0' .. '9';
   subtype Upper_Hex is Character range 'A' .. 'F';
   subtype Lower_Hex is Character range 'a' .. 'f';
   subtype Extended_Digit is Character with Static_Predicate => 
      Extended_Digit in Upper_Hex | Lower_Hex;
   subtype Hex_Digit is Character with Static_Predicate =>
      Hex_Digit in Numeral_Digit | Extended_Digit;
   subtype E is Character with Static_Predicate =>
      E in Upper_E | Lower_E;

   -- Lexical separator types
   subtype Whitespace    is Character with Static_Predicate =>
      Whitespace in Tab .. Carriage_Return | Space;

   -- Delimiter detection types
   subtype Delimiter_1_Character is Character with Static_Predicate =>
      Delimiter_1_Character in 
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
   subtype Delimiter_2_Character is Character with Static_Predicate =>
      Delimiter_2_Character in 
           Minus 
         | Asterisk
         | Forward_Slash
         | Less_Than
         | Greater_Than
         | Colon
         | Equals
         | Period;

   -- Type used for Hex_Digit conversions
   type Number_Base is range 2 .. 16;
   
   -- Type conversion operations
   function To_String(Value : Standard.String) return String
      is (Value) with Inline; -- Use Ada.Characters.Conversions for Wide_*
   function To_Standard_String(Value : String) return Standard.String
      is (Value) with Inline; -- Use Ada.Characters.Conversions for Wide_*
   function Image(Value : Integer) return String is
      (if Value < 0 then 
         Value'Image
       else 
         Ada.Strings.Fixed.Trim(Value'Image, Ada.Strings.Left))
       with Inline;
   function Value(Value : String) return Integer is
      (Integer'Value(Value)) with Inline;
   function Numeric_Value(Value : Character) return Number_Base'Base is
      (case Value is
         when Numeral_Digit => Character'Pos(Value) - Character'Pos(Zero),
         when Lower_Hex     => Character'Pos(Value) - Character'Pos(Lower_A) + 10,
         when Upper_Hex     => Character'Pos(Value) - Character'Pos(Upper_A) + 10,
         when others        => Number_Base'Last)
       with Static, Inline;
   function Pos(Value : Character) return Natural is
      (Character'Pos(Value)) with Inline;
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
   function Is_Letter(Value : Character) return Boolean
      renames Handling.Is_Letter;
   function Is_Alphanumeric(Character : Strings.Character) return Boolean
      renames Handling.Is_Alphanumeric;
   function Is_Graphic(Character : Strings.Character) return Boolean
      renames Handling.Is_Graphic;
   function Is_Line_Terminator(Character : Strings.Character) return Boolean
      renames Handling.Is_Line_Terminator;
   function Is_Space(Character : Strings.Character) return Boolean
      renames Handling.Is_Space;
   function Is_Mark(Character : Strings.Character) return Boolean
      renames Handling.Is_Mark;
   function Is_Punctuation_Connector(Character : Strings.Character) return Boolean
      renames Handling.Is_Punctuation_Connector;
   function Is_Identifier(Value : Character) return Boolean is
      (Is_Alphanumeric(Value) or else Is_Mark(Value)) with Inline;
   function Is_Numeral(Value : Character) return Boolean
      is (Value in Numeral_Digit) with Static, Inline;
   function Is_Numeral(Value : Character; Base : Number_Base) return Boolean
      is (Numeric_Value(Value) < Base) with Static, Inline;
   function Is_Underline(Value : Character) return Boolean is
      (Value = Underscore) with Static, Inline;
   function Is_Whitespace(Value : Character) return Boolean is
      (        Is_Space(Value) 
       or else Value = Tab 
       or else Is_Line_Terminator(Value)) with Inline;
   function Is_Delimiter(Character : Strings.Character) return Boolean is
      (Character in Delimiter_1_Character | Delimiter_2_Character)
      with Static, Inline;
   function Is_String(Item : Character) return Boolean is
      (Item /= Quote and then Is_Graphic(Item)) with Inline;

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

   function "+"(Item : Holder) return String
      renames Element;
   function "+"(Item : String) return Holder
      renames To_Holder;
   function "&"(L : Holder; R : String) return Holder
      is (+(L.Constant_Reference & R));
   function "&"(L : String; R : Holder) return String
      is (L & R.Constant_Reference);

   Empty_Holder : constant Holder := (Holders.Empty_Holder with null record);

   package Vectors is new Ada.Containers.Indefinite_Vectors(Positive, String);
   type Vector is new Vectors.Vector with null record;

   -- Primary stream type.  Read only, create using constructor
   type Read_Only_Stream(<>) is new Ada.Streams.Root_Stream_Type with private;

   -- Constructing function
   function Make(Value : String) return Read_Only_Stream;

   -- Ada.Streams override
   overriding 
   procedure Read
      (Stream : in out Read_Only_Stream;
       Item   :    out Ada.Streams.Stream_Element_Array;
       Last   :    out Ada.Streams.Stream_Element_Offset);

   -- Ada.Streams override, not implemented, raises an exception
   overriding
   procedure Write
      (Stream : in out Read_Only_Stream;
       Item   : in     Ada.Streams.Stream_Element_Array);

private

   type Read_Only_Stream 
      (Capacity : Natural)
   is new Ada.Streams.Root_Stream_Type with record
      String     : Strings.String(1..Capacity);
      Remaining  : Natural  := 0;
      Index      : Positive := 1;
   end record;

end Compiler.Strings;
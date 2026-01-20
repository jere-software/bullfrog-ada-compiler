-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

-- Scans the stream for a numeric literal (real or integer)
separate (Compiler.Lexer) 
function Get_Numeric_Literal
   (Self   : in out Instance; 
    Stream : not null access Ada.Streams.Root_Stream_Type'Class)
    return Token
is 

   use Strings;

   -- Curren number parsing state
   type Scan_State is (Decimal, Decimal_Real, Based, Based_Real, Exponent);

   Buffer  : Character_Vector       := Empty(Default_Character_Vector_Size);
   First   : constant Column_Number := Self.Column;
   Base    : Number_Base            := 10;
   Is_Real : Boolean                := False;
   State   : Scan_State            := Decimal;

   -- Need a one parameter version that still accounts for 
   -- the current base.  This will be supplied to Generic_Scan
   function Is_Numeral(Item : Character) return Boolean is
      (Is_Numeral(Item, Base));

   -- Numeral scanner.  Will handle digits and underlines, but
   -- will not handle separators
   procedure Scan_Numeral is new Generic_Scan_With_Connector
      (Is_Character => Is_Numeral,
       Is_Connector => Is_Underline,
       Target_Name  => "Numeric literal");

   -- Updates the numeral base when transitioning from 
   -- Decimal state to Based state
   procedure Update_Base
      with Pre => State = Decimal and Self.Next = Pound and not Is_Real;
   procedure Update_Base is
   begin
      Base := Number_Base(Value(Buffer.Copy));
   exception
      when Constraint_Error => Self.Error("Invalid value for base: " & Buffer.Copy);
   end Update_Base;

   -- Adds the next character to the buffer and
   -- moves forward in the stream
   procedure Append_Next is
   begin
      Buffer.Append(Self.Next);
      Self.Advance(Stream);
   end Append_Next;

   -- State transition logic.  Updates local variables based on
   -- the New_State value supplied.  It will munch any separators.
   procedure Set_State_To(New_State : Scan_State)
      with Pre => (case State is
                     when Decimal      => New_State in Decimal_Real | Based | Exponent,
                     when Decimal_Real => New_State in Exponent,
                     when Based        => New_State in Based_Real | Exponent,
                     when Based_Real   => New_State in Exponent,
                     when Exponent     => False); 
   procedure Set_State_To(New_State : Scan_State) is
   begin
      case New_State is
         when Decimal_Real => Is_Real := True;
         when Based        => Update_Base;
         when Based_Real   => Is_Real := True;
         when Exponent     => Base    := 10;
            case Self.Peek is
               when Plus   => Append_Next;
               when Minus  => Append_Next;
                  if not Is_Real then
                     Self.Error("Integer literal cannot have negative exponent");
                  end if;
               when others => null;
            end case;
         when others => raise Program_Error with 
            "Invalid scan state for numeric literal";
      end case;

      State := New_State;
      Append_Next;
   end Set_State_To;

   -- Indicates if this is a range delimiter instead of a decimal period
   function Is_Range return Boolean is (Self.Peek = Period) 
      with Inline, Pre => Self.Next = Period and State = Decimal;

   -- Indicates if an exponent is expected.  Munches the pound sign
   -- regardless of result.
   function No_Exponent return Boolean
      with Pre => State in Based | Based_Real
                  and Self.Next = Pound;
   function No_Exponent return Boolean is
   begin
      Append_Next; -- Munch the pound sign
      return (Self.Next not in E);
   end No_Exponent;

   -- Error messages
   Invalid_Character : constant String := "Invalid character for numeric literal";
   Missing_Based     : constant String := "A numeral digit expected after " & Pound;
   Missing_Real      : constant String := "A numeral digit expected after " & Period;
   Missing_Exponent  : constant String := "A numeral digit expected for exponent";
   Incomplete_Number : constant String := "Unexpected end to numeric literal";
   Incomplete_Based  : constant String := "Based literal must end with a " & Pound;
   Digit_Too_High    : constant String := "Digit too high for supplied base";
   No_Real_Base      : constant String := "Based literal cannot have a real base";
   Base_10_Exponent  : constant String := "Exponent must be expressed in base 10";
   Base_10_Numeral   : constant String := "Decimal literal must be expressed in base 10";
   Not_Separated     : constant String := "Literals must be separated by whitespace or a delimiter";
   
   -- Ensures the next character is a valid numeral to scan
   procedure Validate_Has_Numeral with Pre => State not in Decimal;
   procedure Validate_Has_Numeral is
   begin
      if not Is_Numeral(Self.Next) then
         if Self.Not_Running or else Is_Whitespace(Self.Next) then
            Self.Error(Incomplete_Number);
         else
            case Self.Next is
               when Hex_Digit =>
                  case State is
                     when Decimal | Decimal_Real => Self.Error(Base_10_Numeral);
                     when Based   | Based_Real   => Self.Error(Digit_Too_High);
                     when Exponent               => Self.Error(Base_10_Exponent);
                  end case;
               when others =>
                  case State is
                     when Decimal      => Self.Error(Invalid_Character);
                     when Decimal_Real => Self.Error(Missing_Real);
                     when Based        => Self.Error(Missing_Based);
                     when Based_Real   => Self.Error(Missing_Real);
                     when Exponent     => Self.Error(Missing_Exponent);
                  end case;
            end case;
         end if;
      end if;
   end Validate_Has_Numeral;

   -- Ensures the literal isn't concatenated to an identifier
   -- or another numeric literal, which would be an error
   procedure Validate_End_Of_Literal is
   begin
      if Is_Identifier(Self.Next) then
         case Self.Next is
            when Numeral_Digit =>
               Self.Error(Not_Separated);
            when Extended_Digit =>
               case State is
                  when Decimal | Decimal_Real => Self.Error(Base_10_Numeral);
                  when Exponent               => Self.Error(Base_10_Exponent);
                  when others                 => Self.Error(Invalid_Character);
               end case;
            when others => 
               Self.Error(Invalid_Character);
         end case;
      end if;
   end Validate_End_Of_Literal;

begin

   -- Decimal   Based     Real                Exponent
   -- ------------------------------------------------
   -- numeral
   -- numeral                       E|e [+|-] numeral
   -- numeral           . numeral
   -- numeral           . numeral   E|e [+|-] numeral
   -- numeral # numeral           #
   -- numeral # numeral           # E|e [+|-] numeral
   -- numeral # numeral . numeral #
   -- numeral # numeral . numeral # E|e [+|-] numeral
   loop
      -- Get full numeral, including underlines, but not separators
      Scan_Numeral(Self, Stream, Buffer);

      -- Scan any numeral separators
      case State is
         when Decimal =>
            case Self.Next is
               when Period   => exit when Is_Range; Set_State_To(Decimal_Real);
               when Pound    =>                     Set_State_To(Based);
               when E        =>                     Set_State_To(Exponent);
               when others   => exit;
            end case;
         when Decimal_Real =>
            case Self.Next is
               when E      => Set_State_To(Exponent);
               when Pound  => Self.Error(No_Real_Base);
               when others => exit;
            end case;
         when Based =>
            case Self.Next is
               when Period    =>                        Set_State_To(Based_Real);
               when Pound     => exit when No_Exponent; Set_State_To(Exponent);
               when Hex_Digit =>                        Self.Error(Digit_Too_High);
               when others    =>                        Self.Error(Incomplete_Based);
            end case;
         when Based_Real =>
            case Self.Next is
               when Pound     => exit when No_Exponent; Set_State_To(Exponent);
               when Hex_Digit =>                        Self.Error(Digit_Too_High);
               when others    =>                        Self.Error(Incomplete_Based);
            end case;
         when Exponent => 
            exit;
      end case;

      -- Ensure there is a numeral to scan before
      -- continuing the loop
      Validate_Has_Numeral;

   end loop;

   -- Ensure the end of the literal is valid
   Validate_End_Of_Literal;
   if Is_Real then
      return Self.Make(Tokens.Real_Literal, Buffer.Copy, First);
   else
      return Self.Make(Tokens.Integer_Literal, Buffer.Copy, First);
   end if;
   
end Get_Numeric_Literal;
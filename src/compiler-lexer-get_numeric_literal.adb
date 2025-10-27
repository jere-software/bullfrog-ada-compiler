-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

-- Parses the stream for a numeric literal (real or integer)
separate (Compiler.Lexer) 
procedure Get_Numeric_Literal
   (Self   : in out Instance; 
    Stream : not null access Ada.Streams.Root_Stream_Type'Class)
is 

   use Strings;

   -- Curren number parsing state
   type Parse_State is (Decimal, Decimal_Real, Based, Based_Real, Exponent);

   Buffer  : Character_Vector       := Empty(Default_Character_Vector_Size);
   First   : constant Column_Number := Self.Column;
   Base    : Number_Base            := 10;
   Is_Real : Boolean                := False;
   State   : Parse_State            := Decimal;

   -- Need a one parameter version that still accounts for 
   -- the current base.  This will be supplied to Generic_Parse
   function Is_Numeral(Item : Character) return Boolean is
      (Is_Numeral(Item, Base));

   -- Numeral parser.  Will handle digits and underlines, but
   -- will not handle separators
   procedure Parse_Numeral is new Generic_Parse_With_Connector
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
   procedure Set_State_To(New_State : Parse_State)
      with Pre => (case State is
                     when Decimal      => New_State in Decimal_Real | Based | Exponent,
                     when Decimal_Real => New_State in Exponent,
                     when Based        => New_State in Based_Real | Exponent,
                     when Based_Real   => New_State in Exponent,
                     when Exponent     => False); 
   procedure Set_State_To(New_State : Parse_State) is
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
            "Invalid parse state for numeric literal";
      end case;

      State := New_State;
      Append_Next;
   end Set_State_To;

   -- Indicates if this is a range operator instead of a decimal period
   function Is_Range return Boolean is (Self.Peek = Period) 
      with Inline, Pre => Self.Next = Period and State = Decimal;

   -- Indicates if the current buffer is ending with the correct character
   function Finished return Boolean is (not Is_Identifier(Self.Next)) 
      with Inline, Pre => State in Decimal | Decimal_Real | Exponent;

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
   Incomplete_Based  : constant String := "Based literal must end with a " & Pound;
   Digit_Too_High    : constant String := "Digit too high for supplied base";
   Too_Many_Periods  : constant String := "Real literal cannot have multiple decimal periods";
   No_Real_Base      : constant String := "Based literal cannot have a real base";
   Base_10_Exponent  : constant String := "Exponent must be expressed in base 10";
   Base_10_Numeral   : constant String := "Decimal literal must be expressed in base 10";

   -- Custom error messages based on state.  Called if failed to complete literal
   procedure Raise_Error with No_Return;
   procedure Raise_Error is
   begin
      case State is 
         when Decimal      =>
            case Self.Next is
               when Extended_Digit => Self.Error(Base_10_Numeral);
               when others         => Self.Error(Invalid_Character);
            end case;
         when Decimal_Real =>
            case Self.Next is
               when Extended_Digit => Self.Error(Base_10_Numeral);
               when others         => Self.Error(Invalid_Character);
            end case;
         when Based => 
            case Self.Next is
               when Hex_Digit => Self.Error(Digit_Too_High);
               when others    => Self.Error(Incomplete_Based);
            end case;
         when Based_Real => 
            case Self.Next is
               when Hex_Digit => Self.Error(Digit_Too_High);
               when Period    => Self.Error(Too_Many_Periods);
               when others    => Self.Error(Incomplete_Based);
            end case;
         when Exponent => 
            case Self.Next is
               when Extended_Digit => Self.Error(Base_10_Exponent);
               when others         => Self.Error(Invalid_Character);
            end case;
      end case;
   end Raise_Error;

begin

   loop
      -- Get full numeral, including underlines, but not separators
      Parse_Numeral(Self, Stream, Buffer);

      -- Parse any numeral separators
      case State is
         when Decimal =>
            case Self.Next is
               when Period   => exit when Is_Range; Set_State_To(Decimal_Real);
               when Pound    =>                     Set_State_To(Based);
               when E        =>                     Set_State_To(Exponent);
               when others   => exit when Finished; Raise_Error;
            end case;
         when Decimal_Real =>
            case Self.Next is
               when Pound    =>                     Self.Error(No_Real_Base);
               when E        =>                     Set_State_To(Exponent);
               when others   => exit when Finished; Raise_Error;
            end case;
         when Based =>
            case Self.Next is
               when Period =>                        Set_State_To(Based_Real);
               when Pound  => exit when No_Exponent; Set_State_To(Exponent);
               when others =>                        Raise_Error;
            end case;
         when Based_Real =>
            case Self.Next is
               when Pound  => exit when No_Exponent; Set_State_To(Exponent);
               when others =>                        Raise_Error;
            end case;
         when Exponent => exit when Finished; Raise_Error;
      end case;

      -- Ensure next character is a numeral
      if not Is_Numeral(Self.Next) then
         Self.Error(Invalid_Character);
      end if;
   end loop;
   
   Self.Tokens.Append(Token'
      (Kind  => 
         (if Is_Real then 
            Tokens.Real_Literal 
          else 
            Tokens.Integer_Literal),
       Value => Strings.New_String(Buffer.Copy),
       Line  => Self.Line,
       First => First,
       Last  => Self.Column - 1));
   
end Get_Numeric_Literal;
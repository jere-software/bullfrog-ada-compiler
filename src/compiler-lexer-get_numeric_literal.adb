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

    Line  : constant Positive := Self.Line;
    First : constant Positive := Self.Column;

    -- Used for based literal parsing
    Pound_Count      : Natural range 0 .. 2 := 0;  
    Digit_Count      : Natural range 0 .. 3 := 0; -- leading 0's ignored

    -- Parsing state
    Decimal_Found    : Boolean   := False;
    Exponent_Found   : Boolean   := False;

    -- Base calculation variables.  Type is intentionally
    -- not constrained to 2..16 in order to facilitate 
    -- error detection and report
    Base             : Natural   := 10;
    Base_Digits      : String(1..2);

    -- Used to validate that a '#', '.', or '_' are not preceded
    -- by a symbol
    procedure Check_Preceded_By_Digit is
        use Strings;
    begin
        if Self.Last_In in Period | Underscore | Pound | Plus | Minus then
            Self.Expected("Previous character must be a digit.  Digit");
        end if;
    end Check_Preceded_By_Digit;

    -- Indicates if the literal has completed and ends with the appropriate
    -- character
    function Literal_Finished return Boolean is
        (Pound_Count /= 1 and then Self.Last_In in Strings.Digit | Strings.Pound)
    with Inline;

    -- Recursive parsing function.
    function Get_Numeric return String is
        Result : String(1..Default_String_Length);
        use Strings;
    begin

        for Index in Result'Range loop

            case Self.Next_In is
                when Digit =>
                    case Pound_Count is
                        when 0 => 
                            if Digit_Count <= 2 then
                                if Digit_Count = 2 then
                                    Digit_Count := 3;
                                elsif Digit_Count = 1 then
                                    Base_Digits(2) := Self.Next_In;
                                    Digit_Count := 2;
                                elsif Self.Next_In /= Zero then
                                    Base_Digits(1) := Self.Next_In;
                                    Digit_Count := 1;
                                end if;
                            end if;
                        when 1 => 
                            if Numeric_Value(Self.Next_in) >= Base then
                                Self.Expected("Value outside of base range. Digit");
                            end if;
                        when 2 => 
                            if not Exponent_Found then
                                Self.Expected("Digit cannot be between '#' and 'E'.  Exponent");
                            end if;
                    end case;
                when Extended_Digit =>
                    if Pound_Count /= 1 then
                        if Self.Next_in not in Exponent_Lower | Exponent_Upper then
                            Self.Expected("Extended digit outside of #'s.  Digit");
                        elsif Exponent_Found then
                            Self.Expected("Too many exponents.  Digit");
                        elsif Pound_Count = 0 and Self.Last_In not in Digit then
                            Self.Expected("Exponent must be preceded by a digit.  Digit");
                        elsif Pound_Count = 2 and Self.Last_In /= Pound then
                            Self.Expected("Exponent must be preceded by '#'.  Exponent");
                        end if;
                        Exponent_Found := True;
                    elsif Numeric_Value(Self.Next_in) >= Base then
                        Self.Expected("Value outside of base range. Digit");
                    end if;
                when Underscore =>
                    Check_Preceded_By_Digit;
                when Pound =>
                    Check_Preceded_By_Digit;
                    if Exponent_Found then
                        Self.Expected("'#' cannot be in exponent field.  Digit");
                    elsif Pound_Count = 0 then 
                        if Decimal_Found then
                            Self.Expected("'#' cannot be after decimal.  Digit");
                        end if;
                        case Digit_Count is
                            when 0 => Self.Expected("Base must appear before '#'. Digit");
                            when 1 => Base := Numeric_Value(Base_Digits(1));
                            when 2 => Base := 
                                10 * Numeric_Value(Base_Digits(1))
                                    + Numeric_Value(Base_Digits(2));
                            when others => Self.Expected("'#' comes too late. Digit");
                        end case;
                        if Base not in 2 .. 16 then
                            Self.Expected("Base value out of range.  2 .. 16", Line, First);
                        end if;
                    elsif Pound_Count = 2 then
                        Self.Expected("Too many '#'.  Exponent");
                    end if;
                    Pound_Count := Pound_Count + 1;
                when Period =>
                    Check_Preceded_By_Digit;
                    if Exponent_Found or else Pound_Count > 1 then
                        Self.Expected("Period cannot be in exponent field.  Digit");
                    elsif Decimal_Found then
                        Self.Expected("Too many periods.  Digit");
                    end if;
                    Decimal_Found := True;
                when Plus =>
                    if Literal_Finished then
                        return Result(1..Natural(Index)-1);
                    elsif not Exponent_Found then
                        Self.Expected("'+' can only appear in exponent. 'E'");
                    elsif Self.Last_In not in Exponent_Lower | Exponent_Upper then
                        Self.Expected("'+' must follow an 'E' or 'e'.  Digit");
                    end if;
                when Minus =>
                    if Literal_Finished then
                        return Result(1..Natural(Index)-1);
                    elsif not Exponent_Found then
                        Self.Expected("'-' can only appear in exponent. 'E'");
                    elsif not Decimal_Found then
                        Self.Expected("Integer exponents cannot be negative.  Digit");
                    elsif Self.Last_In not in Exponent_Lower | Exponent_Upper then
                        Self.Expected("- must follow an 'E' or 'e'.  Digit");
                    end if;
                when others => 
                    if Is_Alpha(Self.Next_In) then
                        Self.Expected("End of numeric literal invalid.  Digit");
                    elsif not Literal_Finished then
                        if Pound_Count = 1 then
                            Self.Expected("End of numeric literal invalid.  '#'");
                        else
                            Self.Expected("End of numeric literal invalid.  Digit");
                        end if;
                    end if;
                    return Result(1..Natural(Index)-1);
                    
            end case;

            Result(Index)  := Self.Next_In;
            Self.Get_Character(Stream);
        end loop;

        return Result & Get_Numeric;
    end Get_Numeric;

    Result : constant String := Get_Numeric;

begin
    Self.Tokens.Append(Token'
        (Kind  => 
            (if Decimal_Found then 
                Tokens.Real_Literal 
             else 
                Tokens.Integer_Literal),
         Value => Strings.New_String(Result),
         Line  => Line,
         First => First,
         Last  => Self.Column - 1));
end Get_Numeric_Literal;
-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Keywords;
with Ada.Containers.Vectors;

package body Compiler.Lexer is

   ------------------------------------------------------
   --------------- Token Type Operations ----------------
   ------------------------------------------------------

   procedure Debug(Self : Token) is
      use Strings.Text_IO;
      use Strings;
      use Tokens;
   begin
      Put_Line(To_String(Self.Kind'Image)
         & ": (" 
         & Image(Self.Line)
         & ": "
         & Image(Self.First)
         & ", "
         & Image(Self.Last)
         & ") => "
         & Self.Value.Get);
   end Debug;

   ------------------------------------------------------
   --------------- Lexer Token Operations ---------------
   ------------------------------------------------------

   function All_Tokens(Self : aliased Instance) 
      return not null access constant Token_List 
   is (Self.Tokens'Access);

   ------------------------------------------------------
   --------------- Lexer Core Operations ----------------
   ------------------------------------------------------

   procedure Run(Self : in out Instance; Filename : Standard.String) is
      package Text_IO renames Strings.Text_IO;
      File : Strings.File_Type;
   begin
      Text_IO.Open
         (File => File,
          Name => Filename,
          Mode => Text_IO.In_File);
      Self.Run(Strings.Stream(File));
      
      Text_IO.Close(File);
   exception
      when others =>
         if Text_IO.Is_Open(File) then
            Text_IO.Close(File);
         end if;
         raise;
   end Run;

   procedure Run
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is 
      Count : Natural := 0;
   begin
      
      Self.Initialize(Stream);

      pragma Assert(Self.Is_Running);  -- Lexer should be running here

      -- Keep getting tokens until End_Of_Stream token encountered
      loop
         Count := Count + 1;
         Self.Tokens.Append(Self.Get_Next_Token(Stream));
         exit when Self.Not_Running and then Self.Last_Token in Tokens.End_Of_Stream;
      end loop;

      pragma Assert(Self.Not_Running);  -- Lexer should be finished here
      
   end Run;

   procedure Enable_Comments(Self : in out Instance) is
   begin
      Self.Comments_On := True;
   end Enable_Comments;

   function Is_Running(Self : Instance) return Boolean is
      (Self.State /= Off);

   function Not_Running(Self : Instance) return Boolean is
      (Self.State = Off);

   procedure Initialize
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is 
      function Read_Peek return Character is
      begin
         return Result : Character
         do
            Character'Read(Stream, Result);
         end return;
      exception   
         when others => Self.Error("Unexpected end of file");
      end Read_Peek;
   begin
      Self.Tokens      := Tokens.Empty_Token_List;
      Self.Next        := Strings.Nul;
      Self.Peek        := Strings.Nul;
      Self.Line        := 1;
      Self.Column      := 1;
      Self.Peek_Line   := 1;
      Self.Peek_Column := 1;
      Self.State       := Running;

      -- Prepopulate first character without using Advance
      -- so that Line and Column values will be in sync
      -- with the incoming values once Advance is called
      Self.Peek := Read_Peek;  
      Self.Advance(Stream); -- Pushes Peek into Next

   end Initialize;

   procedure Advance
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is 
      use Strings;

      -- Determine Peek_Line should be incremented
      function Is_Newline return Boolean is
         (Is_Line_Terminator(Self.Next)
          and then    (Self.Next /= Carriage_Return
               or else Self.Peek /= New_Line)) with Inline;

      -- Attempts to read the next character from a stream
      function Read(Item : out Character) return Boolean is
      begin
         Character'Read(Stream, Item);
         return True;
      exception
         when others => return False;
      end Read;

   begin

      -- Update "Next" parameters
      Self.Next    := Self.Peek;
      Self.Line    := Self.Peek_Line;
      Self.Column  := Self.Peek_Column;

      case Self.State is
         when Off =>
            Self.Error("Unexpected end of file");
         when Running =>

            -- Try to read in character.  If end of
            -- stream, then set to space and update
            -- lexer state for next call
            if not Read(Self.Peek) then
               Self.Peek  := Nul;
               Self.State := End_Of_Stream;
            end if;

            --Self.Debug;

            -- Calculate next line and column
            -- to match location of Peek character
            if Is_Newline then
               Self.Peek_Line   := @ + 1;
               Self.Peek_Column := 1;
            else
               Self.Peek_Column := @ + 1;
            end if;
               when End_Of_Stream =>
                  Self.State := Off;
            end case;

   exception
      -- Should only get constraint error from calculations
      -- of Peek_Line and Peek_Column
      when Constraint_Error =>
         if Is_Newline then
            Self.Error("Unable to tokenize: Too many lines in file");
         else 
            Self.Error("Unable to tokenize: Too many columns in line");
         end if;
   end Advance;

   procedure Skip_Whitespace
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class) 
   is
      use Strings;
   begin
      while Self.Is_Running and Is_Whitespace(Self.Next) loop
         Self.Advance(Stream);
      end loop;
   end Skip_Whitespace;

   procedure Skip_Comment
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is 
      function Is_Comment(Item : Character) return Boolean is
         (not Strings.Is_Line_Terminator(Item)) with Inline;
   begin
      while Self.Is_Running and Is_Comment(Self.Next) loop
         Self.Advance(Stream);
      end loop; 
   end Skip_Comment;

   -------------------------------------------------------
   -------- Generic Scanning Types and Operations --------
   -------------------------------------------------------

   -- Temporary dynamic character buffers for reading strings of
   -- unknown length from the input stream
   package Character_Vectors is new Ada.Containers.Vectors(Positive, Character);
   type Character_Vector is new Character_Vectors.Vector with null record;

   -- Generates a string copy of the character buffer data
   function Copy(Buffer : Character_Vector) return String is
   begin
      return Result : String(Buffer.First_Index .. Buffer.Last_Index) do
         for Index in Result'Range loop
            Result(Index) := Buffer.Element(Index);
         end loop;
      end return;
   end Copy;

   -- Default size to make a character buffer.  This
   -- is used to avoid unnecessary copies while appending
   -- new elements
   Default_Character_Vector_Size : constant := 256;

   -- Provides a generic algorithm for parsing variable length
   -- token string values.  This will read in characters and 
   -- append them to the supplied buffer until the upcoming
   -- input character fails the supplied Is_Charactar test.
   --
   -- NOTE:  This doesn't check the first character in the
   -- stream
   generic
      with function Is_Character(Item : Character) return Boolean;
   procedure Generic_Scan
      (Lexer   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Buffer : in out Character_Vector);
   procedure Generic_Scan
      (Lexer  : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Buffer : in out Character_Vector)
   is begin
      loop
         Buffer.Append(Lexer.Next);
         Lexer.Advance(Stream);
         exit when Lexer.Not_Running or else not Is_Character(Lexer.Next);
      end loop;
   end Generic_Scan;
   
   -- Provides a generic algorithm for parsing variable length
   -- token string values.  This will read in characters and 
   -- append them to the supplied buffer until the upcoming
   -- input character fails the supplied Is_Charactar test.
   -- After that, it will check if the next character passes
   -- the Is_Connector test and, if so, restarts the original
   -- loop to scan more characters.  Connector characters
   -- cannot be adjacent to each other (EX: 123_456 is 
   -- potentially valid but 123__456 is not)
   --
   -- NOTE:  This doesn't check the first character in the
   -- stream
   generic
      with function Is_Character(Item : Character) return Boolean;
      with function Is_Connector(Item : Character) return Boolean;
      Target_Name : String;
   procedure Generic_Scan_With_Connector
      (Lexer   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Buffer : in out Character_Vector);
   procedure Generic_Scan_With_Connector
      (Lexer   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Buffer : in out Character_Vector)
   is begin

      -- Look for:  value {connector value}
      Outer: loop

         -- Get value before any connectors
         Inner : loop
            Buffer.Append(Lexer.Next);
            Lexer.Advance(Stream);
            exit Outer when Lexer.Not_Running;
            exit Inner when not Is_Character(Lexer.Next);
         end loop Inner;

         -- Repeat inner loop if a connector is found
         exit Outer when not Is_Connector(Lexer.Next);
         Buffer.Append(Lexer.Next);
         Lexer.Advance(Stream);

         -- Ensure there is a valid character after the connector
         if Lexer.Not_Running or else not Is_Character(Lexer.Next) then
            if Is_Connector(Lexer.Next) then
               Lexer.Error(Target_Name & " cannot have back to back connectors");
            elsif Strings.Is_Whitespace(Lexer.Next) then
               Lexer.Error(Target_Name & " cannot end with a connector");
            else
               Lexer.Error(Target_Name & " has invalid character after connector");
            end if;
         end if;

      end loop Outer;
   end Generic_Scan_With_Connector;   

   -------------------------------------------------------
   --------------- High Level Tokenization ---------------
   -------------------------------------------------------

   -- Local rename
   function "+"(Item : String) return Strings.Holder
      renames Strings."+";

   function Make
      (Self  : in out Instance;
       Kind  : Tokens.Token_Kind; 
       Value : String; 
       First : Column_Number) 
       return Token
   is begin
      Self.Last_Token := Kind;
      return (Kind, +Value, Self.Line, First, Self.Column-1);
   end Make;

   function End_Of_Stream(Self : in out Instance) return Token is
   begin
      Self.Last_Token := Tokens.End_Of_Stream;
      return (Tokens.End_Of_Stream, +"", Self.Line, Self.Column, Self.Column);
   end End_Of_Stream;

   function Get_Next_Token
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
   is 
      use Strings;
      use Tokens;

      -- Local rename
      subtype Token_Kind is Tokens.Token_Kind; 

      -- This is for single character delimiters
      function Basic_Delimiter(Kind : Token_Kind) return Token is
         Value : constant String        := "" & Self.Next;
         First : constant Column_Number := Self.Column;
      begin
         Self.Advance(Stream);
         return Self.Make(Kind, Value, First);
      end Basic_Delimiter;
      
      -- This is for double character delimiters
      function Compound_Delimiter(Kind : Token_Kind) return Token is
         Value : constant String        := "" & Self.Next & Self.Peek;
         First : constant Column_Number := Self.Column;
      begin
         Self.Advance(Stream);
         Self.Advance(Stream);
         return Self.Make(Kind, Value, First);
      end Compound_Delimiter;

   begin
      << Restart_Location >> -- Location to return to when a comment is skipped
      Self.Skip_Whitespace(Stream);

      if Strings.Is_Letter(Self.Next) then -- Handle identifiers
         return Self.Get_Identifier(Stream);
      elsif Strings.Is_Numeral(Self.Next) then -- Handle numbers
         return Self.Get_Numeric_Literal(Stream);
      end if;

      case Self.Next is
         when Plus              => return Basic_Delimiter(Delimiter_Plus);
         when Ampersand         => return Basic_Delimiter(Delimiter_Concatenate);
         when Bar               => return Basic_Delimiter(Delimiter_Membership);
         when Open_Parenthesis  => return Basic_Delimiter(Delimiter_Open_Parenthesis);
         when Close_Parenthesis => return Basic_Delimiter(Delimiter_Close_Parenthesis);
         when Open_Bracket      => return Basic_Delimiter(Delimiter_Open_Bracket);
         when Close_Bracket     => return Basic_Delimiter(Delimiter_Close_Bracket);
         when Semicolon         => return Basic_Delimiter(Delimiter_Semicolon);
         when Comma             => return Basic_Delimiter(Delimiter_Comma);
         when At_Sign           => return Basic_Delimiter(Delimiter_Target);
         when Apostrophe        => return Self.Get_Character_Or_Apostrophe(Stream);
         when Quote             => return Self.Get_String_Literal(Stream);
         when Asterisk => 
            case Self.Peek is
               when Asterisk => return Compound_Delimiter(Delimiter_Exponent);
               when others   => return Basic_Delimiter   (Delimiter_Multiply);
            end case;
         when Forward_Slash => 
            case Self.Peek is
               when Equals => return Compound_Delimiter(Delimiter_Not_Equals);
               when others => return Basic_Delimiter   (Delimiter_Divide);
            end case;
         when Less_Than => 
            case Self.Peek is
               when Equals       => return Compound_Delimiter(Delimiter_Less_Than_Equals);
               when Greater_Than => return Compound_Delimiter(Delimiter_Box);
               when Less_Than    => return Compound_Delimiter(Delimiter_Left_Label);
               when others       => return Basic_Delimiter   (Delimiter_Less_Than);
            end case;
         when Greater_Than => 
            case Self.Peek is
               when Equals       => return Compound_Delimiter(Delimiter_Greater_Than_Equals);
               when Greater_Than => return Compound_Delimiter(Delimiter_Right_Label);
               when others       => return Basic_Delimiter   (Delimiter_Greater_Than);
            end case;
         when Colon => 
            case Self.Peek is
               when Equals => return Compound_Delimiter(Delimiter_Assignment);
               when others => return Basic_Delimiter   (Delimiter_Colon);
            end case;
         when Equals => 
            case Self.Peek is
               when Greater_Than => return Compound_Delimiter(Delimiter_Arrow);
               when others       => return Basic_Delimiter   (Delimiter_Equals);
            end case;
         when Period => 
            case Self.Peek is
               when Period => return Compound_Delimiter(Delimiter_Range);
               when others => return Basic_Delimiter   (Delimiter_Dot);
            end case;
         when Minus => 
            case Self.Peek is
               when Minus  => 
                  if Self.Comments_On then -- This is only for testing the lexer
                     return Self.Get_Comment(Stream);
                  else
                     Self.Skip_Comment(Stream); -- Normal code path is here
                     goto Restart_Location;
                  end if;
               when others => 
                  return Basic_Delimiter(Delimiter_Minus);
            end case;
         when others => 
            if Self.Is_Running then
               Self.Error("Unexpected input to lexer");
            elsif Self.Last_Token in Tokens.End_Of_Stream then
               raise Program_Error with "Invalid lexer state";
            else
               return Self.End_Of_Stream;
            end if;
      end case;

   end Get_Next_Token;

   function Get_Identifier
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token 
   is 
      use Compiler.Strings;
      use Compiler.Tokens;
      subtype Token_Kind is Tokens.Token_Kind;

      First  : constant Column_Number := Self.Column;
      Buffer : Character_Vector;

      -- Instantiate a character scanner
      procedure Scan_Identifier is new Generic_Scan_With_Connector
         (Is_Character => Is_Identifier,
          Is_Connector => Is_Punctuation_Connector,
          Target_Name  => "Identifier");
   begin
      Scan_Identifier(Self, Stream, Buffer);
      pragma Assert(Self.Column > First);

      -- Identify the correct token kind based on the previous token.  This is
      -- done to avoid parsing the identifier into a keyword prematurely
      case Self.Last_Token is
         when Delimiter_Apostrophe => return Self.Make(Attribute, Buffer.Copy, First);
         when Keyword_Pragma       => return Self.Make(Pragma_ID, Buffer.Copy, First);
         when others => 
            -- Either a keyword or identifier
            declare
               Result : constant String     := Buffer.Copy;
               Kind   : constant Token_Kind := Keywords.Token_Kind(Result);
            begin
               return Self.Make(Kind, Result, First);
            end;
      end case;
   end Get_Identifier;

   function Get_Numeric_Literal
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token is separate;
       
   function Get_String_Literal
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
   is 
      use Compiler.Tokens;
      use Compiler.Strings;

      Buffer  : Character_Vector;
      First   : constant Column_Number := Self.Column;
      Bad_End : constant String        := "Unexpected end to string literal";

      -- Empty string occurs when Current = Quote and Peek /= quote
      function Is_Empty_String return Boolean is
         (Self.Next = Quote and Self.Peek /= Quote) with Inline;
   begin

      Self.Advance(Stream);  -- Munch opening quote

      -- Look for empty string for quick result.  If not, then
      -- ensures there is at least some valid content to scan
      if Is_Empty_String then
         Self.Advance(Stream); -- Munch closing quote
         return Self.Make(String_Literal, "", First);
      elsif Self.Not_Running or not Is_Graphic(Self.Next) then
         Self.Error(Bad_End);
      end if;

      -- Preallocate some space
      Buffer.Reserve_Capacity(Default_Character_Vector_Size);

      loop
         while Self.Is_Running and Is_String(Self.Next) loop
            Buffer.Append(Self.Next);
            Self.Advance(Stream);
         end loop;

         -- There must be a closing quote to a string literal
         if Self.Next /= Quote then
            Self.Error(Bad_End);
         end if;

         Self.Advance(Stream); -- Munch closing quote
         exit when Self.Next /= Quote;  -- Exit if not an escaped quote

         Buffer.Append(Quote);  -- Add escaped quote
         Self.Advance(Stream);
      end loop;

      return Self.Make(String_Literal, Buffer.Copy, First);

   end Get_String_Literal;

   function Get_Character_Or_Apostrophe
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
   is 
      First : constant Column_Number := Self.Column;
      Value : Character;
      use Strings;
      use Tokens;
   begin
      Self.Advance(Stream); -- Munch opening apostrophe
      if          Self.Peek = Apostrophe
         and then Self.Last_Token not in Identifier | Attribute
         and then Is_Graphic(Self.Next)
      then 
         Value := Self.Next;
         Self.Advance(Stream); -- Munch value
         Self.Advance(Stream); -- Munch closing apostrophe
         return Self.Make(Character_Literal, "" & Value, First);
      else
         return Self.Make(Delimiter_Apostrophe, "'", First);
      end if;
   end Get_Character_Or_Apostrophe;

   function Get_Comment
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
       return Token
   is 
      -- Comments end the end of a line
      function Is_Comment(Item : Character) return Boolean is
         (         Self.Is_Running 
          and then not Strings.Is_Line_Terminator(Item))
         with Inline;

      procedure Scan_Comment is new Generic_Scan(Is_Comment);

      Buffer : Character_Vector;
      First  : constant Column_Number := Self.Column;
   begin
      Self.Advance(Stream); -- Munch dash
      Self.Advance(Stream); -- Munch dash

      -- If there is a comment to read, then save it
      -- and update the last token
      if Is_Comment(Self.Next) then

         Buffer.Reserve_Capacity(Default_Character_Vector_Size);

         Scan_Comment(Self, Stream, Buffer);

      end if;

      -- Don't update Last_Token, so return raw value
      return (Tokens.Comment, +Buffer.Copy, Self.Line, First, Self.Column-1);
   end Get_Comment;

   ------------------------------------------------------
   -------------- Lexer Output Operations ---------------
   ------------------------------------------------------

   procedure Halt(Self : Instance; Message : String) is
   begin
      Strings.Text_IO.Put_Line(Message);
      raise Lexical_Error;
   end Halt;

   procedure Error(Self : Instance; Message : String) is
   begin
      Self.Error(Message, Self.Line, Self.Column);
   end Error;

   procedure Error
      (Self    : Instance; 
       Message : String; 
       Line    : Line_Number; 
       Column  : Column_Number)
   is 
      use Compiler.Tokens;
   begin
      Self.Halt
         ("Lexical Error @ "
          & Image(Line) & ":" & Image(Column)
          & " => " & Message);
   end Error;

   procedure Debug(Self : Instance) is
      use Strings.Text_IO;
      use Strings;
      use type Ada.Containers.Count_Type;
      use Tokens;
   begin
      Put(Image(Self.Line) & ":" & Image(Self.Column) & " => "
         & Image(Pos(Self.Next)) & " => "
         & "Count: " & Image(Natural(Self.Tokens.Length)) & " => ");
      if Self.Tokens.Length > 0 then
         Debug(Self.Tokens(Self.Tokens.Last_Index));
      else
         Text_IO.New_Line;
      end if;
   end Debug;
   

end Compiler.Lexer;
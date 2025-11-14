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
   --------------- Lexar Token Operations ---------------
   ------------------------------------------------------

   procedure Add_Token
      (Self  : in out Instance; 
       Kind  : Tokens.Token_Kind;
       Value : String;
       Line  : Line_Number;
       First : Column_Number;
       Last  : Column_Number) 
   is begin
      Self.Tokens.Append(Token'
         (Kind  => Kind,
          Value => Strings.New_String(Value),
          Line  => Line,
          First => First,
          Last  => Last));
   end Add_Token;

   procedure Set_Token_Value(Self : in out Instance; Value : String) is
   begin
      Self.Tokens(Self.Tokens.Last_Index).Value.Set(Value);
   end Set_Token_Value;

   procedure Set_Token_Last(Self : in out Instance; Value : Column_Number) is
   begin
      Self.Tokens(Self.Tokens.Last_Index).Last := Value;
   end Set_Token_Last;

   function Token_Kind(Self : Instance) return Tokens.Token_Kind is
      (Self.Tokens(Self.Tokens.Last_Index).Kind);
   function Token_Value(Self : Instance) return Strings.String is
      (Self.Tokens(Self.Tokens.Last_Index).Value.Get);

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
   is begin
      
      Self.Initialize(Stream);

      while Self.Is_Running loop
         Self.Get_Token(Stream);
      end loop;

      Self.Add_Token
         (Kind   => Tokens.End_Of_File,
          Value  => "",
          Line   => Self.Line,
          First  => Self.Column,
          Last   => Self.Column);

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
      Self.Tokens      := Empty_Token_List;
      Self.Next        := Strings.Space;
      Self.Peek        := Strings.Space;
      Self.Line        := 1;
      Self.Column      := 1;
      Self.Next_Line   := 1;
      Self.Next_Column := 1;
      Self.State       := Running;

      -- Prepopulate first character without using Advance
      -- so that Line and Column values will be in sync
      -- with the incoming values once Advance is called
      Self.Peek := Read_Peek;  

   end Initialize;

   procedure Advance
      (Self   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is 
      use Strings;

      -- Determine Next_Line should be incremented
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
      Self.Line    := Self.Next_Line;
      Self.Column  := Self.Next_Column;

      case Self.State is
         when Off =>
            Self.Error("Unexpected end of file");
         when Running =>

            -- Try to read in character.  If end of
            -- stream, then set to space and update
            -- lexer state for next call
            if not Read(Self.Peek) then
               Self.Peek  := Space;
               Self.State := End_Of_File;
            end if;

            --Self.Debug;

            -- Calculate next line and column
            -- to match location of Peek character
            if Is_Newline then
               Self.Next_Line   := @ + 1;
               Self.Next_Column := 1;
            else
               Self.Next_Column := @ + 1;
            end if;
               when End_Of_File =>
                  Self.State := Off;
            end case;

   exception
      -- Should only get constraint error from calculations
      -- of Next_Line and Next_Column
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

   ------------------------------------------------------
   ----------- Lexer Tokenization Operations ------------
   ------------------------------------------------------
   
   procedure Get_Token
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class) 
   is
      use Strings;
      use Tokens;

      use type Ada.Containers.Count_Type;
      function Is_Literal return Boolean is
         (        Self.Tokens.Length = 0 
          or else Self.Token_Kind not in 
              Identifier 
            | Attribute
            | Keyword_All 
            | Delimiter_Close_Parenthesis
            | Delimiter_Close_Bracket);

      use Strings.Text_IO;
   begin

      << Restart_Location >> -- Place to return to if skipping comments

      Self.Skip_Whitespace(Stream); 
      if Is_Letter(Self.Next) then
         Self.Get_Identifier(Stream);
      elsif Is_Numeral(Self.Next) then -- may find range delimiter
         Self.Get_Numeric_Literal(Stream);
      elsif Self.Next = Quote then
         Self.Get_String_Literal(Stream);
      elsif Self.Next = Apostrophe and then Is_Literal then
         Self.Get_Character_Literal(Stream);
      elsif Is_Delimiter(Self.Next) then -- may find comment
         Self.Get_Delimiter(Stream);

         -- if the delimiter ended up being a comment
         -- instead, then read the rest of the line
         -- as a comment and update the token
         if Self.Token_Kind = Tokens.Comment then
            if Self.Comments_On then
               Self.Get_Comment(Stream);  -- usually for testing the lexer
            else
               Self.Skip_Comment(Stream); -- Standard mode
               goto Restart_Location;
            end if;
         end if;
      elsif Self.Is_Running then
         Self.Error("Expected a valid token");
      end if;

   end Get_Token;

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

      -- Remove the token since we aren't keeping comments
      Self.Tokens.Delete_Last;  
   end Skip_Comment;

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
   procedure Generic_Parse
      (Lexer   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Buffer : in out Character_Vector);
   procedure Generic_Parse
      (Lexer  : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Buffer : in out Character_Vector)
   is begin
      loop
         Buffer.Append(Lexer.Next);
         Lexer.Advance(Stream);
         exit when Lexer.Not_Running or else not Is_Character(Lexer.Next);
      end loop;
   end Generic_Parse;
   
   -- Provides a generic algorithm for parsing variable length
   -- token string values.  This will read in characters and 
   -- append them to the supplied buffer until the upcoming
   -- input character fails the supplied Is_Charactar test.
   -- After that, it will check if the next character passes
   -- the Is_Connector test and, if so, restarts the original
   -- loop to parse more characters.  Connector characters
   -- cannot be adjacent to each other (EX: 123_456 is 
   -- potentially valid but 123__456 is not)
   --
   -- NOTE:  This doesn't check the first character in the
   -- stream
   generic
      with function Is_Character(Item : Character) return Boolean;
      with function Is_Connector(Item : Character) return Boolean;
      Target_Name : String;
   procedure Generic_Parse_With_Connector
      (Lexer   : in out Instance;
       Stream : not null access Ada.Streams.Root_Stream_Type'Class;
       Buffer : in out Character_Vector);
   procedure Generic_Parse_With_Connector
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
   end Generic_Parse_With_Connector;   

   procedure Get_Identifier
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is 
      use Strings;

      -- Instantiation of generic parser that will get 
      -- the entire identifier
      procedure Parse is new Generic_Parse_With_Connector
         (Is_Character => Is_Identifier,
          Is_Connector => Is_Punctuation_Connector,
          Target_Name  => "Identifier");

      Buffer : Character_Vector := Empty(Default_Character_Vector_Size);

      use type Tokens.Token_Kind;
      
      -- Used to find Attribute identifiers
      function Follows_Apostrophe return Boolean is
         (Self.Tokens.Length not in 0 
          and then Self.Token_Kind = Tokens.Delimiter_Apostrophe)
      with Inline;

      -- Used to find pragma identifiers
      function Follows_Pragma return Boolean is
         (Self.Tokens.Length not in 0 
          and then Self.Token_Kind = Tokens.Keyword_Pragma)
      with Inline;

      -- Utility function to parse and return a copy of the result
      function Parse return String is
      begin
         Parse(Self, Stream, Buffer);
         return Buffer.Copy;
      end Parse;

      First  : constant Column_Number := Self.Column;
      Result : constant String        := Parse;
      
   begin
      Self.Tokens.Append(Token'
         (Kind  => (if Follows_Apostrophe then
                       Tokens.Attribute
                    elsif Follows_Pragma then
                       Tokens.Pragma_ID
                    else
                       Keywords.Token_Kind(Result)),
          Value => Strings.New_String(Result),
          Line  => Self.Line,
          First => First,
          Last  => Self.Column - 1));
   end Get_Identifier;

   procedure Get_Comment
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
      -- Comments end the end of a line
      function Is_Comment(Item : Character) return Boolean is
         (not Strings.Is_Line_Terminator(Item)) with Inline;

      procedure Parse_Comment is new Generic_Parse(Is_Comment);

      Buffer : Character_Vector;
   begin
      -- If there is a comment to read, then save it
      -- and update the last token
      if Self.Is_Running and Is_Comment(Self.Next) then

         Buffer.Reserve_Capacity(Default_Character_Vector_Size);

         Parse_Comment(Self, Stream, Buffer);

      end if;

      -- Update the existing token.  This removes the
      -- delimiter `--` added by the original Get_Delimiter
      Self.Set_Token_Value(String'(Buffer.Copy));
      Self.Set_Token_Last(Self.Column-1);
   end Get_Comment;

   procedure Get_String_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is 

      use Strings;

      Buffer : Character_Vector;

      -- Strings end with a quote and must contain a graphic character
      function Is_String(Item : Character) return Boolean is
         (Item /= Quote and then Is_Graphic(Item)) with Inline;

      -- Indicates if a graphic value lies between the two quotes.
      -- Just looking for two side by side quotes isn't enough
      -- as an escaped quote looks like """"
      function Not_Empty_String return Boolean is
         (Self.Next /= Quote or Self.Peek = Quote) 
          with Inline, Pre => Is_Graphic(Self.Next);

      -- Common error
      Bad_End : constant String := "Unexpected end to string literal";

      First : constant Column_Number := Self.Column;
   begin
      Self.Advance(Stream);  -- Munch first quote

      -- Make sure there is some valid character to check
      if Self.Not_Running or not Is_Graphic(Self.Next) then
         Self.Error(Bad_End);
      elsif Not_Empty_String then 
         -- If there is going to be data to add, reserve some
         -- buffer capacity as an optimization
         Buffer.Reserve_Capacity(Default_Character_Vector_Size);
      end if;

      loop
         -- Get every graphic character up to quote
         while Self.Is_Running and Is_String(Self.Next) loop
            Buffer.Append(Self.Next);
            Self.Advance(Stream);
         end loop;

         -- Strings must end in a quote
         if Self.Next /= Quote then
            Self.Error(Bad_End);
         end if;

         Self.Advance(Stream); -- Munch quote
         exit when Self.Next /= Quote;  -- Done unless escaping a quote

         -- Here we had "", which is an escaped quote
         Buffer.Append(Quote); -- Add escaped quote
         Self.Advance(Stream);

      end loop;

      Self.Add_Token
         (Kind  => Tokens.String_Literal,
          Value => Buffer.Copy,
          Line  => Self.Line,
          First => First,
          Last  => Self.Column - 1);
      
   end Get_String_Literal;

   procedure Get_Numeric_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      is separate;

   procedure Get_Delimiter
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      is separate;

   procedure Get_Character_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is 
      Temp  : constant Character     := Self.Peek; -- The expected character
      First : constant Column_Number := Self.Column;
   begin
      
      Self.Advance(Stream); -- Munch apostrophe
      if Self.Not_Running or else not Strings.Is_Graphic(Self.Next) then
         Self.Error("Non graphic character found.  Character literal expected");
      end if;

      Self.Advance(Stream); -- Munch the graphic character
      if Self.Next /= Strings.Apostrophe then
         Self.Error("Closing apostrophe not found.  Character literal expected");
      end if;

      Self.Add_Token
         (Kind  => Tokens.Character_Literal,
          Value => "" & Temp,
          Line  => Self.Line,
          First => First,
          Last  => Self.Column);

      Self.Advance(Stream); -- Munch the closing apostrophe

      -- Character literals are always 3 "characters" long,
      -- counting the apostrophes
      pragma Assert((Self.Column - First) = 3);

   end Get_Character_Literal;

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
   is begin
      Self.Halt
         ("Lexical Error @ "
          & Image(Line) & ":" & Image(Column)
          & " => " & Message);
   end Error;

   procedure Debug(Self : Instance) is
      use Strings.Text_IO;
      use Strings;
      use type Ada.Containers.Count_Type;
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
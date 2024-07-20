-- Copyright (C) 2024
-- Jeremiah Breeden     
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Keywords;

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
       Line  : Positive;
       First : Positive;
       Last  : Positive) 
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

   procedure Set_Token_Last(Self : in out Instance; Value : Positive) is
   begin
      Self.Tokens(Self.Tokens.Last_Index).Last := Value;
   end Set_Token_Last;

   function Token_Kind(Self : Instance) return Tokens.Token_Kind is
      (Self.Tokens(Self.Tokens.Last_Index).Kind);
   function Token_Value(Self : Instance) return Strings.String is
      (Self.Tokens(Self.Tokens.Last_Index).Value.Get);
   function Token_Line(Self : Instance) return Positive is
      (Self.Tokens(Self.Tokens.Last_Index).Line);
   function Token_First(Self : Instance) return Positive is
      (Self.Tokens(Self.Tokens.Last_Index).First);
   function Token_Last(Self : Instance) return Positive is
      (Self.Tokens(Self.Tokens.Last_Index).Last);

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
      
      Self.Initialize;

      while Self.Is_Running loop
         Self.Get_Token(Stream);
      end loop;

   end Run;

   function Is_Running(Self : Instance) return Boolean is
      (Self.State /= Idle);

   procedure Initialize(Self : in out Instance) is
   begin
      Self.Tokens  := Empty_Token_List;
      Self.Next_In := Strings.Space;
      Self.Line   := 1;
      Self.Column  := 1;
      Self.State   := Running;
   end Initialize;

   procedure Get_Character
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
      package Text_IO renames Strings.Text_IO;
      function Already_Found return Boolean is
         (Self.Last_In = Strings.Carriage_Return and Self.Next_In = Strings.New_Line)
      with Inline;
   begin
      Self.Last_In := Self.Next_In;
      Self.Line   := Self.Next_Line;
      Self.Column  := Self.Next_Column;
      Character'Read(Stream, Self.Next_In);
      
      --Self.Debug;

      if Strings.Is_Newline(Self.Next_In) and then not Already_Found then
         Self.Next_Line   := Self.Next_Line + 1;
         Self.Next_Column := 1;
      else
         Self.Next_Column := Self.Next_Column + 1;
      end if;

   exception
      when others => 
         case Self.State is
            when Idle => raise;
            when others => 
               Self.State   := Idle;
               Self.Next_In := Strings.Space;
               --Self.Debug;
         end case;
   end Get_Character;

   procedure Skip_Whitespace
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class) 
   is
      use Strings;
   begin
      while Self.State /= Idle and Is_Space(Self.Next_In) loop
         Self.Get_Character(Stream);
      end loop;
   end Skip_Whitespace;

   ------------------------------------------------------
   ------------- Tokenization Declarations --------------
   ------------------------------------------------------

   -- Used for recursive operations.  Should be chosen such that
   -- recursion rarely every occurs.
   Default_String_Length : constant := 200;

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
          or else Self.Token_Kind /= Identifier);
   begin
      Self.Skip_Whitespace(Stream);
      if Is_Alpha(Self.Next_In) then
         Self.Get_Name(Stream);
      elsif Is_Digit(Self.Next_In) then
         Self.Get_Numeric_Literal(Stream);
      elsif Self.Next_In = Quote then
         Self.Get_String_Literal(Stream);
      elsif Self.Next_in = Apostrophe and then Is_Literal then
         Self.Get_Character_Literal(Stream);
      elsif Is_Operator(Self.Next_In) then
         Self.Get_Operator(Stream);

         -- if the operator ended up being a comment
         -- instead, then read the rest of the line
         -- as a comment and update the token
         if Self.Token_Kind = Tokens.Comment then
            Self.Get_Comment(Stream);
         end if;
      elsif Self.State /= Idle then
         Self.Expected("A valid token");
      end if;

   end Get_Token;

   procedure Get_Comment
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
      function Get_Comment return String is
         Result : String(1..Default_String_Length);
         Index : Positive := 1;
      begin
         while not Strings.Is_Newline(Self.Next_In) and Self.State /= Idle loop
            Result(Index) := Self.Next_In;
            Self.Get_Character(Stream);
            if Index = Default_String_Length then
               return Result & Get_Comment;
            else
               Index := Index + 1;
            end if;
         end loop;
         return Result(1..Natural(Index)-1);
      end Get_Comment;
   begin
      Self.Set_Token_Value(Get_Comment);
      Self.Set_Token_Last(Self.Column-1);
   end Get_Comment;

   procedure Get_Name
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is

      use Strings;

      Line  : constant Positive := Self.Line;
      First : constant Positive := Self.Column;
      Last  :          Positive := Self.Column;

      function Get_Name return String is
         Temp : String(1..Default_String_Length);
      begin
         for Index in Temp'Range loop
            if Is_Name(Self.Next_In) then
               if          Self.Next_In = Strings.Underscore 
                  and then Self.Last_In = Self.Next_In 
               then
                  Self.Expected("Double underscore found, single underscore");
               end if;
               Temp(Index) := Self.Next_In;
               Last := Self.Column;
               Self.Get_Character(Stream);
            else
               if Self.Last_In = Strings.Underscore then
                  Self.Expected("Name ends in underscore, alphanumeric");
               end if;
               return Temp(1..Index-1);
            end if;
         end loop;
         return Temp & Get_Name;
      end Get_Name;

      Result : constant String := Get_Name;

   begin
      Self.Tokens.Append(Token'
         (Kind  => Keywords.Token_Kind(Result),
          Value => Strings.New_String(Result),
          Line  => Line,
          First => First,
          Last  => Last));
   end Get_Name;

   procedure Get_Operator
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      is separate;

   procedure Get_Numeric_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      is separate;

   procedure Get_Character_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is 
      Temp : Character;
      First : constant Positive := Self.Column;
   begin
      
      Self.Get_Character(Stream); -- Munch apostrophe
      if not Strings.Is_Graphic(Self.Next_In) then
         Self.Expected("Non graphic character found.  Character literal");
      end if;

      Temp := Self.Next_In;

      Self.Get_Character(Stream);
      if Self.Next_In /= Strings.Apostrophe then
         Self.Expected("Closing apostrophe not found.  Character literal");
      end if;

      Self.Add_Token
         (Kind  => Tokens.Character_Literal,
          Value => "" & Temp,
          Line  => Self.Line,
          First => First,
          Last  => First + 2);  -- character literals always 3 characters long

      Self.Get_Character(Stream); -- Munch the apostrophe

   end Get_Character_Literal;

   procedure Get_String_Literal
      (Self   : in out Instance; 
       Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is

      First : constant Positive := Self.Column;

      Quote_Found : Boolean := False;

      function Get_String return String is
         Result : String(1..Default_String_Length);
         Index  : Positive := 1;
      begin

         -- Loop while graphics characters are coming in
         while Self.State /= Idle and Strings.Is_Graphic(Self.Next_In) loop

            -- See if character is quote or not
            if Self.Next_In in Strings.Quote then

               -- Found a quote, so toggle found state
               Quote_Found := not Quote_Found;  -- toggle

               -- If still looking for closing quote, then 
               -- save the value and move on
               if not Quote_Found then
                  Result(Index) := Self.Next_In;
                  if Index = Default_String_Length then
                     Self.Get_Character(Stream);
                     return Result & Get_String;
                  else
                     Index := Index + 1;
                  end if;
               end if;

            elsif Quote_Found then
               -- Not a quote, but last character was a quote,
               -- so we are done
               return Result(1..Natural(Index)-1);
            else
               -- Not a quote and last character was not a quote,
               -- so save the value and move on
               Result(Index) := Self.Next_In;
               if Index = Default_String_Length then
                  Self.Get_Character(Stream);
                  return Result & Get_String;
               else
                  Index := Index + 1;
               end if;
            end if;

            Self.Get_Character(Stream);
            
         end loop;

         if not Quote_Found then
            Self.Expected("Closing quotation not found.  String literal");
         end if;

         return Result(1..Natural(Index)-1);
         
      end Get_String;
   begin
      Self.Get_Character(Stream); -- Munch the quote
      declare
         Result : constant String := Get_String;
      begin
         Self.Add_Token
            (Kind  => Tokens.String_Literal,
             Value => Result,
             Line  => Self.Line,
             First => First,
             Last  => Self.Column - 1);
      end;
   end Get_String_Literal;

   ------------------------------------------------------
   -------------- Lexer Output Operations ---------------
   ------------------------------------------------------

   procedure Halt(Self : Instance; Message : String) is
   begin
      raise Lexical_Error with Message;
   end Halt;

   procedure Expected(Self : Instance; Message : String) is
   begin
      Self.Expected(Message, Self.Line, Self.Column);
   end Expected;

   procedure Expected(Self : Instance; Message : String; Line, Column : Positive) is
   begin
      Self.Halt
         (Message 
          & " expected at " 
          & Strings.Image(Line) 
          & ":"
          & Strings.Image(Column));
   end Expected;

   procedure Debug(Self : Instance) is
      use Strings.Text_IO;
      use Strings;
      use type Ada.Containers.Count_Type;
   begin
      Put(Image(Self.Line) & ":" & Image(Self.Column) & " => "
         & Image(Pos(Self.Next_In)) & " => "
         & "Count: " & Image(Natural(Self.Tokens.Length)) & " => ");
      if Self.Tokens.Length > 0 then
         Debug(Self.Tokens(Self.Tokens.Last_Index));
      else
         Text_IO.New_Line;
      end if;
   end Debug;

end Compiler.Lexer;
-- Copyright (C) 2024
-- Jeremiah Breeden      
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Containers.Indefinite_Hashed_Maps;

package body Compiler.Keywords is

    use type Strings.String;
    use type Tokens.Token_Kind;

    -- Implementation for the search is a hashed map.
    package Maps is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type        => Strings.String, 
         Element_Type    => Tokens.Token_Kind,
         Hash            => Strings.Hash,
         Equivalent_Keys => Strings."=");

    -- This operation takes in a token identifier in the keywords range
    -- and converts it to a key value string.  This takes advantage of
    -- the fact that all keyword tokens start with a Keyword_ prefix
    -- and that the Image attribute for enumerations prints the name
    -- as the image value
    function Format(Value : Tokens.Keyword) return Strings.String is
        Prefix : constant Strings.String := "KEYWORD_";
        Start  : constant                := Prefix'Length + 1;
        Image  : constant Strings.String := Strings.To_String(Value'Image);
    begin
        return Strings.To_Lower(Image(Start .. Image'Last));
    end Format;

    -- Initializing function for the keyword map.
    function Make return Maps.Map is
    begin
        return Result : Maps.Map := Maps.Empty do
            for K in Tokens.Keyword'Range loop
                Result.Insert(Format(K), K);
            end loop;
        end return;
    end Make;

    -- This is the keyword database
    Keywords : constant Maps.Map := Make;

    function Token_Kind(Keyword : Strings.String) return Tokens.Token_Kind is
        Cursor : constant Maps.Cursor := Keywords.Find(Strings.To_Lower(Keyword));
        use type Maps.Cursor;
    begin

        if Cursor = Maps.No_Element then
            return Tokens.Identifier;
        end if;

        return Maps.Element(Cursor);

    end Token_Kind;

end Compiler.Keywords;
-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

package body Compiler.Enumeration_Search is

   -- Local renaming
   subtype String is Compiler.Strings.String;

   -- Converts an enumeration to a searchable key value
   function Format(Value : Enumeration) return String is
      Image : constant Standard.String := Value'Image;
      First : constant Positive        := Prefix_Length + 1;
      Last  : constant Positive        := Image'Last;

      pragma Assert
         (First <= Last, 
          "Prefix length too small for " & Value'Image);

      use Strings;
   begin
      return To_Lower(To_String(Image(First .. Last)));
   exception
      when Constraint_Error => 
         raise Program_Error
            with "Duplicate enumeration value or invalid Prefix_Length";
   end Format;

   -- Hash map package
   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Enumeration,
       Hash            => Strings.Hash,
       Equivalent_Keys => Strings."=");

   -- Primary map type
   subtype Enumeration_Map is Maps.Map;

   -- Creates a map
   function Make return Enumeration_Map is
   begin
      return Result : Enumeration_Map do
         for Item in Enumeration'Range loop
            Result.Insert(Format(Item), Item);
         end loop;
      end return;
   end Make;

   -- The core searchable object
   Map : constant Enumeration_Map := Make;

   function Search(Value : Compiler.Strings.String) return Enumeration'Base is
   
      use Strings;
      use type Maps.Cursor;

      Cursor : constant Maps.Cursor := Map.Find(To_Lower(Value));
      
   begin
      if Cursor = Maps.No_Element then
         return Not_Found;
      else
         return Maps.Element(Cursor);
      end if;
   end Search;

end Compiler.Enumeration_Search;
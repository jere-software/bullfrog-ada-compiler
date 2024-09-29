-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Enumeration_Search;
with Compiler.Attributes;

package body Compiler.Aspects is

   -- Instantiate the enumeration searcher
   package Mapper is new Enumeration_Search
      (Enumeration   => Valid_Aspect,
       Invalid       => Invalid,
       Prefix_Length => 7);

   function Aspect_ID(Name : Strings.String) return Aspect_Identifier is 
      ID : constant Aspect_Identifier := Mapper.Search(Name);
   begin
      if ID in Valid_Simple_Aspect then
         return ID;
      else
         return Invalid;
      end if;
   end Aspect_ID;

   function Aspect_ID
      (Name      : Strings.String;
       Attribute : Strings.String) 
       return Aspect_Identifier
   is 
      use Attributes;
   begin
      -- Confirm the attribute is a valid one
      declare
         ID : constant Attribute_Identifier := Attribute_ID(Attribute);
      begin
         if ID = Invalid then
            return Invalid;
         end if;
      end;

      declare
         ID : constant Aspect_Identifier := Aspect_ID(Name & "_" & Attribute);
      begin
         if ID in Valid_Complex_Aspect then
            return ID;
         else
            return Invalid;
         end if;
      end;
   end Aspect_ID;

   -- Default information value
   Not_Supported : constant Info := (Supported => False);

   -- Contains a mapping from each aspect to its information
   Ada_Info_Map : constant array(Valid_Aspect) of Info := 
      (others => Not_Supported);

   function Aspect_Info(ID : Valid_Aspect) return Info is
      (Ada_Info_Map(ID));

end Compiler.Aspects;

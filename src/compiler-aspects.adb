-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Enumeration_Search;

package body Compiler.Aspects is

   -- Instantiate the enumeration searcher
   package Mapper is new Enumeration_Search
      (Enumeration   => Valid_Aspect,
       Invalid       => Invalid,
       Prefix_Length => 7);

   function Aspect_ID(Name : Strings.String) return Aspect_Identifier
      renames Mapper.Search;

   function Aspect_ID
      (Name      : Strings.String;
       Attribute : Strings.String) 
       return Aspect_Identifier
   is (Aspect_ID(Name & "_" & Attribute));

   -- Default information value
   Not_Supported : constant Info := (Supported => False);

   -- Contains a mapping from each aspect to its information
   Ada_Info_Map : constant array(Valid_Aspect) of Info := 
      (others => Not_Supported);

   function Aspect_Info(ID : Valid_Aspect) return Info is
      (Ada_Info_Map(ID));

end Compiler.Aspects;

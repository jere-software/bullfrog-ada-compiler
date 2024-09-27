-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Enumeration_Search;

package body Compiler.Attributes is 

   -- Instantiate the enumeration searcher
   package Ada_Mapper is new Enumeration_Search
      (Enumeration   => Valid_Attribute,
       Invalid       => Invalid,
       Prefix_Length => 10);

   function Attribute_ID(Name : Strings.String) return Attribute_Identifier 
      renames Ada_Mapper.Search;

   -- Default support value
   Not_Supported : constant Info := (Supported => False);

   -- Contains a mapping from each attribute to its information
   Info_Map : constant array(Valid_Attribute) of Info := 
      (others => Not_Supported);

   function Attribute_Info(ID : Valid_Attribute) return Info is
      (Info_Map(ID));
   
end Compiler.Attributes;
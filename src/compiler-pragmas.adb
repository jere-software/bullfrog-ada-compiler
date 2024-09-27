-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Enumeration_Search;

package body Compiler.Pragmas is 

   -- Instantiate the enumeration searcher
   package Ada_Mapper is new Enumeration_Search
      (Enumeration   => Valid_Pragma,
       Invalid       => Invalid,
       Prefix_Length => 7);

   function Pragma_ID(Name : Strings.String) return Pragma_Identifier 
      renames Ada_Mapper.Search;

   -- Default support value
   Not_Supported : constant Info := (Supported => False);

   -- Contains a mapping from each pragma to its information
   Info_Map : constant array(Valid_Pragma) of Info := 
      (others => Not_Supported);

   function Pragma_Info(ID : Valid_Pragma) return Info is
      (Info_Map(ID));
   
end Compiler.Pragmas;
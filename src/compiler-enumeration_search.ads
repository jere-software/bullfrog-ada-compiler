-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

private with Ada.Containers.Indefinite_Hashed_Maps;

with Compiler.Strings;

-- Provides a generic search operation for a list of enumerations
-- that are prefixed with the same name
generic

   -- Core enumeration type, all values prefixed with same 
   -- name
   type Enumeration is (<>);

   -- Value outside the core enumeration type that will
   -- be returned if the enumeration is not found
   Invalid : Enumeration'Base;

   -- Length of the prefix used for all enumeration values
   Prefix_Length : Positive;

package Compiler.Enumeration_Search is

   function Search(Value : Compiler.Strings.String) return Enumeration'Base;

private

   -- Verify that Invalid is not part of Enumeration
   Not_Found : constant Enumeration'Base := 
      (if Invalid in Enumeration then
         raise Program_Error 
            with "Value for Invalid is inside the range of Enumeration"
       else
         Invalid);

end Compiler.Enumeration_Search;




-- Copyright (C) 2024
-- Jeremiah Breeden      
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Strings;
with Compiler.Tokens;

-- Provides a means to identify if a supplied value is a
-- specific keyword or not.
package Compiler.Keywords is

    -- Searches keyword list and returns the appropriate token.  Returns
    -- the token Identifier if the keyword is not found
    function Token_Kind(Keyword : Strings.String) return Tokens.Token_Kind;
    
end Compiler.Keywords;
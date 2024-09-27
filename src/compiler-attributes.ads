-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Strings;

-- Provides utilities for supporting attributes
package Compiler.Attributes is

   -- List of all language defined attributes along with 
   -- invalid case
   type Attribute_Identifier is
      (Invalid,
       Attribute_Access,
       Attribute_Address,
       Attribute_Adjacent,
       Attribute_Aft,
       Attribute_Alignment,
       Attribute_Base,
       Attribute_Bit_Order,
       Attribute_Body_Version,
       Attribute_Callable,
       Attribute_Caller,
       Attribute_Ceiling,
       Attribute_Class,
       Attribute_Component_Size,
       Attribute_Compose,
       Attribute_Constrained,
       Attribute_Copy_Sign,
       Attribute_Count,
       Attribute_Definite,
       Attribute_Delta,
       Attribute_Denorm,
       Attribute_Digits,
       Attribute_Enum_Rep,
       Attribute_Enum_Val,
       Attribute_Exponent,
       Attribute_External_Tag,
       Attribute_First,
       Attribute_First_Bit,
       Attribute_First_Valid,
       Attribute_Floor,
       Attribute_Fore,
       Attribute_Fraction,
       Attribute_Has_Same_Storage,
       Attribute_Identity,
       Attribute_Image,
       Attribute_Index,
       Attribute_Input,
       Attribute_Last,
       Attribute_Last_Bit,
       Attribute_Last_Valid,
       Attribute_Leading_Part,
       Attribute_Length,
       Attribute_Machine,
       Attribute_Machine_Emax,
       Attribute_Machine_Emin,
       Attribute_Machine_Mantissa,
       Attribute_Machine_Overflows,
       Attribute_Machine_Radix,
       Attribute_Machine_Rounding,
       Attribute_Machine_Rounds,
       Attribute_Max,
       Attribute_Max_Alignment_For_Allocation,
       Attribute_Max_Size_In_Storage_Elements,
       Attribute_Min,
       Attribute_Mod,
       Attribute_Model,
       Attribute_Model_Emin,
       Attribute_Model_Epsilon,
       Attribute_Model_Mantissa,
       Attribute_Model_Small,
       Attribute_Modulus,
       Attribute_Object_Size,
       Attribute_Old,
       Attribute_Output,
       Attribute_Overlaps_Storage,
       Attribute_Parallel_Reduce,
       Attribute_Partition_Id,
       Attribute_Pos,
       Attribute_Position,
       Attribute_Pred,
       Attribute_Preelaborable_Initialization,
       Attribute_Priority,
       Attribute_Put_Image,
       Attribute_Range,
       Attribute_Read,
       Attribute_Reduce,
       Attribute_Relative_Deadline,
       Attribute_Remainder,
       Attribute_Result,
       Attribute_Round,
       Attribute_Rounding,
       Attribute_Safe_First,
       Attribute_Safe_Last,
       Attribute_Scale,
       Attribute_Scaling,
       Attribute_Signed_Zeros,
       Attribute_Size,
       Attribute_Small,
       Attribute_Storage_Pool,
       Attribute_Storage_Size,
       Attribute_Stream_Size,
       Attribute_Succ,
       Attribute_Tag,
       Attribute_Terminated,
       Attribute_Truncation,
       Attribute_Unbiased_Rounding,
       Attribute_Unchecked_Access,
       Attribute_Val,
       Attribute_Valid,
       Attribute_Value,
       Attribute_Version,
       Attribute_Wide_Image,
       Attribute_Wide_Value,
       Attribute_Wide_Wide_Image,
       Attribute_Wide_Wide_Value,
       Attribute_Wide_Wide_Width,
       Attribute_Wide_Width,
       Attribute_Width,
       Attribute_Write);

   -- All valid attributes, language defined and implementation defined
   subtype Valid_Attribute is Attribute_Identifier 
      range Attribute_Access .. Attribute_Write;

   -- Only language defined attributes
   subtype Language_Defined_Attribute is Attribute_Identifier 
      range Attribute_Access .. Attribute_Write;
   
   -- Verifies the supplied string is a valid language defined attribute.
   -- If the input is valid, it returns the associated attribute enumeration
   -- value or the value Invalid if not.
   function Attribute_ID(Name : Strings.String) return Attribute_Identifier;

   -- Information about the attribute
   type Info(Supported : Boolean := False) is null record;

   -- Returns important information assoicated with an attribute
   function Attribute_Info(ID : Valid_Attribute) return Info;

end Compiler.Attributes;
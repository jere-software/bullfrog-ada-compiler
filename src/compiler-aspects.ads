-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Strings;

package Compiler.Aspects is

   -- List of all language defined aspects along with 
   -- invalid case
   type Aspect_Identifier is 
      (Invalid,
       Aspect_Address,
       Aspect_Aggregate,
       Aspect_Alignment,
       Aspect_All_Calls_Remote,
       Aspect_Asynchronous,
       Aspect_Atomic,
       Aspect_Atomic_Components,
       Aspect_Attach_Handler,
       Aspect_Bit_Order,
       Aspect_Coding,
       Aspect_Component_Size,
       Aspect_Constant_Indexing,
       Aspect_Convention,
       Aspect_CPU,
       Aspect_Default_Component_Value,
       Aspect_Default_Initial_Condition,
       Aspect_Default_Iterator,
       Aspect_Default_Storage_Pool,
       Aspect_Default_Value,
       Aspect_Discard_Names,
       Aspect_Dispatching,
       Aspect_Dispatching_Domain,
       Aspect_Dynamic_Predicate,
       Aspect_Elaborate_Body,
       Aspect_Exclusive_Functions,
       Aspect_Export,
       Aspect_External_Name,
       Aspect_External_Tag,
       Aspect_Full_Access_Only,
       Aspect_Global,
       Aspect_Global_Class,
       Aspect_Implicit_Dereference,
       Aspect_Import,
       Aspect_Independent,
       Aspect_Independent_Components,
       Aspect_Inline,
       Aspect_Input,
       Aspect_Input_Class,
       Aspect_Integer_Literal,
       Aspect_Interrupt_Handler,
       Aspect_Interrupt_Priority,
       Aspect_Iterator_Element,
       Aspect_Iterator_View,
       Aspect_Layout,
       Aspect_Link_Name,
       Aspect_Machine_Radix,
       Aspect_Max_Entry_Queue_Length,
       Aspect_No_Controlled_Parts,
       Aspect_No_Return,
       Aspect_Nonblocking,
       Aspect_Output,
       Aspect_Output_Class,
       Aspect_Pack,
       Aspect_Parallel_Calls,
       Aspect_Post,
       Aspect_Post_Class,
       Aspect_Pre,
       Aspect_Pre_Class,
       Aspect_Predicate_Failure,
       Aspect_Preelaborate,
       Aspect_Priority,
       Aspect_Put_Image,
       Aspect_Read,
       Aspect_Read_Class,
       Aspect_Real_Literal,
       Aspect_Relative_Deadline,
       Aspect_Remote_Call_Interface,
       Aspect_Remote_Types,
       Aspect_Shared_Passive,
       Aspect_Size,
       Aspect_Small,
       Aspect_Stable_Properties,
       Aspect_Stable_Properties_Class,
       Aspect_Static,
       Aspect_Static_Predicate,
       Aspect_Storage_Pool,
       Aspect_Storage_Size,
       Aspect_Stream_Size,
       Aspect_String_Literal,
       Aspect_Synchronization,
       Aspect_Type_Invariant,
       Aspect_Type_Invariant_Class,
       Aspect_Unchecked_Union,
       Aspect_Use_Formal,
       Aspect_Variable_Indexing,
       Aspect_Volatile,
       Aspect_Volatile_Components,
       Aspect_Write,
       Aspect_Write_Class,
       Aspect_Yield);

   -- All valid aspects, language defined and implementation defined
   subtype Valid_Aspect is Aspect_Identifier 
      range Aspect_Address .. Aspect_Yield;

   -- Only language defined aspects
   subtype Language_Defined_Aspect is Aspect_Identifier
      range Aspect_Address .. Aspect_Yield;

   -- Aspects that are class wide and require a 'Class attribute
   -- in the specification
   subtype Classwide_Aspect is Aspect_Identifier
      with Static_Predicate => Classwide_Aspect in
           Aspect_Global_Class
         | Aspect_Input_Class
         | Aspect_Output_Class
         | Aspect_Post_Class
         | Aspect_Pre_Class
         | Aspect_Read_Class
         | Aspect_Stable_Properties_Class
         | Aspect_Type_Invariant_Class
         | Aspect_Write_Class;

   -- Verifies the supplied string is a valid language defined aspect.
   -- If the input is valid, it returns the associated aspect enumeration
   -- value or the value Invalid if not.
   function Aspect_ID(Name : Strings.String) return Aspect_Identifier;

   -- Used for aspects that contain attributes (EX: Read'Class => )
   function Aspect_ID
      (Name      : Strings.String;
       Attribute : Strings.String) 
       return Aspect_Identifier;

   -- Information about the aspect
   type Info(Supported : Boolean := False) is null record;

   -- Returns important information assoicated with an aspect
   function Aspect_Info(ID : Valid_Aspect) return Info;

end Compiler.Aspects;
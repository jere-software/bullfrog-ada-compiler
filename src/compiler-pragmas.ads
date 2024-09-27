-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Compiler.Strings;

package Compiler.Pragmas is

   -- List of all language defined pragmas along with 
   -- invalid case
   type Pragma_Identifier is 
      (Invalid,
       Pragma_Admission_Policy,
       Pragma_All_Calls_Remote,
       Pragma_Assert,
       Pragma_Assertion_Policy,
       Pragma_Asynchronous,
       Pragma_Atomic,
       Pragma_Atomic_Components,
       Pragma_Attach_Handler,
       Pragma_Conflict_Check_Policy,
       Pragma_Convention,
       Pragma_CPU,
       Pragma_Default_Storage_Pool,
       Pragma_Detect_Blocking,
       Pragma_Discard_Names,
       Pragma_Dispatching_Domain,
       Pragma_Elaborate,
       Pragma_Elaborate_All,
       Pragma_Elaborate_Body,
       Pragma_Export,
       Pragma_Generate_Deadlines,
       Pragma_Import,
       Pragma_Independent,
       Pragma_Independent_Components,
       Pragma_Inline,
       Pragma_Inspection_Point,
       Pragma_Interrupt_Handler,
       Pragma_Interrupt_Priority,
       Pragma_Linker_Options,
       Pragma_List,
       Pragma_Locking_Policy,
       Pragma_No_Return,
       Pragma_Normalize_Scalars,
       Pragma_Optimize,
       Pragma_Pack,
       Pragma_Page,
       Pragma_Partition_Elaboration_Policy,
       Pragma_Preelaborable_Initialization,
       Pragma_Preelaborate,
       Pragma_Priority,
       Pragma_Priority_Specific_Dispatching,
       Pragma_Profile,
       Pragma_Pure,
       Pragma_Queuing_Policy,
       Pragma_Relative_Deadline,
       Pragma_Remote_Call_Interface,
       Pragma_Remote_Types,
       Pragma_Restrictions,
       Pragma_Reviewable,
       Pragma_Shared_Passive,
       Pragma_Storage_Size,
       Pragma_Suppress,
       Pragma_Task_Dispatching_Policy,
       Pragma_Unchecked_Union,
       Pragma_Unsuppress,
       Pragma_Volatile,
       Pragma_Volatile_Components);

   -- All valid pragmas, language defined and implementation defined
   subtype Valid_Pragma is Pragma_Identifier 
      range Pragma_Admission_Policy .. Pragma_Volatile_Components;

   -- Only language defined pragmas
   subtype Language_Defined_Pragma is Pragma_Identifier
      range Pragma_Admission_Policy .. Pragma_Volatile_Components;

   -- Verifies the supplied string is a valid language defined pragma.
   -- If the input is valid, it returns the associated pragma enumeration
   -- value or the value Invalid if not.
   function Pragma_ID(Name : Strings.String) return Pragma_Identifier;

   -- Information about the pragma
   type Info(Supported : Boolean := False) is null record;

   -- Returns important information assoicated with a pragma
   function Pragma_Info(ID : Valid_Pragma) return Info;

end Compiler.Pragmas;
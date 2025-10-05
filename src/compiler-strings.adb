-- Copyright (C) 2025
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

package body Compiler.Strings is

   function Make(Value : String) return Read_Only_Stream is
      (Ada.Streams.Root_Stream_Type with
         Capacity  => Value'Length,
         String    => Value,
         Remaining => Value'Length,
         Index     => 1);

   procedure Read
      (Stream : in out Read_Only_Stream;
       Item   :    out Ada.Streams.Stream_Element_Array;
       Last   :    out Ada.Streams.Stream_Element_Offset)
   is 
      subtype Stream_Element is Ada.Streams.Stream_Element;
      use type Ada.Streams.Stream_Element_Offset;

      Target : Ada.Streams.Stream_Element_Offset := Item'First - 1;
   begin

      if Stream.Remaining = 0 then
         raise Storage_Error;
      elsif Stream.Remaining < Item'Length then

         -- Copy remaining
         for Source in Stream.Index .. Stream.String'Last loop
            Target       := Target + 1;
            Item(Target) := Stream_Element(Pos(Stream.String(Source)));
         end loop;
         Last             := Target;
         Stream.Remaining := 0;
         Stream.Index     := Stream.String'Last + 1;

      else -- Item'Length <= Remaining
         
         -- Copy as much as possible
         for Element of Item loop
            Element      := Stream_Element(Pos(Stream.String(Stream.Index)));
            Stream.Index := Stream.Index + 1;
         end loop;
         Last             := Item'Last;
         Stream.Remaining := Stream.Remaining - Item'Length;

      end if;

   end Read;

   procedure Write
      (Stream : in out Read_Only_Stream;
       Item   : in     Ada.Streams.Stream_Element_Array)
   is begin
      raise Program_Error with "Write operation not implemented";
   end Write;

end Compiler.Strings;
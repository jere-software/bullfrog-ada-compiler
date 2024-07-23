-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

package body Object_Streams is

   procedure Read 
      (Stream : in out Object_Streams.Stream;
       Item   :    out Stream_Element_Array;
       Last   :    out Stream_Element_Offset)
   is 
      use type Stream_Element_Offset;
      use type Ada.Containers.Count_Type;
   begin

      -- Default to error value
      Last := Item'First - 1;

      -- Check if the buffer has more than enough elements
      if Stream.Elements.Length > Item'Length then

         -- Copy the buffer elements over
         for Index in 1 .. Item'Length loop
            Last := Last + 1;
            Item(Last) := Stream.Elements(Index);
         end loop;

         -- Delete the elements that where just copied
         Stream.Elements.Delete(Item'Length);

      -- Check if the buffer has some elements but not
      -- enough to fully fill the Item array
      elsif Stream.Elements.Length > 0 then

         -- Copy the buffer elements over
         for Element of Stream.Elements loop
            Last := Last + 1;
            Item(Last) := Element;
         end loop;

         -- The buffer is empty, so clear it
         Stream.Elements.Clear;

      end if;

   end Read;

   procedure Write 
      (Stream : in out Object_Streams.Stream;
       Item   : in     Stream_Element_Array) 
   is 
      use type Ada.Containers.Count_Type;
   begin

      -- Make rooom in the buffer
      Stream.Elements.Reserve_Capacity(Stream.Elements.Length + Item'Length);

      -- Copy the elements over
      for Thing of Item loop
         Stream.Elements.Append(Thing);
      end loop;
      
   end Write;

end Object_Streams;
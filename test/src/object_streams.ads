-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

private with Ada.Containers.Vectors;
with Ada.Streams;

package Object_Streams is

   -- Local redeclarations
   subtype Root_Stream_type      is Ada.Streams.Root_Stream_type;
   subtype Stream_Element_Array  is Ada.Streams.Stream_Element_Array;
   subtype Stream_Element_Offset is Ada.Streams.Stream_Element_Offset;

   -- Core type
   type Stream is new Root_Stream_type with private;

   -- Abstract class overrides
   overriding
   procedure Read 
      (Stream : in out Object_Streams.Stream;
       Item   :    out Stream_Element_Array;
       Last   :    out Stream_Element_Offset);

   overriding
   procedure Write 
      (Stream : in out Object_Streams.Stream;
       Item   : in     Stream_Element_Array);

private

   -- Local redeclaration
   subtype Stream_Element is Ada.Streams.Stream_Element;
   use type Stream_Element;

   -- Use a vector as the underlying buffer
   package Stream_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Stream_Element);

   -- Local redeclaration
   subtype Vector is Stream_Vectors.Vector;

   -- Pick a default buffer size to help keep allocations down
   Default_Buffer_Size : constant := 240;

   type Stream is new Root_Stream_type with record
      Elements : Vector := Stream_Vectors.Empty(Default_Buffer_Size);
   end record;

end Object_Streams;
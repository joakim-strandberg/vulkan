with Aida.Generic_Shared_Ptr;

with Ada.Containers;

with Aida.Strings.Generic_Mutable_Unbounded_String;

generic
   Capacity : Ada.Containers.Count_Type;
package Aida.Strings.Generic_Mutable_Unbounded_String_Shared_Ptr with SPARK_Mode is

   use type Ada.Containers.Count_Type;

   type T is private with Default_Initial_Condition => null;

   function Length (This : T) return Ada.Containers.Count_Type with
     Global => null;

   function Char_At (This  : T;
                     Index : Positive) return Character with
     Global => null,
     Pre => Length (This) > 0 and then Index <= Positive (Length (This));

   procedure Initialize (This : out T;
                         Text : String) with
     Global => null,
     Pre    => Text'Length <= Positive'Last,
     Post   => (Text'Length = Length (This));

   procedure Append (This : in out T;
                     Text : String) with
     Global => null;

   function To_String (This : T) return String with
     Pre    => Length (This) < MAX_LENGTH,
     Global => null;

   function "=" (L, R : T) return Boolean;

private
   pragma SPARK_Mode (Off);

   package US is new Aida.Strings.Generic_Mutable_Unbounded_String (Capacity);

   type US_Ptr is access US.T;

   package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => US.T,
                                                          P => US_Ptr);

   type T is
      record
         SP : Smart_Pointers.Pointer;
      end record;

   function Length (This : T) return Ada.Containers.Count_Type is (US.Length (Smart_Pointers.Value (This.SP).all));

   function Char_At (This  : T;
                     Index : Positive) return Character is (US.Char_At (Smart_Pointers.Value (This.SP).all, Index));

   function To_String (This : T) return String is (US.To_String (Smart_Pointers.Value (This.SP).all));

   pragma Inline (Length);
   pragma Inline (Char_At);
   pragma Inline (Initialize);
   pragma Inline (Append);
   pragma Inline (To_String);

end Aida.Strings.Generic_Mutable_Unbounded_String_Shared_Ptr;

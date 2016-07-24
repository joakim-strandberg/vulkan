with Ada.Containers.Formal_Vectors;

generic
   Capacity : Ada.Containers.Count_Type;
package Aida.Strings.Generic_Immutable_Unbounded_String with SPARK_Mode is

   use type Ada.Containers.Count_Type;

   type T is limited private with Default_Initial_Condition => null;

   function Length (This : T) return Ada.Containers.Count_Type with
     Global => null;

   function Char_At (This  : T;
                     Index : Positive) return Character with
     Global => null,
     Pre => Length (This) > 0 and then Index <= Positive (Length (This));

   function To_String (This : T) return String with
     Pre    => Length (This) < MAX_LENGTH,
     Global => null;

   function "=" (L, R : T) return Boolean with
     Global => null;

private

   package Char_Vectors is new Ada.Containers.Formal_Vectors (Index_Type   => Positive,
                                                              Element_Type => Character,
                                                              "="          => "=",
                                                              Bounded      => False);

   use all type Char_Vectors.Vector;

   type T is limited
      record
         Text : Char_Vectors.Vector (Capacity);
      end record;

   function "=" (L, R : T) return Boolean is (if Length (L.Text) = Length (R.Text) then
                                               (for all I in First_Index (L.Text)..Last_Index (L.Text) => Element (L.Text, I) = Element (R.Text, First_Index (R.Text) + I - First_Index (L.Text)))
                                             else
                                                False);

end Aida.Strings.Generic_Immutable_Unbounded_String;

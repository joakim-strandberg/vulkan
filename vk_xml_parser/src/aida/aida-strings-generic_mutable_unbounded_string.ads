with Ada.Containers.Formal_Vectors;

generic
   Capacity : Ada.Containers.Count_Type;
package Aida.Strings.Generic_Mutable_Unbounded_String with SPARK_Mode is

   use type Ada.Containers.Count_Type;

   type T is limited private with Default_Initial_Condition => null;

   function Length (This : T) return Ada.Containers.Count_Type with
     Global => null;

   function Char_At (This  : T;
                     Index : Positive) return Character with
    Pre => Length (This) > 0 and then Index <= Positive (Length (This));

   procedure Initialize (This : in out T;
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

   package Char_Vectors is new Ada.Containers.Formal_Vectors (Index_Type   => Positive,
                                                              Element_Type => Character,
                                                              "="          => "=",
                                                              Bounded      => False);

   use all type Char_Vectors.Vector;

   type T is limited
      record
         Text : Char_Vectors.Vector (Capacity);
      end record;

   function "=" (L, R : T) return Boolean is (L.Text = R.Text);

end Aida.Strings.Generic_Mutable_Unbounded_String;

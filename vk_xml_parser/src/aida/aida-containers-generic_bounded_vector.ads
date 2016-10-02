generic
   type Index_T is range <>;
   type Extended_Index_T is range <>;
   type Element_T is private;
   Null_Element : Element_T;
   with function "=" (Left, Right : Element_T) return Boolean is <>;
package Aida.Containers.Generic_Bounded_Vector with SPARK_Mode is

   pragma Assert (Index_T'Base (Extended_Index_T'First) = Index_T'First - 1);
   pragma Assert (Index_T'Base (Extended_Index_T'Last) = Index_T'Last);

   subtype Length_T is Count_Type range 0 .. Count_Type (Index_T'Last - Index_T'First + 1);

   type T is private with Default_Initial_Condition => True;

   pragma Warnings (Off, "unused variable",
                    Reason => "We cannot yet use pragma Unused(..)");
   function First_Index (This : T) return Index_T with
     Global => null;
   pragma Warnings (On, "unused variable");

   function Last_Index (This : T) return Extended_Index_T with
     Global => null;

   function Length (This : T) return Length_T with
     Global => null;

   function Element (This  : T;
                     Index : Index_T) return Element_T with
     Global => null;

   procedure Append (This     : in out T;
                     New_Item : Element_T) with
     Global => null,
     Pre  => Length (This) < Length_T'Last,
     Post => Length (This) = Length (This)'Old + 1;

   procedure Clear (This : in out T) with
     Global => null,
     Post => Length (This) = 0;

private

   type Element_Array_T is array (Index_T) of Element_T;

   type T is
      record
         My_Elements           : Element_Array_T := (others => Null_Element);
         My_Number_Of_Elements : Length_T := 0;
      end record;

   function Last_Index (This : T) return Extended_Index_T is (Extended_Index_T (Index_T'First - 1) + Extended_Index_T (This.My_Number_Of_Elements));

   function Length (This : T) return Length_T is (This.My_Number_Of_Elements);

end Aida.Containers.Generic_Bounded_Vector;

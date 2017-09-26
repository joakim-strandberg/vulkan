with Ada.Strings.Unbounded;
with Aida.Generic_Subprogram_Call_Result;
with Ada.Containers.Vectors;

pragma Elaborate_All (Aida.Generic_Subprogram_Call_Result);

use all type Ada.Containers.Count_Type;

package Aida.XML is

   type Tag_Name_T is new Ada.Strings.Unbounded.Unbounded_String;

   use all type Tag_Name_T;

   package Tag_Name_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                           Element_Type => Tag_Name_T,
                                                           "="          => "=");

   use all type Tag_Name_Vectors.Vector;

   subtype Tag_Name_Vector_T is Tag_Name_Vectors.Vector;
   -- This type is used in the API because it can be used in the containers of
   -- the standard library (it is not possible to use Tag_Name_Vectors.Vector
   -- to this end, the discriminant must be known)

   package Subprogram_Call_Result is new Aida.Generic_Subprogram_Call_Result;

private

   package Boolean_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                          Element_Type => Boolean,
                                                          "="          => "=");

end Aida.XML;

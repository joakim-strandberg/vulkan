with Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr.Mutable;
with Ada.Containers.Formal_Vectors;
with Aida.Generic_Subprogram_Call_Result;
with Aida.Containers.Generic_Immutable_Vector.Generic_Mutable_Vector;
with Aida.Containers.Formal_Vectors_Shared_Ptr;

pragma Elaborate_All (Aida.Containers.Formal_Vectors_Shared_Ptr);
pragma Elaborate_All (Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr.Mutable);
pragma Elaborate_All (Aida.Generic_Subprogram_Call_Result);
pragma Elaborate_All (Aida.Containers.Generic_Immutable_Vector.Generic_Mutable_Vector);

use all type Ada.Containers.Count_Type;

package Aida.XML with SPARK_Mode is

   package Tag_Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

   use all type Tag_Name.T;

--     package Tag_Name_Immutable_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
--                                                                                         Element_Type => Tag_Name.T,
--                                                                                         "="          => Tag_Name."=",
--                                                                                         Bounded      => False);
--
--     package Tag_Name_Vectors is new Tag_Name_Immutable_Vectors.Generic_Mutable_Vector;
--
--     subtype Tag_Name_Vector_T is Tag_Name_Vectors.T (100);

   package Tag_Name_Vectors is new Aida.Containers.Formal_Vectors_Shared_Ptr (Index_Type   => Positive,
                                                                              Element_Type => Tag_Name.T,
                                                                              "="          => Tag_Name."=",
                                                                              Bounded      => False);

   use all type Tag_Name_Vectors.Vector;

   subtype Tag_Name_Vector_T is Tag_Name_Vectors.Vector (10);
   -- This type is used in the API because it can be used in the containers of
   -- the standard library (it is not possible to use Tag_Name_Vectors.Vector
   -- to this end, the discriminant must be known)

   package Subprogram_Call_Result is new Aida.Generic_Subprogram_Call_Result (1000);

private

   package Boolean_Vectors is new Ada.Containers.Formal_Vectors (Index_Type   => Positive,
                                                                 Element_Type => Boolean,
                                                                 "="          => "=",
                                                                 Bounded      => False);

   package Mutable_Tag_Name is new Tag_Name.Mutable;

   use all type Mutable_Tag_Name.Mutable_T;

end Aida.XML;

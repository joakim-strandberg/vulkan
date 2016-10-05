with Vk_XML;

private with Aida.Strings;
private with Ada.Containers.Formal_Vectors;
private with Ada.Containers.Formal_Hashed_Maps;

package Vk_Package_Creator with SPARK_Mode is

   procedure Create_Vk_Package (R : Vk_XML.Registry_Shared_Ptr.T) with
     Global => null;

private

   package Member_Vectors is new Ada.Containers.Formal_Vectors (Index_Type   => Positive,
                                                                Element_Type => Vk_XML.Member_Shared_Ptr.T,
                                                                "="          => Vk_XML.Member_Shared_Ptr."=",
                                                                Bounded      => False);

   function Hash_Of_Unbounded_String (Key : Aida.Strings.Unbounded_String_Type) return Ada.Containers.Hash_Type;

   package C_Type_Name_To_Ada_Name_Map_Owner is new Ada.Containers.Formal_Hashed_Maps (Key_Type        => Aida.Strings.Unbounded_String_Type,
                                                                                       Element_Type    => Aida.Strings.Unbounded_String_Type,
                                                                                       Hash            => Hash_Of_Unbounded_String,
                                                                                       Equivalent_Keys => Aida.Strings."=",
                                                                                       "="             => Aida.Strings."=");

   package Struct_Type_Vectors is new Ada.Containers.Formal_Vectors (Index_Type   => Positive,
                                                                     Element_Type => Vk_XML.Type_Shared_Ptr.T,
                                                                     "="          => Vk_XML.Type_Shared_Ptr."=",
                                                                     Bounded      => False);

end Vk_Package_Creator;

with Vk_XML2;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;

package Vk_Package_Creator is

   procedure Create_Vk_Package (R : Vk_XML2.Registry.Ptr) with
     Global => null;

private

   package Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                         Element_Type => Vk_XML2.Member.Ptr,
                                                         "="          => Vk_XML2.Member."=");

   package Param_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Vk_XML2.Param.Ptr,
                                                        "="          => Vk_XML2.Param."=");

   function Hash_Of_Unbounded_String (Key : Ada.Strings.Unbounded.Unbounded_String) return Ada.Containers.Hash_Type;

   package C_Type_Name_To_Ada_Name_Map_Owner is new Ada.Containers.Hashed_Maps (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
                                                                                Element_Type    => Ada.Strings.Unbounded.Unbounded_String,
                                                                                Hash            => Hash_Of_Unbounded_String,
                                                                                Equivalent_Keys => Ada.Strings.Unbounded."=",
                                                                                "="             => Ada.Strings.Unbounded."=");

   package Struct_Type_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Vk_XML2.Type_T.Ptr,
                                                              "="          => Vk_XML2.Type_T."=");

end Vk_Package_Creator;

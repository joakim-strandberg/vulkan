with Generic_Vk_XML;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Vectors;
private with Ada.Containers.Hashed_Maps;

generic
   with package Vk_XML is new Generic_Vk_XML (<>);
package Generic_Vk_Package_Creator is

   procedure Create_Vk_Package (R : Vk_XML.Registry_Tag.Ptr);

private

   package Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                         Element_Type => Vk_XML.Member_Tag.Ptr,
                                                         "="          => Vk_XML.Member_Tag."=");

   package Param_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Vk_XML.Param_Tag.Ptr,
                                                        "="          => Vk_XML.Param_Tag."=");

   function Hash_Of_Unbounded_String (Key : Ada.Strings.Unbounded.Unbounded_String) return Ada.Containers.Hash_Type;

   package C_Type_Name_To_Ada_Name_Map_Owner is new Ada.Containers.Hashed_Maps (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
                                                                                Element_Type    => Ada.Strings.Unbounded.Unbounded_String,
                                                                                Hash            => Hash_Of_Unbounded_String,
                                                                                Equivalent_Keys => Ada.Strings.Unbounded."=",
                                                                                "="             => Ada.Strings.Unbounded."=");

   package Struct_Type_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Vk_XML.Type_Tag.Ptr,
                                                              "="          => Vk_XML.Type_Tag."=");

end Generic_Vk_Package_Creator;

with Ada.Containers.Formal_Vectors;
with Vk_XML;

package Vk_Package_Creator with SPARK_Mode is

   procedure Create_Vk_Package (R : Vk_XML.Registry_Shared_Ptr.T) with
     Global => null;

private

   package Member_Vectors is new Ada.Containers.Formal_Vectors (Index_Type   => Positive,
                                                                Element_Type => Vk_XML.Member_Shared_Ptr.T,
                                                                "="          => Vk_XML.Member_Shared_Ptr."=",
                                                                Bounded      => False);

end Vk_Package_Creator;

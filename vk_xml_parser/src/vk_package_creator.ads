with Vk_XML;

package Vk_Package_Creator with SPARK_Mode is

   procedure Create_Vk_Package (R : Vk_XML.Registry_Shared_Ptr.T) with
     Global => null;

end Vk_Package_Creator;

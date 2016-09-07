with Aida.XML;
with Vk_XML;

package Vk_XML_Reader with SPARK_Mode is

   procedure Parse (Contents      : String;
                    Registry      : in out Vk_XML.Registry_Shared_Ptr.T;
                    Call_Result   : in out Aida.XML.Subprogram_Call_Result.T) with
     Global => null;

end Vk_XML_Reader;

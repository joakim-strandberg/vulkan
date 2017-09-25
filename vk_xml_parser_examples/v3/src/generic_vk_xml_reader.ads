with Aida.XML;
with Generic_Vk_XML;
with Dynamic_Pools;

generic
   with package Vk_XML is new Generic_Vk_XML (<>);
package Generic_Vk_XML_Reader is

   procedure Parse (Contents    : Aida.String_T;
                    Registry    : not null Vk_XML.Registry_Tag.Ptr;
                    Call_Result : in out Aida.XML.Subprogram_Call_Result.T);

end Generic_Vk_XML_Reader;

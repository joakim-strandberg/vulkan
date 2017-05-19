with Aida.XML;
with Generic_Vk_XML;

generic
   with package Vk_XML is new Generic_Vk_XML (<>);
package Generic_Vk_XML_Reader is

   procedure Parse (Contents    : String;
                    Registry    : not null access Vk_XML.Registry.T;
                    Call_Result : in out Aida.XML.Subprogram_Call_Result.T);

end Generic_Vk_XML_Reader;

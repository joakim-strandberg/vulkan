with Aida.XML;
with Vk_XML2;
with Dynamic_Pools;

package Vk_XML_Reader is

   procedure Parse (Contents    : String;
                    Registry    : not null access Vk_XML2.Registry.T;
                    Subpool     : Dynamic_Pools.Subpool_Handle;
                    Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
     Global => null;

end Vk_XML_Reader;

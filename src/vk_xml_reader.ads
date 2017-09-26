with Aida.XML;
with Vk_XML.Registry_Tag;
with Dynamic_Pools;

package Vk_XML_Reader is

   procedure Parse (Contents    : Aida.String_T;
                    Registry    : not null Vk_XML.Registry_Tag.Ptr;
                    SH          : Dynamic_Pools.Subpool_Handle;
                    Call_Result : in out Aida.XML.Subprogram_Call_Result.T);

end Vk_XML_Reader;

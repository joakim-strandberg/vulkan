package body Vk_XML.Comment_Tag is

   procedure Set_Value (This  : in out T;
                        Value : Aida.String_T;
                        SP    : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Value := (Exists => True,
                        Value  => new (SP) Aida.String_T'(Value));
   end Set_Value;

end Vk_XML.Comment_Tag;

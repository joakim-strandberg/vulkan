package body Vk_XML.Require_Command_Tag is

   procedure Set_Name (This  : in out T;
                       Value : Aida.String_T;
                       SP    : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (SP) Aida.String_T'(Value));
   end Set_Name;

end Vk_XML.Require_Command_Tag;

package body Vk_XML.Unused_Tag is

   procedure Set_Start (This : in out T;
                        Value : Aida.String_T;
                        SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Start := (Exists => True,
                        Value  => new (SP) Aida.String_T'(Value));
   end Set_Start;

end Vk_XML.Unused_Tag;

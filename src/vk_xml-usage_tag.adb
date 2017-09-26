package body Vk_XML.Usage_Tag is

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Set_Struct (This : in out T;
                         Value : Aida.String_T;
                         SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Struct := (Exists => True,
                         Value  => new (SP) Aida.String_T'(Value));
   end Set_Struct;

   procedure Set_Command (This : in out T;
                          Value : Aida.String_T;
                          SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Command := (Exists => True,
                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Command;

end Vk_XML.Usage_Tag;

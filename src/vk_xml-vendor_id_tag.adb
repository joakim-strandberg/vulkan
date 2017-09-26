package body Vk_XML.Vendor_Id_Tag is

   procedure Set_Comment (This  : in out T;
                          Value : Aida.String_T;
                          SP    : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Comment := (Exists => True,
                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Comment;

   procedure Set_Id (This  : in out T;
                     Value : Aida.String_T;
                     SP    : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Id := (Exists => True,
                     Value  => new (SP) Aida.String_T'(Value));
   end Set_Id;

   procedure Set_Name (This  : in out T;
                       Value : Aida.String_T;
                       SP    : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (SP) Aida.String_T'(Value));
   end Set_Name;

end Vk_XML.Vendor_Id_Tag;

package body Vk_XML.Tag_Tag is

   pragma Suppress (Discriminant_Check);

   procedure Set_Name (This : in out T;
                       Value : Aida.String_T;
                       SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (SP) Aida.String_T'(Value));
   end Set_Name;

   procedure Set_Author (This  : in out T;
                         Value : Aida.String_T;
                         SP    : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Author := (Exists => True,
                         Value  => new (SP) Aida.String_T'(Value));
   end Set_Author;

   procedure Set_Contact (This  : in out T;
                          Value : Aida.String_T;
                          SP    : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Contact := (Exists => True,
                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Contact;

end Vk_XML.Tag_Tag;

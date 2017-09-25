package body Vk_XML.Extension_Tag is

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Set_Contact (This : in out T;
                          Value : Aida.String_T;
                          SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Contact := (Exists => True,
                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Contact;

   procedure Set_Author (This : in out T;
                         Value : Aida.String_T;
                         SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Author := (Exists => True,
                         Value  => new (SP) Aida.String_T'(Value));
   end Set_Author;

   procedure Set_Protect (This : in out T;
                          Value : Aida.String_T;
                          SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Protect := (Exists => True,
                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Protect;

   procedure Set_Supported (This : in out T;
                            Value : Supported_T)
   is
   begin
      This.My_Supported := (Exists => True,
                            Value  => Value);
   end Set_Supported;

   procedure Set_Number (This : in out T;
                         Value : Number_T)
   is
   begin
      This.My_Number := (Exists => True,
                         Value  => Value);
   end Set_Number;

   procedure Set_Name (This : in out T;
                       Value : Aida.String_T;
                       SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (SP) Aida.String_T'(Value));
   end Set_Name;

end Vk_XML.Extension_Tag;

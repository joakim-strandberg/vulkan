package body Vk_XML.Enums_Tag is

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Set_Comment (This : in out T;
                          Value : Aida.String_T;
                          SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Comment := (Exists => True,
                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Comment;

   procedure Set_Name (This : in out T;
                       Value : Aida.String_T;
                       SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (SP) Aida.String_T'(Value));
   end Set_Name;

   procedure Set_Type_Attribute (This : in out T;
                                 Value : Type_Attribue_T)
   is
   begin
      This.My_Type_Attribute := (Exists => True,
                                 Value  => Value);
   end Set_Type_Attribute;

end Vk_XML.Enums_Tag;

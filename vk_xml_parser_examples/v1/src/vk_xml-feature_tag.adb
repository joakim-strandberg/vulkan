package body Vk_XML.Feature_Tag is

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

   procedure Set_Number (This : in out T;
                         Value : Aida.String_T;
                         SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Number := (Exists => True,
                         Value  => new (SP) Aida.String_T'(Value));
   end Set_Number;

   procedure Set_Name (This : in out T;
                       Value : Aida.String_T;
                       SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (SP) Aida.String_T'(Value));
   end Set_Name;

   procedure Set_API (This : in out T;
                      Value : Aida.String_T;
                      SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_API := (Exists => True,
                      Value  => new (SP) Aida.String_T'(Value));
   end Set_API;

end Vk_XML.Feature_Tag;

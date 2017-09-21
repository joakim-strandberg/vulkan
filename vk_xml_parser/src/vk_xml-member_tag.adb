package body Vk_XML.Member_Tag is

   procedure Set_Valid_Extension_Structs (This : in out T;
                                          Value : Aida.String_T;
                                          SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Valid_Extension_Structs := (Exists => True,
                                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Valid_Extension_Structs;

   procedure Set_No_Auto_Validity (This : in out T;
                                   Value : Boolean)
   is
   begin
      This.My_No_Auto_Validity := (Exists => True,
                                   Value  => Value);
   end Set_No_Auto_Validity;

   procedure Set_Len (This : in out T;
                      Value : Aida.String_T;
                      SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Len := (Exists => True,
                      Value  => new (SP) Aida.String_T'(Value));
   end Set_Len;

   procedure Set_Optional (This : in out T;
                           Value : Boolean)
   is
   begin
      This.My_Optional := (Exists => True,
                           Value  => Value);
   end Set_Optional;

   procedure Append_Child (This  : in out T;
                           Child : Child_T)
   is
   begin
      This.My_Children.Append (Child);
   end Append_Child;

end Vk_XML.Member_Tag;

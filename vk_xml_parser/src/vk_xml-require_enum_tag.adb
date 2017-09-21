package body Vk_XML.Require_Enum_Tag is

   procedure Set_Bit_Position (This : in out T;
                               Value : Bit_Position_T)
   is
   begin
      This.My_Bit_Position := (Exists => True,
                               Value  => Value);
   end Set_Bit_Position;

   procedure Set_Extends (This : in out T;
                          Value : Aida.String_T;
                          SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Extends := (Exists => True,
                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Extends;

   procedure Set_Comment (This : in out T;
                          Value : Aida.String_T;
                          SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Comment := (Exists => True,
                          Value  => new (SP) Aida.String_T'(Value));
   end Set_Comment;

   procedure Set_Dir (This : in out T;
                      Value : Aida.String_T;
                      SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Dir := (Exists => True,
                      Value  => new (SP) Aida.String_T'(Value));
   end Set_Dir;

   procedure Set_Offset (This : in out T;
                         Value : Offset_T)
   is
   begin
      This.My_Offset := (Exists => True,
                         Value  => Value);
   end Set_Offset;

   procedure Set_Value (This : in out T;
                        Value : Aida.String_T;
                        SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Value := (Exists => True,
                        Value  => new (SP) Aida.String_T'(Value));
   end Set_Value;

   procedure Set_Name (This : in out T;
                       Value : Aida.String_T;
                       SP   : Dynamic_Pools.Subpool_Handle)
   is
   begin
      This.My_Name := (Exists => True,
                       Value  => new (SP) Aida.String_T'(Value));
   end Set_Name;

end Vk_XML.Require_Enum_Tag;

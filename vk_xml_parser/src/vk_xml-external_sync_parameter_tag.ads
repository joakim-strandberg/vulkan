package Vk_XML.External_Sync_Parameter_Tag is

   type T is tagged limited private;

   procedure Set_Value (This  : in out T;
                               Value : Aida.String_T;
                               SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Value,
     Post   => This.Exists_Value and This.Value = Value;

   function Value (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Value;

   function Exists_Value (This : T) return Boolean with
     Global => null;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type T is tagged limited
      record
         My_Value : Nullable_String_Ptr;
      end record;

   function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

   function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

end Vk_XML.External_Sync_Parameter_Tag;

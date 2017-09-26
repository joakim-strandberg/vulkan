package Vk_XML.Unused_Tag is

   type T is tagged limited private;

   procedure Set_Start (This  : in out T;
                        Value : Aida.String_T;
                        SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Start,
     Post   => This.Exists_Start and This.Start = Value;

   function Start (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Start;

   function Exists_Start (This : T) return Boolean with
     Global => null;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type T is tagged limited
      record
         My_Start : Nullable_String_Ptr;
      end record;

   function Start (This : T) return Aida.String_T is (This.My_Start.Value.all);

   function Exists_Start (This : T) return Boolean is (This.My_Start.Exists);

end Vk_XML.Unused_Tag;

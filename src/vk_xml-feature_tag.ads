with Vk_XML.Require_Tag;

package Vk_XML.Feature_Tag is

   type Child_Kind_Id_T is (
                            Child_Dummy,
                            Child_Require
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Dummy) is record
      case Kind_Id is
         when Child_Dummy   => Dummy   : not null String_Ptr := Empty_String'Access;
         when Child_Require => Require : not null Require_Tag.Ptr;
      end case;
   end record;

   package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Child_T,
                                                        "="          => "=");

   type Children_Ref (E : not null access constant Child_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type T is tagged limited private;

   function Children (This : aliased T) return Children_Ref;

   procedure Append_Child (This  : in out T;
                           Child : Child_T);

   procedure Set_API (This  : in out T;
                               Value : Aida.String_T;
                               SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_API,
     Post   => This.Exists_API and This.API = Value;

   function API (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_API;

   function Exists_API (This : T) return Boolean with
     Global => null;

   procedure Set_Name (This  : in out T;
                               Value : Aida.String_T;
                               SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Name,
     Post   => This.Exists_Name and This.Name = Value;

   function Name (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Name;

   function Exists_Name (This : T) return Boolean with
     Global => null;

   procedure Set_Number (This  : in out T;
                               Value : Aida.String_T;
                               SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Number,
     Post   => This.Exists_Number and This.Number = Value;

   function Number (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Number;

   function Exists_Number (This : T) return Boolean with
     Global => null;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type T is tagged limited
      record
         My_API      : Nullable_String_Ptr;
         My_Name     : Nullable_String_Ptr;
         My_Number   : Nullable_String_Ptr;
         My_Children : aliased Child_Vectors.Vector;
      end record;

   function API (This : T) return Aida.String_T is (This.My_API.Value.all);

   function Exists_API (This : T) return Boolean is (This.My_API.Exists);

   function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

   function Number (This : T) return Aida.String_T is (This.My_Number.Value.all);

   function Exists_Number (This : T) return Boolean is (This.My_Number.Exists);

   function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

end Vk_XML.Feature_Tag;

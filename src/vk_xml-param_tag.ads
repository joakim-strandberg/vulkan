with Vk_XML.Nested_Type_Tag;
with Vk_XML.Name_Tag;

package Vk_XML.Param_Tag is

   type Child_Kind_Id_T is (
                            Child_Nested_Type,
                            Child_XML_Text,
                            Child_Name
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Text) is record
      case Kind_Id is
         when Child_Nested_Type => Nested_Type : not null Nested_Type_Tag.Ptr;
         when Child_XML_Text    => XML_Text    : not null String_Ptr := Empty_String'Access;
         when Child_Name        => Name        : not null Name_Tag.Ptr;
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

   procedure Set_Optional (This  : in out T;
                           Value : Boolean) with
     Global => null,
     Pre    => not This.Exists_Optional,
     Post   => This.Exists_Optional and This.Optional = Value;

   function Optional (This : T) return Boolean with
     Global => null,
     Pre    => This.Exists_Optional;

   function Exists_Optional (This : T) return Boolean with
     Global => null;

   procedure Set_External_Sync (This  : in out T;
                               Value : Aida.String_T;
                               SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_External_Sync,
     Post   => This.Exists_External_Sync and This.External_Sync = Value;

   function External_Sync (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_External_Sync;

   function Exists_External_Sync (This : T) return Boolean with
     Global => null;

   procedure Set_Len (This  : in out T;
                               Value : Aida.String_T;
                               SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Len,
     Post   => This.Exists_Len and This.Len = Value;

   function Len (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Len;

   function Exists_Len (This : T) return Boolean with
     Global => null;

   procedure Set_No_Auto_Validity (This  : in out T;
                                   Value : Boolean) with
     Global => null,
     Pre    => not This.Exists_No_Auto_Validity,
     Post   => This.Exists_No_Auto_Validity and This.No_Auto_Validity = Value;

   function No_Auto_Validity (This : T) return Boolean with
     Global => null,
     Pre    => This.Exists_No_Auto_Validity;

   function Exists_No_Auto_Validity (This : T) return Boolean with
     Global => null;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type Nullable_Optional_T (Exists : Boolean := False) is record
      case Exists is
         when True => Value : Boolean;
         when False => null;
      end case;
   end record;

   type Nullable_No_Auto_Validity_T (Exists : Boolean := False) is record
      case Exists is
         when True => Value : Boolean;
         when False => null;
      end case;
   end record;

   type T is tagged limited
      record
         My_Children         : aliased Child_Vectors.Vector;
         My_Optional         : Nullable_Optional_T;
         My_External_Sync    : Nullable_String_Ptr;
         My_Len              : Nullable_String_Ptr;
         My_No_Auto_Validity : Nullable_No_Auto_Validity_T;
      end record;

   function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   function Optional (This : T) return Boolean is (This.My_Optional.Value);

   function Exists_Optional (This : T) return Boolean is (This.My_Optional.Exists);

   function External_Sync (This : T) return Aida.String_T is (This.My_External_Sync.Value.all);

   function Exists_External_Sync (This : T) return Boolean is (This.My_External_Sync.Exists);

   function Len (This : T) return Aida.String_T is (This.My_Len.Value.all);

   function Exists_Len (This : T) return Boolean is (This.My_Len.Exists);

   function No_Auto_Validity (This : T) return Boolean is (This.My_No_Auto_Validity.Value);

   function Exists_No_Auto_Validity (This : T) return Boolean is (This.My_No_Auto_Validity.Exists);

end Vk_XML.Param_Tag;

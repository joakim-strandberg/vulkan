with Vk_XML.Name_Tag;
with Vk_XML.Nested_Type_Tag;
with Vk_XML.Enum_Tag;
with Vk_XML.Comment_Tag;

package Vk_XML.Member_Tag is

   type Child_Kind_Id_T is (
                            Child_Name,
                            Child_Nested_Type,
                            Child_Enum,
                            Child_XML_Text,
                            Child_Comment
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Text) is record
      case Kind_Id is
         when Child_Name        => Name        : not null Name_Tag.Ptr;
         when Child_Nested_Type => Nested_Type : not null Nested_Type_Tag.Ptr;
         when Child_Enum        => Enum        : not null Enum_Tag.Ptr;
         when Child_XML_Text    => XML_Text    : not null String_Ptr := Empty_String'Access;
         when Child_Comment     => Comment     : not null Comment_Tag.Ptr;
      end case;
   end record;

   package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Child_T,
                                                        "="          => "=");

   type Children_Ref_T (E : not null access constant Child_Vectors.Vector) is limited null record with
     Implicit_Dereference => E;

   type T is tagged limited private;

   function Children (This : aliased T) return Children_Ref_T;

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

   procedure Set_Alt_Len (This  : in out T;
                          Value : Aida.String_T;
                          SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Alt_Len,
     Post   => This.Exists_Alt_Len and This.Alt_Len = Value;

   function Alt_Len (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Alt_Len;

   function Exists_Alt_Len (This : T) return Boolean with
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

   procedure Set_Valid_Extension_Structs (This  : in out T;
                                          Value : Aida.String_T;
                                          SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Valid_Extension_Structs,
     Post   => This.Exists_Valid_Extension_Structs and This.Valid_Extension_Structs = Value;

   function Valid_Extension_Structs (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Valid_Extension_Structs;

   function Exists_Valid_Extension_Structs (This : T) return Boolean with
     Global => null;

   procedure Set_Values (This  : in out T;
                         Value : Aida.String_T;
                         SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Values,
     Post   => This.Exists_Values and This.Values = Value;

   function Values (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Values;

   function Exists_Values (This : T) return Boolean with
     Global => null;

   procedure Set_Extern_Sync (This  : in out T;
                              Value : Boolean) with
     Global => null,
     Pre    => not This.Exists_Extern_Sync,
     Post   => This.Exists_Extern_Sync and This.Extern_Sync = Value;

   function Extern_Sync (This : T) return Boolean with
     Global => null,
     Pre    => This.Exists_Extern_Sync;

   function Exists_Extern_Sync (This : T) return Boolean with
     Global => null;

   function To_String (This : T) return Aida.String_T;

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

   type Nullable_Extern_Sync_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Boolean;
         when False => null;
      end case;
   end record;

   type T is tagged limited record
      My_Children                : aliased Child_Vectors.Vector;
      My_Optional                : Nullable_Optional_T;
      My_Len                     : Nullable_String_Ptr;
      My_No_Auto_Validity        : Nullable_No_Auto_Validity_T;
      My_Valid_Extension_Structs : Nullable_String_Ptr;
      My_Values                  : Nullable_String_Ptr;
      My_Alt_Len                 : Nullable_String_Ptr;
      My_Extern_Sync             : Nullable_Extern_Sync_T;
   end record;

   function Children (This : aliased T) return Children_Ref_T is ((E => This.My_Children'Access));

   function Optional (This : T) return Boolean is (This.My_Optional.Value);

   function Exists_Optional (This : T) return Boolean is (This.My_Optional.Exists);

   function Len (This : T) return Aida.String_T is (This.My_Len.Value.all);

   function Exists_Len (This : T) return Boolean is (This.My_Len.Exists);

   function No_Auto_Validity (This : T) return Boolean is (This.My_No_Auto_Validity.Value);

   function Exists_No_Auto_Validity (This : T) return Boolean is (This.My_No_Auto_Validity.Exists);

   function Valid_Extension_Structs (This : T) return Aida.String_T is (This.My_Valid_Extension_Structs.Value.all);

   function Exists_Valid_Extension_Structs (This : T) return Boolean is (This.My_Valid_Extension_Structs.Exists);

   function Values (This : T) return Aida.String_T is (This.My_Values.Value.all);

   function Exists_Values (This : T) return Boolean is (This.My_Values.Exists);

   function Alt_Len (This : T) return Aida.String_T is (This.My_Alt_Len.Value.all);

   function Exists_Alt_Len (This : T) return Boolean is (This.My_Alt_Len.Exists);

   function Extern_Sync (This : T) return Boolean is (This.My_Extern_Sync.Value);

   function Exists_Extern_Sync (This : T) return Boolean is (This.My_Extern_Sync.Exists);

end Vk_XML.Member_Tag;

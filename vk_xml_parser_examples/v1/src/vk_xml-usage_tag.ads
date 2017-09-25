package Vk_XML.Usage_Tag is

   type Child_Kind_Id_T is (
                            Child_XML_Text
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Text) is record
      case Kind_Id is
         when Child_XML_Text => XML_Text : String_Ptr;
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

   procedure Set_Command (This  : in out T;
                          Value : Aida.String_T;
                          SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Command,
     Post   => This.Exists_Command and This.Command = Value;

   function Command (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Command;

   function Exists_Command (This : T) return Boolean with
     Global => null;

   procedure Set_Struct (This  : in out T;
                         Value : Aida.String_T;
                         SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Struct,
     Post   => This.Exists_Struct and This.Struct = Value;

   function Struct (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Struct;

   function Exists_Struct (This : T) return Boolean with
     Global => null;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type T is tagged limited
      record
         My_Children : aliased Child_Vectors.Vector;
         My_Command  : Nullable_String_Ptr;
         My_Struct   : Nullable_String_Ptr;
      end record;

   function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   function Command (This : T) return Aida.String_T is (This.My_Command.Value.all);

   function Exists_Command (This : T) return Boolean is (This.My_Command.Exists);

   function Struct (This : T) return Aida.String_T is (This.My_Struct.Value.all);

   function Exists_Struct (This : T) return Boolean is (This.My_Struct.Exists);

end Vk_XML.Usage_Tag;

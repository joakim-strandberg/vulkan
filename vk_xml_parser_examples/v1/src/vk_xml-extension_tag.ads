with Vk_XML.Require_Tag;

package Vk_XML.Extension_Tag is

   type Number_T is new Positive range 1..30;

   type Supported_T is (
                        Vulkan,
                        Disabled
                       );

   type Child_Kind_Id_T is (
                            Child_Require
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Require) is record
      case Kind_Id is
         when Child_Require => Require : Require_Tag.Ptr;
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
                         Value : Number_T) with
     Global => null,
     Pre    => not This.Exists_Number,
     Post   => This.Exists_Number and This.Number = Value;

   function Number (This : T) return Number_T with
     Global => null,
     Pre    => This.Exists_Number;

   function Exists_Number (This : T) return Boolean with
     Global => null;

   procedure Set_Supported (This  : in out T;
                            Value : Supported_T) with
     Global => null,
     Pre    => not This.Exists_Supported,
     Post   => This.Exists_Supported and This.Supported = Value;

   function Supported (This : T) return Supported_T with
     Global => null,
     Pre    => This.Exists_Supported;

   function Exists_Supported (This : T) return Boolean with
     Global => null;

   procedure Set_Protect (This  : in out T;
                          Value : Aida.String_T;
                          SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Protect,
     Post   => This.Exists_Protect and This.Protect = Value;

   function Protect (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Protect;

   function Exists_Protect (This : T) return Boolean with
     Global => null;

   procedure Set_Author (This  : in out T;
                         Value : Aida.String_T;
                         SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Author,
     Post   => This.Exists_Author and This.Author = Value;

   function Author (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Author;

   function Exists_Author (This : T) return Boolean with
     Global => null;

   procedure Set_Contact (This  : in out T;
                          Value : Aida.String_T;
                          SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Contact,
     Post   => This.Exists_Contact and This.Contact = Value;

   function Contact (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Contact;

   function Exists_Contact (This : T) return Boolean with
     Global => null;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type Nullable_Number_T (Exists : Boolean := False) is record
      case Exists is
        when True => Value : Number_T;
         when False => null;
      end case;
   end record;

   type Nullable_Supported_T (Exists : Boolean := False) is record
      case Exists is
         when True => Value : Supported_T;
         when False => null;
      end case;
   end record;

   type T is tagged limited
      record
         My_Name      : Nullable_String_Ptr;
         My_Number    : Nullable_Number_T;
         My_Supported : Nullable_Supported_T;
         My_Children  : aliased Child_Vectors.Vector;
         My_Protect   : Nullable_String_Ptr;
         My_Author    : Nullable_String_Ptr;
         My_Contact   : Nullable_String_Ptr;
      end record;

   function Children (This : aliased T) return Children_Ref is ((E => This.My_Children'Access));

   function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

   function Number (This : T) return Number_T is (This.My_Number.Value);

   function Exists_Number (This : T) return Boolean is (This.My_Number.Exists);

   function Supported (This : T) return Supported_T is (This.My_Supported.Value);

   function Exists_Supported (This : T) return Boolean is (This.My_Supported.Exists);

   function Protect (This : T) return Aida.String_T is (This.My_Protect.Value.all);

   function Exists_Protect (This : T) return Boolean is (This.My_Protect.Exists);

   function Author (This : T) return Aida.String_T is (This.My_Author.Value.all);

   function Exists_Author (This : T) return Boolean is (This.My_Author.Exists);

   function Contact (This : T) return Aida.String_T is (This.My_Contact.Value.all);

   function Exists_Contact (This : T) return Boolean is (This.My_Contact.Exists);

end Vk_XML.Extension_Tag;

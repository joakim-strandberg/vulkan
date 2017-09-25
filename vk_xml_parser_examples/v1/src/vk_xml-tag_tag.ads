package Vk_XML.Tag_Tag is

   type T is tagged limited private with
     Default_Initial_Condition => not T.Exists_Name and not T.Exists_Author and not T.Exists_Contact;

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
     Pre    => This.Exists_Author;

   function Exists_Contact (This : T) return Boolean with
     Global => null;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type T is tagged limited
      record
         My_Name    : Nullable_String_Ptr;
         My_Author  : Nullable_String_Ptr;
         My_Contact : Nullable_String_Ptr;
      end record;

   function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

   function Author (This : T) return Aida.String_T is (This.My_Author.Value.all);

   function Exists_Author (This : T) return Boolean is (This.My_Author.Exists);

   function Contact (This : T) return Aida.String_T is (This.My_Contact.Value.all);

   function Exists_Contact (This : T) return Boolean is (This.My_Contact.Exists);

end Vk_XML.Tag_Tag;

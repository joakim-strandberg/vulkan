-- <vendorid name="KHR"    id="0x10000"            comment="This is the next available Khronos vendor ID"/>
package Vk_XML.Vendor_Id_Tag is

   type T is tagged limited private;

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

   procedure Set_Id (This  : in out T;
                     Value : Aida.String_T;
                     SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Id,
     Post   => This.Exists_Id and This.Id = Value;

   function Id (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Id;

   function Exists_Id (This : T) return Boolean with
     Global => null;

   procedure Set_Comment (This  : in out T;
                          Value : Aida.String_T;
                          SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Comment,
     Post   => This.Exists_Comment and This.Comment = Value;

   function Comment (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Comment;

   function Exists_Comment (This : T) return Boolean with
     Global => null;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type T is tagged limited
      record
         My_Name    : Nullable_String_Ptr;
         My_Id      : Nullable_String_Ptr;
         My_Comment : Nullable_String_Ptr;
      end record;

   function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

   function Id (This : T) return Aida.String_T is (This.My_Id.Value.all);

   function Exists_Id (This : T) return Boolean is (This.My_Id.Exists);

   function Comment (This : T) return Aida.String_T is (This.My_Comment.Value.all);

   function Exists_Comment (This : T) return Boolean is (This.My_Comment.Exists);

end Vk_XML.Vendor_Id_Tag;

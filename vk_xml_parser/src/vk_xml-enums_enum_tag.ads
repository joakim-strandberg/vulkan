with Aida;

package Vk_XML.Enums_Enum_Tag is

   use all type Aida.Int32_T;

   subtype Bit_Position_T is Aida.Int32_T range 0..32;

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

   procedure Set_Bit_Position (This  : in out T;
                               Value : Bit_Position_T) with
     Global => null,
     Pre    => not This.Exists_Bit_Position,
     Post   => This.Exists_Bit_Position and This.Bit_Position = Value;

   function Bit_Position (This : T) return Bit_Position_T with
     Global => null,
     Pre    => This.Exists_Bit_Position;

   function Exists_Bit_Position (This : T) return Boolean with
     Global => null;

   type Ptr is access all T with Storage_Pool => Main_Pool;

private

   type Nullable_Bit_Position_T (Exists : Boolean := False) is record
      case Exists is
      when True => Value : Bit_Position_T;
         when False => null;
      end case;
   end record;

   type T is tagged limited record
      My_Value        : Nullable_String_Ptr;
      My_Name         : Nullable_String_Ptr;
      My_Comment      : Nullable_String_Ptr;
      My_Bit_Position : Nullable_Bit_Position_T;
   end record;

   function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

   function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

   function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

   function Comment (This : T) return Aida.String_T is (This.My_Comment.Value.all);

   function Exists_Comment (This : T) return Boolean is (This.My_Comment.Exists);

   function Bit_Position (This : T) return Bit_Position_T is (This.My_Bit_Position.Value);

   function Exists_Bit_Position (This : T) return Boolean is (This.My_Bit_Position.Exists);

end Vk_XML.Enums_Enum_Tag;

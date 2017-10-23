package Vk_XML.Require_Enum_Tag is

   type Offset_T is new Aida.Int32_T range 0..45;

   type Bit_Position_T is new Aida.Int32_T range 0..23;

   type Dir_T is (
                  Minus_Sign
                  );

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

   procedure Set_Offset (This  : in out T;
                         Value : Offset_T) with
     Global => null,
     Pre    => not This.Exists_Offset,
     Post   => This.Exists_Offset and This.Offset = Value;

   function Offset (This : T) return Offset_T with
     Global => null,
     Pre    => This.Exists_Offset;

   function Exists_Offset (This : T) return Boolean with
     Global => null;

   procedure Set_Dir (This  : in out T;
                      Value : Dir_T) with
     Global => null,
     Pre    => not This.Exists_Dir,
     Post   => This.Exists_Dir and This.Dir = Value;

   function Dir (This : T) return Dir_T with
     Global => null,
     Pre    => This.Exists_Dir;

   function Exists_Dir (This : T) return Boolean with
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

   procedure Set_Extends (This  : in out T;
                          Value : Aida.String_T;
                          SP    : Dynamic_Pools.Subpool_Handle) with
     Global => null,
     Pre    => not This.Exists_Extends,
     Post   => This.Exists_Extends and This.Extends = Value;

   function Extends (This : T) return Aida.String_T with
     Global => null,
     Pre    => This.Exists_Extends;

   function Exists_Extends (This : T) return Boolean with
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

   type Nullable_Offset_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Offset_T;
         when False => null;
      end case;
   end record;

   type Nullable_Bit_Position_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Bit_Position_T;
         when False => null;
      end case;
   end record;

   type Nullable_Dir_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Dir_T;
         when False => null;
      end case;
   end record;

   type T is tagged limited
      record
         My_Name         : Nullable_String_Ptr;
         My_Value        : Nullable_String_Ptr;
         My_Offset       : Nullable_Offset_T;
         My_Dir          : Nullable_Dir_T;
         My_Extends      : Nullable_String_Ptr;
         My_Comment      : Nullable_String_Ptr;
         My_Bit_Position : Nullable_Bit_Position_T;
      end record;

   function Name (This : T) return Aida.String_T is (This.My_Name.Value.all);

   function Exists_Name (This : T) return Boolean is (This.My_Name.Exists);

   function Value (This : T) return Aida.String_T is (This.My_Value.Value.all);

   function Exists_Value (This : T) return Boolean is (This.My_Value.Exists);

   function Offset (This : T) return Offset_T is (This.My_Offset.Value);

   function Exists_Offset (This : T) return Boolean is (This.My_Offset.Exists);

   function Dir (This : T) return Dir_T is (This.My_Dir.Value);

   function Exists_Dir (This : T) return Boolean is (This.My_Dir.Exists);

   function Extends (This : T) return Aida.String_T is (This.My_Extends.Value.all);

   function Exists_Extends (This : T) return Boolean is (This.My_Extends.Exists);

   function Comment (This : T) return Aida.String_T is (This.My_Comment.Value.all);

   function Exists_Comment (This : T) return Boolean is (This.My_Comment.Exists);

   function Bit_Position (This : T) return Bit_Position_T is (This.My_Bit_Position.Value);

   function Exists_Bit_Position (This : T) return Boolean is (This.My_Bit_Position.Exists);

end Vk_XML.Require_Enum_Tag;

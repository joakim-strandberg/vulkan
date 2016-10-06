with Aida.Generic_Shared_Ptr;
with Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr.Mutable;
with Aida.Containers.Generic_Immutable_Vector.Generic_Mutable_Vector;
with Aida.Strings.Generic_Mutable_Unbounded_String;

pragma Elaborate_All (Aida.Generic_Shared_Ptr);
pragma Elaborate_All (Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr.Mutable);
pragma Elaborate_All (Aida.Containers.Generic_Immutable_Vector.Generic_Mutable_Vector);
pragma Elaborate_All (Aida.Strings.Generic_Mutable_Unbounded_String);

package Vk_XML with SPARK_Mode is

   package XML_Text is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

   package XML_Out_Commented_Message_Shared_Ptr is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

   -- Created in order to avoid making the Vk_XML.xml parser application a memory hog.
   type XML_Dummy_T is
      record
         null;
      end record;

   package Tag with SPARK_Mode is

      package Fs is
         package Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         package Author is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         package Contact is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);
      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name.T;

      function Author (This : T) return Fs.Author.T;

      function Contact (This : T) return Fs.Contact.T;

      procedure Set_Name (This : in out T;
                          Text : String);

      procedure Set_Author (This : in out T;
                            Text : String) with
        Global => null;

      procedure Set_Contact (This : in out T;
                             Text : String) with
        Global => null;

   private

      package Name_P is new Fs.Name.Mutable;

      package Author_P is new Fs.Author.Mutable;

      package Contact_P is new Fs.Contact.Mutable;

      type T is limited
         record
            My_Name    : Name_P.Mutable_T;
            My_Author  : Author_P.Mutable_T;
            My_Contact : Contact_P.Mutable_T;
         end record;

      function Name (This : T) return Fs.Name.T is (Fs.Name.T (This.My_Name));

      function Author (This : T) return Fs.Author.T is (Fs.Author.T (This.My_Author));

      function Contact (This : T) return Fs.Contact.T is (Fs.Contact.T (This.My_Contact));

   end Tag;

   package Tag_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Name (This : T) return Tag.Fs.Name.T;

      function Author (This : T) return Tag.Fs.Author.T;

      function Contact (This : T) return Tag.Fs.Contact.T;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Author (This : in out T;
                            Text : String) with
        Global => null;

      procedure Set_Contact (This : in out T;
                             Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Tag.T;

      type Tag_Ptr is access Tag.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Tag.T,
                                                             P => Tag_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Tag.T);
         end record;

      function Name (This : T) return Tag.Fs.Name.T is (Name (Smart_Pointers.Value (This.SP).all));

      function Author (This : T) return Tag.Fs.Author.T is (Author (Smart_Pointers.Value (This.SP).all));

      function Contact (This : T) return Tag.Fs.Contact.T is (Contact (Smart_Pointers.Value (This.SP).all));

      pragma Inline (Name);
      pragma Inline (Author);
      pragma Inline (Contact);

   end Tag_Shared_Ptr;

   package Tags with SPARK_Mode is

      package Fs is
         type Child_Kind_Id_T is (
                                  Child_Dummy,
                                  Child_Tag
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_Dummy) is record
            case Kind_Id is
               when Child_Dummy => Dummy_V : aliased XML_Dummy_T;
               when Child_Tag   => Tag_V : aliased Tag_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);
      end Fs;

      type T is private;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Children_P is new Tags.Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is
         record
            My_Children : Children_P.T (100);
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Tags;

   package Tags_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Tags.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Tags.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Vk_XML.Tags.T;

      type Tags_Ptr is access Tags.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Tags.T,
                                                             P => Tags_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Tags.T);
         end record;

      function Children (This : T) return Vk_XML.Tags.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

   end Tags_Shared_Ptr;

   -- Representation of <comment>...<comment>
   package Comment with SPARK_Mode is

      package Fs is
         package Value is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);
      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Value (This : T) return Fs.Value.T with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String);

   private

      package Value_P is new Vk_XML.Comment.Fs.Value.Mutable;

      use all type Value_P.Mutable_T;

      type T is limited
         record
            My_Value : Value_P.Mutable_T;
         end record;

      function Value (This : T) return Fs.Value.T is (Fs.Value.T (This.My_Value));

   end Comment;

   package Comment_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Value (This : T) return Comment.Fs.Value.T with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String);

   private
      pragma SPARK_Mode (Off);

      type Comment_Ptr is access Comment.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Comment.T,
                                                             P => Comment_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Comment.T);
         end record;

      function Value (This : T) return Comment.Fs.Value.T is (Comment.Value (Smart_Pointers.Value (This.SP).all));

      pragma Inline (Value);

   end Comment_Shared_Ptr;

   package Name with SPARK_Mode is
      package Fs is
         package Value is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);
      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Value (This : T) return Fs.Value.T;

      procedure Set_Value (This : in out T;
                           Text : String);

   private

      package Mutable_Value is new Fs.Value.Mutable;

      use all type Mutable_Value.Mutable_T;

      type T is limited
         record
            My_Value : Mutable_Value.Mutable_T;
         end record;

      function Value (This : T) return Fs.Value.T is (Fs.Value.T (This.My_Value));

   end Name;

   package Name_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Value (This : T) return Name.Fs.Value.T;

      procedure Set_Value (This : in out T;
                           Text : String);

   private
      pragma SPARK_Mode (Off);

      use all type Name.T;

      type Name_Ptr is access Name.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Name.T,
                                                             P => Name_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Name.T);
         end record;

      function Value (This : T) return Name.Fs.Value.T is (Value (Smart_Pointers.Value (This.SP).all));

   end Name_Shared_Ptr;

   package Nested_Type with SPARK_Mode is

      package Fs is

         package Value is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Value_T (Exists : Boolean := False) is
            record
               case Exists is
                  when True  => Value_V : Value.T;
                  when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Value (This : T) return Fs.Nullable_Value_T with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String) with
        Global => null;

   private

      package Mutable_Value is new Fs.Value.Mutable;

      use all type Mutable_Value.Mutable_T;

      type Nullable_Mutable_Value_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Value.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_Value : Nullable_Mutable_Value_T;
         end record;

      function Value (This : T) return Fs.Nullable_Value_T is (if This.My_Value.Exists then
                                                                               (Exists => True, Value_V => Fs.Value.T (This.My_Value.Value))
                                                                             else
                                                                               (Exists => False));

   end Nested_Type;

   package Nested_Type_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Value (This : T) return Nested_Type.Fs.Nullable_Value_T with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Nested_Type.T;

      type Nested_Type_Ptr is access Nested_Type.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Nested_Type.T,
                                                             P => Nested_Type_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Nested_Type.T);
         end record;

      function Value (This : T) return Nested_Type.Fs.Nullable_Value_T is (Value (Smart_Pointers.Value (This.SP).all));

   end Nested_Type_Shared_Ptr;

   package Enum with SPARK_Mode is

      package Fs is

         package Value is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Value (This : T) return Fs.Value.T with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String) with
        Global => null;

   private

      package Mutable_Value is new Fs.Value.Mutable;

      use all type Mutable_Value.Mutable_T;


      type T is limited
         record
            My_Value : Mutable_Value.Mutable_T;
         end record;

      function Value (This : T) return Fs.Value.T is (Fs.Value.T (This.My_Value));

   end Enum;

   package Enum_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Value (This : T) return Enum.Fs.Value.T with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Enum.T;

      type Enum_Ptr is access Enum.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Enum.T,
                                                             P => Enum_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Enum.T);
         end record;

      function Value (This : T) return Enum.Fs.Value.T is (Value (Smart_Pointers.Value (This.SP).all));

   end Enum_Shared_Ptr;

   -- enum tags that has an <enums>-tag as parent
   package Enums_Enum with SPARK_Mode is

      package Fs is

         package Value is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Value_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value_V : Value.T;
               when False => null;
               end case;
            end record;

         package Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Name_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Name.T;
               when False => null;
               end case;
            end record;

         package Comment is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Comment_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Comment.T;
               when False => null;
               end case;
            end record;

         type Bit_Position_T is new Natural range 0..32;

         type Nullable_Bit_Position_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Bit_Position_T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Value (This : T) return Fs.Nullable_Value_T with
        Global => null;

      function Name (This : T) return Fs.Nullable_Name_T with
        Global => null;

      function Comment (This : T) return Fs.Nullable_Comment_T with
        Global => null;

      function Bit_Position (This : T) return Fs.Nullable_Bit_Position_T with
        Global => null;

      procedure To_Standard_Out (This : T) with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String) with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Bit_Position (This  : in out T;
                                  Value : Fs.Bit_Position_T) with
        Global => null;

   private

      package Mutable_Value is new Fs.Value.Mutable;

      use all type Mutable_Value.Mutable_T;

      type Nullable_Mutable_Value_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Value.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Name is new Fs.Name.Mutable;

      use all type Mutable_Name.Mutable_T;

      type Nullable_Mutable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Name.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Comment is new Fs.Comment.Mutable;

      use all type Mutable_Comment.Mutable_T;

      type Nullable_Mutable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Comment.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_Value        : Nullable_Mutable_Value_T;
            My_Name         : Nullable_Mutable_Name_T;
            My_Comment      : Nullable_Mutable_Comment_T;
            My_Bit_Position : Fs.Nullable_Bit_Position_T;
         end record;

      function Value (This : T) return Fs.Nullable_Value_T is (if This.My_Value.Exists then
                                                                 (Exists => True, Value_V => Fs.Value.T (This.My_Value.Value))
                                                               else
                                                                 (Exists => False));

      function Name (This : T) return Fs.Nullable_Name_T is (if This.My_Name.Exists then
                                                               (Exists => True, Value => Fs.Name.T (This.My_Name.Value))
                                                             else
                                                               (Exists => False));

      function Comment (This : T) return Fs.Nullable_Comment_T is (if This.My_Comment.Exists then
                                                                     (Exists => True, Value => Fs.Comment.T (This.My_Comment.Value))
                                                                   else
                                                                     (Exists => False));

      function Bit_Position (This : T) return Fs.Nullable_Bit_Position_T is (This.My_Bit_Position);

   end Enums_Enum;

   package Enums_Enum_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Value (This : T) return Enums_Enum.Fs.Nullable_Value_T with
        Global => null;

      function Name (This : T) return Enums_Enum.Fs.Nullable_Name_T with
        Global => null;

      function Comment (This : T) return Enums_Enum.Fs.Nullable_Comment_T with
        Global => null;

      function Bit_Position (This : T) return Enums_Enum.Fs.Nullable_Bit_Position_T with
        Global => null;

      procedure To_Standard_Out (This : T) with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String) with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Bit_Position (This  : in out T;
                                  Value : Enums_Enum.Fs.Bit_Position_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Enums_Enum.T;

      type Enums_Enum_Ptr is access Enums_Enum.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Enums_Enum.T,
                                                             P => Enums_Enum_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Enums_Enum.T);
         end record;

      function Value (This : T) return Enums_Enum.Fs.Nullable_Value_T is (Value (Smart_Pointers.Value (This.SP).all));

      function Name (This : T) return Enums_Enum.Fs.Nullable_Name_T is (Name (Smart_Pointers.Value (This.SP).all));

      function Comment (This : T) return Enums_Enum.Fs.Nullable_Comment_T is (Comment (Smart_Pointers.Value (This.SP).all));

      function Bit_Position (This : T) return Enums_Enum.Fs.Nullable_Bit_Position_T is (Bit_Position (Smart_Pointers.Value (This.SP).all));

   end Enums_Enum_Shared_Ptr;

   package Member with SPARK_Mode is

      package Fs is

         type Child_Kind_Id_T is (
                                  Child_Dummy,
                                  Child_Name,
                                  Child_Nested_Type,
                                  Child_Enum,
                                  Child_XML_Text
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_Dummy) is record
            case Kind_Id is
               when Child_Dummy       => Dummy_V       : aliased XML_Dummy_T;
               when Child_Name        => Name_V        : aliased Vk_XML.Name_Shared_Ptr.T;
               when Child_Nested_Type => Nested_Type_V : aliased Vk_XML.Nested_Type_Shared_Ptr.T;
               when Child_Enum        => Enum_V        : aliased Vk_XML.Enum_Shared_Ptr.T;
               when Child_XML_Text    => XML_Text_V    : aliased Vk_XML.XML_Text.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

         type Optional_T is new Boolean;

         package Len is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Len_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Len.T;
               when False => null;
               end case;
            end record;

         type No_Auto_Validity_T is new Boolean;

         package Valid_Extension_Structs is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Valid_Extension_Structs_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Valid_Extension_Structs.T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Optional (This : T) return Fs.Optional_T with
        Global => null;

      function Len (This : T) return Fs.Nullable_Len_T with
        Global => null;

      function No_Auto_Validity (This : T) return Fs.No_Auto_Validity_T with
        Global => null;

      function Valid_Extension_Structs (This : T) return Fs.Nullable_Valid_Extension_Structs_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) with
        Global => null;

      procedure Set_Len (This : in out T;
                         Text : String) with
        Global => null;

      procedure Set_No_Auto_Validity (This  : in out T;
                                      Value : Boolean) with
        Global => null;

      procedure Set_Valid_Extension_Structs (This : in out T;
                                             Text : String) with
        Global => null;

   private

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      package Mutable_Len is new Fs.Len.Mutable;

      use all type Mutable_Len.Mutable_T;

      type Nullable_Mutable_Len_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Len.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Valid_Extension_Structs is new Fs.Valid_Extension_Structs.Mutable;

      use all type Mutable_Valid_Extension_Structs.Mutable_T;

      type Nullable_Mutable_Valid_Extension_Structs_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Valid_Extension_Structs.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_Children                : Mutable_Children.T (10);
            My_Optional                : Fs.Optional_T := Fs.Optional_T (False);
            My_Len                     : Nullable_Mutable_Len_T;
            My_No_Auto_Validity        : Fs.No_Auto_Validity_T := Fs.No_Auto_Validity_T (False);
            My_Valid_Extension_Structs : Nullable_Mutable_Valid_Extension_Structs_T;
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

      function Optional (This : T) return Fs.Optional_T is (This.My_Optional);

      function Len (This : T) return Fs.Nullable_Len_T is (if This.My_Len.Exists then
                                                             (Exists => True, Value => Fs.Len.T (This.My_Len.Value))
                                                           else
                                                             (Exists => False));

      function No_Auto_Validity (This : T) return Fs.No_Auto_Validity_T is (This.My_No_Auto_Validity);

      function Valid_Extension_Structs (This : T) return Fs.Nullable_Valid_Extension_Structs_T is (if This.My_Valid_Extension_Structs.Exists then
                                                                                                     (Exists => True, Value => Fs.Valid_Extension_Structs.T (This.My_Valid_Extension_Structs.Value))
                                                                                                   else
                                                                                                     (Exists => False));

   end Member;

   package Member_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Member.Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Optional (This : T) return Member.Fs.Optional_T with
        Global => null;

      function Len (This : T) return Member.Fs.Nullable_Len_T with
        Global => null;

      function No_Auto_Validity (This : T) return Member.Fs.No_Auto_Validity_T with
        Global => null;

      function Valid_Extension_Structs (This : T) return Member.Fs.Nullable_Valid_Extension_Structs_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Member.Fs.Child_T) with
        Global => null;

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) with
        Global => null;

      procedure Set_Len (This : in out T;
                         Text : String) with
        Global => null;

      procedure Set_No_Auto_Validity (This  : in out T;
                                      Value : Boolean) with
        Global => null;

      procedure Set_Valid_Extension_Structs (This : in out T;
                                             Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Member.T;

      type Member_Ptr is access Member.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Member.T,
                                                             P => Member_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Member.T);
         end record;

      function Children (This : T) return Member.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

      function Optional (This : T) return Member.Fs.Optional_T is (Optional (Smart_Pointers.Value (This.SP).all));

      function Len (This : T) return Member.Fs.Nullable_Len_T is (Len (Smart_Pointers.Value (This.SP).all));

      function No_Auto_Validity (This : T) return Member.Fs.No_Auto_Validity_T is (No_Auto_Validity (Smart_Pointers.Value (This.SP).all));

      function Valid_Extension_Structs (This : T) return Member.Fs.Nullable_Valid_Extension_Structs_T is (Valid_Extension_Structs (Smart_Pointers.Value (This.SP).all));

   end Member_Shared_Ptr;

   package Usage with SPARK_Mode is

      package Fs is

         type Child_Kind_Id_T is (
                                  Child_Dummy,
                                  Child_XML_Text
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_Dummy) is record
            case Kind_Id is
               when Child_Dummy    => Dummy_V    : aliased XML_Dummy_T;
               when Child_XML_Text => XML_Text_V : aliased Vk_XML.XML_Text.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

         package Command is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Command_T (Exists : Boolean := False) is
            record
               case Exists is
                  when True  => Value : Command.T;
                  when False => null;
               end case;
            end record;

         package Struct is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Struct_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Struct.T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Command (This : T) return Fs.Nullable_Command_T with
        Global => null;

      function Struct (This : T) return Fs.Nullable_Struct_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

      procedure Set_Command (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Struct (This : in out T;
                            Text : String) with
        Global => null;

   private

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      package Mutable_Command is new Fs.Command.Mutable;

      use all type Mutable_Command.Mutable_T;

      type Nullable_Mutable_Command_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Command.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Struct is new Fs.Struct.Mutable;

      use all type Mutable_Struct.Mutable_T;

      type Nullable_Mutable_Struct_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Struct.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_Children : Mutable_Children.T (10);
            My_Command  : Nullable_Mutable_Command_T;
            My_Struct   : Nullable_Mutable_Struct_T;
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

      function Command (This : T) return Fs.Nullable_Command_T is (if This.My_Command.Exists then
                                                                     (Exists => True, Value => Fs.Command.T (This.My_Command.Value))
                                                                   else
                                                                     (Exists => False));

      function Struct (This : T) return Fs.Nullable_Struct_T is (if This.My_Struct.Exists then
                                                                   (Exists => True, Value => Fs.Struct.T (This.My_Struct.Value))
                                                                 else
                                                                   (Exists => False));

   end Usage;

   package Usage_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Usage.Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Command (This : T) return Usage.Fs.Nullable_Command_T with
        Global => null;

      function Struct (This : T) return Usage.Fs.Nullable_Struct_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Usage.Fs.Child_T) with
        Global => null;

      procedure Set_Command (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Struct (This : in out T;
                            Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Usage.T;

      type Usage_Ptr is access Usage.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Usage.T,
                                                             P => Usage_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Usage.T);
         end record;

      function Children (This : T) return Usage.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

      function Command (This : T) return Usage.Fs.Nullable_Command_T is (Command (Smart_Pointers.Value (This.SP).all));

      function Struct (This : T) return Usage.Fs.Nullable_Struct_T is (Struct (Smart_Pointers.Value (This.SP).all));

   end Usage_Shared_Ptr;

   package Validity with SPARK_Mode is

      package Fs is

         type Child_Kind_Id_T is (
                                  Child_Dummy,
                                  Child_Usage
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_Dummy) is record
            case Kind_Id is
               when Child_Dummy => Dummy_V : aliased XML_Dummy_T;
               when Child_Usage => Usage_V : aliased Vk_XML.Usage_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Children : Mutable_Children.T (10);
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Validity;

   package Validity_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Validity.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Validity.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Validity.T;

      type Validity_Ptr is access Validity.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Validity.T,
                                                             P => Validity_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Validity.T);
         end record;

      function Children (This : T) return Validity.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

   end Validity_Shared_Ptr;

   -- <type>..</type>
   package Type_T with SPARK_Mode is

      package Fs is

         package Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Name_T (Exists : Boolean := False) is
            record
               case Exists is
                  when True  => Value : Name.T;
                  when False => null;
               end case;
            end record;

         package Category is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         package Requires is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Requires_T (Exists : Boolean := False) is
            record
               case Exists is
                  when True  => Value : Requires.T;
                  when False => null;
               end case;
            end record;

         package Parent is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Parent_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Parent.T;
               when False => null;
               end case;
            end record;

         type Returned_Only_T is new Boolean;

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_XML_Text,
                                  Child_Name,
                                  Child_Nested_Type,
                                  Child_Member,
                                  Child_Validity,
                                  Child_Out_Commented_Message
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy   => XML_Dummy_V   : aliased XML_Dummy_T;
               when Child_XML_Text    => XML_Text_V    : aliased Vk_XML.XML_Text.T;
               when Child_Name        => Name_V        : aliased Vk_XML.Name_Shared_Ptr.T;
               when Child_Nested_Type => Nested_Type_V : aliased Vk_XML.Nested_Type_Shared_Ptr.T;
               when Child_Member      => Member_V      : aliased Vk_XML.Member_Shared_Ptr.T;
               when Child_Validity    => Validity_V    : aliased Vk_XML.Validity_Shared_Ptr.T;
               when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

         package Comment is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Comment_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Comment.T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_T with
        Global => null;

      function Category (This : T) return Fs.Category.T with
        Global => null;

      function Requires (This : T) return Fs.Nullable_Requires_T with
        Global => null;

      function Parent (This : T) return Fs.Nullable_Parent_T with
        Global => null;

      function Returned_Only (This : T) return Fs.Returned_Only_T with
        Global => null;

      function Comment (This : T) return Fs.Nullable_Comment_T with
        Global => null;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      function To_String (This : T) return String with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Category (This : in out T;
                              Text : String) with
        Global => null;

      procedure Set_Requires (This : in out T;
                              Text : String) with
        Global => null;

      procedure Set_Parent (This : in out T;
                            Text : String) with
        Global => null;

      procedure Set_Returned_Only (This  : in out T;
                                   Value : Boolean) with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Name_P is new Fs.Name.Mutable;

      type Nullable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True => Value : Name_P.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Requires is new Fs.Requires.Mutable;

      use all type Mutable_Requires.Mutable_T;

      type Nullable_Mutable_Requires_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Requires.Mutable_T;
               when False => null;
            end case;
         end record;

      package Category_P is new Fs.Category.Mutable;

      package Mutable_Parent is new Fs.Parent.Mutable;

      use all type Mutable_Parent.Mutable_T;

      type Nullable_Mutable_Parent_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Parent.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Comment is new Fs.Comment.Mutable;

      use all type Mutable_Comment.Mutable_T;

      type Nullable_Mutable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Comment.Mutable_T;
               when False => null;
            end case;
         end record;

      package Children_P is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Name          : Nullable_Name_T;
            My_Category      : Category_P.Mutable_T;
            My_Requires      : Nullable_Mutable_Requires_T;
            My_Parent        : Nullable_Mutable_Parent_T;
            My_Returned_Only : Fs.Returned_Only_T := Fs.Returned_Only_T(False);
            My_Comment       : Nullable_Mutable_Comment_T;
            My_Children      : Children_P.T (5_000);
         end record;

      function Name (This : T) return Fs.Name_T is (if This.My_Name.Exists then
                                                       (Exists => True, Value => Fs.Name.T (This.My_Name.Value))
                                                    else
                                                      (Exists => False));

      function Category (This : T) return Fs.Category.T is (Fs.Category.T (This.My_Category));

      function Requires (This : T) return Fs.Nullable_Requires_T is (if This.My_Requires.Exists then
                                                                       (Exists => True, Value => Fs.Requires.T (This.My_Requires.Value))
                                                                     else
                                                                       (Exists => False));

      function Parent (This : T) return Fs.Nullable_Parent_T is (if This.My_Parent.Exists then
                                                                               (Exists => True, Value => Fs.Parent.T (This.My_Parent.Value))
                                                                             else
                                                                               (Exists => False));

      function Returned_Only (This : T) return Fs.Returned_Only_T is (This.My_Returned_Only);

      function Comment (This : T) return Fs.Nullable_Comment_T is (if This.My_Comment.Exists then
                                                                     (Exists => True, Value => Fs.Comment.T (This.My_Comment.Value))
                                                                   else
                                                                     (Exists => False));

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

      package Mutable_Unbounded_String is new Aida.Strings.Generic_Mutable_Unbounded_String (Capacity => 1000);

      use all type Mutable_Unbounded_String.T;

      use all type Children_P.T;

      use all type Name_P.Mutable_T;

      use all type Category_P.Mutable_T;

   end Type_T;

   package Type_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Name (This : T) return Type_T.Fs.Name_T with
        Global => null;

      function Category (This : T) return Type_T.Fs.Category.T with
        Global => null;

      function Requires (This : T) return Type_T.Fs.Nullable_Requires_T with
        Global => null;

      function Parent (This : T) return Type_T.Fs.Nullable_Parent_T with
        Global => null;

      function Comment (This : T) return Type_T.Fs.Nullable_Comment_T with
        Global => null;

      function Children (This : T) return Type_T.Fs.Child_Vectors.Immutable_T with
        Global => null;

      function To_String (This : T) return String with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Category (This : in out T;
                              Text : String) with
        Global => null;

      procedure Set_Requires (This : in out T;
                              Text : String);

      procedure Set_Parent (This : in out T;
                            Text : String) with
        Global => null;

      procedure Set_Returned_Only (This  : in out T;
                                   Value : Boolean) with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Type_T.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Type_T.T;

      type Type_Ptr is access Type_T.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Type_T.T,
                                                             P => Type_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Type_T.T);
         end record;

      function Name (This : T) return Type_T.Fs.Name_T is (Name (Smart_Pointers.Value (This.SP).all));

      function Category (This : T) return Type_T.Fs.Category.T is (Category (Smart_Pointers.Value (This.SP).all));

      function Requires (This : T) return Type_T.Fs.Nullable_Requires_T is (Requires (Smart_Pointers.Value (This.SP).all));

      function Parent (This : T) return Type_T.Fs.Nullable_Parent_T is (Parent (Smart_Pointers.Value (This.SP).all));

      function Comment (This : T) return Type_T.Fs.Nullable_Comment_T is (Comment (Smart_Pointers.Value (This.SP).all));

      function Children (This : T) return Type_T.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

      function To_String (This : T) return String is (To_String (Smart_Pointers.Value (This.SP).all));

      pragma Inline (Name);
      pragma Inline (Category);
      pragma Inline (Children);

   end Type_Shared_Ptr;

   package Types with SPARK_Mode is

      package Fs is
         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Type,
                                  Child_Out_Commented_Message
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy             => Dummy_V                 : aliased XML_Dummy_T;
               when Child_Type                  => Type_V                  : aliased Type_Shared_Ptr.T;
               when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);
      end Fs;

      type T is limited private;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Children_P is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Children : Children_P.T (100);
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Types;

   package Types_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Types.Fs.Child_Vectors.Immutable_T;

      procedure Append_Child (This  : in out T;
                              Child : Types.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Types.T;

      type Types_Ptr is access Types.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Types.T,
                                                             P => Types_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Types.T);
         end record;

      function Children (This : T) return Types.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

      pragma Inline (Children);

   end Types_Shared_Ptr;

   package Vendor_Id with SPARK_Mode is

      package Fs is

         package Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         package Id is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         package Comment is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name.T with
        Global => null;

      function Id (This : T) return Fs.Id.T with
        Global => null;

      function Comment (This : T) return Fs.Comment.T with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Id (This : in out T;
                        Text : String) with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

   private

      package Name_P is new Fs.Name.Mutable;

      package Id_P is new Fs.Id.Mutable;

      package Comment_P is new Fs.Comment.Mutable;

      type T is limited
         record
            My_Name    : Name_P.Mutable_T;
            My_Id      : Id_P.Mutable_T;
            My_Comment : Comment_P.Mutable_T;
         end record;

      function Name (This : T) return Fs.Name.T is (Fs.Name.T (This.My_Name));

      function Id (This : T) return Fs.Id.T is (Fs.Id.T (This.My_Id));

      function Comment (This : T) return Fs.Comment.T is (Fs.Comment.T (This.My_Comment));

   end Vendor_Id;

   package Vendor_Id_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Name (This : T) return Vk_XML.Vendor_Id.Fs.Name.T with
        Global => null;

      function Id (This : T) return Vk_XML.Vendor_Id.Fs.Id.T with
        Global => null;

      function Comment (This : T) return Vk_XML.Vendor_Id.Fs.Comment.T with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Id (This : in out T;
                        Text : String) with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Vk_XML.Vendor_Id.T;

      type Vendor_Id_Ptr is access Vendor_Id.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Vendor_Id.T,
                                                             P => Vendor_Id_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Vendor_Id.T);
         end record;

      function Name (This : T) return Vk_XML.Vendor_Id.Fs.Name.T is (Name (Smart_Pointers.Value (This.SP).all));

      function Id (This : T) return Vk_XML.Vendor_Id.Fs.Id.T is (Id (Smart_Pointers.Value (This.SP).all));

      function Comment (This : T) return Vk_XML.Vendor_Id.Fs.Comment.T is (Comment (Smart_Pointers.Value (This.SP).all));

      pragma Inline (Name);
      pragma Inline (Id);
      pragma Inline (Comment);

   end Vendor_Id_Shared_Ptr;

   package Vendor_Ids is

      package Fs is
         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Vendor_Id
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy  => XML_Dummy_V : aliased XML_Dummy_T;
               when Child_Vendor_Id  => Vendor_Id_V : aliased Vk_XML.Vendor_Id_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);
      end Fs;

      type T is private;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Mutable_Child_Vector is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is
         record
            My_Children : Mutable_Child_Vector.T (10);
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Vendor_Ids;

   package Vendor_Ids_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Vendor_Ids.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Vendor_Ids.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Vk_XML.Vendor_Ids.T;

      type Vendor_Ids_Ptr is access Vendor_Ids.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Vendor_Ids.T,
                                                             P => Vendor_Ids_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Vendor_Ids.T);
         end record;

      function Children (This : T) return Vk_XML.Vendor_Ids.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

   end Vendor_Ids_Shared_Ptr;

   package Unused with SPARK_Mode is

      package Fs is

         package Start is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Start_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Start.T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Start (This : T) return Fs.Nullable_Start_T with
        Global => null;

      procedure Set_Start (This : in out T;
                           Text : String) with
        Global => null;

   private

      package Mutable_Start is new Fs.Start.Mutable;

      use all type Mutable_Start.Mutable_T;

      type Nullable_Mutable_Start_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Start.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_Start : Nullable_Mutable_Start_T;
         end record;

      function Start (This : T) return Fs.Nullable_Start_T is (if This.My_Start.Exists then
                                                                 (Exists => True, Value => Fs.Start.T (This.My_Start.Value))
                                                               else
                                                                 (Exists => False));

   end Unused;

   package Unused_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Start (This : T) return Unused.Fs.Nullable_Start_T with
        Global => null;

      procedure Set_Start (This : in out T;
                           Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Unused.T;

      type Unused_Ptr is access Unused.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Unused.T,
                                                             P => Unused_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Unused.T);
         end record;

      function Start (This : T) return Unused.Fs.Nullable_Start_T is (Start (Smart_Pointers.Value (This.SP).all));

   end Unused_Shared_Ptr;

   package Enums with SPARK_Mode is

      package Fs is

         package Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Name_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Name.T;
               when False => null;
               end case;
            end record;

         package Comment is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Comment_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Comment.T;
               when False => null;
               end case;
            end record;

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Enums_Enum,
                                  Child_Out_Commented_Message,
                                  Child_Unused
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy             => XML_Dummy_V             : aliased XML_Dummy_T;
               when Child_Enums_Enum            => Enums_Enum_V            : aliased Vk_XML.Enums_Enum_Shared_Ptr.T;
               when Child_Out_Commented_Message => Out_Commented_Message_V : aliased Vk_XML.XML_Out_Commented_Message_Shared_Ptr.T;
               when Child_Unused                => Unused_V                : aliased Vk_XML.Unused_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

         type Type_Attribue_T is (
                                  Enum,
                                  Bit_Mask
                                  );

         type Nullable_Type_Attribue_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Type_Attribue_T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Name (This : T) return Fs.Nullable_Name_T with
        Global => null;

      function Comment (This : T) return Fs.Nullable_Comment_T with
        Global => null;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Type_Attribue (This : T) return Fs.Nullable_Type_Attribue_T with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

      procedure Set_Type_Attribue (This  : in out T;
                                   Value : Fs.Type_Attribue_T) with
        Global => null;

   private

      package Mutable_Name is new Fs.Name.Mutable;

      use all type Mutable_Name.Mutable_T;

      type Nullable_Mutable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Name.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Comment is new Fs.Comment.Mutable;

      use all type Mutable_Comment.Mutable_T;

      type Nullable_Mutable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Comment.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Name          : Nullable_Mutable_Name_T;
            My_Comment       : Nullable_Mutable_Comment_T;
            My_Children      : Mutable_Children.T (10);
            My_Type_Attribue : Fs.Nullable_Type_Attribue_T;
         end record;

      function Name (This : T) return Fs.Nullable_Name_T is (if This.My_Name.Exists then
                                                               (Exists => True, Value => Fs.Name.T (This.My_Name.Value))
                                                             else
                                                               (Exists => False));

      function Comment (This : T) return Fs.Nullable_Comment_T is (if This.My_Comment.Exists then
                                                                     (Exists => True, Value => Fs.Comment.T (This.My_Comment.Value))
                                                                   else
                                                                     (Exists => False));

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

      function Type_Attribue (This : T) return Fs.Nullable_Type_Attribue_T is (This.My_Type_Attribue);

   end Enums;

   package Enums_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Name (This : T) return Enums.Fs.Nullable_Name_T with
        Global => null;

      function Comment (This : T) return Enums.Fs.Nullable_Comment_T with
        Global => null;

      function Children (This : T) return Enums.Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Type_Attribue (This : T) return Enums.Fs.Nullable_Type_Attribue_T with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Enums.Fs.Child_T) with
        Global => null;

      procedure Set_Type_Attribue (This  : in out T;
                                   Value : Enums.Fs.Type_Attribue_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Enums.T;

      type Enums_Ptr is access Enums.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Enums.T,
                                                             P => Enums_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Enums.T);
         end record;

      function Name (This : T) return Enums.Fs.Nullable_Name_T is (Name (Smart_Pointers.Value (This.SP).all));

      function Comment (This : T) return Enums.Fs.Nullable_Comment_T is (Comment (Smart_Pointers.Value (This.SP).all));

      function Children (This : T) return Enums.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

      function Type_Attribue (This : T) return Enums.Fs.Nullable_Type_Attribue_T is (Type_Attribue (Smart_Pointers.Value (This.SP).all));

   end Enums_Shared_Ptr;

   package Proto with SPARK_Mode is

      package Fs is

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Nested_Type,
                                  Child_Name
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy   => XML_Dummy_V   : aliased XML_Dummy_T;
               when Child_Nested_Type => Nested_Type_V : aliased Vk_XML.Nested_Type_Shared_Ptr.T;
               when Child_Name        => Name_V        : aliased Vk_XML.Name_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Children : Mutable_Children.T (10);
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Proto;

   package Proto_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Proto.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Proto.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Proto.T;

      type Proto_Ptr is access Proto.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Proto.T,
                                                             P => Proto_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Proto.T);
         end record;

      function Children (This : T) return Proto.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

   end Proto_Shared_Ptr;

   package Param with SPARK_Mode is

      package Fs is

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Nested_Type,
                                  Child_XML_Text,
                                  Child_Name
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy   => XML_Dummy_V   : aliased XML_Dummy_T;
               when Child_Nested_Type => Nested_Type_V : aliased Vk_XML.Nested_Type_Shared_Ptr.T;
               when Child_XML_Text    => XML_Text_V    : aliased Vk_XML.XML_Text.T;
               when Child_Name        => Name_V        : aliased Vk_XML.Name_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

         type Optional_T is new Boolean;

         package External_Sync is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_External_Sync_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : External_Sync.T;
               when False => null;
               end case;
            end record;

         package Len is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Len_T (Exists : Boolean := False) is
            record
               case Exists is
                  when True  => Value : Len.T;
                  when False => null;
               end case;
            end record;

         type No_Auto_Validity_T is new Boolean;

         type Nullable_No_Auto_Validity_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : No_Auto_Validity_T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Optional (This : T) return Fs.Optional_T with
        Global => null;

      function External_Sync (This : T) return Fs.Nullable_External_Sync_T with
        Global => null;

      function Len (This : T) return Fs.Nullable_Len_T with
        Global => null;

      function No_Auto_Validity (This : T) return Fs.Nullable_No_Auto_Validity_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) with
        Global => null;

      procedure Set_External_Sync (This : in out T;
                                   Text : String) with
        Global => null;

      procedure Set_Len (This : in out T;
                         Text : String) with
        Global => null;

      procedure Set_No_Auto_Validity (This  : in out T;
                                      Value : Boolean) with
        Global => null;

   private

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      package Mutable_External_Sync is new Fs.External_Sync.Mutable;

      use all type Mutable_External_Sync.Mutable_T;

      type Nullable_Mutable_External_Sync_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_External_Sync.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Len is new Fs.Len.Mutable;

      use all type Mutable_Len.Mutable_T;

      type Nullable_Mutable_Len_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Len.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_Children         : Mutable_Children.T (10);
            My_Optional         : Fs.Optional_T;
            My_External_Sync    : Nullable_Mutable_External_Sync_T;
            My_Len              : Nullable_Mutable_Len_T;
            My_No_Auto_Validity : Fs.Nullable_No_Auto_Validity_T;
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

      function Optional (This : T) return Fs.Optional_T is (This.My_Optional);

      function External_Sync (This : T) return Fs.Nullable_External_Sync_T is (if This.My_External_Sync.Exists then
                                                                                 (Exists => True, Value => Fs.External_Sync.T (This.My_External_Sync.Value))
                                                                               else
                                                                                 (Exists => False));

      function Len (This : T) return Fs.Nullable_Len_T is (if This.My_Len.Exists then
                                                             (Exists => True, Value => Fs.Len.T (This.My_Len.Value))
                                                           else
                                                             (Exists => False));

      function No_Auto_Validity (This : T) return Fs.Nullable_No_Auto_Validity_T is (This.My_No_Auto_Validity);

   end Param;

   package Param_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Param.Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Optional (This : T) return Param.Fs.Optional_T with
        Global => null;

      function External_Sync (This : T) return Param.Fs.Nullable_External_Sync_T with
        Global => null;

      function Len (This : T) return Param.Fs.Nullable_Len_T with
        Global => null;

      function No_Auto_Validity (This : T) return Param.Fs.Nullable_No_Auto_Validity_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Param.Fs.Child_T) with
        Global => null;

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) with
        Global => null;

      procedure Set_External_Sync (This : in out T;
                                   Text : String) with
        Global => null;

      procedure Set_Len (This : in out T;
                         Text : String) with
        Global => null;

      procedure Set_No_Auto_Validity (This  : in out T;
                                      Value : Boolean) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Param.T;

      type Param_Ptr is access Param.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Param.T,
                                                             P => Param_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Param.T);
         end record;

      function Children (This : T) return Param.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

      function Optional (This : T) return Param.Fs.Optional_T is (Optional (Smart_Pointers.Value (This.SP).all));

      function External_Sync (This : T) return Param.Fs.Nullable_External_Sync_T is (External_Sync (Smart_Pointers.Value (This.SP).all));

      function Len (This : T) return Param.Fs.Nullable_Len_T is (Len (Smart_Pointers.Value (This.SP).all));

      function No_Auto_Validity (This : T) return Param.Fs.Nullable_No_Auto_Validity_T is (No_Auto_Validity (Smart_Pointers.Value (This.SP).all));

   end Param_Shared_Ptr;

   package External_Sync_Parameter with SPARK_Mode is

      package Fs is

         package XML_Value is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_XML_Value_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : XML_Value.T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function XML_Value (This : T) return Fs.Nullable_XML_Value_T with
        Global => null;

      procedure Set_XML_Value (This : in out T;
                               Text : String) with
        Global => null;

   private

      package Mutable_XML_Value is new Fs.XML_Value.Mutable;

      use all type Mutable_XML_Value.Mutable_T;

      type Nullable_Mutable_XML_Value_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_XML_Value.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_XML_Value : Nullable_Mutable_XML_Value_T;
         end record;

         function XML_Value (This : T) return Fs.Nullable_XML_Value_T is (if This.My_XML_Value.Exists then
                                                                                  (Exists => True, Value => Fs.XML_Value.T (This.My_XML_Value.Value))
                                                                                else
                                                                                  (Exists => False));

   end External_Sync_Parameter;

   package External_Sync_Parameter_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function XML_Value (This : T) return External_Sync_Parameter.Fs.Nullable_XML_Value_T with
        Global => null;

      procedure Set_XML_Value (This : in out T;
                               Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type External_Sync_Parameter.T;

      type External_Sync_Parameter_Ptr is access External_Sync_Parameter.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => External_Sync_Parameter.T,
                                                             P => External_Sync_Parameter_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new External_Sync_Parameter.T);
         end record;

      function XML_Value (This : T) return External_Sync_Parameter.Fs.Nullable_XML_Value_T is (XML_Value (Smart_Pointers.Value (This.SP).all));

   end External_Sync_Parameter_Shared_Ptr;

   package Implicit_External_Sync_Parameters with SPARK_Mode is

      package Fs is

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_External_Sync_Parameter
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy               => XML_Dummy_V               : aliased XML_Dummy_T;
               when Child_External_Sync_Parameter => External_Sync_Parameter_V : aliased Vk_XML.External_Sync_Parameter_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Children : Mutable_Children.T (10);
         end record;

         function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Implicit_External_Sync_Parameters;

   package Implicit_External_Sync_Parameters_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Implicit_External_Sync_Parameters.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Implicit_External_Sync_Parameters.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Implicit_External_Sync_Parameters.T;

      type Implicit_External_Sync_Parameters_Ptr is access Implicit_External_Sync_Parameters.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Implicit_External_Sync_Parameters.T,
                                                             P => Implicit_External_Sync_Parameters_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Implicit_External_Sync_Parameters.T);
         end record;

      function Children (This : T) return Implicit_External_Sync_Parameters.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

   end Implicit_External_Sync_Parameters_Shared_Ptr;

   package Command with SPARK_Mode is

      package Fs is

         package Success_Code is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         package Success_Code_Vector is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                      Element_Type => Success_Code.T,
                                                                                      "="          => Success_Code."=",
                                                                                      Bounded      => False);

         package Error_Code is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         package Error_Code_Vector is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                    Element_Type => Error_Code.T,
                                                                                    "="          => Error_Code."=",
                                                                                    Bounded      => False);

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Proto,
                                  Child_Param,
                                  Child_Validity,
                                  Child_Implicit_External_Sync_Parameters
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy                         => XML_Dummy_V  : aliased XML_Dummy_T;
               when Child_Proto                             => Proto_V      : aliased Vk_XML.Proto_Shared_Ptr.T;
               when Child_Param                             => Param_V      : aliased Vk_XML.Param_Shared_Ptr.T;
               when Child_Validity                          => Validity_V   : aliased Vk_XML.Validity_Shared_Ptr.T;
               when Child_Implicit_External_Sync_Parameters => Parameters_V : aliased Vk_XML.Implicit_External_Sync_Parameters_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

         type Queue_T is (
                          Sparse_Binding,
                          Graphics,
                          Compute,
                          Transfer
                         );

         package Queue_Vector is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                               Element_Type => Queue_T,
                                                                               "="          => "=",
                                                                               Bounded      => False);

         type Render_Pass_T is (
                                Inside,
                                Outside,
                                Both
                               );

         package Render_Pass_Vector is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                     Element_Type => Render_Pass_T,
                                                                                     "="          => "=",
                                                                                     Bounded      => False);

         type Command_Buffer_Level_T is (
                                         Primary,
                                         Secondary
                                        );

         package Command_Buffer_Level_Vector is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                      Element_Type => Command_Buffer_Level_T,
                                                                                      "="          => "=",
                                                                                      Bounded      => False);

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Success_Codes (This : T) return Fs.Success_Code_Vector.Immutable_T with
        Global => null;

      function Error_Codes (This : T) return Fs.Error_Code_Vector.Immutable_T with
        Global => null;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Queues (This : T) return Fs.Queue_Vector.Immutable_T with
        Global => null;

      function Render_Passes (This : T) return Fs.Render_Pass_Vector.Immutable_T with
        Global => null;

      function Command_Buffer_Levels (This : T) return Fs.Command_Buffer_Level_Vector.Immutable_T with
        Global => null;

      function To_String (This : T) return String with
        Global => null;

      procedure Append_Success_Code (This : in out T;
                                     Text : String) with
        Global => null;

      procedure Append_Error_Code (This : in out T;
                                   Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

      procedure Append_Queue (This  : in out T;
                              Queue : Fs.Queue_T) with
        Global => null;

      procedure Append_Render_Pass (This  : in out T;
                                    Value : Fs.Render_Pass_T) with
        Global => null;

      procedure Append_Command_Buffer_Level (This  : in out T;
                                             Value : Fs.Command_Buffer_Level_T) with
        Global => null;

   private

      package Mutable_Success_Code_Vector is new Fs.Success_Code_Vector.Generic_Mutable_Vector;

      use all type Mutable_Success_Code_Vector.T;

      package Mutable_Success_Code is new Fs.Success_Code.Mutable;

      use all type Mutable_Success_Code.Mutable_T;

      package Mutable_Error_Code_Vector is new Fs.Error_Code_Vector.Generic_Mutable_Vector;

      use all type Mutable_Error_Code_Vector.T;

      package Mutable_Error_Code is new Fs.Error_Code.Mutable;

      use all type Mutable_Error_Code.Mutable_T;

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      package Mutable_Queue_Vector is new Fs.Queue_Vector.Generic_Mutable_Vector;

      use all type Mutable_Queue_Vector.T;

      package Mutable_Render_Pass_Vector is new Fs.Render_Pass_Vector.Generic_Mutable_Vector;

      use all type Mutable_Render_Pass_Vector.T;

      package Mutable_Command_Buffer_Level_Vector is new Fs.Command_Buffer_Level_Vector.Generic_Mutable_Vector;

      use all type Mutable_Command_Buffer_Level_Vector.T;

      type T is limited
         record
            My_Success_Codes         : Mutable_Success_Code_Vector.T (10);
            My_Error_Codes           : Mutable_Error_Code_Vector.T (10);
            My_Children              : Mutable_Children.T (10);
            My_Queues                : Mutable_Queue_Vector.T (5);
            My_Render_Passes         : Mutable_Render_Pass_Vector.T (5);
            My_Command_Buffer_Levels : Mutable_Command_Buffer_Level_Vector.T (10);
         end record;

      function Success_Codes (This : T) return Fs.Success_Code_Vector.Immutable_T is (Fs.Success_Code_Vector.Immutable_T (This.My_Success_Codes));

      function Error_Codes (This : T) return Fs.Error_Code_Vector.Immutable_T is (Fs.Error_Code_Vector.Immutable_T (This.My_Error_Codes));

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

      function Queues (This : T) return Fs.Queue_Vector.Immutable_T is (Fs.Queue_Vector.Immutable_T (This.My_Queues));

      function Render_Passes (This : T) return Fs.Render_Pass_Vector.Immutable_T is (Fs.Render_Pass_Vector.Immutable_T (This.My_Render_Passes));

      function Command_Buffer_Levels (This : T) return Fs.Command_Buffer_Level_Vector.Immutable_T is (Fs.Command_Buffer_Level_Vector.Immutable_T (This.My_Command_Buffer_Levels));

   end Command;

   package Command_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Success_Codes (This : T) return Command.Fs.Success_Code_Vector.Immutable_T with
        Global => null;

      function Error_Codes (This : T) return Command.Fs.Error_Code_Vector.Immutable_T with
        Global => null;

      function Children (This : T) return Command.Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Queues (This : T) return Command.Fs.Queue_Vector.Immutable_T with
        Global => null;

      function Render_Passes (This : T) return Command.Fs.Render_Pass_Vector.Immutable_T with
        Global => null;

      function Command_Buffer_Levels (This : T) return Command.Fs.Command_Buffer_Level_Vector.Immutable_T with
        Global => null;

      function To_String (This : T) return String with
        Global => null;

      procedure Append_Success_Code (This : in out T;
                                     Text : String) with
        Global => null;

      procedure Append_Error_Code (This : in out T;
                                   Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Command.Fs.Child_T) with
        Global => null;

      procedure Append_Queue (This  : in out T;
                              Queue : Command.Fs.Queue_T) with
        Global => null;

      procedure Append_Render_Pass (This  : in out T;
                                    Value : Command.Fs.Render_Pass_T) with
        Global => null;

      procedure Append_Command_Buffer_Level (This  : in out T;
                                             Value : Command.Fs.Command_Buffer_Level_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Command.T;

      type Command_Ptr is access Command.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Command.T,
                                                             P => Command_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Command.T);
         end record;

      function Success_Codes (This : T) return Command.Fs.Success_Code_Vector.Immutable_T is (Success_Codes (Smart_Pointers.Value (This.SP).all));

      function Error_Codes (This : T) return Command.Fs.Error_Code_Vector.Immutable_T is (Error_Codes (Smart_Pointers.Value (This.SP).all));

      function Children (This : T) return Command.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

      function Queues (This : T) return Command.Fs.Queue_Vector.Immutable_T is (Queues (Smart_Pointers.Value (This.SP).all));

      function Render_Passes (This : T) return Command.Fs.Render_Pass_Vector.Immutable_T is (Render_Passes (Smart_Pointers.Value (This.SP).all));

      function Command_Buffer_Levels (This : T) return Command.Fs.Command_Buffer_Level_Vector.Immutable_T is (Command_Buffer_Levels (Smart_Pointers.Value (This.SP).all));

   end Command_Shared_Ptr;

   package Commands with SPARK_Mode is

      package Fs is

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Command
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy      => XML_Dummy_V      : aliased XML_Dummy_T;
               when Child_Command        => Command_V        : aliased Vk_XML.Command_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Children : Mutable_Children.T (10);
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Commands;

   package Commands_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Commands.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Commands.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Commands.T;

      type Commands_Ptr is access Commands.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Commands.T,
                                                             P => Commands_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Commands.T);
         end record;

      function Children (This : T) return Commands.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

   end Commands_Shared_Ptr;

   package Require_Enum with SPARK_Mode is

      package Fs is

         package Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Name_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Name.T;
               when False => null;
               end case;
            end record;

         package Value is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Value_T (Exists : Boolean := False) is
            record
               case Exists is
                  when True  => Value_V : Value.T;
                  when False => null;
               end case;
            end record;

         type Offset_T is new Natural range 0..10;

         type Nullable_Offset_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Offset_T;
               when False => null;
               end case;
            end record;

         package Dir is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Dir_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Dir.T;
               when False => null;
               end case;
            end record;

         package Extends is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Extends_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Extends.T;
               when False => null;
               end case;
            end record;

         package Comment is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Comment_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Comment.T;
               when False => null;
               end case;
            end record;

         type Bit_Position_T is new Natural range 0..20;

         type Nullable_Bit_Position_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Bit_Position_T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Name (This : T) return Fs.Nullable_Name_T with
        Global => null;

      function Value (This : T) return Fs.Nullable_Value_T with
        Global => null;

      function Offset (This : T) return Fs.Nullable_Offset_T with
        Global => null;

      function Dir (This : T) return Fs.Nullable_Dir_T with
        Global => null;

      function Extends (This : T) return Fs.Nullable_Extends_T with
        Global => null;

      function Comment (This : T) return Fs.Nullable_Comment_T with
        Global => null;

      function Bit_Position (This : T) return Fs.Nullable_Bit_Position_T with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String) with
        Global => null;

      procedure Set_Offset (This  : in out T;
                            Value : Fs.Offset_T) with
        Global => null;

      procedure Set_Dir (This : in out T;
                         Text : String) with
        Global => null;

      procedure Set_Extends (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Bit_Position (This  : in out T;
                                  Value : Fs.Bit_Position_T) with
        Global => null;

   private

      package Mutable_Name is new Fs.Name.Mutable;

      use all type Mutable_Name.Mutable_T;

      type Nullable_Mutable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Name.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Value is new Fs.Value.Mutable;

      use all type Mutable_Value.Mutable_T;

      type Nullable_Mutable_Value_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Value.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Dir is new Fs.Dir.Mutable;

      use all type Mutable_Dir.Mutable_T;

      type Nullable_Mutable_Dir_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Dir.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Extends is new Fs.Extends.Mutable;

      use all type Mutable_Extends.Mutable_T;

      type Nullable_Mutable_Extends_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Extends.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Comment is new Fs.Comment.Mutable;

      use all type Mutable_Comment.Mutable_T;

      type Nullable_Mutable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Comment.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_Name         : Nullable_Mutable_Name_T;
            My_Value        : Nullable_Mutable_Value_T;
            My_Offset       : Fs.Nullable_Offset_T;
            My_Dir          : Nullable_Mutable_Dir_T;
            My_Extends      : Nullable_Mutable_Extends_T;
            My_Comment      : Nullable_Mutable_Comment_T;
            My_Bit_Position : Fs.Nullable_Bit_Position_T;
         end record;

      function Name (This : T) return Fs.Nullable_Name_T is (if This.My_Name.Exists then
                                                               (Exists => True, Value => Fs.Name.T (This.My_Name.Value))
                                                             else
                                                               (Exists => False));

      function Value (This : T) return Fs.Nullable_Value_T is (if This.My_Value.Exists then
                                                                 (Exists => True, Value_V => Fs.Value.T (This.My_Value.Value))
                                                               else
                                                                 (Exists => False));

      function Offset (This : T) return Fs.Nullable_Offset_T is (This.My_Offset);

      function Dir (This : T) return Fs.Nullable_Dir_T is (if This.My_Dir.Exists then
                                                             (Exists => True, Value => Fs.Dir.T (This.My_Dir.Value))
                                                           else
                                                             (Exists => False));

      function Extends (This : T) return Fs.Nullable_Extends_T is (if This.My_Extends.Exists then
                                                                     (Exists => True, Value => Fs.Extends.T (This.My_Extends.Value))
                                                                   else
                                                                     (Exists => False));

      function Comment (This : T) return Fs.Nullable_Comment_T is (if This.My_Comment.Exists then
                                                                     (Exists => True, Value => Fs.Comment.T (This.My_Comment.Value))
                                                                   else
                                                                     (Exists => False));

      function Bit_Position (This : T) return Fs.Nullable_Bit_Position_T is (This.My_Bit_Position);

   end Require_Enum;

   package Require_Enum_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Name (This : T) return Require_Enum.Fs.Nullable_Name_T with
        Global => null;

      function Value (This : T) return Require_Enum.Fs.Nullable_Value_T with
        Global => null;

      function Offset (This : T) return Require_Enum.Fs.Nullable_Offset_T with
        Global => null;

      function Dir (This : T) return Require_Enum.Fs.Nullable_Dir_T with
        Global => null;

      function Extends (This : T) return Require_Enum.Fs.Nullable_Extends_T with
        Global => null;

      function Comment (This : T) return Require_Enum.Fs.Nullable_Comment_T with
        Global => null;

      function Bit_Position (This : T) return Require_Enum.Fs.Nullable_Bit_Position_T with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Value (This : in out T;
                           Text : String) with
        Global => null;

      procedure Set_Offset (This  : in out T;
                            Value : Require_Enum.Fs.Offset_T) with
        Global => null;

      procedure Set_Dir (This : in out T;
                         Text : String) with
        Global => null;

      procedure Set_Extends (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Bit_Position (This  : in out T;
                                  Value : Require_Enum.Fs.Bit_Position_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Require_Enum.T;

      type Require_Enum_Ptr is access Require_Enum.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Require_Enum.T,
                                                             P => Require_Enum_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Require_Enum.T);
         end record;

      function Name (This : T) return Require_Enum.Fs.Nullable_Name_T is (Name (Smart_Pointers.Value (This.SP).all));

      function Value (This : T) return Require_Enum.Fs.Nullable_Value_T is (Value (Smart_Pointers.Value (This.SP).all));

      function Offset (This : T) return Require_Enum.Fs.Nullable_Offset_T is (Offset (Smart_Pointers.Value (This.SP).all));

      function Dir (This : T) return Require_Enum.Fs.Nullable_Dir_T is (Dir (Smart_Pointers.Value (This.SP).all));

      function Extends (This : T) return Require_Enum.Fs.Nullable_Extends_T is (Extends (Smart_Pointers.Value (This.SP).all));

      function Comment (This : T) return Require_Enum.Fs.Nullable_Comment_T is (Comment (Smart_Pointers.Value (This.SP).all));

      function Bit_Position (This : T) return Require_Enum.Fs.Nullable_Bit_Position_T is (Bit_Position (Smart_Pointers.Value (This.SP).all));

   end Require_Enum_Shared_Ptr;

   package Require_Command with SPARK_Mode is

      package Fs is

         package Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Name_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Name.T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Name (This : T) return Fs.Nullable_Name_T with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

   private

      package Mutable_Name is new Fs.Name.Mutable;

      use all type Mutable_Name.Mutable_T;

      type Nullable_Mutable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Name.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_Name : Nullable_Mutable_Name_T;
         end record;

      function Name (This : T) return Fs.Nullable_Name_T is (if This.My_Name.Exists then
                                                                  (Exists => True, Value => Fs.Name.T (This.My_Name.Value))
                                                                else
                                                                  (Exists => False));

   end Require_Command;

   package Require_Command_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Name (This : T) return Require_Command.Fs.Nullable_Name_T with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Require_Command.T;

      type Require_Command_Ptr is access Require_Command.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Require_Command.T,
                                                             P => Require_Command_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Require_Command.T);
         end record;

      function Name (This : T) return Require_Command.Fs.Nullable_Name_T is (Name (Smart_Pointers.Value (This.SP).all));

   end Require_Command_Shared_Ptr;

   package Require with SPARK_Mode is

      package Fs is

         package Comment is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Comment_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Comment.T;
               when False => null;
               end case;
            end record;

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Type,
                                  Child_Enum,
                                  Child_Command,
                                  Child_Out_Commented_Message,
                                  Child_Usage
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy             => XML_Dummy_V             : aliased XML_Dummy_T;
               when Child_Type                  => Type_V                  : aliased Vk_XML.Type_Shared_Ptr.T;
               when Child_Enum                  => Enum_V                  : aliased Vk_XML.Require_Enum_Shared_Ptr.T;
               when Child_Command               => Command_V               : aliased Vk_XML.Require_Command_Shared_Ptr.T;
               when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Shared_Ptr.T;
               when Child_Usage                 => Usage_V                 : aliased Vk_XML.Usage_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Comment (This : T) return Fs.Nullable_Comment_T with
        Global => null;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Mutable_Comment is new Fs.Comment.Mutable;

      use all type Mutable_Comment.Mutable_T;

      type Nullable_Mutable_Comment_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Comment.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Comment  : Nullable_Mutable_Comment_T;
            My_Children : Mutable_Children.T (10);
         end record;

      function Comment (This : T) return Fs.Nullable_Comment_T is (if This.My_Comment.Exists then
                                                                     (Exists => True, Value => Fs.Comment.T (This.My_Comment.Value))
                                                                   else
                                                                     (Exists => False));

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Require;

   package Require_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Comment (This : T) return Require.Fs.Nullable_Comment_T with
        Global => null;

      function Children (This : T) return Require.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Set_Comment (This : in out T;
                             Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Require.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Require.T;

      type Require_Ptr is access Require.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Require.T,
                                                             P => Require_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Require.T);
         end record;

      function Comment (This : T) return Require.Fs.Nullable_Comment_T is (Comment (Smart_Pointers.Value (This.SP).all));

      function Children (This : T) return Require.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

   end Require_Shared_Ptr;

   package Feature with SPARK_Mode is

      package Fs is

         package API is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_API_T (Exists : Boolean := False) is
            record
               case Exists is
                  when True  => Value : API.T;
                  when False => null;
               end case;
            end record;

         package Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Name_T (Exists : Boolean := False) is
            record
               case Exists is
                  when True  => Value : Name.T;
                  when False => null;
               end case;
            end record;

         package Number is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Number_T (Exists : Boolean := False) is
            record
               case Exists is
                  when True  => Value : Number.T;
                  when False => null;
               end case;
            end record;

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Require
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy => XML_Dummy_V : aliased XML_Dummy_T;
               when Child_Require   => Require_V   : aliased Vk_XML.Require_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function API (This : T) return Fs.Nullable_API_T with
        Global => null;

      function Name (This : T) return Fs.Nullable_Name_T with
        Global => null;

      function Number (This : T) return Fs.Nullable_Number_T with
        Global => null;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Set_API (This : in out T;
                         Text : String) with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Number (This : in out T;
                            Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Mutable_API is new Fs.API.Mutable;

      use all type Mutable_API.Mutable_T;

      type Nullable_Mutable_API_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_API.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Name is new Fs.Name.Mutable;

      use all type Mutable_Name.Mutable_T;

      type Nullable_Mutable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Name.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Number is new Fs.Number.Mutable;

      use all type Mutable_Number.Mutable_T;

      type Nullable_Mutable_Number_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Number.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_API      : Nullable_Mutable_API_T;
            My_Name     : Nullable_Mutable_Name_T;
            My_Number   : Nullable_Mutable_Number_T;
            My_Children : Mutable_Children.T (10);
         end record;

      function API (This : T) return Fs.Nullable_API_T is (if This.My_API.Exists then
                                                             (Exists => True, Value => Fs.API.T (This.My_API.Value))
                                                           else
                                                             (Exists => False));

      function Name (This : T) return Fs.Nullable_Name_T is (if This.My_Name.Exists then
                                                               (Exists => True, Value => Fs.Name.T (This.My_Name.Value))
                                                             else
                                                               (Exists => False));

      function Number (This : T) return Fs.Nullable_Number_T is (if This.My_Number.Exists then
                                                                   (Exists => True, Value => Fs.Number.T (This.My_Number.Value))
                                                                 else
                                                                   (Exists => False));

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Feature;

   package Feature_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function API (This : T) return Feature.Fs.Nullable_API_T with
        Global => null;

      function Name (This : T) return Feature.Fs.Nullable_Name_T with
        Global => null;

      function Number (This : T) return Feature.Fs.Nullable_Number_T with
        Global => null;

      function Children (This : T) return Feature.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Set_API (This : in out T;
                         Text : String) with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Number (This : in out T;
                            Text : String) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Feature.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Feature.T;

      type Feature_Ptr is access Feature.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Feature.T,
                                                             P => Feature_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Feature.T);
         end record;

      function API (This : T) return Feature.Fs.Nullable_API_T is (API (Smart_Pointers.Value (This.SP).all));

      function Name (This : T) return Feature.Fs.Nullable_Name_T is (Name (Smart_Pointers.Value (This.SP).all));

      function Number (This : T) return Feature.Fs.Nullable_Number_T is (Number (Smart_Pointers.Value (This.SP).all));

      function Children (This : T) return Feature.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

   end Feature_Shared_Ptr;

   package Extension with SPARK_Mode is

      package Fs is

         package Name is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Name_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Name.T;
               when False => null;
               end case;
            end record;

         type Number_T is new Positive range 1..30;

         type Nullable_Number_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Number_T;
               when False => null;
               end case;
            end record;

         type Supported_T is (
                              Vulkan,
                              Disabled
                             );

         type Nullable_Supported_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Supported_T;
               when False => null;
               end case;
            end record;

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Require
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy => XML_Dummy_V : aliased XML_Dummy_T;
               when Child_Require   => Require_V   : aliased Vk_XML.Require_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

         package Protect is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Protect_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Protect.T;
               when False => null;
               end case;
            end record;

         package Author is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Author_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Author.T;
               when False => null;
               end case;
            end record;

         package Contact is new Aida.Strings.Generic_Immutable_Unbounded_String_Shared_Ptr (100);

         type Nullable_Contact_T (Exists : Boolean := False) is
            record
               case Exists is
               when True  => Value : Contact.T;
               when False => null;
               end case;
            end record;

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Name (This : T) return Fs.Nullable_Name_T with
        Global => null;

      function Number (This : T) return Fs.Nullable_Number_T with
        Global => null;

      function Supported (This : T) return Fs.Nullable_Supported_T with
        Global => null;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Protect (This : T) return Fs.Nullable_Protect_T with
        Global => null;

      function Author (This : T) return Fs.Nullable_Author_T with
        Global => null;

      function Contact (This : T) return Fs.Nullable_Contact_T with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Number (This  : in out T;
                            Value : Fs.Number_T) with
        Global => null;

      procedure Set_Supported (This  : in out T;
                               Value : Fs.Supported_T) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

      procedure Set_Protect (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Author (This : in out T;
                            Text : String) with
        Global => null;

      procedure Set_Contact (This : in out T;
                             Text : String) with
        Global => null;

   private

      package Mutable_Name is new Fs.Name.Mutable;

      use all type Mutable_Name.Mutable_T;

      type Nullable_Mutable_Name_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Name.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      package Mutable_Protect is new Fs.Protect.Mutable;

      use all type Mutable_Protect.Mutable_T;

      type Nullable_Mutable_Protect_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Protect.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Author is new Fs.Author.Mutable;

      use all type Mutable_Author.Mutable_T;

      type Nullable_Mutable_Author_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Author.Mutable_T;
               when False => null;
            end case;
         end record;

      package Mutable_Contact is new Fs.Contact.Mutable;

      use all type Mutable_Contact.Mutable_T;

      type Nullable_Mutable_Contact_T (Exists : Boolean := False) is
         record
            case Exists is
               when True  => Value : Mutable_Contact.Mutable_T;
               when False => null;
            end case;
         end record;

      type T is limited
         record
            My_Name      : Nullable_Mutable_Name_T;
            My_Number    : Fs.Nullable_Number_T;
            My_Supported : Fs.Nullable_Supported_T;
            My_Children  : Mutable_Children.T (20);
            My_Protect   : Nullable_Mutable_Protect_T;
            My_Author    : Nullable_Mutable_Author_T;
            My_Contact   : Nullable_Mutable_Contact_T;
         end record;

      function Name (This : T) return Fs.Nullable_Name_T is (if This.My_Name.Exists then
                                                               (Exists => True, Value => Fs.Name.T (This.My_Name.Value))
                                                             else
                                                               (Exists => False));

      function Number (This : T) return Fs.Nullable_Number_T is (This.My_Number);

      function Supported (This : T) return Fs.Nullable_Supported_T is (This.My_Supported);

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

      function Protect (This : T) return Fs.Nullable_Protect_T is (if This.My_Protect.Exists then
                                                                     (Exists => True, Value => Fs.Protect.T (This.My_Protect.Value))
                                                                   else
                                                                     (Exists => False));

      function Author (This : T) return Fs.Nullable_Author_T is (if This.My_Author.Exists then
                                                                   (Exists => True, Value => Fs.Author.T (This.My_Author.Value))
                                                                 else
                                                                   (Exists => False));

      function Contact (This : T) return Fs.Nullable_Contact_T is (if This.My_Contact.Exists then
                                                                     (Exists => True, Value => Fs.Contact.T (This.My_Contact.Value))
                                                                   else
                                                                     (Exists => False));

   end Extension;

   package Extension_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Name (This : T) return Extension.Fs.Nullable_Name_T with
        Global => null;

      function Number (This : T) return Extension.Fs.Nullable_Number_T with
        Global => null;

      function Supported (This : T) return Extension.Fs.Nullable_Supported_T with
        Global => null;

      function Children (This : T) return Extension.Fs.Child_Vectors.Immutable_T with
        Global => null;

      function Protect (This : T) return Extension.Fs.Nullable_Protect_T with
        Global => null;

      function Author (This : T) return Extension.Fs.Nullable_Author_T with
        Global => null;

      function Contact (This : T) return Extension.Fs.Nullable_Contact_T with
        Global => null;

      procedure Set_Name (This : in out T;
                          Text : String) with
        Global => null;

      procedure Set_Number (This  : in out T;
                            Value : Extension.Fs.Number_T) with
        Global => null;

      procedure Set_Supported (This  : in out T;
                               Value : Extension.Fs.Supported_T) with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Extension.Fs.Child_T) with
        Global => null;

      procedure Set_Protect (This : in out T;
                             Text : String) with
        Global => null;

      procedure Set_Author (This : in out T;
                            Text : String) with
        Global => null;

      procedure Set_Contact (This : in out T;
                             Text : String) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Extension.T;

      type Extension_Ptr is access Extension.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Extension.T,
                                                             P => Extension_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Extension.T);
         end record;

      function Name (This : T) return Extension.Fs.Nullable_Name_T is (Name (Smart_Pointers.Value (This.SP).all));

      function Number (This : T) return Extension.Fs.Nullable_Number_T is (Number (Smart_Pointers.Value (This.SP).all));

      function Supported (This : T) return Extension.Fs.Nullable_Supported_T is (Supported (Smart_Pointers.Value (This.SP).all));

      function Children (This : T) return Extension.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

      function Protect (This : T) return Extension.Fs.Nullable_Protect_T is (Protect (Smart_Pointers.Value (This.SP).all));

      function Author (This : T) return Extension.Fs.Nullable_Author_T is (Author (Smart_Pointers.Value (This.SP).all));

      function Contact (This : T) return Extension.Fs.Nullable_Contact_T is (Contact (Smart_Pointers.Value (This.SP).all));

   end Extension_Shared_Ptr;

   package Extensions with SPARK_Mode is

      package Fs is

         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Extension,
                                  Child_Out_Commented_Message
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy             => XML_Dummy_V             : aliased XML_Dummy_T;
               when Child_Extension             => Extension_V             : aliased Vk_XML.Extension_Shared_Ptr.T;
               when Child_Out_Commented_Message => Out_Commented_Message_V : aliased XML_Out_Commented_Message_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);

      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Mutable_Children is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Children : Mutable_Children.T (100);
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Extensions;

   package Extensions_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Extensions.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Extensions.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      use all type Extensions.T;

      type Extensions_Ptr is access Extensions.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Extensions.T,
                                                             P => Extensions_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Extensions.T);
         end record;

      function Children (This : T) return Extensions.Fs.Child_Vectors.Immutable_T is (Children (Smart_Pointers.Value (This.SP).all));

   end Extensions_Shared_Ptr;

   package Registry with SPARK_Mode is

      package Fs is
         type Child_Kind_Id_T is (
                                  Child_XML_Dummy,
                                  Child_Comment,
                                  Child_XML_Text,
                                  Child_Out_Commented_Message,
                                  Child_Vendor_Ids,
                                  Child_Tags,
                                  Child_Types,
                                  Child_Enums,
                                  Child_Commands,
                                  Child_Feature,
                                  Child_Extensions
                                 );

         type Child_T (Kind_Id : Child_Kind_Id_T := Child_XML_Dummy) is record
            case Kind_Id is
               when Child_XML_Dummy             => XML_Dummy_V             : aliased XML_Dummy_T;
               when Child_XML_Text              => XML_Text_V              : aliased Vk_XML.XML_Text.T;
               when Child_Comment               => C                       : aliased Vk_XML.Comment_Shared_Ptr.T;
               when Child_Out_Commented_Message => Out_Commented_Message_V : aliased Vk_XML.XML_Out_Commented_Message_Shared_Ptr.T;
               when Child_Vendor_Ids            => Vendor_Ids_V            : aliased Vk_XML.Vendor_Ids_Shared_Ptr.T;
               when Child_Tags                  => Tags_V                  : aliased Vk_XML.Tags_Shared_Ptr.T;
               when Child_Types                 => Types_V                 : aliased Vk_XML.Types_Shared_Ptr.T;
               when Child_Enums                 => Enums_V                 : aliased Vk_XML.Enums_Shared_Ptr.T;
               when Child_Commands              => Commands_V              : aliased Vk_XML.Commands_Shared_Ptr.T;
               when Child_Feature               => Feature_V               : aliased Vk_XML.Feature_Shared_Ptr.T;
               when Child_Extensions            => Extensions_V            : aliased Vk_XML.Extensions_Shared_Ptr.T;
            end case;
         end record;

         package Child_Vectors is new Aida.Containers.Generic_Immutable_Vector (Index_Type   => Positive,
                                                                                Element_Type => Child_T,
                                                                                "="          => "=",
                                                                                Bounded      => False);
      end Fs;

      type T is limited private with Default_Initial_Condition => True;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) with
        Global => null;

   private

      package Mutable_Child_Vector is new Fs.Child_Vectors.Generic_Mutable_Vector;

      type T is limited
         record
            My_Children : Mutable_Child_Vector.T (100);
         end record;

      function Children (This : T) return Fs.Child_Vectors.Immutable_T is (Fs.Child_Vectors.Immutable_T (This.My_Children));

   end Registry;

   package Registry_Shared_Ptr with SPARK_Mode is

      type T is private with Default_Initial_Condition => True;

      function Children (This : T) return Registry.Fs.Child_Vectors.Immutable_T with
        Global => null;

      procedure Append_Child (This  : in out T;
                              Child : Registry.Fs.Child_T) with
        Global => null;

   private
      pragma SPARK_Mode (Off);

      type Registry_Ptr is access Registry.T;

      package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Registry.T,
                                                             P => Registry_Ptr);

      type T is
         record
            SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Registry.T);
         end record;

      function Children (This : T) return Registry.Fs.Child_Vectors.Immutable_T is (Registry.Children (Smart_Pointers.Value (This.SP).all));

      pragma Inline (Children);

   end Registry_Shared_Ptr;


end Vk_XML;

with Aida.Text_IO;

package body Vk_XML with SPARK_Mode is

   use type Aida.Containers.Count_Type;

   use all type Proto.Fs.Child_Kind_Id_T;
   use all type Proto.Fs.Child_Vectors.Immutable_T;
   use all type Proto_Shared_Ptr.T;
   use all type Command.Fs.Child_Kind_Id_T;
   use all type Command.Fs.Child_Vectors.Immutable_T;
   use all type Nested_Type.Fs.Value.T;
   use all type Nested_Type_Shared_Ptr.T;
   use all type Name.Fs.Value.T;

   package Mutable_XML_Text is new XML_Text.Mutable;

   package body Tag with SPARK_Mode is

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Name_P.Initialize (This => This.My_Name,
                            Text => Text);
      end Set_Name;

      procedure Set_Author (This : in out T;
                            Text : String) is
      begin
         Author_P.Initialize (This => This.My_Author,
                              Text => Text);
      end Set_Author;

      procedure Set_Contact (This : in out T;
                             Text : String) is
      begin
         Contact_P.Initialize (This => This.My_Contact,
                               Text => Text);
      end Set_Contact;

   end Tag;

   package body Tag_Shared_Ptr with SPARK_Mode => Off is

      use all type Tag.T;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Tag.Set_Name (This => Smart_Pointers.Value (This.SP).all,
                       Text => Text);
      end Set_Name;

      procedure Set_Author (This : in out T;
                            Text : String) is
      begin
         Set_Author (This => Smart_Pointers.Value (This.SP).all,
                     Text => Text);
      end Set_Author;

      procedure Set_Contact (This : in out T;
                             Text : String) is
      begin
         Set_Contact (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Contact;

   end Tag_Shared_Ptr;

   package body Tags is

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Children_P.Append (Container => This.My_Children,
                            New_Item  => Child);
      end Append_Child;

   end Tags;

   package body Tags_Shared_Ptr with SPARK_Mode => Off is

      use all type Vk_XML.
        Tags.T;

      procedure Append_Child (This  : in out T;
                              Child : Tags.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Tags_Shared_Ptr;

   package body Nested_Type with SPARK_Mode is

      procedure Set_Value (This : in out T;
                           Text : String)
      is
         Value_V : Mutable_Value.Mutable_T;
      begin
         Initialize (This => Value_V,
                     Text => Text);
         This.My_Value := (Exists => True, Value => Value_V);
      end Set_Value;

   end Nested_Type;

   package body Nested_Type_Shared_Ptr with SPARK_Mode => Off is

      use all type Nested_Type.T;

      procedure Set_Value (This : in out T;
                           Text : String) is
      begin
         Set_Value (This => Smart_Pointers.Value (This.SP).all,
                    Text => Text);
      end Set_Value;

   end Nested_Type_Shared_Ptr;

   package body Enum with SPARK_Mode is

      procedure Set_Value (This : in out T;
                           Text : String) is
      begin
         Initialize (This => This.My_Value,
                     Text => Text);
      end Set_Value;

   end Enum;

   package body Enum_Shared_Ptr with SPARK_Mode => Off is

      use all type Enum.T;

      procedure Set_Value (This : in out T;
                           Text : String) is
      begin
         Set_Value (This => Smart_Pointers.Value (This.SP).all,
                    Text => Text);
      end Set_Value;

   end Enum_Shared_Ptr;

   package body Member with SPARK_Mode is

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) is
      begin
         This.My_Optional := Fs.Optional_T (Value);
      end Set_Optional;

      procedure Set_Len (This : in out T;
                         Text : String)
      is
         Len_V : Mutable_Len.Mutable_T;
      begin
         Initialize (This => Len_V,
                     Text => Text);
         This.My_Len := (Exists => True, Value => Len_V);
      end Set_Len;

      procedure Set_No_Auto_Validity (This  : in out T;
                                      Value : Boolean) is
      begin
         This.My_No_Auto_Validity := Fs.No_Auto_Validity_T (Value);
      end Set_No_Auto_Validity;

      procedure Set_Valid_Extension_Structs (This : in out T;
                                             Text : String)
      is
         Valid_Extension_Structs_V : Mutable_Valid_Extension_Structs.Mutable_T;
      begin
         Initialize (This => Valid_Extension_Structs_V,
                     Text => Text);
         This.My_Valid_Extension_Structs := (Exists => True, Value => Valid_Extension_Structs_V);
      end Set_Valid_Extension_Structs;

   end Member;

   package body Member_Shared_Ptr with SPARK_Mode => Off is

      use all type Member.T;

      procedure Append_Child (This  : in out T;
                              Child : Member.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) is
      begin
         Set_Optional(This  => Smart_Pointers.Value (This.SP).all,
                      Value => Value);
      end Set_Optional;

      procedure Set_Len (This : in out T;
                         Text : String) is
      begin
         Set_Len (This => Smart_Pointers.Value (This.SP).all,
                  Text => Text);
      end Set_Len;

      procedure Set_No_Auto_Validity (This  : in out T;
                                      Value : Boolean) is
      begin
         Set_No_Auto_Validity (This  => Smart_Pointers.Value (This.SP).all,
                               Value => Value);
      end Set_No_Auto_Validity;

      procedure Set_Valid_Extension_Structs (This : in out T;
                                             Text : String) is
      begin
         Set_Valid_Extension_Structs (This => Smart_Pointers.Value (This.SP).all,
                                      Text => Text);
      end Set_Valid_Extension_Structs;

   end Member_Shared_Ptr;

   package body Validity with SPARK_Mode is

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Validity;

   package body Validity_Shared_Ptr with SPARK_Mode => Off is

      procedure Append_Child (This  : in out T;
                              Child : Validity.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Validity_Shared_Ptr;

   package body Usage with SPARK_Mode is

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

      procedure Set_Command (This : in out T;
                             Text : String)
      is
         Command_V : Mutable_Command.Mutable_T;
      begin
         Initialize (This => Command_V,
                     Text => Text);
         This.My_Command := (Exists => True, Value => Command_V);
      end Set_Command;

      procedure Set_Struct (This : in out T;
                            Text : String)
      is
         Struct_V : Mutable_Struct.Mutable_T;
      begin
         Initialize (This => Struct_V,
                     Text => Text);
         This.My_Struct := (Exists => True, Value => Struct_V);
      end Set_Struct;

   end Usage;

   package body Usage_Shared_Ptr with SPARK_Mode => Off is

      procedure Append_Child (This  : in out T;
                              Child : Usage.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

      procedure Set_Command (This : in out T;
                             Text : String) is
      begin
         Set_Command (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Command;

      procedure Set_Struct (This : in out T;
                            Text : String) is
      begin
         Set_Struct (This => Smart_Pointers.Value (This.SP).all,
                     Text => Text);
      end Set_Struct;

   end Usage_Shared_Ptr;

   package body Type_T is

      procedure Set_Name (This : in out T;
                          Text : String)
      is
         Name_V : Name_P.Mutable_T;
      begin
         Name_P.Initialize (This => Name_V,
                            Text => Text);
         This.My_Name := (Exists => True, Value => Name_V);
      end Set_Name;

      procedure Set_Category (This : in out T;
                              Text : String) is
      begin
         Category_P.Initialize (This => This.My_Category,
                                Text => Text);
      end Set_Category;

      procedure Set_Requires (This : in out T;
                              Text : String)
      is
         Requires_V : Mutable_Requires.Mutable_T;
      begin
         Initialize (This => Requires_V,
                     Text => Text);
         This.My_Requires := (Exists => True, Value => Requires_V);
      end Set_Requires;

      procedure Set_Parent (This : in out T;
                            Text : String)
      is
         Parent_V : Mutable_Parent.Mutable_T;
      begin
         Initialize (This => Parent_V,
                     Text => Text);
         This.My_Parent := (Exists => True, Value => Parent_V);
      end Set_Parent;

      procedure Set_Returned_Only (This  : in out T;
                                   Value : Boolean) is
      begin
         This.My_Returned_Only := Fs.Returned_Only_T (Value);
      end Set_Returned_Only;

      procedure Set_Comment (This : in out T;
                             Text : String)
      is
         Comment_V : Mutable_Comment.Mutable_T;
      begin
         Initialize (This => Comment_V,
                     Text => Text);
         This.My_Comment := (Exists => True, Value => Comment_V);
      end Set_Comment;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Children_P.Append (Container => This.My_Children,
                            New_Item  => Child);
      end Append_Child;

      function To_String (This : T) return String is
         S : Mutable_Unbounded_String.T;

         procedure Append_Name_To_String is
         begin
            if This.My_Name.Exists then
               Append (This => S,
                       Text => "name='");
               Append (This => S,
                       Text => To_String (This.My_Name.Value));
               Append (This => S,
                       Text => "' ");
            else
               null;
            end if;
         end Append_Name_To_String;

         procedure Append_Category_To_String is
         begin
            if Length (This.My_Category) = 0 then
               null;
            else
               Append (This => S,
                       Text => "category='");
               Append (This => S,
                       Text => To_String (This.My_Category));
               Append (This => S,
                       Text => "' ");
            end if;
         end Append_Category_To_String;

         procedure Append_Requires_To_String is
         begin
            if This.My_Requires.Exists then
               Append (This => S,
                       Text => "requires='");
               Append (This => S,
                       Text => To_String (This.My_Requires.Value));
               Append (This => S,
                       Text => "' ");
            else
               null;
            end if;
         end Append_Requires_To_String;

      begin
         if Length (This.My_Children) = 0 then
            Append (This => S,
                    Text => "<type ");
            Append_Name_To_String;
            Append_Category_To_String;
            Append_Requires_To_String;
            Append (This => S,
                    Text => "/>");
         else
            Append (This => S,
                    Text => "<type ");
            Append_Name_To_String;
            Append_Category_To_String;
            Append_Requires_To_String;
            Append (This => S,
                    Text => ">");
            Append (This => S,
                    Text => "</type>");
         end if;

         return To_String (S);
      end To_String;

   end Type_T;

   package body Type_Shared_Ptr with SPARK_Mode => Off is

      use all type Vk_XML.Type_T.T;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Set_Name (This => Smart_Pointers.Value (This.SP).all,
                   Text => Text);
      end Set_Name;

      procedure Set_Category (This : in out T;
                              Text : String) is
      begin
         Set_Category (This => Smart_Pointers.Value (This.SP).all,
                       Text => Text);
      end Set_Category;

      procedure Set_Requires (This : in out T;
                              Text : String) is
      begin
         Set_Requires (This => Smart_Pointers.Value (This.SP).all,
                       Text => Text);
      end Set_Requires;

      procedure Set_Parent (This : in out T;
                            Text : String) is
      begin
         Set_Parent (This => Smart_Pointers.Value (This.SP).all,
                     Text => Text);
      end Set_Parent;

      procedure Set_Returned_Only (This  : in out T;
                                   Value : Boolean) is
      begin
         Set_Returned_Only (This  => Smart_Pointers.Value (This.SP).all,
                            Value => Value);
      end Set_Returned_Only;

      procedure Set_Comment (This : in out T;
                             Text : String) is
      begin
         Set_Comment (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Comment;

      procedure Append_Child (This  : in out T;
                              Child : Type_T.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Type_Shared_Ptr;

   package body Types is

      use all type Children_P.T;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Append (Container => This.My_Children,
                 New_Item  => Child);
      end Append_Child;

   end Types;

   package body Types_Shared_Ptr with SPARK_Mode => Off is

      use all type Types.T;

      procedure Append_Child (This  : in out T;
                              Child : Types.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Types_Shared_Ptr;

   package body Vendor_Id with SPARK_Mode is

      use all type Name_P.Mutable_T;
      use all type Id_P.Mutable_T;
      use all type Comment_P.Mutable_T;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Initialize (This => This.My_Name,
                     Text => Text);
      end Set_Name;

      procedure Set_Id (This : in out T;
                        Text : String) is
      begin
         Initialize (This => This.My_Id,
                     Text => Text);
      end Set_Id;

      procedure Set_Comment (This : in out T;
                             Text : String) is
      begin
         Initialize (This => This.My_Comment,
                     Text => Text);
      end Set_Comment;

   end Vendor_Id;

   package body Vendor_Id_Shared_Ptr with SPARK_Mode => Off is

      use all type Vendor_Id.T;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Set_Name (This => Smart_Pointers.Value (This.SP).all,
                   Text => Text);
      end Set_Name;

      procedure Set_Id (This : in out T;
                        Text : String) is
      begin
         Set_Id (This => Smart_Pointers.Value (This.SP).all,
                 Text => Text);
      end Set_Id;

      procedure Set_Comment (This : in out T;
                             Text : String) is
      begin
         Set_Comment (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Comment;

   end Vendor_Id_Shared_Ptr;

   package body Vendor_Ids is

      use all type Mutable_Child_Vector.T;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T)
      is
      begin
         Append (Container => This.My_Children,
                 New_Item  => Child);
      end Append_Child;

   end Vendor_Ids;

   package body Vendor_Ids_Shared_Ptr with SPARK_Mode => Off is

      use all type Vendor_Ids.T;

      procedure Append_Child (This  : in out T;
                              Child : Vendor_Ids.Fs.Child_T)
      is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Vendor_Ids_Shared_Ptr;

   package body Comment is

      procedure Set_Value (This : in out T;
                           Text : String) is
      begin
         Initialize (This => This.My_Value,
                     Text => Text);
      end Set_Value;

   end Comment;

   package body Comment_Shared_Ptr with SPARK_Mode => Off is

      use all type Vk_XML.Comment.T;

      procedure Set_Value (This : in out T;
                           Text : String) is
      begin
         Set_Value (This => Smart_Pointers.Value (This.SP).all,
                    Text => Text);
      end Set_Value;

   end Comment_Shared_Ptr;

   package body Name with SPARK_Mode is

      procedure Set_Value (This : in out T;
                           Text : String) is
      begin
         Initialize (This => This.My_Value,
                     Text => Text);
      end Set_Value;

   end Name;

   package body Name_Shared_Ptr with SPARK_Mode => Off is

      use all type Name.T;

      procedure Set_Value (This : in out T;
                           Text : String) is
      begin
         Set_Value (This => Smart_Pointers.Value (This.SP).all,
                    Text => Text);
      end Set_Value;

   end Name_Shared_Ptr;

   package body Unused with SPARK_Mode is

      procedure Set_Start (This : in out T;
                           Text : String)
      is
         Start_V : Mutable_Start.Mutable_T;
      begin
         Initialize (This => Start_V,
                     Text => Text);
         This.My_Start := (Exists => True, Value => Start_V);
      end Set_Start;

   end Unused;

   package body Unused_Shared_Ptr with SPARK_Mode => Off is

      use all type Unused.T;

      procedure Set_Start (This : in out T;
                           Text : String) is
      begin
         Set_Start (This => Smart_Pointers.Value (This.SP).all,
                    Text => Text);
      end Set_Start;

   end Unused_Shared_Ptr;

   package body Enums with SPARK_Mode is

      procedure Set_Comment (This : in out T;
                             Text : String) is
         Comment_V : Mutable_Comment.Mutable_T;
      begin
         Initialize (This => Comment_V,
                     Text => Text);
         This.My_Comment := (Exists => True, Value => Comment_V);
      end Set_Comment;

      procedure Set_Name (This : in out T;
                          Text : String)
      is
         Name_V : Mutable_Name.Mutable_T;
      begin
         Initialize (This => Name_V,
                     Text => Text);
         This.My_Name := (Exists => True, Value => Name_V);
      end Set_Name;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

      procedure Set_Type_Attribue (This : in out T;
                                   Value : Fs.Type_Attribue_T) is
      begin
         This.My_Type_Attribue := (Exists => True, Value => Value);
      end Set_Type_Attribue;

   end Enums;

   package body Enums_Shared_Ptr with SPARK_Mode => Off is

      use all type Enums.T;

      procedure Set_Comment (This : in out T;
                             Text : String) is
      begin
         Set_Comment (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Comment;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Set_Name (This => Smart_Pointers.Value (This.SP).all,
                   Text => Text);
      end Set_Name;

      procedure Append_Child (This  : in out T;
                              Child : Enums.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

      procedure Set_Type_Attribue (This : in out T;
                                   Value : Enums.Fs.Type_Attribue_T) is
      begin
         Set_Type_Attribue (This => Smart_Pointers.Value (This.SP).all,
                            Value => Value);
      end Set_Type_Attribue;

   end Enums_Shared_Ptr;

   package body Enums_Enum with SPARK_Mode is

      procedure Set_Value (This : in out T;
                           Text : String)
      is
         Value_V : Mutable_Value.Mutable_T;
      begin
         Initialize (This => Value_V,
                     Text => Text);
         This.My_Value := (Exists => True, Value => Value_V);
      end Set_Value;

      procedure Set_Name (This : in out T;
                             Text : String)
      is
         Name_V : Mutable_Name.Mutable_T;
      begin
         Initialize (This => Name_V,
                     Text => Text);
         This.My_Name := (Exists => True, Value => Name_V);
      end Set_Name;

      procedure Set_Comment (This : in out T;
                             Text : String)
      is
         Comment_V : Mutable_Comment.Mutable_T;
      begin
         Initialize (This => Comment_V,
                     Text => Text);
         This.My_Comment := (Exists => True, Value => Comment_V);
      end Set_Comment;

      procedure Set_Bit_Position (This : in out T;
                                  Value : Fs.Bit_Position_T) is
      begin
         This.My_Bit_Position := (Exists => True, Value => Value);
      end Set_Bit_Position;

      procedure To_Standard_Out (This : T) with
        SPARK_Mode => Off
      is
      begin
         Aida.Text_IO.Put ("<enum ");

         if This.My_Value.Exists then
            Aida.Text_IO.Put ("value='");
            Aida.Text_IO.Put (To_String (This.My_Value.Value));
            Aida.Text_IO.Put ("' ");
         end if;

         if This.My_Name.Exists then
            Aida.Text_IO.Put ("name='");
            Aida.Text_IO.Put (To_String (This.My_Name.Value));
            Aida.Text_IO.Put ("' ");
         end if;

         if This.My_Comment.Exists then
            Aida.Text_IO.Put ("comment='");
            Aida.Text_IO.Put (To_String (This.My_Comment.Value));
            Aida.Text_IO.Put ("' ");
         end if;

         if This.My_Bit_Position.Exists then
            Aida.Text_IO.Put ("bitposition='");
            Aida.Text_IO.Put (This.My_Bit_Position.Value'Img);
            Aida.Text_IO.Put ("' ");
         end if;

         Aida.Text_IO.Put_Line ("/>");
      end To_Standard_Out;

   end Enums_Enum;

   package body Enums_Enum_Shared_Ptr with SPARK_Mode => Off is

      use all type Enums_Enum.T;

      procedure Set_Value (This : in out T;
                           Text : String) is
      begin
         Set_Value (This => Smart_Pointers.Value (This.SP).all,
                    Text => Text);
      end Set_Value;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Set_Name (This => Smart_Pointers.Value (This.SP).all,
                   Text => Text);
      end Set_Name;

      procedure Set_Comment (This : in out T;
                             Text : String) is
      begin
         Set_Comment (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Comment;

      procedure Set_Bit_Position (This : in out T;
                                  Value : Enums_Enum.Fs.Bit_Position_T) is
      begin
         Set_Bit_Position (This  => Smart_Pointers.Value (This.SP).all,
                           Value => Value);
      end Set_Bit_Position;

      procedure To_Standard_Out (This : T) is
      begin
         To_Standard_Out (This => Smart_Pointers.Value (This.SP).all);
      end To_Standard_Out;

   end Enums_Enum_Shared_Ptr;

   package body Proto with SPARK_Mode is

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Proto;

   package body Proto_Shared_Ptr with SPARK_Mode => Off is

      use all type Proto.T;

      procedure Append_Child (This  : in out T;
                              Child : Proto.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Proto_Shared_Ptr;

   package body Param with SPARK_Mode is

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) is
      begin
         This.My_Optional := Fs.Optional_T (Value);
      end Set_Optional;

      procedure Set_External_Sync (This : in out T;
                                   Text : String)
      is
         External_Sync_V : Mutable_External_Sync.Mutable_T;
      begin
         Initialize (This => External_Sync_V,
                     Text => Text);
         This.My_External_Sync := (Exists => True, Value => External_Sync_V);
      end Set_External_Sync;

      procedure Set_Len (This : in out T;
                         Text : String)
      is
         Len_V : Mutable_Len.Mutable_T;
      begin
         Initialize (This => Len_V,
                     Text => Text);
         This.My_Len := (Exists => True, Value => Len_V);
      end Set_Len;

      procedure Set_No_Auto_Validity (This : in out T;
                                      Value : Boolean) is
      begin
         This.My_No_Auto_Validity := (Exists => True, Value => Fs.No_Auto_Validity_T (Value));
      end Set_No_Auto_Validity;

   end Param;

   package body Param_Shared_Ptr with SPARK_Mode => Off is

      use all type Param.T;

      procedure Append_Child (This  : in out T;
                              Child : Param.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

      procedure Set_Optional (This  : in out T;
                              Value : Boolean) is
      begin
         Set_Optional (This  => Smart_Pointers.Value (This.SP).all,
                       Value => Value);
      end Set_Optional;

      procedure Set_External_Sync (This : in out T;
                                   Text : String) is
      begin
         Set_External_Sync (This => Smart_Pointers.Value (This.SP).all,
                            Text => Text);
      end Set_External_Sync;

      procedure Set_Len (This : in out T;
                         Text : String) is
      begin
         Set_Len (This => Smart_Pointers.Value (This.SP).all,
                  Text => Text);
      end Set_Len;

      procedure Set_No_Auto_Validity (This : in out T;
                                      Value : Boolean) is
      begin
         Set_No_Auto_Validity (This => Smart_Pointers.Value (This.SP).all,
                               Value => Value);
      end Set_No_Auto_Validity;

   end Param_Shared_Ptr;

   package body External_Sync_Parameter with SPARK_Mode is

      procedure Set_XML_Value (This : in out T;
                               Text : String)
      is
         XML_Value_V : Mutable_XML_Value.Mutable_T;
      begin
         Initialize (This => XML_Value_V,
                     Text => Text);
         This.My_XML_Value := (Exists => True, Value => XML_Value_V);
      end Set_XML_Value;

   end External_Sync_Parameter;

   package body External_Sync_Parameter_Shared_Ptr with SPARK_Mode => Off is

      use all type External_Sync_Parameter.T;

      procedure Set_XML_Value (This : in out T;
                               Text : String) is
      begin
         Set_XML_Value (This => Smart_Pointers.Value (This.SP).all,
                        Text => Text);
      end Set_XML_Value;

   end External_Sync_Parameter_Shared_Ptr;

   package body Implicit_External_Sync_Parameters with SPARK_Mode is

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Implicit_External_Sync_Parameters;

   package body Implicit_External_Sync_Parameters_Shared_Ptr with SPARK_Mode => Off is

      use all type Implicit_External_Sync_Parameters.T;

      procedure Append_Child (This  : in out T;
                              Child : Implicit_External_Sync_Parameters.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Implicit_External_Sync_Parameters_Shared_Ptr;

   package body Command with SPARK_Mode is

      procedure Append_Success_Code (This : in out T;
                                     Text : String)
      is
         SC : Mutable_Success_Code.Mutable_T;
      begin
         Initialize (This => SC,
                     Text => Text);

         Append (Container => This.My_Success_Codes,
                 New_Item  => Fs.Success_Code.T (SC));
      end Append_Success_Code;

      procedure Append_Error_Code (This : in out T;
                                   Text : String)
      is
         EC : Mutable_Error_Code.Mutable_T;
      begin
         Initialize (This => EC,
                     Text => Text);

         Append (Container => This.My_Error_Codes,
                 New_Item  => Fs.Error_Code.T (EC));
      end Append_Error_Code;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

      procedure Append_Queue (This  : in out T;
                              Queue : Fs.Queue_T) is
      begin
         Append (Container => This.My_Queues,
                 New_Item  => Queue);
      end Append_Queue;

      procedure Append_Render_Pass (This  : in out T;
                                    Value : Fs.Render_Pass_T) is
      begin
         Append (Container => This.My_Render_Passes,
                 New_Item  => Value);
      end Append_Render_Pass;

      procedure Append_Command_Buffer_Level (This  : in out T;
                                             Value : Fs.Command_Buffer_Level_T) is
      begin
         Append (Container => This.My_Command_Buffer_Levels,
                 New_Item  => Value);
      end Append_Command_Buffer_Level;

      function To_String (This : T) return String is
         R : Aida.Strings.Unbounded_String_Type;

         procedure Handle_Proto (Proto_V : Vk_XML.Proto_Shared_Ptr.T) is

            procedure Handle_Nested_Type (Nested_Type_V : Nested_Type_Shared_Ptr.T) is
            begin
               R.Append ("<type>");

               if Nested_Type_Shared_Ptr.Value (Nested_Type_V).Exists then
                  R.Append (To_String (Nested_Type_Shared_Ptr.Value (Nested_Type_V).Value_V));
               end if;

               R.Append ("</type>");
            end Handle_Nested_Type;

            procedure Handle_Name (Name_V : Name_Shared_Ptr.T) is
            begin
               R.Append ("<name>");

               R.Append (To_String (Name_Shared_Ptr.Value (Name_V)));

               R.Append ("</name>");
            end Handle_Name;

         begin
            for I in Positive range First_Index (Proto_Shared_Ptr.Children (Proto_V))..Last_Index (Proto_Shared_Ptr.Children (Proto_V)) loop
               case Element (Proto_Shared_Ptr.Children (Proto_V), I).Kind_Id is
                  when Child_XML_Dummy   => null;
                  when Child_Nested_Type => Handle_Nested_Type (Element (Proto_Shared_Ptr.Children (Proto_V), I).Nested_Type_V);
                  when Child_Name        => Handle_Name (Element (Proto_Shared_Ptr.Children (Proto_V), I).Name_V);
               end case;
            end loop;
         end Handle_Proto;

      begin
         R.Append ("<command >");
         for I in Positive range First_Index (Children (This))..Last_Index (Children (This)) loop
            case Element (Children (This), I).Kind_Id is
               when Child_Proto => Handle_Proto (Element (Children (This), I).Proto_V);
               when Child_Param => null;
               when Child_Validity => null;
               when Child_Implicit_External_Sync_Parameters => null;
               when Child_XML_Dummy => null;
            end case;
         end loop;
         R.Append ("</command>");

         return R.To_String;
      end To_String;

   end Command;

   package body Command_Shared_Ptr with SPARK_Mode => Off is

      use all type Command.T;

      procedure Append_Success_Code (This : in out T;
                                     Text : String) is
      begin
         Append_Success_Code (This => Smart_Pointers.Value (This.SP).all,
                              Text => Text);
      end Append_Success_Code;

      procedure Append_Error_Code (This : in out T;
                                   Text : String) is
      begin
         Append_Error_Code (This => Smart_Pointers.Value (This.SP).all,
                            Text => Text);
      end Append_Error_Code;

      procedure Append_Child (This  : in out T;
                              Child : Command.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

      procedure Append_Queue (This  : in out T;
                              Queue : Command.Fs.Queue_T) is
      begin
         Append_Queue (This  => Smart_Pointers.Value (This.SP).all,
                       Queue => Queue);
      end Append_Queue;

      procedure Append_Render_Pass (This  : in out T;
                                    Value : Command.Fs.Render_Pass_T) is
      begin
         Append_Render_Pass (This  => Smart_Pointers.Value (This.SP).all,
                             Value => Value);
      end Append_Render_Pass;

      procedure Append_Command_Buffer_Level (This  : in out T;
                                             Value : Command.Fs.Command_Buffer_Level_T) is
      begin
         Append_Command_Buffer_Level (This  => Smart_Pointers.Value (This.SP).all,
                                      Value => Value);
      end Append_Command_Buffer_Level;

      function To_String (This : T) return String is
      begin
         return To_String (Smart_Pointers.Value (This.SP).all);
      end To_String;

   end Command_Shared_Ptr;

   package body Require_Enum with SPARK_Mode is

      procedure Set_Name (This : in out T;
                          Text : String)
      is
         Name_V : Mutable_Name.Mutable_T;
      begin
         Initialize (This => Name_V,
                     Text => Text);
         This.My_Name := (Exists => True, Value => Name_V);
      end Set_Name;

      procedure Set_Value (This : in out T;
                           Text : String)
      is
         Value_V : Mutable_Value.Mutable_T;
      begin
         Initialize (This => Value_V,
                     Text => Text);
         This.My_Value := (Exists => True, Value => Value_V);
      end Set_Value;

      procedure Set_Offset (This : in out T;
                            Value : Fs.Offset_T) is
      begin
         This.My_Offset := (Exists => True, Value => Value);
      end Set_Offset;

      procedure Set_Dir (This : in out T;
                         Text : String)
      is
         Dir_V : Mutable_Dir.Mutable_T;
      begin
         Initialize (This => Dir_V,
                     Text => Text);
         This.My_Dir := (Exists => True, Value => Dir_V);
      end Set_Dir;

      procedure Set_Extends (This : in out T;
                             Text : String)
      is
         Extends_V : Mutable_Extends.Mutable_T;
      begin
         Initialize (This => Extends_V,
                     Text => Text);
         This.My_Extends := (Exists => True, Value => Extends_V);
      end Set_Extends;

      procedure Set_Comment (This : in out T;
                             Text : String)
      is
         Comment_V : Mutable_Comment.Mutable_T;
      begin
         Initialize (This => Comment_V,
                     Text => Text);
         This.My_Comment := (Exists => True, Value => Comment_V);
      end Set_Comment;

      procedure Set_Bit_Position (This : in out T;
                                  Value : Fs.Bit_Position_T) is
      begin
         This.My_Bit_Position := (Exists => True, Value => Value);
      end Set_Bit_Position;

   end Require_Enum;

   package body Require_Enum_Shared_Ptr with SPARK_Mode => Off is

      use all type Require_Enum.T;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Set_Name (This => Smart_Pointers.Value (This.SP).all,
                   Text => Text);
      end Set_Name;

      procedure Set_Value (This : in out T;
                           Text : String) is
      begin
         Set_Value (This => Smart_Pointers.Value (This.SP).all,
                    Text => Text);
      end Set_Value;

      procedure Set_Offset (This : in out T;
                            Value : Require_Enum.Fs.Offset_T) is
      begin
         Set_Offset (This => Smart_Pointers.Value (This.SP).all,
                     Value => Value);
      end Set_Offset;

      procedure Set_Dir (This : in out T;
                         Text : String) is
      begin
         Set_Dir (This => Smart_Pointers.Value (This.SP).all,
                  Text => Text);
      end Set_Dir;

      procedure Set_Extends (This : in out T;
                             Text : String) is
      begin
         Set_Extends (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Extends;

      procedure Set_Comment (This : in out T;
                             Text : String) is
      begin
         Set_Comment (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Comment;

      procedure Set_Bit_Position (This : in out T;
                                  Value : Require_Enum.Fs.Bit_Position_T) is
      begin
         Set_Bit_Position (This => Smart_Pointers.Value (This.SP).all,
                           Value => Value);
      end Set_Bit_Position;

   end Require_Enum_Shared_Ptr;

   package body Require_Command with SPARK_Mode is

      procedure Set_Name (This : in out T;
                          Text : String)
      is
         Name_V : Mutable_Name.Mutable_T;
      begin
         Initialize (This => Name_V,
                     Text => Text);
         This.My_Name := (Exists => True, Value => Name_V);
      end Set_Name;

   end Require_Command;

   package body Require_Command_Shared_Ptr with SPARK_Mode => Off is

      use all type Require_Command.T;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Set_Name (This => Smart_Pointers.Value (This.SP).all,
                   Text => Text);
      end Set_Name;

   end Require_Command_Shared_Ptr;

   package body Require with SPARK_Mode is

      procedure Set_Comment (This : in out T;
                             Text : String)
      is
         Comment_V : Mutable_Comment.Mutable_T;
      begin
         Initialize (This => Comment_V,
                     Text => Text);
         This.My_Comment := (Exists => True, Value => Comment_V);
      end Set_Comment;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Require;

   package body Require_Shared_Ptr with SPARK_Mode => Off is

      use all type Require.T;

      procedure Set_Comment (This : in out T;
                             Text : String) is
      begin
         Set_Comment (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Comment;

      procedure Append_Child (This  : in out T;
                              Child : Require.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Require_Shared_Ptr;

   package body Feature with SPARK_Mode is

      procedure Set_API (This : in out T;
                         Text : String)
      is
         API_V : Mutable_API.Mutable_T;
      begin
         Initialize (This => API_V,
                     Text => Text);
         This.My_API := (Exists => True, Value => API_V);
      end Set_API;

      procedure Set_Name (This : in out T;
                          Text : String)
      is
         Name_V : Mutable_Name.Mutable_T;
      begin
         Initialize (This => Name_V,
                     Text => Text);
         This.My_Name := (Exists => True, Value => Name_V);
      end Set_Name;

      procedure Set_Number (This : in out T;
                            Text : String)
      is
         Number_V : Mutable_Number.Mutable_T;
      begin
         Initialize (This => Number_V,
                     Text => Text);
         This.My_Number := (Exists => True, Value => Number_V);
      end Set_Number;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Feature;

   package body Feature_Shared_Ptr with SPARK_Mode => Off is

      use all type Feature.T;

      procedure Set_API (This : in out T;
                         Text : String) is
      begin
         Set_API (This => Smart_Pointers.Value (This.SP).all,
                  Text => Text);
      end Set_API;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Set_Name (This => Smart_Pointers.Value (This.SP).all,
                   Text => Text);
      end Set_Name;

      procedure Set_Number (This : in out T;
                            Text : String) is
      begin
         Set_Number (This => Smart_Pointers.Value (This.SP).all,
                     Text => Text);
      end Set_Number;

      procedure Append_Child (This  : in out T;
                              Child : Feature.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Feature_Shared_Ptr;

   package body Commands with SPARK_Mode is

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Commands;

   package body Commands_Shared_Ptr with SPARK_Mode => Off is

      use all type Commands.T;

      procedure Append_Child (This  : in out T;
                              Child : Commands.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Commands_Shared_Ptr;

   package body Extension with SPARK_Mode is

      procedure Set_Name (This : in out T;
                          Text : String)
      is
         Name_V : Mutable_Name.Mutable_T;
      begin
         Initialize (This => Name_V,
                     Text => Text);
         This.My_Name := (Exists => True, Value => Name_V);
      end Set_Name;

      procedure Set_Number (This : in out T;
                            Value : Fs.Number_T) is
      begin
         This.My_Number := (Exists => True, Value => Value);
      end Set_Number;

      procedure Set_Supported (This : in out T;
                               Value : Fs.Supported_T) is
      begin
         This.My_Supported := (Exists => True, Value => Value);
      end Set_Supported;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

      procedure Set_Protect (This : in out T;
                             Text : String)
      is
         Protect_V : Mutable_Protect.Mutable_T;
      begin
         Initialize (This => Protect_V,
                     Text => Text);
         This.My_Protect := (Exists => True, Value => Protect_V);
      end Set_Protect;

      procedure Set_Author (This : in out T;
                            Text : String)
      is
         Author_V : Mutable_Author.Mutable_T;
      begin
         Initialize (This => Author_V,
                     Text => Text);
         This.My_Author := (Exists => True, Value => Author_V);
      end Set_Author;

      procedure Set_Contact (This : in out T;
                             Text : String)
      is
         Contact_V : Mutable_Contact.Mutable_T;
      begin
         Initialize (This => Contact_V,
                     Text => Text);
         This.My_Contact := (Exists => True, Value => Contact_V);
      end Set_Contact;

   end Extension;

   package body Extension_Shared_Ptr with SPARK_Mode => Off is

      use all type Extension.T;

      procedure Set_Name (This : in out T;
                          Text : String) is
      begin
         Set_Name (This => Smart_Pointers.Value (This.SP).all,
                   Text => Text);
      end Set_Name;

      procedure Set_Number (This : in out T;
                            Value : Extension.Fs.Number_T) is
      begin
         Set_Number (This => Smart_Pointers.Value (This.SP).all,
                     Value => Value);
      end Set_Number;

      procedure Set_Supported (This : in out T;
                               Value : Extension.Fs.Supported_T) is
      begin
         Set_Supported (This => Smart_Pointers.Value (This.SP).all,
                        Value => Value);
      end Set_Supported;

      procedure Append_Child (This  : in out T;
                              Child : Extension.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

      procedure Set_Protect (This : in out T;
                             Text : String) is
      begin
         Set_Protect (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Protect;

      procedure Set_Author (This : in out T;
                            Text : String) is
      begin
         Set_Author (This => Smart_Pointers.Value (This.SP).all,
                     Text => Text);
      end Set_Author;

      procedure Set_Contact (This : in out T;
                             Text : String) is
      begin
         Set_Contact (This => Smart_Pointers.Value (This.SP).all,
                      Text => Text);
      end Set_Contact;

   end Extension_Shared_Ptr;

   package body Extensions with SPARK_Mode is

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Mutable_Children.Append (Container => This.My_Children,
                                  New_Item  => Child);
      end Append_Child;

   end Extensions;

   package body Extensions_Shared_Ptr with SPARK_Mode => Off is

      use all type Extensions.T;

      procedure Append_Child (This  : in out T;
                              Child : Extensions.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Extensions_Shared_Ptr;

   package body Registry with SPARK_Mode is

      use all type Mutable_Child_Vector.T;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_T) is
      begin
         Append (Container => This.My_Children,
                 New_Item  => Child);
      end Append_Child;

   end Registry;

   package body Registry_Shared_Ptr with SPARK_Mode => Off is

      use all type Registry.T;

      procedure Append_Child (This  : in out T;
                              Child : Registry.Fs.Child_T) is
      begin
         Append_Child (Smart_Pointers.Value (This.SP).all, Child);
      end Append_Child;

   end Registry_Shared_Ptr;

end Vk_XML;

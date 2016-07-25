package body Vk with SPARK_Mode is

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

      use all type Vk.Tags.T;

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


   end Usage;

   package body Usage_Shared_Ptr with SPARK_Mode => Off is

      procedure Append_Child (This  : in out T;
                              Child : Usage.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

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

   end Type_T;

   package body Type_Shared_Ptr with SPARK_Mode => Off is

      use all type Vk.Type_T.T;

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

      use all type Vk.Comment.T;

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

   end Param;

   package body Param_Shared_Ptr with SPARK_Mode => Off is

      use all type Param.T;

      procedure Append_Child (This  : in out T;
                              Child : Param.Fs.Child_T) is
      begin
         Append_Child (This  => Smart_Pointers.Value (This.SP).all,
                       Child => Child);
      end Append_Child;

   end Param_Shared_Ptr;

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

   end Command_Shared_Ptr;

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

end Vk;

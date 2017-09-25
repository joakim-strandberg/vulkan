package body Generic_Vk_XML is

--     use all type Name.Value_T;
--     use all type Proto.Child_Kind_Id_T;

   use all type Ada.Strings.Unbounded.Unbounded_String;

   use type Ada.Containers.Count_Type;

   package body Tag_Tag is

      procedure Set_Name (This : in out T;
                          Value : Aida.String_T)
      is
      begin
         This.My_Name := (Exists => True,
                          Value  => new Aida.String_T'(Value));
      end Set_Name;

      procedure Set_Author (This  : in out T;
                            Value : Aida.String_T)
      is
      begin
         This.My_Author := (Exists => True,
                            Value  => new Aida.String_T'(Value));
      end Set_Author;

      procedure Set_Contact (This  : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Contact := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Contact;

   end Tag_Tag;

   package body Tags_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Tags_Tag;

   package body Comment_Tag is

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T)
      is
      begin
         This.My_Value := (Exists => True,
                           Value  => new Aida.String_T'(Value));
      end Set_Value;

   end Comment_Tag;

   package body Name_Tag is

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T)
      is
      begin
         This.My_Value := (Exists => True,
                           Value  => new Aida.String_T'(Value));
      end Set_Value;

   end Name_Tag;

   package body Nested_Type_Tag is

      procedure Set_Value (This : in out T;
                           Value : Aida.String_T)
      is
      begin
         This.My_Value := (Exists => True,
                           Value  => new Aida.String_T'(Value));
      end Set_Value;

   end Nested_Type_Tag;

   package body Enum_Tag is

      procedure Set_Value (This  : in out T;
                           Value : Aida.String_T)
      is
      begin
         This.My_Value := (Exists => True,
                           Value  => new Aida.String_T'(Value));
      end Set_Value;

   end Enum_Tag;

   package body Enums_Enum_Tag is

      procedure Set_Bit_Position (This : in out T;
                                  Value : Bit_Position_T)
      is
      begin
         This.My_Bit_Position := (Exists => True,
                                  Value  => Value);
      end Set_Bit_Position;

      procedure Set_Comment (This : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Comment := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Comment;

      procedure Set_Name (This : in out T;
                          Value : Aida.String_T)
      is
      begin
         This.My_Name := (Exists => True,
                          Value  => new Aida.String_T'(Value));
      end Set_Name;

      procedure Set_Value (This : in out T;
                           Value : Aida.String_T)
      is
      begin
         This.My_Value := (Exists => True,
                           Value  => new Aida.String_T'(Value));
      end Set_Value;

   end Enums_Enum_Tag;

   package body Member_Tag is

      use all type Ada.Strings.Unbounded.Unbounded_String;

      procedure Set_Valid_Extension_Structs (This : in out T;
                                             Value : Aida.String_T)
      is
      begin
         This.My_Valid_Extension_Structs := (Exists => True,
                                             Value  => new Aida.String_T'(Value));
      end Set_Valid_Extension_Structs;

      procedure Set_No_Auto_Validity (This : in out T;
                                      Value : Boolean)
      is
      begin
         This.My_No_Auto_Validity := (Exists => True,
                                      Value  => Value);
      end Set_No_Auto_Validity;

      procedure Set_Len (This : in out T;
                         Value : Aida.String_T)
      is
      begin
         This.My_Len := (Exists => True,
                         Value  => new Aida.String_T'(Value));
      end Set_Len;

      procedure Set_Optional (This : in out T;
                              Value : Boolean)
      is
      begin
         This.My_Optional := (Exists => True,
                              Value  => Value);
      end Set_Optional;

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

      function To_String (This : T) return Aida.String_T is
         S : Ada.Strings.Unbounded.Unbounded_String;

      begin
         if This.My_Children.Is_Empty then
            Append (S, "<member ");
            Append (S, "/>");
         else
            Append (S, "<member>");

            for Type_V of This.My_Children loop
               case Type_V.Kind_Id is
               when Child_Name =>
                  Append (S, "<name/>");
               when Child_Nested_Type =>
                  Append (S, "<type/>");
               when Child_Enum =>
                  Append (S, "<enum/>");
               when Child_XML_Text =>
                  Append (S, String (Type_V.XML_Text.all));
               end case;
            end loop;

            Append (S, "</member>");
         end if;

         return Aida.String_T (To_String (S));
      end To_String;

   end Member_Tag;

   package body Usage_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

      procedure Set_Struct (This : in out T;
                            Value : Aida.String_T)
      is
      begin
         This.My_Struct := (Exists => True,
                            Value  => new Aida.String_T'(Value));
      end Set_Struct;

      procedure Set_Command (This : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Command := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Command;

   end Usage_Tag;

   package body Validity_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Validity_Tag;

   package body Type_Tag is

      use all type Ada.Strings.Unbounded.Unbounded_String;

      procedure Set_Name (This : in out T;
                          Value : Aida.String_T)
      is
      begin
         This.My_Name := (Exists => True,
                          Value  => new Aida.String_T'(Value));
      end Set_Name;

      procedure Set_Category (This : in out T;
                              Value : Aida.String_T)
      is
      begin
         This.My_Category := (Exists => True,
                              Value  => new Aida.String_T'(Value));
      end Set_Category;

      procedure Set_Requires (This : in out T;
                              Value : Aida.String_T)
      is
      begin
         This.My_Requires := (Exists => True,
                              Value  => new Aida.String_T'(Value));
      end Set_Requires;

      procedure Set_Parent (This : in out T;
                            Value : Aida.String_T)
      is
      begin
         This.My_Parent := (Exists => True,
                            Value  => new Aida.String_T'(Value));
      end Set_Parent;

      procedure Set_Returned_Only (This : in out T;
                                   Value : Boolean)
      is
      begin
         This.My_Returned_Only := (Exists => True,
                                   Value  => Value);
      end Set_Returned_Only;

      procedure Set_Comment (This : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Comment := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Comment;

      function To_String (This : T) return String is
         S : Ada.Strings.Unbounded.Unbounded_String;

         procedure Append_Name_To_String is
         begin
            if This.Exists_Name then
               Append (S, "name='");
               Append (S, String (This.Name));
               Append (S, "' ");
            else
               null;
            end if;
         end Append_Name_To_String;

         procedure Append_Category_To_String is
         begin
            if This.Exists_Category then
               Append (S, "category='");
               Append (S, String (This.Category));
               Append (S, "' ");
            else
               null;
            end if;
         end Append_Category_To_String;

         procedure Append_Requires_To_String is
         begin
            if This.Exists_Requires then
               Append (S, "requires='");
               Append (S, String (This.Requires));
               Append (S, "' ");
            else
               null;
            end if;
         end Append_Requires_To_String;

      begin
         if This.My_Children.Is_Empty then
            Append (S, "<type ");
            Append_Name_To_String;
            Append_Category_To_String;
            Append_Requires_To_String;
            Append (S, "/>");
         else
            Append (S, "<type ");
            Append_Name_To_String;
            Append_Category_To_String;
            Append_Requires_To_String;
            Append (S, ">");
            Append (S, "</type>");
         end if;

         return To_String (S);
      end To_String;

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Type_Tag;

   package body Types_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Types_Tag;

   package body Vendor_Id_Tag is

      procedure Set_Comment (This  : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Comment := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Comment;

      procedure Set_Id (This  : in out T;
                        Value : Aida.String_T)
      is
      begin
         This.My_Id := (Exists => True,
                        Value  => new Aida.String_T'(Value));
      end Set_Id;

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T)
      is
      begin
         This.My_Name := (Exists => True,
                          Value  => new Aida.String_T'(Value));
      end Set_Name;

   end Vendor_Id_Tag;

   package body Vendor_Ids_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Vendor_Ids_Tag;

   package body Unused_Tag is

      procedure Set_Start (This : in out T;
                           Value : Aida.String_T)
      is
      begin
         This.My_Start := (Exists => True,
                           Value  => new Aida.String_T'(Value));
      end Set_Start;

   end Unused_Tag;

   package body Enums_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

      procedure Set_Comment (This : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Comment := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Comment;

      procedure Set_Name (This : in out T;
                          Value : Aida.String_T)
      is
      begin
         This.My_Name := (Exists => True,
                          Value  => new Aida.String_T'(Value));
      end Set_Name;

      procedure Set_Type_Attribute (This : in out T;
                                    Value : Type_Attribue_T)
      is
      begin
         This.My_Type_Attribute := (Exists => True,
                                    Value  => Value);
      end Set_Type_Attribute;

   end Enums_Tag;

   package body Proto_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Proto_Tag;

   package body Param_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

      procedure Set_No_Auto_Validity (This : in out T;
                                      Value : Boolean)
      is
      begin
         This.My_No_Auto_Validity := (Exists => True,
                                      Value  => Value);
      end Set_No_Auto_Validity;

      procedure Set_Len (This : in out T;
                         Value : Aida.String_T)
      is
      begin
         This.My_Len := (Exists => True,
                         Value  => new Aida.String_T'(Value));
      end Set_Len;

      procedure Set_External_Sync (This : in out T;
                                   Value : Aida.String_T)
      is
      begin
         This.My_External_Sync := (Exists => True,
                                   Value  => new Aida.String_T'(Value));
      end Set_External_Sync;

      procedure Set_Optional (This : in out T;
                              Value : Boolean)
      is
      begin
         This.My_Optional := (Exists => True,
                              Value  => Value);
      end Set_Optional;

   end Param_Tag;

   package body External_Sync_Parameter_Tag is

      procedure Set_Value (This : in out T;
                           Value : Aida.String_T)
      is
      begin
         This.My_Value := (Exists => True,
                           Value  => new Aida.String_T'(Value));
      end Set_Value;

   end External_Sync_Parameter_Tag;

   package body Implicit_External_Sync_Parameters_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Implicit_External_Sync_Parameters_Tag;

   package body Command_Tag is

      use all type Ada.Strings.Unbounded.Unbounded_String;

      use all type Proto_Tag.Child_Kind_Id_T;

      procedure Append_Success_Code (This : in out T;
                                     Item : Aida.String_T)
      is
         R : String_Ptr := new Aida.String_T'(Item);
      begin
         This.My_Success_Codes.Append (R);
      end Append_Success_Code;

      procedure Append_Error_Code (This : in out T;
                                   Item : Aida.String_T)
      is
         R : String_Ptr := new Aida.String_T'(Item);
      begin
         This.My_Error_Codes.Append (R);
      end Append_Error_Code;

      procedure Append_Child (This  : in out T;
                              Item  : Child_T)
      is
      begin
         This.My_Children.Append (Item);
      end Append_Child;

      procedure Append_Queue (This : in out T;
                              Item : Queue_T)
      is
      begin
         This.My_Queues.Append (Item);
      end Append_Queue;

      procedure Append_Render_Pass (This : in out T;
                                    Item : Render_Pass_T)
      is
      begin
         This.My_Render_Passes.Append (Item);
      end Append_Render_Pass;

      procedure Append_Command_Buffer_Level (This : in out T;
                                             Item : Command_Buffer_Level_T)
      is
      begin
         This.My_Command_Buffer_Levels.Append (Item);
      end Append_Command_Buffer_Level;

      function To_String (This : T) return String is
         R : Ada.Strings.Unbounded.Unbounded_String;

         procedure Handle_Proto (Proto_V : Proto_Tag.Ptr) is

            procedure Handle_Nested_Type (Nested_Type_V : Nested_Type_Tag.Ptr) is
            begin
               Append (R, "<type>");

               if Nested_Type_V.Exists_Value then
                  Append (R, String (Nested_Type_V.Value));
               end if;

               Append (R, "</type>");
            end Handle_Nested_Type;

            procedure Handle_Name (Name_V : Name_Tag.Ptr) is
            begin
               Append (R, "<name>");
               Append (R, String (Name_V.Value));
               Append (R, "</name>");
            end Handle_Name;

         begin
            for Child of Proto_V.Children loop
               case Child.Kind_Id is
               when Child_Nested_Type => Handle_Nested_Type (Child.Nested_Type);
               when Child_Name        => Handle_Name (Child.Name);
               end case;
            end loop;
         end Handle_Proto;

      begin
         Append (R, "<command >");
         for Child of This.Children loop
            case Child.Kind_Id is
            when Child_Proto => Handle_Proto (Child.Proto);
            when Child_Param => null;
            when Child_Validity => null;
            when Child_Implicit_External_Sync_Parameters => null;
            end case;
         end loop;
         Append (R, "</command>");

         return To_String (R);
      end To_String;

   end Command_Tag;

   package body Commands_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Commands_Tag;

   package body Require_Enum_Tag is

      procedure Set_Bit_Position (This : in out T;
                                  Value : Bit_Position_T)
      is
      begin
         This.My_Bit_Position := (Exists => True,
                                  Value  => Value);
      end Set_Bit_Position;

      procedure Set_Extends (This : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Extends := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Extends;

      procedure Set_Comment (This : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Comment := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Comment;

      procedure Set_Dir (This : in out T;
                         Value : Aida.String_T)
      is
      begin
         This.My_Dir := (Exists => True,
                         Value  => new Aida.String_T'(Value));
      end Set_Dir;

      procedure Set_Offset (This : in out T;
                            Value : Offset_T)
      is
      begin
         This.My_Offset := (Exists => True,
                            Value  => Value);
      end Set_Offset;

      procedure Set_Value (This : in out T;
                           Value : Aida.String_T)
      is
      begin
         This.My_Value := (Exists => True,
                           Value  => new Aida.String_T'(Value));
      end Set_Value;

      procedure Set_Name (This : in out T;
                          Value : Aida.String_T)
      is
      begin
         This.My_Name := (Exists => True,
                          Value  => new Aida.String_T'(Value));
      end Set_Name;

   end Require_Enum_Tag;

   package body Require_Command_Tag is

      procedure Set_Name (This  : in out T;
                          Value : Aida.String_T)
      is
      begin
         This.My_Name := (Exists => True,
                          Value  => new Aida.String_T'(Value));
      end Set_Name;

   end Require_Command_Tag;

   package body Require_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

      procedure Set_Comment (This : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Comment := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Comment;

   end Require_Tag;

   package body Feature_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

      procedure Set_Number (This : in out T;
                            Value : Aida.String_T)
      is
      begin
         This.My_Number := (Exists => True,
                            Value  => new Aida.String_T'(Value));
      end Set_Number;

      procedure Set_Name (This : in out T;
                          Value : Aida.String_T)
      is
      begin
         This.My_Name := (Exists => True,
                          Value  => new Aida.String_T'(Value));
      end Set_Name;

      procedure Set_API (This : in out T;
                         Value : Aida.String_T)
      is
      begin
         This.My_API := (Exists => True,
                         Value  => new Aida.String_T'(Value));
      end Set_API;

   end Feature_Tag;

   package body Extension_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

      procedure Set_Contact (This : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Contact := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Contact;

      procedure Set_Author (This : in out T;
                            Value : Aida.String_T)
      is
      begin
         This.My_Author := (Exists => True,
                            Value  => new Aida.String_T'(Value));
      end Set_Author;

      procedure Set_Protect (This : in out T;
                             Value : Aida.String_T)
      is
      begin
         This.My_Protect := (Exists => True,
                             Value  => new Aida.String_T'(Value));
      end Set_Protect;

      procedure Set_Supported (This : in out T;
                               Value : Supported_T)
      is
      begin
         This.My_Supported := (Exists => True,
                               Value  => Value);
      end Set_Supported;

      procedure Set_Number (This : in out T;
                            Value : Number_T)
      is
      begin
         This.My_Number := (Exists => True,
                            Value  => Value);
      end Set_Number;

      procedure Set_Name (This : in out T;
                          Value : Aida.String_T)
      is
      begin
         This.My_Name := (Exists => True,
                          Value  => new Aida.String_T'(Value));
      end Set_Name;

   end Extension_Tag;

   package body Extensions_Tag is

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Extensions_Tag;

   package body Registry_Tag is

      procedure Append_Child (This    : in out T;
                              Comment : not null Comment_Tag.Ptr)
      is
         Child : Child_T := (Kind_Id => Child_Comment,
                             Comment => Comment);
      begin
         This.My_Children.Append (Child);
      end Append_Child;

      procedure Append_Child (This  : in out T;
                              Child : Child_T)
      is
      begin
         This.My_Children.Append (Child);
      end Append_Child;

   end Registry_Tag;

end Generic_Vk_XML;

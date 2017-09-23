with Ada.Text_IO;
with GNAT.Source_Info;
with Ada.Strings.Hash;
with Ada.Exceptions;
with Ada.Containers.Hashed_Maps;
with Aida.XML;
with Aida.Generic_Subprogram_Call_Result;
with Aida.XML.Generic_Parse_XML_File;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Aida.Text_IO;

with Vk_XML.Command_Tag;
with Vk_XML.Commands_Tag;
with Vk_XML.Comment_Tag;
with Vk_XML.Enum_Tag;
with Vk_XML.Enums_Enum_Tag;
with Vk_XML.Enums_Tag;
with Vk_XML.Extension_Tag;
with Vk_XML.Extensions_Tag;
with Vk_XML.External_Sync_Parameter_Tag;
with Vk_XML.Feature_Tag;
with Vk_XML.Implicit_External_Sync_Parameters_Tag;
with Vk_XML.Member_Tag;
with Vk_XML.Name_Tag;
with Vk_XML.Nested_Type_Tag;
with Vk_XML.Param_Tag;
with Vk_XML.Proto_Tag;
with Vk_XML.Registry_Tag;
with Vk_XML.Require_Command_Tag;
with Vk_XML.Require_Enum_Tag;
with Vk_XML.Require_Tag;
with Vk_XML.Tag_Tag;
with Vk_XML.Tags_Tag;
with Vk_XML.Type_Tag;
with Vk_XML.Types_Tag;
with Vk_XML.Unused_Tag;
with Vk_XML.Usage_Tag;
with Vk_XML.Validity_Tag;
with Vk_XML.Vendor_Id_Tag;
with Vk_XML.Vendor_Ids_Tag;

pragma Elaborate_All (Aida.Generic_Subprogram_Call_Result);
pragma Elaborate_All (Aida.XML.Generic_Parse_XML_File);

package body Vk_XML_Reader is

   use all type Aida.String_T;
   use all type Aida.Int32_T;

   XML_Tag_Registry                                 : constant Aida.String_T := "registry";
   XML_Tag_Comment                                  : constant Aida.String_T := "comment";
   XML_Tag_Vendor_Ids                               : constant Aida.String_T := "vendorids";
   XML_Tag_Vendor_Id                                : constant Aida.String_T := "vendorid";
   XML_Tag_Vendor_Id_Attribute_Name                 : constant Aida.String_T := "name";
   XML_Tag_Vendor_Id_Attribute_Id                   : constant Aida.String_T := "id";
   XML_Tag_Vendor_Id_Attribute_Comment              : constant Aida.String_T := "comment";
   XML_Tag_Tags                                     : constant Aida.String_T := "tags";
   XML_Tag_Tag                                      : constant Aida.String_T := "tag";
   XML_Tag_Tag_Attribute_Name                       : constant Aida.String_T := "name";
   XML_Tag_Tag_Attribute_Author                     : constant Aida.String_T := "author";
   XML_Tag_Tag_Attribute_Contact                    : constant Aida.String_T := "contact";
   XML_Tag_Types                                    : constant Aida.String_T := "types";
   XML_Tag_Type                                     : constant Aida.String_T := "type";
   XML_Tag_Type_Attribute_Name                      : constant Aida.String_T := "name";
   XML_Tag_Type_Attribute_Category                  : constant Aida.String_T := "category";
   XML_Tag_Type_Attribute_Requires                  : constant Aida.String_T := "requires";
   XML_Tag_Type_Attribute_Parent                    : constant Aida.String_T := "parent";
   XML_Tag_Type_Attribute_Returned_Only             : constant Aida.String_T := "returnedonly";
   XML_Tag_Type_Attribute_Comment                   : constant Aida.String_T := "comment";
   XML_Tag_Name                                     : constant Aida.String_T := "name";
   XML_Tag_Member                                   : constant Aida.String_T := "member";
   XML_Tag_Member_Attribute_Optional                : constant Aida.String_T := "optional";
   XML_Tag_Member_Attribute_Len                     : constant Aida.String_T := "len";
   XML_Tag_Member_Attribute_No_Auto_Validity        : constant Aida.String_T := "noautovalidity";
   XML_Tag_Member_Attribute_Valid_Extension_Structs : constant Aida.String_T := "validextensionstructs";
   XML_Tag_Validity                                 : constant Aida.String_T := "validity";
   XML_Tag_Usage                                    : constant Aida.String_T := "usage";
   XML_Tag_Usage_Attribute_Command                  : constant Aida.String_T := "command";
   XML_Tag_Usage_Attribute_Struct                   : constant Aida.String_T := "struct";
   XML_Tag_Enum                                     : constant Aida.String_T := "enum";
   XML_Tag_Enums                                    : constant Aida.String_T := "enums";
   XML_Tag_Enums_Attribute_Name                     : constant Aida.String_T := "name";
   XML_Tag_Enums_Attribute_Comment                  : constant Aida.String_T := "comment";
   XML_Tag_Enums_Attribute_Type                     : constant Aida.String_T := "type";
   XML_Tag_Enums_Enum_Attribute_Value               : constant Aida.String_T := "value";
   XML_Tag_Enums_Enum_Attribute_Name                : constant Aida.String_T := "name";
   XML_Tag_Enums_Enum_Attribute_Comment             : constant Aida.String_T := "comment";
   XML_Tag_Enums_Enum_Attribute_Bit_Position        : constant Aida.String_T := "bitpos";
   XML_Tag_Unused                                   : constant Aida.String_T := "unused";
   XML_Tag_Unused_Attribute_Start                   : constant Aida.String_T := "start";
   XML_Tag_Commands                                 : constant Aida.String_T := "commands";
   XML_Tag_Command                                  : constant Aida.String_T := "command";
   XML_Tag_Command_Attribute_Success_Codes          : constant Aida.String_T := "successcodes";
   XML_Tag_Command_Attribute_Error_Codes            : constant Aida.String_T := "errorcodes";
   XML_Tag_Command_Attribute_Queues                 : constant Aida.String_T := "queues";
   XML_Tag_Command_Attribute_Render_Pass            : constant Aida.String_T := "renderpass";
   XML_Tag_Command_Attribute_Cmd_Buffer_Level       : constant Aida.String_T := "cmdbufferlevel";
   XML_Tag_Proto                                    : constant Aida.String_T := "proto";
   XML_Tag_Param                                    : constant Aida.String_T := "param";
   XML_Tag_Param_Attribute_Optional                 : constant Aida.String_T := "optional";
   XML_Tag_Param_Attribute_External_Sync            : constant Aida.String_T := "externsync";
   XML_Tag_Param_Attribute_Len                      : constant Aida.String_T := "len";
   XML_Tag_Param_Attribute_No_Auto_Validity         : constant Aida.String_T := "noautovalidity";
   XML_Tag_Implicit_External_Syns_Params            : constant Aida.String_T := "implicitexternsyncparams";
   XML_Tag_External_Sync_Parameter                  : constant Aida.String_T := "param";
   XML_Tag_Feature                                  : constant Aida.String_T := "feature";
   XML_Tag_Feature_Attribute_API                    : constant Aida.String_T := "api";
   XML_Tag_Feature_Attribute_Name                   : constant Aida.String_T := "name";
   XML_Tag_Feature_Attribute_Number                 : constant Aida.String_T := "number";
   XML_Tag_Require                                  : constant Aida.String_T := "require";
   XML_Tag_Require_Attribute_Comment                : constant Aida.String_T := "comment";
   XML_Tag_Require_Enum_Attribute_Name              : constant Aida.String_T := "name";
   XML_Tag_Require_Enum_Attribute_Value             : constant Aida.String_T := "value";
   XML_Tag_Require_Enum_Attribute_Offset            : constant Aida.String_T := "offset";
   XML_Tag_Require_Enum_Attribute_Dir               : constant Aida.String_T := "dir";
   XML_Tag_Require_Enum_Attribute_Extends           : constant Aida.String_T := "extends";
   XML_Tag_Require_Enum_Attribute_Comment           : constant Aida.String_T := "comment";
   XML_Tag_Require_Enum_Attribute_Bit_Position      : constant Aida.String_T := "bitpos";
   XML_Tag_Require_Command_Attribute_Name           : constant Aida.String_T := "name";
   XML_Tag_Extensions                               : constant Aida.String_T := "extensions";
   XML_Tag_Extension                                : constant Aida.String_T := "extension";
   XML_Tag_Extension_Attribute_Name                 : constant Aida.String_T := "name";
   XML_Tag_Extension_Attribute_Number               : constant Aida.String_T := "number";
   XML_Tag_Extension_Attribute_Supported            : constant Aida.String_T := "supported";
   XML_Tag_Extension_Attribute_Protect              : constant Aida.String_T := "protect";
   XML_Tag_Extension_Attribute_Author               : constant Aida.String_T := "author";
   XML_Tag_Extension_Attribute_Contact              : constant Aida.String_T := "contact";

   use all type Ada.Strings.Unbounded.Unbounded_String;
   use all type Aida.XML.Tag_Name_T;
   use all type Aida.XML.Tag_Name_Vectors.Vector;
   use all type Aida.XML.Subprogram_Call_Result.T;
   use all type Vk_XML.Registry_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Vendor_Ids_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Tags_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Types_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Type_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Member_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Validity_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Usage_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Enums_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Enums_Tag.Type_Attribue_T;
   use all type Vk_XML.Commands_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Command_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Command_Tag.Queue_T;
   use all type Vk_XML.Command_Tag.Render_Pass_T;
   use all type Vk_XML.Command_Tag.Command_Buffer_Level_T;
   use all type Vk_XML.Proto_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Param_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Implicit_External_Sync_Parameters_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Feature_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Require_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Extension_Tag.Child_Kind_Id_T;
   use all type Vk_XML.Extension_Tag.Supported_T;
   use all type Vk_XML.Extensions_Tag.Child_Kind_Id_T;
--     use all type Vk_XML.Vendor_Id_Tag.Name_T;
--     use all type Vk_XML.Vendor_Id_Tag.Id_T;
--     use all type Vk_XML.Vendor_Id_Tag.Comment_T;
--     use all type Vk_XML.Tag_Tag.Name_T;
--     use all type Vk_XML.Tag_Tag.Author_T;
--     use all type Vk_XML.Tag_Tag.Contact_T;
--     use all type Vk_XML.Type_Tag.Category_T;
--     use all type Vk_XML.Type_Tag.Returned_Only_T;
--     use all type Vk_XML.Member_Tag.No_Auto_Validity_T;
--     use all type Vk_XML.Member_Tag.Optional_T;
--     use all type Vk_XML.Command_Tag.Success_Code_T;
--     use all type Vk_XML.Command_Tag.Error_Code_T;
--     use all type Vk_XML.Param_Tag.Optional_T;
--     use all type Vk_XML.Comment.Value_T;
--     use all type Vk_XML.Name_Tag.Value_T;
--     use all type Vk_XML.Nested_Type_Tag.Nullable_Value_T;
--     use all type Vk_XML.Enum_Tag.Value_T;

   use type Ada.Containers.Count_Type;

   package Current_Tag_Def is

      type Id_T is mod 1000_000;

      package Tag_Id is

         type Enumeration_T is (
                                Registry,
                                Comment,
                                Vendor_Ids,
                                Vendor_Id,
                                Tags,
                                Tag,
                                Types,
                                Type_T,
                                Name,
                                Nested_Type,
                                Member,
                                Validity,
                                Usage,
                                Enum,
                                Enums,
                                Enums_Enum,
                                Unused,
                                Commands,
                                Command,
                                Proto,
                                Param,
                                Implicit_External_Sync_Parameters,
                                External_Sync_Parameter,
                                Feature,
                                Require,
                                Require_Enum,
                                Require_Command,
                                Extensions,
                                Extension
                               );
      end Tag_Id;

      use Tag_Id;

      type T (Kind_Id : Tag_Id.Enumeration_T := Registry) is record
         Id         : Id_T;
         Parent_Tag : Id_T;
         case Kind_Id is
            when Registry                          => Registry                  : Vk_XML.Registry_Tag.Ptr;
            when Comment                           => Comment                   : Vk_XML.Comment_Tag.Ptr;
            when Vendor_Ids                        => Vendor_Ids_V              : Vk_XML.Vendor_Ids_Tag.Ptr;
            when Vendor_Id                         => Vendor_Id_V               : Vk_XML.Vendor_Id_Tag.Ptr;
            when Tags                              => Tags_V                    : Vk_XML.Tags_Tag.Ptr;
            when Tag                               => Tag_V                     : Vk_XML.Tag_Tag.Ptr;
            when Types                             => Types_V                   : Vk_XML.Types_Tag.Ptr;
            when Type_T                            => Type_V                    : Vk_XML.Type_Tag.Ptr;
            when Name                              => Name_V                    : Vk_XML.Name_Tag.Ptr;
            when Nested_Type                       => Nested_Type_V             : Vk_XML.Nested_Type_Tag.Ptr;
            when Member                            => Member_V                  : Vk_XML.Member_Tag.Ptr;
            when Validity                          => Validity_V                : Vk_XML.Validity_Tag.Ptr;
            when Usage                             => Usage_V                   : Vk_XML.Usage_Tag.Ptr;
            when Enum                              => Enum_V                    : Vk_XML.Enum_Tag.Ptr;
            when Enums                             => Enums_V                   : Vk_XML.Enums_Tag.Ptr;
            when Enums_Enum                        => Enums_Enum_V              : Vk_XML.Enums_Enum_Tag.Ptr;
            when Unused                            => Unused_V                  : Vk_XML.Unused_Tag.Ptr;
            when Commands                          => Commands_V                : Vk_XML.Commands_Tag.Ptr;
            when Command                           => Command_V                 : Vk_XML.Command_Tag.Ptr;
            when Proto                             => Proto_V                   : Vk_XML.Proto_Tag.Ptr;
            when Param                             => Param_V                   : Vk_XML.Param_Tag.Ptr;
            when Implicit_External_Sync_Parameters => Parameters_V              : Vk_XML.Implicit_External_Sync_Parameters_Tag.Ptr;
            when External_Sync_Parameter           => External_Sync_Parameter_V : Vk_XML.External_Sync_Parameter_Tag.Ptr;
            when Feature                           => Feature_V                 : Vk_XML.Feature_Tag.Ptr;
            when Require                           => Require_V                 : Vk_XML.Require_Tag.Ptr;
            when Require_Enum                      => Require_Enum_V            : Vk_XML.Require_Enum_Tag.Ptr;
            when Require_Command                   => Require_Command_V         : Vk_XML.Require_Command_Tag.Ptr;
            when Extensions                        => Extensions_V              : Vk_XML.Extensions_Tag.Ptr;
            when Extension                         => Extension_V               : Vk_XML.Extension_Tag.Ptr;
         end case;
      end record;

      procedure Initialize (This : in out T);

      function Get_Id return Id_T;

   end Current_Tag_Def;

   package body Current_Tag_Def is

      use type Id_T;

      Id_Counter : Id_T;

      procedure Initialize (This : in out T) is
      begin
         This.Id := Id_Counter;
         Id_Counter := Id_Counter + 1;
      end Initialize;

      function Get_Id return Id_T is
      begin
         return R : Id_T := Id_Counter do
            Id_Counter := Id_Counter + 1;
         end return;
      end Get_Id;

   end Current_Tag_Def;

   subtype Current_Tag_T is Current_Tag_Def.T;

   use all type Current_Tag_Def.T;

   use all type Current_Tag_Def.Tag_Id.Enumeration_T;

   --     package Mutable_XML_Out_Commented_Message_Shared_Ptr is new Vk_XML.XML_Out_Commented_Message_Shared_Ptr.Mutable;
   --
   --     use all type Mutable_XML_Out_Commented_Message_Shared_Ptr.Mutable_T;

   --     package Mutable_XML_Text_Shared_Ptr is new Vk_XML.XML_Text.Mutable;
   --
   --     use all type Mutable_XML_Text_Shared_Ptr.Mutable_T;

   function Make_Current_Tag (Kind_Id    : Current_Tag_Def.Tag_Id.Enumeration_T;
                              Parent_Tag_Id : Current_Tag_Def.Id_T) return Current_Tag_T;

   function Make_Current_Tag (Kind_Id       : Current_Tag_Def.Tag_Id.Enumeration_T;
                              Parent_Tag_Id : Current_Tag_Def.Id_T) return Current_Tag_T
   is
      R : Current_Tag_T (Kind_Id);
   begin
      R.Parent_Tag := Parent_Tag_Id;
      return R;
   end Make_Current_Tag;

   function Hash (Parent_And_Self_Tags : Aida.XML.Tag_Name_Vectors.Vector) return Ada.Containers.Hash_Type is
      R : Ada.Containers.Hash_Type := 0;

      use type Ada.Containers.Hash_Type;
   begin
      if Is_Empty (Parent_And_Self_Tags) then
         return 0;
      end if;

      for I in Integer range First_Index (Parent_And_Self_Tags)..Last_Index (Parent_And_Self_Tags) loop
         R := R + Ada.Strings.Hash (To_String (Element (Parent_And_Self_Tags, I)));
      end loop;
      R := R mod Ada.Containers.Hash_Type (Natural'Last);
      return R;
   end Hash;

   package Current_Tag_To_Tags_Map_Type_Owner is new Ada.Containers.Hashed_Maps (Key_Type        => Aida.XML.Tag_Name_Vector_T,
                                                                                 Element_Type    => Current_Tag_T,
                                                                                 Hash            => Hash,
                                                                                 Equivalent_Keys => Aida.XML.Tag_Name_Vectors."=",
                                                                                 "="             => Current_Tag_Def."=");

   use type Current_Tag_To_Tags_Map_Type_Owner.Cursor;

   Parents_Including_Self_To_Current_Tag_Map : Current_Tag_To_Tags_Map_Type_Owner.Map;

   procedure Populate_Parents_Including_Self (Parents_Including_Self : in out Aida.XML.Tag_Name_Vectors.Vector;
                                              Parents                : Aida.XML.Tag_Name_Vectors.Vector;
                                              Tag_Name               : Aida.String_T)
   is
   begin
      for Parent of Parents loop
         Append (Container => Parents_Including_Self,
                 New_Item  => Parent);
      end loop;

      Append (Container => Parents_Including_Self,
              New_Item  => To_Unbounded_String (String (Tag_Name)));
   end Populate_Parents_Including_Self;

   type Find_Tag_Call_Result_T (Exists : Boolean := False) is
      record
         case Exists is
            when True  => Current_Tag_V : Current_Tag_T;
            when False => null;
         end case;
      end record;

   function To_String (Tags : Aida.XML.Tag_Name_Vector_T) return String is
      R : Ada.Strings.Unbounded.Unbounded_String;
   begin
      for Tag of Tags loop
         Append (R, To_String (Tag) & ", ");
      end loop;

      return To_String (R);
   end To_String;

   function Find_Tag (Key : Aida.XML.Tag_Name_Vector_T) return Find_Tag_Call_Result_T
   is
   begin
      declare
         Searched_For : Current_Tag_To_Tags_Map_Type_Owner.Cursor :=
           Current_Tag_To_Tags_Map_Type_Owner.Find (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                    Key       => Key);
      begin
         if Searched_For /= Current_Tag_To_Tags_Map_Type_Owner.No_Element then
            return (Exists => True,
                    Current_Tag_V => Current_Tag_To_Tags_Map_Type_Owner.Element (Parents_Including_Self_To_Current_Tag_Map, Key));
         else
            return (Exists => False);
         end if;
      end;
   end Find_Tag;

   procedure Parse (Contents    : Aida.String_T;
                    Registry    : not null Vk_XML.Registry_Tag.Ptr;
                    SH          : Dynamic_Pools.Subpool_Handle;
                    Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
   is
      procedure Start_Tag (Tag_Name    : Aida.String_T;
                           Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                           Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
        Global => null;

      procedure Start_Tag (Tag_Name    : Aida.String_T;
                           Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                           Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Parents_Including_Self : Aida.XML.Tag_Name_Vector_T;

         CR : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags);
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         if not CR.Exists then
            if Length (Parent_Tags) > 0 then
               Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Could not find parent of ");
            else
               if Tag_Name = XML_Tag_Registry then
                  declare
                     Current_Tag_V : Current_Tag_T (Current_Tag_Def.Tag_Id.Registry);
                  begin
                     Current_Tag_V.Registry := Registry;

                     Initialize (This => Current_Tag_V);

                     Parents_Including_Self_To_Current_Tag_Map.Insert (Parents_Including_Self, Current_Tag_V);
                  end;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Expected ");
               end if;
            end if;
         else
            declare
               Prev_Tag : Current_Tag_T := CR.Current_Tag_V;
            begin
               case Prev_Tag.Kind_Id is
               when Current_Tag_Def.Tag_Id.Registry =>
                  if Tag_Name = XML_Tag_Comment then
                     declare
                        Comment : not null Vk_XML.Comment_Tag.Ptr := new (SH) Vk_XML.Comment_Tag.T;

                        Temp_Tag : Current_Tag_T := (Kind_Id    => Current_Tag_Def.Tag_Id.Comment,
                                                     Parent_Tag => Prev_Tag.Id,
                                                     Comment    => Comment,
                                                     Id         => Current_Tag_Def.Get_Id);
                     begin
                        Prev_Tag.Registry.Append_Child (Comment);

                        Parents_Including_Self_To_Current_Tag_Map.Insert (Key       => Parents_Including_Self,
                                                                          New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Vendor_Ids then
                     declare
                        Vendor_Ids_V : not null Vk_XML.Vendor_Ids_Tag.Ptr := new (SH) Vk_XML.Vendor_Ids_Tag.T;
                        Child : Vk_XML.Registry_Tag.Child_T := (Kind_Id     => Child_Vendor_Ids,
                                                                Vendor_Ids  => Vendor_Ids_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Vendor_Ids);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Vendor_Ids_V := Vendor_Ids_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Registry.Append_Child (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Tags then
                     declare
                        Tags_V : not null Vk_XML.Tags_Tag.Ptr := new (SH) Vk_XML.Tags_Tag.T;
                        Child : Vk_XML.Registry_Tag.Child_T := (Kind_Id => Child_Tags,
                                                                Tags  => Tags_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Tags);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Tags_V := Tags_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Registry.Append_Child (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Types then
                     declare
                        Types_V : not null Vk_XML.Types_Tag.Ptr := new (SH) Vk_XML.Types_Tag.T;
                        Child : Vk_XML.Registry_Tag.Child_T := (Kind_Id => Child_Types,
                                                                Types   => Types_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Types);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Types_V := Types_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Registry.Append_Child (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Enums then
                     declare
                        Enums_V : not null Vk_XML.Enums_Tag.Ptr := new (SH) Vk_XML.Enums_Tag.T;
                        Child : Vk_XML.Registry_Tag.Child_T := (Kind_Id => Child_Enums,
                                                                Enums  => Enums_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Enums);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Enums_V := Enums_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Registry.Append_Child (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Commands then
                     declare
                        Commands_V : not null Vk_XML.Commands_Tag.Ptr := new (SH) Vk_XML.Commands_Tag.T;
                        Child : Vk_XML.Registry_Tag.Child_T := (Kind_Id    => Child_Commands,
                                                                Commands => Commands_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Commands);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Commands_V := Commands_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Registry.Append_Child (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Feature then
                     declare
                        Feature_V : not null Vk_XML.Feature_Tag.Ptr := new (SH) Vk_XML.Feature_Tag.T;
                        Child : Vk_XML.Registry_Tag.Child_T := (Kind_Id   => Child_Feature,
                                                                Feature => Feature_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Feature);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Feature_V := Feature_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Registry.Append_Child (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Extensions then
                     declare
                        Extensions_V : not null Vk_XML.Extensions_Tag.Ptr := new (SH) Vk_XML.Extensions_Tag.T;
                        Child : Vk_XML.Registry_Tag.Child_T := (Kind_Id   => Child_Extensions,
                                                                Extensions => Extensions_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Extensions);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Extensions_V := Extensions_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Registry.Append_Child (Child);
                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Vendor_Ids =>
                  if Tag_Name = XML_Tag_Vendor_Id then
                     declare
                        Vendor_Id_V : not null Vk_XML.Vendor_Id_Tag.Ptr := new (SH) Vk_XML.Vendor_Id_Tag.T;
                        Child : Vk_XML.Vendor_Ids_Tag.Child_T := (Kind_Id   => Child_Vendor_Id,
                                                                  Vendor_Id => Vendor_Id_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Vendor_Id);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Vendor_Id_V := Vendor_Id_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Vendor_Ids_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Tags =>
                  if Tag_Name = XML_Tag_Tag then
                     declare
                        Tag_V : not null Vk_XML.Tag_Tag.Ptr := new (SH) Vk_XML.Tag_Tag.T;
                        Child : Vk_XML.Tags_Tag.Child_T := (Kind_Id => Child_Tag,
                                                            Tag     => Tag_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Tag);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Tag_V := Tag_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Tags_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Types =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Type_V : not null Vk_XML.Type_Tag.Ptr := new (SH) Vk_XML.Type_Tag.T;
                        Child : Vk_XML.Types_Tag.Child_T := (Kind_Id => Child_Type,
                                                             Type_V  => Type_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Type_T);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Type_V := Type_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Types_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Type_T =>
                  if Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : not null Vk_XML.Name_Tag.Ptr := new (SH) Vk_XML.Name_Tag.T;
                        Child : Vk_XML.Type_Tag.Child_T := (Kind_Id => Child_Name,
                                                            Name    => Name_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Type_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Type then
                     declare
                        Nested_Type_V : not null Vk_XML.Nested_Type_Tag.Ptr := new (SH) Vk_XML.Nested_Type_Tag.T;
                        Child : Vk_XML.Type_Tag.Child_T := (Kind_Id     => Child_Nested_Type,
                                                            Nested_Type => Nested_Type_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Type_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Member then
                     declare
                        Member_V : not null Vk_XML.Member_Tag.Ptr := new (SH) Vk_XML.Member_Tag.T;
                        Child : Vk_XML.Type_Tag.Child_T := (Kind_Id => Child_Member,
                                                            Member  => Member_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Member);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Member_V := Member_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Type_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Validity then
                     declare
                        Validity_V : not null Vk_XML.Validity_Tag.Ptr := new (SH) Vk_XML.Validity_Tag.T;
                        Child : Vk_XML.Type_Tag.Child_T := (Kind_Id  => Child_Validity,
                                                            Validity => Validity_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Validity);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Validity_V := Validity_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Type_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Member =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Nested_Type_V : not null Vk_XML.Nested_Type_Tag.Ptr := new (SH) Vk_XML.Nested_Type_Tag.T;
                        Child : Vk_XML.Member_Tag.Child_T := (Kind_Id     => Child_Nested_Type,
                                                              Nested_Type => Nested_Type_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Member_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : not null Vk_XML.Name_Tag.Ptr := new (SH) Vk_XML.Name_Tag.T;
                        Child : Vk_XML.Member_Tag.Child_T := (Kind_Id => Child_Name,
                                                              Name  => Name_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Member_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Enum then
                     declare
                        Enum_V : not null Vk_XML.Enum_Tag.Ptr := new (SH) Vk_XML.Enum_Tag.T;
                        Child : Vk_XML.Member_Tag.Child_T := (Kind_Id => Child_Enum,
                                                              Enum  => Enum_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Enum);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Enum_V := Enum_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Member_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Validity =>
                  if Tag_Name = XML_Tag_Usage then
                     declare
                        Usage_V : not null Vk_XML.Usage_Tag.Ptr := new (SH) Vk_XML.Usage_Tag.T;
                        Child : Vk_XML.Validity_Tag.Child_T := (Kind_Id => Child_Usage,
                                                                Usage   => Usage_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Usage);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Usage_V := Usage_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Validity_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Enums =>
                  if Tag_Name = XML_Tag_Enum then
                     declare
                        Enums_Enum_V : not null Vk_XML.Enums_Enum_Tag.Ptr := new (SH) Vk_XML.Enums_Enum_Tag.T;
                        Child : Vk_XML.Enums_Tag.Child_T := (Kind_Id    => Child_Enums_Enum,
                                                             Enums_Enum => Enums_Enum_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Enums_Enum);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Enums_Enum_V := Enums_Enum_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Enums_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Unused then
                     declare
                        Unused_V : not null Vk_XML.Unused_Tag.Ptr := new (SH) Vk_XML.Unused_Tag.T;
                        Child : Vk_XML.Enums_Tag.Child_T := (Kind_Id => Child_Unused,
                                                             Unused  => Unused_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Unused);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Unused_V := Unused_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Enums_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Commands =>
                  if Tag_Name = XML_Tag_Command then
                     declare
                        Command_V : not null Vk_XML.Command_Tag.Ptr := new (SH) Vk_XML.Command_Tag.T;
                        Child : Vk_XML.Commands_Tag.Child_T := (Kind_Id => Child_Command,
                                                                Command => Command_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Command);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Command_V := Command_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Commands_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Command =>
                  if Tag_Name = XML_Tag_Proto then
                     declare
                        Proto_V : not null Vk_XML.Proto_Tag.Ptr := new (SH) Vk_XML.Proto_Tag.T;
                        Child : Vk_XML.Command_Tag.Child_T := (Kind_Id => Child_Proto,
                                                               Proto   => Proto_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Proto);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Proto_V := Proto_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Command_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Param then
                     declare
                        Param_V : not null Vk_XML.Param_Tag.Ptr := new (SH) Vk_XML.Param_Tag.T;
                        Child : Vk_XML.Command_Tag.Child_T := (Kind_Id => Child_Param,
                                                               Param   => Param_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Param);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Param_V := Param_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Command_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Validity then
                     declare
                        Validity_V : not null Vk_XML.Validity_Tag.Ptr := new Vk_XML.Validity_Tag.T;
                        Child : Vk_XML.Command_Tag.Child_T := (Kind_Id => Child_Validity,
                                                               Validity => Validity_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Validity);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Validity_V := Validity_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Command_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Implicit_External_Syns_Params then
                     declare
                        Parameters_V : not null Vk_XML.Implicit_External_Sync_Parameters_Tag.Ptr := new (SH) Vk_XML.Implicit_External_Sync_Parameters_Tag.T;
                        Child : Vk_XML.Command_Tag.Child_T := (Kind_Id      => Child_Implicit_External_Sync_Parameters,
                                                               Parameters => Parameters_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Implicit_External_Sync_Parameters);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Parameters_V := Parameters_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Command_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Proto =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Nested_Type_V : not null Vk_XML.Nested_Type_Tag.Ptr := new (SH) Vk_XML.Nested_Type_Tag.T;
                        Child : Vk_XML.Proto_Tag.Child_T := (Kind_Id     => Child_Nested_Type,
                                                             Nested_Type => Nested_Type_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Proto_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : not null Vk_XML.Name_Tag.Ptr := new (SH) Vk_XML.Name_Tag.T;
                        Child : Vk_XML.Proto_Tag.Child_T := (Kind_Id       => Child_Name,
                                                             Name => Name_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Proto_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Param =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Nested_Type_V : not null Vk_XML.Nested_Type_Tag.Ptr := new (SH) Vk_XML.Nested_Type_Tag.T;
                        Child : Vk_XML.Param_Tag.Child_T := (Kind_Id     => Child_Nested_Type,
                                                             Nested_Type => Nested_Type_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Nested_Type);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Nested_Type_V := Nested_Type_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Param_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Name then
                     declare
                        Name_V : not null Vk_XML.Name_Tag.Ptr := new (SH) Vk_XML.Name_Tag.T;
                        Child : Vk_XML.Param_Tag.Child_T := (Kind_Id => Child_Name,
                                                             Name    => Name_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Name);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Name_V := Name_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Param_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Implicit_External_Sync_Parameters =>
                  if Tag_Name = XML_Tag_External_Sync_Parameter then
                     declare
                        External_Sync_Parameter_V : not null Vk_XML.External_Sync_Parameter_Tag.Ptr := new (SH) Vk_XML.External_Sync_Parameter_Tag.T;
                        Child : Vk_XML.Implicit_External_Sync_Parameters_Tag.Child_T := (Kind_Id                 => Child_External_Sync_Parameter,
                                                                                         External_Sync_Parameter => External_Sync_Parameter_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.External_Sync_Parameter);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.External_Sync_Parameter_V := External_Sync_Parameter_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Parameters_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Feature =>
                  if Tag_Name = XML_Tag_Require then
                     declare
                        Require_V : not null Vk_XML.Require_Tag.Ptr := new (SH) Vk_XML.Require_Tag.T;
                        Child : Vk_XML.Feature_Tag.Child_T := (Kind_Id   => Child_Require,
                                                               Require => Require_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Require);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_V := Require_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Feature_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Require =>
                  if Tag_Name = XML_Tag_Type then
                     declare
                        Type_V : not null Vk_XML.Type_Tag.Ptr := new (SH) Vk_XML.Type_Tag.T;
                        Child : Vk_XML.Require_Tag.Child_T := (Kind_Id => Child_Type,
                                                            Type_V  => Type_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Type_T);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Type_V := Type_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Require_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Enum then
                     declare
                        Enum_V : not null Vk_XML.Require_Enum_Tag.Ptr := new (SH) Vk_XML.Require_Enum_Tag.T;
                        Child : Vk_XML.Require_Tag.Child_T := (Kind_Id => Child_Enum,
                                                               Enum    => Enum_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Require_Enum);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_Enum_V := Enum_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Require_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Command then
                     declare
                        Command_V : not null Vk_XML.Require_Command_Tag.Ptr := new (SH) Vk_XML.Require_Command_Tag.T;
                        Child : Vk_XML.Require_Tag.Child_T := (Kind_Id => Child_Command,
                                                               Command => Command_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Require_Command);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_Command_V := Command_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Require_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  elsif Tag_Name = XML_Tag_Usage then
                     declare
                        Usage_V : not null Vk_XML.Usage_Tag.Ptr := new (SH) Vk_XML.Usage_Tag.T;
                        Child : Vk_XML.Require_Tag.Child_T := (Kind_Id => Child_Usage,
                                                               Usage  => Usage_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Usage);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Usage_V := Usage_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Require_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Extensions =>
                  if Tag_Name = XML_Tag_Extension then
                     declare
                        Extension_V : not null Vk_XML.Extension_Tag.Ptr := new (SH) Vk_XML.Extension_Tag.T;
                        Child : Vk_XML.Extensions_Tag.Child_T := (Kind_Id   => Child_Extension,
                                                                  Extension => Extension_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Extension);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Extension_V := Extension_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Extensions_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Extension =>
                  if Tag_Name = XML_Tag_Require then
                     declare
                        Require_V : not null Vk_XML.Require_Tag.Ptr := new (SH) Vk_XML.Require_Tag.T;
                        Child : Vk_XML.Extension_Tag.Child_T := (Kind_Id   => Child_Require,
                                                                 Require => Require_V);

                        Temp_Tag : Current_Tag_T (Current_Tag_Def.Tag_Id.Require);
                     begin
                        Temp_Tag.Parent_Tag := Prev_Tag.Id;
                        Temp_Tag.Require_V := Require_V;

                        Initialize (Temp_Tag);

                        Prev_Tag.Extension_V.Append_Child (Child);

                        Current_Tag_To_Tags_Map_Type_Owner.Insert (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                   Key       => Parents_Including_Self,
                                                                   New_Item  => Temp_Tag);
                     end;
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
                  end if;
               when Current_Tag_Def.Tag_Id.Comment |
                    Current_Tag_Def.Tag_Id.Vendor_Id |
                    Current_Tag_Def.Tag_Id.Tag |
                    Current_Tag_Def.Tag_Id.Name |
                    Current_Tag_Def.Tag_Id.Nested_Type |
                    Current_Tag_Def.Tag_Id.Usage |
                    Current_Tag_Def.Tag_Id.Enum |
                    Current_Tag_Def.Tag_Id.Enums_Enum |
                    Current_Tag_Def.Tag_Id.Unused |
                    Current_Tag_Def.Tag_Id.External_Sync_Parameter |
                    Current_Tag_Def.Tag_Id.Require_Enum |
                    Current_Tag_Def.Tag_Id.Require_Command =>
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Found unexpected start tag ");
               end case;
            end;
         end if;
      end Start_Tag;

      procedure Attribute (Attribute_Name              : Aida.String_T;
                           Attribute_Value             : Aida.String_T;
                           Parent_Tags_And_Current_Tag : Aida.XML.Tag_Name_Vector_T;
                           Call_Result                 : in out Aida.XML.Subprogram_Call_Result.T) with
        Global => null;

      procedure Attribute (Attribute_Name              : Aida.String_T;
                           Attribute_Value             : Aida.String_T;
                           Parent_Tags_And_Current_Tag : Aida.XML.Tag_Name_Vector_T;
                           Call_Result                 : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Searched_For : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags_And_Current_Tag);
      begin
         if not Searched_For.Exists then
            Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", attribute name ");
            return;
         end if;

         declare
            Current_Tag_V : Current_Tag_T := Searched_For.Current_Tag_V;
         begin
            case Current_Tag_V.Kind_Id is
            when Current_Tag_Def.Tag_Id.Vendor_Id =>
               if Attribute_Name = XML_Tag_Vendor_Id_Attribute_Name then
                  Current_Tag_V.Vendor_Id_V.Set_Name (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Vendor_Id_Attribute_Id then
                  Current_Tag_V.Vendor_Id_V.Set_Id (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Vendor_Id_Attribute_Comment then
                  Current_Tag_V.Vendor_Id_V.Set_Comment (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Tag =>
               if Attribute_Name = XML_Tag_Tag_Attribute_Name then
                  Current_Tag_V.Tag_V.Set_Name (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Tag_Attribute_Author then
                  Current_Tag_V.Tag_V.Set_Author (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Tag_Attribute_Contact then
                  Current_Tag_V.Tag_V.Set_Contact (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Type_T =>
               if Attribute_Name = XML_Tag_Type_Attribute_Name then
                  Current_Tag_V.Type_V.Set_Name (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Type_Attribute_Category then
                  Current_Tag_V.Type_V.Set_Category (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Type_Attribute_Requires then
                  Current_Tag_V.Type_V.Set_Requires (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Type_Attribute_Parent then
                  Current_Tag_V.Type_V.Set_Parent (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Type_Attribute_Returned_Only then
                  if Attribute_Value = "true" then
                     Current_Tag_V.Type_V.Set_Returned_Only (True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               elsif Attribute_Name = XML_Tag_Type_Attribute_Comment then
                  Current_Tag_V.Type_V.Set_Comment (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Member =>
               if Attribute_Name = XML_Tag_Member_Attribute_Optional then
                  if Attribute_Value = "true" then
                     Current_Tag_V.Member_V.Set_Optional (True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               elsif Attribute_Name = XML_Tag_Member_Attribute_Len then
                  Current_Tag_V.Member_V.Set_Len (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Member_Attribute_No_Auto_Validity then
                  if Attribute_Value = "true" then
                     Current_Tag_V.Member_V.Set_No_Auto_Validity (True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               elsif Attribute_Name = XML_Tag_Member_Attribute_Valid_Extension_Structs then
                  Current_Tag_V.Member_V.Set_Valid_Extension_Structs (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Enums =>
               if Attribute_Name = XML_Tag_Enums_Attribute_Name then
                  Current_Tag_V.Enums_V.Set_Name (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Enums_Attribute_Comment then
                  Current_Tag_V.Enums_V.Set_Comment (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Enums_Attribute_Type then
                  if Attribute_Value = "enum" then
                     Current_Tag_V.Enums_V.Set_Type_Attribute (Enum);
                  elsif Attribute_Value = "bitmask" then
                     Current_Tag_V.Enums_V.Set_Type_Attribute (Bit_Mask);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Enums_Enum =>
               if Attribute_Name = XML_Tag_Enums_Enum_Attribute_Value then
                  Current_Tag_V.Enums_Enum_V.Set_Value (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Enums_Enum_Attribute_Name then
                  Current_Tag_V.Enums_Enum_V.Set_Name (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Enums_Enum_Attribute_Comment then
                  Current_Tag_V.Enums_Enum_V.Set_Comment (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Enums_Enum_Attribute_Bit_Position then
                  declare
                     V : Aida.Int32_T;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Attribute_Value,
                               Target     => V,
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                     else
                        Current_Tag_V.Enums_Enum_V.Set_Bit_Position (V);
                     end if;
                  end;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Unused =>
               if Attribute_Name = XML_Tag_Unused_Attribute_Start then
                  Current_Tag_V.Unused_V.Set_Start (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Command =>
               if Attribute_Name = XML_Tag_Command_Attribute_Success_Codes then
                  Current_Tag_V.Command_V.Append_Success_Code (Attribute_Value);
                  -- TODO: Split the String into several success codes later
               elsif Attribute_Name = XML_Tag_Command_Attribute_Error_Codes then
                  Current_Tag_V.Command_V.Append_Error_Code (Attribute_Value);
                  -- TODO: Split the String into several error codes later
               elsif Attribute_Name = XML_Tag_Command_Attribute_Queues then
                  if Attribute_Value = "sparse_binding" then
                     Current_Tag_V.Command_V.Append_Queue (Sparse_Binding);
                  elsif Attribute_Value = "graphics,compute" then
                     Current_Tag_V.Command_V.Append_Queue (Graphics);
                     Current_Tag_V.Command_V.Append_Queue (Compute);
                  elsif Attribute_Value = "graphics" then
                     Current_Tag_V.Command_V.Append_Queue (Graphics);
                  elsif Attribute_Value = "compute" then
                     Current_Tag_V.Command_V.Append_Queue (Compute);
                  elsif Attribute_Value = "transfer,graphics,compute" then
                     Current_Tag_V.Command_V.Append_Queue (Graphics);
                     Current_Tag_V.Command_V.Append_Queue (Compute);
                     Current_Tag_V.Command_V.Append_Queue (Transfer);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               elsif Attribute_Name = XML_Tag_Command_Attribute_Render_Pass then
                  if Attribute_Value = "inside" then
                     Current_Tag_V.Command_V.Append_Render_Pass (Inside);
                  elsif Attribute_Value = "outside" then
                     Current_Tag_V.Command_V.Append_Render_Pass (Outside);
                  elsif Attribute_Value = "both" then
                     Current_Tag_V.Command_V.Append_Render_Pass (Both);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               elsif Attribute_Name = XML_Tag_Command_Attribute_Cmd_Buffer_Level then
                  if Attribute_Value = "primary,secondary" then
                     Current_Tag_V.Command_V.Append_Command_Buffer_Level (Primary);
                     Current_Tag_V.Command_V.Append_Command_Buffer_Level (Secondary);
                  elsif Attribute_Value = "primary" then
                     Current_Tag_V.Command_V.Append_Command_Buffer_Level (Primary);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Param =>
               if Attribute_Name = XML_Tag_Param_Attribute_Optional then
                  if Attribute_Value = "true" or Attribute_Value = "false,true" then
                     Current_Tag_V.Param_V.Set_Optional (True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               elsif Attribute_Name = XML_Tag_Param_Attribute_External_Sync then
                  Current_Tag_V.Param_V.Set_External_Sync (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Param_Attribute_Len then
                  Current_Tag_V.Param_V.Set_Len (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Param_Attribute_No_Auto_Validity then
                  if Attribute_Value = "true" then
                     Current_Tag_V.Param_V.Set_No_Auto_Validity (True);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Feature =>
               if Attribute_Name = XML_Tag_Feature_Attribute_API then
                  Current_Tag_V.Feature_V.Set_API (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Feature_Attribute_Name then
                  Current_Tag_V.Feature_V.Set_Name (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Feature_Attribute_Number then
                  Current_Tag_V.Feature_V.Set_Number (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Require =>
               if Attribute_Name = XML_Tag_Require_Attribute_Comment then
                  Current_Tag_V.Require_V.Set_Comment (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Require_Enum =>
               if Attribute_Name = XML_Tag_Require_Enum_Attribute_Name then
                  Current_Tag_V.Require_Enum_V.Set_Name (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Value then
                  Current_Tag_V.Require_Enum_V.Set_Value (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Offset then
                  declare
                     V : Vk_XML.Require_Enum_Tag.Offset_T;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Attribute_Value,
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                     else
                        Current_Tag_V.Require_Enum_V.Set_Offset (V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Dir then
                  Current_Tag_V.Require_Enum_V.Set_Dir (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Extends then
                  Current_Tag_V.Require_Enum_V.Set_Extends (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Comment then
                  Current_Tag_V.Require_Enum_V.Set_Comment (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Require_Enum_Attribute_Bit_Position then
                  declare
                     V : Vk_XML.Require_Enum_Tag.Bit_Position_T;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Attribute_Value,
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                     else
                        Current_Tag_V.Require_Enum_V.Set_Bit_Position (V);
                     end if;
                  end;
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Require_Command =>
               if Attribute_Name = XML_Tag_Require_Command_Attribute_Name then
                  Current_Tag_V.Require_Command_V.Set_Name (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Extension =>
               if Attribute_Name = XML_Tag_Extension_Attribute_Name then
                  Current_Tag_V.Extension_V.Set_Name (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Number then
                  declare
                     V : Vk_XML.Extension_Tag.Number_T;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Attribute_Value,
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                     else
                        Current_Tag_V.Extension_V.Set_Number (V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Supported then
                  if Attribute_Value = "vulkan" then
                     Current_Tag_V.Extension_V.Set_Supported (Vulkan);
                  elsif Attribute_Value = "disabled" then
                     Current_Tag_V.Extension_V.Set_Supported (Disabled);
                  else
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
                  end if;
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Protect then
                  Current_Tag_V.Extension_V.Set_Protect (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Author then
                  Current_Tag_V.Extension_V.Set_Author (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Extension_Attribute_Contact then
                  Current_Tag_V.Extension_V.Set_Contact (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Usage =>
               if Attribute_Name = XML_Tag_Usage_Attribute_Command then
                  Current_Tag_V.Usage_V.Set_Command (Attribute_Value, SH);
               elsif Attribute_Name = XML_Tag_Usage_Attribute_Struct then
                  Current_Tag_V.Usage_V.Set_Struct (Attribute_Value, SH);
               else
                  Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
               end if;
            when Current_Tag_Def.Tag_Id.Registry |
                 Current_Tag_Def.Tag_Id.Comment |
                 Current_Tag_Def.Tag_Id.Vendor_Ids |
                 Current_Tag_Def.Tag_Id.Tags |
                 Current_Tag_Def.Tag_Id.Types |
                 Current_Tag_Def.Tag_Id.Name |
                 Current_Tag_Def.Tag_Id.Nested_Type |
                 Current_Tag_Def.Tag_Id.Validity |
                 Current_Tag_Def.Tag_Id.Enum |
                 Current_Tag_Def.Tag_Id.Commands |
                 Current_Tag_Def.Tag_Id.Proto |
                 Current_Tag_Def.Tag_Id.Implicit_External_Sync_Parameters |
                 Current_Tag_Def.Tag_Id.External_Sync_Parameter |
                 Current_Tag_Def.Tag_Id.Extensions =>
               Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected attribute name ");
            end case;
         end;
      end Attribute;

      procedure End_Tag (Tag_Name    : Aida.String_T;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
        Global => null;

      procedure End_Tag (Tag_Name    : Aida.String_T;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Parents_Including_Self : Aida.XML.Tag_Name_Vector_T;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self => Parents_Including_Self,
                                          Parents                => Parent_Tags,
                                          Tag_Name               => Tag_Name);

         if
           Current_Tag_To_Tags_Map_Type_Owner.Contains (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                        Key       => Parents_Including_Self)
         then
            Current_Tag_To_Tags_Map_Type_Owner.Delete (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                       Key       => Parents_Including_Self);
         else
            Initialize (Call_Result, "Parents_Including_Self_To_Current_Tag_Map did not contain expected key for end tag ");
         end if;
      end End_Tag;

      procedure End_Tag (Tag_Name    : Aida.String_T;
                         Tag_Value   : Aida.String_T;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T) with
        Global => null;

      procedure End_Tag (Tag_Name    : Aida.String_T;
                         Tag_Value   : Aida.String_T;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Parents_Including_Self : Aida.XML.Tag_Name_Vector_T;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         declare
            Find_Tag_Call_Result : Find_Tag_Call_Result_T := Find_Tag (Parents_Including_Self);
         begin
            if Find_Tag_Call_Result.Exists then
               declare
                  Current_Tag_V : Current_Tag_T := Find_Tag_Call_Result.Current_Tag_V;
               begin

                  if
                    Current_Tag_To_Tags_Map_Type_Owner.Contains (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                 Key       => Parents_Including_Self)
                  then
                     Current_Tag_To_Tags_Map_Type_Owner.Delete (Container => Parents_Including_Self_To_Current_Tag_Map,
                                                                Key       => Parents_Including_Self);
                  else
                     Initialize (Call_Result, "Parents_Including_Self_To_Current_Tag_Map did not contain expected key for end tag ");
                  end if;

                  case Current_Tag_V.Kind_Id is
                  when Current_Tag_Def.Tag_Id.Comment =>
                     Current_Tag_V.Comment.Set_Value (Tag_Value, SH);
                  when Current_Tag_Def.Tag_Id.Type_T =>
                     declare
                        XML_Text_V : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Tag_Value);
                        Child : Vk_XML.Type_Tag.Child_T := (Kind_Id  => Child_XML_Text,
                                                            XML_Text => XML_Text_V);
                     begin
                        Current_Tag_V.Type_V.Append_Child (Child);
                     end;
                  when Current_Tag_Def.Tag_Id.Name =>
                     Current_Tag_V.Name_V.Set_Value (Tag_Value, SH);
                  when Current_Tag_Def.Tag_Id.Nested_Type =>
                     Current_Tag_V.Nested_Type_V.Set_Value (Tag_Value, SH);
                  when Current_Tag_Def.Tag_Id.Usage =>
                     declare
                        XML_Text_V : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Tag_Value);
                        Child : Vk_XML.Usage_Tag.Child_T := (Kind_Id  => Child_XML_Text,
                                                             XML_Text => XML_Text_V);
                     begin
                        Current_Tag_V.Usage_V.Append_Child (Child);
                     end;
                  when Current_Tag_Def.Tag_Id.Enum =>
                     Current_Tag_V.Enum_V.Set_Value (Tag_Value, SH);
                  when Current_Tag_Def.Tag_Id.External_Sync_Parameter =>
                     Current_Tag_V.External_Sync_Parameter_V.Set_Value (Tag_Value, SH);
                  when Current_Tag_Def.Tag_Id.Registry |
                       Current_Tag_Def.Tag_Id.Vendor_Ids |
                       Current_Tag_Def.Tag_Id.Vendor_Id |
                       Current_Tag_Def.Tag_Id.Tags |
                       Current_Tag_Def.Tag_Id.Tag |
                       Current_Tag_Def.Tag_Id.Types |
                       Current_Tag_Def.Tag_Id.Member |
                       Current_Tag_Def.Tag_Id.Validity |
                       Current_Tag_Def.Tag_Id.Enums |
                       Current_Tag_Def.Tag_Id.Enums_Enum |
                       Current_Tag_Def.Tag_Id.Unused |
                       Current_Tag_Def.Tag_Id.Commands |
                       Current_Tag_Def.Tag_Id.Command |
                       Current_Tag_Def.Tag_Id.Proto |
                       Current_Tag_Def.Tag_Id.Param |
                       Current_Tag_Def.Tag_Id.Implicit_External_Sync_Parameters |
                       Current_Tag_Def.Tag_Id.Feature |
                       Current_Tag_Def.Tag_Id.Require |
                       Current_Tag_Def.Tag_Id.Require_Enum |
                       Current_Tag_Def.Tag_Id.Require_Command |
                       Current_Tag_Def.Tag_Id.Extensions |
                       Current_Tag_Def.Tag_Id.Extension =>
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected end tag '");
                  end case;
               end;
            else
               declare
                  Prev_Tag : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags);
               begin
                  if not Prev_Tag.Exists then
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & "Both Current_Tag and Prev_Tag was null for end tag '");
                     return;
                  end if;

                  case Prev_Tag.Current_Tag_V.Kind_Id is
                  when others =>
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", found unexpected end tag '");
                  end case;
               end;
            end if;
         end;
      end End_Tag;

      procedure Text (Value       : Aida.String_T;
                      Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                      Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Searched_For : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags);
      begin
         if Length (Parent_Tags) > 0 then
            if not Searched_For.Exists then
               Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", " & To_String (Parent_Tags));
               return;
            end if;

            if (for all I in Value'Range => Value (I) = ' ' or Value (I) = Ada.Characters.Latin_1.CR or Value (I) = Ada.Characters.Latin_1.LF) then
               null;
            else
               declare
                  Current_Tag_V : Current_Tag_T := Searched_For.Current_Tag_V;
               begin
                  case Current_Tag_V.Kind_Id is
                  when Current_Tag_Def.Tag_Id.Registry =>
                     declare
                        XML_Text_V : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                        Child : Vk_XML.Registry_Tag.Child_T := (Kind_Id  => Child_XML_Text,
                                                                XML_Text => XML_Text_V);
                     begin
                        Current_Tag_V.Registry.Append_Child (Child);
                     end;
                  when Current_Tag_Def.Tag_Id.Type_T =>
                     declare
                        XML_Text_V : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                        Child : Vk_XML.Type_Tag.Child_T := (Kind_Id  => Child_XML_Text,
                                                            XML_Text => XML_Text_V);
                     begin
                        Current_Tag_V.Type_V.Append_Child (Child);
                     end;
                  when Current_Tag_Def.Tag_Id.Member =>
                     declare
                        XML_Text_V : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                        Child : Vk_XML.Member_Tag.Child_T := (Kind_Id  => Child_XML_Text,
                                                              XML_Text => XML_Text_V);
                     begin
                        Current_Tag_V.Member_V.Append_Child (Child);
                     end;
                  when Current_Tag_Def.Tag_Id.Param =>
                     declare
                        XML_Text_V : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                        Child : Vk_XML.Param_Tag.Child_T := (Kind_Id  => Child_XML_Text,
                                                             XML_Text => XML_Text_V);
                     begin
                        Current_Tag_V.Param_V.Append_Child (Child);
                     end;
                  when others =>
                     Aida.Text_IO.Put ("Text:");
                     Aida.Text_IO.Put_Line (String (Value));
                     Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", does not have text, " & To_String (Parent_Tags));
                  end case;
               end;
            end if;
         end if;
      end Text;

      procedure Comment (Value       : Aida.String_T;
                         Parent_Tags : Aida.XML.Tag_Name_Vector_T;
                         Call_Result : in out Aida.XML.Subprogram_Call_Result.T)
      is
         Searched_For : Find_Tag_Call_Result_T := Find_Tag (Parent_Tags);
      begin
         if not Searched_For.Exists then
            Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", " & To_String (Parent_Tags));
            return;
         end if;

         declare
            Current_Tag_V : Current_Tag_T := Searched_For.Current_Tag_V;
         begin
            case Current_Tag_V.Kind_Id is
            when Current_Tag_Def.Tag_Id.Registry =>
               declare
                  Comment : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                  Child : Vk_XML.Registry_Tag.Child_T := (Kind_Id               => Child_Out_Commented_Message,
                                                          Out_Commented_Message => Comment);
               begin
                  Current_Tag_V.Registry.Append_Child (Child);
               end;
            when Current_Tag_Def.Tag_Id.Types =>
               declare
                  Comment : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                  Child : Vk_XML.Types_Tag.Child_T := (Kind_Id               => Child_Out_Commented_Message,
                                                       Out_Commented_Message => Comment);
               begin
                  Current_Tag_V.Types_V.Append_Child (Child);
               end;
            when Current_Tag_Def.Tag_Id.Type_T =>
               declare
                  Comment : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                  Child : Vk_XML.Type_Tag.Child_T := (Kind_Id               => Child_Out_Commented_Message,
                                                      Out_Commented_Message => Comment);
               begin
                  Current_Tag_V.Type_V.Append_Child (Child);
               end;
            when Current_Tag_Def.Tag_Id.Enums =>
               declare
                  Comment : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                  Child : Vk_XML.Enums_Tag.Child_T := (Kind_Id               => Child_Out_Commented_Message,
                                                       Out_Commented_Message => Comment);
               begin
                  Current_Tag_V.Enums_V.Append_Child (Child);
               end;
            when Current_Tag_Def.Tag_Id.Require =>
               declare
                  Comment : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                  Child : Vk_XML.Require_Tag.Child_T := (Kind_Id               => Child_Out_Commented_Message,
                                                         Out_Commented_Message => Comment);
               begin
                  Current_Tag_V.Require_V.Append_Child (Child);
               end;
            when Current_Tag_Def.Tag_Id.Extensions =>
               declare
                  Comment : not null Vk_XML.String_Ptr := new (SH) Aida.String_T'(Value);
                  Child : Vk_XML.Extensions_Tag.Child_T := (Kind_Id               => Child_Out_Commented_Message,
                                                            Out_Commented_Message => Comment);
               begin
                  Current_Tag_V.Extensions_V.Append_Child (Child);
               end;
            when Current_Tag_Def.Tag_Id.Comment |
                 Current_Tag_Def.Tag_Id.Vendor_Ids |
                 Current_Tag_Def.Tag_Id.Vendor_Id |
                 Current_Tag_Def.Tag_Id.Tags |
                 Current_Tag_Def.Tag_Id.Tag |
                 Current_Tag_Def.Tag_Id.Name |
                 Current_Tag_Def.Tag_Id.Nested_Type |
                 Current_Tag_Def.Tag_Id.Member |
                 Current_Tag_Def.Tag_Id.Validity |
                 Current_Tag_Def.Tag_Id.Usage |
                 Current_Tag_Def.Tag_Id.Enum |
                 Current_Tag_Def.Tag_Id.Enums_Enum |
                 Current_Tag_Def.Tag_Id.Unused |
                 Current_Tag_Def.Tag_Id.Commands |
                 Current_Tag_Def.Tag_Id.Command |
                 Current_Tag_Def.Tag_Id.Proto |
                 Current_Tag_Def.Tag_Id.Param |
                 Current_Tag_Def.Tag_Id.Implicit_External_Sync_Parameters |
                 Current_Tag_Def.Tag_Id.External_Sync_Parameter |
                 Current_Tag_Def.Tag_Id.Feature |
                 Current_Tag_Def.Tag_Id.Require_Enum |
                 Current_Tag_Def.Tag_Id.Require_Command |
                 Current_Tag_Def.Tag_Id.Extension =>
               Initialize (Call_Result, GNAT.Source_Info.Source_Location & ", does not have out commented comments, " & To_String (Parent_Tags));
            end case;
         end;
         --        Aida.Text_IO.Put ("Comment:");
         --        Aida.Text_IO.Put_Line (Value);
      end Comment;

      procedure Parse_XML_File is new Aida.XML.Generic_Parse_XML_File (Start_Tag,
                                                                       Attribute,
                                                                       Text,
                                                                       Comment,
                                                                       End_Tag,
                                                                       End_Tag);
   begin
      --        My_Registry := Registry;
      Parse_XML_File (Contents,
                      Call_Result);
   end Parse;

end Vk_XML_Reader;

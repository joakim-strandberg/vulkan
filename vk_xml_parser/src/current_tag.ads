with Vk_XML;

with Current_Tag_Fs;

package Current_Tag with SPARK_Mode is

   use Current_Tag_Fs.Tag_Id;

   type T (Kind_Id : Current_Tag_Fs.Tag_Id.Enumeration_T := Registry) is record
      Id         : Current_Tag_Fs.Id_T;
      Parent_Tag : Current_Tag_Fs.Id_T;
      case Kind_Id is
         when Registry                          => Registry                  : Vk_XML.Registry_Shared_Ptr.T;
         when Comment                           => Comment                   : Vk_XML.Comment_Shared_Ptr.T;
         when Vendor_Ids                        => Vendor_Ids_V              : Vk_XML.Vendor_Ids_Shared_Ptr.T;
         when Vendor_Id                         => Vendor_Id_V               : Vk_XML.Vendor_Id_Shared_Ptr.T;
         when Tags                              => Tags_V                    : Vk_XML.Tags_Shared_Ptr.T;
         when Tag                               => Tag_V                     : Vk_XML.Tag_Shared_Ptr.T;
         when Types                             => Types_V                   : Vk_XML.Types_Shared_Ptr.T;
         when Type_T                            => Type_V                    : Vk_XML.Type_Shared_Ptr.T;
         when Name                              => Name_V                    : Vk_XML.Name_Shared_Ptr.T;
         when Nested_Type                       => Nested_Type_V             : Vk_XML.Nested_Type_Shared_Ptr.T;
         when Member                            => Member_V                  : Vk_XML.Member_Shared_Ptr.T;
         when Validity                          => Validity_V                : Vk_XML.Validity_Shared_Ptr.T;
         when Usage                             => Usage_V                   : Vk_XML.Usage_Shared_Ptr.T;
         when Enum                              => Enum_V                    : Vk_XML.Enum_Shared_Ptr.T;
         when Enums                             => Enums_V                   : Vk_XML.Enums_Shared_Ptr.T;
         when Enums_Enum                        => Enums_Enum_V              : Vk_XML.Enums_Enum_Shared_Ptr.T;
         when Unused                            => Unused_V                  : Vk_XML.Unused_Shared_Ptr.T;
         when Commands                          => Commands_V                : Vk_XML.Commands_Shared_Ptr.T;
         when Command                           => Command_V                 : Vk_XML.Command_Shared_Ptr.T;
         when Proto                             => Proto_V                   : Vk_XML.Proto_Shared_Ptr.T;
         when Param                             => Param_V                   : Vk_XML.Param_Shared_Ptr.T;
         when Implicit_External_Sync_Parameters => Parameters_V              : Vk_XML.Implicit_External_Sync_Parameters_Shared_Ptr.T;
         when External_Sync_Parameter           => External_Sync_Parameter_V : Vk_XML.External_Sync_Parameter_Shared_Ptr.T;
         when Feature                           => Feature_V                 : Vk_XML.Feature_Shared_Ptr.T;
         when Require                           => Require_V                 : Vk_XML.Require_Shared_Ptr.T;
         when Require_Enum                      => Require_Enum_V            : Vk_XML.Require_Enum_Shared_Ptr.T;
         when Require_Command                   => Require_Command_V         : Vk_XML.Require_Command_Shared_Ptr.T;
         when Extensions                        => Extensions_V              : Vk_XML.Extensions_Shared_Ptr.T;
         when Extension                         => Extension_V               : Vk_XML.Extension_Shared_Ptr.T;
      end case;
   end record;

   procedure Initialize (This : in out T);

end Current_Tag;

with Vk_XML;

with Current_Tag_Fs;

package Current_Tag is

   use Current_Tag_Fs.Tag_Id;

   type T (Kind_Id : Current_Tag_Fs.Tag_Id.Enumeration_T := Registry) is record
      Id         : Current_Tag_Fs.Id_T;
      Parent_Tag : Current_Tag_Fs.Id_T;
      case Kind_Id is
         when Registry                          => Registry                  : access Vk_XML.Registry.T;
         when Comment                           => Comment                   : access Vk_XML.Comment.T;
         when Vendor_Ids                        => Vendor_Ids_V              : access Vk_XML.Vendor_Ids.T;
         when Vendor_Id                         => Vendor_Id_V               : access Vk_XML.Vendor_Id.T;
         when Tags                              => Tags_V                    : access Vk_XML.Tags.T;
         when Tag                               => Tag_V                     : access Vk_XML.Tag.T;
         when Types                             => Types_V                   : access Vk_XML.Types.T;
         when Type_T                            => Type_V                    : access Vk_XML.Type_T.T;
         when Name                              => Name_V                    : access Vk_XML.Name.T;
         when Nested_Type                       => Nested_Type_V             : access Vk_XML.Nested_Type.T;
         when Member                            => Member_V                  : access Vk_XML.Member.T;
         when Validity                          => Validity_V                : access Vk_XML.Validity.T;
         when Usage                             => Usage_V                   : access Vk_XML.Usage.T;
         when Enum                              => Enum_V                    : access Vk_XML.Enum.T;
         when Enums                             => Enums_V                   : access Vk_XML.Enums.T;
         when Enums_Enum                        => Enums_Enum_V              : access Vk_XML.Enums_Enum.T;
         when Unused                            => Unused_V                  : access Vk_XML.Unused.T;
         when Commands                          => Commands_V                : access Vk_XML.Commands.T;
         when Command                           => Command_V                 : access Vk_XML.Command.T;
         when Proto                             => Proto_V                   : access Vk_XML.Proto.T;
         when Param                             => Param_V                   : access Vk_XML.Param.T;
         when Implicit_External_Sync_Parameters => Parameters_V              : access Vk_XML.Implicit_External_Sync_Parameters.T;
         when External_Sync_Parameter           => External_Sync_Parameter_V : access Vk_XML.External_Sync_Parameter.T;
         when Feature                           => Feature_V                 : access Vk_XML.Feature.T;
         when Require                           => Require_V                 : access Vk_XML.Require.T;
         when Require_Enum                      => Require_Enum_V            : access Vk_XML.Require_Enum.T;
         when Require_Command                   => Require_Command_V         : access Vk_XML.Require_Command.T;
         when Extensions                        => Extensions_V              : access Vk_XML.Extensions.T;
         when Extension                         => Extension_V               : access Vk_XML.Extension.T;
      end case;
   end record;

   procedure Initialize (This : in out T);

end Current_Tag;

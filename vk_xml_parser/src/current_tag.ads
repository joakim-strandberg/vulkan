with Vk_XML2;

with Current_Tag_Fs;

package Current_Tag is

   use Current_Tag_Fs.Tag_Id;

   type T (Kind_Id : Current_Tag_Fs.Tag_Id.Enumeration_T := Registry) is record
      Id         : Current_Tag_Fs.Id_T;
      Parent_Tag : Current_Tag_Fs.Id_T;
      case Kind_Id is
         when Registry                          => Registry                  : not null access Vk_XML2.Registry.T;
         when Comment                           => Comment                   : not null access Vk_XML2.Comment.T;
         when Vendor_Ids                        => Vendor_Ids_V              : not null access Vk_XML2.Vendor_Ids.T;
         when Vendor_Id                         => Vendor_Id_V               : not null access Vk_XML2.Vendor_Id.T;
         when Tags                              => Tags_V                    : not null access Vk_XML2.Tags.T;
         when Tag                               => Tag_V                     : not null access Vk_XML2.Tag.T;
         when Types                             => Types_V                   : not null access Vk_XML2.Types.T;
         when Type_T                            => Type_V                    : not null access Vk_XML2.Type_T.T;
         when Name                              => Name_V                    : not null access Vk_XML2.Name.T;
         when Nested_Type                       => Nested_Type_V             : not null access Vk_XML2.Nested_Type.T;
         when Member                            => Member_V                  : not null access Vk_XML2.Member.T;
         when Validity                          => Validity_V                : not null access Vk_XML2.Validity.T;
         when Usage                             => Usage_V                   : not null access Vk_XML2.Usage.T;
         when Enum                              => Enum_V                    : not null access Vk_XML2.Enum.T;
         when Enums                             => Enums_V                   : not null access Vk_XML2.Enums.T;
         when Enums_Enum                        => Enums_Enum_V              : not null access Vk_XML2.Enums_Enum.T;
         when Unused                            => Unused_V                  : not null access Vk_XML2.Unused.T;
         when Commands                          => Commands_V                : not null access Vk_XML2.Commands.T;
         when Command                           => Command_V                 : not null access Vk_XML2.Command.T;
         when Proto                             => Proto_V                   : not null access Vk_XML2.Proto.T;
         when Param                             => Param_V                   : not null access Vk_XML2.Param.T;
         when Implicit_External_Sync_Parameters => Parameters_V              : not null access Vk_XML2.Implicit_External_Sync_Parameters.T;
         when External_Sync_Parameter           => External_Sync_Parameter_V : not null access Vk_XML2.External_Sync_Parameter.T;
         when Feature                           => Feature_V                 : not null access Vk_XML2.Feature.T;
         when Require                           => Require_V                 : not null access Vk_XML2.Require.T;
         when Require_Enum                      => Require_Enum_V            : not null access Vk_XML2.Require_Enum.T;
         when Require_Command                   => Require_Command_V         : not null access Vk_XML2.Require_Command.T;
         when Extensions                        => Extensions_V              : not null access Vk_XML2.Extensions.T;
         when Extension                         => Extension_V               : not null access Vk_XML2.Extension.T;
      end case;
   end record;

   procedure Initialize (This : in out T);

end Current_Tag;

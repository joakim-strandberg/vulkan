with Vk;

with Current_Tag_Fs;

package Current_Tag with SPARK_Mode is

   use Current_Tag_Fs.Tag_Id;

   type T (Kind_Id : Current_Tag_Fs.Tag_Id.Enumeration_T := Registry) is record
      Id         : Current_Tag_Fs.Id_T;
      Parent_Tag : Current_Tag_Fs.Id_T;
      case Kind_Id is
         when Registry                          => Registry                  : Vk.Registry_Shared_Ptr.T;
         when Comment                           => Comment                   : Vk.Comment_Shared_Ptr.T;
         when Vendor_Ids                        => Vendor_Ids_V              : Vk.Vendor_Ids_Shared_Ptr.T;
         when Vendor_Id                         => Vendor_Id_V               : Vk.Vendor_Id_Shared_Ptr.T;
         when Tags                              => Tags_V                    : Vk.Tags_Shared_Ptr.T;
         when Tag                               => Tag_V                     : Vk.Tag_Shared_Ptr.T;
         when Types                             => Types_V                   : Vk.Types_Shared_Ptr.T;
         when Type_T                            => Type_V                    : Vk.Type_Shared_Ptr.T;
         when Name                              => Name_V                    : Vk.Name_Shared_Ptr.T;
         when Nested_Type                       => Nested_Type_V             : Vk.Nested_Type_Shared_Ptr.T;
         when Member                            => Member_V                  : Vk.Member_Shared_Ptr.T;
         when Validity                          => Validity_V                : Vk.Validity_Shared_Ptr.T;
         when Usage                             => Usage_V                   : Vk.Usage_Shared_Ptr.T;
         when Enum                              => Enum_V                    : Vk.Enum_Shared_Ptr.T;
         when Enums                             => Enums_V                   : Vk.Enums_Shared_Ptr.T;
         when Enums_Enum                        => Enums_Enum_V              : Vk.Enums_Enum_Shared_Ptr.T;
         when Unused                            => Unused_V                  : Vk.Unused_Shared_Ptr.T;
         when Commands                          => Commands_V                : Vk.Commands_Shared_Ptr.T;
         when Command                           => Command_V                 : Vk.Command_Shared_Ptr.T;
         when Proto                             => Proto_V                   : Vk.Proto_Shared_Ptr.T;
         when Param                             => Param_V                   : Vk.Param_Shared_Ptr.T;
         when Implicit_External_Sync_Parameters => Parameters_V              : Vk.Implicit_External_Sync_Parameters_Shared_Ptr.T;
         when External_Sync_Parameter           => External_Sync_Parameter_V : Vk.External_Sync_Parameter_Shared_Ptr.T;
         when Feature                           => Feature_V                 : Vk.Feature_Shared_Ptr.T;
         when Require                           => Require_V                 : Vk.Require_Shared_Ptr.T;
      end case;
   end record;

   procedure Initialize (This : in out T);

end Current_Tag;

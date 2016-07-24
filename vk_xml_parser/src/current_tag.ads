with Vk;

with Current_Tag_Fs;

package Current_Tag with SPARK_Mode is

   use Current_Tag_Fs.Tag_Id;

   type T (Kind_Id : Current_Tag_Fs.Tag_Id.Enumeration_T := Registry) is record
      Id         : Current_Tag_Fs.Id_T;
      Parent_Tag : Current_Tag_Fs.Id_T;
      case Kind_Id is
         when Registry    => Registry      : Vk.Registry_Shared_Ptr.T;
         when Comment     => Comment       : Vk.Comment_Shared_Ptr.T;
         when Vendor_Ids  => Vendor_Ids_V  : Vk.Vendor_Ids_Shared_Ptr.T;
         when Vendor_Id   => Vendor_Id_V   : Vk.Vendor_Id_Shared_Ptr.T;
         when Tags        => Tags_V        : Vk.Tags_Shared_Ptr.T;
         when Tag         => Tag_V         : Vk.Tag_Shared_Ptr.T;
         when Types       => Types_V       : Vk.Types_Shared_Ptr.T;
         when Type_T      => Type_V        : Vk.Type_Shared_Ptr.T;
         when Name        => Name_V        : Vk.Name_Shared_Ptr.T;
         when Nested_Type => Nested_Type_V : Vk.Nested_Type_Shared_Ptr.T;
         when Member      => Member_V      : Vk.Member_Shared_Ptr.T;
         when Validity    => Validity_V    : Vk.Validity_Shared_Ptr.T;
         when Usage       => Usage_V       : Vk.Usage_Shared_Ptr.T;
         when Enum        => Enum_V        : Vk.Enum_Shared_Ptr.T;
      end case;
   end record;

   procedure Initialize (This : in out T);

end Current_Tag;

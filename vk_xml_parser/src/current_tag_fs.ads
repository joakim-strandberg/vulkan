package Current_Tag_Fs with SPARK_Mode is

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
                             Require_Command
                            );

   end Tag_Id;

end Current_Tag_Fs;

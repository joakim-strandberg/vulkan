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
                             Enum
                            );

   end Tag_Id;

end Current_Tag_Fs;

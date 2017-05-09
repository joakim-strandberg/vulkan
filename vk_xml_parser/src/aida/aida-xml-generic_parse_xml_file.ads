generic
   with procedure Start_Tag (Tag_Name    : String;
                             Parent_Tags : Tag_Name_Vector_T;
                             Call_Result : in out Subprogram_Call_Result.T);

   with procedure Attribute (Attribute_Name              : String;
                             Attribute_Value             : String;
                             Parent_Tags_And_Current_Tag : Tag_Name_Vector_T;
                             Call_Result                 : in out Subprogram_Call_Result.T);

   with procedure Text (Value       : String;
                        Parent_Tags : Tag_Name_Vector_T;
                        Call_Result : in out Subprogram_Call_Result.T);

   with procedure Comment (Value       : String;
                           Parent_Tags : Tag_Name_Vector_T;
                           Call_Result : in out Subprogram_Call_Result.T);

   with procedure End_Tag (Tag_Name    : String;
                           Parent_Tags : Tag_Name_Vector_T;
                           Call_Result : in out Subprogram_Call_Result.T);

   with procedure End_Tag (Tag_Name    : String;
                           Tag_Value   : String;
                           Parent_Tags : Tag_Name_Vector_T;
                           Call_Result : in out Subprogram_Call_Result.T);
procedure Aida.XML.Generic_Parse_XML_File (Contents      : String;
                                           Call_Result   : out Subprogram_Call_Result.T);

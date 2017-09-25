generic
   with procedure Start_Tag (Tag_Name    : Aida.String_T;
                             Parent_Tags : Tag_Name_Vector_T;
                             Call_Result : in out Subprogram_Call_Result.T);

   with procedure Attribute (Attribute_Name              : Aida.String_T;
                             Attribute_Value             : Aida.String_T;
                             Parent_Tags_And_Current_Tag : Tag_Name_Vector_T;
                             Call_Result                 : in out Subprogram_Call_Result.T);

   with procedure Text (Value       : Aida.String_T;
                        Parent_Tags : Tag_Name_Vector_T;
                        Call_Result : in out Subprogram_Call_Result.T);

   with procedure Comment (Value       : Aida.String_T;
                           Parent_Tags : Tag_Name_Vector_T;
                           Call_Result : in out Subprogram_Call_Result.T);

   with procedure End_Tag (Tag_Name    : Aida.String_T;
                           Parent_Tags : Tag_Name_Vector_T;
                           Call_Result : in out Subprogram_Call_Result.T);

   with procedure End_Tag (Tag_Name    : Aida.String_T;
                           Tag_Value   : Aida.String_T;
                           Parent_Tags : Tag_Name_Vector_T;
                           Call_Result : in out Subprogram_Call_Result.T);
procedure Aida.XML.Generic_Parse_XML_File (Contents      : Aida.String_T;
                                           Call_Result   : out Subprogram_Call_Result.T);

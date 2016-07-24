with Aida.UTF8;
with Ada.Characters.Latin_1;
    with Aida.Text_IO;

procedure Aida.XML.Generic_Parse_XML_File (Contents      : String;
                                           Call_Result   : out Subprogram_Call_Result.T) with SPARK_Mode => On
is
   use type Aida.UTF8.Code_Point;

   use Tag_Name;
   use Subprogram_Call_Result;

   Tag_Names : Tag_Name_Vector_T;

   type State_Id_Type is (
                          Searching_For_XML_Start_String,
                          Expecting_NL_Sign_Or_Space_Or_Less_Sign, -- NL = New Line
                          Found_Less_Sign,
                          Found_Less_Followed_By_Exclamation_Sign,
                          Found_Less_Followed_By_Exclamation_And_Dash_Sign,
                          Extracting_Start_Tag_Name,
                          Expecting_G_Sign_Or_Extracting_Attributes,
                          Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash,
                          Extracting_Attribute_Name,
                          Expecting_Attribute_Value_Quotation_Mark,
                          Extracting_Attribute_Value,
                          Expecting_New_Tag_Or_Extracting_Tag_Value, -- Or start of comment or start- tag or end-tag
                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L,
                          Extracting_End_Tag_Name,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash,
                          -- Enumeration values introduced to handle <!CDATA[--]]>
                          Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA,
                          Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket,
                          Extracting_CDATA,
                          Extracting_CDATA_Found_Square_Bracket,
                          Extracting_CDATA_Found_Two_Square_Brackets,
                          Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign,
                          Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign,
                          Extracting_Comment,
                          Extracting_Comment_And_Found_Dash,
                          Extracting_Comment_And_Found_Dash_Dash
                         );

   State_Id : State_Id_Type := Searching_For_XML_Start_String;

   XML_File_Start_String  : String := "<?xml version=""1.0"" encoding=""utf-8""?>";
   XML_File_Start_String2 : String := "<?xml version=""1.0"" encoding=""UTF-8""?>";

   F : Natural := Contents'First;
   L : Natural := Contents'Last;

   Shall_Ignore_Tag_Value_List : Boolean_Vectors.Vector (50);

   function Is_Special_Symbol (CP : Aida.UTF8.Code_Point) return Boolean is
   begin
      if CP = Character'Pos ('<') then
         return True;
      elsif CP = Character'Pos ('>') then
         return True;
      elsif CP = Character'Pos ('/') then
         return True;
      elsif CP = Character'Pos ('"') then
         return True;
      else
         return False;
      end if;
   end Is_Special_Symbol;

   procedure Append (This : in out Tag_Name_Vectors.Vector;
                     Item : String)
   is
      N : Mutable_Tag_Name.Mutable_T;
   begin
      Initialize (N, Item);
      Tag_Name_Vectors.Append (This, Tag_Name.T (N));
   end Append;

   type Expected_Quotation_Symbol_T is (
                                        Single_Quotes, -- Example: 'hello'
                                        Double_Quotes  -- Example: "hello"
                                       );
begin
   if
     Contents (F..F+XML_File_Start_String'Length-1) /= XML_File_Start_String and
     Contents (F..F+XML_File_Start_String'Length-1) /= XML_File_Start_String2
   then
      Initialize (This    => Call_Result,
                  Message => "File does not start with <?xml version...>");
      return;
   end if;

   State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

   declare
      P           : Integer := Aida.UTF8.Length (XML_File_Start_String) + 1;
      Prev_P      : Integer := P;
      Prev_Prev_P : Integer := Prev_P;

      CP      : Aida.UTF8.Code_Point := 0;

      Start_Tag_Name_First_Index : Integer;
      Start_Tag_Name_Last_Index  : Integer;

      Tag_Value_First_Index : Integer;
      Tag_Value_Last_Index  : Integer;

      End_Tag_Name_First_Index : Integer;
      End_Tag_Name_Last_Index  : Integer;

      Attribute_First_Index : Integer;
      Attribute_Last_Index  : Integer;

      Attribute_Value_First_Index : Integer;
      Attribute_Value_Last_Index  : Integer;

      Comment_First_Index : Integer;

      Shall_Ignore_Tag_Value : Boolean;

      Shall_Ignore_Until_Next_Quotation_Mark : Boolean := False;

      Expected_Quotation_Symbol : Expected_Quotation_Symbol_T := Double_Quotes;
   begin
      while P < Contents'Last loop
         Prev_Prev_P := Prev_P;

         Prev_P := P;

         if not Aida.UTF8.Is_Valid_UTF8_Code_Point (Source  => Contents,
                                                    Pointer => P)
         then
            Initialize (Call_Result, "Found invalid UTF-8 character.");
            return;
         end if;

         Aida.UTF8.Get (Source  => Contents,
                        Pointer => P,
                        Value   => CP);

--                     Aida.Text_IO.Put_Line ("Extracted:" & Aida.UTF8.Image (CP) & ", state " & State_Id_Type'Image (State_Id));
--                   Aida.Text_IO.Put (Aida.UTF8.Image (CP));

         case State_Id is
            when Searching_For_XML_Start_String =>
               Initialize (Call_Result, "It should not be possible to be in the state " & State_Id_Type'Image (State_Id));
               return;
            when Expecting_NL_Sign_Or_Space_Or_Less_Sign =>
--                 if CP = Character'Pos (Ada.Characters.Latin_1.LF) or CP = Character'Pos (' ') then
--                    null; -- Normal
--                 elsif CP = Character'Pos ('<') then
--                    State_Id := Found_Less_Sign;
--                 else
--                    Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Aida.UTF8.Code_Point'Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
--                    return;
--                 end if;
               if CP = Character'Pos ('<') then
                  State_Id := Found_Less_Sign;
               end if;
            when Found_Less_Sign =>
               if CP = Character'Pos ('!') then
                  State_Id := Found_Less_Followed_By_Exclamation_Sign;
               elsif CP = Character'Pos ('/') then
                  State_Id := Extracting_End_Tag_Name;
                  End_Tag_Name_First_Index := P;
                  Tag_Value_Last_Index := Prev_Prev_P - 1;
               elsif not Is_Special_Symbol (CP) then
                  State_Id := Extracting_Start_Tag_Name;
                  Start_Tag_Name_First_Index := Prev_P;
               else
                  Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Aida.UTF8.Code_Point'Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Found_Less_Followed_By_Exclamation_Sign =>
               if CP = Character'Pos ('-') then
                  State_Id := Found_Less_Followed_By_Exclamation_And_Dash_Sign;
               else
                  Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Aida.UTF8.Code_Point'Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Found_Less_Followed_By_Exclamation_And_Dash_Sign =>
               if CP = Character'Pos ('-') then
                  State_Id := Extracting_Comment;
               else
                  Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Aida.UTF8.Code_Point'Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Extracting_Start_Tag_Name =>
               if CP = Character'Pos (' ') then
                  Start_Tag_Name_Last_Index := Prev_Prev_P;
                  Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                             Tag_Names,
                             Call_Result);

                  if Has_Failed (Call_Result) then
                     return;
                  end if;

                  Append (Tag_Names, Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index));

                  Boolean_Vectors.Append (Shall_Ignore_Tag_Value_List, Shall_Ignore_Tag_Value);

                  Shall_Ignore_Tag_Value := False;

                  State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
               elsif CP = Character'Pos ('>') then
                  Start_Tag_Name_Last_Index := Prev_Prev_P;
                  Start_Tag (Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index),
                             Tag_Names,
                             Call_Result);

                  if Has_Failed (Call_Result) then
                     return;
                  end if;

                  Append (Tag_Names, Contents (Start_Tag_Name_First_Index..Start_Tag_Name_Last_Index));

                  Boolean_Vectors.Append (Shall_Ignore_Tag_Value_List, Shall_Ignore_Tag_Value);

                  Shall_Ignore_Tag_Value := False;
                  Tag_Value_First_Index := P;

                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               elsif Is_Special_Symbol (CP) then
                  Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Aida.UTF8.Code_Point'Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Expecting_G_Sign_Or_Extracting_Attributes =>
               if CP = Character'Pos (' ') or CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                  null; -- Normal
               elsif CP = Character'Pos ('>') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                  Tag_Value_First_Index := P;
               elsif CP = Character'Pos ('/') then
                  State_Id := Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash;
               elsif not Is_Special_Symbol (CP) then
                  Attribute_First_Index := Prev_P;
                  State_Id := Extracting_Attribute_Name;
               end if;
            when Expecting_G_Sign_Or_Extracting_Attributes_And_Found_Slash =>
               if CP = Character'Pos ('>') then
                  State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

                  declare
                     Name : String := Tag_Name.To_String (Tag_Name_Vectors.Last_Element (Tag_Names));
                  begin
                     Tag_Name_Vectors.Delete_Last (Tag_Names);

                     End_Tag (Name,
                              Tag_Names,
                              Call_Result);
                     if Has_Failed (Call_Result) then
                        return;
                     end if;
                  end;
                  Tag_Value_First_Index := P;
                  Shall_Ignore_Tag_Value := True;
               else
                  Initialize (Call_Result, "Expected '>', state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Extracting_Attribute_Name =>
               if CP = Character'Pos ('=') then
                  Attribute_Last_Index := Prev_Prev_P;
                  State_Id := Expecting_Attribute_Value_Quotation_Mark;
--                  Ada.Text_IO.Put_Line ("Extracted attribute name: '" & Contents (Attribute_First_Index..Attribute_Last_Index) & "'");
               elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                  Initialize (Call_Result, "New line is forbidden inside attribute name, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               elsif not Is_Special_Symbol (CP) then
                  null; -- Normal
               end if;
            when Expecting_Attribute_Value_Quotation_Mark =>
               if CP = Character'Pos ('"') then
                  Expected_Quotation_Symbol := Double_Quotes;
                  Attribute_Value_First_Index := P;
                  State_Id := Extracting_Attribute_Value;
               elsif CP = Character'Pos (''') then
                  Expected_Quotation_Symbol := Single_Quotes;
                  Attribute_Value_First_Index := P;
                  State_Id := Extracting_Attribute_Value;
               else
                  Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Aida.UTF8.Code_Point'Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Extracting_Attribute_Value =>
               if
                 (CP = Character'Pos ('"') and Expected_Quotation_Symbol = Double_Quotes) or
                 (CP = Character'Pos (''') and Expected_Quotation_Symbol = Single_Quotes)
               then
                  Attribute_Value_Last_Index := Prev_Prev_P;
                  State_Id := Expecting_G_Sign_Or_Extracting_Attributes;
                  --                  Ada.Text_IO.Put_Line ("Extracted attribute value: '" & Contents (Attribute_Value_First_Index..Attribute_Value_Last_Index) & "'");
                  declare
                     Name : String := Contents (Attribute_First_Index..Attribute_Last_Index);
                     Value : String := Contents (Attribute_Value_First_Index..Attribute_Value_Last_Index);
                  begin
                     Attribute (Name,
                                Value,
                                Tag_Names,
                                Call_Result);
                  end;

                  if Has_Failed (Call_Result) then
                     return;
                  end if;
               elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                  Initialize (Call_Result, "New line is forbidden inside attribute value, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value =>
               if CP = Character'Pos ('"') then
                  Shall_Ignore_Until_Next_Quotation_Mark := not Shall_Ignore_Until_Next_Quotation_Mark;
               elsif CP = Character'Pos ('<') then
                  if not Shall_Ignore_Until_Next_Quotation_Mark then
                     State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L;
                     Tag_Value_Last_Index := Prev_Prev_P;
                  end if;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L =>
               if CP = Character'Pos ('!') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation;
               elsif CP = Character'Pos ('/') then
                  State_Id := Extracting_End_Tag_Name;
                  End_Tag_Name_First_Index := P;
               elsif Is_Special_Symbol (CP) then
                  Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Aida.UTF8.Code_Point'Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               else
                  -- Will ignore tag value, and start parsing child tag!
                  declare
                     Value : String := Contents (Tag_Value_First_Index..(P - 3));
                  begin
                     Text (Value       => Value,
                           Parent_Tags => Tag_Names,
                           Call_Result => Call_Result);
                  end;
                  Shall_Ignore_Tag_Value := True;
                  State_Id := Extracting_Start_Tag_Name;
                  Start_Tag_Name_First_Index := Prev_P;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation =>
               if CP = Character'Pos ('[') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C;
               elsif CP = Character'Pos ('-') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_C =>
               if CP = Character'Pos ('C') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CD =>
               if CP = Character'Pos ('D') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDA =>
               if CP = Character'Pos ('A') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDAT =>
               if CP = Character'Pos ('T') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA =>
               if CP = Character'Pos ('A') then
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_But_Expecting_CDATA_And_Square_Bracket =>
               if CP = Character'Pos ('[') then
                  State_Id := Extracting_CDATA;
                  Tag_Value_First_Index := P;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Extracting_CDATA =>
               if CP = Character'Pos (']') then
                  Tag_Value_Last_Index := Prev_Prev_P;
                  State_Id := Extracting_CDATA_Found_Square_Bracket;
               end if;
            when Extracting_CDATA_Found_Square_Bracket =>
               if CP = Character'Pos (']') then
                  State_Id := Extracting_CDATA_Found_Two_Square_Brackets;
               else
                  State_Id := Extracting_CDATA;
               end if;
            when Extracting_CDATA_Found_Two_Square_Brackets =>
               if CP = Character'Pos ('>') then
                  State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign;
               else
                  State_Id := Extracting_CDATA;
               end if;
            when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign =>
               if CP = Character'Pos ('<') then
                  State_Id := Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign;
               else
                  Initialize (Call_Result, "Expecting '<' followed by end tag but was something else, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Extracting_CDATA_Found_Two_Square_Brackets_And_G_Sign_And_L_Sign =>
               if CP = Character'Pos ('/') then
                  State_Id := Extracting_End_Tag_Name;
                  End_Tag_Name_First_Index := P;
               else
                  Initialize (Call_Result, "Expecting '/' followed by end tag but was something else, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Extracting_End_Tag_Name =>
               if CP = Character'Pos ('>') then
                  End_Tag_Name_Last_Index := Prev_Prev_P;

                  declare
                     Tag_Name : String := To_String (Tag_Name_Vectors.Last_Element (Tag_Names));
                     Value : String := Contents (Tag_Value_First_Index..Tag_Value_Last_Index);
                  begin
                     if Tag_Name /= Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) then
                        Initialize (Call_Result, "Tag names does not match! Start tag is '" & Tag_Name &
                                                "', and end tag is '" & Contents (End_Tag_Name_First_Index..End_Tag_Name_Last_Index) & "' state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                        return;
                     end if;

                     Tag_Name_Vectors.Delete_Last (Tag_Names);

                     if not Shall_Ignore_Tag_Value then
                        End_Tag (Tag_Name,
                                 Value,
                                 Tag_Names,
                                 Call_Result);

                        if Has_Failed (Call_Result) then
                           return;
                        end if;
                     else
                        declare
                           Text_Value : String := Contents (Tag_Value_First_Index..Tag_Value_Last_Index);
                        begin
                           Text (Value       => Text_Value,
                                 Parent_Tags => Tag_Names,
                                 Call_Result => Call_Result);

                           if Has_Failed (Call_Result) then
                              return;
                           end if;
                        end;

                        End_Tag (Tag_Name,
                                 Tag_Names,
                                 Call_Result);

                        if Has_Failed (Call_Result) then
                           return;
                        end if;
                     end if;

                     Tag_Value_First_Index := P;
                     Shall_Ignore_Tag_Value := Boolean_Vectors.Last_Element (Shall_Ignore_Tag_Value_List);
                     Boolean_Vectors.Delete_Last (Shall_Ignore_Tag_Value_List);
                  end;

                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               elsif CP = Character'Pos (Ada.Characters.Latin_1.LF) then
                  Initialize (Call_Result, "New line is forbidden inside attribute value, state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               elsif Is_Special_Symbol (CP) then
                  Initialize (Call_Result, "Unexpected UTF8 symbol (code point" & Aida.UTF8.Code_Point'Image (CP) & "), state " & State_Id_Type'Image (State_Id) & " " & Contents (F..P));
                  return;
               end if;
            when Expecting_New_Tag_Or_Extracting_Tag_Value_And_Found_L_And_Exclamation_And_Dash =>
               if CP = Character'Pos ('-') then
                  declare
                     Value : String := Contents (Tag_Value_First_Index..(P - 5));
                  begin
                     Text (Value       => Value,
                           Parent_Tags => Tag_Names,
                           Call_Result => Call_Result);
                  end;
                  Comment_First_Index := P;
                  State_Id := Extracting_Comment;
               else
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
            when Extracting_Comment =>
               if CP = Character'Pos ('-') then
                  State_Id := Extracting_Comment_And_Found_Dash;
               end if;
            when Extracting_Comment_And_Found_Dash =>
               if CP = Character'Pos ('-') then
                  State_Id := Extracting_Comment_And_Found_Dash_Dash;
               end if;
            when Extracting_Comment_And_Found_Dash_Dash =>
               if CP = Character'Pos ('>') then
                  declare
                     Value : String := Contents (Comment_First_Index..(P - 4));
                  begin
                     Comment (Value       => Value,
                              Parent_Tags => Tag_Names,
                              Call_Result => Call_Result);
                  end;

                  Tag_Value_First_Index := P;
                  State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
               end if;
         end case;
      end loop;
   end;

   return;
end Aida.XML.Generic_Parse_XML_File;

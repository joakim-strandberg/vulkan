package body Aida.XML is

   type State_T is (
                    Normal,
                    Found_And,
                    Found_And_Q,
                    Found_And_Q_U,
                    Found_And_Q_U_O,
                    Found_And_Q_U_O_T,
                    Found_And_A,
                    Found_And_A_P,
                    Found_And_A_P_O,
                    Found_And_A_P_O_S,
                    Found_And_L,
                    Found_And_L_T,
                    Found_And_G,
                    Found_And_G_T,
                    Found_And_A_M,
                    Found_And_A_M_P
                    );


   Unexpected_Character_Code : constant String := "9c1da0b1-0a35-46d5-89df-0f9103ce41fb";

   --  "   &quot;
   --  '   &apos;
   --  <   &lt;
   --  >   &gt;
   --  &   &amp;
   procedure XML_Decode (S           : String_T;
                         Arg         : Custom_T;
                         Call_Result : out Subprogram_Call_Result.T)
   is
      State : State_T := Normal;
   begin
      for I in S'Range loop
         case State is
            when Normal =>
               if S (I) /= '&' then
                  Char (S (I), Arg);
               else
                  State := Found_And;
               end if;
            when Found_And =>
               if S (I) = 'q' then
                  State := Found_And_Q;
               elsif S (I) = 'a' then
                  State := Found_And_A;
               elsif S (I) = 'l' then
                  State := Found_And_L;
               elsif S (I) = 'g' then
                  State := Found_And_G;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_Q =>
               if S (I) = 'u' then
                  State := Found_And_Q_U;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_Q_U =>
               if S (I) = 'o' then
                  State := Found_And_Q_U_O;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_Q_U_O =>
               if S (I) = 't' then
                  State := Found_And_Q_U_O_T;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_Q_U_O_T =>
               if S (I) = ';' then
                  State := Normal;
                  Char ('"', Arg);
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_A =>
               if S (I) = 'p' then
                  State := Found_And_A_P;
               elsif S (I) = 'm' then
                  State := Found_And_A_M;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_A_P =>
               if S (I) = 'o' then
                  State := Found_And_A_P_O;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_A_P_O =>
               if S (I) = 's' then
                  State := Found_And_A_P_O_S;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_A_P_O_S =>
               if S (I) = ';' then
                  State := Normal;
                  Char (''', Arg);
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_L =>
               if S (I) = 't' then
                  State := Found_And_L_T;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_L_T =>
               if S (I) = ';' then
                  State := Normal;
                  Char ('<', Arg);
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_G =>
               if S (I) = 't' then
                  State := Found_And_G_T;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_G_T =>
               if S (I) = ';' then
                  State := Normal;
                  Char ('>', Arg);
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_A_M =>
               if S (I) = 'p' then
                  State := Found_And_A_M_P;
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
            when Found_And_A_M_P =>
               if S (I) = ';' then
                  State := Normal;
                  Char ('&', Arg);
               else
                  Subprogram_Call_Result.Initialize (Call_Result, Unexpected_Character_Code);
               end if;
         end case;
      end loop;
   end XML_Decode;

end Aida.XML;

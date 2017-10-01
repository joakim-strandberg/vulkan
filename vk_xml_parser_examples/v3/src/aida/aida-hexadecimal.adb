package body Aida.Hexadecimal is
   pragma Suppress (Discriminant_Check);
   pragma Suppress (Division_Check);
   pragma Suppress (Index_Check);
   pragma Suppress (Length_Check);
   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);
   pragma Suppress (Tag_Check);
   pragma Suppress (Elaboration_Check);
   pragma SPARK_Mode;

   function To_Nibbles (B : Byte_Type) return Byte_Record_Type is
   begin
      case B is
         when 0   => return (High => 0, Low => 0);
         when 1   => return (High => 0, Low => 1);
         when 2   => return (High => 0, Low => 2);
         when 3   => return (High => 0, Low => 3);
         when 4   => return (High => 0, Low => 4);
         when 5   => return (High => 0, Low => 5);
         when 6   => return (High => 0, Low => 6);
         when 7   => return (High => 0, Low => 7);
         when 8   => return (High => 0, Low => 8);
         when 9   => return (High => 0, Low => 9);
         when 10  => return (High => 0, Low => 10);
         when 11  => return (High => 0, Low => 11);
         when 12  => return (High => 0, Low => 12);
         when 13  => return (High => 0, Low => 13);
         when 14  => return (High => 0, Low => 14);
         when 15  => return (High => 0, Low => 15);
         when 16  => return (High => 1, Low => 0);
         when 17  => return (High => 1, Low => 1);
         when 18  => return (High => 1, Low => 2);
         when 19  => return (High => 1, Low => 3);
         when 20  => return (High => 1, Low => 4);
         when 21  => return (High => 1, Low => 5);
         when 22  => return (High => 1, Low => 6);
         when 23  => return (High => 1, Low => 7);
         when 24  => return (High => 1, Low => 8);
         when 25  => return (High => 1, Low => 9);
         when 26  => return (High => 1, Low => 10);
         when 27  => return (High => 1, Low => 11);
         when 28  => return (High => 1, Low => 12);
         when 29  => return (High => 1, Low => 13);
         when 30  => return (High => 1, Low => 14);
         when 31  => return (High => 1, Low => 15);
         when 32  => return (High => 2, Low => 0);
         when 33  => return (High => 2, Low => 1);
         when 34  => return (High => 2, Low => 2);
         when 35  => return (High => 2, Low => 3);
         when 36  => return (High => 2, Low => 4);
         when 37  => return (High => 2, Low => 5);
         when 38  => return (High => 2, Low => 6);
         when 39  => return (High => 2, Low => 7);
         when 40  => return (High => 2, Low => 8);
         when 41  => return (High => 2, Low => 9);
         when 42  => return (High => 2, Low => 10);
         when 43  => return (High => 2, Low => 11);
         when 44  => return (High => 2, Low => 12);
         when 45  => return (High => 2, Low => 13);
         when 46  => return (High => 2, Low => 14);
         when 47  => return (High => 2, Low => 15);
         when 48  => return (High => 3, Low => 0);
         when 49  => return (High => 3, Low => 1);
         when 50  => return (High => 3, Low => 2);
         when 51  => return (High => 3, Low => 3);
         when 52  => return (High => 3, Low => 4);
         when 53  => return (High => 3, Low => 5);
         when 54  => return (High => 3, Low => 6);
         when 55  => return (High => 3, Low => 7);
         when 56  => return (High => 3, Low => 8);
         when 57  => return (High => 3, Low => 9);
         when 58  => return (High => 3, Low => 10);
         when 59  => return (High => 3, Low => 11);
         when 60  => return (High => 3, Low => 12);
         when 61  => return (High => 3, Low => 13);
         when 62  => return (High => 3, Low => 14);
         when 63  => return (High => 3, Low => 15);
         when 64  => return (High => 4, Low => 0);
         when 65  => return (High => 4, Low => 1);
         when 66  => return (High => 4, Low => 2);
         when 67  => return (High => 4, Low => 3);
         when 68  => return (High => 4, Low => 4);
         when 69  => return (High => 4, Low => 5);
         when 70  => return (High => 4, Low => 6);
         when 71  => return (High => 4, Low => 7);
         when 72  => return (High => 4, Low => 8);
         when 73  => return (High => 4, Low => 9);
         when 74  => return (High => 4, Low => 10);
         when 75  => return (High => 4, Low => 11);
         when 76  => return (High => 4, Low => 12);
         when 77  => return (High => 4, Low => 13);
         when 78  => return (High => 4, Low => 14);
         when 79  => return (High => 4, Low => 15);
         when 80  => return (High => 5, Low => 0);
         when 81  => return (High => 5, Low => 1);
         when 82  => return (High => 5, Low => 2);
         when 83  => return (High => 5, Low => 3);
         when 84  => return (High => 5, Low => 4);
         when 85  => return (High => 5, Low => 5);
         when 86  => return (High => 5, Low => 6);
         when 87  => return (High => 5, Low => 7);
         when 88  => return (High => 5, Low => 8);
         when 89  => return (High => 5, Low => 9);
         when 90  => return (High => 5, Low => 10);
         when 91  => return (High => 5, Low => 11);
         when 92  => return (High => 5, Low => 12);
         when 93  => return (High => 5, Low => 13);
         when 94  => return (High => 5, Low => 14);
         when 95  => return (High => 5, Low => 15);
         when 96  => return (High => 6, Low => 0);
         when 97  => return (High => 6, Low => 1);
         when 98  => return (High => 6, Low => 2);
         when 99  => return (High => 6, Low => 3);
         when 100 => return (High => 6, Low => 4);
         when 101 => return (High => 6, Low => 5);
         when 102 => return (High => 6, Low => 6);
         when 103 => return (High => 6, Low => 7);
         when 104 => return (High => 6, Low => 8);
         when 105 => return (High => 6, Low => 9);
         when 106 => return (High => 6, Low => 10);
         when 107 => return (High => 6, Low => 11);
         when 108 => return (High => 6, Low => 12);
         when 109 => return (High => 6, Low => 13);
         when 110 => return (High => 6, Low => 14);
         when 111 => return (High => 6, Low => 15);
         when 112 => return (High => 7, Low => 0);
         when 113 => return (High => 7, Low => 1);
         when 114 => return (High => 7, Low => 2);
         when 115 => return (High => 7, Low => 3);
         when 116 => return (High => 7, Low => 4);
         when 117 => return (High => 7, Low => 5);
         when 118 => return (High => 7, Low => 6);
         when 119 => return (High => 7, Low => 7);
         when 120 => return (High => 7, Low => 8);
         when 121 => return (High => 7, Low => 9);
         when 122 => return (High => 7, Low => 10);
         when 123 => return (High => 7, Low => 11);
         when 124 => return (High => 7, Low => 12);
         when 125 => return (High => 7, Low => 13);
         when 126 => return (High => 7, Low => 14);
         when 127 => return (High => 7, Low => 15);
         when 128 => return (High => 8, Low => 0);
         when 129 => return (High => 8, Low => 1);
         when 130 => return (High => 8, Low => 2);
         when 131 => return (High => 8, Low => 3);
         when 132 => return (High => 8, Low => 4);
         when 133 => return (High => 8, Low => 5);
         when 134 => return (High => 8, Low => 6);
         when 135 => return (High => 8, Low => 7);
         when 136 => return (High => 8, Low => 8);
         when 137 => return (High => 8, Low => 9);
         when 138 => return (High => 8, Low => 10);
         when 139 => return (High => 8, Low => 11);
         when 140 => return (High => 8, Low => 12);
         when 141 => return (High => 8, Low => 13);
         when 142 => return (High => 8, Low => 14);
         when 143 => return (High => 8, Low => 15);
         when 144 => return (High => 9, Low => 0);
         when 145 => return (High => 9, Low => 1);
         when 146 => return (High => 9, Low => 2);
         when 147 => return (High => 9, Low => 3);
         when 148 => return (High => 9, Low => 4);
         when 149 => return (High => 9, Low => 5);
         when 150 => return (High => 9, Low => 6);
         when 151 => return (High => 9, Low => 7);
         when 152 => return (High => 9, Low => 8);
         when 153 => return (High => 9, Low => 9);
         when 154 => return (High => 9, Low => 10);
         when 155 => return (High => 9, Low => 11);
         when 156 => return (High => 9, Low => 12);
         when 157 => return (High => 9, Low => 13);
         when 158 => return (High => 9, Low => 14);
         when 159 => return (High => 9, Low => 15);
         when 160 => return (High => 10, Low => 0);
         when 161 => return (High => 10, Low => 1);
         when 162 => return (High => 10, Low => 2);
         when 163 => return (High => 10, Low => 3);
         when 164 => return (High => 10, Low => 4);
         when 165 => return (High => 10, Low => 5);
         when 166 => return (High => 10, Low => 6);
         when 167 => return (High => 10, Low => 7);
         when 168 => return (High => 10, Low => 8);
         when 169 => return (High => 10, Low => 9);
         when 170 => return (High => 10, Low => 10);
         when 171 => return (High => 10, Low => 11);
         when 172 => return (High => 10, Low => 12);
         when 173 => return (High => 10, Low => 13);
         when 174 => return (High => 10, Low => 14);
         when 175 => return (High => 10, Low => 15);
         when 176 => return (High => 11, Low => 0);
         when 177 => return (High => 11, Low => 1);
         when 178 => return (High => 11, Low => 2);
         when 179 => return (High => 11, Low => 3);
         when 180 => return (High => 11, Low => 4);
         when 181 => return (High => 11, Low => 5);
         when 182 => return (High => 11, Low => 6);
         when 183 => return (High => 11, Low => 7);
         when 184 => return (High => 11, Low => 8);
         when 185 => return (High => 11, Low => 9);
         when 186 => return (High => 11, Low => 10);
         when 187 => return (High => 11, Low => 11);
         when 188 => return (High => 11, Low => 12);
         when 189 => return (High => 11, Low => 13);
         when 190 => return (High => 11, Low => 14);
         when 191 => return (High => 11, Low => 15);
         when 192 => return (High => 12, Low => 0);
         when 193 => return (High => 12, Low => 1);
         when 194 => return (High => 12, Low => 2);
         when 195 => return (High => 12, Low => 3);
         when 196 => return (High => 12, Low => 4);
         when 197 => return (High => 12, Low => 5);
         when 198 => return (High => 12, Low => 6);
         when 199 => return (High => 12, Low => 7);
         when 200 => return (High => 12, Low => 8);
         when 201 => return (High => 12, Low => 9);
         when 202 => return (High => 12, Low => 10);
         when 203 => return (High => 12, Low => 11);
         when 204 => return (High => 12, Low => 12);
         when 205 => return (High => 12, Low => 13);
         when 206 => return (High => 12, Low => 14);
         when 207 => return (High => 12, Low => 15);
         when 208 => return (High => 13, Low => 0);
         when 209 => return (High => 13, Low => 1);
         when 210 => return (High => 13, Low => 2);
         when 211 => return (High => 13, Low => 3);
         when 212 => return (High => 13, Low => 4);
         when 213 => return (High => 13, Low => 5);
         when 214 => return (High => 13, Low => 6);
         when 215 => return (High => 13, Low => 7);
         when 216 => return (High => 13, Low => 8);
         when 217 => return (High => 13, Low => 9);
         when 218 => return (High => 13, Low => 10);
         when 219 => return (High => 13, Low => 11);
         when 220 => return (High => 13, Low => 12);
         when 221 => return (High => 13, Low => 13);
         when 222 => return (High => 13, Low => 14);
         when 223 => return (High => 13, Low => 15);
         when 224 => return (High => 14, Low => 0);
         when 225 => return (High => 14, Low => 1);
         when 226 => return (High => 14, Low => 2);
         when 227 => return (High => 14, Low => 3);
         when 228 => return (High => 14, Low => 4);
         when 229 => return (High => 14, Low => 5);
         when 230 => return (High => 14, Low => 6);
         when 231 => return (High => 14, Low => 7);
         when 232 => return (High => 14, Low => 8);
         when 233 => return (High => 14, Low => 9);
         when 234 => return (High => 14, Low => 10);
         when 235 => return (High => 14, Low => 11);
         when 236 => return (High => 14, Low => 12);
         when 237 => return (High => 14, Low => 13);
         when 238 => return (High => 14, Low => 14);
         when 239 => return (High => 14, Low => 15);
         when 240 => return (High => 15, Low => 0);
         when 241 => return (High => 15, Low => 1);
         when 242 => return (High => 15, Low => 2);
         when 243 => return (High => 15, Low => 3);
         when 244 => return (High => 15, Low => 4);
         when 245 => return (High => 15, Low => 5);
         when 246 => return (High => 15, Low => 6);
         when 247 => return (High => 15, Low => 7);
         when 248 => return (High => 15, Low => 8);
         when 249 => return (High => 15, Low => 9);
         when 250 => return (High => 15, Low => 10);
         when 251 => return (High => 15, Low => 11);
         when 252 => return (High => 15, Low => 12);
         when 253 => return (High => 15, Low => 13);
         when 254 => return (High => 15, Low => 14);
         when 255 => return (High => 15, Low => 15);
      end case;
   end To_Nibbles;

   function To_String (C : Character)
                       return String_Of_Byte_Type is
      BR : constant Byte_Record_Type := To_Nibbles (To_Byte (C));
   begin
      return (1 => To_Character_Of_Nibble (BR.High), 2 => To_Character_Of_Nibble (BR.Low));
   end To_String;

end Aida.Hexadecimal;
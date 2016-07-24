with Ada.Containers;
--  with Aida.Generic_Subprogram_Call_Result;

use type Ada.Containers.Count_Type;

--  pragma Elaborate_All (Aida.Generic_Subprogram_Call_Result);

package Aida.UTF8 with SPARK_Mode is

   type Code_Point is mod 2**32;
   subtype UTF8_Code_Point is Code_Point range 0..16#10FFFF#;

--     package Subprogram_Call_Result is new Aida.Generic_Subprogram_Call_Result (1000);
--
--     use Subprogram_Call_Result;

   function Is_Valid_UTF8_Code_Point (Source      : String;
                                      Pointer     : Integer) return Boolean is ((Source'First <= Pointer and Pointer <= Source'Last) and then
                                                                                  (if (Character'Pos (Source (Pointer)) in 0..16#7F#) then Pointer < Integer'Last
                                                                                   else (Pointer < Source'Last and then (if Character'Pos (Source (Pointer)) in 16#C2#..16#DF# and Character'Pos (Source (Pointer + 1)) in 16#80#..16#BF# then Pointer < Integer'Last - 1
                                                                                                                         else (Pointer < Source'Last - 1 and then (if (Character'Pos (Source (Pointer)) = 16#E0# and Character'Pos (Source (Pointer + 1)) in 16#A0#..16#BF# and Character'Pos (Source (Pointer + 2)) in 16#80#..16#BF#) then Pointer < Integer'Last - 2
                                                                                                                                                                   elsif (Character'Pos (Source (Pointer)) in 16#E1#..16#EF# and Character'Pos (Source (Pointer + 1)) in 16#80#..16#BF# and Character'Pos (Source (Pointer + 2)) in 16#80#..16#BF#) then Pointer < Integer'Last - 2
                                                                                                                                                                   else (Pointer < Source'Last - 2 and then (if (Character'Pos (Source (Pointer)) = 16#F0# and Character'Pos (Source (Pointer + 1)) in 16#90#..16#BF# and Character'Pos (Source (Pointer + 2)) in 16#80#..16#BF# and Character'Pos (Source (Pointer + 3)) in 16#80#..16#BF#) then Pointer < Integer'Last - 3
                                                                                                                                                                                                            elsif (Character'Pos (Source (Pointer)) in 16#F1#..16#F3# and Character'Pos (Source (Pointer + 1)) in 16#80#..16#BF# and Character'Pos (Source (Pointer + 2)) in 16#80#..16#BF# and Character'Pos (Source (Pointer + 3)) in 16#80#..16#BF#) then Pointer < Integer'Last - 3
                                                                                                                                                                                                            elsif (Character'Pos (Source (Pointer)) = 16#F4# and Character'Pos (Source (Pointer + 1)) in 16#80#..16#8F# and Character'Pos (Source (Pointer + 2)) in 16#80#..16#BF# and Character'Pos (Source (Pointer + 3)) in 16#80#..16#BF#) then Pointer < Integer'Last - 3 else False))))))));

--
-- Get -- Get one UTF-8 code point
--
--    Source  - The source string
--    Pointer - The string position to start at
--    Value   - The result
--
-- This  procedure  decodes one UTF-8 code point from the string Source.
-- It starts at Source (Pointer). After successful completion Pointer is
-- advanced to the first character following the input.  The  result  is
-- returned through the parameter Value.
--
-- Exceptions :
--
--    Data_Error   - UTF-8 syntax error
--    End_Error    - Nothing found
--    Layout_Error - Pointer is not in Source'First..Source'Last + 1
--
   procedure Get (Source      : String;
                  Pointer     : in out Integer;
                  Value       : out UTF8_Code_Point) with
     Global => null,
     Pre    => Is_Valid_UTF8_Code_Point (Source, Pointer),
     Post   => (if (Character'Pos (Source (Pointer'Old)) in 0..16#7F#) then Character'Pos (Source (Pointer'Old)) = Value and Pointer = Pointer'Old + 1
                    elsif (Pointer'Old < Source'Last and then (Character'Pos (Source (Pointer'Old)) in 16#C2#..16#DF# and Character'Pos (Source (Pointer'Old + 1)) in 16#80#..16#BF#)) then Pointer = Pointer'Old + 2
                  elsif (Pointer'Old < Source'Last - 1 and then ((Character'Pos (Source (Pointer'Old)) = 16#E0# and Character'Pos (Source (Pointer'Old + 1)) in 16#A0#..16#BF# and Character'Pos (Source (Pointer'Old + 2)) in 16#80#..16#BF#) or
                        (Character'Pos (Source (Pointer'Old)) in 16#E1#..16#EF# and Character'Pos (Source (Pointer'Old + 1)) in 16#80#..16#BF# and Character'Pos (Source (Pointer'Old + 2)) in 16#80#..16#BF#))) then Pointer = Pointer'Old + 3
                    elsif (Pointer < Source'Last - 2 and then ((Character'Pos (Source (Pointer'Old)) = 16#F0# and Character'Pos (Source (Pointer'Old + 1)) in 16#90#..16#BF# and Character'Pos (Source (Pointer'Old + 2)) in 16#80#..16#BF# and Character'Pos (Source (Pointer'Old + 3)) in 16#80#..16#BF#) or
                      (Character'Pos (Source (Pointer'Old)) in 16#F1#..16#F3# and Character'Pos (Source (Pointer'Old + 1)) in 16#80#..16#BF# and Character'Pos (Source (Pointer'Old + 2)) in 16#80#..16#BF# and Character'Pos (Source (Pointer'Old + 3)) in 16#80#..16#BF#) or
                 (Character'Pos (Source (Pointer'Old)) = 16#F4# and Character'Pos (Source (Pointer'Old + 1)) in 16#80#..16#8F# and Character'Pos (Source (Pointer'Old + 2)) in 16#80#..16#BF# and Character'Pos (Source (Pointer'Old + 3)) in 16#80#..16#BF#))) then Pointer = Pointer'Old + 4
               );

   --function Is_Valid_UTF8 (Source : String) return Boolean;

--
-- Length -- The length of an UTF-8 string
--
--    Source - The string containing UTF-8 encoded code points
--
-- Returns :
--
--    The number of UTF-8 encoded code points in Source
--
-- Exceptions :
--
--    Data_Error - Invalid string
--
   function Length (Source : String) return Natural with
     Global => null;

--
-- Image -- Of an UTF-8 code point
--
--    Value - The code point
--
-- Returns :
--
--    UTF-8 encoded equivalent
--
   function Image (Value : UTF8_Code_Point) return String with
     Global => null;

--
-- Put -- Put one UTF-8 code point
--
--    Destination - The target string
--    Pointer     - The position where to place the character
--    Value       - The code point to put
--
-- This  procedure  puts  one  UTF-8  code  point into the string Source
-- starting from the position Source (Pointer). Pointer is then advanced
-- to the first character following the output.
--
-- Exceptions :
--
--    Layout_Error - Pointer is not in Destination'Range of there is  no
--                   room for output
--
   procedure Put (Destination : in out String;
                  Pointer     : in out Integer;
                  Value       : UTF8_Code_Point) with
     Global => null,
     Pre    => (Pointer in Destination'Range and Destination'Last < Integer'Last) and then (if Value <= 16#7F# then Pointer < Integer'Last
                                                                                              elsif Value <= 16#7FF# then Pointer < Integer'Last - 1 and Pointer + 1 in Destination'Range
                                                                                                elsif Value <= 16#FFFF# then (Pointer < Integer'Last - 2 and then (Pointer + 1 in Destination'Range and Pointer + 2 in Destination'Range))
                                                                                                  else Pointer < Integer'Last - 3 and then (Pointer + 1 in Destination'Range and Pointer + 2 in Destination'Range and Pointer + 3 in Destination'Range)),
     Post   => Pointer /= Pointer'Old and (Pointer in Destination'Range or Pointer = Destination'Last + 1);

end Aida.UTF8;

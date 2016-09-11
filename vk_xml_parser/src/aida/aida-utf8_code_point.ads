package Aida.UTF8_Code_Point with SPARK_Mode is

   type Code_Point is mod 2**32;
   subtype T is Code_Point range 0..16#10FFFF#;

   --
   -- Image -- Of an UTF-8 code point
   --
   --    Value - The code point
   --
   -- Returns :
   --
   --    UTF-8 encoded equivalent
   --
   function Image (Value : T) return String with
     Global => null;

end Aida.UTF8_Code_Point;

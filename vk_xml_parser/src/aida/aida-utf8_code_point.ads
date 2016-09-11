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

   --
   -- Is_Uppercase -- Case test
   --
   --    Value - Code point
   --
   -- Returns :
   --
   --    True if Value is a lower case point
   --
   function Is_Uppercase (Value : T) return Boolean with
     Global => null;

   procedure Check;

end Aida.UTF8_Code_Point;

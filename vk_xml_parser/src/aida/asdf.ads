with Aida.Strings.Generic_Mutable_Unbounded_String_Shared_Ptr;

pragma Elaborate_All (Aida.Strings.Generic_Mutable_Unbounded_String_Shared_Ptr);

package Asdf with SPARK_Mode is

   package qawert is new Aida.Strings.Generic_Mutable_Unbounded_String_Shared_Ptr (100);

end Asdf;

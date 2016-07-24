with Aida.Strings.Generic_Mutable_Unbounded_String;
with Ada.Containers;

generic
   Capacity : Ada.Containers.Count_Type;
package Aida.Generic_Subprogram_Call_Result with SPARK_Mode is

   use type Ada.Containers.Count_Type;

   type T is limited private with Default_Initial_Condition => Length (T) = 0 and Has_Failed (T) = False;

   procedure Initialize (This    : in out T;
                         Message : String) with
     Global => null,
     Pre    => Length (This) = 0,
     Post   => Has_Failed (This) = True;

   function Has_Failed (This : T) return Boolean with
     Global => null;

   function Length (This : T) return Ada.Containers.Count_Type with
     Global => null;

   function Message (This : T) return String with
     Pre    => Length (This) < Aida.Strings.MAX_LENGTH,
     Global => null;

private

   package Unbounded_String is new Aida.Strings.Generic_Mutable_Unbounded_String (Capacity);

   type T is limited
      record
         My_Message    : Unbounded_String.T;
         My_Has_Failed : Boolean := False;
      end record;

   function Length (This : T) return Ada.Containers.Count_Type is (Unbounded_String.Length (This.My_Message));

end Aida.Generic_Subprogram_Call_Result;

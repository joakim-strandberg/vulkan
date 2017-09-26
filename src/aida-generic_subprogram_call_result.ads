with Ada.Strings.Unbounded;

generic
package Aida.Generic_Subprogram_Call_Result is

   type T is limited private with Default_Initial_Condition => Length (T) = 0 and Has_Failed (T) = False;

   procedure Initialize (This    : in out T;
                         Message : String) with
     Global => null,
     Pre    => Length (This) = 0,
     Post   => Has_Failed (This) = True;

   function Has_Failed (This : T) return Boolean with
     Global => null;

   function Length (This : T) return Natural with
     Global => null;

   function Message (This : T) return String;

private

   type Unbounded_String_T is new Ada.Strings.Unbounded.Unbounded_String;

   type T is limited
      record
         My_Message    : Unbounded_String_T;
         My_Has_Failed : Boolean := False;
      end record;

   function Length (This : T) return Natural is (Ada.Strings.Unbounded.Length (Ada.Strings.Unbounded.Unbounded_String (This.My_Message)));

end Aida.Generic_Subprogram_Call_Result;

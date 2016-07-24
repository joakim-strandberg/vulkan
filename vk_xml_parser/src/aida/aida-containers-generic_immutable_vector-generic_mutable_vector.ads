generic
package Aida.Containers.Generic_Immutable_Vector.Generic_Mutable_Vector with SPARK_Mode is

   type T (Capacity : Capacity_Range) is new Immutable_T (Capacity);

   procedure Clear (Container : in out T) with
     Global => null;
   --  Note that this reclaims storage in the unbounded case. You need to call
   --  this before a container goes out of scope in order to avoid storage
   --  leaks. In addition, "X := ..." can leak unless you Clear(X) first.

   procedure Append
     (Container : in out T;
      New_Item  : T)
   with
     Global => null,
     Pre    => (if Bounded then
                 Length (Container) + Length (New_Item) <= Container.Capacity);

   procedure Append
     (Container : in out T;
      New_Item  : Element_Type)
   with
     Global => null,
     Pre    => (if Bounded then
                  Length (Container) < Container.Capacity);

   procedure Delete_Last
     (Container : in out T)
   with
     Global => null;

end Aida.Containers.Generic_Immutable_Vector.Generic_Mutable_Vector;

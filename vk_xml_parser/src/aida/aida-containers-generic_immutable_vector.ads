with Ada.Containers.Formal_Vectors;
with Aida.Generic_Shared_Ptr;

pragma Elaborate_All (Ada.Containers.Formal_Vectors);

generic
   type Index_Type is range <>;
   type Element_Type is private;

   with function "=" (Left, Right : Element_Type) return Boolean is <>;

   Bounded : Boolean := True;
   --  If True, the containers are bounded; the initial capacity is the maximum
   --  size, and heap allocation will be avoided. If False, the containers can
   --  grow via heap allocation.
package Aida.Containers.Generic_Immutable_Vector with SPARK_Mode is

   subtype Extended_Index is Index_Type'Base range Index_Type'First - 1 .. Index_Type'Min (Index_Type'Base'Last - 1, Index_Type'Last) + 1;

   subtype Capacity_Range is
     Count_Type range 0 .. Count_Type (Index_Type'Last - Index_Type'First + 1);

   type Immutable_T (Capacity : Capacity_Range) is private;

   function Length (Container : Immutable_T) return Capacity_Range with
     Global => null;

   function Is_Empty (Container : Immutable_T) return Boolean with
     Global => null;

   function Element
     (Container : Immutable_T;
      Index     : Index_Type) return Element_Type
   with
     Global => null,
     Pre    => Index in First_Index (Container) .. Last_Index (Container);

   function First_Index (Container : Immutable_T) return Index_Type with
     Global => null;

   function First_Element (Container : Immutable_T) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);

   function Last_Index (Container : Immutable_T) return Extended_Index with
     Global => null;

   function Last_Element (Container : Immutable_T) return Element_Type with
     Global => null,
     Pre    => not Is_Empty (Container);

   function Find_Index
     (Container : Immutable_T;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'First) return Extended_Index
   with
     Global => null;

   function Reverse_Find_Index
     (Container : Immutable_T;
      Item      : Element_Type;
      Index     : Index_Type := Index_Type'Last) return Extended_Index
   with
     Global => null;

   function Contains
     (Container : Immutable_T;
      Item      : Element_Type) return Boolean
   with
     Global => null;

   function Has_Element
     (Container : Immutable_T;
      Position  : Extended_Index) return Boolean
   with
     Global => null;

private
   pragma SPARK_Mode (Off);

   package Vectors is new Ada.Containers.Formal_Vectors (Index_Type   => Index_Type,
                                                         Element_Type => Element_Type,
                                                         "="          => "=",
                                                         Bounded      => Bounded);

   type Vector_Ptr is access Vectors.Vector;

   package Smart_Pointers is new Aida.Generic_Shared_Ptr (T => Vectors.Vector,
                                                          P => Vector_Ptr);

   type Immutable_T (Capacity : Capacity_Range) is
      record
         SP : Smart_Pointers.Pointer := Smart_Pointers.Create (new Vectors.Vector (Capacity));
      end record;

end Aida.Containers.Generic_Immutable_Vector;

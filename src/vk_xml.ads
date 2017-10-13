with Dynamic_Pools;
with Aida;
with Ada.Containers.Vectors;
pragma Unreferenced (Ada.Containers.Vectors); -- Is used in child packages

package Vk_XML is

   Main_Pool : Dynamic_Pools.Dynamic_Pool (0);

   use all type Aida.String_T; -- Is used in child packages

   type String_Ptr is access all Aida.String_T with Storage_Pool => Main_Pool;

   Empty_String : aliased Aida.String_T := "";

private

   type Nullable_String_Ptr (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : not null String_Ptr := Empty_String'Access;
         when False => null;
      end case;
   end record;

   type Nullable_Boolean_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Boolean;
         when False => null;
      end case;
   end record;

end Vk_XML;

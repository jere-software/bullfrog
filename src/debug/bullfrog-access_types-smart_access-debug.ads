generic
package Bullfrog.Access_Types.Smart_Access.Debug is
   function Print(Object : Shared_Access) return String;
   function Print(Object : Weak_Access)   return String;
   function Print(Object : Unique_Access) return String;
end Bullfrog.Access_Types.Smart_Access.Debug;

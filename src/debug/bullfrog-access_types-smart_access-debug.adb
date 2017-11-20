with System.Address_Image;
with Ada.Unchecked_Conversion;
with System.Storage_Elements; use System.Storage_Elements;

package body Bullfrog.Access_Types.Smart_Access.Debug is

   function To_Integer is new Ada.Unchecked_Conversion
      (Source => Item_Access,
       Target => Integer_Address);

    function To_Integer is new Ada.Unchecked_Conversion
      (Source => Counts_Access,
       Target => Integer_Address);

   function Print(Object : Shared_Access) return String is
   begin
      return
         "("
         & System.Address_Image(Object'Address)
         & ","
         & Integer_Address'Image(To_Integer(Object.Item_Reference))
         & ","
         & Integer_Address'Image(To_Integer(Object.Counts_Reference))
         & ","
         & Reference_Counts.Count_Type'Image(Utilities.Use_Count(Object))
         & ","
         & Reference_Counts.Count_Type'Image(Utilities.Weak_Count(Object))
         & ")";
   end Print;
   function Print(Object : Weak_Access) return String is
   begin
      return
         "("
         & System.Address_Image(Object'Address)
         & ","
         & Integer_Address'Image(To_Integer(Object.Item_Reference))
         & ","
         & Integer_Address'Image(To_Integer(Object.Counts_Reference))
         & ","
         & Reference_Counts.Count_Type'Image(Utilities.Use_Count(Object))
         & ","
         & Reference_Counts.Count_Type'Image(Utilities.Weak_Count(Object))
         & ")";
   end Print;
   function Print(Object : Unique_Access) return String is
   begin
      return
         "("
         & System.Address_Image(Object'Address)
         & ","
         & Integer_Address'Image(To_Integer(Object.Item_Reference))
         & ")";
   end Print;
end Bullfrog.Access_Types.Smart_Access.Debug;

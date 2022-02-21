with Bullfrog.Access_Types.Custom_Smart_Access.Make;

package body Bullfrog.Access_Types.Smart_Access is

   -- Shared_Access operations
   procedure Set_Null
      (Self : in out Shared_Access)
       renames Core.Set_Null;
   function Reference
      (Self : in Shared_Access)
       return Reference_Holder
       renames Core.Reference;
   function Constant_Reference
      (Self : in Shared_Access)
       return Constant_Reference_Holder
       renames Core.Constant_Reference;
   function "="
      (Left,Right : Shared_Access)
       return Boolean
       renames Core."=";
   function Is_Null
      (Self : in Shared_Access)
       return Boolean
       renames Core.Is_Null;
   function Not_Null
      (Self : in Shared_Access)
       return Boolean
       renames Core.Not_Null;
   procedure Swap (Left, Right : in out Shared_Access) renames Core.Swap;
   procedure Move (Target, Source : in out Shared_Access ) renames Core.Move;
   procedure Adjust  (Self : in out Shared_Access) renames Core.Adjust;
   procedure Finalize(Self : in out Shared_Access) renames Core.Finalize;

   -- Weak_Access operations
   procedure Remove_Assignment
      (Self : in out Weak_Access)
       renames Core.Remove_Assignment;
   function "="
      (Left,Right : Weak_Access)
       return Boolean
       renames Core."=";
   function Is_Assigned
      (Self : in Weak_Access)
       return Boolean
       renames Core.Is_Assigned;
   function Not_Assigned
      (Self : in Weak_Access)
       return Boolean
       renames Core.Not_Assigned;
   procedure Swap(Left, Right : in out Weak_Access) renames Core.Swap;
   procedure Move (Target, Source : in out Weak_Access ) renames Core.Move;
   procedure Adjust  (Self : in out Weak_Access) renames Core.Adjust;
   procedure Finalize(Self : in out Weak_Access) renames Core.Finalize;

   -- Unique_Access operations
   procedure Set_Null
      (Self : in out Unique_Access)
       renames Core.Set_Null;
   function Reference
      (Self : in Unique_Access)
       return Reference_Holder
       renames Core.Reference;
   function Constant_Reference
      (Self : in Unique_Access)
       return Constant_Reference_Holder
       renames Core.Constant_Reference;
   function Is_Null
      (Self : in Unique_Access)
       return Boolean
       renames Core.Is_Null;
   function Not_Null
      (Self : in Unique_Access)
       return Boolean
       renames Core.Not_Null;
   procedure Swap(Left, Right : in out Unique_Access) renames Core.Swap;
   procedure Move (Target, Source : in out Unique_Access ) renames Core.Move;
   procedure Finalize(Self : in out Unique_Access) renames Core.Finalize;

   package body Make is

      -- Core implementation
      package Core_Make is new Core.Make
         (Element_Type   => Element_Type,
          Element_Access => Element_Access,
          Traits         => Core_Traits);

      procedure Shared_Access
         (Target : in out Smart_Access.Shared_Access;
          Source : in     not null Element_Access)
          renames Core_Make.Shared_Access;
      procedure Shared_Access
         (Target : in out Smart_Access.Shared_Access;
          Source : in     Smart_Access.Shared_Access)
          renames Core_Make.Shared_Access;
      procedure Shared_Access
         (Target : in out Smart_Access.Shared_Access;
          Source : in out Smart_Access.Unique_Access)
          renames Core_Make.Shared_Access;
      function Shared_Access
         (Source : in not null Element_Access)
          return Smart_Access.Shared_Access
          renames Core_Make.Shared_Access;
      function Shared_Access
         (Source : in out Smart_Access.Unique_Access)
          return Smart_Access.Shared_Access
          renames Core_Make.Shared_Access;

      -- Weak_Access operations
      procedure Weak_Access
         (Target : in out Smart_Access.Weak_Access;
          Source : in     Smart_Access.Weak_Access)
          renames Core_Make.Weak_Access;
      procedure Weak_Access
         (Target : in out Smart_Access.Weak_Access;
          Source : in     Smart_Access.Shared_Access)
          renames Core_Make.Weak_Access;
      function Weak_Access
         (Source : in Smart_Access.Shared_Access)
          return Smart_Access.Weak_Access
          renames Core_Make.Weak_Access;

      -- Unique_Access operations
      procedure Unique_Access
         (Target : in out Smart_Access.Unique_Access;
          Source : in     not null Element_Access)
          renames Core_Make.Unique_Access;
      procedure Unique_Access
         (Target : in out Smart_Access.Unique_Access;
          Source : in out Smart_Access.Unique_Access)
          renames Core_Make.Unique_Access;
      function Unique_Access
         (Source : in not null Element_Access)
          return Smart_Access.Unique_Access
          renames Core_Make.Unique_Access;
      function Unique_Access
         (Source : in out Smart_Access.Unique_Access)
          return Smart_Access.Unique_Access
          renames Core_Make.Unique_Access;

   end Make;

   package body Utilities is

      -- Shared Access Operations
      function Use_Count
         (Self : in Shared_Access)
          return Basic_Count
          renames Core.Utilities.Use_Count;
      function Weak_Count
         (Self : in Shared_Access)
          return Basic_Count
          renames Core.Utilities.Weak_Count;
      function Raw_Access
         (Self : in Shared_Access)
          return Element_Access
          renames Core.Utilities.Raw_Access;
      function Raw_Constant_Access
         (Self : in Shared_Access)
          return Constant_Element_Access
          renames Core.Utilities.Raw_Constant_Access;


      -- Weak_Access operations
      function Use_Count
         (Self : in Weak_Access)
          return Basic_Count
          renames Core.Utilities.Use_Count;
      function Weak_Count
         (Self : in Weak_Access)
          return Basic_Count
          renames Core.Utilities.Weak_Count;
      function Is_Assigned_To
         (Weak   : in Weak_Access;
          Shared : in Shared_Access)
          return Boolean
          renames Core.Utilities.Is_Assigned_To;
      function Not_Assigned_To
         (Weak   : in Weak_Access;
          Shared : in Shared_Access)
          return Boolean
          renames Core.Utilities.Not_Assigned_To;

      function Raw_Access
         (Self : in Unique_Access)
          return Element_Access
          renames Core.Utilities.Raw_Access;
      function Raw_Constant_Access
         (Self : in Unique_Access)
          return Constant_Element_Access
          renames Core.Utilities.Raw_Constant_Access;

   end Utilities;

end Bullfrog.Access_Types.Smart_Access;

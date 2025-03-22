-- Program to display dnsmasq leases file in a more user friendly format
-- particularly the remaining lease time. Provides sorting options based on
-- Host Name, IP Address, MAC Address or remaining lease time.

-- Author    : David Haley
-- Created   : 05/11/2019
-- Last Edit : 22/03/2025

-- 20250322 : Tidy up, change to linked list and include version number.
-- 20220716 : Adds number of leases to header.
-- 20210304 : Unified version of date and time strings used.
-- 20191112 : MAC_Addresses in now an unsigned number

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Interfaces; use Interfaces;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;

procedure Leases is

   Version_String : String := "leases version 20250322";
   
   package Leases_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   use Leases_Lists;

   subtype MAC_Addresses is Unsigned_64 range 0 .. 2 ** 48 - 1;
   -- no Unsigned 48
   subtype MAC_Byte_Counts is Positive range 1 .. 6;
   -- bytes in MAC address
   subtype IP_Addresses is Unsigned_32;
   subtype IP_Byte_Counts is Positive range 1 .. 4;
   -- bytes in IPV4 IP address
   subtype Bytes is Unsigned_8;

   type Lease_Elements is record
      Lease_End : Posix_Times;
      IP_Address : IP_Addresses;
      MAC_Address : MAC_Addresses;
      Host_Name : Unbounded_String;
   end record; -- Leases_Elements

   package Leases_Tables is new
     Ada.Containers.Doubly_Linked_Lists (Lease_Elements);
   use Leases_Tables;

   function Host_Compare (Left, Right : Lease_Elements) return Boolean is
      -- compare function is not case sensitive

      Left_Host : Unbounded_String :=
        Translate (Left.Host_Name, Upper_Case_Map);
      Right_Host : Unbounded_String :=
        Translate (Right.Host_Name, Upper_Case_Map);

   begin -- Host_Compare
      return Left_Host < Right_Host;
   end Host_Compare;

   package Host_Sort is new Leases_Tables.Generic_Sorting (Host_Compare);

   function IP_Compare (Left, Right : Lease_Elements) return Boolean is
      (Left.IP_Address < Right.IP_Address);

   package IP_Sort is new Leases_Tables.Generic_Sorting (IP_Compare);

   function MAC_Compare (Left, Right : Lease_Elements) return Boolean is
      (Left.MAC_Address < Right.MAC_Address);

   package MAC_Sort is new Leases_Tables.Generic_Sorting (MAC_Compare);

   function Time_Compare (Left, Right : Lease_Elements) return Boolean is
      (Left.Lease_End < Right.Lease_End);

   package Time_Sort is new Leases_Tables.Generic_Sorting (Time_Compare);

   procedure Read_List (Leases_List : out Leases_Lists.List) is

      Leases_File_Name : String := "/var/lib/misc/dnsmasq.leases";
      Leases_File : File_Type;
      Text : Unbounded_String;

   begin -- Read_List
      Clear (Leases_List);
      Open (Leases_File, In_File, Leases_File_Name);
      while not End_Of_File (Leases_File) loop
         Ada.Text_IO.Unbounded_IO.Get_Line (Leases_File, Text);
         Append (Leases_List, Text);
      end loop; -- not End_Of_File (Leases_File)
      Close (Leases_File);
   end Read_List;

   procedure Build_Table (Leases_List : in Leases_Lists.List;
                          Leases_Table : out Leases_Tables.List) is

      Lease_Element : Lease_Elements;
      Delimiter : Character_Set := To_Set (' ');
      Start_At, First : Positive;
      Last : Natural;

   begin -- Build_Table
      Clear (Leases_Table);
      for I in Iterate (Leases_List) loop
         Start_At := 1;
         -- Parse Posix time
         Find_Token (Leases_List (I), Delimiter, Start_At, Outside, First,
                     Last);
         Lease_Element.Lease_End :=
           Posix_Times'Value (Slice (Leases_List (I), First, Last));
         Start_At := Last + 1;
         -- Parse MAC address
         Lease_Element.MAC_Address := 0;
         for Byte in MAC_Byte_Counts loop
            Lease_Element.MAC_Address :=
              Shift_Left (Lease_Element.MAC_Address, Bytes'Size);
            Find_Token (Leases_List (I), Hexadecimal_Digit_Set, Start_At, Inside,
                        First, Last);
            Lease_Element.MAC_Address := Lease_Element.MAC_Address +
              MAC_Addresses (Bytes'Value ("16#" & Slice (Leases_List (I), First,
                             Last) & '#'));
            Start_At := Last + 1;
         end loop; --Byte in MAC_Byte_Counts
         -- Parse dotted IP address
         Lease_Element.IP_Address := 0;
         for Byte in IP_Byte_Counts loop
            Lease_Element.IP_Address :=
              Shift_Left (Lease_Element.IP_Address, Bytes'Size);
            Find_Token (Leases_List (I), Decimal_Digit_Set, Start_At, Inside,
                        First, Last);
            Lease_Element.IP_Address := Lease_Element.IP_Address +
              IP_Addresses (Bytes'Value (Slice (Leases_List (I), First, Last)));
            Start_At := Last + 1;
         end loop; --Byte in IP_Byte_Counts
         -- Parse host name
         Find_Token (Leases_List (I), Delimiter, Start_At, Outside, First,
                     Last);
         Lease_Element.Host_Name :=
           Unbounded_Slice (Leases_List (I), First, Last);
         Append (Leases_Table, Lease_Element);
      end loop; -- I in Iterate (Leases_List)
   end Build_Table;

   function MAC_String (MAC_Address : in MAC_Addresses) return String is

      Result_String : String (1 .. 16);
      Result : Unbounded_String := Null_Unbounded_String;

      package MAC_IO is new Ada.Text_IO.Modular_IO (MAC_Addresses);

   begin -- MAC_String
      MAC_IO.Put (Result_String, MAC_Address, 16);
      Delete (Result_String, Result_String'Last, Result_String'Last, Right);
      -- remove trailing '#'
      Overwrite (Result_String, Index (Result_String, "16#", 1), "000");
      -- replace "16#" with "000"
      for Byte in MAC_Byte_Counts loop
         Result := Result & Result_String (Byte * 2 + 3 .. Byte * 2 + 4);
         If Byte /= MAC_Byte_Counts'Last then
            Result := Result & ':';
         end if; -- Byte /= MAC_Byte_Counts'Last
      end loop; -- Byte in MAC_Byte_Counts
      Translate (Result, Upper_Case_Map);
      return To_String (Result);
   end MAC_String;

   function IP_String (IP_Address : in IP_Addresses) return String is

      Number : IP_Addresses := IP_Address;
      Result : Unbounded_String := Null_Unbounded_String;

   begin -- IP_String
      for Byte in IP_Byte_Counts loop
         Result := Bytes'Image (Bytes'Mod (Number)) & Result;
         Trim (Result, Left);
         Number := Shift_Right (Number, Bytes'Size);
         If Byte /= IP_Byte_Counts'Last then
            Result := '.' & Result;
         end if; -- Byte /= IP_Byte_Counts'Last
      end loop; -- Byte in IP_Byte_Counts
      Head (Result, 15);
      return To_String (Result);
   end IP_String;

   procedure Write_Table (Leases_Table : in Leases_Tables.List) is

   begin -- Write_Table
      Put_Line (Version_String);
      Put_Line ("current dnsmasq leases:" & Length (Leases_Table)'img &
                " at " & Time_String);
      Put_Line ("IP Address      MAC Address       Time     Host Name");
      for I in Iterate (Leases_Table) loop
         Put_Line (IP_String (Leases_Table (I).IP_Address) & " " &
                     MAC_String (Leases_Table (I).MAC_Address) & " " &
                     Time_String (Leases_Table (I).Lease_End) & " " &
                     To_String (Leases_Table (I).Host_Name));
      end loop; -- I in Iterate (Leases_Table)
   end Write_Table;

   Sort_Type : Character := 'i';
   Leases_List : Leases_Lists.List;
   Leases_Table : Leases_Tables.List;

begin -- Leases
   if Argument_Count = 1 then
      Sort_Type := Argument (1) (1);
   end if; -- Argument_Count = 1
   Read_List (Leases_List);
   Build_Table (Leases_List, Leases_Table);
   case Sort_Type is
      when 'h' | 'H' =>
         Host_Sort.Sort (Leases_Table);
         Write_Table (Leases_Table);
      when 'i' | 'I' =>
         IP_Sort.Sort (Leases_Table);
         Write_Table (Leases_Table);
      when 'm' | 'M' =>
         MAC_Sort.Sort (Leases_Table);
         Write_Table (Leases_Table);
      when 't' | 'T' =>
         Time_Sort.Sort (Leases_Table);
         Write_Table (Leases_Table);
      when others =>
         Put_Line ("Unknown sort type '" & Sort_Type & "'");
         Put_Line ("Usage: leases {h | i | m | t}");
         Put_Line ("h or H: sort by host name");
         Put_Line ("i or I: sort by IP addres (default option)");
         Put_Line ("m or M: sort by MAC address");
         Put_Line ("t or T: sort by lease time");
         IP_Sort.Sort (Leases_Table);
   end case; -- Sort_Type
end Leases;

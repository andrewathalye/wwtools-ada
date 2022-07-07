with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions; use Ada.Exceptions;

with Interfaces; use Interfaces;

with WWHash; use WWHash;
with Util; use Util;

procedure WWMap is
	Map_F : File_Type;

	package Hash_Maps is new Ada.Containers.Ordered_Maps
		(Key_Type => Hash_Type, Element_Type => String_Access);
begin
	Put_Line (Standard_Error, "WWMap v0.2");

	-- Check for required arguments
	if Argument_Count < 2 then
		Put_Line (Standard_Error, "Usage:"
			& " " & Command_Name
			& " " & "NAME_FILE"
			& " " & "HASHES");
		return;
	end if;

	declare
		File_Name : String renames Argument (1);
		Map : Hash_Maps.Map;
		Map_Failure : exception;
	begin
		-- Load all names into hash map
		Open (Map_F, In_File, File_Name);

		Read_Mapping :
		while not End_Of_File (Map_F) loop
			declare
				L : constant String := Get_Line (Map_F);
				Index : Natural := 0;
			begin
				Find_Separator :
				for I in L'Range loop
					if L (I) = ' ' then
						Index := I;
						exit Find_Separator;
					end if;
				end loop Find_Separator;

				-- Check for invalid line formats
				if Index = 0 then
					raise Map_Failure with "Could not find separator on line"
						& Count'Image (Line (Map_F));
				elsif Index = L'Last then
					raise Map_Failure with "Malformed line without hash on line"
						& Count'Image (Line (Map_F));
				end if;

				declare
					Name : constant String_Access := new String'(L (L'First .. Index - 1));
					Hash : constant Hash_Type := Hash_Type'Value (L (Index + 1 .. L'Last));
				begin
					Hash_Maps.Insert (Map, Hash, Name);
				exception
					when E : Constraint_Error =>
						raise Map_Failure with "Could not add line "
							& L & ": "
							& Exception_Message (E);
				end;
			exception
				when E : Map_Failure =>
					Put_Line (Standard_Error, Exception_Message (E));
					return;
			end;
		end loop Read_Mapping;

		Close (Map_F);

		-- Map arguments based upon map file
		Map_Arguments :
		for I in 2 .. Argument_Count loop
			declare
				Hash : constant Hash_Type := Hash_Type'Value (Argument (I));
				C : constant Hash_Maps.Cursor :=
					Hash_Maps.Find (Map, Hash);
			begin
				-- If we have a name
				if Hash_Maps.Has_Element (C) then
					Put_Line (Hash_Maps.Element (C).all);
				else
					-- If this is a Destiny 2 progression identifier
					if (Hash and 16#00FFF000#) = 16#00ED3000# then
						Put_Line ("__stage"
						& "_" & To_Hex (Unsigned_8 (Shift_Right (Hash, 24)))
							-- Stage Identifier
						& "_progress_" &  To_Hex (Unsigned_12 (Hash and 16#FFF#)));
							-- Progress Identifier
					else
						Put_Line (Argument (I));
					end if;

				end if;
			end;
		end loop Map_Arguments;

	end;
end WWMap;

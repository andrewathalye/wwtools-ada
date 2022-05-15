with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with WWHash; use WWHash;
with Interfaces; use Interfaces;

procedure WWDict is
	-- Define hash task
	task type Hash_Task is
		entry Load (Ext_Prefix : String; Ext_Dictionary : String_Array;
			Ext_Hashes : Hash_Array);
		entry Run (Ext_W1 : String);
	end Hash_Task;

	-- Implement hash task
	task body Hash_Task is
		Dictionary : String_Array_Access;
		Hashes : Hash_Array_Access;
		W1 : String_Access;
		Prefix : String_Access;
		Result : Hash_Type;
		Base_Hash : Hash_Type;
	begin
		-- Load dictionary and hashes to match against
		select
			accept Load (Ext_Prefix : String; Ext_Dictionary : String_Array;
				Ext_Hashes : Hash_Array)
			do
				Prefix := new String'(Ext_Prefix);
				Dictionary := new String_Array'(Ext_Dictionary);
				Hashes := new Hash_Array'(Ext_Hashes);
			end Load;
		or
			terminate;
		end select;

		loop
			select
				accept Run (Ext_W1 : String) do
					W1 := new String'(Ext_W1);
				end Run;

				-- Check for single-word result
				Result := Calculate_Hash (Prefix.all & W1.all);
				for R of Hashes.all loop
					if Result = R then
						Put_Line (Prefix.all & W1.all & Hash_Type'Image (R));
					end if;
				end loop;

				-- Setup base hash for speed improvement
				Base_Hash := Calculate_Hash (Result, "_");

				-- Check all possible multiwords
				for W2 of Dictionary.all loop
					Result := Calculate_Hash (Base_Hash, W2.all);
					for R of Hashes.all loop
						if Result = R then
							Put_Line (Prefix.all & W1.all & "_" & W2.all & Hash_Type'Image (R));
						end if;
					end loop;

					-- TODO: Possibly very slow check for digit in third position
--					for W3 of Digit_Array loop
--						Result := Calculate_Hash (W1.all & "_" & W2.all & "_" & W3);
--						for R of Hashes.all loop
--							if Result = R then
--								Put_Line (W1.all & "_" & W2.all & "_" & W3 & Hash_Type'Image (R));
--							end if;
--						end loop;
--					end loop;
				end loop;
			or
				terminate;
			end select;
		end loop;
	end Hash_Task;

	Hash_Tasks : array (1 .. 11) of Hash_Task;

	-- Show usage message
	procedure Show_Usage is
	begin
		Put_Line (Standard_Error, "Usage: "
			& Command_Name
			& " prefix"
			& " dictionary_file"
			& " list_of_hashes");
	end Show_Usage;
begin
	Put_Line (Standard_Error, "WWDict v0.1");

	-- Check arguments
	if Argument_Count < 3 then
		Show_Usage;
		return;
	end if;

	-- Load words and targets
	declare
		Prefix : String renames Argument (1);
		Dictionary_File : String renames Argument (2);
		Dictionary_Entries : Positive;
		DF : File_Type;

		Hashes : Hash_Array (1 .. Argument_Count - 2);
	begin
		-- Open dictionary and read number of entries
		Open (DF, In_File, Dictionary_File);
		while not End_Of_File (DF) loop
			declare
				Discard : String := Get_Line (DF);
			begin
				null;
			end;
		end loop;
		Dictionary_Entries := Positive (Line (DF)) - 1;
		Close (DF);
		Open (DF, In_File, Dictionary_File);

		-- Load target hashes
		for I in Hashes'Range loop
			Hashes (I) := Hash_Type'Value (Argument (I + 2));
		end loop;

		-- Load dictionary words
		declare
			Dictionary : String_Array (1 .. Dictionary_Entries);
		begin
			for SA of Dictionary loop
				SA := new String'(To_Lower (Get_Line (DF)));
			end loop;
			Close (DF);

			-- Setup hash tasks
			for T of Hash_Tasks loop
				T.Load (Prefix, Dictionary, Hashes);
			end loop;

			-- Begin dictionary attack
			for W1 of Dictionary loop
				-- TODO: Use parallel for when available
				Outer :
				loop
					for T of Hash_Tasks loop
						select
							T.Run (W1.all);
							exit Outer;
						else
							null;
						end select;
					end loop;
					delay 0.001;
				end loop Outer;
			end loop;
		end;
	end;

end WWDict;

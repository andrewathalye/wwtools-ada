with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with WWHash; use WWHash;
with Interfaces; use Interfaces;

procedure WWDict is
	-- Define hash task
	task type Hash_Task is
		entry Load (Ext_Prefix : String; Ext_Suffix : String; Ext_Words : Positive;
			Ext_Dictionary : String_Array; Ext_Hashes : Hash_Array);
		entry Run (Ext_W : String);
	end Hash_Task;

	-- Implement hash task
	task body Hash_Task is
		-- Variables
		Dictionary : String_Array_Access;
		Hashes : Hash_Array_Access;
		W : String_Access;
		Prefix : String_Access;
		Suffix : String_Access;
		New_Hash : Hash_Type;
		Words : Positive;

		Full_String : String_Array_Access;

		-- Subprogram
		procedure Dict_Attack (Base_Hash : Hash_Type; Attack_Length : Positive) is
			New_Hash : Hash_Type;
		begin
			for W of Dictionary.all loop
				New_Hash := Calculate_Hash (Base_Hash, "_" & W.all);

				-- Loop and identify matches
				for R of Hashes.all loop
					if New_Hash = R then
						Put (Prefix.all);
						for SA of Full_String (1 .. Words - Attack_Length) loop
							Put (SA.all & "_");
						end loop;
						Put_Line (W.all
							& Suffix.all
							& Hash_Type'Image (Calculate_Hash (R, Suffix.all)));
					end if;
				end loop;

				-- Continue attack if necessary
				if Attack_Length /= 1 then
					Full_String (Words - (Attack_Length - 1)) := W;
					Dict_Attack (New_Hash, Attack_Length - 1);
				end if;
			end loop;
		end Dict_Attack;
	begin
		-- Load dictionary and hashes to match against
		select
			accept Load (Ext_Prefix : String; Ext_Suffix : String; Ext_Words : Positive;
				Ext_Dictionary : String_Array; Ext_Hashes : Hash_Array)
			do
				Prefix := new String'(Ext_Prefix);
				Suffix := new String'(Ext_Suffix);
				Words := Ext_Words;
				Dictionary := new String_Array'(Ext_Dictionary);
				Hashes := new Hash_Array'(Ext_Hashes);
			end Load;
		or
			terminate;
		end select;

		loop
			select
				accept Run (Ext_W : String) do
					W := new String'(Ext_W);
				end Run;

				-- Check for single-word result
				New_Hash := Calculate_Hash (Prefix.all & W.all);
				for R of Hashes.all loop
					if New_Hash = R then
						Put_Line (Prefix.all
							& W.all
							& Suffix.all
							& Hash_Type'Image (Calculate_Hash (R, Suffix.all)));
					end if;
				end loop;

				-- Continue attack if necessary
				if Words /= 1 then
					Full_String := new String_Array (1 .. Words);
					Full_String (1) := W;
					Dict_Attack (New_Hash, Words - 1);
					Free (Full_String);
				end if;
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
			& " number_of_words"
			& " dictionary_file"
			& " list_of_hashes");
	end Show_Usage;
begin
	Put_Line (Standard_Error, "WWDict v0.3");

	-- Check arguments
	if Argument_Count < 5 then
		Show_Usage;
		return;
	end if;

	-- Load words and targets
	declare
		Prefix : String renames Argument (1);
		Suffix : String renames Argument (2);
		Words : String renames Argument (3);
		Dictionary_File : String renames Argument (4);
		Dictionary_Entries : Positive;
		DF : File_Type;

		Hashes : Hash_Array (1 .. Argument_Count - 4);
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
		-- If a Suffix is present, return the hash prior to the suffix
		for I in Hashes'Range loop
			Hashes (I) := Reverse_Hash (Hash_Type'Value (Argument (I + 4)), Suffix);
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
				T.Load (Prefix, Suffix, Positive'Value (Words), Dictionary, Hashes);
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

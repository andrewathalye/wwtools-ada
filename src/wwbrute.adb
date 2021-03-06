with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with WWHash; use WWHash;

procedure WWBrute is
	-- Constants
	type Character_Array is array (Positive range <>) of Character;
	Characters : constant Character_Array := ('a', 'b', 'c', 'd', 'e', 'f', 'g',
		'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',
		'v', 'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8',
		'9', '_');

	-- Exclude List Type
	type Position is (Beginning, Other);
	type Exclude_Array is array (Position, Character, Character) of Boolean;

	-- Nothing will be excluded by default (set all entries in 3D array to False)
	Exclude_Bigrams : Exclude_Array := [others => [others => [others => False]]];

	-- Load exclude list from text file
	procedure Load_Exclude_List (File_Name : String) is
		EF : File_Type;
	begin
		Open (EF, In_File, File_Name);
		while not End_Of_File (EF) loop
			declare
				L : constant String := Get_Line (EF);
				C : constant Character := L (1);
			begin
				case C is
					when '#' => null; -- Comment line
					when '^' =>
						for C of L (L'First + 3 .. L'Last - 1) loop
							Exclude_Bigrams (Beginning, L (L'First + 1), C) := True;
						end loop;
					when others =>
						for C of L (L'First + 2 .. L'Last - 1) loop
							Exclude_Bigrams (Other, L (L'First), C) := True;
						end loop;
				end case;
			end;
		end loop;
		Close (EF);
	end Load_Exclude_List;

	-- Task Types
	task type Hash_Task is
		entry Load (Ext_Prefix : String; Ext_Suffix : String;
			Ext_Attack_Length : Natural; Ext_Hashes : Hash_Array);
		entry Run (Ext_C : Character);
	end Hash_Task;

	-- Task Implementation
	task body Hash_Task is
		Attack_Length : Natural;
		Hashes : Hash_Array_Access;
		Base_Hash : Hash_Type;
		Initial_C : Character;
		S : String_Access;
		Prefix : String_Access;
		Suffix : String_Access;

		-- Generic attack implementation
		procedure Attack (Base_Hash : Hash_Type; Attack_Length : Natural)
		is
			New_Hash : Hash_Type;
		begin
			-- Check if it is possible to find hash on final iteration
			if Attack_Length = 1 then
				Outer : loop -- Will run at most once
					for R of Hashes.all loop
						if ((Base_Hash * Multiplier)
							and 16#FFFFFF00#) = (R and 16#FFFFFF00#)
						then
							exit Outer;
						end if;
					end loop;
					return;
				end loop Outer;
			end if;

			for C of Characters loop
				Inner :
				loop
					-- Check to ensure that bigram combination is acceptable before nesting
					if Exclude_Bigrams (Other, S (S'Last - Attack_Length), C) then
						exit Inner;
					end if;

					-- Calculate new hash and update string
					New_Hash := (Base_Hash * Multiplier) xor Character'Pos (C);
					S (S'Last - (Attack_Length - 1)) := C;

					-- Check to see if hash was cracked
					for R of Hashes.all loop
						if R = New_Hash then
							Put_Line (Prefix.all
								& S.all (1 .. S'Last - (Attack_Length - 1))
								& Suffix.all
								& Hash_Type'Image (Calculate_Hash (New_Hash, Suffix.all)));
						end if;
					end loop;

					-- Launch next attack level
					if Attack_Length /= 1 then
						Attack (New_Hash, Attack_Length - 1);
					end if;

					exit Inner;
				end loop Inner;
			end loop;
		end Attack;

		-- Special implementation for first attack
		procedure First_Attack (Base_Hash : Hash_Type; Attack_Length : Natural) is
			New_Hash : Hash_Type;
		begin
			for C of Characters loop
				Inner :
				loop
					-- Check to ensure that bigram combination is acceptable before nesting
					if Exclude_Bigrams (Beginning, S (S'Last - Attack_Length), C) then
						exit Inner;
					end if;

					-- Calculate new hash and update string
					New_Hash := (Base_Hash * Multiplier) xor Character'Pos (C);
					S (S'Last - (Attack_Length - 1)) := C;

					-- Check to see if hash was cracked
					for R of Hashes.all loop
						if R = New_Hash then
							Put_Line (Prefix.all
								& S.all (1 .. 2)
								& Suffix.all
								& Hash_Type'Image (Calculate_Hash (New_Hash, Suffix.all)));
						end if;
					end loop;

					-- Launch next attack level
					if Attack_Length /= 1 then
						Attack (New_Hash, Attack_Length - 1);
					end if;

					exit Inner;
				end loop Inner;
			end loop;
		end First_Attack;
	begin
		select
			accept Load (Ext_Prefix : String; Ext_Suffix : String;
				Ext_Attack_Length : Natural; Ext_Hashes : Hash_Array)
			do
				Attack_Length := Ext_Attack_Length;
				Hashes := new Hash_Array'(Ext_Hashes);
				Prefix := new String'(Ext_Prefix);
				Suffix := new String'(Ext_Suffix);
			end Load;
		or
			terminate;
		end select;

		loop
			select
				accept Run (Ext_C : Character) do
					Base_Hash := Calculate_Hash (Prefix.all & Ext_C);
					Initial_C := Ext_C;
				end Run;

				-- If base hash matches
				for R of Hashes.all loop
					if Base_Hash = R then
						Put_Line (Prefix.all
							& Initial_C
							& Suffix.all
							& Hash_Type'Image (Calculate_Hash (Base_Hash, Suffix.all)));
					end if;
				end loop;

				declare
					-- Initialise all letters to placeholder value
					New_S : constant String (1 .. Attack_Length) := [others => '#'];
				begin
					S := new String'(Initial_C & New_S);

					-- If using a prefix, the "first" letter is really in the middle.
					if Prefix.all = "" then
						First_Attack (Base_Hash, Attack_Length);
					else
						Attack (Base_Hash, Attack_Length);
					end if;
				end;

				Free (S);
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
			& " suffix"
			& " attack_length"
			& " list_of_hashes");
	end Show_Usage;

begin
	Put_Line (Standard_Error, "WWBrute v0.3");

	-- Check arguments
	if Argument_Count < 4 then
		Show_Usage;
		return;
	end if;

	-- Setup information
	declare
		Prefix : String renames Argument (1);
		Suffix : String renames Argument (2);
		Attack_Length : constant Natural := Natural'Value (Argument (3));
		Hashes : Hash_Array (1 .. Argument_Count - 3);
	begin
		-- Load hashes
		for I in Hashes'Range loop
			Hashes (I) := Reverse_Hash (Hash_Type'Value (Argument (I + 3)), Suffix);
		end loop;

		-- Setup tasks
		for T of Hash_Tasks loop
			T.Load (Prefix, Suffix, Attack_Length - 1, Hashes);
		end loop;

		-- Load exclude list
		Load_Exclude_List ("dict/exclude.lst");

		-- Begin brute force attack
		for C of Characters loop
			Outer :
			loop
				for T of Hash_Tasks loop
					select
						T.Run (C);
						exit Outer;
					else
						null;
					end select;
				end loop;
				delay 0.01;
			end loop Outer;
		end loop;
	end;
end WWBrute;

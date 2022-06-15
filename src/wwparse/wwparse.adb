with Ada.Text_IO; use Ada.Text_IO; use Ada;
with Ada.Directories; use Ada.Directories;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line; use Ada.Command_Line;

with Bank; use Bank;
with Hierarchy; use Hierarchy;

procedure WWParse is
	-- Possible errors
	Invalid_Bank_Exception : exception;
	Chunk_Exception : exception;
begin
	Put_Line ("WWParse v0.1");
	Put_Line ("Usage: " & Command_Name & " INPUT_DIR OUTPUT_DIR");

	if Argument_Count < 2 then
		return;
	end if;

	-- Start Searching for BNK files
	declare
		SE : Search_Type;
		DE : Directory_Entry_Type;
		In_F : Stream_IO.File_Type;
		In_S : Stream_Access;
		Out_F : Text_IO.File_Type;
	begin
		Start_Search (SE, Argument (1), "*.bnk");
		while More_Entries (SE) loop
			Get_Next_Entry (SE, DE);
			Open (In_F, In_File, Full_Name (DE));
			In_S := Stream (In_F);
			Create (Out_F, Out_File, Argument (2)
				& "/"
				& Base_Name (Full_Name (DE)) & ".txt");

			Put_Line (Full_Name (DE));

			-- Begin reading chunks
			declare
				BCH : Chunk_Header_Type (First_Chunk => True);
			begin
				-- Read chunk header for first chunk and check type
				Chunk_Header_Type'Read (In_S, BCH);
				Put_Line (Chunk_Header_Type'Image (BCH));
				if BCH.Identifier /= Bank_Header then
					raise Invalid_Bank_Exception;
				end if;

				-- Read bank header
				declare
					BH : Bank_Header_Type (BCH.Version);
					CH : Chunk_Header_Type;
				begin
					Bank_Header_Type'Read (In_S, BH);
					Put_Line (Bank_Header_Type'Image (BH));

					loop
						Chunk_Header_Type'Read (In_S, CH);
						Put_Line (Chunk_Header_Type'Image (CH));
						case CH.Identifier is
							when Bank.Hierarchy => -- Read hierarchy type
								declare
									HH : Hierarchy_Header_Type;
									HOH : Hierarchy_Object_Header (BCH.Version);
								begin
									Hierarchy_Header_Type'Read (In_S, HH);

									-- Read each hierarchy object
									for I in 1 .. HH.Releasable_Items loop
										Hierarchy_Object_Header'Read (In_S, HOH);
										declare
											HO : Hierarchy_Object (
												BCH.Version,
												HOH.Identifier,
												HOH.Section_Size);
										begin
											Hierarchy_Object'Read (In_S, HO);
											Put_Line (Hierarchy_Object'Image (HO));
										end;
									end loop;
								end;
							when others =>
								raise Chunk_Exception with "Unable to process type: "
									& Chunk_Identifier_Type'Image (CH.Identifier);
						end case;
					end loop;
				end;
			exception
				when Invalid_Bank_Exception =>
					Put_Line (Standard_Error, "Input file "
						& Full_Name (DE)
						& "is not a supported bank file.");
					Close (In_F);
					Close (Out_F);
				when CE : Chunk_Exception =>
					Put_Line (Standard_Error, "Aborted reading input file"
						& Full_Name (DE)
						& " due to " & Exception_Message (CE));
					Close (In_F);
					Close (Out_F);
				when E : others =>
					Put_Line (Standard_Error, "Unknown error: "
						& Exception_Message (E));
					Close (In_F);
					Close (Out_F);
			end;

			exit; -- TODO Debug, test one bank at time
		end loop;
		End_Search (SE);
	end;
end WWParse;

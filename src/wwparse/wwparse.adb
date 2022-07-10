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
	Put_Line ("WWParse v0.2");

	if Argument_Count < 2 then
		Put_Line (Standard_Error,
			"Usage: "
			& Command_Name
			& " INPUT_DIR OUTPUT_DIR");
		return;
	end if;

	-- Start Searching for BNK files
	declare
		-- Renames
		Input_Dir : String renames Argument (1);
		Output_Dir : String renames Argument (2);

		-- Variables
		SE : Search_Type;
		DE : Directory_Entry_Type;
		Input_File : Stream_IO.File_Type;
		Input_Stream : Stream_Access;
		Output_File : Text_IO.File_Type;
	begin
		Start_Search (SE, Input_Dir, "*.bnk");

		Read_Entries :
		while More_Entries (SE) loop
			Get_Next_Entry (SE, DE);
			Open (Input_File, In_File, Full_Name (DE));
			Input_Stream := Stream (Input_File);

			-- Create output directory if necessary
			if not Exists (Output_Dir) then
				Create_Directory (Output_Dir);
			end if;

			Create (
				Output_File,
				Out_File,
				Output_Dir
				& "/" & Base_Name (Full_Name (DE)) & ".txt");

			Put_Line (Full_Name (DE));

			-- Begin reading chunks
			declare
				Bank_CH : Chunk_Header_Type (First_Chunk => True);
			begin
				-- Read chunk header for first chunk and check type
				Chunk_Header_Type'Read (Input_Stream, Bank_CH);
				Put_Line (Bank_CH'Image);

				if Bank_CH.Identifier /= Bank_Header then
					raise Invalid_Bank_Exception;
				end if;

				-- Read bank header
				declare
					Bank_H : Bank_Header_Type (Bank_CH.Version);
					Chunk_H : Chunk_Header_Type (First_Chunk => False);
				begin
					Bank_Header_Type'Read (Input_Stream, Bank_H);
					Put_Line (Bank_Header_Type'Image (Bank_H));

					Read_Chunks :
					while Size (Input_File) - Index (Input_File)
						>= Chunk_Header_Type'Size / 8
					loop
						Chunk_Header_Type'Read (Input_Stream, Chunk_H);
						Put_Line (Chunk_H'Image);
						case Chunk_H.Identifier is
							when Bank.Hierarchy => -- Read hierarchy type
								declare
									Hierarchy_H : Hierarchy_Header_Type;
									Hierarchy_Object_H : Hierarchy_Object_Header (Bank_CH.Version);
								begin
									Hierarchy_Header_Type'Read (Input_Stream, Hierarchy_H);

									-- Read each hierarchy object
									Read_Hierarchy :
									for I in 1 .. Hierarchy_H.Releasable_Items loop
										Hierarchy_Object_Header'Read (Input_Stream, Hierarchy_Object_H);
										declare
											Hierarchy_O : Hierarchy_Object (
												Bank_CH.Version,
												Hierarchy_Object_H.Identifier,
												Hierarchy_Object_H.Section_Size);
										begin
											Hierarchy_Object'Read (Input_Stream, Hierarchy_O);
											Put_Line ("Identifier: "
												& Hierarchy_O.Identifier'Image);
											Put_Line ("Object_ID:"
												& Hierarchy_O.Object_ID'Image);

											-- Print specific info for supported types
											case Hierarchy_O.Identifier is
												when Event =>
													Put_Line ("Action ID List: "
														& Hierarchy_O.Action_ID_List.all'Image);
												when Action =>
													Put_Line ("Action Type: "
														& Hierarchy_O.Action_Type'Image);
													Put_Line ("Action Controller ID: "
														& Hierarchy_O.Action_Controller_ID'Image);
													Put_Line ("Action Bits: "
														& Hierarchy_O.Action_Bits'Image);
													Put_Line ("Action Properties: "
														& Hierarchy_O.Action_Properties.all'Image);
													Put_Line ("Action Modifiers: "
														& Hierarchy_O.Action_Modifiers.all'Image);
													Put_Line ("Action Specifics: "
														& Hierarchy_O.Action_Specifics.all'Image);
												when Music_Track =>
													Put_Line (Hierarchy_O'Image);
												when others => null;
											end case;

											New_Line;
										end;
									end loop Read_Hierarchy;
								end;
							when others =>
								raise Chunk_Exception with "Unable to process type: "
									& Chunk_H.Identifier'Image;
						end case;
					end loop Read_Chunks;
				end;
			exception
				when Invalid_Bank_Exception =>
					Put_Line (Standard_Error, "Input file "
						& Full_Name (DE)
						& "is not a supported bank file.");
					Close (Input_File);
					Close (Output_File);
				when CE : Chunk_Exception =>
					Put_Line (Standard_Error, "Aborted reading input file"
						& Full_Name (DE)
						& " due to " & Exception_Message (CE));
					Close (Input_File);
					Close (Output_File);
			end;

			exit Read_Entries; -- TODO Debug, test one bank at time
		end loop Read_Entries;
		End_Search (SE);
	end;
end WWParse;

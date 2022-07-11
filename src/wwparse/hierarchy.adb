with Ada.Text_IO; use Ada.Text_IO;

with Types; use Types;

package body Hierarchy is
	procedure Read_Hierarchy_Object (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Hierarchy_Object)
	is
		-- Size of the Object less the Object_ID, which is universally supported
		Discard : Discard_Array (1 .. Natural (Item.Size) - (Unsigned_32'Size / 8));
	begin
		Unsigned_32'Read (Stream, Item.Object_ID);

		case Item.Identifier is
			when Event =>
				declare
					Action_ID_List_Size_U32 : Unsigned_32;
					Action_ID_List_Size_Var : Unsigned_8; -- TODO: Should be a var type
					Action_ID_List_Size : Natural;
				begin
					-- The list size is stored in a different format before D2WQ
					case Item.Version is
						when D1RI | D2SK =>
							Unsigned_32'Read (Stream, Action_ID_List_Size_U32);
							Action_ID_List_Size := Natural (Action_ID_List_Size_U32);
						when D2WQ =>
							Unsigned_8'Read (Stream, Action_ID_List_Size_Var);
							Action_ID_List_Size := Natural (Action_ID_List_Size_Var);
					end case;

					-- Create the array and setup the list
					Item.Action_ID_List := new Action_ID_Array (1 .. Action_ID_List_Size);
					Action_ID_Array'Read (Stream, Item.Action_ID_List.all);
				end;
			when Action =>
				-- Read General Action Details
				Action_Type_Type'Read (Stream, Item.Action_Type);
				Unsigned_32'Read (Stream, Item.Action_Controller_ID);
				Unsigned_8'Read (Stream, Item.Action_Bits);

				-- Read Action Properties and Action Modifiers
				declare
					Action_Properties_Length : Unsigned_8;
					Action_Modifiers_Length : Unsigned_8;
				begin
					Unsigned_8'Read (Stream, Action_Properties_Length);

					Item.Action_Properties := new Property_Array_CU8_IU8_VU32
						(1 .. Action_Properties_Length);
					Property_Array_CU8_IU8_VU32'Read (
						Stream,
						Item.Action_Properties.all);

					Unsigned_8'Read (Stream, Action_Modifiers_Length);
					Item.Action_Modifiers := new Ranged_Property_Array_CU8_IU8_VU32
						(1 .. Action_Modifiers_Length);
					Ranged_Property_Array_CU8_IU8_VU32'Read (
						Stream,
						Item.Action_Modifiers.all);
				end;

				-- Read Action Specifics
				Item.Action_Specifics := new Action_Specifics_Type (
					Item.Version,
					Item.Action_Type);
				Action_Specifics_Type'Read (Stream, Item.Action_Specifics.all);
			when Music_Track =>
				-- Music_Flags is absent before D2SK
				case Item.Version is
					when D1RI => null;
					when D2SK | D2WQ =>
						Unsigned_8'Read (Stream, Item.Music_Flags);
				end case;

				-- Read Source Array
				declare
					Sources_Count : Unsigned_32;
				begin
					Unsigned_32'Read (Stream, Sources_Count);
					Item.Music_Source_List := new Source_Container (
						Version => Item.Version,
						Length => Natural (Sources_Count));
				end;
				Source_Container'Read (Stream, Item.Music_Source_List.all);

				-- Read Playlist Array
				declare
					Playlists_Count : Unsigned_32;
				begin
					Unsigned_32'Read (Stream, Playlists_Count);
					Item.Playlist_List := new Playlist_Container (
						Version => Item.Version,
						Length => Natural (Playlists_Count));
				end;
				Playlist_Container'Read (Stream, Item.Playlist_List.all);

				-- Read Clip Automation List
				declare
					Clips_Count : Unsigned_32;
				begin
					Unsigned_32'Read (Stream, Clips_Count);
					Item.Clip_Automation_List := new Clip_Array (1 .. Natural (Clips_Count));
				end;
				Clip_Array'Read (Stream, Item.Clip_Automation_List.all);

				Parameter_Node'Read (Stream, Item.Music_Track_Parameter_Node);

				-- D1RI has Random Sequence Type, while D2SK and D2WQ have Track Type
				-- Music Switch Params and Transition Params are only in D2SK and D2WQ
				case Item.Version is
					when D1RI =>
						Unsigned_32'Read (Stream, Item.Random_Sequence_Type);
					when D2SK | D2WQ =>
						Music_Track_Type'Read (Stream, Item.Track_Type);
						Music_Switch_Parameters'Read (Stream, Item.Music_Switch_Params);
						Transition_Parameters'Read (Stream, Item.Transition_Params);
				end case;

				Unsigned_32'Read (Stream, Item.Look_Ahead_Time);
			when others => -- Items without specific support
				Discard_Array'Read (Stream, Discard);
		end case;
	end Read_Hierarchy_Object;

end Hierarchy;

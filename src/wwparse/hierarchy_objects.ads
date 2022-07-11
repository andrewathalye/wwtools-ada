with Ada.Streams;
with Interfaces; use Interfaces;

with Types; use Types;
with Bank; use Bank;

package Hierarchy_Objects is
	-- Enumerations
	type Music_Track_Type is (Normal, Random, Sequence, Switch);
	for Music_Track_Type'Size use 8;
	for Music_Track_Type use (
		Normal => 16#0#,
		Random => 16#1#,
		Sequence => 16#2#,
		Switch => 16#3#);

	-- Unimplemented types
	type Parameter_Node (Version : Bank_Version_Type) is null record; -- TODO Implement
	type Switch_List is null record; -- TODO Find a better name
	type Switch_Parameters is null record; -- TODO Find a better name
	type Music_Node_Type is null record;


	subtype Fade_Type_32 is Unsigned_32; -- TODO Add names

	-- Simple Parameter Types
	-- TODO Implement
	type Music_Switch_Parameters is record
		Group_Type : Unsigned_8; -- TODO Add names
		Group_ID : FNV_Hash;
		Default_Switch : FNV_Hash;
		Associated_Switches : FNV_Hash_Array_Access;
	end record
	with
		Read => Read_Music_Switch_Parameters;

	type Transition_Parameters is record
		Source_Transition_Time : Integer_32;
		Source_Fade_Curve : Fade_Type_32;
		Source_Fade_Offset : Integer_32;
		Sync_Type : Unsigned_32; -- TODO Add names
		Cue_Filter_Hash : Unsigned_32;
		Dest_Transition_Time : Integer_32;
		Dest_Fade_Curve : Fade_Type_32;
		Dest_Fade_Offset : Integer_32;
	end record;

	-- TODO Implement
	-- Complex :(
	type Decision_Tree is null record;

	-- TODO Implement
	type Marker_Type is record
		Identifier : Unsigned_32;
		Name : String_Access;
	end record
	with
		Read => Read_Marker_Type;

	type Marker_Array is array (Natural range <>) of Marker_Type;

	-- TODO Implement
	type Stinger_Type is record
		Trigger_ID : FNV_Hash;
		Segment_ID : FNV_Hash;
		Sync_Type : Unsigned_32;
		Cue_Filter_Hash : Unsigned_32;
		Repeat_Exclude_Duration : Unsigned_32;
		Segment_Look_Ahead : Boolean_32;
	end record;

	type Stinger_Array is array (Natural range <>) of Stinger_Type;

	-- RTPC Graphs
	type RTPC_Point is record
		From : Unsigned_32;
		To : Unsigned_32;
		Interpolation : Unsigned_32; -- TODO add names
	end record;

	type RTPC_Graph is array (Natural range <>) of RTPC_Point;
	type RTPC_Graph_Access is access RTPC_Graph;

	-- Clips
	type Clip_Type is record
		Clip_Index : Unsigned_32;
		Clip_Type : Unsigned_32; -- TODO add names
		Clip_Graph : RTPC_Graph_Access;
	end record
	with
		Read => Read_Clip_Type;
	type Clip_Array is array (Natural range <>) of Clip_Type;
	type Clip_Array_Access is access Clip_Array;

	-- Plugin Info
--	-- Custom 'Read
	type Plugin_Info_Type is record
		Plugin_ID : Unsigned_16;
		Plugin_Vendor : Unsigned_12;
		Plugin_Type : Unsigned_4;
	end record
	with
		Pack => True;
	for Plugin_Info_Type'Size use 32;

	-- Sources
	type Source_Type (Version : Bank_Version_Type) is record
		Plugin_Info : Plugin_Info_Type;
		Stream_Type : Unsigned_32; -- May actually be U8 after D1RI
		Source_ID : Unsigned_32; -- TODO check if FNV
		File_ID : Unsigned_32 := 0; -- Absent with D2WQ
		File_Offset :  Unsigned_32 := 0; -- Absent if built-in
		In_Memory_Media_Size : Unsigned_32;
		Source_Bits : Unsigned_8; -- TODO add print
	end record
	with
		Read => Read_Source_Type;

	type Source_Array_D1RI is array (Natural range <>)
		of Source_Type (Version => D1RI);
	type Source_Array_D2SK is array (Natural range <>)
		of Source_Type (Version => D2SK);
	type Source_Array_D2WQ is array (Natural range <>)
		of Source_Type (Version => D2WQ);

	type Source_Container (
		Version : Bank_Version_Type;
		Length : Natural)
	is record
		case Version is
			when D1RI =>
				Contents_D1RI : Source_Array_D1RI (1 .. Length);
			when D2SK =>
				Contents_D2SK : Source_Array_D2SK (1 .. Length);
			when D2WQ =>
				Contents_D2WQ : Source_Array_D2WQ (1 .. Length);
		end case;
	end record;

	type Source_Container_Access is access Source_Container;

	-- Playlist
	type Playlist_Type (Version : Bank_Version_Type) is record
		Track_ID : Unsigned_32;
		Source_ID : Unsigned_32;
		Event_ID : Unsigned_32 := 0; -- Only D2WQ
		Play_At : Float_64;
		Begin_Trim_Offset : Float_64;
		End_Trim_Offset : Float_64;
		Source_Duration : Float_64;
	end record
	with
		Read => Read_Playlist_Type;

	type Playlist_Array_D1RI_D2SK is array (Natural range <>)
		of Playlist_Type (D1RI);
	type Playlist_Array_D2WQ is array (Natural range <>)
		of Playlist_Type (D2WQ);

	type Playlist_Container (
		Version : Bank_Version_Type;
		Length : Natural)
	is record
		case Version is
			when D1RI | D2SK =>
				Contents_D1RI_D2SK : Playlist_Array_D1RI_D2SK (1 .. Length);
				case Length is
					when 0 => null;
					when others =>
						Subtrack_Number_D1RI_D2SK : Unsigned_32;
				end case;
			when D2WQ =>
				Contents_D2WQ : Playlist_Array_D2WQ (1 .. Length);
				case Length is
					when 0 => null;
					when others =>
						Subtrack_Number_D2WQ : Unsigned_32;
				end case;
		end case;
	end record;

	type Playlist_Container_Access is access Playlist_Container;

	-- Subprograms
	procedure Read_Marker_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Marker_Type);

--	procedure Read_Plugin_Info_Type (
--		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
--		Item : out Plugin_Info_Type);
--	for Plugin_Info_Type'Read use Read_Plugin_Info_Type;

	procedure Read_Source_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Source_Type);

	procedure Read_Playlist_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Playlist_Type);

	procedure Read_Clip_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Clip_Type);

	procedure Read_Music_Switch_Parameters (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Music_Switch_Parameters);

end Hierarchy_Objects;

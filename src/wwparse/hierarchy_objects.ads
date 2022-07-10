with Ada.Streams;
with Interfaces; use Interfaces;

with Types; use Types;

package Hierarchy_Objects is
	-- Unimplemented types
	type Parameter_Node is null record; -- TODO Implement
	type Switch_List is null record; -- TODO Find a better name
	type Switch_Parameters is null record; -- TODO Find a better name
	type Music_Node_Type is null record;

	type Clip is null record;
	type Clip_Array is array (Natural range <>) of Clip;

	type Music_Switch_Parameters is null record;
		-- TODO Find a better name, split?
	type Transition_Parameters is null record;
		-- TODO Find a better name, split?

	type Decision_Tree is null record;
	type Marker_Type is record
		Identifier : Unsigned_32;
		-- String_Size : Unsigned_32
		Name : String_Access;
	end record
	with
		Read => Read_Marker_Type;

	type Marker_Array is array (Natural range <>) of Marker_Type;

	type Stinger_Type is record
		Trigger_ID : FNV_Hash;
		Segment_ID : FNV_Hash;
		Sync_Type : Unsigned_32;
		Cue_Filter_Hash : Unsigned_32;
		Repeat_Exclude_Duration : Unsigned_32;
		Segment_Look_Ahead : Boolean_32;
	end record;

	type Stinger_Array is array (Natural range <>) of Stinger_Type;

	type Source_Type is null record; -- TODO Implement
	type Source_Array is array (Natural range <>) of Source_Type;

	type Playlist_Type is null record; -- TODO Implement
	type Playlist_Array is array (Natural range <>) of Playlist_Type;

	-- Subprograms
	procedure Read_Marker_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Marker_Type);

end Hierarchy_Objects;

with Ada.Streams; use Ada.Streams;
with Interfaces; use Interfaces;

with Bank; use Bank;

package Hierarchy is
	-- Hierarchy Types and Identifiers
	type Hierarchy_Identifier_Type is
		(Invalid, State, Sound, Action, Event, Random_Sequence, Switch, Actor_Mixer,
		Bus, Layer, Music_Segment, Music_Track, Music_Switch, Music_Random_Sequence,
		Attenuation, Dialogue_Event)
	with
		Size => 8;

	type Marker_Type is record
		Identifier : Unsigned_32;
		-- String_Size : Unsigned_32
		Name : String_Access;
	end record
	with
		Read => Read_Marker_Type;

	subtype Action_Type is Unsigned_32;
	type Action_Array is array (Natural range <>) of Action_Type;

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

	type Property_Bundle is null record; -- TODO Implement
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

	-- Hierarchy Objects
	type Hierarchy_Object_Header (Version : Bank_Version_Type) is record
		Identifier : Hierarchy_Identifier_Type;
		Section_Size : Unsigned_32;
	end record;

	type Hierarchy_Object (Version : Bank_Version_Type;
		Identifier : Hierarchy_Identifier_Type;
		Size : Unsigned_32)
	is record
		Object_ID : Unsigned_32;

		case Identifier is
			when State =>
				State_Properties : access Property_Bundle;
				-- TODO Impl
			when Sound =>
				Sound_Source : Source_Type;
				-- TODO: See if parse_plugin_params is needed
				Sound_Parameter_Node : Parameter_Node;
				-- TODO Impl
			when Action =>
				Action_Type : Unsigned_16;
				Action_ID : Unsigned_32;
				Action_Bits : Unsigned_8;
				Action_Properties : access Property_Bundle;
				Action_Modifiers : access Property_Bundle;
				-- TODO Impl
			when Event =>
				Action_List : access Action_Array;
				-- TODO Impl
			when Switch =>
				-- Parameter_Node
				Group_ID : Unsigned_32;
				Default_Switch : Unsigned_32;
				Continuous_Validation : Boolean_8;
				-- Parameter Node?
				Switches : access Switch_List;
				Switch_Params : access Switch_Parameters;
				-- TODO Impl
			when Music_Segment =>
				Music_Node : access Music_Node_Type;
				Duration : Long_Float;
				Marker_List : access Marker_Array;
			when Music_Track =>
				Music_Flags : Unsigned_8;
				Music_Source_List : access Source_Array;
				Playlist_List : access Playlist_Array;
				Clip_Automation_List : access Clip_Array;
				Parameters : Parameter_Node;
				Look_Ahead_Time : Unsigned_32;
				Music_Switch_Params : access Music_Switch_Parameters;
				Transition_Params : access Transition_Parameters;

				-- TODO Impl
			when Music_Switch =>
				Switch_Transition_Params : access Transition_Parameters;
				Continuous_Playback : Boolean_8;
				Arguments : access Music_Switch_Parameters;
				Tree : access Decision_Tree;
				-- TODO Impl
			when others => null;
		end case;
	end record
	with
		Read => Read_Hierarchy_Object;

	-- Subprograms
	procedure Read_Marker_Type (
		Stream : not null access Root_Stream_Type'Class;
		Item : out Marker_Type);

	procedure Read_Hierarchy_Object (
		Stream : not null access Root_Stream_Type'Class;
		Item : out Hierarchy_Object);

end Hierarchy;

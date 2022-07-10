with Ada.Streams;
with Interfaces; use Interfaces;

with Bank; use Bank;

with Hierarchy_Objects; use Hierarchy_Objects;
with Hierarchy_Objects.Actions; use Hierarchy_Objects.Actions;
with Hierarchy_Objects.Property_Arrays;
	use Hierarchy_Objects.Property_Arrays;

package Hierarchy is
	-- Hierarchy Types and Identifiers
	type Hierarchy_Identifier_Type is
		(Invalid, State, Sound, Action, Event, Random_Sequence, Switch, Actor_Mixer,
		Bus, Layer, Music_Segment, Music_Track, Music_Switch, Music_Random_Sequence,
		Attenuation, Dialogue_Event)
	with
		Size => 8;

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
--			when State =>
--				State_Properties : access Property_Bundle;
--				-- TODO Impl
--			when Sound =>
--				Sound_Source : Source_Type;
--				-- TODO: See if parse_plugin_params is needed
--				Sound_Parameter_Node : Parameter_Node;
--				-- TODO Impl
			when Action =>
				Action_Type : Action_Type_Type;
				Action_External_ID : Unsigned_32;
				Action_Bits : Unsigned_8; -- TODO Create printable type
				Action_Properties : Property_Array_CU8_IU8_VU32_Access;
				Action_Modifiers : Ranged_Property_Array_CU8_IU8_VU32_Access;
				Action_Specifics : Action_Specifics_Access;
			when Event =>
				Action_ID_List : Action_ID_Array_Access;
--			when Switch =>
--				-- Parameter_Node
--				Group_ID : Unsigned_32;
--				Default_Switch : Unsigned_32;
--				Continuous_Validation : Boolean_8;
--				-- Parameter Node?
--				Switches : access Switch_List;
--				Switch_Params : access Switch_Parameters;
--				-- TODO Impl
--			when Music_Segment =>
--				Music_Node : access Music_Node_Type;
--				Duration : Long_Float;
--				Marker_List : access Marker_Array;
--			when Music_Track =>
--				Music_Flags : Unsigned_8;
--				Music_Source_List : access Source_Array;
--				Playlist_List : access Playlist_Array;
--				Clip_Automation_List : access Clip_Array;
--				Parameters : Parameter_Node;
--				Look_Ahead_Time : Unsigned_32;
--				Music_Switch_Params : access Music_Switch_Parameters;
--				Transition_Params : access Transition_Parameters;
--
--				-- TODO Impl
--			when Music_Switch =>
--				Switch_Transition_Params : access Transition_Parameters;
--				Continuous_Playback : Boolean_8;
--				Arguments : access Music_Switch_Parameters;
--				Tree : access Decision_Tree;
--				-- TODO Impl
			when others => null;
		end case;
	end record
	with
		Read => Read_Hierarchy_Object;

	-- Subprograms
		procedure Read_Hierarchy_Object (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Hierarchy_Object);

end Hierarchy;

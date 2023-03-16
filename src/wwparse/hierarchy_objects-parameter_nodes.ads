with Ada.Streams;
with Interfaces; use Interfaces;

with Hierarchy_Objects.Property_Arrays; use Hierarchy_Objects.Property_Arrays;

package Hierarchy_Objects.Parameter_Nodes is
	-- Parameter Node
	type FX_Element is record
		FX_Index : Unsigned_8;
		FX_ID : Unsigned_32;
		Is_Share_Set : Boolean_8;
		Is_Rendered : Boolean_8;
	end record;

	type FX_Element_Array is array (Unsigned_8 range <>) of FX_Element;
	type FX_Element_Array_Access is access FX_Element_Array;

	type Parameter_Node_FX is record
		Override_Parent_FX : Boolean_8;
		FX_Bypass_Bits : Unsigned_8;
		FX_Elements : FX_Element_Array_Access;
	end record
	with
		Read => Read_Parameter_Node_FX;
	
	-- Major TODO
	type Vertex is record
		X, Y, Z : Float_32;
		Duration : Integer_32;
	end record;
	type Vertex_Array is array (Unsigned_32 range <>) of Vertex;
	type Vertex_Array_Access is access Vertex_Array;

	type Automation_Component is record
		Vertex_Offset : Unsigned_32;
		Vertex_Count : Unsigned_32;
	end record;
	type Automation_Playlist is array (Unsigned_32 range <>)
		of Automation_Component;
	type Automation_Playlist_Access is access Automation_Playlist;

	type Automation_Param (Version : Bank_Version_Type) is record
		Range_X : Float_32;
		Range_Y : Float_32;

		case Version is
			when D1RI | D2SK => null;
			when D2WQ =>
				Range_Z : Float_32;
		end case;
	end record;

	type Automation_Param_Array_D1RI_D2SK is array (Unsigned_32 range <>) of
		Automation_Param (D2SK);
	type Automation_Param_Array_D2WQ is array (Unsigned_32 range <>) of
		Automation_Param (D2WQ);
	type Automation_Param_Container (
		Version : Bank_Version_Type;
		Length : Unsigned_32)
	is record
		case Version is
			when D1RI | D2SK =>
				Contents_D1RI_D2SK : Automation_Param_Array_D1RI_D2SK (1 .. Length);
			when D2WQ =>
				Contents_D2WQ : Automation_Param_Array_D2WQ (1 .. Length);
		end case;
	end record;
	type Automation_Param_Container_Access is access Automation_Param_Container;
	
	type Parameter_Node_Positioning (Version : Bank_Version_Type) is record
		Positioning_Bits : Unsigned_8;
		Positioning_Available_2D : Boolean_8 := False; -- Before D2WQ
		Positioning_Available_3D : Boolean_8 := False; -- Before D2WQ
		Positioning_Type_3D : Unsigned_32 := 0; -- TODO add names, Before D2WQ
		Bits_3D : Unsigned_8 := 0; -- TODO add names, D2WQ
		Attenuation_ID_3D : Unsigned_32; -- Before D2WQ
		Is_Spatialised_3D : Boolean_8; -- Before D2WQ
		Is_Dynamic : Boolean_8;
		Automation_Path_Mode : Unsigned_32; -- TODO add names, size change
		Automation_Is_Looping : Boolean_8; -- Before D2WQ
		Automation_Transition_Time : Integer_32;
		Automation_Follow_Orientation : Boolean_8; -- Before D2WQ
		Automation_Vertices : Vertex_Array_Access;
		Automation_Playlist : Automation_Playlist_Access;
		Automation_Params_3D : Automation_Param_Container_Access;
	end record
	with
		Read => Read_Parameter_Node_Positioning;
	
	type Parameter_Node_Aux (Version : Bank_Version_Type) is record
		-- The below Boolean_8s are only D1RI
		Override_Game_Aux : Boolean_8 := False;
		Use_Game_Aux : Boolean_8 := False;
		Override_User_Aux : Boolean_8 := False;
		Has_Aux : Boolean_8 := False;
		Aux_Bits : Unsigned_8 := 0; -- After D1RI
		Aux_IDs : Unsigned_32_Array (1 .. 4); -- Only if Has_Aux / equiv.
	end record
	with
		Read => Read_Parameter_Node_Aux;
	
	type Parameter_Node_Advanced (Version : Bank_Version_Type) is record
		Virtual_Queue_Behaviour : Unsigned_8; -- TODO add names
		-- Only D1RI
		Kill_Newest : Boolean_8 := False;
		Use_Virtual_Behaviour : Boolean_8 := False;
		-- Everywhere
		Max_Instances : Unsigned_16;
		-- Only D1RI
		Is_Global_Limit : Boolean_8 := False;
		-- Everywhere
		Below_Threshold_Behaviour : Unsigned_8; -- TODO add names
		-- Only D1RI
		Max_Instances_Overrides_Parent : Boolean_8 := False;
		Virtual_Voices_Overrides_Parent : Boolean_8 := False;
		Override_Header_Envelope : Boolean_8 := False;
		Override_Analysis : Boolean_8 := False;
		Normalise_Loudness : Boolean_8 := False;
		Enable_Envelope : Boolean_8 := False;
		-- The below are only after D1RI
		Adv_Bits_1 : Unsigned_8; -- TODO Add names
		Adv_Bits_2 : Unsigned_8;
	end record
	with
		Read => Read_Parameter_Node_Advanced;
	
	type Parameter_Node_State_Property (Version : Bank_Version_Type) is record
		Identifier : Unsigned_8; -- TODO var
		Accumulator_Type : Unsigned_8; -- TODO AkRtpcAccum definition

		case Version is
			when D1RI .. D2SK => null;
			when D2WQ =>
				In_Database : Boolean;
		end case;
	end record;

	type Parameter_Node_State_Properties_D1RI_D2SK is array (Unsigned_8 range <>) of Parameter_Node_State_Property (D2SK);
	type Parameter_Node_State_Properties_D1RI_D2SK_Access is access Parameter_Node_State_Properties_D1RI_D2SK;

	type Parameter_Node_State_Properties_D2WQ is array (Unsigned_8 range <>) of Parameter_Node_State_Property (D2WQ);
	type Parameter_Node_State_Properties_D2WQ_Access is access Parameter_Node_State_Properties_D2WQ;

	type Parameter_Node_State_Groups is array (Unsigned_

	type Parameter_Node_State_Chunk (Version : Bank_Version_Type) is record
		case Version is
			when D1RI .. D2SK =>
				Properties_D1RI_D2SK => Parameter_Node_State_Properties_D1RI_D2SK_Access;
				Groups_1 => Parameter_Node_State_Groups_Access;
			when D2WQ =>
				Properties_D2WQ : Parameter_Node_State_Properties_D2WQ_Access;
				Groups_2 : Parameter_Node_State_Groups_Access;
		end case;
	end record
	with
		Read => Read_Parameter_Node_State_Chunk;
	
	type Parameter_Node_Feedback (Feedback_Presence : Boolean) is record
		case Feedback_Presence is
			when False => null;
			when True =>
				Bus_ID : Unsigned_32;
		end case;
	end record;

	type Parameter_Node (
		Version : Bank_Version_Type;
		Feedback_Presence : Boolean)
	is record
		FX : Parameter_Node_FX;
		Override_Attachment_Params : Boolean_8 := False; -- D2WQ
		Override_Bus_ID : Unsigned_32;
		Direct_Parent_ID : Unsigned_32;
		Priority_Override_Parent : Boolean_8 := False; -- Before D2WQ
		Priority_Apply_Dist_Factor : Boolean_8 := False; -- Before D2WQ
		Object_Bits : Unsigned_8; -- D2WQ
		Initial_Params : Property_Array_CU8_IU8_VU32_Access;
		Initial_Params_Ranged : Ranged_Property_Array_CU8_IU8_VU32_Access;
		Positioning : Parameter_Node_Positioning (Version);
		Aux : Parameter_Node_Aux (Version);
		Advanced : Parameter_Node_Advanced (Version);
		State_Chunk : Parameter_Node_State_Chunk;
		RTPC : Parameter_Node_RTPC;
		Feedback : Parameter_Node_Feedback (Feedback_Presence); -- Before D2WQ
	end record
	with
		Read => Read_Parameter_Node;

	procedure Read_Parameter_Node (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Parameter_Node);

	procedure Read_Parameter_Node_FX (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Parameter_Node_FX);

	procedure Read_Parameter_Node_Positioning (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Parameter_Node_Positioning);

	procedure Read_Parameter_Node_Aux (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Parameter_Node_Aux);

	procedure Read_Parameter_Node_Advanced (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Parameter_Node_Advanced);

end Hierarchy_Objects.Parameter_Nodes;

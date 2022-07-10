with Bank; use Bank;

package Hierarchy_Objects.Actions is
	-- Action-specific types
	subtype Action_ID_Type is Unsigned_32;
	type Action_ID_Array is array (Natural range <>) of Action_ID_Type;
	type Action_ID_Array_Access is access Action_ID_Array;

	-- Mapped list of all Action types
	type Action_Type_Type is (
		None,
		Stop_E,
		Stop_E_O,
		Stop_All,
		Stop_All_O,
		Stop_AE,
		Stop_AE_O,
		Pause_E,
		Pause_E_O,
		Pause_All,
		Pause_All_O,
		Pause_AE,
		Pause_AE_O,
		Resume_E,
		Resume_E_O,
		Resume_All,
		Resume_All_O,
		Resume_AE,
		Resume_AE_O,
		Play,
		Play_And_Continue,
		Mute_M,
		Mute_O,
		Unmute_M,
		Unmute_O,
		Unmute_All,
		Unmute_All_O,
		Unmute_AE,
		Unmute_AE_O,
		Set_Pitch_M,
		Set_Pitch_O,
		Reset_Pitch_M,
		Reset_Pitch_O,
		Reset_Pitch_All,
		Reset_Pitch_All_O,
		Reset_Pitch_AE,
		Reset_Pitch_AE_O,
		Set_Volume_M,
		Set_Volume_O,
		Reset_Volume_M,
		Reset_Volume_O,
		Reset_Volume_All,
		Reset_Volume_All_O,
		Reset_Volume_AE,
		Reset_Volume_AE_O,
		Set_Bus_Volume_M,
		Set_Bus_Volume_O,
		Reset_Bus_Volume_M,
		Reset_Bus_Volume_O,
		Reset_Bus_Volume_All,
		Reset_Bus_Volume_AE,
		Set_LPF_M,
		Set_LPF_O,
		Reset_LPF_M,
		Reset_LPF_O,
		Reset_LPF_All,
		Reset_LPF_All_O,
		Reset_LPF_AE,
		Reset_LPF_AE_O,
		Use_State_E,
		Unuse_State_E,
		Set_State,
		Set_Game_Parameter,
		Set_Game_Parameter_O,
		Reset_Game_Parameter,
		Reset_Game_Parameter_O,
		Stop_Event,
		Pause_Event,
		Resume_Event,
		Duck,
		Set_Switch,
		Bypass_FX_M,
		Bypass_FX_O,
		Reset_Bypass_FX_M,
		Reset_Bypass_FX_O,
		Reset_Bypass_FX_All,
		Reset_Bypass_FX_All_O,
		Reset_Bypass_FX_AE,
		Reset_Bypass_FX_AE_O,
		Break_E,
		Break_E_O,
		Trigger,
		Trigger_O,
		Trigger_E,
		Trigger_E_O,
		Seek_E,
		Seek_E_O,
		Seek_All,
		Seek_All_O,
		Seek_AE,
		Seek_AE_O,
		Release,
		Release_O,
		Set_HPF_M,
		Set_HPF_O,
		Play_Event,
		Reset_Playlist_E,
		Reset_Playlist_E_O,
		Play_Unknown,
		Reset_HPF_M,
		Reset_HPF_O,
		Reset_HPF_All,
		Reset_HPF_All_O,
		Reset_HPF_AE,
		Reset_HPF_AE_O,
		Set_FX_M,
		Reset_Set_FX_M,
		Reset_Set_FX_All,
		None_Alt)
	with
		Size => 16;

	for Action_Type_Type use (
		None => 16#0000#,
		Stop_E => 16#0102#,
		Stop_E_O => 16#0103#,
		Stop_All => 16#0104#,
		Stop_All_O => 16#0105#,
		Stop_AE => 16#0108#,
		Stop_AE_O => 16#0109#,
		Pause_E => 16#0202#,
		Pause_E_O => 16#0203#,
		Pause_All => 16#0204#,
		Pause_All_O => 16#0205#,
		Pause_AE => 16#0208#,
		Pause_AE_O => 16#0209#,
		Resume_E => 16#0302#,
		Resume_E_O => 16#0303#,
		Resume_All => 16#0304#,
		Resume_All_O => 16#0305#,
		Resume_AE => 16#0308#,
		Resume_AE_O => 16#0309#,
		Play => 16#0403#,
		Play_And_Continue => 16#0503#,
		Mute_M => 16#0602#,
		Mute_O => 16#0603#,
		Unmute_M => 16#0702#,
		Unmute_O => 16#0703#,
		Unmute_All => 16#0704#,
		Unmute_All_O => 16#0705#,
		Unmute_AE => 16#0708#,
		Unmute_AE_O => 16#0709#,
		Set_Pitch_M => 16#0802#,
		Set_Pitch_O => 16#0803#,
		Reset_Pitch_M => 16#0902#,
		Reset_Pitch_O => 16#0903#,
		Reset_Pitch_All => 16#0904#,
		Reset_Pitch_All_O => 16#0905#,
		Reset_Pitch_AE => 16#0908#,
		Reset_Pitch_AE_O => 16#0909#,
		Set_Volume_M => 16#0A02#,
		Set_Volume_O => 16#0A03#,
		Reset_Volume_M => 16#0B02#,
		Reset_Volume_O => 16#0B03#,
		Reset_Volume_All => 16#0B04#,
		Reset_Volume_All_O => 16#0B05#,
		Reset_Volume_AE => 16#0B08#,
		Reset_Volume_AE_O => 16#0B09#,
		Set_Bus_Volume_M => 16#0C02#,
		Set_Bus_Volume_O => 16#0C03#,
		Reset_Bus_Volume_M => 16#0D02#,
		Reset_Bus_Volume_O => 16#0D03#,
		Reset_Bus_Volume_All => 16#0D04#,
		Reset_Bus_Volume_AE => 16#0D08#,
		Set_LPF_M => 16#0E02#,
		Set_LPF_O => 16#0E03#,
		Reset_LPF_M => 16#0F02#,
		Reset_LPF_O => 16#0F03#,
		Reset_LPF_All => 16#0F04#,
		Reset_LPF_All_O => 16#0F05#,
		Reset_LPF_AE => 16#0F08#,
		Reset_LPF_AE_O => 16#0F09#,
		Use_State_E => 16#1002#,
		Unuse_State_E => 16#1102#,
		Set_State => 16#1204#,
		Set_Game_Parameter => 16#1302#,
		Set_Game_Parameter_O => 16#1303#,
		Reset_Game_Parameter => 16#1402#,
		Reset_Game_Parameter_O => 16#1403#,
		Stop_Event => 16#1511#,
		Pause_Event => 16#1611#,
		Resume_Event => 16#1711#,
		Duck => 16#1820#,
		Set_Switch => 16#1901#,
		Bypass_FX_M => 16#1A02#,
		Bypass_FX_O => 16#1A03#,
		Reset_Bypass_FX_M => 16#1B02#,
		Reset_Bypass_FX_O => 16#1B03#,
		Reset_Bypass_FX_All => 16#1B04#,
		Reset_Bypass_FX_All_O => 16#1B05#,
		Reset_Bypass_FX_AE => 16#1B08#,
		Reset_Bypass_FX_AE_O => 16#1B09#,
		Break_E => 16#1C02#,
		Break_E_O => 16#1C03#,
		Trigger => 16#1D00#,
		Trigger_O => 16#1D01#,
		Trigger_E => 16#1D02#,
		Trigger_E_O => 16#1D03#,
		Seek_E => 16#1E02#,
		Seek_E_O => 16#1E03#,
		Seek_All => 16#1E04#,
		Seek_All_O => 16#1E05#,
		Seek_AE => 16#1E08#,
		Seek_AE_O => 16#1E09#,
		Release => 16#1F02#,
		Release_O => 16#1F03#,
		Set_HPF_M => 16#2002#,
		Set_HPF_O => 16#2003#,
		Play_Event => 16#2103#,
		Reset_Playlist_E => 16#2202#,
		Reset_Playlist_E_O => 16#2203#,
		Play_Unknown => 16#2303#,
		Reset_HPF_M => 16#3002#,
		Reset_HPF_O => 16#3003#,
		Reset_HPF_All => 16#3004#,
		Reset_HPF_All_O => 16#3005#,
		Reset_HPF_AE => 16#3008#,
		Reset_HPF_AE_O => 16#3009#,
		Set_FX_M => 16#3102#,
		Reset_Set_FX_M => 16#3202#,
		Reset_Set_FX_All => 16#3204#,
		None_Alt => 16#4000#);

	-- General action categories
	-- Defined by the upper byte of Action_Type_Type
	-- If no category fits, assign None
	type Action_Category_Type is (
		None,
		Stop,
		Pause,
		Resume,
		Play,
		Play_And_Continue,
		Mute,
		Unmute,
		Set_Pitch,
		Reset_Pitch,
		Set_None,
		Reset_None,
		Set_Bus_Volume,
		Reset_Bus_Volume,
		Set_LPF,
		Reset_LPF,
		Use_State,
		Unuse_State,
		Set_State,
		Set_Game_Parameter,
		Reset_Game_Parameter,
		Stop_Event,
		Pause_Event,
		Resume_Event,
		Set_Switch,
		Bypass_FX,
		Reset_Bypass_FX,
		Break,
		Trigger,
		Seek,
		Release,
		Set_HPF,
		Play_Event,
		Reset_Playlist,
		Play_Unknown,
		Reset_HPF)
	with
		Size => 16;

	for Action_Category_Type use (
		None => 16#0000#,
		Stop => 16#0100#,
		Pause => 16#0200#,
		Resume => 16#0300#,
		Play => 16#0400#,
		Play_And_Continue => 16#0500#,
		Mute => 16#0600#,
		Unmute => 16#0700#,
		Set_Pitch => 16#0800#,
		Reset_Pitch => 16#0900#,
		Set_None => 16#0A00#,
		Reset_None => 16#0B00#,
		Set_Bus_Volume => 16#0C00#,
		Reset_Bus_Volume => 16#0D00#,
		Set_LPF => 16#0E00#,
		Reset_LPF => 16#0F00#,
		Use_State => 16#1000#,
		Unuse_State => 16#1100#,
		Set_State => 16#1200#,
		Set_Game_Parameter => 16#1300#,
		Reset_Game_Parameter => 16#1400#,
		Stop_Event => 16#1500#,
		Pause_Event => 16#1600#,
		Resume_Event => 16#1700#,
		Set_Switch => 16#1900#,
		Bypass_FX => 16#1A00#,
		Reset_Bypass_FX => 16#1B00#,
		Break => 16#1C00#,
		Trigger => 16#1D00#,
		Seek => 16#1E00#,
		Release => 16#1F00#,
		Set_HPF => 16#2000#,
		Play_Event => 16#2100#,
		Reset_Playlist => 16#2200#,
		Play_Unknown => 16#2300#,
		Reset_HPF => 16#3000#);

	-- Specific additional info for various Action categories
	type Fade_Type is new Unsigned_8; -- TODO add names

	-- Exception Parameters for an Action
	-- TODO: Actually implement
	type Except_Params_Access (Version : Bank_Version_Type) is record
		case Version is
			when D1RI | D2SK =>
				Exception_List_Size_D1RI_D2SK : Unsigned_32;
			when D2WQ =>
				Exception_List_Size_D2WQ : Unsigned_8;
		end case;
	end record;

	type Action_Specifics_Type (
		Version : Bank_Version_Type;
		Action_Category : Action_Category_Type)
	is record
		case Action_Category is
			when Stop =>
				Stop_Fade : Fade_Type;
				case Version is
					when D2WQ =>
						Stop_Bits : Unsigned_8; -- TODO Display bits
						Stop_Except_D2WQ : Except_Params_Access (Version);
					when D1RI | D2SK =>
						Stop_Except_D1RI_D2SK : Except_Params_Access (Version);
				end case;
			when Pause =>
				Pause_Fade : Fade_Type;
				Pause_Bits : Unsigned_8; -- TODO Display bits
				Pause_Except : Except_Params_Access (Version);
			when Resume =>
				Resume_Fade : Fade_Type;
				Resume_Bits : Unsigned_8; -- TODO Display bits
				Resume_Except : Except_Params_Access (Version);
			when Play | Play_And_Continue | Play_Unknown =>
				Play_Fade : Fade_Type;
				File_ID : FNV_Hash; -- TODO check if correct
			when Mute | Unmute =>
				Mute_Fade : Fade_Type;
				Mute_Except : Except_Params_Access (Version);
			when Set_Pitch | Reset_Pitch -- These include additional properties
				| Set_None | Reset_None
				| Set_Bus_Volume | Reset_Bus_Volume
				| Set_LPF | Reset_LPF
				| Set_HPF | Reset_HPF
			=>
				Set_Value_Fade : Fade_Type;
				Set_Value_Meaning : Unsigned_8; -- TODO Map to values
				Set_Value_Base : Float_32; -- TODO check if correct
				Set_Value_Min : Float_32;
				Set_Value_Max : Float_32;
				Set_Value_Except : Except_Params_Access (Version);
			when Use_State | Unuse_State => null;
			when Set_State =>
				Set_State_Group_ID : FNV_Hash;
				Set_State_Target_ID : FNV_Hash;
			when Set_Game_Parameter | Reset_Game_Parameter =>
				Set_Game_Parameter_Fade : Fade_Type;
				Set_Game_Parameter_Bypass_Transition : Boolean_8;
				Set_Game_Parameter_Value_Meaning : Unsigned_8;
				Set_Game_Parameter_Base : Float_32; -- TODO check if correct
				Set_Game_Parameter_Min : Float_32;
				Set_Game_Parameter_Max : Float_32;
				Set_Game_Parameter_Except : Except_Params_Access (Version);
			when Set_Switch =>
				Set_Switch_Group_ID : FNV_Hash;
				Set_Switch_State_ID : FNV_Hash;
			when Bypass_FX | Reset_Bypass_FX =>
				Bypass_FX_Is_Bypass : Boolean_8;
				Bypass_FX_Target_Mask : Boolean_8;
				Bypass_FX_Except : Except_Params_Access (Version);
			when Break | Trigger => null;
			when Seek =>
				Seek_Is_Relative_To_Duration : Boolean_8;
				Seek_Value : Float_32; -- TODO check if correct
				Seek_Value_Min : Float_32;
				Seek_Value_Max : Float_32;
				Seek_Snap_To_Nearest_Marker : Boolean_8;
				Seek_Except : Except_Params_Access (Version);
			when Release => null;
			when Play_Event | Stop_Event | Pause_Event | Resume_Event => null;
			when Reset_Playlist =>
				Reset_Playlist_Fade : Fade_Type;
				Reset_Except : Except_Params_Access (Version);
			when None => null;
		end case;
	end record;

	type Action_Specifics_Access is access Action_Specifics_Type;
end Hierarchy_Objects.Actions;

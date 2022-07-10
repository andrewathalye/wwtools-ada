package Hierarchy_Objects.Property_Arrays is
	-- Store Property Lists
	generic
		type Property_Count_Type is mod <>;
		type Property_ID_Type is mod <>;
		type Property_Value_Type is (<>);

	package Generic_Property_Array is
		-- TODO: Convert IDs to names later

		type Property_Type is record
			Property_ID : Property_ID_Type;
			Property_Value : Property_Value_Type;
		end record;

		type Ranged_Property_Type is record
			Property_ID : Property_ID_Type;
			Property_Min : Property_Value_Type;
			Property_Max : Property_Value_Type;
		end record;

		type Property_Array is array (Property_Count_Type range <>) of Property_Type;
		type Ranged_Property_Array is
			array (Property_Count_Type range <>) of Ranged_Property_Type;
	end Generic_Property_Array;

	-- Property Array with Count_Type=U8, ID_Type=U8, and Value_Type=U32
	package Property_Arrays_CU8_IU8_VU32 is new Generic_Property_Array (
		Property_Count_Type => Unsigned_8,
		Property_ID_Type => Unsigned_8,
		Property_Value_Type => Unsigned_32);

	subtype Property_Array_CU8_IU8_VU32 is
		Property_Arrays_CU8_IU8_VU32.Property_Array;
	type Property_Array_CU8_IU8_VU32_Access is
		access Property_Arrays_CU8_IU8_VU32.Property_Array;
	subtype Ranged_Property_Array_CU8_IU8_VU32 is
		Property_Arrays_CU8_IU8_VU32.Ranged_Property_Array;
	type Ranged_Property_Array_CU8_IU8_VU32_Access is
		access Property_Arrays_CU8_IU8_VU32.Ranged_Property_Array;

end Hierarchy_Objects.Property_Arrays;

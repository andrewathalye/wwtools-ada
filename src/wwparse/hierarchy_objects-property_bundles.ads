package Hierarchy_Objects.Property_Bundles is
	-- Store Property Lists
	generic
		type Property_Count_Type is mod <>;
		type Property_ID_Type is mod <>;
		type Property_Value_Type is (<>);

	package Generic_Property_Bundle is
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

		type Property_Bundle (
			Length : Property_Count_Type;
			Ranged : Boolean)
		is record
			case Ranged is
				when True =>
					Ranged_Properties : Ranged_Property_Array (1 .. Length);
				when False =>
					Properties : Property_Array (1 .. Length);
			end case;
		end record;
	end Generic_Property_Bundle;

	-- Property_Bundle instantiations
	-- Property Bundle with Count_Type=U8, ID_Type=U8, and Value_Type=U32
	package Property_Bundles_CU8_IU8_VU32 is new Generic_Property_Bundle (
		Property_Count_Type => Unsigned_8,
		Property_ID_Type => Unsigned_8,
		Property_Value_Type => Unsigned_32);

	subtype Property_Bundle_CU8_IU8_VU32 is
		Property_Bundles_CU8_IU8_VU32.Property_Bundle;
	type Property_Bundle_CU8_IU8_VU32_Access is
		access Property_Bundle_CU8_IU8_VU32;

end Hierarchy_Objects.Property_Bundles;

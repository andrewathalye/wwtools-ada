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
				Unsigned_32'Read (Stream, Item.Action_External_ID);
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
					Action_Category_Type'Val (
						Unsigned_16 (
							Action_Type_Type'Enum_Rep (Item.Action_Type))
							and 16#ff00#));
				Action_Specifics_Type'Read (Stream, Item.Action_Specifics.all);
			when others => -- Items without specific support
				Discard_Array'Read (Stream, Discard);
		end case;
	end Read_Hierarchy_Object;

end Hierarchy;

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
					Action_ID_List_Size_D1RI_D2SK : Unsigned_32;
					Action_ID_List_Size_D2WQ : Unsigned_8; -- TODO: Should be a var type
				begin
					-- The list size is stored in a different format before D2WQ
					case Item.Version is
						when D1RI | D2SK =>
							Unsigned_32'Read (Stream, Action_ID_List_Size_D1RI_D2SK);
						when D2WQ =>
							Unsigned_8'Read (Stream, Action_ID_List_Size_D2WQ);
					end case;

					-- Create the array and setup the list
					declare
						Action_ID_List : Action_ID_Array (1 .. (case Item.Version is
							when D1RI | D2SK =>
								Natural (Action_ID_List_Size_D1RI_D2SK),
							when D2WQ =>
								Natural (Action_ID_List_Size_D2WQ)));
					begin
						Action_ID_Array'Read (Stream, Action_ID_List);
						Item.Action_ID_List := new Action_ID_Array'(Action_ID_List);
					end;
				end;
			when Action =>
				Action_Type_Type'Read (Stream, Item.Action_Type);
				Unsigned_32'Read (Stream, Item.Action_External_ID);
				Unsigned_8'Read (Stream, Item.Action_Bits);

				-- Read Property Bundle sizes
				declare
					Action_Properties_Length : Unsigned_8;
					Action_Modifiers_Length : Unsigned_8;
				begin
					Unsigned_8'Read (Stream, Action_Properties_Length);
					Unsigned_8'Read (Stream, Action_Modifiers_Length);

					-- Read Property Bundles
					declare
						Action_Properties : Property_Bundle_CU8_IU8_VU32 (
							Length => Action_Properties_Length,
							Ranged => False);
						Action_Modifiers : Property_Bundle_CU8_IU8_VU32 (
							Length => Action_Modifiers_Length,
							Ranged => True);
					begin
						Property_Bundle_CU8_IU8_VU32'Read (
							Stream,
							Action_Properties);
						Property_Bundle_CU8_IU8_VU32'Read (
							Stream,
							Action_Modifiers);

						Item.Action_Properties :=
							new Property_Bundle_CU8_IU8_VU32'(Action_Properties);
						Item.Action_Modifiers :=
							new Property_Bundle_CU8_IU8_VU32'(Action_Modifiers);
					end;
				end;
			when others => -- Items without specific support
				Discard_Array'Read (Stream, Discard);
		end case;
	end Read_Hierarchy_Object;

end Hierarchy;

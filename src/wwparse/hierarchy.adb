with Ada.Text_IO; use Ada.Text_IO;

package body Hierarchy is
	procedure Read_Marker_Type (
		Stream : not null access Root_Stream_Type'Class;
		Item : out Marker_Type)
	is
	begin
		null;
	end Read_Marker_Type;

	procedure Read_Hierarchy_Object (
		Stream : not null access Root_Stream_Type'Class;
		Item : out Hierarchy_Object)
	is
		Object_ID : Unsigned_32;
		Discard : Discard_Array (1 .. Natural (Item.Size) - (Unsigned_32'Size / 8));
	begin
		Unsigned_32'Read (Stream, Object_ID);
		Item.Object_ID := Object_ID;

		case Item.Identifier is
			when Event =>
				declare
					Action_ID_List_Size : Unsigned_8;
				begin
					Unsigned_8'Read (Stream, Action_ID_List_Size);
					Put_Line (Action_ID_List_Size'Image);
					declare
						Action_ID_List : Action_ID_Array (1 .. Unsigned_32 (Action_ID_List_Size));
					begin
						Action_ID_Array'Read (Stream, Action_ID_List);
						Put_Line (Action_ID_List'Image);
						Item.Action_ID_List := new Action_ID_Array'(Action_ID_List);
					end;

				end;
			when others =>
				Discard_Array'Read (Stream, Discard);
		end case;
	end Read_Hierarchy_Object;

end Hierarchy;

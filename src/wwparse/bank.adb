with Types; use Types;

package body Bank is
	procedure Read_Bank_Header (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Bank_Header_Type)
	is
		type Bank_Header_Raw (Version : Bank_Version_Type) is record
			Bank_ID : Unsigned_32; -- 00 .. 03
			Language_ID : Unsigned_32; -- 04 .. 07

			case Version is
				when D1RI =>
					Feedback_Included : Boolean_32; -- 08 .. 0D
					Project_ID_D1RI : Unsigned_32; -- 0E .. 11
					Discard_D1RI : Discard_Array (16#12# .. 16#15#);
				when D2SK .. D2WQ =>
					Alignment : Unsigned_16; -- 08 .. 0B
					Device_Allocated : Boolean_16; -- 0C .. 0D
					Project_ID_D2SK_D2WQ : Unsigned_32; -- 0E .. 11
					Discard : Discard_Array (16#12# .. 16#15#);
			end case;
		end record;

		R : Bank_Header_Raw (Item.Version);
	begin
		Bank_Header_Raw'Read (Stream, R);
		Item := (R.Version,
			R.Bank_ID,
			R.Language_ID,
			Project_ID => (case R.Version is
				when D1RI => R.Project_ID_D1RI,
				when D2SK .. D2WQ => R.Project_ID_D2SK_D2WQ));
	end Read_Bank_Header;

	procedure Read_Chunk_Identifier (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Chunk_Identifier_Type)
	is
		R : String (1 .. 4);
	begin
		String'Read (Stream, R);
		Item := Chunk_Identifier_Type'Value (R);
	end Read_Chunk_Identifier;
end Bank;

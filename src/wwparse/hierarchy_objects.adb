package body Hierarchy_Objects is
	procedure Read_Marker_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Marker_Type)
	is
	begin
		null;
	end Read_Marker_Type;

	procedure Read_Source_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Source_Type)
	is
		-- Internal, version-specific types
		type Source_Stream_Type_D1RI is (
			Data,
			Streaming,
			Prefetch_Streaming);
		for Source_Stream_Type_D1RI'Size use 32;

		type Source_Stream_Type_D2SK_D2WQ is (
			Data,
			Prefetch_Streaming,
			Streaming);
		for Source_Stream_Type_D2SK_D2WQ'Size use 8;
	begin
		Plugin_Info_Type'Read (Stream, Item.Plugin_Info);

		-- Read Stream Type
		case Item.Version is
			when D1RI =>
				declare
					SST_D1RI : Source_Stream_Type_D1RI;
				begin
					Source_Stream_Type_D1RI'Read (Stream, SST_D1RI);
					Item.Stream_Type := (case SST_D1RI is
						when Data => Data,
						when Streaming => Streaming,
						when Prefetch_Streaming => Prefetch_Streaming);
				end;
			when D2SK | D2WQ =>
				declare
					SST_D2SK_D2WQ : Source_Stream_Type_D2SK_D2WQ;
				begin
					Source_Stream_Type_D2SK_D2WQ'Read (Stream, SST_D2SK_D2WQ);
					Item.Stream_Type := (case SST_D2SK_D2WQ is
						when Data => Data,
						when Streaming => Streaming,
						when Prefetch_Streaming => Prefetch_Streaming);
				end;
		end case;

		Unsigned_32'Read (Stream, Item.Source_ID);

		-- Read File ID
		case Item.Version is
			when D1RI | D2SK =>
				Unsigned_32'Read (Stream, Item.File_ID);
			when D2WQ => null;
		end case;

		-- Read File Offset if not in-memory
		case Item.Stream_Type is
			when Data | Prefetch_Streaming =>
				Unsigned_32'Read (Stream, Item.File_Offset);
			when Streaming => null;
		end case;

		Unsigned_32'Read (Stream, Item.In_Memory_Media_Size);
		Unsigned_8'Read (Stream, Item.Source_Bits);
	end Read_Source_Type;

	procedure Read_Playlist_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Playlist_Type)
	is
	begin
		Unsigned_32'Read (Stream, Item.Track_ID);
		Unsigned_32'Read (Stream, Item.Source_ID);

		-- Read Event ID
		case Item.Version is
			when D1RI | D2SK => null;
			when D2WQ =>
				Unsigned_32'Read (Stream, Item.Event_ID);
		end case;

		Float_64'Read (Stream, Item.Play_At);
		Float_64'Read (Stream, Item.Begin_Trim_Offset);
		Float_64'Read (Stream, Item.End_Trim_Offset);
		Float_64'Read (Stream, Item.Source_Duration);
	end Read_Playlist_Type;

	procedure Read_Clip_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Clip_Type)
	is
		RTPC_Graph_Length : Unsigned_32;
	begin
		Unsigned_32'Read (Stream, Item.Clip_Index);
		Unsigned_32'Read (Stream, Item.Clip_Type);

		-- Read RTPC Graph
		Unsigned_32'Read (Stream, RTPC_Graph_Length);
		Item.Clip_Graph := new RTPC_Graph (1 .. Natural (RTPC_Graph_Length));
		RTPC_Graph'Read (Stream, Item.Clip_Graph.all);
	end Read_Clip_Type;

	procedure Read_Music_Switch_Parameters (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Music_Switch_Parameters)
	is
		Associated_Switches_Length : Unsigned_32;
	begin
		Unsigned_8'Read (Stream, Item.Group_Type);
		FNV_Hash'Read (Stream, Item.Group_ID);
		FNV_Hash'Read (Stream, Item.Default_Switch);

		-- Read Associated Switches
		Unsigned_32'Read (Stream, Associated_Switches_Length);
		Item.Associated_Switches := new FNV_Hash_Array
			(1 .. Natural (Associated_Switches_Length));
		FNV_Hash_Array'Read (Stream, Item.Associated_Switches.all);
	end Read_Music_Switch_Parameters;

end Hierarchy_Objects;

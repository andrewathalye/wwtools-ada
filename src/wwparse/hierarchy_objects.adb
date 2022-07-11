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
	begin
		null;
	end Read_Source_Type;
	procedure Read_Playlist_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Playlist_Type)
	is
	begin
		null;
	end Read_Playlist_Type;
	procedure Read_Clip_Type (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Clip_Type)
	is
	begin
		null;
	end Read_Clip_Type;
	procedure Read_Music_Switch_Parameters (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Item : out Music_Switch_Parameters)
	is
	begin
		null;
	end Read_Music_Switch_Parameters;

end Hierarchy_Objects;

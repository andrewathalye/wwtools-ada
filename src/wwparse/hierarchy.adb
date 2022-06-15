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
	begin
		null;
	end Read_Hierarchy_Object;

end Hierarchy;

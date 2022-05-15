with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with WWHash; use WWHash;

procedure WWGen is
	procedure Show_Usage is
	begin
		Put_Line (Standard_Error, "Usage: "
			& Command_Name
			& " strings");
	end Show_Usage;
begin
	-- Check arguments
	if Argument_Count < 1 then
		Show_Usage;
		return;
	end if;

	-- Calculate hashes
	for I in 1 .. Argument_Count loop
		Put_Line (Argument (I) & Hash_Type'Image (Calculate_Hash (Argument (I))));
	end loop;
end WWGen;

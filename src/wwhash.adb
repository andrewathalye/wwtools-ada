package body WWHash is
	function Calculate_Hash (S : String) return Hash_Type is
		-- Base hash
		H : constant Hash_Type := 2166136261;
	begin
		return Calculate_Hash (H, S);
	end Calculate_Hash;

	function Calculate_Hash (H : Hash_Type; S : String) return Hash_Type is
		Hash : Hash_Type := H;
	begin
		for C of S loop
			Hash := (Hash * Multiplier) xor Character'Pos (C);
		end loop;

		return Hash;
	end Calculate_Hash;
end WWHash;

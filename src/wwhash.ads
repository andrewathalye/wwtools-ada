with Interfaces; use Interfaces;

package WWHash is
	-- Define types
	subtype Hash_Type is Unsigned_32;
	type Hash_Array is array (Natural range <>) of Hash_Type;
	type Hash_Array_Access is access constant Hash_Array;

	type String_Access is access String;
	type String_Array is array (Natural range <>) of String_Access;
	type String_Array_Access is access constant String_Array;

	-- Prime multiplier
	Multiplier : constant Unsigned_32 := 16777619;

	-- Calculate hash for input string (must be lowercase)
	function Calculate_Hash (S : String) return Hash_Type;

	-- Calculate hash for base hash and input string (see above)
	function Calculate_Hash (H : Hash_Type; S : String) return Hash_Type;
end WWHash;

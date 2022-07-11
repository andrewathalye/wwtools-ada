with Interfaces; use Interfaces;

package Types is
	-- Generic Types in Objects
	type Discard_Array is array (Positive range <>) of Unsigned_8;
	type Boolean_8 is new Boolean
	with
		Size => 8;

	type Boolean_16 is new Boolean
	with
		Size => 16;
	-- A boolean value encoded in 16 bits

	type Boolean_32 is new Boolean
	with
		Size => 32;

	type FNV_Hash is new Unsigned_32;
	type FNV_Hash_Array is array (Natural range <>) of FNV_Hash;
	type FNV_Hash_Array_Access is access FNV_Hash_Array;

	type String_Access is access String;

	type Float_32 is new Float
	with
		Size => 32;
	
	type Float_64 is new Long_Float
	with
		Size => 64;

	-- Extremely nonstandard types
	-- These usually require custom 'Read attributes
	type Unsigned_12 is mod 12;
	type Unsigned_4 is mod 4;

end Types;

with Ada.Streams; use Ada.Streams;

with Interfaces; use Interfaces;

package Bank is
	-- Chunk Identifier
	type Chunk_Identifier_Type is
		(BKHD, HIRC, Data, FXPR, STID, STMG, DIDX, PLAT, INIT);

	procedure Read_Chunk_Identifier (
		Stream : not null access Root_Stream_Type'Class;
		Item : out Chunk_Identifier_Type);

	for Chunk_Identifier_Type'Read use Read_Chunk_Identifier;

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
	type String_Access is access String;

	-- Known and supported Bank Versions
	-- TODO: Support more
	type Bank_Version_Type is (D1RI, D2SK, D2WQ)
	with
		Size => 32;
	for Bank_Version_Type use (D1RI => 89, D2SK => 113, D2WQ => 134);

	-- Chunk types
	Bank_Header : Chunk_Identifier_Type renames BKHD;
	Hierarchy : Chunk_Identifier_Type renames HIRC;
	Effects_Parameters : Chunk_Identifier_Type renames FXPR;
	String_Identifiers : Chunk_Identifier_Type renames STID;
	Settings_Manager : Chunk_Identifier_Type renames STMG;
	Data_Index : Chunk_Identifier_Type renames DIDX;
	Custom_Platform : Chunk_Identifier_Type renames PLAT;
	Plugin : Chunk_Identifier_Type renames INIT;

	-- Header Types
	-- Base chunk header
	type Chunk_Header_Type (First_Chunk : Boolean := False) is record
		Identifier : Chunk_Identifier_Type;
		Chunk_Size : Unsigned_32;
		case First_Chunk is
			when True =>
				Version : Bank_Version_Type;
			when False => null;
		end case;
	end record;

	type Bank_Header_Type (Version : Bank_Version_Type) is record
		Bank_ID : Unsigned_32;
		Language_ID : Unsigned_32;
		Project_ID : Unsigned_32;
	end record
	with
		Read => Read_Bank_Header;

	type Hierarchy_Header_Type is record
		Releasable_Items : Unsigned_32;
	end record;

	-- Subprograms
	procedure Read_Bank_Header (
		Stream : not null access Root_Stream_Type'Class;
		Item : out Bank_Header_Type);
end Bank;


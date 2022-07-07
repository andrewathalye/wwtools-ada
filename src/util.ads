with Interfaces; use Interfaces;
package Util is
	type Unsigned_12 is new Unsigned_16 range 0 .. 16#fff#;
	function To_Hex (Num : Unsigned_8) return String;
	function To_Hex (Num : Unsigned_12) return String;
end Util;

package body Util is
	Hex_Digits : constant array (Natural range 0 .. 16#F#) of Character :=
		['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e',
		'f'];

	function To_Hex (Num : Unsigned_8) return String is
		([Hex_Digits (Natural (Shift_Right (Num, 4))),
			Hex_Digits (Natural (Num and 16#f#))]);

	function To_Hex (Num : Unsigned_12) return String is
		([Hex_Digits (Natural (Shift_Right (Num, 8))),
			Hex_Digits (Natural (Shift_Right (Num, 4) and 16#f#)),
			Hex_Digits (Natural (Num and 16#f#))]);
end Util;

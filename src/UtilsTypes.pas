// Global declarations for reusable units.

unit UtilsTypes;
interface
uses
	MacOSAll;

type
	AnsiStringArray = array of AnsiString;
	StringArr = array of AnsiString;
	
	FSSpecString = AnsiString; // Marks a string as a file specification string
	FileArr = array of FSSpecString; // Should be FSRef in the future. No, strings for portability!
	CharFile = file of Char;
	
//implementation
end.

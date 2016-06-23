unit FileData;

// Skiss till ny datastruktur som gšr fšnsterdata mindre utspridd.

// All information about an editor window and its file

type
	FileRec = record

// From LWPEdit
		dirty: Boolean; // meaningless with MLTE (maybe not with new undo)
		boundToFile: Boolean;	// What file is this?
		fileSpec: FSSpecString; // It is this file!
		dataOnDisk: Handle;	{Data from last read from or write to disc}

		functionView, usesView: HIViewRef; // Views for pop-up menus

// From LWPGlobals
		editWind: WindowPtr;
		editWindIsMain: Boolean;
		editMainWindIsProgram: Boolean; // To tell a library from a program
		editWindIsCocoaProgram: Boolean; // 0.8.11 - Cocoa in Pascal program, program with CocoaAll
		editWindIsiOSProgram: Boolean; // 0.8.11 - FPC iPhone, with iPhoneAll
		teEdit: TXNObject;		{ handle to text window TextEdit record }
		
		// Function menu, created by ColorCoder
		editFunctionMenu: MenuHandle;
		editFunctionNameStart: array[1..kMaxFunctionMenuItems] of Longint;
		editFunctionNameEnd: array[1..kMaxFunctionMenuItems] of Longint;
		
		// More data from ColorCoder, for uses menu and auto-framework
		usesList: AnsiStringArray;
		frameworkList: AnsiStringArray;
		extraOptions: AnsiString;
		extraLinkOptions: AnsiString;
	end;

var
	gW: array of FileRec;
//	gW: array [1..kMaxEditWindows] of FileRec;



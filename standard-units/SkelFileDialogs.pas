// SkelFileDialogs
// Based on the older NSStandardFile, but without the ambition to be backwards
// compatible with Apple's old standard file calls. Stripped down the number of
// calls to what I really find relevant.

{$mode objfpc}
{$modeswitch objectivec1}

unit SkelFileDialogs;
interface
uses
	ctypes, MacOSAll, CocoaAll, BaseUnix;

type
//	SFTypeList = ARRAY [0..3] OF OSType;
	SkelFileReplyPtr = ^SkelFileReply;
	SkelFileReply = RECORD
		sfGood:				BOOLEAN;
//		sfReplacing:		BOOLEAN;
//		sfType:				OSType;
//		sfFile:				FSSpec;
//		sfScript:			ScriptCode;
//		sfFlags:			INTEGER;
//		sfIsFolder:			BOOLEAN;
//		sfIsVolume:			BOOLEAN;

		// FSRef support
		getFileFSRef: FSRef; // Only for SkelGetFile!
		parentDirFSRef: FSRef; // Parent dir
		fileNameAndPathCF: CFStringRef; // Name & path
		fileNameAndPath: AnsiString; // Name & path in UTF-8
		fileNameAndPathMacRoman: AnsiString; // Name & path in MacRoman
		parentDirName: AnsiString; // UTF-8
		fileURL: CFURLRef;
		
		fileName: AnsiString; // Name in UTF-8

//		sfFile2: FSSpec2;
	END;
//	FileFilterUPP = UniversalProcPtr;
//	SFTypeListPtr = ^SFTypeList;
//	ConstSFTypeListPtr = SFTypeListPtr;

// Simple functions for reading or saving a file using full path
// or current directory
function ReadEntireFile(fileNameWithPath: AnsiString): AnsiString;
function WriteStringToFile(fileNameWithPath, dataString: AnsiString): Boolean;

// File dialogs. All are modeless, can be in sheets or not.
type
	SkelFileProc = procedure(reply: SkelFileReply; userData: Pointer);

// Get a folder
PROCEDURE SkelGetFolder(parentWindow: NSWindow; callback: SkelFileProc; userData: Pointer);

// Obsolete! Untyped put file, this is primitive and undesirable.
// Possibly usable for creating folders.
//PROCEDURE SkelPutFile(prompt: AnsiString; defaultName: Str255; parentWindow: NSWindow;
//			callback: SkelFileProc; userData: Pointer);overload;

// Preferred functions: with type lists
PROCEDURE SkelGetFile(typeList: NSArray; parentWindow: NSWindow;
			callback: SkelFileProc; userData: Pointer); overload;
PROCEDURE SkelPutFile(typeList, typesNamesList: NSArray; prompt: AnsiString; defaultName: Str255;
			parentWindow: NSWindow; callback: SkelFileProc; userData: Pointer);overload;
PROCEDURE SkelPutFile(typeList, typesNamesList: array of AnsiString; prompt: AnsiString; defaultName: Str255;
			parentWindow: NSWindow; callback: SkelFileProc; userData: Pointer);overload;
PROCEDURE SkelGetFile(typeList: array of AnsiString; parentWindow: NSWindow;
			callback: SkelFileProc; userData: Pointer); overload;

implementation

function CFStringToAnsiString(input: CFStringRef): AnsiString;
var
	output: AnsiString;
	used: Longint;
begin
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingUTF8, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	result := output;
end;

function CFStringToMacRomanAnsiString(input: CFStringRef): AnsiString;
var
	output: AnsiString;
	used: Longint;
begin
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, nil, CFStringGetLength(input)*2, used);
	SetLength(output, used);
	CFStringGetBytes( input, CFRangeMake(0, CFStringGetLength(input)),
			kCFStringEncodingMacRoman, Ord('^'), false, @output[1], CFStringGetLength(input)*2, used);
	result := output;
end;

function AbsolutePathToFSRef(path: AnsiString): FSRef;
var
	ref: FSRef;
	isDirectory: Boolean;
	err: OSErr;
begin
	err := FSPathMakeRef(@path[1], ref, @isDirectory);
	if err <> noErr then
		WriteLn('FSPathMakeRef failed');
	result := ref;
end;

function StringToNSString(input: AnsiString): NSString;
begin
	result := NSString.stringWithUTF8String(PChar(input));
	// Autoreleased
end;

// For debugging, write an NSArray of NSString
(*
procedure WriteNSArray(arr: NSArray);
var
	s: NSString;
	fname: String;
	i: Longint;
begin
	for i := 0 to arr.count - 1 do
	begin
		s := NSString(arr.objectAtIndex(i));
		CFStringGetPascalString(CFStringRef(s), @fName, 255, kCFStringEncodingUTF8);
		WriteLn(fName);
	end;
end;*)

// ----------------------- Simple save and read ------------------------

function ReadEntireFile(fileNameWithPath: AnsiString): AnsiString;
var
	FD : Cint;
	data: AnsiString;
	amount: cint;
const
	kMaxAmount = 1024;
begin
	data := '';
	FD:=fpOpen (fileNameWithPath, O_RDONLY);
	if FD>0 then
	begin
		repeat
			SetLength(data, Length(data) + kMaxAmount);
			amount := fpRead (FD, data[Length(data) - kMaxAmount + 1], kMaxAmount);
		until amount < kMaxAmount;
		// Correct length for last step
		if amount > 0 then
			SetLength(data, Length(data) + amount - kMaxAmount) // Some read last time
		else
			SetLength(data, Length(data) - kMaxAmount); // Nothing read last time
		fpClose(FD);
	end;
	WriteLn('Read ', Length(data), ' bytes from ', fileNameWithPath);
	result := data;
end;

function WriteStringToFile(fileNameWithPath, dataString: AnsiString): Boolean;
var
	FD : Cint;
begin
	WriteLn('Writing to ', fileNameWithPath);
	FD:=fpOpen (fileNameWithPath, O_WrOnly or O_Creat);
	if FD>0 then
	begin
		if length(dataString)<>fpwrite (FD,dataString[1],Length(dataString)) then
			Writeln ('Error when writing to file !');
		fpClose(FD);
		result := true;
	end
	else
		result := false;
end;

// --------- Sheet-using versions ------------

// Menu controller for type menu (experimental)
type TSFMenuController = objcclass(NSObject)
// Variabler?
	typeList, typesNamesList: NSArray;
	mySavePanel: NSSavePanel;
	popup: NSPopUpButton;
	public
		procedure doMenuCommand(sender: id); message 'doMenuCommand:';
	end;

// This method handles the file type popup menu
procedure TSFMenuController.doMenuCommand(sender: id);
var
	i: Longint;
	nameFieldString, nameFieldStringWithExt: NSString;
begin
//	WriteLn(CFStringToAnsiString(CFStringRef(NSMenuItem(sender).title)));
	
	// JŠmfšr NSMenuItem(sender).title med typesNamesList!
	for i := 0 to typesNamesList.count-1 do
		if NSMenuItem(sender).title.isEqualToString(typesNamesList.objectAtIndex(i)) then
		begin
//			WriteLn('Found at index ', i);
			mySavePanel.setRequiredFileType(typeList.objectAtIndex(i)); // Change to setAllowedFileTypes?
			popup.setTitle(typesNamesList.objectAtIndex(i));
			
			// Added 131008; change file type when menu changed
			// Not correct yet
			nameFieldString := mySavePanel.nameFieldStringValue;
			nameFieldStringWithExt := NSString.stringWithFormat(NSSTR('%@.%@'),nameFieldString, typeList.objectAtIndex(i));
//			mySavePanel.setNameFieldStringValue(nameFieldStringWithExt);
			
		end;
end;


type SFSheetDelegate = objcclass(NSObject)
	public
		procedure sheetDidEnd_returnCode_contextInfo(sheet: NSWindow; returnCode: NSInteger; contextInfo: Pointer);
			message 'sheetDidEnd:returnCode:contextInfo:';
	end;
type
	SFSheetRec = record
		sheetProc: SkelFileProc;
		userData: Pointer;
		isPutFile: Boolean; // Getfile or Putfile?
		
		// NEW/experimental:
		menuController: TSFMenuController;
		typeList, typesNamesList: NSArray;
		popup: NSPopUpButton;
		formatsMenu: NSMenu;
	end;
	SFSheetPtr = ^SFSheetRec;

	{Get the file name from a path}
	function GetLastToken(s: AnsiString): AnsiString;
	var
		i: Longint;
	begin
		i := Length(s);
		while i > 0 do
		begin
			if s[i] = '/' then
			begin
//				SetLength(s, i-1);
				Exit(Copy(s, i+1, Length(s) - i));
			end;
			i := i - 1;
		end;
		Exit(s);
	end;

// This method is called when the sheet created by any of the sheet-using
// calls finish. We extract data from the sheet and pass to the callback.
procedure SFSheetDelegate.sheetDidEnd_returnCode_contextInfo(sheet: NSWindow;
	returnCode: NSInteger; contextInfo: Pointer);
var
	ctx: SFSheetPtr;
	reply: SkelFileReply;
	myOpenPanel: NSOpenPanel;
begin
//	WriteLn('Badabing!');
	// Get data from sheet!
	ctx := SFSheetPtr(contextInfo);
	myOpenPanel := NSOpenPanel(sheet);

	// Get result
	if returnCode = NSFileHandlingPanelOKButton then
	begin
		reply.fileURL := CFURLRef(myOpenPanel.URL);
		reply.fileNameAndPathCF := CFURLCopyFileSystemPath(reply.fileURL, kCFURLPOSIXPathStyle);
		reply.fileNameAndPath := CFStringToAnsiString(reply.fileNameAndPathCF);
		
		reply.parentDirName := CFStringToAnsiString(CFStringRef(myOpenPanel.directory));
		if not ctx^.isPutFile then
			reply.getFileFSRef := AbsolutePathToFSRef(reply.fileNameAndPath);
		reply.parentDirFSRef := AbsolutePathToFSRef(reply.parentDirName);
		reply.fileNameAndPathMacRoman := CFStringToMacRomanAnsiString(reply.fileNameAndPathCF);
		
		reply.fileName := GetLastToken(reply.fileNameAndPath);
	end
	else
		reply.sfGood := false;
	
	if contextInfo <> nil then
	begin
		ctx^.sheetProc(reply, ctx^.userData);
		
		// More to dispose!
		if ctx^.menuController <> nil then
		begin
			if ctx^.menuController.typeList <> nil then
				ctx^.menuController.typeList.autoRelease;
			if ctx^.menuController.typesNamesList <> nil then
				ctx^.menuController.typesNamesList.autoRelease;
			ctx^.menuController.autoRelease;
		end;
		if ctx^.formatsMenu <> nil then
			ctx^.formatsMenu.autoRelease;
		if ctx^.popup <> nil then
			ctx^.popup.autoRelease;
	end;
	Dispose(ctx);
//	myOpenPanel.dealloc; This is not our job to do.
//	myOpenPanel.autoRelease;
end;

var
	internalSheetDelegate: SFSheetDelegate;

// type list as NSArray/CFArray
// New variant for sheets, includes window and callback
// Works for modeless non-sheet dialogs too! Just pass nil for the parent window!
PROCEDURE SkelGetFile(typeList: NSArray; // VAR reply: SkelFileReply;
				parentWindow: NSWindow; callback: SkelFileProc;
				userData: Pointer); overload;
var
	myOpenPanel: NSOpenPanel;
	ctx: SFSheetPtr;
begin
	// if callback = nil or parentWindow = nil, ignore or call modal
	
	// Create panel
	myOpenPanel := NSOpenPanel.openPanel;
	
// Create delegate
	if internalSheetDelegate = nil then
		internalSheetDelegate := SFSheetDelegate.alloc;
	
// Create context data
	ctx := New(SFSheetPtr);
	ctx^.sheetProc := callback;
	ctx^.userData := userData;
	ctx^.isPutFile := false;
// Clear extra fields
	ctx^.menuController := nil;
	ctx^.popup := nil;
	ctx^.formatsMenu := nil;
	
// Run open file dialog
	myOpenPanel.beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo(
		NSSTR('/Documents'),
		NSSTR('the name'),
		typeList, // filetypes: NSArray
		parentWindow,
		internalSheetDelegate,
		sel_registerName('sheetDidEnd:returnCode:contextInfo:'), // didEndSaveSheet:returnCode:conextInfo:
		Pointer(ctx)); // This is where to put user data?
end;


// Simple clone of previous, can be merged
PROCEDURE SkelGetFolder(parentWindow: NSWindow; callback: SkelFileProc; userData: Pointer);
var
	myOpenPanel: NSOpenPanel;
	ctx: SFSheetPtr;
begin
	// if callback = nil or parentWindow = nil, ignore or call modal
	
	// Create panel
	myOpenPanel := NSOpenPanel.openPanel;
	myOpenPanel.setCanChooseDirectories(true);
	myOpenPanel.setCanChooseFiles(false);
	
// Create delegate
	if internalSheetDelegate = nil then
		internalSheetDelegate := SFSheetDelegate.alloc;
	
// Create context data
	ctx := New(SFSheetPtr);
	ctx^.sheetProc := callback;
	ctx^.userData := userData;
	ctx^.isPutFile := false;
// Clear extra fields
	ctx^.menuController := nil;
	ctx^.popup := nil;
	ctx^.formatsMenu := nil;
	
// Run open file dialog
	myOpenPanel.beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo(
		NSSTR('/Documents'),
		NSSTR('the name'),
		nil,
		parentWindow,
		internalSheetDelegate,
		sel_registerName('sheetDidEnd:returnCode:contextInfo:'), // didEndSaveSheet:returnCode:conextInfo:
		Pointer(ctx)); // This is where to put user data?
end;

(*
// Untyped SkelPutFile. Questionable if it is really useable.
PROCEDURE SkelPutFile(prompt: AnsiString; defaultName: Str255; parentWindow: NSWindow;
						callback: SkelFileProc; userData: Pointer);overload;
var
	mySavePanel: NSSavePanel;
	ctx: SFSheetPtr;
	promptCF, defaultNameCF: NSString;
begin
// Create panel
	mySavePanel := NSSavePanel.savePanel;
	
// Create delegate
	if internalSheetDelegate = nil then
		internalSheetDelegate := SFSheetDelegate.alloc;

// Create context data
	ctx := New(SFSheetPtr);
	ctx^.sheetProc := callback;
	ctx^.userData := userData;
	ctx^.isPutFile := true;
	
// Clear extra fields
	ctx^.menuController := nil;
	ctx^.popup := nil;
	ctx^.formatsMenu := nil;
	
	promptCF := StringToNSString(prompt);
	defaultNameCF := StringToNSString(defaultName);
	mySavePanel.setMessage(promptCF);

	mySavePanel.beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo(
		NSSTR('/Documents'), // This should be user configurable
		defaultNameCF,
		parentWindow,
		internalSheetDelegate,
		sel_registerName('sheetDidEnd:returnCode:contextInfo:'), // didEndSaveSheet:returnCode:conextInfo:
		Pointer(ctx)); // This is where to put user data?
end;*)

PROCEDURE SkelPutFile(typeList, typesNamesList: NSArray; prompt: AnsiString; defaultName: Str255;
			parentWindow: NSWindow; callback: SkelFileProc; userData: Pointer);overload;
var
	mySavePanel: NSSavePanel;
	ctx: SFSheetPtr;
	promptCF, defaultNameCF: NSString;
	formatsMenu: NSMenu;
	item1: NSMenuItem;
	popup: NSPopUpButton;
	i: Longint;
	btnFrame: NSRect = (origin:(x: 0; y: 0); size:(width: 300; height: 40));
	diff, count: Longint;
begin
// Create panel
	mySavePanel := NSSavePanel.savePanel;
	
// Create delegate
	if internalSheetDelegate = nil then
		internalSheetDelegate := SFSheetDelegate.alloc;
	
// Create context data
	ctx := New(SFSheetPtr);
	ctx^.sheetProc := callback;
	ctx^.userData := userData;
	ctx^.isPutFile := true;
// Clear extra fields
	ctx^.menuController := nil;
	ctx^.popup := nil;
	ctx^.formatsMenu := nil;
	
	// Check for mismatching arrays
	if (typeList <> nil) and (typesNamesList <> nil) then
	begin
		if typeList.count > typesNamesList.count then
		begin
			WriteLn('WARNING! Mismatching array lengths!');
			count := typesNamesList.count;
			diff := typeList.count - typesNamesList.count;
			NSMutableArray(typeList).removeObjectsFromIndices_numIndices(@count, diff);
		end;
		if typeList.count < typesNamesList.count then
		begin
			WriteLn('WARNING! Mismatching array lengths!');
			count := typeList.count;
			NSMutableArray(typesNamesList).removeObjectsFromIndices_numIndices(@count, typesNamesList.count - typeList.count);
		end;
	end;
	
	promptCF := StringToNSString(prompt);
	defaultNameCF := StringToNSString(defaultName);
	mySavePanel.setMessage(promptCF);
	mySavePanel.setAllowedFileTypes(typeList); // no go

	if typeList.count = 1 then
	// Only one allowed
		mySavePanel.setRequiredFileType(typeList.objectAtIndex(0))
	else
	if typeList.count > 0 then
	// Create popup menu with allowed types
	begin
		mySavePanel.setRequiredFileType(typeList.objectAtIndex(0));
		mySavePanel.setCanSelectHiddenExtension(true);
		formatsMenu := NSMenu.alloc;
		formatsMenu.initWithTitle(NSSTR('File formats:'));
	
		ctx^.menuController := TSFMenuController.alloc;
		ctx^.menuController.typeList := typeList;
		ctx^.menuController.typesNamesList := typesNamesList;
		ctx^.menuController.mySavePanel := mySavePanel;

		item1 := NSMenuItem.alloc;
		item1.initWithTitle_action_keyEquivalent(NSSTR('Dummy item'), nil, NSSTR(''));
		formatsMenu.addItem(item1);

		for i := 0 to typesNamesList.count-1 do
		begin
			item1 := NSMenuItem.alloc;
//			WriteLn(CFStringToAnsiString(CFStringRef(typesNamesList.objectAtIndex(i))));
			item1.initWithTitle_action_keyEquivalent(typesNamesList.objectAtIndex(i), nil, NSSTR(''));
			formatsMenu.addItem(item1);
			
		// And someone must handle the menu too!
			item1.setTarget(ctx^.menuController);
			item1.setAction(sel_registerName(PChar('doMenuCommand:')));
		end;
		popup := NSPopUpButton.alloc;
		popup.initWithFrame_pullsDown(btnFrame, true);
		popup.setMenu(formatsMenu);
		popup.setTitle(typesNamesList.objectAtIndex(0));
//		popup.setTitle(NSSTR('A popup menu'));
		mySavePanel.setAccessoryView(popup);
		
		// Save for future disposal
		ctx^.popup := popup;
		ctx^.formatsMenu := formatsMenu;
		ctx^.menuController.popup := popup;
	end;
		
	mySavePanel.beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo(
			NSSTR('/Documents'), // This should be user configurable
			defaultNameCF,
			parentWindow,
			internalSheetDelegate,
			sel_registerName('sheetDidEnd:returnCode:contextInfo:'), // didEndSaveSheet:returnCode:conextInfo:
			Pointer(ctx)); // This is where to put user data?
end;


PROCEDURE SkelPutFile(typeList, typesNamesList: array of AnsiString; prompt: AnsiString; defaultName: Str255;
				parentWindow: NSWindow; callback: SkelFileProc; userData: Pointer);overload;
var
	i: Longint;
	typeListNS, typesNamesListNS: NSMutableArray;
begin
	typeListNS := NSMutableArray.new;
	for i := Low(typeList) to High(typeList) do
	begin
		typeListNS.addObject(StringToNSString(typeList[i]));
		// release???
	end;
	typesNamesListNS := NSMutableArray.new;
	for i := Low(typesNamesList) to High(typesNamesList) do
	begin
		typesNamesListNS.addObject(StringToNSString(typesNamesList[i]));
		// release???
	end;
	
	SkelPutFile(typeListNS, typesNamesListNS, prompt, defaultName, parentWindow,
						callback, userData);
end;

PROCEDURE SkelGetFile(typeList: array of AnsiString; parentWindow: NSWindow;
				callback: SkelFileProc; userData: Pointer); overload;
var
	i: Longint;
	typeListNS: NSMutableArray;
begin
	typeListNS := NSMutableArray.new;
	for i := Low(typeList) to High(typeList) do
	begin
//		WriteLn(typeList[i]);
		typeListNS.addObject(StringToNSString(typeList[i]));
		// release???
	end;
	
	SkelGetFile(typeListNS, parentWindow, callback, userData);
end;

end.

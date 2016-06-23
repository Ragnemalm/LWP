// Lightweight IDE, editor unit
// © Ingemar Ragnemalm 2006-2015
// Lots of changes 2006-2013
// 130416: Reorganized the event processing routine, split it in three. Also made first prototype for code completion and speeded up block indentation.
// 131010: Revised for new undo system.
// 1401??: Revised to use Halda.
// 1402?? working on Halda QDCG revision.
// 140315: QGCG revision mostly working.
// 140506: Revision fully working with split view support.
// 150406: First Retina supporting version.
// 150630: Added block-commenting.

{$mode macpas}
unit LWPEdit;
interface
	uses
		MacOSAll, UtilsTypes, LWPGlobals, TransSkel4, CarbonStandardFile,
		FileUtils, Settings, ColorCoding, ProcessUtils, AlertsUtils,
		Console, TXNUtilities, {CustomTXNUnit,} LWPColorGlobals, Breakpoints,
		RecentFiles, CodeCompletion, UndoUnit,
		Halda, HaldaTypes, HaldaSysDept, SysUtils, EditQDCGParts, QDCG,
		IncludeAnalyzer, Backtrace;
	
	function EditWindInit: WindowPtr;
	procedure EditWindEditMenu (item: integer);
	function IsEditWindow(w: WindowPtr): Boolean;
	function GetEditFSSpec(w: WindowPtr; var theSpec: FSSpecString): OSErr;
	procedure EditWindFileMenu(item:Integer);
//	procedure EditOpenAndSelect(fileName: AnsiString; rowNumber, columnNumber: Longint);
	function EditOpenAndSelect(fileName: AnsiString; {Str255;} rowNumber, columnNumber: Longint): Longint;
	
	procedure RotateWindows;
	function GetFrontEditWindow: WindowPtr;
	function GetFrontMainWindow: WindowPtr;
	procedure InvalFrontEditWindow; // Re-check the front edit when requested
	procedure InvalFrontMainWindow; // Re-check the front main when requested
	function CalcFrontEditWindow: WindowPtr; // Re-check the front edit now
	function CalcFrontMainWindow: WindowPtr; // Re-check the front main now
	
	function EditGetCurrentTE: HaldaPtr;
//	function EditGetCurrentTEIndex: Longint;
	procedure EditSaveAll;
	
	procedure EditOpen;
	procedure EditSelectWindowNumber(item: Integer); {1 and up}
	
	procedure EditAEOpen(fs: FSRef; isLastInBatch: Boolean);
	procedure EditAEPrint(fs: FSRef; isLastInBatch: Boolean);
	
	// Internal help functions exported for the multi-file search
	function TestIfDirty(editIndex: Integer): Boolean;
	function OpenFileToWindow(theSpec: FSSpecString): Integer;
	function FileIsOpen(theSpec: FSSpecString): Longint;
	function GetAllEditWindows: FileArr;
	function GetAllEditWindowsSelections: StringArr;
//	function GetIndexFromSpec(theSpec: FSSpecString): Longint;
	function GetPathFromIndex(editIndex: Longint): FSSpecString;

	procedure SynchPopups(editIndex: Longint);
	procedure UpdateLineNumber(editWind: WindowPtr);
	procedure UpdateFinger(fileName: AnsiString; lineNumber: Longint);
	function GetWindowNumber(w: WindowPtr): Longint;
	procedure GoToLine(w: WindowPtr; rowNumber, columnNumber: Longint);
	function FindOrOpen(fileName: AnsiString{Str255}; fileInHomeFolder: FSSpecString): Integer; // Used internally and also by ClassBrowser
	
// Must be visible since these constants are used from DoCommand in the main file.
	const
		kFileNew = 1;
		kFileOpen = 2;
		kFileOpenSelection = 3;
		kFileBackSelection = 4;
		kFileClose = 6;
		kFileSave = 7;
		kFileSaveAs = 8;
		kFileRevert = 9;
		kFilePageSetup = 11;
		kFilePrint = 12;
		
		kEditUndo = 1;
		kEditRedo = 2;
		kEditCut = 4;
		kEditCopy = 5;
		kEditPaste = 6;
		kEditClear = 7;
		kEditSelectAll = 8;
//		kEditSettings = 9;
//		kEditEditorSettings = 10;
//		kEditManualColorCode = 9;

		kEditShiftLeft = 10;
		kEditShiftRight = 11;
		kFormatCode = 12;
		kToggleDual = 13;
		kEditBlockCommentIn = 14;
		kEditBlockCommentOut = 15;

var
	gWindowMenuOriginalItemCount: Longint;

	var
		dirty, autodirty: array [1..kMaxEditWindows] of Boolean; // meaningless with MLTE

implementation
{ edit menu item numbers }
uses
	Lightbugs, BuildWithPascal {included for AppendCustomPaths}, GoToLineDialog, ProjectWindow,
	ClosedFilesDataManager, FormatterUnit, CFormatterUnit, AutoReopenUnit, ErrorMessageWindow;
		
		// Search
		// Find
		// Enter selection
		// Find again
		// Replace and find again

// Constants for the layout (breakpoint area and line numbers box)
const
	kTextBoxLeftMargin = 10;
	kTextBoxLeftMarginPlus = 14;
	kLineNumberBoxWidth = 60;
	kLineNumberBoxHeight = 15;
	
	var
//		teEdit: array [1..kMaxEditWindows] of TXNObject;		{ handle to text window TextEdit record }
//		teFrameID: array [1..kMaxEditWindows] of TXNFrameID;	{ frame id (pointless?) }
//		dirty: array [1..kMaxEditWindows] of Boolean; // meaningless with MLTE MOVED UP TO INTERFACE
		boundToFile: array [1..kMaxEditWindows] of Boolean;	// What file is this?
		fileSpec: array [1..kMaxEditWindows] of FSSpecString;
		
		dataOnDisk: array[1..kMaxEditWindows] of AnsiString;	{Data from last read from or write to disc}
		
		//editWindCount: Longint;
		
		//editScroll: array [1..kMaxEditWindows] of ControlHandle;	{ edit window scroll bar }
		//helpLine, halfPage: array [1..kMaxEditWindows] of integer;	{ line currently at top of window }
		//autoScrollProc: TEClickLoopUPP;

// Pop-up menu views, allocated by EditWindInit
//	functionIconRef, usesIconRef: IconRef;
	functionView, usesView, nameView: array [1..kMaxEditWindows] of HIViewRef;
	gNameViewVisible: Boolean = true;
// nameView is an invisibe view for catching mouse downs in the window border

procedure DoMouse(thePoint: Point; theTime: UInt32; theMods: Integer); forward;

	
	function GetAllEditWindows: FileArr;
	var
		files: FileArr;
		i: Longint;
	begin
		SetLength(files, 0);
		for i := 1 to kMaxEditWindows do
		// Change 150630: Only include open windows!
		if teEdit[i] <> nil then
			if boundToFile[i] then
			begin
				SetLength(files, Length(files) + 1);
				files[High(files)] := fileSpec[i];
			end;
		return files;
	end;
	
	// For auto-reopen
	function GetAllEditWindowsSelections: StringArr;
	var
		files: StringArr;
		i: Longint;
		rowNumber, columnNumber: Longint;
	begin
		SetLength(files, 0);
		for i := 1 to kMaxEditWindows do
		if teEdit[i] <> nil then
			if boundToFile[i] then
			begin
				HGetSelection(teEdit[i], rowNumber, columnNumber);
				SetLength(files, Length(files)+2);
				Str(rowNumber, files[High(files)-1]);
				Str(columnNumber, files[High(files)]);
			end;
		return files;
	end;
	
	
	function GetEditFSSpec(w: WindowPtr; var theSpec: FSSpecString): OSErr;
	var
		editIndex: Integer;
	begin
		GetEditFsSpec := -1;
		
//		editIndex := GetWRefCon(GetWindowFromPort(GetQDGlobalsThePort));
		editIndex := GetWRefCon(w);
		if editIndex > 0 then
			if editIndex <= kMaxEditWindows then
				if editWind[editIndex] <> nil then
					if editWind[editIndex] = w then
					begin
						GetEditFsSpec := noErr;
						theSpec := fileSpec[editIndex];
					end;
	end;

	procedure SynchUndoMenu;
	var
		editIndex: Integer;
		w :WindowPtr;
	begin
		w := GetFrontEditWindow;
		if w <> nil then
		begin
//			editIndex := GetWRefCon(GetWindowFromPort(GetQDGlobalsThePort)); // Shouldn't this be made from the front editing window?
			editIndex := GetWRefCon(w);
			if editIndex >= 1 then
			begin
				if CanUndoText(editIndex) then
					EnableMenuItem(editMenu, kEditUndo)
				else
					DisableMenuItem(editMenu, kEditUndo);
				if CanRedoText(editIndex) then
					EnableMenuItem(editMenu, kEditRedo)
				else
					DisableMenuItem(editMenu, kEditRedo);
			
			// Force-enable the rest for now
			// It seems OSX messes with this?
			// Should be based on selection and clipboard
				EnableMenuItem(editMenu, kEditCut);
				EnableMenuItem(editMenu, kEditCopy);
				EnableMenuItem(editMenu, kEditPaste);
				EnableMenuItem(editMenu, kEditClear);
				EnableMenuItem(editMenu, kEditSelectAll);
				Exit(SynchUndoMenu);
			end;
		end;
		
		// No window - disable
		DisableMenuItem(editMenu, kEditUndo);
		DisableMenuItem(editMenu, kEditRedo);
		DisableMenuItem(editMenu, kEditCut);
		DisableMenuItem(editMenu, kEditCopy);
		DisableMenuItem(editMenu, kEditPaste);
		DisableMenuItem(editMenu, kEditClear);
		DisableMenuItem(editMenu, kEditSelectAll);
	end;

	// No longer used?!
(*	procedure Activate (active: Boolean);
	var
		editIndex: Integer;
		s: Str255;
	begin
		editIndex := GetWRefCon(GetWindowFromPort(GetQDGlobalsThePort));
//		DrawGrowBox(editWind[editIndex]);
		if active then
			begin
			// If split view, focus which one?
				HFocus(teEdit[editIndex]^.views[0], true);
				HSetScrollbarState(teEdit[editIndex]^.views[0], true);
				//TXNFocus(teEdit[editIndex]);
				SynchUndoMenu;
				EnableMenuItem(editMenu, kEditCut);
				EnableMenuItem(editMenu, kEditCopy);
				EnableMenuItem(editMenu, kEditPaste);
				EnableMenuItem(editMenu, kEditClear);
				EnableMenuItem(editMenu, kEditSelectAll);
			end
		else
			begin
				// Check if it is not an edit window? If so, don't save/revert...
				HFocus(teEdit[editIndex]^.views[0], false);
				HSetScrollbarState(teEdit[editIndex]^.views[0], false);
				// TXNActivate(teEdit[editIndex]);
				// TXNFocus(teEdit[editIndex]);
				// EnableMenuItem(editMenu, undo);
			end;
		
		// Update settings to reflect the current main window.
		editIndex := GetWindowNumber(GetFrontMainWindow);
		GetWTitle(GetFrontMainWindow, s);
		MessageAlert('Loading settings for ', s);
		LoadSettingsForWindow(editIndex);
	end;*)

//	function FindOrOpen(fileName: Str255; fileInHomeFolder: FSSpecString): Integer; forward;
//	function FindOrOpen(fileName: AnsiString{Str255}; fileInHomeFolder: FSSpecString): Integer; forward;

	procedure RebuildWindowMenu;
	var
		iItemCount, I: Longint;
		theTitle: Str255;
		count: Longint;
	begin
	// Delete all editor windows in the Window menu,
	// and then append all current windows
		iItemCount := CountMenuItems(windowMenu);
		for i := gWindowMenuOriginalItemCount to iItemCount do
			DeleteMenuItem(windowMenu, gWindowMenuOriginalItemCount+1);
//		for i := 4 to iItemCount do
//			DeleteMenuItem(windowMenu, 5);
		
		count := 0;
		for i := 1 to kMaxEditWindows do
			if teEdit[i] <> nil then
			begin
				count := count + 1;
				GetWTitle(editWind[i], theTitle); { + command-number?}
				if count < 10 then
//					AppendMenu(windowMenu, theTitle + '/' + Char(count + Ord('0')) )
					MyAppendMenu(windowMenu, theTitle, Char(count + Ord('0')))
				else
					MyAppendMenu(windowMenu, theTitle, ' ');
//					AppendMenu(windowMenu, theTitle);
//				WriteLn('Appended ', theTitle, ' to windowMenu for window ', i);
			end;
	end;

	procedure EditSelectWindowNumber(item: Integer); {1 and up}
	var
		i: Longint;
		count: Longint;
	begin
		count := 0;
		for i := 1 to kMaxEditWindows do
		begin
			if teEdit[i] <> nil then
			begin
				count := count + 1;
				if count = item then
			      	   SelectWindow(editWind[i]);
			end;
		end;
	end;

	procedure ConvertToMacLineBreaks(var chars: AnsiString);
	begin
		// 1. Gör om alla CRLF till CR
		// 2. Gör om alla LF till CR
//		CRLFtoCR(chars);
		chars := StringReplace(chars, #13#10, #13, [rfReplaceAll]);
		chars := StringReplace(chars, #10, #13, [rfReplaceAll]);
	end;
	
	// Samma som ovan men till LF (Unix)
	procedure ConvertToUnixLineBreaks(var chars: AnsiString);
	begin
		// 1. Gör om alla CRLF till CR
		// 2. Gör om alla CR till LF
		chars := StringReplace(chars, #13#10, #13, [rfReplaceAll]);
		chars := StringReplace(chars, #13, #10, [rfReplaceAll]);
(*		
		CRLFtoCR(chars);
		
		// 2 görs lättast genom att loopa genom hela arrayen
		// FUNKAR DETTA PÅ STORA STRÄNGAR??? Säkrare att Mungra?
		for foundOffset := 1 to Length(chars) do
			if chars[foundOffset] = Char(13) then 
			begin
				chars[foundOffset] := Char(10);
			//	WriteLn('Bytte en LF vid ', foundOffset);
			end;*)
	end;
	
	function FileIsOpen(theSpec: FSSpecString): Longint;
	var
		i: Longint;
	begin
		for i := 1 to kMaxEditWindows do
		begin
			if teEdit[i] <> nil then
				if boundToFile[i] then
//					if theSpec = fileSpec[i] then
// An attempt to make LWP tolerant for case errors. 141119
					if LowerCase(theSpec) = LowerCase(fileSpec[i]) then
					begin
						FileIsOpen := i; // Bugfix 080307
						Exit(FileIsOpen);
					end;
		end;
		FileIsOpen := 0; // Found nothing - not open
	end;
	
	procedure UpdateWindowViews(editIndex: Longint);forward;

	// Open file with given FSSpecString, return the window's index
	function OpenFileToWindow(theSpec: FSSpecString): Integer;
	var
		err: OSErr;
		editIndex: Integer;
		theWind: WindowPtr;
		i, j: Longint;
		theData: AnsiString;
//		readOnlyFlag: Boolean;
	begin
//WriteLn('OpenFileToWindow');
		// Kolla om filen redan är öppen!
//WriteLn('Is it open?');

		i := FileIsOpen(theSpec);
		if i > 0 then
		begin
//WriteLn('Yes!');
			SelectWindow(editWind[i]);
			OpenFileToWindow := i; // Bugfix 080307
			Exit(OpenFileToWindow);
		end;
//WriteLn('No!');
		
		if not FileExists(theSpec) then
		begin
//WriteLn('There is no file "', theSpec, '"');

			MessageAlert('Failed to open file', theSpec + ' does not exist');
//			MessageAlert('Failed to open file', UTF8ToMacRoman(theSpec) + ' does not exist');
			return 0;
		end;

		if GetExtensionType(theSpec) = kExtTypeUnknown then
			if (LowerCase(GetLastToken(theSpec)) <> 'makefile') and (Copy(LowerCase(GetLastToken(theSpec)), 1, 6) <> 'readme') then
				if not YesNoQuestionAlert('Are you sure you want to open ' + GetLastToken(theSpec), 'Unknown file type') then
					Exit(OpenFileToWindow);
		
		// Check file size! (Added 160220)
		if GetFileSize(theSpec) > 1000000 then
				if not YesNoQuestionAlert('Are you sure you want to open ' + GetLastToken(theSpec), 'This is a very large file') then
					Exit(OpenFileToWindow);		

//WriteLn('Time to read to string');
		theData := ReadFileToString(theSpec, err);
// Convert to CR should not be needed - done by Halda
(*		for i := 1 to Length(theData) do
			if theData[i] = #10 then
			begin
				theData[i] := #13;
				WriteLn('LF/CR at ', i);
			end;*)

//		WriteLn('Loaded ', theSpec, ' to string');
		if err <> noErr then
		begin
			MessageAlert('Failed to open file', UTF8ToMacRoman(theSpec));
			OpenFileToWindow := -1;
			Exit(OpenFileToWindow);
		end;
//		if readOnlyFlag then
//		begin
//			MessageAlert('Warning: File is read only', UTF8ToMacRoman(theSpec));
//		end;
		
		// Look for the evil $CA character (often comes when pasting code from a web page).
		for i := 1 to Length(theData) do
			if theData[i] = Char($CA) then
			begin
				if YesNoQuestionAlert('Zap gremlins?', 'There seems to be bad characters in the file. Shall I remove them?') then
				begin
					for j := 1 to Length(theData) do
						if theData[j] = Char($CA) then
							theData[j] := ' ';
				end;
				Leave;
			end;

		theWind := EditWindInit; {OBS! Kan bli väldigt fel om denna inte lyckas!!!}
		if theWind = nil then {så vi kolla om det gick bra}
		begin
			DebugStr('Failed to create window');
			Exit(OpenFileToWindow);
		end;

//		editIndex := GetWRefCon(FrontWindow); // Why not theWind? I don't remember.
		editIndex := GetWRefCon(theWind); // Why not theWind? I don't remember.
		
//		WriteLn('Loaded ', theSpec, ' to string');
		HSetText(teEdit[editIndex], theData);
		InitUndoForIndex(editIndex);
		
		// Save memory copy of original data
		dataOnDisk[editIndex] := theData;
		
		//Activate(true);
		
		fileSpec[editIndex] := theSpec;
		boundToFile[editIndex] := true;
		dirty[editIndex] := false;
		SetWTitle(editWind[editIndex], UTF8ToMacRoman(GetLastToken(fileSpec[editIndex]))); // SetWTitle is MacRoman 121107
// Bra för debugging:
//		SetWTitle(editWind[editIndex], UTF8ToMacRoman(fileSpec[editIndex])); // SetWTitle is MacRoman 121107

// Go to start (could also be to old selection if it had been saved)
		HSetSelection(teEdit[editIndex], 0, 0);
		HShowSelection(teEdit[editIndex]^.views[0], true);

		//if gFlags^^.colorCodeOnOpen then
		if editIndex > 0 then
			ResetColorCoder(teEdit[editIndex], editIndex); {Efter öppnande!}
//		WriteLn('Color scheme at ResetColorCoder = ', gSettings.textSaveColorCoding);
		SynchPopups(editIndex);

// If read-only, NOT editable!
// This should be changed at "save as"
		
		UpdateWindowViews(editIndex);
		OpenFileToWindow := editIndex;

//WriteLn('UpdateWindowViews done');
		
		RebuildWindowMenu;
		RefreshProjectWindow;
//WriteLn('RefreshProjectWindow done');

		if editIndex = GetWRefCon(GetFrontMainWindow) then
			LoadSettingsForWindow(editIndex);
		
		SaveToRecent(theSpec);
//WriteLn('SaveToRecent done');

		AutoReopenSave; // Good time to update this!
	end;

function OpenFilter(pb: CInfoPBPtr): Boolean; MWPascal;
begin
	return false; // false = don't skip
end;

	const
		kMaxPath = 2048;
	procedure EditOpen;
	var
		editIndex: Integer;
		reply: ReplyArray;
		typeList: SFTypeList;
		i: Longint;
		path: AnsiString;
	begin
//WriteLn('EditOpen');
		typeList[0] := FOUR_CHAR_CODE('TEXT');
		typeList[1] := FOUR_CHAR_CODE('****'); // ???? Hjälper detta?
//		StandardGetFiles(nil, 2, @typeList, reply);
		StandardGetFiles(@OpenFilter, 2, @typeList, reply);
		
//WriteLn('EditOpen got ', Length(reply), ' replies');
		
		for i := 0 to High(reply) do
		begin
			if reply[i].sfGood then
			begin
//				SetLength(path, kMaxPath);
//				FSRefMakePath(reply[i].getFileFSRef, PChar(path), kMaxPath);
//				WriteLn(path, Length(path));

// Another problem with CarbonStandardFile. MacRoman/UTF8 is NOT correct there.

// BIG PROBLEM! SYSTEM VERSION PROBLEM?
//				path := MacRomanToUTF8(reply[i].filePath);
				path := reply[i].filePath; // No, it already was UTF8. No?!
// Version mess in CarbonStandardFile? Usage of FSRefToPathUTF8 vs FSRefToPath?
//				WriteLn('path = ', path);
//				WriteLn('MacRoman path = ', UTF8ToMacRoman(path)); // CRASH
//				WriteLn('UTF8 (again?) path = ', MacRomanToUTF8(path));
				path := MacRomanToUTF8(path);

//				WriteLn(path, Length(path));

//WriteLn('Tries to open ', path);

				editIndex := OpenFileToWindow(path);
				if editIndex < 0 then
					DebugStr('Could not open file');
			end
			else
				WriteLn('User cancel');
		end;
	end;
	
	function TestIfDirty(editIndex: Integer): Boolean;
	// Might be able to remove this in favor of a "dirty" flag in Halda
	var
		i, l1, l2: Longint;
	begin
		if not boundToFile[editIndex] then
		begin
			// How much data is there?
			if Length(teEdit[editIndex]^.text) = 0 then
				TestIfDirty := false
			else
				TestIfDirty := true;
		end
		else
		if Length(dataOnDisk[editIndex]) = 0 then TestIfDirty := true
		else
		if Length(dataOnDisk[editIndex]) <> Length(teEdit[editIndex]^.text) then
			TestIfDirty := true
		else
		begin
			l1 := Length(dataOnDisk[editIndex]);
			l2 := Length(teEdit[editIndex]^.text);
			if l2 > l1 then l1 := l2;
			for i := 1 to l1 do
			begin
				if not ((dataOnDisk[editIndex][i] in [#13, #10]) and (teEdit[editIndex]^.text[i] in [#13, #10])) then
				if dataOnDisk[editIndex][i] <> teEdit[editIndex]^.text[i] then // FARLIGT! BUGG!
				begin
					TestIfDirty := true;
					Exit(TestIfDirty);
				end;
			end;
			TestIfDirty := false;
		end;
	end;
	
	procedure Save(editIndex: Integer; as: Boolean);
	var
		reply: StandardFileReply;
		prompt, default: Str255;
//		chars: AnsiString;
//		refNum: Integer;
//		fileSize: Longint;
		err: OSErr;
		fileData: AnsiString;
	begin
		if not boundToFile[editIndex] then as := true;
		
		if as then
		begin
			prompt := 'Prompt';
			if boundToFile[editIndex] then
				default := GetLastToken(fileSpec[editIndex])
			else
				default := 'untitled';
			StandardPutFile(@prompt, default, reply);
			if reply.sfGood then
			begin
			// FRÅGA: Borde jag inte korta path? Men hur?
//				SetLength(path, kMaxPath);
//				FSRefMakePath(reply.getFileFSRef, PChar(path), kMaxPath);
//				SetLength(path, Length(path)); // ?????

//				fileSpec[editIndex] := MacRomanToUTF8(reply.filePath);
				fileSpec[editIndex] := reply.filePath;
				// Bug fix 130811. FilePath is already UTF8. No? CSF version?
				fileSpec[editIndex] := MacRomanToUTF8(fileSpec[editIndex]);
				// CHECK THIS!
				
//				WriteLn('Save as does special chars wrong!');
//				WriteLn('File path (MacRoman): ', UTF8ToMacRoman(reply.filePath), ' (should be clean)');
//				WriteLn('File path (UTF8): ', fileSpec[editIndex], ' (should be mangled)');
			end
			else
				WriteLn('User aborted save');
		end
		else
			reply.sfGood := true;

		if reply.sfGood then
		begin
//			WriteLn('Saving ', fileSpec[editIndex]);
			//chars := TEGetText(teEdit[editIndex]);
			
			// Check if save is needed, otherwise skip it! But only if not "save as".
			if not as then
				if not TestIfDirty(editIndex) then
				begin
//					WriteLn('Save not needed');
					SetWindowModified (editWind[editIndex], false);
					Exit(Save);
				end;
			
			fileData := teEdit[editIndex]^.text;
			
			// Save memory copy of original data
			dataOnDisk[editIndex] := fileData;
			
			// Make a copy
//			HandToHand(Handle(chars));
			if gSettings.textSaveFormat = kSaveUnix then
				ConvertToUnixLineBreaks(fileData);
			
//			WriteLn('Now writing ', fileSpec[editIndex]);
			err := WriteStringToFile(fileData, fileSpec[editIndex]);
			if err <> noErr then
			begin
				MessageAlert('Error: Failed to save file!', UTF8ToMacRoman(fileSpec[editIndex]));
				Exit(Save);
			end
			else
			begin
//				WriteLn('WriteStringToFile returned noErr!');
				SetWindowModified (editWind[editIndex], false);
			end;
			
			if not FileExists(fileSpec[editIndex]) then
			begin
//				MessageAlert('Error: Where is my file?', UTF8ToMacRoman(fileSpec[editIndex]));
				MessageAlert('Error: Where is my file? Your data might be unsaved!', fileSpec[editIndex]);
//				WriteLn('File does not exist!');
			end;
			
			boundToFile[editIndex] := true;
			dirty[editIndex] := false;
			SetWTitle(editWind[editIndex], UTF8ToMacRoman(GetLastToken(fileSpec[editIndex]))); // 121107

			UpdateWindowViews(editIndex); // Since the title has changed
			
			//if gSettings.colorCodeOnSave then
// PAJAR UNDO!!!
//			if editIndex > 0 then
//				BetterColorCoder(teEdit[editIndex], editIndex); {Efter sparandet!}
//				BufferedTXNColorCoding(teEdit[editIndex], editIndex); {Efter sparandet!}
			SynchPopups(editIndex);
			
			RebuildWindowMenu; // Added 080402
			
			// If save as, redo color coding (may have changed type) 150305
			if as then
				ResetColorCoder(teEdit[editIndex], editIndex);
			
			// If this is the main window, and either changed name or was not
			// main before, load settings
			if editIndex = GetWRefCon(GetFrontMainWindow) then
				LoadSettingsForWindow(editIndex);
		end;
		AutoReopenSave; // Good time to update this!
	end;
	
	
	
	procedure DoHalt; // Är det säkert att det är FrontWindow??? NEJ! Current port? Ja!
	var
		editIndex: Longint;
		w: WindowPtr;
		wTitle: Str255;
	begin
//DebugStr('DoHalt');

		w := GetWindowFromPort(GetQDGlobalsThePort);
		editIndex := GetWRefCon(w);

//DebugStr('DoHalt' + fileSpec[editIndex].name);

//		Close(editIndex);

// "Dirty" måste kollas! Men hur? Behövs nog speciellt test.
// Vad gör man där då? Jämför data mot sparat? Olika stora, dirty.
// Men hur vet jag att den är "ren"? Exakt kontroll duger vid
// sparfråga i alla fall.
// Kanske man ska ha en kopia av sparade data i minnet? Då kan vi testa ofta!
		dirty[editIndex] := TestIfDirty(editIndex);

		if dirty[editIndex] then
			// FRÅGA om man vill spara!
		begin
			GetWTitle(editWind[editIndex], wTitle);
			if YesNoQuestionStr('Save "' + wTitle + '" before closing?') then // ADD FILENAME
				Save(editIndex, false);
		end;
		{CloseWindow(editWind);}
		//TEDispose(teEdit[editIndex]);
//		TXNDeleteObject(teEdit[editIndex]);

		HDispose(teEdit[editIndex]);
//		HDisposeView(teEdit[editIndex]); görs av HDispose!
		
		// Dispose views (in dragbar)
		DisposeControl(functionView[editIndex]);
		DisposeControl(usesView[editIndex]);
		DisposeControl(nameView[editIndex]);
		functionView[editIndex] := nil;
		usesView[editIndex] := nil;
		nameView[editIndex] := nil;
		
		if editWind[editIndex] = GetFrontEditWindow then
			InvalFrontEditWindow;
		if editWind[editIndex] = GetFrontMainWindow then
			InvalFrontMainWindow;
		
		SkelRmveWind(editWind[editIndex]);
		DisposeWindow(editWind[editIndex]); // This is where the window goes away
		// file name&path = fileSpec[editIndex]
		//To search If the closed file is one of the open files, which opened before its main file and to add it's functions list 
		CloseJTFNForWindow(editIndex);
 		
		editWind[editIndex] := nil;
		teEdit[editIndex] := nil;
		boundToFile[editIndex] := false; // FIX 080206 - prevented opening of files that were recently open
		// NOT TESTED YET

		dataOnDisk[editIndex] := '';
		
		RebuildWindowMenu;
	end;

	procedure Close(editIndex: Integer);
	begin
// Everything needed is done in DoHalt, called by SkelRmveWind

		ClearBreakpoints(editIndex);

		SkelRmveWind(GetWindowFromPort(GetQDGlobalsThePort)); // Anropar DoHalt?
	end;
	
	procedure DoClose;
	var
		editIndex: Integer;
	begin
//DebugStr('DoClose');

		editIndex := GetWRefCon(GetWindowFromPort(GetQDGlobalsThePort));
		
		writeln('Closed: ', editIndex);

//DebugStr('DoClose' + fileSpec[editIndex].name);

		Close(editIndex);
	end;

(*procedure SetNoWrap(editIndex: Integer);
var
	controlTag: array [0..0] of TXNControlTag; {array 0..0?}
	controlData: array [0..0] of TXNControlData;
	err: OSErr;
begin
	if editIndex < 1 then Exit(SetNoWrap);
	if editIndex > 5 then Exit(SetNoWrap); // old junk constant
// This didn't work either:
	controlTag[0] := kTXNWordWrapStateTag;
	err := TXNGetTXNObjectControls (teEdit[editIndex], 1, @controlTag, @controlData);
	if err = noErr then
		controlData[0].uValue := 0; // Ord(kTXNNoAutoWrap);
	err := TXNSetTXNObjectControls (teEdit[editIndex], false, 1, @controlTag, @controlData);
end;*)


// Utility function that finds the "best" open file to use as "root"
// for a file search (typically passed to FindOrOpen when gLastMainFile
// is not wanted).
		function GetRootSpec(var theRootSpec: FSSpecString): OSErr;
		var
			theRootWindow: WindowPtr;
			err: OSErr;
		begin
			theRootWindow := GetFrontMainWindow;
			if theRootWindow = nil then
				theRootWindow := GetFrontEditWindow;
			if theRootWindow = nil then
				return -1;
			
			err := GetEditFSSpec(theRootWindow, theRootSpec);
			if err <> noErr then
				return err;
		end;


// This should go in some utility module
// and there may be an identical one somewhere.
// Yes - FileIsOpen!
(*function GetIndexFromSpec(theSpec: FSSpecString): Longint;
var
	i: Longint;
begin
	for i := Low(fileSpec) to High(fileSpec) do
	begin
		if boundToFile[i] then
			if theSpec = fileSpec[i] then // Must be replaced by comparison function
				return i;
	end;
end;*)

function GetPathFromIndex(editIndex: Longint): FSSpecString;
begin
	if editIndex > 0 then
		GetPathFromIndex := fileSpec[editIndex];
end;


// New procedure 111122
// Indent block right or left
procedure IndentSelection(doAddTab: Boolean);
var
	theTab: Char = #9;
	theSpace: Char = ' ';
	i: Longint;
	txnObj: HaldaPtr;
	startPos, endPos: Longint;
	isBreak, useSpace: Boolean;
	count: Longint;
	editIndex: Longint;
const
	LF = Char(10);
	CR = Char(13);
begin
		txnObj := EditGetCurrentTE;
		editIndex := GetWRefCon(FrontWindow);
		if editIndex < 1 then
			Exit(IndentSelection);
		if txnObj = nil then
			Exit(IndentSelection); // Inget fönster öppet

		HGetSelection(txnObj, startPos, endPos);
		
		// Two fixes on the selection:
		// Expand backwards to CR or start
		while (startPos > 0) and (txnObj^.text[startPos-1] <> CR) do
			startPos-=1;
		// If on start of line, expand one character (otherwise the row will not be indented).
		if startPos = endPos then
			if startPos < Length(txnObj^.text) then
				if txnObj^.text[startPos+1] <> CR then
					endPos += 1;
		
		// Sök bakåt och framåt till CR/mellanslag mm, eller ände
		count := 0;
//		for i := startPos to endPos-1 do
		for i := endPos-1 downto startPos do
		begin
			if i > 1 then
				isBreak := (txnObj^.text[i-1] in [CR, LF]) and (not (txnObj^.text[i] in [CR, LF]))
			else
				isBreak := true; // Start of text is considered a break
			
			// Support space-based indentation - somewhat
			useSpace := txnObj^.text[i] = ' ';
			
			if isBreak then
				if doAddTab then
				begin
				// Add a TAB
				
					if useSpace then
						SetTextUndoable(editIndex, i, i, theSpace)
					else
						SetTextUndoable(editIndex, i, i, theTab);
					count += 1;
				end
				else
				begin
					if txnObj^.text[i] in [#9, ' '] then
					begin
					// Get rid of a TAB if there is one
						SetTextUndoable(editIndex, i, i+1, '');
						count -= 1;
					end;
				end;
		end;
		HSetSelection(txnObj, startPos, endPos+count);

// Now we need an update!!
	HInvalViews(txnObj);
end;


procedure BlockCommentSelection(add: Boolean);
var
	i: Longint;
	txnObj: HaldaPtr;
	startPos, endPos: Longint;
	isBreak{, useSpace}: Boolean;
	count: Longint;
	editIndex: Longint;
const
	LF = Char(10);
	CR = Char(13);
begin
		txnObj := EditGetCurrentTE;
		editIndex := GetWRefCon(FrontWindow);
		if editIndex < 1 then
			Exit(BlockCommentSelection);
		if txnObj = nil then
			Exit(BlockCommentSelection); // Inget fönster öppet

		HGetSelection(txnObj, startPos, endPos);
		
		// Two fixes on the selection:
		// Expand backwards to CR or start
		while (startPos > 0) and (txnObj^.text[startPos-1] <> CR) do
			startPos-=1;
		// If on start of line, expand one character (otherwise the row will not be indented).
		if startPos = endPos then
			if startPos < Length(txnObj^.text) then
				if txnObj^.text[startPos+1] <> CR then
					endPos += 1;
		
		// Sök bakåt och framåt till CR/mellanslag mm, eller ände
		count := 0;
//		for i := startPos to endPos-1 do
		for i := endPos-1 downto startPos do
		begin
			if i > 1 then
				isBreak := (txnObj^.text[i-1] in [CR, LF]) and (not (txnObj^.text[i] in [CR, LF]))
			else
				isBreak := true; // Start of text is considered a break
			
			if isBreak then
				if add then
				begin
				// Add //				
					SetTextUndoable(editIndex, i, i, '//');
					count += 2;
				end
				else
				begin
					if Copy(txnObj^.text, i, 2) = '//' then
					begin
					// Get rid of // if there is one
						SetTextUndoable(editIndex, i, i+2, '');
						count -= 2;
					end;
				end;
		end;
		HSetSelection(txnObj, startPos, endPos+count);

// Now we need an update!!
	HInvalViews(txnObj);
end;



procedure DoOpenSelection;
var
		editIndex: Integer;
		txnObj: HaldaPtr;
		startPos, endPos: Longint;
		itemString: AnsiString;
		i, theTextSize: Longint;
		theRootSpec: FSSpecString;
		frontMain: WindowPtr;
begin
//			WriteLn('###EditWindFileMenu: case kFileOpenSelection');
	// Get text selected in the front editing window - or the message window!
	
	// Hitta txnObj
	// Antingen främsta editfönstret eller message window!
	txnObj := EditGetCurrentTE; // S: Is txnObj has details of front window?
	if txnObj = nil then
	begin
//				WriteLn('EditWindFileMenu/kFileOpenSelection: No edit window open');
		Exit(DoOpenSelection); // Inget fönster öppet
	end;
	// Hämta data
	HGetSelection(txnObj, startPos, endPos); //S: Is it data (text) highlighted in the front window?
	// Sök bakåt och framåt till CR/mellanslag mm, eller ände
	while (startPos > 1) and (txnObj^.text[startPos-1] in ['.', 'a'..'z', 'A'..'Z', '_', '0'..'9', '/']) do //S: why startPos of highlighted text is symbols?
		startPos := startPos - 1;
	theTextSize := Length(txnObj^.text);
	endPos := endPos - 1; // so we start testing at endPos, which is the first OUTSIDE
	while (endPos < theTextSize) and (txnObj^.text[endPos+1] in ['.', 'a'..'z', 'A'..'Z', '_', '0'..'9', '/']) do //S: why is this?
		endPos := endPos + 1;
	HSetSelection(txnObj, startPos, endPos+1);
	
	// Skapa sträng av det som hittas
//			SetLength(itemString, endPos - startPos+1);
//			BlockMove(@myChars^^[startPos], @itemString[1], endPos-startPos+1);
	itemString := Copy(txnObj^.text, startPos, endPos-startPos+1); //what copies this? I guess It is file name.
	// MoveBytes
	
	// FAILS IN GetRootSpec!!!
	// Strange: Returns error, but works.
	// I should probably check what error it is.
	//err := GetRootSpec(theRootSpec);
	frontMain := GetFrontMainWindow;
	
	{err :=} GetEditFSSpec(frontMain, theRootSpec);
	
	// TO DO: We should search all files "in the project" regardless of location (GetIncludeFileList).
	
	// FindOrOpen it!
//			WriteLn('###EditWindFileMenu/kFileOpenSelection: Looking for "', itemString, '" in the folder containing "', theRootSpec, '"');
	i := FindOrOpen(itemString, theRootSpec); 
	
	// This should work for .h files. If failed, try some good extensions. (I.e. in uses.)
	// Pascal units are given without extensions, so we must try all.
	// (TO DO: Extension list not complete. Better to work from a directory list.)
	if i = 0 then
		i := FindOrOpen(itemString + '.pas', theRootSpec);
	if i = 0 then
		i := FindOrOpen(itemString + '.p', theRootSpec);
	if i = 0 then
		i := FindOrOpen(itemString + '.Pas', theRootSpec);
	if i = 0 then
		i := FindOrOpen(itemString + '.PAS', theRootSpec);
	if i > 0 then
		SelectWindow(editWind[i]) // FÖRSÖK: Visa fönstret om det redan fanns!
	else
	begin
		// Search function data for open windows
		// Search function data for closed windows
		writeln('Item String: ', itemString);
		
		writeln('Item String in LC: ', LowerCase(itemString));
		//w := GetFrontMainWindow;

		editIndex:= getWRefCon(frontMain);
		
		PerformJumpToFunction(editIndex, itemString, theRootSpec);
	end;
end;

procedure DoBackSelection;
var
	editIndex: Longint;
begin
	editIndex := GetWRefCon(GetFrontMainWindow);
	if editIndex > 0 then
		PerformBackJTFN(editIndex);
end;


{	Handle Edit menu items for text window}
	
	procedure EditWindFileMenu(item:Integer);
	var
		//reply: StandardFileReply;
		//typeList: SFTypeList;
		//prompt, default: Str255;
		editIndex: Integer;
//		err: OSErr;
		//theWind: WIndowPtr;
//		txnObj: HaldaPtr;
//		mychars: CharsHandle;
//		startPos, endPos: Longint;
//		itemString: AnsiString;
//		i, theTextSize: Longint;
//		theRootSpec: FSSpecString;
	begin
		case item of
		kFileNew:
		begin
			{theWind :=} EditWindInit;
		end;
		kFileOpen:
		begin
			EditOpen;
		end;
		kFileOpenSelection:
			DoOpenSelection;
		kFileBackSelection:
			DoBackSelection;
			
		kFileClose:
		if IsEditWindow(FrontWindow) then
		begin
			editIndex := GetWRefCon(FrontWindow);
			Close(editIndex);
		end
		else
			HideWindow(FrontWindow); {All other windows can be hidden}
		kFileSave:
		if IsEditWindow(FrontWindow) then
		begin
			editIndex := GetWRefCon(FrontWindow);
			Save(editIndex, false);
		end;
		kFileSaveAs:
		if IsEditWindow(FrontWindow) then
		begin
			editIndex := GetWRefCon(FrontWindow);
			Save(editIndex, true);
		end;
		kFileRevert:
		if QuestionStr('Really go back to previous version?') then
		begin
			editIndex := GetWRefCon(FrontWindow);
			// Revert - put back memory copy!
			if Length(dataOnDisk[editIndex]) <> 0 then
			begin
//				SetTextUndoable(editIndex, kTXNStartOffset, kTXNEndOffset, dataOnDisk[editIndex]);
				SetTextUndoable(editIndex, dataOnDisk[editIndex]);
			end;
		end;
		kFilePageSetup:
		if IsEditWindow(FrontWindow) then
		begin
(*			editIndex := GetWRefCon(FrontWindow);
			if editIndex > 0 then
				err := TXNPageSetup(teEdit[editIndex]);*)
		end;
		kFilePrint:
		if IsEditWindow(FrontWindow) then
		begin
(*			editIndex := GetWRefCon(FrontWindow);
			if editIndex > 0 then
				err := TXNPrint(teEdit[editIndex]);*)
		end;
		otherwise
		end;
	end;


	procedure DoDialogEditMenu (item: integer);
		var
			theDialogWind: WindowPtr;
			theDialog: DialogPtr;
//			ignore: integer;
	begin
//		WriteLn('DoDialogEditMenu');
		Exit(DoDialogEditMenu); // Onödigt i moderna dialoger?
	
		theDialogWind := FrontWindow;
		theDialog := GetDialogFromWindow(FrontWindow);
		if GetWindowKind(theDialogWind) = dialogKind then
			case item of
				kEditCut: 
					begin
						DialogCut(theDialog);
						{ignore :=} ClearCurrentScrap; {ignore := ZeroScrap;}
						{ignore :=} TEToScrap;
					end;
				kEditCopy: 
					begin
						DialogCopy(theDialog);
						{ignore :=} ClearCurrentScrap; {ignore := ZeroScrap;}
						{ignore :=} TEToScrap;
					end;
				kEditPaste: 
					begin
						{ignore :=} TEFromScrap;
						DialogPaste(theDialog);
					end;
				kEditClear: 
					DialogDelete(theDialog);
			end;
	end;
	
	procedure ToggleDoubleView; forward;

	procedure EditWindEditMenu (item: integer);
	var
		editIndex: Integer;
		s: AnsiString;
		selStart, selEnd: Longint;
		fileType: Longint;
	begin
//		if item = kEditSettings then
//			DoSettings
//		else
		if IsEditWindow(FrontWindow) then
		begin
			editIndex := GetWRefCon(FrontWindow);
			case item of
				kEditUndo: 
					if CanUndoText(editIndex) then
					begin
						TouchColorCoder(editIndex, teEdit[editIndex]^.selStart-100); // Jump more backwards?
						UndoText(editIndex);
						UpdateLineNumber(editWind[editIndex]);
					end
  					else
  						SysBeep(1);
				kEditRedo: 
					if CanRedoText(editIndex) then
					begin
						TouchColorCoder(editIndex, teEdit[editIndex]^.selStart-100); // Jump more backwards?
						RedoText(editIndex);
						UpdateLineNumber(editWind[editIndex]);
					end
  					else
  						SysBeep(1);
				kEditCut:
				begin
					TouchColorCoder(editIndex, teEdit[editIndex]^.selStart-100); // Jump more backwards?
					CutText(editIndex);
					UpdateLineNumber(editWind[editIndex]);
				end;
				kEditCopy: 
					CopyText(editIndex);
//					{err :=} TXNCopy(teEdit[editIndex]);
				kEditPaste:
				begin
					TouchColorCoder(editIndex, teEdit[editIndex]^.selStart-100); // Jump more backwards?
					PasteText(editIndex);
					UpdateLineNumber(editWind[editIndex]);
				end;
				kEditClear: 
				begin
					TouchColorCoder(editIndex, teEdit[editIndex]^.selStart-100); // Jump more backwards?
					SetTextUndoable(editIndex, ''); // Replaces TXNSetData
					UpdateLineNumber(editWind[editIndex]);
				end;
				kEditSelectAll:
					HSelectAll(teEdit[editIndex]);
//				kEditSettings:
//					DoSettings;
//				kEditEditorSettings:
//					DoEditorSettings;
//				kEditManualColorCode:
					//SetNoWrap(GetWRefCon(FrontWindow));
//					BetterColorCoder(teEdit[GetWRefCon(FrontWindow)], GetWRefCon(FrontWindow)); {Dålig felkoll, men det är ett test!}

				kEditShiftLeft:
					IndentSelection(false);
				kEditShiftRight:
					IndentSelection(true);
				kFormatCode:
				begin
				// We might want to apply some settings here!
					fileType := GetExtensionType(fileSpec[editIndex]);
					if fileType = kExtTypePascal then
					begin
						s := teEdit[editIndex]^.text;
						s := PascalCRFormatter(s);
						s := PascalFormatter(s);
						HGetSelection (teEdit[editIndex], selStart, selEnd);
						SetTextUndoable(editIndex, 1, Length(teEdit[editIndex]^.text)+1, s);
						HSetSelection (teEdit[editIndex], selStart, selEnd);
					end;
					// 160220: C formatter applied on GLSL, C++, Java...
					if fileType in [kExtTypeC, kExtTypeGLSL, kExtTypeJava, kExtTypeObjC, kExtTypeCPP, kExtTypeCL] then
					begin
						s := teEdit[editIndex]^.text;
						s := CCRFormatter(s);
						s := CFormatter(s);
						HGetSelection (teEdit[editIndex], selStart, selEnd);
						SetTextUndoable(editIndex, 1, Length(teEdit[editIndex]^.text)+1, s);
						HSetSelection (teEdit[editIndex], selStart, selEnd);
					end;
				end;
				kToggleDual:
					ToggleDoubleView;
				kEditBlockCommentIn:
					BlockCommentSelection(true);
				kEditBlockCommentOut:
					BlockCommentSelection(false);
				otherwise
					;
			end; {case}
		end
		else
		if FrontWindow = helpWind then
		begin
			DoHelpEditMenu(item)
		end
		else
		if FrontWindow = theErrorMsgWind then
		begin
			// Copy current error message!
			DoErrorEditMenu(item);
		end
		else
			DoDialogEditMenu(item);
		SynchUndoMenu;
	end;

	function IsEditWindow(w: WindowPtr): Boolean;
	var
		i: Integer;
	begin
		IsEditWindow := false;
		if w <> nil then
			for i := 1 to kMaxEditWindows do
				if w = editWind[i] then
					IsEditWindow := true;

		{Alternativt:}
		{editIndex := GetWRefCon(w);}
		{IsEditWindow := editIndex <> 0}

	end;
	
	function CalcFrontEditWindow: WindowPtr;
	var
		front: WindowPtr;
//		title: Str255;
	begin
//		DebugStr('enter CalcFrontEditWindow');
		
		front := FrontWindow;
		while front <> nil do // (not IsEditWindow(front)) and (front <> nil) do
		begin
			if IsEditWindow(front) then
			begin
//				GetWTitle(front, title);
//				DebugStr('Found edit window: ' + title);
				
				CalcFrontEditWindow := front;
				Exit(CalcFrontEditWindow);
			end
			else
				front := GetNextWindow(front);
		end;
//		DebugStr('GetFrontEditWindow returns nil');
		CalcFrontEditWindow := nil; // front;
	end;
	
	// Variant på GetFrontEditWindow som bara returnerar huvudprogramfiler!
	function GetFrontMainWindowOLD: WindowPtr;
	var
		front: WindowPtr;
//		title: Str255;
		editIndex: Integer;
	begin
		front := FrontWindow;
		while front <> nil do // (not IsEditWindow(front)) and (front <> nil) do
		begin
			editIndex := GetWRefCon(front); // Eller sök i listan?
			if editIndex > 0 then
			if editIndex <= kMaxEditWindows then
			if editWindIsMain[editIndex] then // Om huvudprogram enligt colorcoding-analysen
//			if IsEditWindow(front) then
// Skip file types that can't be main = shaders or OpenCL kernels
			if not (GetExtensionType(fileSpec[editIndex]) in [kExtTypeGLSL, kExtTypeCL]) then
			begin
				GetFrontMainWindowOLD := front;
				gMainFrontIndex := editIndex; // Sparas för att testa editMainWindIsProgram[editIndex]
							// i första hand för att kolla "library" för FPC-byggen.
				Exit(GetFrontMainWindowOLD);
			end;
			front := GetNextWindow(front);
		end;
		GetFrontMainWindowOLD := nil; // front;
		gMainFrontIndex := -1;
	end;
	
	var
		gFrontEditWindowValid: Boolean = false;
		gFrontMainWindowValid: Boolean = false;
		gFrontEditWindow: WindowPtr = nil;
		gFrontMainWindow: WindowPtr = nil;
		
	function GetFrontEditWindow: WindowPtr;
	begin
// Turned off "smart" front window selection in search of bugs.
//		if not gFrontEditWindowValid then
//		begin
			gFrontEditWindow := CalcFrontEditWindow;
//			gFrontEditWindowValid := true;
//		end;
//		GetFrontEditWindow := gFrontEditWindow;
	end;
	function GetFrontMainWindow: WindowPtr;
	begin
		if not gFrontMainWindowValid then
		begin
			gFrontMainWindow := CalcFrontMainWindow;
			gFrontMainWindowValid := true;
		end;
		GetFrontMainWindow := gFrontMainWindow;
	end;
	
	procedure InvalFrontEditWindow; // Re-check the front edit when requested
	begin
		gFrontEditWindowValid := false;
		gFrontEditWindow := nil;
	end;
	procedure InvalFrontMainWindow; // Re-check the front main when requested
	begin
		gFrontMainWindowValid := false;
		gFrontMainWindow := nil;
	end;

	// Variant på GetFrontEditWindow som bara returnerar huvudprogramfiler!
	// Ny variant 160503 som söker efter huvudprogram som refererar till främsta fönstret!
	// Important change 160621: Always call LoadSettingsForWindow after changing gFrontMainWindow!
	// Is this enough to guarantee a solid behavior for project settings?
	function CalcFrontMainWindow: WindowPtr;
	var
		search: WindowPtr;
//		title: Str255;
		searchIndex, foundIndex, frontIndex, i: Integer;
		frontedit, found: WindowPtr;
		searchFile, frontEditFile: AnsiString;
		frontName: AnsiString;
	begin
//		CalcFrontMainWindow := CalcFrontMainWindowOLD;
//		Exit(CalcFrontMainWindow);
		
//		front := FrontWindow; // front window
		frontedit := GetFrontEditWindow; // front editing window (not necessarily main)
		found := nil;
		if frontedit <> nil then
		begin
			frontIndex := GetWRefCon(frontedit);
			if (frontIndex > 0) and (frontIndex <= kMaxEditWindows) then
			begin
				// Get title of frontedit
				frontEditFile := fileSpec[frontIndex];
				frontName := GetLastToken(frontEditFile);
				// if pascal then
				frontName := TrimExtension(frontName);
				if editWindIsMain[frontIndex] then
				begin // The front edit window is a main file! Stop!
//					WriteLn('CalcFrontMainWindow hits the first which is '+frontEditFile);
					CalcFrontMainWindow := frontedit;
					gMainFrontIndex := frontIndex; // Sparas för att testa editMainWindIsProgram[editIndex]
					// i första hand för att kolla "library" för FPC-byggen.
					LoadSettingsForWindow(gMainFrontIndex); // Ingemar 160621
					Exit(CalcFrontMainWindow);
				end;

				search := frontedit;
				searchIndex := GetWRefCon(search);

				// Now find that referenced by a window that is as frontmost as possible.
				WriteLn('CalcFrontMainWindow candidate is '+frontEditFile);

				while search <> nil do // (not IsEditWindow(front)) and (front <> nil) do
				begin
					searchIndex := GetWRefCon(search); // Eller sök i listan?
					searchFile := fileSpec[searchIndex]; // Get path/name of the current
					if searchIndex > 0 then
					if searchIndex <= kMaxEditWindows then
					if editWindIsMain[searchIndex] then // Om huvudprogram enligt colorcoding-analysen
		//			if IsEditWindow(front) then
		// Skip file types that can't be main = shaders or OpenCL kernels
					if not (GetExtensionType(fileSpec[searchIndex]) in [kExtTypeGLSL, kExtTypeCL]) then
					begin
						// We have a candidate! If this either IS the frontedit or includes frontedit in its dependencies,
//						WriteLn('CalcFrontMainWindow candidate is '+frontEditFile);
						// we will use it!
						if found = nil then
						begin
							found := search; // No matter what, if we don't find any window that works we use the fontmost.
							foundIndex := searchIndex;
						end;
//						gProjectFiles eller usesList?
						for i := 0 to High(usesList[searchIndex]) do
						begin
//							WriteLn(frontName + ' compared to ' + usesList[searchIndex][i]);
							if frontName = TrimExtension(usesList[searchIndex][i]) then // Trim extension in case it is a C/C++ file
							begin
//								WriteLn('*** SAME ***');
								CalcFrontMainWindow := search;
								gMainFrontIndex := searchIndex; // Sparas för att testa editMainWindIsProgram[editIndex]
								// i första hand för att kolla "library" för FPC-byggen.
								LoadSettingsForWindow(gMainFrontIndex); // Ingemar 160621
								Exit(CalcFrontMainWindow);
							end
							else
//								WriteLn('NOT SAME')
								;
						end;
						
//						CalcFrontMainWindow := front;
//						gMainFrontIndex := editIndex; // Sparas för att testa editMainWindIsProgram[editIndex]
//									// i första hand för att kolla "library" för FPC-byggen.
//						Exit(CalcFrontMainWindow);
					end;
					search := GetNextWindow(search);
//					searchIndex := GetWRefCon(search); // Eller sök i listan?
//					searchFile := fileSpec[searchIndex]; // Get path/name of the current
				end;
			end;
		end;		

		// No hit in gProjectFiles but a main window found
		if found <> nil then
		begin
			CalcFrontMainWindow := found;
			gMainFrontIndex := foundIndex; // Sparas för att testa editMainWindIsProgram[editIndex]
			// i första hand för att kolla "library" för FPC-byggen.
			LoadSettingsForWindow(gMainFrontIndex); // Ingemar 160621
			Exit(CalcFrontMainWindow);
		end;
	end;


	// Added 131118
	procedure RotateWindows;
	var
		front: WindowPtr;
		editIndex, frontIndex, theIndex: Longint;
	begin
		WriteLn('RotateWindows');
		front := GetFrontEditWindow;
		if front = nil then
		begin
//			WriteLn('No front');
			Exit(RotateWindows);
		end;
		editIndex := GetWRefCon(front);
		if (editIndex < 1) or (editIndex > kMaxEditWindows) then // Exit(RotateWindows);
		begin
//			WriteLn('Illegal editIndex = ', editIndex);
			Exit(RotateWindows);
		end;
		frontIndex := editIndex;
		repeat
			editIndex += 1;
			if editIndex > kMaxEditWindows then
				editIndex := 1;
			if editWind[editIndex] <> nil then
			begin
				theIndex := GetWRefCon(editWind[editIndex]);
				if (theIndex > 0) and (theIndex <= kMaxEditWindows) then // sanity check
				begin
					BringToFront(editWind[editIndex]);
					SelectWindow(editWind[editIndex]);
					Exit(RotateWindows);
				end
				else
					WriteLn('Sanity check failed for ', editIndex);
			end;
		until frontIndex = editIndex;
	end;
	
	procedure EditSaveAll;
	var
		i: Integer;
		wTitle: Str255;
	begin
		for i := kMaxEditWindows downto 1 do
		if editWind[i] <> nil then
		begin
			dirty[i] := TestIfDirty(i); // Necessary!
			
			if not gSettings.autoSave then
			begin // not auto save
				GetWTitle(editWind[i], wTitle);
				if dirty[i] then // Is dirty reliable?! Only after TestIfDirty.
					if YesNoQuestionAlert('Save "'+ wTitle+'" before compiling?', 'Only saved data can be compiled.') then
						Save(i, false);
			end
			else // auto save
				Save(i, false);
		end;
	end;
	
	// Apple event processing
	procedure EditAEOpen(fs: FSRef; isLastInBatch: Boolean);
//	var
//		editIndex: Integer;
	begin
		{editIndex :=} OpenFileToWindow(FSRefToPathUTF8(fs));
	end;
	
	procedure EditAEPrint(fs: FSRef; isLastInBatch: Boolean);
	var
		editIndex: Integer;
		//err: OSErr;
	begin
		// Temporary solution, just open
		editIndex := OpenFileToWindow(FSRefToPathUTF8(fs));
		// Print
//		{err :=} TXNPrint(teEdit[editIndex]);
		// Close OM fönstret inte redan var öppet!
		
		// To do: HPrint?
	end;
	
	
	function EditGetCurrentTE: HaldaPtr; // Better to use use GetWRefCon(FrontWindow)?
	// No, not if we want the frontmost editing window
	// Should this support OTHER texts, like the console? (Right now solved for the console in SearchEnterSelection.)
	var
		i: Integer;
		front: WindowPtr;
	begin
		EditGetCurrentTE := nil;
		
		front := GetFrontEditWindow;
		if front <> nil then
			for i := 1 to kMaxEditWindows do
				if editWind[i] = front then
					EditGetCurrentTE := teEdit[i];
	end;

(*
Not really the way to go - use GetWRefCon(FrontWindow)
	function EditGetCurrentTEIndex: Longint;
	var
		i: Integer;
		front: WindowPtr;
	begin
// It should be better/faster to use GetWRefCon(FrontWindow);
		EditGetCurrentTEIndex := -1;
		
		front := GetFrontEditWindow;
		if front <> nil then
			for i := 1 to kMaxEditWindows do
				if editWind[i] = front then
					EditGetCurrentTEIndex := i;
	end;
*)

procedure DoFunctionMenu(editIndex: Longint; foundWindow: WindowPtr; where: MacOSAll.Point);
var
	i, goodIndex, iItemCount, menuSelection, item: Longint;
	startOffset, endOffset: Longint;
	editFunctionMenu: MenuRef;
begin
	editFunctionMenu := NewMenu(editIndex, 'FcnMenu');
	for i := 0 to High(editFunctionName[editIndex]) do
//		AppendMenu(editFunctionMenu, editFunctionName[editIndex][i]); // Extra space to separate functions from classes
		MyAppendMenu(editFunctionMenu, editFunctionName[editIndex][i], ' ');

	if editFunctionMenu <> nil then
	begin
		InsertMenu(editFunctionMenu,-1);
		
		// Get current selection to choose initial menu position
		HGetSelection(teEdit[editIndex], startOffset, endOffset);
		iItemCount := CountMenuItems(editFunctionMenu);
		// Find editFunctionNameStart close to these offsets
		goodIndex := 1;
		for i := iItemCount downto 1 do
			if startOffset > editFunctionNameStart[editIndex, i] then
				goodIndex := i;
		
		menuSelection := PopUpMenuSelect(editFunctionMenu, where.v, where.h, goodIndex);
//WriteLn('Did functionmenu with result ', menuSelection);
		if menuSelection > 0 then
		begin
			item := Lo(menuSelection) - 1;
			HSetSelection(teEdit[editIndex], editFunctionNameStart[editIndex, item], editFunctionNameEnd[editIndex, item]+1);
			HShowSelection(teEdit[editIndex]^.views[teEdit[editIndex]^.focusedView], true);
			UpdateLineNumber(foundWindow);
			HInvalViews(teEdit[editIndex]);
		end;
//WriteLn('Functionmenu cleanup');
		
		// Last item should match the current selection! Get selection, search the editFunctionNameStart list and find the best.
		DeleteMenu(editIndex); // ???
		DisposeMenu(editFunctionMenu);
//WriteLn('Functionmenu cleanup done');
	end;
end;

procedure DoUsesMenu(editIndex: Longint; where: MacOSAll.Point);
var
	pathMenu: MenuRef;
	i, menuSelection, item: Longint;
	itemString: Str255;
begin
//	function BuildMenuFromUses(teWind: WindowPtr; teEdit: TXNObject): MenuHandle;
// Varför inte hellre bygga denna vid save? Då kan synlighet hos extraikoner styras.
	pathMenu := BuildMenuFromUses(editWind[editIndex]);
	if pathMenu <> nil then
	begin
		InsertMenu(pathMenu, -1);
		menuSelection := PopUpMenuSelect(pathMenu, where.v, where.h, 0);
		// Weakness: We get ALL "uses", not only the ones that can be opened!
		
//		saveMainFile := gLastMainFile; {We want to restore it or error window will break}
//		gLastMainFile := fileSpec[editIndex]; {Set "home directory" for FindOrOpen}
		
		// Try to open the file
		item := Lo(menuSelection);
		if item >= 1 then // Fixed 071015, was >1 
		begin
			GetMenuItemText(pathMenu, item, itemString);
			
//			GetRootSpec(theRootSpec);
// What is best, GetRootSpec or fileSpec[editIndex]?
			
			i := FindOrOpen(itemString, fileSpec[editIndex]);
			if i > 0 then
				SelectWindow(editWind[i]); // FÖRSÖK: Visa fönstret om det redan fanns!
			{if i = 0: Try other extensions?}
		end;
		
//		gLastMainFile := saveMainFile; {Restore for standard use of FindOrOpen}
		
		DeleteMenu(kPathMenuID); // kPathMenuID eller nåt sånt
		// 1000 MYCKET olämpligt - det använde jag ju till applemenyn förut!
		// Men varför har detta alls fungerat?
		DisposeMenu(pathMenu);
	end;
end;


// Local Code Completion
procedure BuildCCMenu (editIndex: Longint; pathMenuID: Integer);
var
	ccMenu: MenuHandle;
	arr: AnsiStringArray;
	fromPos, toPos, i, menuSelection, item: Longint;
	oStartOffset, oEndOffset: Longint;
	where: MacOSAll.Point;
	itemString: String;
	te: HaldaPtr;
	frontMainEditIndex:LongInt;
	frontMain: WindowPtr;
begin
	te := teEdit[editIndex];
//	WriteLn('Wants to build CC menu');
	// Ask the LocalCodeCompletion module for possible completions
	
	//Get editIndex of frontMain file.
	frontMain := GetFrontMainWindow;
	frontMainEditIndex:= getWRefCon(frontMain);

	arr := FindCompletionsInTE(te, fromPos, toPos, editIndex, frontMainEditIndex);
//	WriteLn('Found ', Length(arr), ' completions for ', fromPos, ',', toPos);
	if Length(arr) = 0 then
		Exit(BuildCCMenu);
	
	// Create a menu with all completions
	ccMenu := NewMenu(pathMenuID, ''); // Vilket ID???
	for i := 0 to High(arr) do
		MyAppendMenu(ccMenu, arr[i], ' ');
//		AppendMenu(ccMenu, arr[i]);
	InsertMenu(ccMenu, -1);
	// We really need "where" to be at the insertion point
	HGetSelection (te, oStartOffset, oEndOffset);
	where := PointToMacPoint(HOffsetToPoint(te^.views[te^.focusedView], oEndOffset));
//	if err <> noErr then WriteLn('TXNOffsetToPoint: ', err);

	SetPortWindowPort(editWind[editIndex]); // Just in case...
	LocalToGlobal(where);
	menuSelection := PopUpMenuSelect(ccMenu, where.v, where.h, 0);
	
	if menuSelection > 0 then
	begin
		item := Lo(menuSelection);
		GetMenuItemText(ccMenu, item, itemString);
//		WriteLn('Selected: ', itemString);
		// Insert!
//		err := TXNSetData(te, kTXNTextData, @itemString[1], Length(itemString), fromPos-1, toPos);
		SetTextUndoable(editIndex, fromPos-0, toPos, itemString);
	end;
	
	DisposeMenu(ccMenu);
//	WriteLn(menuSelection);
end;


// For up-arrow fix in DragClickEventHandler
	var
		systemVersion: record
			major, minor, bugfix: Integer;
		end;
//		savedMax: Longint;


	function EditContentClickEventHandler(inEvent: EventRef; editIndex: Longint; nextHandler: EventHandlerCallRef; foundWindow: WindowRef): Boolean;
	var
		err: OSErr;
		curPort: MacOSAll.GrafPtr;
		where: MacOSAll.Point;
		portRect: MacOSAll.Rect;
		theTime: EventTime;
		theMods: Longint;
		whereHI: HIPoint;
		myEvent: EventRef;
		oStartOffset, oEndOffset, oStartOffsetAfter: Longint;
		row, i: Longint;
	begin
		if editIndex < 1 then
			Exit(EditContentClickEventHandler);		
		
		// First some tedious data extraction
		// (that usually TransSkel does for us):
		err := GetEventParameter (inEvent, kEventParamMouseLocation,
					typeQDPoint, nil, SizeOf(MacOSAll.Point), nil, @where);
		if err <> noErr then WriteLn('GetEventParameter: ', err);
			MacOSAll.GetPort(curPort);
		SetPortWindowPort(foundWindow);
		GlobalToLocal(where);
		
//		WriteLn('Content click'); // Upptäcker alla - även TXN!
		// Men TXN verkar inte jobba genom denna.

		for i := 0 to High(teEdit[editIndex]^.views) do
			if PtInRect(MacPointToPoint(where), teEdit[editIndex]^.views[i]^.viewRect) then
			begin
				theMods := 0; // or poll mods?
				HMouse(teEdit[editIndex]^.views[i], MacPointToPoint(where), theMods);
				return true; // = event was handled
			end;
		
		// Are we in the line number area?
		// OBSOLETE - see MouseNbr
		portRect := GetWindowPortBounds(foundWindow, portRect)^;
if false then
		if where.h < kLineNumberBoxWidth then
			if where.v > portRect.bottom - kLineNumberBoxHeight then
			begin
//				WriteLn('Bappabediba, badapamboom');
				
				HGetSelection (teEdit[editIndex], oStartOffset, oEndOffset);
				row := HOffsetToRow(teEdit[editIndex], oEndOffset);
//				row := UpdateRowFromCharOffset(editIndex, oEndOffset);
//				WriteLn('Trying to show line ', row);
				ShowGoToLineDialog(row);
				
				return true; // = event was handled
			end;
		
		// Are we in the breakpoint area?
		if where.h < kTextBoxLeftMargin then
		begin
//				WriteLn('Mousing around');
			theTime := GetEventTime(inEvent);
			theMods := GetCurrentEventKeyModifiers;
			DoMouse(MacPointToPoint(where), Trunc(theTime*60), theMods);
		end
		else
		if where.h < kTextBoxLeftMarginPlus then
		// We are in the extra margin between the TXN field and breakpoint field.
		// This should be equivalent to click in the TXN field!
		begin
			where.h := kTextBoxLeftMarginPlus+1;
			
			// We send a new mouse down event! kEventClassMouse, kEventMouseDown works!
			// (I have also tried modifying the existing event or sending an kEventClassWindow, kEventWindowClickContentRgn
			// but that doesn't work. TXN works with kEventClassMouse.)
			// The HIPoint seems unnecessary, but I include it just in case.
			
			SetPortWindowPort(editWind[editIndex]); // Just in case...
			LocalToGlobal(where);
			whereHI.x := where.h;
			whereHI.y := where.v;
			err := CreateEvent(nil, kEventClassMouse, kEventMouseDown, GetCurrentEventTime(),
				kEventAttributeUserEvent, myEvent);
			err := SetEventParameter(myEvent, kEventParamMouseLocation, typeQDPoint, sizeof( MacOSAll.Point ), @where);
			err := SetEventParameter( inEvent, kEventParamMouseLocation, typeHIPoint, sizeof( HIPoint ), @whereHI );
			err := PostEventToQueue(GetMainEventQueue(), myEvent, kEventPriorityLow);
			ReleaseEvent(myEvent);

			return true; // Event was handled
		end;
			// Exit?
		MacOSAll.SetPort(curPort);
		
		HGetSelection(teEdit[editIndex], oStartOffset, oEndOffset);
		CallNextEventHandler(nextHandler, inEvent); // Do the click
		HGetSelection(teEdit[editIndex], oStartOffsetAfter, oEndOffset);
		if oStartOffset <> oStartOffsetAfter then
			UpdateLineNumber(foundWindow);
//WriteLn('kEventWindowClickContentRgn ', oStartOffset, ' ', oStartOffsetAfter);
//			return eventNotHandledErr; // Not processed by us!
		return true; // Click already processed
	end;

(*
	function EditDragClickEventHandler(inEvent: EventRef; editIndex: Longint; nextHandler: EventHandlerCallRef; foundWindow: WindowRef): Boolean;
//	procedure EditDragClickEventHandler(editIndex: Longint; foundWindow: WindowRef; where: MacOSAll.Point);
	var
		err: OSErr;
		where: MacOSAll.Point;
		theMods: Longint;
		pathMenu: MenuRef;
		menuSelection, item, i: Longint;
		path: AnsiString;
		itemString: String;
	begin
			//HelpAppendLn('drag click');

//			WriteLn('kEventWindowClickDragRgn!');
			theMods := GetCurrentEventKeyModifiers;
			
			err := GetEventParameter (inEvent, kEventParamMouseLocation, typeQDPoint, nil, SizeOf(MacOSAll.Point), nil, @where);
//			if err <> noErr then WriteLn('GetEventParameter:', err);

						if BitAnd(theMods, optionKey) <> 0 then // option-click in drag bar - function menu
						begin
// 081129: Commented out the update. It takes time, sometimes too much.
							//if gSettings.colorCodeOnMenu then
							//	BetterColorCoder(teEdit[GetWRefCon(foundWindow)], GetWRefCon(foundWindow)); {Dålig felkoll, men det är ett test!}
							DoFunctionMenu(editIndex, foundWindow, where);
						end
						else if BitAnd(theMods, cmdKey) <> 0 then	// command-click in drag bar
						begin
							pathMenu := FSSpecToPathNameMenu(fileSpec[editIndex], kPathMenuID);
							if pathMenu <> nil then
							begin
								InsertMenu(pathMenu, -1);
								menuSelection := PopUpMenuSelect(pathMenu, where.v, where.h, 0);
								
								// Build path from selection and menu items
								// Could also call a path-builder in FileNameUtils.p
								item := Lo(menuSelection);
								if item > 1 then
								begin
//									path := '/Volumes'; // All volumes are under this.
									path := ''; // All volumes are under this.
									for i := CountMenuItems(pathMenu) downto item do // Loop from last item to selected
									begin
										GetMenuItemText(pathMenu, i, itemString);
										path := path + '/' + itemString;
									end;
									
									//ConvertPathToCommandLineFormat(path); // Must do this to support spaces
									
									// Reveal folder in Finder using open CLI command.
									//LaunchSilent('open ' + path);
									ProcessToString('/usr/bin/open "' + path + '"');
//WriteLn('fileSpec debug: ', fileSpec[1]);
								end;
								
								DeleteMenu(kPathMenuID); // kPathMenuID eller nåt sånt
								DisposeMenu(pathMenu);
//WriteLn('fileSpec debug: ', fileSpec[1]);
							end;
						end
//						else if BitAnd(theMods, shiftKey) <> 0 then
//						begin
//						end
						else if BitAnd(theMods , controlKey) <> 0 then
						begin
							DoUsesMenu(editIndex, where);
						end
						else
						return false; {Ask TransSkel to handle dragging}			
			
			return true;
	end;
*)

	function EditKeyEventHandler(inEvent: EventRef; nextHandler: EventHandlerCallRef): Boolean;
	var
		editIndex: Longint;
		oStartOffset, oEndOffset: Longint;
		key: Char;
		err: OSErr;

//		chars: Handle = nil;
		theString: Str255;
//		dataPtr: CharsPtr;
		pos: Longint;
		theMods: Longint;
	begin
		editIndex := GetWRefCon(FrontWindow);
		// Check that editIndex is OK!
		if (editIndex < 1) or (editIndex > kMaxEditWindows) then
			return false;
		
		// What character?
		err := GetEventParameter (inEvent, kEventParamKeyMacCharCodes,
			typeChar, nil, SizeOf(Char), nil, @key);

		// If undo/redo are disabled, we get here. Do not enter the character!
		theMods := GetCurrentEventKeyModifiers;
		if BitAnd(theMods, cmdKey) <> 0 then
			if key in ['a'..'z', 'A'..'Z', '0'..'9'] then // Or only accept cmd-arrow?
			return true; // ignore if undo
		
		if systemVersion.major = 0 then
			GetSystemVersion(systemVersion.major, systemVersion.minor, systemVersion.bugfix);

		// Up-arrow bug in 10.5
		if systemVersion.minor >= 5 then // Leopard
		if ord(key) = 30 then
		begin
			// Not entirely correct!
//			err := DoUpArrowFix(inEvent, teEdit[editIndex], savedMax);
			UpdateLineNumber(FrontWindow);
			return err = noErr;
		end;

		// Draft for code completion (simple local CC so far)
		if ord(key) = 27 then // ESC
		begin
			BuildCCMenu(editIndex, kPathMenuID);
			return true;
		end;

// Specialfall att hantera:
// Alla piltangenter
// Delete
// Fler? Forward delete? Fler?
// Enter normally in TXN
//				WriteLn('Key = ', Ord(key));
// 31 = down, 28 = left, 29 = right, 8 = BS 127 = delete
		if key in [Char(28), Char(29), Char(30), Char(31)] then
		begin
			CallNextEventHandler(nextHandler, inEvent);
			return true;
		end;
		
		theString := key;
		HGetSelection(teEdit[editIndex], oStartOffset, oEndOffset);	
		if key = Char(8) then // Backspace (delete)
		begin
			theString := '';
//					WriteLn(oStartOffset, ',', oEndOffset);
			if oStartOffset = oEndOffset then
				if oStartOffset > 0 then
					oStartOffset -= 1;
		end;
		if key = Char(127) then // Backspace (delete)
		begin
			theString := '';
//					WriteLn(oStartOffset, ',', oEndOffset);
			if oStartOffset = oEndOffset then
				oEndOffset += 1;
		end;
		
		// *** Auto-indent on CR
		if key = Char(13) then // Very simple filter
		begin
			// Optional color coding on newline
// THIS MIGHT STILL BE A GOOD IDEA
// but only from the earliest change!
//			if gSettings.updateColorCoding = kUpdateCCOnNewline then
//			begin
//				editIndex := GetWRefCon(FrontWindow); // Why not theWind? I don't remember.
//				if editIndex > 0 then
//					ResetColorCoder(teEdit[editIndex], editIndex); {Efter öppnande!}
//							BufferedTXNColorCoding(teEdit[editIndex], editIndex); {Efter öppnande!}
//			end;
			
//			teEdit[editIndex]^.h^.text
//			err := TXNGetDataEncoded(teEdit[editIndex], kTXNStartOffset, kTXNEndOffset, Handle(chars), kTXNTextData);
//			if err <> noErr then WriteLn('TXNGetDataEncoded: ', err);
//			HLock(chars);
			// Find two CR back, get indent, insert copy
//			dataPtr := CharsPtr(chars^);
			pos := oStartOffset - 2;
			while (pos > 1) and (teEdit[editIndex]^.text[pos] <> Char(13)) do
				pos := pos - 1;
			pos := pos + 1;
			//theString := '';
			while teEdit[editIndex]^.text[pos] in [' ', Char(9)] do
			begin
				theString := theString + teEdit[editIndex]^.text[pos]; // Will stop at CR or others
				pos := pos + 1;
			end;
//					err := TXNSetData(teEdit[editIndex], kTXNTextData, @theString[1], Length(theString), kTXNUseCurrentSelection, kTXNUseCurrentSelection);
		end;
// Put key (or more) over selection
//		err := TXNSetData(teEdit[editIndex], kTXNTextData, @theString[1], Length(theString), oStartOffset, oEndOffset);
		SetTextUndoable(editIndex, oStartOffset, oEndOffset, theString);

//WriteLn('kEventClassKeyboard');
		UpdateLineNumber(FrontWindow);
		SynchUndoMenu;
		return true; // Whatever was printed is already entered
		
		// INDENT HAS BUGS! OLD AND MAYBE NEW!
	end;
	
	
	
procedure PrintEvent(inEvent: EventRef);
var
	eventClass: UInt32;
	eventKind: UInt32;
begin
	eventClass := GetEventClass(inEvent);
	eventKind  := GetEventKind(inEvent);
	
	case eventClass of
		kEventClassMouse: Write('kEventClassMouse, ');
		kEventClassKeyboard: Write('kEventClassKeyboard, ');
		kEventClassTextInput: Write('kEventClassTextInput, ');
		kEventClassApplication: Write('kEventClassApplication, ');
		kEventClassAppleEvent: Write('kEventClassAppleEvent, ');
		kEventClassMenu: Write('kEventClassMenu, ');
		kEventClassWindow: Write('kEventClassWindow, ');
		kEventClassControl: Write('kEventClassControl, ');
		kEventClassCommand: Write('kEventClassCommand, ');
		kEventClassTablet: Write('kEventClassTablet, ');
		kEventClassVolume: Write('kEventClassVolume, ');
		kEventClassAppearance: Write('kEventClassAppearance, ');
		kEventClassService: Write('kEventClassService, ');
		kEventClassToolbar: Write('kEventClassToolbar, ');
		kEventClassToolbarItem: Write('kEventClassToolbarItem, ');
		kEventClassToolbarItemView: Write('kEventClassToolbarItemView, ');
		kEventClassAccessibility: Write('kEventClassAccessibility, ');
		kEventClassSystem: Write('kEventClassSystem, ');
		kEventClassInk: Write('kEventClassInk, ');
		kEventClassTSMDocumentAccess: Write('kEventClassTSMDocumentAccess, ');
		otherwise Write('Unknown event class ', eventClass, ', ');
	end;
	
	if eventClass = kEventClassMouse then
	case eventKind of
		kEventMouseDown: WriteLn('kEventMouseDown');
		kEventMouseUp: WriteLn('kEventMouseUp');
		kEventMouseMoved: WriteLn('kEventMouseMoved');
		kEventMouseDragged: WriteLn('kEventMouseDragged');
		kEventMouseEntered: WriteLn('kEventMouseEntered');
		kEventMouseExited: WriteLn('kEventMouseExited');
		kEventMouseWheelMoved: WriteLn('kEventMouseWheelMoved');
//		kEventMouseButtonPrimary: WriteLn('kEventMouseButtonPrimary');
//		kEventMouseButtonSecondary: WriteLn('kEventMouseButtonSecondary');
		kEventMouseButtonTertiary: WriteLn('kEventMouseButtonTertiary');
		kEventMouseWheelAxisX: WriteLn('kEventMouseWheelAxisX');
//		kEventMouseWheelAxisY: WriteLn('kEventMouseWheelAxisY');
		otherwise WriteLn('Unknown mouse event kind ', eventClass);
	end
	else    
	if eventClass = kEventClassWindow then
	case eventKind of
		kEventWindowUpdate: WriteLn('kEventWindowUpdate');
		kEventWindowDrawContent: WriteLn('kEventWindowDrawContent');
		kEventWindowGetClickActivation: WriteLn('kEventWindowGetClickActivation');
		kEventWindowActivated: WriteLn('kEventWindowActivated');
		kEventWindowDeactivated: WriteLn('kEventWindowDeactivated');
		kEventWindowShown: WriteLn('kEventWindowShown');
		kEventWindowHidden: WriteLn('kEventWindowHidden');
		kEventWindowBoundsChanging: WriteLn('kEventWindowBoundsChanging');
		kEventWindowBoundsChanged: WriteLn('kEventWindowBoundsChanged');
		kEventWindowResizeStarted: WriteLn('kEventWindowResizeStarted');
		kEventWindowResizeCompleted: WriteLn('kEventWindowResizeCompleted');
		kEventWindowDragStarted: WriteLn('kEventWindowDragStarted');
		kEventWindowDragCompleted: WriteLn('kEventWindowDragCompleted');
		
		kEventWindowHandleActivate: WriteLn('kEventWindowHandleActivate');
		kEventWindowHandleDeactivate: WriteLn('kEventWindowHandleDeactivate');

		kEventWindowClickDragRgn: WriteLn('kEventWindowClickDragRgn');
		kEventWindowClickResizeRgn: WriteLn('kEventWindowClickResizeRgn');
		kEventWindowClickCollapseRgn: WriteLn('kEventWindowClickCollapseRgn');
		kEventWindowClickCloseRgn: WriteLn('kEventWindowClickCloseRgn');
		kEventWindowClickZoomRgn: WriteLn('kEventWindowClickZoomRgn');
		kEventWindowClickContentRgn: WriteLn('kEventWindowClickContentRgn');
		kEventWindowClickProxyIconRgn: WriteLn('kEventWindowClickProxyIconRgn');
		kEventWindowClickToolbarButtonRgn: WriteLn('kEventWindowClickToolbarButtonRgn');
		kEventWindowClickStructureRgn: WriteLn('kEventWindowClickStructureRgn');
		kEventWindowExpand: WriteLn('kEventWindowExpand');
		kEventWindowClose: WriteLn('kEventWindowClose');
		kEventWindowHandleContentClick: WriteLn('kEventWindowHandleContentClick');
		kEventWindowSheetOpening: WriteLn('kEventWindowSheetOpening');
		kEventWindowSheetOpened: WriteLn('kEventWindowSheetOpened');
		kEventWindowSheetClosing: WriteLn('kEventWindowSheetClosing');
		kEventWindowSheetClosed: WriteLn('kEventWindowSheetClosed');
		kEventWindowDrawerOpening: WriteLn('kEventWindowDrawerOpening');
		kEventWindowDrawerOpened: WriteLn('kEventWindowDrawerOpened');
		kEventWindowDrawerClosing: WriteLn('kEventWindowDrawerClosing');
		kEventWindowDrawerClosed: WriteLn('kEventWindowDrawerClosed');
		kEventWindowDrawGrowBox: WriteLn('kEventWindowDrawGrowBox');
		kEventWindowGetGrowImageRegion: WriteLn('kEventWindowGetGrowImageRegion');
		kEventWindowPaint: WriteLn('kEventWindowPaint');
		otherwise WriteLn('Unknown event kind ', eventKind);
	end
	else
	if eventClass = kEventClassKeyboard then
	case eventKind of
		kEventRawKeyDown: WriteLn('kEventRawKeyDown');
		kEventRawKeyRepeat: WriteLn('kEventRawKeyRepeat');
		kEventRawKeyUp: WriteLn('kEventRawKeyUp');
		kEventRawKeyModifiersChanged: WriteLn('kEventRawKeyModifiersChanged');
		kEventHotKeyPressed: WriteLn('kEventHotKeyPressed');
		kEventHotKeyReleased: WriteLn('kEventHotKeyReleased');
		otherwise WriteLn('Unknown keyboard event kind ', eventClass);
	end
	else
	case eventKind of
		kEventAppActivated: WriteLn('kEventAppActivated');
		kEventAppDeactivated: WriteLn('kEventAppDeactivated');
		kEventAppQuit: WriteLn('kEventAppQuit');
		kEventAppHidden: WriteLn('kEventAppHidden');
		kEventAppShown: WriteLn('kEventAppShown');
		otherwise WriteLn('Unknown event kind ', eventClass);
	end

end;

	

// Was DragClickEventHandler
function EditWindowEventHandler (nextHandler: EventHandlerCallRef; inEvent: EventRef;
						userData: Pointer): OSStatus; MWPascal;
var
	eventClass: UInt32;
	eventKind: UInt32;
	foundWindow: WindowRef;
	editIndex{, f}: Longint;
	err: OSErr;
//	wheelDelta: Longint;
//	hView: HaldaViewPtr;
//	max, value: Longint;
//	axis: EventMouseWheelAxis;
//	scrollbar: HIViewRef;
	where: MacOSAll.Point;
	i: Longint;
	c: CursHandle;
	mods: UInt32;
begin
	EditWindowEventHandler := eventNotHandledErr;
//		WriteLn('Custom proc edit wind?');
	eventClass := GetEventClass(inEvent);
	eventKind  := GetEventKind(inEvent);
//		PrintEvent(inEvent);
	
//		foundWindow := nil;
	foundWindow := FrontWindow;
	err := GetEventParameter (inEvent, kEventParamDirectObject,
		typeWindowRef, nil, SizeOf(MacOSAll.WindowPtr), nil, @foundWindow);

(*		if err = noErr then
		if foundWindow <> FrontWindow then
		begin
			return eventNotHandledErr;
		end
		else
			foundWindow := FrontWindow
	else
		foundWindow := FrontWindow;*)

	if foundWindow = nil then
		foundWindow := FrontWindow;
	if foundWindow = nil then
		return eventNotHandledErr;

	SetPortWindowPort(foundWindow); // Added 150331, bug hunt!
	editIndex := GetWRefCon(foundWindow);
	
	if eventClass = kEventClassMouse then
	begin
		// Upon exit, set to arrow!
		if ((eventClass = kEventClassMouse) and (eventKind = kEventMouseExited)) then
			InitCursor;
		// Adjust cursor!
		if ((eventClass = kEventClassMouse) and (eventKind = kEventMouseMoved)) then
		begin
			err := GetEventParameter (inEvent, kEventParamMouseLocation,
				typeQDPoint, nil, SizeOf(MacOSAll.Point), nil, @where);
			MacOSAll.GlobalToLocal(where);
			// If in editing area
			editIndex := GetWRefCon(foundWindow);
			if editIndex > 0 then
			for i := 0 to High(teEdit[editIndex]^.views) do
			begin
				if QDCG.PtInRect(MacPointToPoint(where), teEdit[editIndex]^.views[i]^.viewRect) then
				begin
					c := GetCursor(iBeamCursor);
					SetCursor(c^^);
					return noErr;
				end;
			InitCursor; // (arrowCursor);
			end;
			// Inside 
		end;
	
		// 140625: Mouse wheel!
//		if ((eventClass = kEventClassMouse) and (eventKind = kEventMouseWheelMoved)) then
//		begin
//			if GetEventParameter(inEvent, kEventParamMouseWheelDelta, typeLongInteger,
//								nil, sizeof(Longint), nil, @wheelDelta ) = noErr then
//			begin
//				// Scroll by wheelDelta!
//				err := GetEventParameter (inEvent, kEventParamMouseLocation,
//					typeQDPoint, nil, SizeOf(MacOSAll.Point), nil, @where);
//				{partId :=} FindWindow(where, foundWindow);
//				if foundWindow <> nil then
//					editIndex := GetWindowNumber(foundWindow)
//				else
//					foundWindow := GetFrontEditWindow;
//				// Could also be console or errors!
//				
//				if editIndex > 0 then
//				begin
//					f := teEdit[editIndex]^.focusedView;
//					if (f >= 0) and (f <= High(teEdit[editIndex]^.views)) then
//					begin
//						hView := teEdit[editIndex]^.views[f];
//						err := GetEventParameter(inEvent, kEventParamMouseWheelAxis, typeMouseWheelAxis,
//									nil, sizeof(EventMouseWheelAxis), nil, @axis);
//						if axis = kEventMouseWheelAxisY then
//							scrollbar := hView^.vScroll
//						else
//							scrollbar := hView^.hScroll;
//						if scrollbar <> nil then
//						begin
//							value := VMGetNumValue(scrollbar) - wheelDelta;
//							max := GetControl32BitMaximum(scrollbar);
//							WriteLn('value = ', value, ' max = ', max);
//							if value < max then
//								VMSetNumValue(scrollbar, value);
//							// ALMOST good but not quite???
//							// Should be <= for horizontal. Why? <= works for console!
//						end;
//						
//					end else WriteLn('No focused view ', teEdit[editIndex]^.focusedView);
//				end else WriteLn('No edit window');

//			end
//		end
//		else
//			return eventNotHandledErr;
	end; // mouse


	{ Click in dragbar and some more places}
	if ((eventClass = kEventClassWindow) and (eventKind = kEventWindowClickContentRgn)) then
	begin
//			if EditContentClickEventHandler(inEvent, editIndex, nextHandler, foundWindow) then
//				return noErr
//			else
			return eventNotHandledErr;
	end
	else
	{ Ignored for composited windows! Evil Apple! }
	if ((eventClass = kEventClassWindow) and (eventKind = kEventWindowClickDragRgn)) then
	begin
//			if EditDragClickEventHandler(inEvent, editIndex, nextHandler, foundWindow) then
//				return noErr
//			else
			return eventNotHandledErr;
	end
	else
	if ((eventClass = kEventClassKeyboard) and (eventKind = kEventRawKeyModifiersChanged)) then
	begin
		// Show or hide the name view!
//		WriteLn('kEventRawKeyModifiersChanged');
		err := GetEventParameter (inEvent, kEventParamKeyModifiers,
			typeUInt32, nil, SizeOf(UInt32), nil, @mods);
//			SetControlVisibility(nameView[editIndex], mods = 0, mods = 0); did not work
		gNameViewVisible := mods <> 0;
		for i := 1 to kMaxEditWindows do
			if editWind[i] <> nil then
				UpdateWindowViews(i);
//			DrawControls(editWind[editIndex]);
		
		return eventNotHandledErr;
	end
	else
	{ Keydowns, do auto-indent etc }
	if eventClass = kEventClassKeyboard then
		if (eventKind = kEventRawKeyDown) or (eventKind = kEventRawKeyRepeat) then
		begin
//				if EditKeyEventHandler(inEvent, nextHandler) then
//					return noErr
//				else
				return eventNotHandledErr;
		end;

	return eventNotHandledErr;
end; {EditWindowEventHandler}
	


var
	gFingerFileName: Str255;
	gFingerLineNumber: Longint;


// Flytta till Halda?
function GetPointFromRow(view: HaldaViewPtr; row: Longint): Point;
var
	p: Point;
	h, v: Real;
begin
	if Length(view^.h^.rowStarts) < 1 then
		Exit(GetPointFromRow);
	if row < 0 then
		row := 0;
	if row > High(view^.h^.rowStarts) then
		row := High(view^.h^.rowStarts);
	v := view^.h^.rowHeight * (row - view^.topRow) + view^.viewRect.top - view^.h^.rowHeight / 2; // + backa en halv rad!
	h := 0; // Or by the scroll
	if v < -32000 then v := -32000;
	if v > 32000 then v := 32000;
	p.h := h;
	p.v := v;
	GetPointFromRow := p;
end;

function GetRowFromPoint(view: HaldaViewPtr; p: Point): Longint;
var
	row: Real;
begin
	row := (p.v - view^.viewRect.top) / view^.h^.rowHeight + view^.topRow;
	if row < 0 then
		row := 0;
	if row > High(view^.h^.rowStarts) then
		row := High(view^.h^.rowStarts);
	GetRowFromPoint := Trunc(row);
end;


// Borde göra detta som två saker:
// Callback per rad, med info om radposition (radnummer, breakpoints)
// Callback per total omritning (radnummerrutan, annan övergripande info)
// Callback för ändrad selection
// CalcRects? Callback i gamla TXN? BOrde den vara med här?

procedure DrawPrivateRow(view: HaldaViewPtr; row, position, rowHeight: Longint);
begin
end;

procedure DrawPrivate(view: HaldaViewPtr);
// Any "private" drawing, nonTXN, line number, breakpoints
//procedure DrawPrivate(myTXNWindowDataPtr: TXNWindowDataPtr);
var
//	portRect: Rect;
	//CaretRect:Rect;
//	pos: Longint;
	//pt,
	CaretPoint: Point;
	line, i: Longint;
//	theString: Str255;
//	ch: Char;
//	chars: CharsHandle;
	err: OSErr;
	savePort: MacOSAll.GrafPtr;
	
	oStartOffset, oEndOffset: Longint;
	
	// Breakpoints:
	br: BreakpointPtr;
	p: Point;
	r: QDCG.Rect;
	qdr: MacOSAll.Rect;
	areacol, linecol: RGBColor;
	theSpec: FSSpecString;
	
	editIndex, level, stopAt: Longint;
//	selStart, selEnd: Longint;
	pt: HaldaPoint;
	theChar, findChar, c: Char;
const
	LF = Char(10);
	CR = Char(13);
begin
	MacOSAll.GetPort(savePort);
	if view^.window <> nil then
		SetPortWindowPort(view^.window);
	if view^.hiview <> nil then
		SetPortWindowPort(GetControlOwner(view^.hiview));
//	chars := CustomTXNGetData(theWind);
//	qdcgPort := CreatePortWindowPort(view^.window); NO - called from a main DoUpdate which does this

	areacol.red := $E800;
	areacol.green := $E800;
	areacol.blue := $F000;
	linecol.red := $C000;
	linecol.green := $C000;
	linecol.blue := $C000;

// Paint areas outside the edit rect, gray is nice
	qdr := MacOSAll.GetPortBounds(GetWindowPort(GetControlOwner(view^.hiview)), qdr)^;
	r := MacRectToRect(qdr);
	r.right := kTextBoxLeftMargin;
	RGBForeColor(areacol);
	PaintRect(r);
	RGBForeColor(linecol);
	MoveTo(kTextBoxLeftMargin-1, 0);
	LineTo(kTextBoxLeftMargin-1, r.bottom);
	ForeColor(blackColor);
	
//WriteLn('line numbers ', Ord(myTXNWindowDataPtr));

// Draw break points!
// What is the window index? MUST BE CORRECTED! Get from myTXNWindowDataPtr^.editWind!
	br := GetWindowBreakpoints(GetControlOwner(view^.hiview));
//	if br = nil then
//		WriteLn('No breakpoint');

	while br <> nil do
	begin
//		WriteLn('Drawing breakpoint ', br^.line);
// SLOW
		p := GetPointFromRow(view, br^.line + 0 ); // ??
		SetRect(r, kTextBoxLeftMargin div 2 - 3, p.v-3, kTextBoxLeftMargin div 2 + 3, p.v+3);
		ForeColor(blackColor);
		if not br^.enabled then
			ForeColor(cyanColor);
		PaintRect(r);
		br := br^.next;
	end;
	
	if BugsRunning then
	begin
		err := GetEditFSSpec(GetControlOwner(view^.hiview), theSpec);
		if err = noErr then
// PROBLEM!
// 1) Borde göra det caseokänsligt!
// 2) gFingerFileName är path, theSpec.name är bara filnamn
// Var GetLastToken. Borde man snarare jämföra hela strängarna? 
		if gFingerFileName = theSpec then // lowercase both???
		begin
//			WriteLn('Drawing finger in ', gFingerFileName);
			p := GetPointFromRow(view, gFingerLineNumber + 0 ); // Borde vara -1??
			MoveTo(0, p.v);
			LineTo(kTextBoxLeftMargin-1, p.v);
			LineTo(kTextBoxLeftMargin-1-4, p.v-4);
			MoveTo(kTextBoxLeftMargin-1, p.v);
			LineTo(kTextBoxLeftMargin-1-4, p.v+4);
		end;
//		else
//			WriteLn(gFingerFileName, ' is not ', theSpec);
	end;

//WriteLn('DrawPrivate update special');

(*
// Update special fields
//	SetPortWindowPort(view^.window);
	qdr := GetWindowPortBounds(GetControlOwner(view^.hiview), qdr)^;		{ paint window dark gray }
	portRect := MacRectToRect(qdr);
	portRect.right := portRect.left + kLineNumberBoxWidth; // 50;
	portRect.top := portRect.bottom - kLineNumberBoxHeight;
	portRect.bottom := portRect.bottom + 1;
	RGBForeColor(areacol);
//	EraseRect(portRect);
	PaintRect(portRect);
	RGBForeColor(linecol);
	FrameRect(portRect);
	ForeColor(blackColor);

// Line number	
//	err := TXNGetDataEncoded(myTXNWindowDataPtr^.txnObj, kTXNStartOffset, kTXNEndOffset, Handle(chars), kTXNTextData);
//	if err <> noErr then WriteLn('TXNGetDataEncoded: ', err);
//	HLock(Handle(chars));
	
	pos := 0;
	line := 0;
	ch := Char(10);
	
//	WriteLn(GetHandleSize(Handle(chars)));
	
	MacOSAll.TextSize(9);
	MacOSAll.TextFont(3);
	
// Start at selection
// Count number of CR to start!
	
	HGetSelection (view^.h, oStartOffset, oEndOffset);
	editIndex := GetWRefCon(GetControlOwner(view^.hiview));
	if (editIndex > 0) and (editIndex < kMaxEditWindows) then
//		line := UpdateRowFromCharOffset(editIndex, oStartOffset);
		line := HOffsetToRow(view^.h, oStartOffset);
	
	MoveTo(3, portRect.bottom-5);
	Str(line+1, theString); // +1 since Halda is 0-based
	DrawString(theString);
	//WriteLn('Line ', theString);
*)

	//If the BackgRound is dark creates a caret color reverse 
//N.B is not as "CaretAction." This only serves to follow the movement of accelerating arrows!
//D.S see also Line 1399 
  // if (gSettings.textSaveColorCoding<>LWP_ColorCoding) then
  if (TestBackColorForCaret(ColorList[LWPSintax.VBackgroundColor])=True) then
     begin
       RGBForeColor(ColorList[LWPSintax.VForeColor]);
      caretPoint := HOffsetToPoint(view, oStartOffSet);
      MoveTo(caretPoint.h,CaretPoint.v);
      LineTo(CaretPoint.h,CaretPoint.v+GetSettingsTextSize);
     end;
     // This is probably in the wrong place now. Should be in Halda. /IR

	// PARENTHESIS BALANCING MARKER:
	// if the selection is empty
	// if the last character before the selection is a )]} (maybe even "end")
	// scan backwards for ([{ (but not beyond the top of the screen)
	// Find location on screen
	// Draw something on the ([{
	HGetSelection(view^.h, oStartOffset, oEndOffset);
	// if oStartOffset = oEndOffset then
	
	// Only search to top of window!
	if view^.topRow < High(view^.h^.rowStarts) then
		stopAt := view^.h^.rowStarts[view^.topRow]
	else
		stopAt := oStartOffset - 5000;
	
	if oStartOffset-1 > 1 then
	if oStartOffset-1 <= Length(view^.h^.text) then
	begin
		theChar := view^.h^.text[oStartOffset-1];
		if theChar in [')', '}', ']'] then
		begin
			if theChar = ')' then
				findChar := '(';
			if theChar = '}' then
				findChar := '{';
			if theChar = ']' then
				findChar := '[';
			i := oStartOffset-1;
			c := 'X';
			level := 1;
			repeat
				i := i - 1;
				if i > 0 then
					c := view^.h^.text[i]
				else
					c := 'X';
				if c = theChar then
					level += 1;
				if c = findChar then
					level -= 1;
			until ((c = findChar) and (level = 0)) or (i < 0) or (i < stopAt);
			if c = findChar then
			begin
				pt := HOffsetToPoint (view, i);
				SetRect(r, pt.h, pt.v, pt.h + view^.h^.charWidth, pt.v + view^.h^.rowHeight);
				ForeColor(greenColor);
				PenMode(srcOr);
				PaintRect(r);
				PenMode(srcCopy);
				QDCG.Line(10.0, 10.0);
			end;
		end;
	end;
	
//	FinishPort;
	MacOSAll.SetPort(savePort);
end;



// Should not use Str255
// Almost same as InvalBreakpoints
procedure InvalSidebar(fileName: AnsiString);
var
	i: Longint;
	bounds: MacOSAll.Rect;
begin
	fileName := GetLastToken(fileName);
	// Search window list!
	for i := 1 to kMaxEditWindows do
		if teEdit[i] <> nil then // What is the CORRCT way to tell if a window is active?
		if editWind[i] <> nil then // What is the CORRCT way to tell if a window is active?
		begin
			GetWindowPortBounds(editWind[i], bounds);
			bounds.right := bounds.left + kTextBoxLeftMargin;
			InvalWindowRect(editWind[i], bounds);
		end;
end;

// Should not be Str255!
procedure UpdateFinger(fileName: AnsiString; lineNumber: Longint);
var
	editIndex: Longint;
begin
//	WriteLn('UpdateFinger ', fileName, ' ', lineNumber);

// Måste uppdatera den som HADE fingret, om det inte är samma!
	if fileName <> gFingerFileName then
	begin
		// Hur?
		gFingerLineNumber := 0; // -1?
		InvalSidebar(gFingerFileName);
	end;
	
// Registrera finger så den ritas! Men hur?
	gFingerFileName := fileName;
	gFingerLineNumber := lineNumber;
	
//	err := HGetSelection(teEdit[editIndex], start, ending); //i + columnNumber, i + columnNumber);
//	WriteLn('Tries to update finger to ', UTF8ToMacRoman(fileName));
	if lineNumber > 0 then
	begin
		editIndex := EditOpenAndSelect({UTF8ToMacRoman}(fileName), lineNumber, 1);
	// OBS! DETTA GÅR SNETT MED SPECIALTECKEN I PATH!
//	WriteLn('Updated finger to ', UTF8ToMacRoman(fileName));

		if editIndex >= 0 then
			RecordFingerPosition(editIndex, lineNumber);
	end;
	
//	err := TXNSetSelection(teEdit[editIndex], start, ending); //i + columnNumber, i + columnNumber);

// Hur tvinga uppdatering då?
	InvalSidebar(fileName);
	OKtoSetBreaks :=true;

end;

//var
//	CurrentTXNWind: txnObject;
procedure UpdateLineNumber(editWind: WindowPtr);
var
	savePort: CGrafPtr;
	bounds: MacOSAll.Rect;
	view: HaldaViewPtr;
	editIndex: Longint;
	control: ControlRef;
begin

(*
	GetPort(savePort);
	SetPortWindowPort(editWind);

	editIndex := GetWRefCon(editWind);
	if editIndex > 0 then
	begin
//		view := teEdit[editIndex]^.views[0];
		view := teEdit[editIndex]^.views[teEdit[editIndex]^.focusedView];
	end
	else
		Exit(UpdateLineNumber);

// Ask for the line number to be redrawn
	MacOSAll.GetWindowPortBounds(view^.window, bounds);
	bounds.right := bounds.left + kLineNumberBoxWidth;
	bounds.top := bounds.bottom - 15;
	InvalWindowRect(editWind, bounds);
//WriteLn('UpdateLineNumber did InvalWindowRect');

// Ask for the breakpoint area to be redrawn
	GetWindowPortBounds(view^.window, bounds);
	bounds.right := bounds.left + kTextBoxLeftMargin;
	InvalWindowRect(editWind, bounds);
*)

// NEW: Replaces above! Update views!
	control := VMGetControl(editWind, 'LNBR', 0);
	HIViewSetNeedsDisplay(control, true);
	control := SkelGetContentView(editWind);	
	HIViewSetNeedsDisplay(control, true);
	
end;

// Obsolete?
procedure CalcRects(editWind: WindowRef; var textBounds, hScrollBounds, vScrollBounds: MacOSAll.Rect);
begin
	MacOSAll.GetPortBounds(GetWindowPort(editWind), textBounds);
	OffsetRect(textBounds, -textBounds.left, -textBounds.top);
	hScrollBounds := textBounds;
	vScrollBounds := textBounds;
	textBounds.left := textBounds.left + kTextBoxLeftMarginPlus;
	textBounds.right := textBounds.right - 15;
	textBounds.bottom := textBounds.bottom - 15;
	hScrollBounds.top := hScrollBounds.bottom - 15;
	vScrollBounds.left := hScrollBounds.right - 15;
	vScrollBounds.bottom := vScrollBounds.bottom - 15;
	hScrollBounds.right := hScrollBounds.right - 15;
	hScrollBounds.left := hScrollBounds.left + kLineNumberBoxWidth; // För radnummer
end;

// Some variables for helping the text editing (when not using extended mouse events).
var
//	count: array [1..kMaxEditWindows] of Longint;
	pressed: array [1..kMaxEditWindows] of Boolean;
	savedp: Point;

// Set or reset breakpoints
procedure DoMouse(thePoint: Point; theTime: UInt32; theMods: Integer);
var
//	oOffset: Longint;
	r: MacOSAll.Rect;
	row: Longint;
//	p: Point;
	textBounds, hScrollBounds, vScrollBounds: MacOSAll.Rect;
	w: WindowPtr;
	editIndex, i: Longint;
begin
//	WriteLn('DoMouse');
	w := GetWindowFromPort(GetQDGlobalsThePort);
	editIndex := GetWRefCon(w);

	for i := 0 to High(teEdit[editIndex]^.views) do
	begin
		if PtInRect(thePoint, teEdit[editIndex]^.views[i]^.viewRect) then
		begin
			HMouse(teEdit[editIndex]^.views[i], thePoint, theMods);
			teEdit[editIndex]^.focusedView := i;
			pressed[editIndex] := true;
			Exit(DoMouse)
		end;

			// Otherwise breakpoints
			// Must test for height of viewRect!
		if OKtoSetBreaks then
			if thePoint.v > teEdit[editIndex]^.views[i]^.viewRect.top then
			if thePoint.v < teEdit[editIndex]^.views[i]^.viewRect.bottom then
		begin
			row := GetRowFromPoint(teEdit[editIndex]^.views[i], thePoint); // Hellre HPointToOffset + HOffsetToRow?
				ToggleBreakpoint(w, row+1, theMods) // 0-based row in Halda
		end
		else
			SysBeep(1);	//  if the program is running and is in debug mode , setting breakpoints will hang the IDE
	end;
//	WriteLn('UPDATED row = ', row);
	
	// InvalRect! Entire column or locally
	CalcRects(w, textBounds, hScrollBounds, vScrollBounds);
//	SetRect(r, p.h-30, p.v-30, p.h+30, p.v+30); Räcker om det bara togglar lokalt
	SetRect(r, 0, 0, textBounds.left, textBounds.bottom);
//	EraseRect(r);

//	InvalWindowRect(w, r); // Irrelevant now, use HIViewSetNeedsDisplay
	HIViewSetNeedsDisplay(SkelGetContentView(w), true);

//	UpdateLineNumber(editWind[editIndex]);
end;

(*

// Added by Sabino D'Elia. Moved from CustomTXNUnit and modified by Ingemar
// This can probably be removed, or at least modified, with Halda.
// Best, integrated in Halda, or a callback directly from it.

var
	CaretoStartOffset:Longint;

procedure CaretAction(theTimer:EventLoopTimerRef; userData:Pointer);MWPascal;
var
	Pt:Point;
	oStartOffset, oEndOffset: Longint;
	front: WindowPtr;
	mytxnObj: HaldaViewPtr;
	savePort: CGrafPtr;
begin
	front := FrontWindow;
	if (front = nil) then
		Exit(CaretAction);
	//if gSettings.textSaveColorCoding = LWP_ColorCoding then
	if  (TestBackColorForCaret(ColorList[LWPSintax.VBackgroundColor])=false) then
		Exit(CaretAction);// No fix needed for standard coding
   //D'S 08/07/25 Caret Action works only on dark background, 
   //through "TestBackColorForCaret" are selected only the darker colours
	
	mytxnObj := CustomTXNGetTE(front);
	if mytxnObj=nil then Exit(CaretAction); 
	
	GetPort(savePort);
	SetPortWindowPort(front);	
	
	HGetSelection (mytxnObj, oStartOffset, oEndOffset);

	if (oStartOffset=oEndOffset) and (CaretoStartOffset<>oStartOffset) then
	begin
		//to delete the fake caret. 
		//we need to move the original caret on the old track the fake caret.
		TXNSetSelection(mytxnObj,CaretoStartOffset,CaretoStartOffset); 
		TXNSetSelection(mytxnObj,oStartOffset,oStartOffset); 
		CaretoStartOffSet:=oStartOffset;
	end
	else
    begin 
		CaretoStartOffSet:=oStartOffset;
		TXNOffsetToPoint(mytxnObj,CaretoStartOffset,Pt);	 
    	// To make this more nice the caret has the same color of the text
		RGBForeColor(ColorList[LWPSintax.VForeColor]);

		MoveTo(pt.h,pt.v);
		LineTo(pt.h,pt.v+GetSettingsTextSize);
	end;
	QDFlushPortBuffer(GetWindowPort(front), nil);
	SetPort(savePort);	  
end;

var
	theCaretTimer: EventLoopTimerRef;
	
procedure InstallCaretTimer;
var
	MainLoop:EventLoopRef;
	timerUpp:EventLoopTimerUPP;
begin
	if theCaretTimer = nil then
	begin // Only install once
	
		mainLoop:=GetMainEventLoop();
		timerUPP:=NewEventLoopTimerUpp(CaretAction);
		InstallEventLoopTimer(mainLoop, 0.01*kEventDurationSecond,0.55*kEventDurationSecond,
			timerUPP,nil, theCaretTimer);
	end;
	
end;

*)


type
	ViewDataRec = record
		view: HIViewRef;
		window: WindowPtr;
		editIndex: Longint;
		icon: IconRef;
	end;
	ViewDataPtr = ^ViewDataRec;

procedure BarWidgetFrame(inWindow: WindowRef; var outBounds, outBounds2: HIRect);
const
	kWidgetSize	= 16;	// width and height of the widget
	kWidgetSpace	= 8;		// space on the left and right of the widget
var
	rTitle, rStructure: MacOSAll.Rect;
//	outBounds: HIRect;
begin
	GetWindowBounds( inWindow, kWindowTitleTextRgn, rTitle );
	GetWindowBounds( inWindow, kWindowStructureRgn, rStructure );
	
	outBounds.origin.x := rTitle.right + kWidgetSpace - rStructure.left;
	outBounds.origin.y := rTitle.top - rStructure.top; {Should this center by kWidgetSize?}
	outBounds.size.width := kWidgetSize;
	outBounds.size.height := kWidgetSize;
	
	outBounds2 := outBounds;
	outBounds2.origin.x := outBounds2.origin.x + kWidgetSize + kWidgetSpace;
	
//	return outBounds;
end;

// Set visibility of popup menu controls
	procedure SynchPopups(editIndex: Longint);
	var
		isVisible: Boolean;
	begin
		isVisible := Length(editFunctionName[editIndex]) > 0; // CountMenuItems(editFunctionMenu[editIndex]) > 0;
		SetControlVisibility(functionView[editIndex], isVisible, isVisible);
//		if not isVisible then
//			WriteLn('NO FUNCTIONS');
		
		isVisible := Length(usesList[editIndex]) > 0;
		SetControlVisibility(usesView[editIndex], isVisible, isVisible);
	end;

procedure LocalToGlobal(var p: Point; theView: HIViewRef);
var
	r, rr: MacOSAll.Rect;
//	rrr: Rect;
	w: WindowRef;
begin
	w := HIViewGetWindow(theView); // Men det är väl fel?
//	GetControlBounds(theView, r); // = relativt fönster - irrelevant?
	GetWindowBounds(w, kWindowContentRgn, rr);
	// HIViewGetFrame relativt förälder
//	p.h += r.left + rr.left;
//	p.v += r.top + rr.top;
	p.h += rr.left;
	p.v += rr.top;
end;

(*
	procedure DoFunctionViewUpdateQD(theView: HIViewRef; r: MacOSAll.Rect; userData: Pointer);
	var
//		err: OSErr;
//		vd: ViewDataPtr;
		pol: MacOSAll.PolyHandle;
	begin
//		CreatePortQDPort(nil);

//		vd := ViewDataPtr(userData);
		MacOSAll.EraseRect(r);
		MacOSAll.FrameRect(r);
		MacOSAll.MoveTo(1, 9);
		MacOSAll.TextSize(9);
		MacOSAll.DrawString('F');

		pol := MacOSAll.OpenPoly;
		MacOSAll.MoveTo(6, 10);
		MacOSAll.LineTo(10, 14);
		MacOSAll.LineTo(14, 10);
		MacOSAll.ClosePoly;
		MacOSAll.PaintPoly(pol);
		MacOSAll.KillPoly(pol);
		
//		FinishPort;

// Varför påverkas ritandet av vad man ritat innan? Transfer mode?	
//		err := PlotIconRef (r, kAlignNone, kTransformNone, kPlotIconRefNormalFlags, vd^.icon);
	end;
*)

	procedure DoFunctionViewUpdateQDCG(theView: HIViewRef; r: QDCG.Rect; userData: Pointer);
	var
//		err: OSErr;
//		vd: ViewDataPtr;
		pol: QDCG.PolyHandle;
	begin
//		CreatePortQDPort(nil);

//		vd := ViewDataPtr(userData);
		BackColor(whiteColor);
		QDCG.EraseRect(r);
		QDCG.FrameRect(r);
		QDCG.MoveTo(1, 9);
		QDCG.TextSize(9);
		QDCG.DrawString('F');

		pol := QDCG.OpenPoly;
		QDCG.MoveTo(6, 10);
		QDCG.LineTo(10, 14);
		QDCG.LineTo(14, 10);
		QDCG.ClosePoly;
		QDCG.PaintPoly(pol);
		QDCG.KillPoly(pol);
		
//		FinishPort;

// Varför påverkas ritandet av vad man ritat innan? Transfer mode?	
//		err := PlotIconRef (r, kAlignNone, kTransformNone, kPlotIconRefNormalFlags, vd^.icon);
	end;

	procedure DoUsesViewUpdateQDCG(theView: HIViewRef; r: QDCG.Rect; userData: Pointer);
	var
//		err: OSErr;
		vd: ViewDataPtr;
		pol: QDCG.PolyHandle;
	begin
		vd := ViewDataPtr(userData);
		BackColor(whiteColor);
		QDCG.EraseRect(r);
		QDCG.FrameRect(r);
		QDCG.MoveTo(1, 9);
		QDCG.TextSize(9);
		QDCG.DrawString('U');
		
		pol := QDCG.OpenPoly;
		QDCG.MoveTo(6, 10);
		QDCG.LineTo(10, 14);
		QDCG.LineTo(14, 10);
		QDCG.ClosePoly;
		QDCG.PaintPoly(pol);
		QDCG.KillPoly(pol);
		
// Varför påverkas ritandet av vad man ritat innan? Transfer mode?	
//		err := PlotIconRef (r, kAlignNone, kTransformNone, kPlotIconRefNormalFlags, vd^.icon);
	end;

(*
	procedure DoMouseQDFunction (theView: HIViewRef; where: MacOSAll.Point; mods: Longint; userData: Pointer);
	var
		vd: ViewDataPtr;
		b1, b2: HIRect;
		thePort: MacOSAll.GrafPtr;
	begin
		MacOSAll.GetPort(thePort);
		vd := ViewDataPtr(userData);
//		WriteLn('Mouse QD');

		SetPortWindowPort(vd^.window); // För att LocalToGlobal skall funka?
		BarWidgetFrame(vd^.window, b1, b2);
		
		where.h := 0;
		where.v := 0;
		MacOSAll.LocalToGlobal(where);
		
		where.h := where.h + Trunc(b2.origin.x)-0{32}; // Where does this offset come from???
		where.v := where.v + Trunc(b2.origin.y)-0{24};
		DoFunctionMenu(vd^.editIndex, vd^.window, where);
		
		// Save data and order a redisplay
//		gWhereQD := where;
//		HIViewSetNeedsDisplay(theView, true);
		MacOSAll.SetPort(thePort);
	end;
*)

	procedure DoMouseQDCGFunction (theView: HIViewRef; where: QDCG.Point; mods, button: Longint; userData: Pointer);
	var
		vd: ViewDataPtr;
		b1, b2: HIRect;
		thePort: MacOSAll.GrafPtr;
	begin
		MacOSAll.GetPort(thePort);
		vd := ViewDataPtr(userData);
//		WriteLn('Mouse QD');

		SetPortWindowPort(vd^.window); // För att LocalToGlobal skall funka?
		BarWidgetFrame(vd^.window, b1, b2);
		
		where.h := 0;
		where.v := 0;
		LocalToGlobal(where, theView);
		
		where.h := where.h + Trunc(b2.origin.x)-0{32}; // Where does this offset come from???
		where.v := where.v + Trunc(b2.origin.y)-0{24};
		DoFunctionMenu(vd^.editIndex, vd^.window, PointToMacPoint(where));
		
		// Save data and order a redisplay
//		gWhereQD := where;
//		HIViewSetNeedsDisplay(theView, true);
		MacOSAll.SetPort(thePort);
	end;

(*
	procedure DoMouseQDUses (theView: HIViewRef; where: MacOSAll.Point; mods: Longint; userData: Pointer);
	var
		vd: ViewDataPtr;
		b1, b2: HIRect;
		
		thePort: MacOSAll.GrafPtr;
//		window: WindowPtr;
//		editIndex: Longint;
	begin
		MacOSAll.GetPort(thePort);
//		window := GetWindowFromPort(thePort);
//		editIndex := GetWRefCon(window);

		vd := ViewDataPtr(userData);
//		WriteLn('Mouse QD');

		SetPortWindowPort(vd^.window); // För att LocalToGlobal skall funka?
		
		where.h := 0;
		where.v := 0;
		BarWidgetFrame(editWind[vd^.editIndex]{vd^.window}, b1, b2);
		MacOSAll.LocalToGlobal(where);
		where.h := where.h + Trunc(b1.origin.x)-0{32};
		where.v := where.v + Trunc(b1.origin.y)-0{24};
		DoUsesMenu(vd^.editIndex, where);
		
		// Save data and order a redisplay
//		gWhereQD := where;
//		HIViewSetNeedsDisplay(theView, true);
		MacOSAll.SetPort(thePort);
	end;
*)

	procedure DoMouseQDCGUses (theView: HIViewRef; where: QDCG.Point; mods, button: Longint; userData: Pointer);
	var
		vd: ViewDataPtr;
		b1, b2: HIRect;
		
		thePort: QDCG.GrafPtr;
		whereqd: MacOSAll.Point;
//		window: WindowPtr;
//		editIndex: Longint;
	begin
//		MacOSAll.GetPort(thePort);
//		window := GetWindowFromPort(thePort);
//		editIndex := GetWRefCon(window);

		vd := ViewDataPtr(userData);
//		WriteLn('Mouse QD');

		SetPortWindowPort(vd^.window); // För att LocalToGlobal skall funka?

		BarWidgetFrame(vd^.window, b1, b2);
		
		where.h := 0;
		where.v := 0;
		LocalToGlobal(where, theView);
		
		where.h := where.h + Trunc(b1.origin.x)-0{32}; // Where does this offset come from???
		where.v := where.v + Trunc(b1.origin.y)-0{24};

		
(*		
		where.h := 0;
		where.v := 0;
		
		LocalToGlobal(where, theView);
		
		BarWidgetFrame(editWind[vd^.editIndex]{vd^.window}, b1, b2);
//		whereqd := PointToMacPoint(where);
//		LocalToGlobal(whereqd);
//		where := MacPointToPoint(whereqd);
		where.h := where.h + Trunc(b1.origin.x)-0{32};
		where.v := where.v + Trunc(b1.origin.y)-0{24};
*)
		DoUsesMenu(vd^.editIndex, PointToMacPoint(where));
		
		// Save data and order a redisplay
//		gWhereQD := where;
//		HIViewSetNeedsDisplay(theView, true);
//		MacOSAll.SetPort(thePort);
	end;

// Click in invisible view over the window title
procedure DoMouseQDCGName (theView: HIViewRef; where: QDCG.Point; mods, button: Longint; userData: Pointer);
var
	foundWindow: WindowPtr;
	editIndex: Longint;
		err: OSErr;
		pt: MacOSAll.Point;
		bounds: MacOSAll.Rect;
		theMods: Longint;
		pathMenu: MenuRef;
		menuSelection, item, i: Longint;
		path: AnsiString;
		itemString: String;
begin
	GetControlBounds(theView, bounds);
	pt := bounds.topLeft;
	MacOSAll.LocalToGlobal(pt);

	foundWindow := GetControlOwner(theView);
	editIndex := GetWRefCon(foundWindow);
	if editIndex > 0 then
	begin
		theMods := GetCurrentEventKeyModifiers;
			
		if BitAnd(theMods, optionKey) <> 0 then // option-click in drag bar - function menu
		begin
			DoFunctionMenu(editIndex, foundWindow, pt);
		end
		else if BitAnd(theMods, cmdKey) <> 0 then	// command-click in drag bar
		begin
			pathMenu := FSSpecToPathNameMenu(fileSpec[editIndex], kPathMenuID);
			if pathMenu <> nil then
			begin
				InsertMenu(pathMenu, -1);
				
				// WARNING: HARD CODED ADJUSTMENT! Should use top of window.
				menuSelection := PopUpMenuSelect(pathMenu, Trunc(pt.v-22), Trunc(pt.h), 0);
				
				// Build path from selection and menu items
				// Could also call a path-builder in FileNameUtils.p
				item := Lo(menuSelection);
				if item > 1 then
				begin
//									path := '/Volumes'; // All volumes are under this.
					path := ''; // All volumes are under this.
					for i := CountMenuItems(pathMenu) downto item do // Loop from last item to selected
					begin
						GetMenuItemText(pathMenu, i, itemString);
						path := path + '/' + itemString;
					end;
					
					ProcessToString('/usr/bin/open "' + path + '"');
				end;
				
				DeleteMenu(kPathMenuID); // kPathMenuID eller nåt sånt
				DisposeMenu(pathMenu);
			end;
		end
//						else if BitAnd(theMods, shiftKey) <> 0 then
//						begin
//						end
		else if BitAnd(theMods , controlKey) <> 0 then
		begin
			DoUsesMenu(editIndex, pt);
		end;
	end;
end;

(*
procedure DoMouseQDCGName (theView: HIViewRef; where: QDCG.Point; mods: Longint; userData: Pointer);
var
	foundWindow: WindowPtr;
	editIndex: Longint;
begin
	WriteLn('Mouse in name view!');
//	SkelCallNextEventHandler;
	foundWindow := GetControlOwner(theView);
	editIndex := GetWRefCon(foundWindow);
	if editIndex > 0 then
		if EditDragClickEventHandler(editIndex, foundWindow, PointToMacPoint(where)) then
			;
end;
*)

	procedure TestDrawName(theView: HIViewRef; r: QDCG.Rect; userData: Pointer);
	var
		vd: ViewDataPtr;
	begin
		vd := ViewDataPtr(userData);
		BackColor(redColor);
//		QDCG.EraseRect(r);
		QDCG.FrameRect(r);
		QDCG.MoveTo(1, 9);
		QDCG.TextSize(9);
		QDCG.DrawString('Name');
	end;

(*
procedure UpdateOLD(resized: Boolean);
var
	b1, b2: HIRect;
	thePort: GrafPtr;
	window: WindowPtr;
	editIndex: Longint;
begin
Halt;
//WriteLn('Updating edit window');
	GetPort(thePort);
	window := GetWindowFromPort(thePort);
	editIndex := GetWRefCon(window);
	
	if editIndex > 0 then
	begin
		if resized then
		begin
			BarWidgetFrame(window, b1, b2);
			HIViewSetFrame( usesView[editIndex],  b1);
			HIViewSetFrame( functionView[editIndex],  b2);
			teEdit[editIndex]^.invalLevel := 2; // Full redraw on resize
			// THIS SHOULD BETTER BE DETECTED BY HALDA!
		end;
		
		if resized then
		begin
//WriteLn('Updating edit window resize');
			HAdjustTextEditArea(teEdit[editIndex]);
			HAdjustScrollBarRange(teEdit[editIndex]);
		end;
		
//WriteLn('Updating edit window, HDraw');
//		CreatePortQDPort(MacOSAll.GrafPtr(nil));
//		HDraw(teEdit[editIndex]);
//		FinishPort;
		UpdateHaldaView(teEdit[editIndex]);
	end;

//WriteLn('Updating edit window, drawing controls');
	DrawControls(window);	{ redraw scroll bar }
	DrawGrowIcon(window);

	SkelCallNextEventHandler;
WriteLn('Done updating edit window');
end;
*)

	procedure UpdateWindowViews(editIndex: Longint);
	var
		b1, b2, outBounds: HIRect;
		rTitle, rStructure: MacOSAll.Rect;
	begin
		BarWidgetFrame(editWind[editIndex], b1, b2);
		HIViewSetFrame( usesView[editIndex],  b1);
		HIViewSetFrame( functionView[editIndex],  b2);

		// Now, how about that 
		GetWindowBounds( editWind[editIndex], kWindowTitleTextRgn, rTitle);
		GetWindowBounds( editWind[editIndex], kWindowStructureRgn, rStructure );

		outBounds.origin.x := rTitle.left - rStructure.left; // rTitle.right + kWidgetSpace - rStructure.left;
		outBounds.origin.y := rTitle.top - rStructure.top; {Should this center by kWidgetSize?}
		outBounds.size.width := rTitle.right - rTitle.left;
		outBounds.size.height := rTitle.bottom - rTitle.top;
		
		if not gNameViewVisible then
		begin
			outBounds.origin.y -= 50;
		end;

		b1.size.width := rStructure.right - rStructure.left;
		b1.size.height := rTitle.bottom - rTitle.top;
		b1.origin.x := rStructure.left;
		b1.origin.y := rStructure.top;
		HIViewSetFrame( nameView[editIndex], outBounds);
	end;

procedure DoInsert (h: HaldaPtr; s: AnsiString);
var
	editIndex: Integer;
	oStartOffset, oEndOffset: Longint;
	pos, row, e: Longint;
begin
//	editIndex := GetWRefCon(GetWindowFromPort(GetQDGlobalsThePort));
	editIndex := GetWRefCon(FrontWindow);
	e := 0;
	
	if Length(h^.text) > 1 then
	// Auto-indent here!
	if s = #13 then
	begin
//		WriteLn('Trying auto-indent');
		// Gå till start av raden
		row := HOffsetToRow(h, h^.selStart);
//		WriteLn('What if row doesnt exist');
		pos := h^.rowStarts[row]; // Previous row
		// Count indentation on previous row
		if (pos < Length(h^.text)) and (pos > 0) then
		while h^.text[pos] in [' ', Char(9)] do
		begin
//			WriteLn('Inside while');
			s := s + h^.text[pos]; // Will stop at CR or others
			pos := pos + 1;
			if pos > Length(h^.text) then Leave;
		end;
		// Räkna antal space eller tab
		// Lägg till dessa på s!
		
		// Count indentation on the next row
		e := h^.selStart;
		if e < Length(h^.text) then
			while (e < Length(h^.text)) and (h^.text[e] in [' ', #9]) do
				e := e + 1;
		e := e - h^.selStart;
	end;
//	WriteLn('Done auto-indent');
	
	TouchColorCoder(editIndex, h^.selStart-100); // Jump more backwards?
	HGetSelection(h, oStartOffset, oEndOffset);
	SetTextUndoable(editIndex, oStartOffset, oEndOffset+e, s);
	HSetSelection(h, oStartOffset+Length(s), oStartOffset+Length(s));	
	UpdateLineNumber(editWind[editIndex]);
	SetWindowModified (editWind[editIndex], true);
end;
	
	// Hjälpfunktion för EditOpenAndSelect
	// Returnerar fönstrets index
	// CHANGE 080605: Takes the "home folder" as parameter
	function FindOrOpen(fileName: AnsiString{Str255}; fileInHomeFolder: FSSpecString): Integer;
	var
//		refNum: Integer;
		pathIndex: Integer;
		i, editIndex: Integer;
		theSpec: FSSpecString;
		
		path: AnsiStringArray;	
		pathCount: Longint;
		pathList: AnsiStringArray;
		err: OSErr;
		isPath, found: Boolean;
		
		front: WindowPtr;
		mainSpec: FSSpecString;
	begin
		FindOrOpen := 0; // Default: Failed
		editIndex := 0; // Bug fix 130810
		
		if Length(fileName) = 0 then // Fix 150413
			Exit(FindOrOpen);

	// Pathlista genereras från inställningar!
	// Allt finns redan där, gSettingsPaths.
	// Dela upp den till strängar
	
	// MEN, om fileName är komplett sökväg så skall enbart denna sökväg ingå!
	// Observera att ErrorParser såväl som FindOrOpen använder Str255 eftersom filnamn förutsätts vara kortare än så!
	// Bör detta ändras? Viss risk för minnesläckor?
	
	// Weaknesses in FindOrOpen:
	// - 255 character file name is too short when a full path is supported. FIXED
	// - Full path can be overruled by a file in the same directory as the main file.
	// - Already open file seems strange; works just great in the same directory. But otherwise?
	
	// All in all, FindOrOpen is important, and nice, but still needs rewriting!
	// This is what it should do:
	// 1: If full path, get FSSpecString for that file.
	// otherwise search in paths, get FSSpecString for first found.
	// 2: Inspect open files, if any is identical to what was found in the paths! (Automatic feature of OpenFileToWindow)
	
	// New revision: Local paths must also be supported (for file lists)!
		
		// Generate path list from gSettingsPaths:
		isPath := false;
		for i := 1 to Length(fileName) do
			if fileName[i] = '/' then
				isPath := true;
		
		if not isPath then
		begin
			// path[0] is same as main file.
			// Additional paths from gSettingsPaths
			BuildListFromText(gSettings.Paths, pathList);

				// NEW: In order to use AppendCustomPaths, we must provide includeanalyzer with a main file!
				// (Same code as in ColorCoding!)
			front := GetFrontMainWindow;
			if front <> nil then
				err := GetEditFSSpec(front, mainSpec)
			else
				mainSpec := fileInHomeFolder;
			AppendCustomPaths('.paths', pathList, mainSpec); // Main file may reside in folder with a matching .paths

			SetLength(path, Length(pathList)+1);
			for i := 0 to Length(pathList)-1 do // corrected 071020
				path[i+1] := pathList[i];
			path[0] := ''; // home dir
			pathCount := Length(path) - 1;
			
			// Search in the search paths starting in the mainfile dir.
			// Changed to take whatever home folder that is provided!
			theSpec := TrimLastToken(fileInHomeFolder); // gLastMainFile;
			pathIndex := 0;
			
			repeat
				theSpec := theSpec + '/' + fileName;
				WriteLn('FindOrOpen trying ', theSpec, ' from "', TrimLastToken(fileInHomeFolder), '" and "', fileName, '"');
				found := FileExists(theSpec);
				
				if found then
				// OpenFileToWindow will open if it can, and check if it is already open
				editIndex := OpenFileToWindow(theSpec);
				if editIndex > 0 then
					return editIndex;
				
				pathIndex := pathIndex + 1;
				if pathIndex <= pathCount then // Fixed 080503!
				begin
					theSpec := RelativePathToFSSpec(fileInHomeFolder, path[pathIndex]); // AbsolutePathToFSSpec(path[pathIndex]);
// This is a workaround for a now fixed bug in RelativePathToFSSpec:
//					theSpec := RelativePathToFSSpec(TrimLastToken(fileInHomeFolder), path[pathIndex]); // AbsolutePathToFSSpec(path[pathIndex]);
					//WriteLn('Got spec for ', path[pathIndex]);
				end;
			until (pathIndex > pathCount);
			return 0;
		end
		else
		// Special case: If fileName includes the path, only include this path!
		begin
			SetLength(path, 1);
			i := Length(fileName);
			repeat
				i := i - 1;
			until (fileName[i] = '/') or (i < 2);
			
			path[0] := Copy(fileName, 1, i);
			fileName := Copy(fileName, i+1, Length(fileName) - i);
			
//			theSpec := RelativePathToFSSpec(TrimLastToken(fileInHomeFolder), path[0]); // AbsolutePathToFSSpec(path[pathIndex]);
			theSpec := RelativePathToFSSpec(fileInHomeFolder, path[0]); // AbsolutePathToFSSpec(path[pathIndex]);
			if theSpec[Length(theSpec)] = '/' then
				theSpec := theSpec + fileName
			else
				theSpec := theSpec + '/' + fileName;

//			found := err = noErr;
			WriteLn('FindOrOpen trying (2) ', theSpec);
			WriteLn('FindOrOpen trying ', theSpec, ' from "', TrimLastToken(fileInHomeFolder), '" and "', fileName, '"');
			return OpenFileToWindow(theSpec);
		end;

	end;
	
	
	// Didn't I already have this?
	function GetWindowNumber(w: WindowPtr): Longint;
	begin
		GetWindowNumber := GetWRefCon(w);
//		GetWindowNumber := 0;
//		for i := 1 to kMaxEditWindows do
//			if editWind[i] = w then
//				GetWindowNumber := i;
	end;
	
// Open file by name (with or without path) and select a specified position.
// MUCH OF THIS COULD BE REPLACED BY GoToLine!
	function EditOpenAndSelect(fileName: AnsiString; {Str255;} rowNumber, columnNumber: Longint): Longint;
	var
		editIndex: Integer;
		i: Longint;
		start, ending: Longint;
	begin
		EditOpenAndSelect := -1;
		
		// Open the file if it is not already open
		// gLastMainFile is now passed as parameter, since this is used for ERRORS.
		editIndex := FindOrOpen(fileName, gLastMainFile); // Felet inne i FindOrOpen?
		if editIndex < 1 then Exit(EditOpenAndSelect);
		// Gå till angiven position i filen
		EditOpenAndSelect := editIndex;
		
		rowNumber -= 1; // 0-based in Halda
		if rowNumber < 0 then rowNumber := 0;
		if rowNumber > High(teEdit[editIndex]^.rowStarts) then
			rowNumber := High(teEdit[editIndex]^.rowStarts);
		if rowNumber < 0 then Exit(EditOpenAndSelect);
		
		i := teEdit[editIndex]^.rowStarts[rowNumber];
		SelectWindow(editWind[editIndex]);
		BringToFront(editWind[editIndex]);
				
// Expand selection with nearby item
		if Length(teEdit[editIndex]^.text) > 0 then
		begin
			if columnNumber = 0 then // GCC style - whole line
			begin
				start := i + columnNumber + 1;
				ending := start;
				if ending <= Length(teEdit[editIndex]^.text) then
					while (teEdit[editIndex]^.text[ending] <> Char(13)) and (ending < Length(teEdit[editIndex]^.text)) do
						ending := ending + 1;
			end
			else
			begin // FPC/GNAT style - selected place
				start := i + columnNumber - 1; // -1 to get the actual start
				if start < 1 then start := 1;
				if start > Length(teEdit[editIndex]^.text) then start := Length(teEdit[editIndex]^.text);
				ending := start;
				if teEdit[editIndex]^.text[start] in ['A'..'Z', 'a'..'z'] then
					while (teEdit[editIndex]^.text[ending] in ['A'..'Z', 'a'..'z']) and (ending < Length(teEdit[editIndex]^.text)) do
						ending := ending + 1
				else
					ending := ending + 1;
			end;
			
			HSetSelection(teEdit[editIndex], start, ending); //i + columnNumber, i + columnNumber);
			HShowSelection(teEdit[editIndex]^.views[teEdit[editIndex]^.focusedView], true);
			HInvalViews(teEdit[editIndex]);
			UpdateLineNumber(editWind[editIndex]);
		end;
				// Skall inte detta göra så man ser valet? Nej. Fixas!
				// TESelView funkar inte utan autoview. Måste fixas själv,
				// med custom clikloop.		
	end;


	procedure GoToLine(w: WindowPtr; rowNumber, columnNumber: Longint);
	var
		editIndex: Integer;
		i: Longint;
		start, ending: Longint;
	begin
		editIndex := GetWindowNumber(w);
		if editIndex < 1 then
			Exit(GoToLine);

		rowNumber -= 1; // Off by 1 for some reason
		if rowNumber < 0 then rowNumber := 0;
		if rowNumber > High(teEdit[editIndex]^.rowStarts) then
			rowNumber := High(teEdit[editIndex]^.rowStarts);
		if rowNumber < 0 then Exit(GoToLine);
		
		i := teEdit[editIndex]^.rowStarts[rowNumber];
		
				SelectWindow(editWind[editIndex]);
				BringToFront(editWind[editIndex]);

// Expand selection with nearby item
				if columnNumber = 0 then // GCC style - whole line
				begin
					start := i + columnNumber + 1;
					ending := start;
					while (teEdit[editIndex]^.text[ending] <> Char(13)) and (ending < Length(teEdit[editIndex]^.text)) do
							ending := ending + 1;
				end
				else
				begin // FPC/GNAT style - selected place
					start := i + columnNumber;
					ending := start;
					if teEdit[editIndex]^.text[start] in ['A'..'Z', 'a'..'z'] then
						while (teEdit[editIndex]^.text[ending] in ['A'..'Z', 'a'..'z']) and (ending < Length(teEdit[editIndex]^.text)) do
							ending := ending + 1
					else
						ending := ending + 1;
				end;
				
				HSetSelection(teEdit[editIndex], start, ending); //i + columnNumber, i + columnNumber);
				HShowSelection(teEdit[editIndex]^.views[teEdit[editIndex]^.focusedView], true);
				UpdateLineNumber(editWind[editIndex]);

				// Skall inte detta göra så man ser valet? Nej. Fixas!
				// TESelView funkar inte utan autoview. Måste fixas själv,
				// med custom clikloop.
	end;

var
//	savedp: Point;
	isPressed: Boolean; // Behövs inte? Jo, för att ersätta mouseUp-events!

//procedure Mouse (qdPt: MacOSAll.Point; t: LongWord; mods: integer);
procedure Mouse(theView: HIViewRef; thePt: QDCG.Point; mods, button: Longint; userData: Pointer);
var
	editIndex, oStartOffset, oEndOffset, row, i: Longint;
	qdPortRect: MacOSAll.Rect;
	portRect: Rect;
//	thePt: Point;
	p: MacOSAll.GrafPtr;
begin
	MacOSAll.GetPort(p);
//	editIndex := GetWRefCon(GetWindowFromPort(p));
	
	editIndex := GetWRefCon(HIViewGetWindow(theView));
	qdPortRect := GetWindowPortBounds(GetWindowFromPort(p), qdPortRect)^;
	
//	thePt := MacPointToPoint(qdPt);
	portRect := MacRectToRect(qdPortRect);
	
	// FIRST test line number area
	// then test if in the proper vertical area for each view
	// No, views first.
	
	for i := 0 to High(teEdit[editIndex]^.views) do
	begin
		if PtInRect(thePt, teEdit[editIndex]^.views[i]^.viewRect) then
		begin
			HMouse(teEdit[editIndex]^.views[i], thePt, mods);
			isPressed := true; // Behövs inte? Jo!
			Exit(Mouse);
		end;
		if thePt.v > teEdit[editIndex]^.views[i]^.viewRect.top then
		if thePt.v < teEdit[editIndex]^.views[i]^.viewRect.bottom then
		if thePt.h < teEdit[editIndex]^.views[i]^.viewRect.left then
		begin
			row := GetRowFromPoint(teEdit[editIndex]^.views[i], thePt);
			if OKtoSetBreaks then
			begin
				WriteLn('Toggle breakpoint at ', row);
				ToggleBreakpoint(GetWindowFromPort(GetQDGlobalsThePort), row+1, mods);
			end
			else
				SysBeep(1);	//  if the program is running and is in debug mode , setting breakpoints will hang the IDE
	
			// InvalRect! Entire column or locally
			portRect.right := kTextBoxLeftMargin;
//			InvalWindowRect(GetWindowFromPort(GetQDGlobalsThePort), RectToMacRect(portRect));
			HIViewSetNeedsDisplay(theView, true); // Why not HInvalView?
			Exit(Mouse);
		end;
	end;
	
	// OBSOLETE
		// Are we in the line number area?
if false then
		if (thePt.h < kLineNumberBoxWidth) and (thePt.v > portRect.bottom - kLineNumberBoxHeight) then
		begin
			HGetSelection (teEdit[editIndex], oStartOffset, oEndOffset);
			row := HOffsetToRow(teEdit[editIndex], oEndOffset);
			ShowGoToLineDialog(row+1);
			Exit(Mouse);
		end;
		
		SkelCallNextEventHandler;
end;

procedure InvalBreakpoints(view: HaldaViewPtr);
var
	portRect: MacOSAll.Rect;
begin
	portRect := GetWindowPortBounds(GetWindowFromPort(GetQDGlobalsThePort), portRect)^;
	portRect.right := kTextBoxLeftMargin;
//	InvalWindowRect(GetWindowFromPort(GetQDGlobalsThePort), portRect);
	HIViewSetNeedsDisplay(SkelGetContentView(HIViewGetWindow(view^.hiview)), true);
end;

var
	theCount: Longint; // Funkar bara om front only
		
procedure Idle;
var
	p: Point;
	qdp: MacOSAll.Point;
	editIndex: Longint;
begin
	editIndex := GetWRefCon(GetWindowFromPort(GetQDGlobalsThePort));
	
{PosToPoint, blinka vid punkten}
	theCount := (theCount + 1) mod 3; // För att detta demo behöver högre frekvens för resten
										// Hellre kolla klockan i HIdle?
	if theCount = 0 then
		HIdle(teEdit[editIndex]^.views[teEdit[editIndex]^.focusedView]);

	if editIndex > 0 then
	begin
		GetMouse(qdp);
		p := MacPointToPoint(qdp);
		
		if not Button then
		begin
			if isPressed then
			begin
				HMouseUp(teEdit[editIndex]^.views[teEdit[editIndex]^.focusedView], p, 0);
				UpdateLineNumber(editWind[editIndex]);
			end;
			isPressed := false; // Behövs inte?
		end;
		
		if (p.h <> savedp.h) or (p.v <> savedp.v) then
			if isPressed then
				HMouseDrag(teEdit[editIndex]^.views[teEdit[editIndex]^.focusedView], p);
		savedp := p;
	end;
end;

procedure DoResize(theEditWind: WindowRef); // theView: ControlRef); //; userData: Pointer);
var
	editIndex, i: Longint;
	b1, b2: HIRect;
	bounds: MacOSAll.Rect;
begin
//WriteLn('DoResize');
	editIndex := GetWRefCon(theEditWind);
// Always adjust (to keep horizontal valid)
//	HAdjustScrollBarRanges(teEdit[editIndex]);
	
	// Resizing of editor area
	HAdjustTextEditArea(teEdit[editIndex], kLineNumberBoxWidth, 0,0,0, 10,0,0,0);
	HAdjustScrollBarRanges(teEdit[editIndex]);
	
	// Resize of dragbar icons
	BarWidgetFrame(editWind[editIndex], b1, b2);
	HIViewSetFrame( usesView[editIndex],  b1);
	HIViewSetFrame( functionView[editIndex],  b2);
	
	HIViewSetNeedsDisplay(SkelGetContentView(theEditWind), true);
end;


procedure DoUpdate(theView: ControlRef; viewRect: QDCG.Rect; userData: Pointer); // theView: NSView; viewRect: Rect; userData: Pointer);register;
var
	editIndex, i: Longint;
	b1, b2: HIRect;
	bounds: MacOSAll.Rect;
begin
//	ForeColor(magentaColor); PaintRect(viewRect);
	InsetRect(viewRect, 20, 20);
	ForeColor(blackColor);
	FrameRect(viewRect);
	
	editIndex := GetWRefCon(HIViewGetWindow(theView));

// Always adjust (to keep horizontal valid)
	HAdjustScrollBarRanges(teEdit[editIndex]);
	
	if editIndex < 1 then WriteLn('Fanken');
	if teEdit[editIndex] = nil then WriteLn('Fanken');

//	if resized then
	for i := 0 to High(teEdit[editIndex]^.views) do
			teEdit[editIndex]^.views[i]^.invalLevel := 2;
	for i := 0 to High(teEdit[editIndex]^.views) do
	begin
//WriteLn('Calling HDraw');
		HDraw(teEdit[editIndex]^.views[i]);
	end;
//WriteLn('Calling DrawPrivate');
	for i := 0 to High(teEdit[editIndex]^.views) do
		DrawPrivate(teEdit[editIndex]^.views[i]);
//WriteLn('Calling DrawControls');
// This is only done for view[0] since all views share the same window
	if teEdit[editIndex]^.views[0]^.window <> nil then
	begin
		DrawControls(teEdit[editIndex]^.views[0]^.window);	{ redraw scroll bar }
		DrawGrowIcon(teEdit[editIndex]^.views[0]^.window);
	end
	else
	begin
		DrawControls(HIViewGetWindow(theView));	{ redraw scroll bar }
		DrawGrowIcon(HIViewGetWindow(theView));
	end;
	
	FinishPort;
end;


(*procedure DoUpdate (resized: Boolean);
var
	editIndex, i: Longint;
	b1, b2: HIRect;
	bounds: MacOSAll.Rect;
begin
	editIndex := GetWRefCon(GetWindowFromPort(GetQDGlobalsThePort));
//	WriteLn('Updating window ', editIndex);

// Always adjust (to keep horizontal valid)
	HAdjustScrollBarRanges(teEdit[editIndex]);

	if resized then
	begin
		// Resizing of editor area
//		WriteLn('Top before resize: ', teEdit[editIndex]^.topRow);
//		HAdjustTextEditArea(teEdit[editIndex]);
		HAdjustTextEditArea(teEdit[editIndex], kLineNumberBoxWidth, 0,0,0, 10,0,0,0);
		HAdjustScrollBarRanges(teEdit[editIndex]);
//		WriteLn('Top after resize: ', teEdit[editIndex]^.topRow);
		
		// Resize of dragbar icons
		BarWidgetFrame(editWind[editIndex], b1, b2);
		HIViewSetFrame( usesView[editIndex],  b1);
		HIViewSetFrame( functionView[editIndex],  b2);
		
//		GetWindowPortBounds(editWind[i], bounds);
//		bounds.right := bounds.left + kTextBoxLeftMargin;
//		InvalWindowRect(editWind[i], bounds);

//		Exit(DoUpdate);

		// Should only erase the special areas on expansion
		CreatePortQDPort(GetQDGlobalsThePort);
		GetWindowPortBounds(editWind[editIndex], bounds);
		EraseRect(MacRectToRect(bounds));
	end
	else
		CreatePortQDPort(GetQDGlobalsThePort);

	// Always adjust (to keep horizontal valid)
	// Crashes! (Moved to above.)
//	HAdjustScrollBarRanges(teEdit[editIndex]);
	
//	HDraw(teEdit[editIndex]);
	if teEdit[editIndex] = nil then WriteLn('Fanken');
//	UpdateHaldaView(teEdit[editIndex]);
//WriteLn('Calling HDraw');
	// Force a full redraw on resize
	
//	HSetFont(teEdit[editIndex]^.h, 'Monaco', GetSettingsTextSize);
// Crashes! Why? Because HSetFont is NOT sane! The problem is QDCG's
// port-handling model!
// Revised - now it doesn't crash... but it doesn't quite work either.

//	if resized then
	for i := 0 to High(teEdit[editIndex]^.views) do
			teEdit[editIndex]^.views[i]^.invalLevel := 2;
	for i := 0 to High(teEdit[editIndex]^.views) do
		HDraw(teEdit[editIndex]^.views[i]);
//WriteLn('Calling DrawPrivate');
	for i := 0 to High(teEdit[editIndex]^.views) do
		DrawPrivate(teEdit[editIndex]^.views[i]);
//WriteLn('Calling DrawControls');
// This is only done for view[0] since all views share the same window
	DrawControls(teEdit[editIndex]^.views[0]^.window);	{ redraw scroll bar }
	DrawGrowIcon(teEdit[editIndex]^.views[0]^.window);
	
	FinishPort;
//WriteLn('DoUpdate done');
end;*)

procedure LiveScroll(view: HaldaViewPtr);
begin
//	WriteLn('LiveScroll');
		SetPortWindowPort(view^.window);
//		DoUpdate(false);
//		HIViewSetNeedsDisplay(SkelGetContentView(view^.window), true);
		HIViewSetNeedsDisplay(SkelGetContentView(HIViewGetWindow(view^.hiview)), true);
//	WriteLn('Done LiveScroll');
end;

procedure Key (ch: char; mods: integer);
//procedure Key(theView: HIViewRef; ch: Char; mods: Longint; userData: Pointer);
var
	editIndex: Longint;
begin
//	editIndex := GetWRefCon(GetWindowFromPort(GetQDGlobalsThePort));
	editIndex := GetWRefCon(FrontWindow);
	if editIndex < 1 then Exit(Key);
//	WriteLn('Key for ', editIndex);
	
//	HKey(teEdit[editIndex], ch, mods);
//	if BitAnd(mods, cmdKey) <> 0 then
//		Exit(Key);

	if ch = Char(27) then
	begin
		BuildCCMenu(editIndex, kPathMenuID);
		Exit(Key);
	end;

	HKeyCustom(teEdit[editIndex]^.views[teEdit[editIndex]^.focusedView], ch, mods, DoInsert);
	UpdateLineNumber(FrontWindow);
	SynchUndoMenu;
//	TouchColorCoder(editIndex, teEdit[editIndex]^.h^.selStart-1); // Jump more backwards? Moved to DoInsert!
end;

procedure DoActivate(myBool: Boolean);
var
	p: MacOSAll.GrafPtr;
	editIndex, mainIndex, i: Longint;
	bounds: MacOSAll.Rect;
	s: Str255;
	mods: UInt32;
begin
//WriteLn('Activate');
	
	// Check for front windows - only re-tested on activation and when the "main window" flags changes.
	InvalFrontEditWindow;
	InvalFrontMainWindow;
	
	if not myBool then
		isPressed := false;
	
	// Needs update on activate?
	if myBool then
	begin
		MacOSAll.GetPort(p);
		editIndex := GetWRefCon(GetWindowFromPort(p));
		GetWindowPortBounds(editWind[editIndex], bounds);
		InvalWindowRect(editWind[editIndex], bounds);
		
		// Reload settings?
		// Is the activated window in front yet? Does GetFrontMainWindow get the right one?
		mainIndex := GetWindowNumber(GetFrontMainWindow);
		GetWTitle(GetFrontMainWindow, s);
		if mainIndex = editIndex then
		begin
			WriteLn('Loading settings for ', s);
			LoadSettingsForWindow(mainIndex);
		end;
		
		RefreshProjectWindow;
	end;
// Update the name view
			mods := GetCurrentEventKeyModifiers;
//			SetControlVisibility(nameView[editIndex], mods = 0, mods = 0); did not work
			gNameViewVisible := mods <> 0;
			for i := 1 to kMaxEditWindows do
				if editWind[i] <> nil then
					UpdateWindowViews(i);
end;

procedure AddView(h: HaldaPtr; w: WindowPtr);
var
	view: HaldaViewPtr;
begin
	view := HNewView(w, h, true, true);
	view^.additionalInval := InvalBreakpoints;
	view^.liveScrollProc := LiveScroll;
	
	view^.hiview := SkelGetContentView(w);
	view^.window := nil;

// Set the margin:
	view^.leftMargin := 10;
	HSetBackgroundColor(view, HSDMakeHaldaColor(1.0, 1.0, 1.0));
	
	HAdjustTextEditArea(h, kLineNumberBoxWidth, 0,0,0, 10,0,0,0);
	HAdjustScrollBarRanges(h);
end;

procedure ToggleDoubleView;
var
	h: HaldaPtr;
	bounds: MacOSAll.Rect;
	w: WindowRef;
begin
	h := EditGetCurrentTE;
	
	if Length(h^.views) = 1 then
	begin
		HInvalViews(h);
		w := HIViewGetWindow(h^.views[0]^.hiview);
		AddView(h, w);
//		AddView(h, h^.views[0]^.window);
	end
	else
	if Length(h^.views) = 2 then
	begin
		HDisposeView(h^.views[1]);
		HInvalViews(h);
	end;
	
//	bounds := HDSGetViewPortRect(h^.views[0]);
//	GetWindowPortBounds(h^.views[0]^.window, bounds);
//	InvalWindowRect(h^.views[0]^.window, bounds);
	HAdjustTextEditArea(h, kLineNumberBoxWidth, 0,0,0, 10,0,0,0);
end;

procedure SendViewToBack(control: HIViewRef);
var
	otherControl: HIViewRef;
begin
	// Put the view below all existing ones!
//	otherControl := HIViewGetFirstSubview(HIViewGetWindow(control)); Looks incporrect! /ingemar
	otherControl := HIViewGetFirstSubview(HIViewGetRoot(HIViewGetWindow(control)));
	while otherControl <> nil do
	begin
		if otherControl <> control then
			HIViewSetZOrder(control, kHIViewZOrderBelow, otherControl);
		otherControl := HIViewGetNextView(otherControl);
	end;
end;

procedure UpdateNbr(theView: HIViewRef; viewRect: QDCG.Rect; userData: Pointer);
var
	qdr: MacOSAll.Rect;
	portRect: Rect;
	oStartOffset, oEndOffset, line, editIndex: Longint;
	theString: String;
begin
//	WriteLn('UpdateNbr');
//	Exit(UpdateNbr);

	GetControlBounds(theView, qdr);
//	qdr := GetWindowPortBounds(view^.window, qdr)^;		{ paint window dark gray }
	portRect := MacRectToRect(qdr);
	portRect.right := portRect.left + kLineNumberBoxWidth; // 50;
	portRect.top := portRect.bottom - kLineNumberBoxHeight;
	portRect.bottom := portRect.bottom + 1;

//	ForeColor(lightBlueColor);
//	EraseRect(portRect);
//	PaintRect(viewRect);
	
	
	ForeColor(lighterGreyColor);
//	EraseRect(portRect);
	PaintRect(viewRect);
//	FrameRect(portRect);

//	RGBForeColor(linecol);
//	FrameRect(portRect);
	ForeColor(blackColor);

// Line number	
	
//	pos := 0;
	line := 0;
//	ch := Char(10);
	
	editIndex := GetWRefCon(HIViewGetWindow(theView));
	if (editIndex > 0) and (editIndex < kMaxEditWindows) then
	begin
//		teEdit[editIndex]^.views[teEdit[editIndex]^.focusedView]
		HGetSelection (teEdit[editIndex], oStartOffset, oEndOffset);
		line := HOffsetToRow(teEdit[editIndex], oStartOffset);
		Str(line+1, theString); // +1 since Halda is 0-based
	end
	else
		theString := '-';
	
	MoveTo(3, portRect.bottom-5);
	MoveTo(3, 12);
	TextSize(12);
	DrawString(theString);

	
//	ForeColor(blackColor);
//	MoveTo(2, 12);
//	DrawString('345');
	FinishPort;
end;
procedure MouseNbr(theView: HIViewRef; where: QDCG.Point; mods, button: Longint; userData: Pointer);
var
	editIndex, row, oStartOffset, oEndOffset: Longint;
begin
	editIndex := GetWRefCon(HIViewGetWindow(theView));
	WriteLn('MouseNbr');
				HGetSelection (teEdit[editIndex], oStartOffset, oEndOffset);
				row := HOffsetToRow(teEdit[editIndex], oEndOffset);
//				row := UpdateRowFromCharOffset(editIndex, oEndOffset);
//				WriteLn('Trying to show line ', row);

				ShowGoToLineDialog(row);
// THIS WORKS BADLY SINCE THE VIEW ISNT MOVED PROPERLY!!!
end;

procedure EditScroll(affectedWindow: WindowPtr; axis: EventMouseWheelAxis; wheelDelta: Longint);
var
	hView: HaldaViewPtr;
	max, value, i: Longint;
	scrollbar: HIViewRef;
	editIndex, f: Longint;
	pt: MacOSAll.Point;
	savePort: MacOSAll.GrafPtr;
begin
	editIndex := GetWindowNumber(affectedWindow);
	if editIndex > 0 then
	begin
		GetMouse(pt);
//		GlobalToLocal?
// Must change to other window's coordinate system!
// This is needed so we can figure out what Haldaview to scroll!
		MacOSAll.GetPort(savePort);
		MacOSAll.LocalToGlobal(pt);
		MacOSAll.SetPortWindowPort(affectedWindow);
		MacOSAll.GlobalToLocal(pt);
		MacOSAll.SetPort(savePort);
//		f := teEdit[editIndex]^.focusedView; // Should rather check which view is under the mouse!
		f := -1;
		for i := 0 to High(teEdit[editIndex]^.views) do
			if PtInRect(MacPointToPoint(pt), teEdit[editIndex]^.views[i]^.viewRect) then
				f := i;
		if f < 0 then
			Exit(EditSCroll);
//		f := teEdit[editIndex]^.focusedView; // Should rather check which view is under the mouse!
		if (f >= 0) and (f <= High(teEdit[editIndex]^.views)) then
		begin
			hView := teEdit[editIndex]^.views[f];
			if axis = kEventMouseWheelAxisY then
				scrollbar := hView^.vScroll
			else
				scrollbar := hView^.hScroll;
			if scrollbar <> nil then
			begin
				value := VMGetNumValue(scrollbar) - wheelDelta;
				max := GetControl32BitMaximum(scrollbar);
				if value < max then
					VMSetNumValue(scrollbar, value);
			end;
		end;
	end;	
end;

function EditWindInit: WindowPtr;
var
	w: WindowPtr;
	r: MacOSAll.Rect;
	editIndex: Longint;
	h: HaldaPtr;
//	view: HaldaViewPtr;
	screenBits: BitMap;
	screenBounds, rStructure: MacOSAll.Rect;
	theWindowAttributes: WindowAttributes;
	err: OSErr;
	width, height, hCount, vCount, iCount, i: Longint;
// For creating popups
	boundsRect: MacOSAll.Rect;
	root: HIViewRef;
	functionDataPtr, usesDataPtr: ViewDataPtr;
	control: HIViewRef;
	layout: HILayoutInfo;
// Window offset
	const
		kOffsetDistance = 20;
	const
{Custom handler for click in window drag bar}
		customEventTypes: array [0..3] of EventTypeSpec =
		(
			( eventClass: kEventClassWindow; eventKind: kEventWindowClickDragRgn ), // Ignored by the stupid OS!
//			( eventClass: kEventClassMouse; eventKind: kEventMouseWheelMoved ), moved into TransSkel
// and for adjusting the cursor as the mouse moves
			( eventClass: kEventClassMouse; eventKind: kEventMouseMoved ),
//			( eventClass: kEventClassMouse; eventKind: kEventMouseEntered ),
			( eventClass: kEventClassMouse; eventKind: kEventMouseExited ),
			( eventClass: kEventClassKeyboard; eventKind: kEventRawKeyModifiersChanged )
);
begin
	EditWindInit := nil;
		
	for editIndex := 1 to kMaxEditWindows do
	begin
		if editWind[editIndex] = nil then
		begin
			WriteLn('Found editIndex for new window = ', editIndex);
			Leave;
		end;
	end;
	if editIndex > kMaxEditWindows then
		Exit(EditWindInit);

// Calculate position of new window
		screenBits := GetQDGlobalsScreenBits(screenBits)^;
		screenBounds := screenBits.bounds;
		
		case gSettings.EditWindowPosition of
			1:
				MacOSAll.SetRect(r,355,45,900,700);//Standard LWP window size 
			2:
				MacOSAll.SetRect(r,20,45,screenBounds.right-60,(screenBounds.bottom div 10)*4); //Middle Edit Up;
			3:
				MacOSAll.SetRect(r,20,(screenBounds.bottom div 10)*4+45,screenBounds.right-60,ScreenBounds.bottom-68);
			4:
			begin
				if editIndex Mod 2 = 0 Then
					MacOSAll.SetRect(r,20,45,screenBounds.right-60,(screenBounds.bottom div 10)*4)
				else
					MacOSAll.SetRect(r,20,(screenBounds.bottom div 10)*4+45,screenBounds.right-60, screenBounds.bottom-68);
			end;
			5:
			begin
				if editIndex Mod 2 = 1 Then
					MacOSAll.SetRect(r,20,45,(screenBounds.right Div 2)-60,screenBounds.bottom-68)
				else
					MacOSAll.SetRect(r,(screenBounds.right Div 2)+20,45,screenBounds.right-60,ScreenBounds.bottom-68);     
			end;
			otherwise
				MacOSAll.SetRect(r,355,45,900,700);//Standard LWP window size 
		end; {case}
		
		// Instead of moving later
		iCount := editIndex;
		if gSettings.editWindowPosition >= 4 then
			iCount := (iCount div 2) + 1;
		width := screenBounds.right - r.right;
		height := screenBounds.bottom - r.bottom;
		hCount := width div kOffsetDistance;
		vCount := height div kOffsetDistance;
		WriteLn(hCount, ',', vCount);
		hCount := (iCount-1) mod hCount;
		vCount := (iCount-1) mod vCount;
		OffsetRect(r, hCount * kOffsetDistance, vCount * kOffsetDistance);
//	r.top := 50;
//	r.left := 20;
//	r.bottom := 300;
//	r.right := 450;

// Create window
(*
//	w := NewCWindow(nil, r, 'New text editor test', true, documentProc, WindowPtr(-1), true, 0);
	theWindowAttributes := kWindowStandardDocumentAttributes or kWindowLiveResizeAttribute;
	editWind[editIndex] := nil;
	err := CreateNewWindow ( kDocumentWindowClass, theWindowAttributes, r, editWind[editIndex]);
	w := editWind[editIndex];
	SetPortWindowPort(w);
//	SkelWindow(w, @Mouse, @Key, @DoUpdate, nil, nil, nil, @Idle, true);
	SkelCustomWindow(w, @Mouse, @Key, @DoUpdate, @DoActivate, @DoClose, @DoHalt, @Idle, true,
							kEventDurationSecond/10,
							// nil, nil, 0);
							@EditWindowEventHandler, @customEventTypes[0], Length(customEventTypes));
*)
	editWind[editIndex] := SkelNewWindow(MacRectToRect(r), 'Untitled', @DoUpdate, @Mouse, @Key, nil,
		@DoActivate, @DoClose, @DoHalt, @Idle, true, kEventDurationSecond/5, @EditWindowEventHandler, @customEventTypes[0], Length(customEventTypes), @DoResize);
	SkelSetScrollProc(editWind[editIndex], @EditScroll);
	w := editWind[editIndex];
	SetPortWindowPort(w);
	editWind[editIndex] := w;
	SetWRefCon(editWind[editIndex], editIndex);
	WriteLn('editIndex for new window = ', editIndex);
//	SetWTitle(editWind[editIndex], 'Untitled');
	InitUndoForIndex(editIndex); // Clear undo for the window
	editWindIsMain[editIndex] := false; {Default - not main program file}

	boundToFile[editIndex] := false;
	fileSpec[editIndex] := '';
	SetWRefCon(editWind[editIndex], editIndex);

//	GetFNum('Monaco', fontNum);
//	TextFont(fontNum); {Monaco}
//	TextSize(10);

//	TextFont('Monaco'); {Monaco}
//	TextSize(10);

WriteLn('Time to create TE');
	
	h := HNew; // CRASH - must have a port?
WriteLn('Time to set font to size ', GetSettingsTextSize);
	HSetFont(h, 'Monaco', GetSettingsTextSize);
	SetTextToNormal(h, false, GetSettingsTextSize); // Try to get it to open in a stable state
//	TouchColorCoder(editIndex, 0);
WriteLn('Set font OK'); // Kommer hit!
	
//	HSetText(h, '(Temporary text for an empty window)');
	HSetText(h, '');
WriteLn('create view');
	teEdit[editIndex] := h;
	AddView(h, w);

WriteLn('Popups');
		// Place popup view
		SetRect(boundsRect, 0, 0, 16, 16);
		CreateUserPaneControl(editWind[editIndex], boundsRect, {features} 0, functionView[editIndex]);
		CreateUserPaneControl(editWind[editIndex], boundsRect, {features} 0, usesView[editIndex]);
		CreateUserPaneControl(editWind[editIndex], boundsRect, {features} 0, nameView[editIndex]);
		root := HIViewGetRoot( editWind[editIndex] );
		HIViewAddSubview( root, functionView[editIndex] );
		HIViewAddSubview( root, usesView[editIndex] );
		HIViewAddSubview( root, nameView[editIndex] );
		
		functionDataPtr := ViewDataPtr(NewPtrClear(SizeOf(ViewDataRec)));
		functionDataPtr^.view := functionView[editIndex];
		functionDataPtr^.window := editWind[editIndex];
		functionDataPtr^.editIndex := editIndex;
//		functionDataPtr^.icon := functionIconRef;
		InstallQDCGSkelViewHandler(editWind[editIndex], functionView[editIndex],
			DoFunctionViewUpdateQDCG, DoMouseQDCGFunction, nil, Ptr(functionDataPtr));
		
		usesDataPtr := ViewDataPtr(NewPtrClear(SizeOf(ViewDataRec)));
		usesDataPtr^.view := usesView[editIndex];
		usesDataPtr^.window := editWind[editIndex];
		usesDataPtr^.editIndex := editIndex;
//		usesDataPtr^.icon := usesIconRef;

		InstallQDCGSkelViewHandler(editWind[editIndex], usesView[editIndex],
			DoUsesViewUpdateQDCG, DoMouseQDCGUses, nil, Ptr(usesDataPtr));
//		InstallEventHandler( GetWindowEventTarget(editWind[editIndex]), BarViewEventHandlerProc, Length(barViewEvents), barViewEvents, nil, nil );
		// position the views

		InstallQDCGSkelViewHandler(editWind[editIndex], nameView[editIndex],
			nil {TestDrawName - for making the nameView visible}, DoMouseQDCGName, nil, Ptr(usesDataPtr));

		UpdateWindowViews(editIndex);

(*	control := SkelGetContentView(editWind[editIndex]);
	GetControlBounds(control, r);
	r.right -= 15;
	r.bottom -= 15;
	SetControlBounds(control, r);*)

// Create line number view!
		GetWindowBounds( editWind[editIndex], kWindowStructureRgn, rStructure );
		GetWindowBounds( editWind[editIndex], kWindowContentRgn, r );
		OffsetRect(r, -rStructure.left, -rStructure.top);
		
		r.top := r.bottom - 15;
		r.right := r.left + kLineNumberBoxWidth;//50;
		
		CreateUserPaneControl(editWind[editIndex], r, {features} 0, control);
		HIViewAddSubview( HIViewGetRoot( editWind[editIndex] ), control);

		// Auto-position
		layout.version := kHILayoutInfoVersionZero;
		layout.binding.top.toView := HIViewGetRoot(editWind[editIndex]);
		layout.binding.top.kind := kHILayoutBindNone;
		layout.binding.left.toView := HIViewGetRoot(editWind[editIndex]);
		layout.binding.left.kind := kHILayoutBindLeft;
		layout.binding.right.toView := HIViewGetRoot(editWind[editIndex]);
		layout.binding.right.kind := kHILayoutBindNone;
		layout.binding.bottom.toView := HIViewGetRoot(editWind[editIndex]);
		layout.binding.bottom.kind := kHILayoutBindBottom;
		layout.scale.x.kind := kHILayoutBindNone;
		layout.scale.y.kind := kHILayoutBindNone;
		layout.position.x.kind := kHILayoutBindNone;
		layout.position.y.kind := kHILayoutBindNone;

		HIViewSetLayoutInfo(control, layout);

		InstallQDCGSkelViewHandler(editWind[editIndex], control,
			@UpdateNbr, @MouseNbr, nil, Ptr(editWind[editIndex]));
		
		VMSetControlID(control, 'LNBR', 0);
//		control := VMGetControl(window, 'LNBR', 0);

// Done line number view!

	SendViewToBack(SkelGetContentView(editWind[editIndex]));

	HAdjustTextEditArea(teEdit[editIndex], kLineNumberBoxWidth, 0,0,0, 10,0,0,0);
	for i := 0 to High(teEdit[editIndex]^.views) do
		HAdjustScrollBarRange(teEdit[editIndex]^.views[i]);
//	HAdjustScrollBarRange(teEdit[editIndex]^.views[0]);
	RebuildWindowMenu;
	ShowWindow(w); // Kraschar här
	
	EditWindInit := w;
WriteLn('Edit window created');
end;

var
	i: Longint;
begin
	for i := 1 to kMaxEditWindows do
		teEdit[i] := nil;
end.

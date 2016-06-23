// Lightweight Pascal IDE, find/replace unit
// © Ingemar Ragnemalm 2006-2007

{$mode macpas}
unit FindReplaceDialog;
interface
uses
	MacOSAll, UtilsTypes, TransSkel4, LWPGlobals, LWPEdit, AlertsUtils, Console,
	IncludeAnalyzer, FileUtils, UndoUnit, Halda, HaldaTypes, BetterFindUnit, ErrorMessageWindow;


procedure DoSearchMenu (item: integer); {Menu handler}
procedure SearchWindInit;
function SearchFindAgain(backwards: Boolean): Boolean;
procedure DoSearchFindAgain(bw: Boolean);
function AbortFind: Boolean; // Try to abort if a search is in progress!

const
	kFind = 1;
	kEnterSelection = 3;
	kFindAgain = 4;
	kFindBackwards = 5;
	kReplaceAndFindAgain = 6;
	kFindInNextFile = 7;
	kReplaceAll = 99; // Not in menu any more

var
	searchWind: WindowPtr; // Export so Settings can access it.

implementation

uses
	Settings;

//	const
//		searchDialogRes = 130;
	var
		gFindText: AnsiString;
		gReplaceText: AnsiString;
//		gIgnoreCase, gEntireWord, gWrapFind, 
		gMultiFileFind: Boolean;
		multiFileFindType: Longint; // 1 = files ref by main, 2 = same folder as front, 3 = all open
//		searchDialog: DialogPtr;

		// Not yet supported:
		gBatchFind: Boolean;
		
		foundOffset: Longint;
		
		//for Multifile Search
		bufferFiles: array of AnsiString;
		totalNoOfFiles: Longint;
		fo, aStart,endchar: Longint;
		currentFile:Longint;
		fB: Boolean;
//		readOnlyFlag:Boolean;
		isOpenFlags: array of Boolean;
		multiFileRunning:Boolean;
		barWheelView: HIViewRef;
   
	var
	pBkgnd: TSNoArgProcPtr;
	gBackgroundTimer:  EventLoopTimerRef;		
	procedure MyBackgroundTimer(theTimer: EventLoopTimerRef; userData: Pointer); MWPascal;
	begin
		if pBkgnd <> nil then
			pBkgnd;
	end;

	procedure MyBackgroundWithDuration (p: TSNoArgProcPtr; intervalTime: EventTime);
	begin
		// Uninstall any previous timer
		if pBkgnd <> nil then
		if gBackgroundTimer <> nil then
		begin
			RemoveEventLoopTimer(gBackgroundTimer);
			gBackgroundTimer := nil;
		end;

		pBkgnd := p;
		if p <> nil then
		begin
			// Install timer!
			InstallEventLoopTimer (GetMainEventLoop(), intervalTime, intervalTime,
				MyBackgroundTimer, nil, gBackgroundTimer);
		end;
	end;
			
					
									
	procedure MyBackground (p: TSNoArgProcPtr);
	begin
		MyBackgroundWithDuration(p, kEventDurationSecond);
	end;

	procedure MyGetBackground (var p: TSNoArgProcPtr);
	begin
		p := pBkgnd;
	end;
	
	procedure DoBatchFind;
	var
		front: WindowPtr;	
		theSpec: FSSpecString;
		err: OSErr;
		tempfile, Bufferfile: Ansistring;
		
		rownumber, columnnumber, columnStart, j, k, editIndex: Longint;
//		tempBuff: AnsiString;

	begin
		ClearErrorMessages;
//		HelpWindInit;
		HIViewSetFrame( barWheelView, BarWidgetFrame(searchWind) );
		HIViewSetVisible(barWheelView, true);
		
		front := GetFrontEditWindow;
		if front <> nil then
			err := GetEditFSSpec(front, theSpec);
							
//		tempfile:= theSpec;
//		Bufferfile := ReadFileToString(tempfile, err, readOnlyFlag);
		editIndex := GetWRefCon(front);
		// If we found a window in the list, get data. Otherwise try to get it from disk.
		if (editIndex < 1) or (editIndex > kMaxEditWindows) then
			Bufferfile := ReadFileToString(theSpec, err)
		else
			Bufferfile := teEdit[editIndex]^.text;

			aStart:= 0;
			endChar:= Length(Bufferfile);
			fB:=false;
			
			rownumber:=1;
//			tempBuff:= Bufferfile;
			
			repeat
			begin
//				fO:=MyFind(Bufferfile, gFindText, aStart, endChar, fB, gIgnoreCase, gEntireWord);
				fO:=MyFind(Bufferfile, gFindText, aStart, endChar, fB, gSettings.searchIgnoreCase, gSettings.searchEntireWord);
//				writeln('tempBuff[fO] ', tempBuff[fO]);
				
				for j:= aStart to fO do
				begin
					if Bufferfile[j]= #13 then
					begin
						rownumber := rownumber+1;
						columnStart:= j+1;
					end;
				end;
//				writeln('rownumber', rownumber);
				columnnumber:=0;
				// UNNECESSARY FOR - remove when I can test
				for k:= columnStart to fO do
					columnnumber:=columnnumber+1;
					
//				writeln('columnnumber', columnnumber);
				
				if (fO>0) then
				StoreErrorMessage (5, rownumber, columnnumber, theSpec, '');
				
				aStart:=fO+ Length(gFindText);
			end;
			until (fO<1);
			
			HIViewSetVisible(barWheelView, false);

	end;
	

	function SearchFindAgain(backwards: Boolean): Boolean;
	var
		teEdit: HaldaPtr;
		searchString: AnsiString;
		startOffset, endOffset: Longint;
		
//		err: OSErr;
		//found: Boolean;
		tries, try: Integer;
		//dbStr: AnsiString;
  	begin
  		if gBatchFind then
		begin
			DoBatchFind;
			Exit(SearchFindAgain);
		end;
  	
		SearchFindAgain := false; {Nothing found yet}
		
		teEdit := EditGetCurrentTE;
		if teEdit = nil then Exit(SearchFindAgain); {Inte editfönster, skippa!}
		
//		WriteLn('SearchFindAgain');
		
		searchString := gFindText;
		if Length(searchString) = 0 then Exit(SearchFindAgain);
		HGetSelection(teEdit, startOffset, endOffset);
		
		// Wrap support
		tries := 1;
		if gSettings.searchWrap then tries := 2;
		try := 1;
		
		repeat
			if try = 2 then
			begin
				startOffset := 0; {Wrap}
				endOffset := 0;
//				WriteLn('Trying again');
			end;
			
//			WriteLn('Running MyFind');
//			foundOffset := MyFind(teEdit^.h^.text, searchString, startOffset, endOffset,
			if backwards then
			foundOffset := MyFind(teEdit^.text, searchString, startOffset, 0,
							backwards, gSettings.searchIgnoreCase, gSettings.searchEntireWord)
			else
			foundOffset := MyFind(teEdit^.text, searchString, endOffset, 0,
							backwards, gSettings.searchIgnoreCase, gSettings.searchEntireWord);
//			WriteLn('MyFind found ', foundOffset);
			
			// Wrap support, end part
			try := try + 1;
		until (foundOffset >= 0) or (try > tries);

		// If something was found, move there
		if foundOffset > 0 then
		begin
//			WriteLn('Found something!');
			HSetSelection(teEdit, foundOffset, foundOffset+Length(searchString));
			HShowSelection(teEdit^.views[teEdit^.focusedView], true); // For the FOCUSED one!
			HInvalViews(teEdit);
			{if gBatchFind then
				BatchFind;}
			
		end;
		
		SearchFindAgain := foundOffset > 0; {Something found}
	end;
	
	procedure SearchEnterSelection;
	var
		teEdit: HaldaPtr;
		startOffset, endOffset: Longint;
		//foundOffset: Longint;
		//err: OSErr;
	begin
		teEdit := EditGetCurrentTE;
		if teEdit = nil then Exit(SearchEnterSelection); {Inte editfönster, skippa!}
		
		HGetSelection(teEdit, startOffset, endOffset);
		if startOffset >= endOffset then Exit(SearchEnterSelection);
		gFindText := Copy(teEdit^.text, startOffset, endOffset - startOffset); // Ev +1
		
		VMSetStringValue(searchWind, 'Find', 0, gFindText);
//		SetTextDItem(searchDialog, 2, gFindText);
	end;
	
	procedure ReplaceSelected;
	var
		teEdit: HaldaPtr;
		startOffset, endOffset: Longint;
		editIndex: Longint;
//		err: OSErr;
	begin
//		gReplaceText := GetTextDItem(searchDialog, 4);
		gReplaceText := VMGetStringValue(searchWind, 'Repl', 0);
		
		editIndex := GetWRefCon(GetFrontEditWindow{FrontWindow});
		if editIndex < 1 then
			Exit(ReplaceSelected);
		teEdit := EditGetCurrentTE;
		if teEdit = nil then Exit(ReplaceSelected); {Inte editfönster, skippa!}
		
		HGetSelection(teEdit, startOffset, endOffset);
//		{err :=} TXNSetData(teEdit, kTXNTextData, @gReplaceText[1], Length(gReplaceText), startOffset, endOffset);
		SetTextUndoable(editIndex, startOffset, endOffset, gReplaceText);
	end;
	
	// Changed 2015-05-??, won't replace empty selection!
	procedure ReplaceAndFindAgain;
//	var
//		found: Boolean;
	var
		editIndex, startOffset, endOffset: Longint;
	begin
		editIndex := GetWRefCon(GetFrontEditWindow{FrontWindow});
		if editIndex < 1 then Exit(ReplaceAndFindAgain);
		HGetSelection(teEdit[editIndex], startOffset, endOffset);
		if endOffset = startOffset then
		begin
			Exit(ReplaceAndFindAgain);
		end;

		ReplaceSelected;
		{found :=} SearchFindAgain(false);
	end;
	
	procedure ReplaceAll;
	var
		found: Boolean;
	begin
		found := SearchFindAgain(false);
		if found then
			repeat
				ReplaceSelected;
				found := SearchFindAgain(false);
			until not found;
	end;
	
// Multi-file search

var
	gMultiFiles: FileArr;
//	gCurMultiFile: Longint;
	wasOpenFlags: array of Boolean;
	curEditIndex: Longint;
	
{procedure MultiFileNextFile;
//var
//	ext: Longint;
begin
	// Stäng förra om den inte var öppen innan!
	// Men bara om inte "dirty". TestIfDirty.
	if not isOpenFlags[gCurMultiFile] then
	if not TestIfDirty(curEditIndex) then
	begin
WriteLn('Klar med ', GetLastToken(gMultiFiles[gCurMultiFile]), ' och stänger den');
		
		SkelRmveWind(editWind[curEditIndex]); // anropar close
	end;
	
	// Stega fram till nästa
	gCurMultiFile := gCurMultiFile + 1;
	if gCurMultiFile >= Length(gMultiFiles) then
	begin
		WriteLn('DONE multi-file search');
		gCurMultiFile := -1; // Stopp! Och stäng sista om ingen träff.
		Exit(MultiFileNextFile);
	end;
	// Öppna den!
	
	// Är den öppen? I så fall lägg överst. Om inte, öppna den!
	isOpenFlags[gCurMultiFile] := FileIsOpen(gMultiFiles[gCurMultiFile]) <> 0;

if isOpenFlags[gCurMultiFile] then
	WriteLn(GetLastToken(gMultiFiles[gCurMultiFile]), ' redan öppen')
else
	WriteLn('Öppnar ', GetLastToken(gMultiFiles[gCurMultiFile]));
// BORDE SPARA MARKERINGEN och sätta den till start?
//	TXNGetSelection(teEdit[editIndex], );
	curEditIndex := OpenFileToWindow(gMultiFiles[gCurMultiFile]);
	HSetSelection(teEdit[curEditIndex], 0, 0); // 1, 1?
end;}
var
nextFile:Boolean;

procedure MultiFileFindAgain; forward;

Procedure MultiNextFile;
begin
	if currentFile< (totalNoOfFiles-1) then
	begin
		currentFile:=currentFile+1;
		aStart:=0;
		endChar:= Length(bufferFiles[currentFile]);
		fB:= false;
		nextFile:= true;
		//writeln('newFile1: ', nextFile);
		
		MultiFileFindAgain;
	end
	else
		multiFileRunning := false;
end;

procedure MultiFileNextFile;
begin
	MultiNextFile;
end;



procedure MultiFileFindAgain;
var
	teEdit1: HaldaPtr;
	f1, editIndex, editIndex1,editIndex2: Longint;
	front: WindowPtr;
	fileChanged: Boolean;
	theSpec: FSSpecString;
	i: longInt;
	
begin

	if (currentFile < Low(bufferFiles)) or (currentFile > High(bufferFiles)) then
	begin
		WriteLn('OUT OF BOUNDS ', currentFile);
		HelpAppendLn('OUT OF BOUNDS in MultiFileFindAgain');
		Exit(MultiFileFindAgain);
	end;

	//teEdit1 := EditGetCurrentTE;
	
	//Get editIndex of front window
	front := GetFrontEditWindow;
	editIndex1 := getWRefCon(front); //editIndex of front.
	
	// If file is open, search the window data, otherwise the on-disc data in bufferFiles.
	
	editIndex := FileIsOpen(gMultiFiles[currentFile]); //FileIsOpen returns editIndex of currentfile, if file is open else returns '0'.  
	fileChanged:=false;
	//WriteLn('currentFile: ', currentFile);
	//writeln('newFile2: ', newFile);
	
	//To check if the fron file is currentFile, else update front File as curentFile.  
	if not nextFile then // no need to check while nextFile.
		if editIndex>0 then
			if editIndex1 <> editIndex then
			begin
				//currentFile should be updated.
				//theSpec should be searched in all gMultiFiles of editMainFile, then assign to the currentFile with hit file.
				GetEditFSSpec(front, theSpec);
				if Length(gMultiFiles)>0 then
					for i := 0 to High(gMultiFiles) do
					begin
						if lowercase(theSpec) = lowercase(gMultiFiles[i]) then
						begin
							editIndex := editIndex1;
							currentFile := i;
							aStart:=0;
							endChar:= Length(bufferFiles[currentFile]);
							fB:= false;
							
							//writeln('i: ', i, ' **currentFile changed to**', currentFile);
							fileChanged:= true;
						end;
					end;	
			end;
	if nextFile then
		nextFile:=false;
		
		{if not fileChanged then
			editIndex:= editIndex2;}
			
	//get editIndex of teEdit1
	if editIndex > 0 then
		f1:= MyFind({bufferFiles[currentFile]}teEdit[editIndex]^.text, gFindText, aStart, endChar, fB, gSettings.searchIgnoreCase, gSettings.searchEntireWord)
	else
		f1:= MyFind(bufferFiles[currentFile], gFindText, aStart, endChar, fB, gSettings.searchIgnoreCase, gSettings.searchEntireWord);
	// Note: In Sreehari's code, he used OpenFileToWindow for every search, which would open ALL
	// files, needed or not. The old solution, opening if something is found, is better!
	//writeln('f1: ', f1);
	aStart:= f1 + Length(gFindText);
	if f1 < 0 then
	begin
		MultiNextFile;	
	end
	else
	begin
		//Open file if it is only isOpenFile<0.
		curEditIndex := OpenFileToWindow(gMultiFiles[currentFile]);
		if curEditIndex > 0 then
		begin
			HSetSelection(teEdit[curEditIndex], f1, f1+Length(gFindText));
			HShowSelection(teEdit[curEditIndex]^.views[teEdit[curEditIndex]^.focusedView], true); // For the FOCUSED one!
			HInvalViews(teEdit[curEditIndex]);
		end;
	end;
end;

// Corrected by Ingemar
var
	rownumber: Longint = 1;
	//By Sreehari 070416
	searchInitiateFlag:Boolean;
	
procedure continueSearch();
var
	columnnumber, columnStart, j, k: Longint;
	tempBuff: AnsiString;
	upTo: Longint;
const
	kSearchStepLength = 16384; // 256; // Something like 16k
begin
	//writeln('currentFile: ', currentFile);
	//writeln('High(bufferFiles): ', High(bufferFiles));
	if (currentFile < Low(bufferFiles)) or (currentFile > High(bufferFiles)) then
	begin
		WriteLn('OUT OF BOUNDS in continueSearch');
		HelpAppendLn('OUT OF BOUNDS in continueSearch');
		Exit(continueSearch);
	end;

// What if the file is being edited? We might search old data.
// Same as above? Switch to window data as needed. (Not fixed yet.)
//		editIndex := FileIsOpen(gMultiFiles[currentFile]);

// Only on new file!
// Done below for all but the first.
// This way we don't depend on detail inits from the outside!
	if searchInitiateFlag then //By Sreehari 070416
		if currentFile = Low(bufferFiles) then
		begin
			aStart:= 0;
			rownumber:=1;
			searchInitiateFlag:= false; //By Sreehari 070416
		end;

	upTo := aStart + kSearchStepLength;
	endChar:= Length(bufferFiles[currentFile]);
	tempBuff:= bufferFiles[currentFile]; // Wasteful
	
	repeat
		// Shouldn't this switch to editor data for open files?
		fO:=MyFind(bufferFiles[currentFile], gFindText, aStart, endChar, fB, gSettings.searchIgnoreCase, gSettings.searchEntireWord);
		for j:= aStart to fO do
		begin
			if tempBuff[j] in [#10, #13] then
			begin
				rownumber := rownumber+1;
				columnStart:= j+1;
			end;
		end;
		columnnumber := fO - columnStart + 1;
			
//				writeln('columnnumber', columnnumber);
	
		if (fO>0) then
			StoreErrorMessage (5, rownumber, columnnumber, gMultiFiles[currentFile], '');
		
		aStart:=fO + Length(gFindText);
		//writeln('fO: ', fO,' aStart: ', aStart, ' upTo: ', upTo, ' Length(bufferFiles[currentFile]): ', Length(bufferFiles[currentFile]));
	until (fO<1) or (aStart > upTo);
	if (fO < 1) or (aStart > Length(bufferFiles[currentFile])) then
	begin
		//writeln('**currentFile: ', currentFile);
		currentFile:=currentFile+1;
		aStart := 0;
		rownumber:=1;
		//writeln('l=', l, 'totalNoOfFiles=', totalNoOfFiles );
		if currentFile >= totalNoOfFiles then
		begin
			MyBackground(nil);
			WriteLn('DONE');
			currentFile := 0;   //By Sreehari 110416. currentFile is the global variable. When currentFile reached totalNoOfFiles, 
								//It should be reset in order to do start multifile search from start of the multi files.
			//SetWTitle(searchWind, 'Find and Replace');
			HIViewSetVisible(barWheelView, false);
			Exit(continueSearch);
		end;
	end;
end;

procedure DoMultiFileFind; //Gets files list
var
	front: WindowPtr;
	err: OSErr;
	theSpec: FSSpecString;
	i, offset, extType, editIndex: Longint;
	tempMultiFiles: FileArr;
begin
	ClearErrorMessages;
	//s:='Multi File Searching';
	//SetWTitle(searchWind, s);
//	HelpWindInit;
	HIViewSetFrame( barWheelView, BarWidgetFrame(searchWind) );
	if gBatchFind then HIViewSetVisible(barWheelView, true);
	
	//writeln('**multiFileFindType: ', multiFileFindType);
	//writeln('**gBatchFind: ', gBatchFind, ' multiFileRunning: ', multiFileRunning);

	//SetLength(bufferFiles, 0);
	// Build file list
	if (not gBatchFind) and multiFileRunning then
	begin
		MultiFileFindAgain;
		Exit(DoMultiFileFind);
	end;
	SetLength(bufferFiles, 0);
	SetLength(gMultiFiles, 0);
	SetLength(isOpenFlags, 0);
	SetLength(wasOpenFlags, 0);
	aStart:= 0;
	rownumber:=1; // For continueSearch
	case multiFileFindType of
		1: // All files in project
		begin
			//writeln('All files in project');
			front := GetFrontMainWindow;
			if front <> nil then
			begin
				err := GetEditFSSpec(front, theSpec);
				if err = noErr then
				begin
					tempMultiFiles := GetIncludeFileList(theSpec);
					if Length(tempMultiFiles) > 0 then
					begin
						// Tag med huvudfilen också!
						SetLength(tempMultiFiles, Length(tempMultiFiles)+1);
						tempMultiFiles[Length(tempMultiFiles)-1] := theSpec;
						//writeln('The Spec', theSpec);
						// Fast egentligen ville jag ha den först...

						SetLength(gMultiFiles, Length(tempMultiFiles));
						for i:= 0 to High(tempMultiFiles) do
						begin
							if i<1 then
								gMultiFiles[i]:= theSpec
							else
								gMultiFiles[i]:= tempMultiFiles[i-1];
								
							//writeln('gMultiFiles',gMultiFiles[i]);
						end;
 						SetLength(wasOpenFlags, Length(gMultiFiles));
						SetLength(isOpenFlags, Length(gMultiFiles));
						
					end;
//					gCurMultiFile := 0; // Index to current file
				end;
			end;
		end;
		2: // All files in same folder
		begin
			front := GetFrontEditWindow;
			if front <> nil then
			begin
				err := GetEditFSSpec(front, theSpec);
				//writeln('theSpec=== ', theSpec);
				if err = noErr then
				begin
					gMultiFiles := ListAllFilesOfFolderFSSpec(theSpec);
					if Length(gMultiFiles) > 0 then
					begin
						// Tag med huvudfilen också!
						// UNNECESSARY
//						SetLength(gMultiFiles, Length(gMultiFiles)+1);
//						gMultiFiles[Length(gMultiFiles)-1] := theSpec;
						// Fast egentligen ville jag ha den först...
						
						SetLength(wasOpenFlags, Length(gMultiFiles));
						SetLength(isOpenFlags, Length(gMultiFiles));
					end;
					
//					gCurMultiFile := 0; // Index to current file
				end;
			end;
		end;
		3: // All open files
		begin
			// Lätt - sök igenom fileSpec och samla alla som är boundToFile.
			// Dessa är lokala i LWPEdit.p. Anrop för att hämta lista?
			gMultiFiles := GetAllEditWindows;
		
			SetLength(wasOpenFlags, Length(gMultiFiles));
			SetLength(isOpenFlags, Length(gMultiFiles));
//			gCurMultiFile := 0; // Index to current file
		end;
		otherwise
//			gCurMultiFile := -1;
	end;

//WriteLn('*** Startar multi-file search! Lista för obj-rens; ***');
//for i := 0 to High(gMultiFiles) do
	//WriteLn(GetLastToken(gMultiFiles[i]));
//WriteLn('*** Lista slut ***');
	
	// Loop through list, remove binary files (fix 110219) (fixed again 140923)
	offset := 0;
	for i := 0 to High(gMultiFiles) do
	begin
		extType := GetExtensionType(gMultiFiles[i]);
		if (extType = kExtTypeObj) or (extType = kExtTypeUnknown) then
		begin
			//WriteLn('Skipped: ', gMultiFiles[i], ' type ', extType);
			offset := offset + 1;
		end
		else
		begin
			//WriteLn('Not skipped: ', gMultiFiles[i], ' type ', extType);
			gMultiFiles[i - offset] := gMultiFiles[i];
		end;
//		if i+offset <= High(gMultiFiles) then
//			gMultiFiles[i - offset] := gMultiFiles[i];
	end;
	SetLength(gMultiFiles, Length(gMultiFiles) - offset);
				
	// Lists done, do search
	{if gCurMultiFile >= 0 then
	begin
WriteLn('*** Startar multi-file search! Lista; ***');
for i := 0 to High(gMultiFiles) do
	WriteLn(GetLastToken(gMultiFiles[i]));
WriteLn('*** Lista slut ***');

		curEditIndex := OpenFileToWindow(gMultiFiles[gCurMultiFile]);
		MultiFileFindAgain;
	end;}
	//for i := 0 to High(gMultiFiles) do
		//WriteLn(GetLastToken(gMultiFiles[i]));
	WriteLn('*** Lista slut ***');

	if Length(gMultiFiles) = 0 then
	begin
		MessageAlert('No files found to search', 'Check the multi-find seach options');
		HIViewSetVisible(barWheelView, false);
		Exit(DoMultiFileFind);
	end;
	
	SetLength(bufferFiles, Length(gMultiFiles));
	
	for i := 0 to High(gMultiFiles) do
	begin
		editIndex := FileIsOpen(gMultiFiles[i]); // 
		isOpenFlags[i] := editIndex <> 0; //isOpenFlags is true If file is open; 
		
			if isOpenFlags[i] then
				bufferFiles[i]:= teEdit[editIndex]^.text
			else
			begin
				bufferFiles[i]:= ReadFileToString(gMultiFiles[i], err);
				//writeln('Files not open are', gMultiFiles[i]);
			end;	
	end;
	
	for i := 0 to High(gMultiFiles) do
	begin
		//writeln(gMultiFiles[i]);
		//writeln('isOpenFlags[i]=', isOpenFlags[i]);
	end;
	
	totalNoOfFiles:= Length(bufferFiles);
	currentFile:=0;
	fB:= false;
	if not gBatchFind then
	begin
		writeln('Multifile Search Selected');
		//SearchFindAgain(false);
		multiFileRunning := true;
		MultiFileFindAgain;
	end
	else
	begin
		searchInitiateFlag:=true; //By Sreehari 070416
		MyBackgroundWithDuration(@continueSearch, kEventDurationSecond / 10);
	end;
end;



 
// Vid tryck på knappen, starta multi-file om det är valt!
procedure DoSearchFindAgain(bw: Boolean);
begin
	if not gMultiFileFind then
		SearchFindAgain(bw)
	else
	begin
		// Check if multiFileRunning should be reset?
		// Changed search word?
		// Changed main file?
		
		DoMultiFileFind;
	end;
	SelectWindow(searchWind); // Keep dialog in front
end;

// Added 160418: When Find or Enter selection are performed,
// Multi-file search and batch find are turned off.
// I find myself doing multi-file searched by accident so
// maybe this will feel right.
procedure DisableMulti;
begin
	VMSetBooleanValue(searchWind, 'Mult', 0, false);
	VMSetBooleanValue(searchWind, 'Batc', 0, false);
end;

procedure DoSearchMenu (item: integer);
var
	control: ControlRef;
begin
	case item of
		kFind:
		begin
// Disable "Multi-file search" and "Batch find"! (I think this makes sense!)			
			DisableMulti;
			
//			WriteLn('kFind 1');
			ShowWindow(searchWind);
//			WriteLn('kFind 1.1');
			SelectWindow(searchWind);
			//SelectDialogItemText(searchDialog, 2, 0, 9999);
//			WriteLn('kFind 2');
			VMGetControl( searchWind, 'Find', 0, control);
			SetKeyboardFocus(searchWind, control, kControlFocusNoPart);
			SetKeyboardFocus(searchWind, control, kControlFocusNextPart);
	
//			WriteLn('kFind 3');
		end;
		kEnterSelection:
		begin
// Disable "Multi-file search" and "Batch find"! (I think this makes sense!)			
			DisableMulti;
			
			SearchEnterSelection;
		end;
		kFindAgain:
			
			//writeln('gMultiFileFind', gMultiFileFind);
			//writeln('gBatchFind', gBatchFind);
			
			if not gMultiFileFind then
				SearchFindAgain(false)
			else
//				MultiFileFindAgain;
// Must do DoMultiFileFind if not initialized!
			if multiFileRunning then
				MultiFileFindAgain
			else
				DoMultiFileFind;
		
			{if gMultiFileFind then
				MultiFileFindAgain
			else if gBatchFind then
				DoBatchFind
			else
				SearchFindAgain(false);}

		kFindBackwards:
			if not gMultiFileFind then
				SearchFindAgain(true);
				
//			else
//				MultiFileFindAgain;
		kReplaceAndFindAgain:
			ReplaceAndFindAgain;
		kReplaceAll: // Not in menu
			ReplaceAll;
		kFindInNextFile:
		if gMultiFileFind then
		begin
			MultiFileNextFile;
			//MultiFileFindAgain; {?}  //By Sreehari 110416 commented as It is causing to skip one search word.
		end;
		otherwise
	end;
end;

procedure DoClose;
var
	thePort: GrafPtr;
begin
	GetPort(thePort);			{ grafport of window to be closed }
	HideWindow(GetWindowFromPort(thePort));
end;

function FindFilter(nextHandler: EventHandlerCallRef; inEvent: EventRef; inUserData: Pointer ):OSStatus; MWPascal;
var
//	eventClass: UInt32;
//	eventKind: UInt32;
	key: Char;
//	err: OSErr;
//	front: WindowPtr;
begin
	//WriteLn('FindFilter');
//	eventClass := GetEventClass(inEvent);
//	eventKind  := GetEventKind(inEvent);
	
	{err :=} GetEventParameter (inEvent, kEventParamKeyMacCharCodes,
					typeChar, nil, SizeOf(Char), nil, @key);
	
	if key = Char(13) then
	begin
		DoSearchFindAgain(false); // Same as click on button
//		if SearchFindAgain(false) then
//		begin
//			// Bring edit window to front
//			front := GetFrontEditWindow;
//			if front <> nil then SelectWindow(front);
//		end;
		return noErr;
	end
	else
	begin
		CallNextEventHandler(nextHandler, inEvent);
		return eventNotHandledErr;
	end;
end;

function FoldUnfoldMulti(theView: HIViewRef; myPtr: Pointer): Boolean;
begin
// Folded size: 340x180
// Unfolded size: 340x265
// New for 0.9.8:
// Folded size: 340x209
// Unfolded size: 340x320
	//WriteLn('FoldUnfoldMulti');
	if gMultiFileFind then
		SizeWindow(searchWind, 340, 284, true)
	else
		SizeWindow(searchWind, 340, 209, false);
		
	return true; // Meaningless function result? Not handled in VM?
end;

function AbortFind: Boolean;
var
	p: TSNoArgProcPtr;
begin
	AbortFind := false;
//	if FrontWindow() = searchWind then
		if gMultiFileFind then
		begin
			if multiFileRunning then // multi-file search without batch
			begin
				multiFileRunning := false;
				AbortFind := true;
			end
			else
			begin
				MyGetBackground(p);
				if p <> nil then // something is running
				begin
					MyBackground(nil); // Turn off batch find timer.
					HIViewSetVisible(barWheelView, false);
					AbortFind := true;
				end;
			end;
		end;
//		else
//			WriteLn('Not aborted since not multi');
//	else
//		WriteLn('Not aborted since dialog not in front');
end;

procedure Activate(active: Boolean);
var
//	dummy: Boolean;
	control, ctrl, subcontrol: ControlRef;
	w: WindowPtr;
	title: Str255;
begin
	if active then
	begin
		w := GetFrontMainWindow;
		GetWTitle(w, title);
		
		VMGetControl( searchWind, 'Radi', 0, control);
		GetIndexedSubControl(control, 1, subcontrol);
		if Length(title) > 0 then
			VMSetStringValue(subcontrol, 'Find in project "'+ title+ '"');
		
		w := GetFrontEditWindow;
		GetWTitle(w, title);
		
		GetIndexedSubControl(control, 2, subcontrol);
		if Length(title) > 0 then
			VMSetStringValue(subcontrol, 'Find in folder of "'+ title+ '"');
	end;
end;

procedure SearchWindInit;
var
//	dummy: Boolean;
	control, ctrl, subcontrol: ControlRef;
	r: MacOSAll.Rect;
	root: HIViewRef;
	
	outNumChildren: UInt16;
begin
	CreateWindowFromNib(SkelGetMainNib, CFSTR('Find'), searchWind);
	SkelWindow(searchWind, nil, nil, nil, @Activate, @DoClose, nil, nil, false);

// These are now installed by Settings!
//	InstallBooleanViewHandler(searchWind, 'Igno', 0, gIgnoreCase, nil);
//	InstallBooleanViewHandler(searchWind, 'Enti', 0, gEntireWord, nil);
//	InstallBooleanViewHandler(searchWind, 'Wrap', 0, gWrapFind, nil);

	InstallBooleanViewHandler(searchWind, 'Mult', 0, gMultiFileFind, @FoldUnfoldMulti);

// New, not yet handled:
// Batc: Batch find
// BatM: Batch multi-file
	InstallBooleanViewHandler(searchWind, 'Batc', 0, gBatchFind, nil);
//	InstallBooleanViewHandler(searchWind, 'Batm', 0, gBatchMultiFind, nil);

	multiFileFindType := 1;
	InstallViewHandler(searchWind, 'Radi', 0, kViewDataLongint, @multiFileFindType, nil);

	{InstallTextViewHandler}
//	InstallTextViewHandler(searchWind, 'Find', 0, gFindText, @HitControl);
//	InstallTextViewHandler(searchWind, 'Repl', 0, gReplaceText, @HitControl);
	
	VMGetControl( searchWind, 'Find', 0, control);
	InstallViewHandlerByRef(searchWind, control, kViewDataString, @gFindText, nil, FindFilter);

	SetKeyboardFocus(searchWind, control, kControlFocusNoPart);
	SetKeyboardFocus(searchWind, control, kControlFocusNextPart);

	VMGetControl( searchWind, 'Repl', 0, control);
	InstallViewHandlerByRef(searchWind, control, kViewDataString, @gReplaceText, nil, FindFilter);

	VMGetControl( searchWind, 'Radi', 0, control);
	GetIndexedSubControl(control, 1, subcontrol);
//	WriteLN(VMGetStringValue(subcontrol));
	VMSetStringValue(subcontrol, 'Find in project files');
	
	GetIndexedSubControl(control, 2, subcontrol);
	//WriteLN(VMGetStringValue(subcontrol));
	VMSetStringValue(subcontrol, 'Find in files in the same folder');
	
//	GetIndexedSubControl(control, 3, subcontrol);
	//WriteLN(VMGetStringValue(subcontrol));
//	VMSetStringValue(subcontrol, 'Find in all open files');

{
	searchDialog := GetNewDialog(searchDialogRes, nil, WindowPtr(-1));
	if searchDialog = nil then DebugStr('ERROR: Could not create find dialog');
	
	searchWind := GetDialogWindow(searchDialog);
//	dummy := SkelDialog(searchDialog, @Event, nil, @Halt, @Filter);
	dummy := SkelDialog(searchDialog, @Event, nil, nil, @Filter);

// Should be saved in settings!
	gIgnoreCase := true;
	gEntireWord := false;
	gWrapFind := false;
	SetBooleanDItem(searchDialog, 7, gIgnoreCase);
	SetBooleanDItem(searchDialog, 8, gEntireWord);
	SetBooleanDItem(searchDialog, 9, gWrapFind);
}
		
		root := HIViewGetRoot( searchWind );
		MacOSAll.SetRect(r, 0, 0, 16, 16);
		CreateChasingArrowsControl(nil, r, barWheelView);
		HIViewSetVisible(barWheelView, false);
		HIViewAddSubview( root, barWheelView );
		HIViewSetFrame( barWheelView, BarWidgetFrame(searchWind) );
end;

end.

{$mode macpas}
unit ErrorMessageWindow;
interface
	uses
		MacOSAll, TransSkel4, LWPGlobals, LWPEdit, Console,
		FileUtils, QDCG, QDCGPictUnit;

	procedure ClearErrorMessages;
	procedure ParseErrorMessage(msg: AnsiString);
	procedure ParseGCCErrorMessage(msg: AnsiString);
	procedure ErrorMsgWindInit;

	procedure StoreErrorMessage (errorNum, rowNumber, columnNumber: Longint; fileName, message: Str255);
	
	procedure DoErrorEditMenu(item: Integer);

implementation

	var
		//theErrorMsgWind: WindowPtr;
		errLine, halfPage: integer;	{ line currently at top of window }
		errorMenu: MenuRef;
	
	const
		theErrorMsgWindRes = 1002;

	type
		ErrorMessagePtr = ^ErrorMessageRec;
		ErrorMessageRec = record
				theError, rowNumber, columnNumber: Longint;
				fileName, message: Str255;
				next: ErrorMessagePtr;
			end;
	const
		kMaxIcon = 5; {Number of bug reporting icons!}
		kError = 1;
		kFatal = 2;
		kWarning = 3;
		kNote = 4;
		kSearch = 5;
	var
		gErrorMessageList, gErrorDisplayList: ErrorMessagePtr;
		gSelectedMessage: Longint;
		gWhenClicked: Longint;
		errScroll: ControlHandle;	{ error window scroll bar }
		//gMaxErrorLevel: Longint; // 2=enbart error, 3=error och varningar, 4=alla
		showError: array[1..kMaxIcon] of Boolean;
		
		//bugIcon: array[1..kMaxIcon] of CIconHandle;
		PicIcon: Array [1..kMaxIcon] of PicHandle;

		
	const
		kSpacing = 25; // Höjd på varje felmeddelande
		
	// This flag is set when FPC or GCC reports
	// "/usr/bin/ld: Undefined symbols:"
	var
		expectsLinkError: Boolean;
	
	// Counters added in 0.9.1
		errCount, warningCount, noteCount, beepCount, searchCount: Longint;
	const
		kMaxErrors = 20;
		kMaxWarnings = 5;
		kMaxNotes = 50;
		kMaxBeeps = 10;
		kSearchMessages = 50;

//	procedure WindUpdate (resized: Boolean);forward;
	procedure RecalcScrollbar; forward;

	
	procedure CopyToDisplay(origMsg: ErrorMessagePtr);
	var
		copyMsg, aMsg: ErrorMessagePtr;
	begin
		if origMsg <> nil then
		if origMsg^.theError >= 1 then
		if origMsg^.theError <= kMaxIcon then
		if showError[origMsg^.theError] {< gMaxErrorLevel} then
		begin
			// Make copy
			copyMsg := ErrorMessagePtr(NewPtrClear(SizeOf(ErrorMessageRec)));
			BlockMoveData(origMsg, copyMsg, SizeOf(ErrorMessageRec));
			// MoveBytes
			copyMsg^.next := nil; // Important! Otherwise we may crosslink to master list!
			
			// Länka in sist
			if gErrorDisplayList = nil then
				begin
					gErrorDisplayList := copyMsg;
				end
			else
				begin
					aMsg := gErrorDisplayList;
					while aMsg^.next <> nil do
					begin
						aMsg := aMsg^.next;
					end;
					aMsg^.next := copyMsg;
				end;
		end;
	end;
	
	procedure RecalcDisplayList;
	var
		aMsg, deleteMe: ErrorMessagePtr;
		windBounds: MacOSAll.Rect;
//		err: OSErr;
	begin
		// Töm gErrorDisplayList
		aMsg := gErrorDisplayList;
		while aMsg <> nil do
			begin
				deleteMe := aMsg;
				aMsg := aMsg^.next;
				DisposePtr(Ptr(deleteMe));
			end;
		gErrorDisplayList := nil;
		
		// Kopiera från gErrorMessageList
		aMsg := gErrorMessageList;
		while aMsg <> nil do
			begin
				CopyToDisplay(aMsg);
				aMsg := aMsg^.next;
			end;
		// InvalRect
		windBounds := GetWindowPortBounds(theErrorMsgWind, windBounds)^;
//		{err :=} InvalWindowRect(theErrorMsgWind, windBounds); {InvalRect(r);}
		HIViewSetNeedsDisplay(SkelGetContentView(theErrorMsgWind), true);
		RecalcScrollBar; // Behövs den?
	end;

	function GetDisplayCount: Longint;
	var
		aMsg: ErrorMessagePtr;
		count: Longint;
	begin
		count := 0;
		aMsg := gErrorDisplayList;
		while aMsg <> nil do
			begin
				count := count + 1;
				aMsg := aMsg^.next;
			end;
		return count;
	end;
	
procedure DoBeep;
begin
	if beepCount < kMaxBeeps then
	begin
		beepCount += 1;
		SysBeep(1);
	end;
end;

	procedure StoreErrorMessage (errorNum, rowNumber, columnNumber: Longint; fileName, message: Str255);
		var
			aMsg, errMsgPtr: ErrorMessagePtr;
			windBounds, r: MacOSALL.Rect;
//			err: OSErr;
			count, visLines: Longint;
			savePort: GWorldPtr;
	begin
{Spara i en lista}
		WriteLn('StoreErrorMessage');
		
		case errorNum of
			kError, kFatal:
			begin
				if errCount > kMaxErrors then
					Exit(StoreErrorMessage);
				errCount += 1;
			end;
			kWarning:
			begin
				if warningCount > kMaxWarnings then
					Exit(StoreErrorMessage);
				
				if Pos('deprecated',message) > 0 then // if "deprecated" in message
					Exit(StoreErrorMessage);
				warningCount += 1;
			end;
			kNote:
			begin
				if noteCount > kMaxNotes then
					Exit(StoreErrorMessage);
				noteCount += 1;
			end;
			kSearch:
			begin
				Writeln('Store Error Messages for search');
//				if searchCount> kSearchMessages then
//					Exit(StoreErrorMessage);
				searchCount +=1;
						WriteLn('StoreErrorMessage ', searchCount);
			end;
		end;

{Skapa record och fyll i}
		errMsgPtr := ErrorMessagePtr(NewPtrClear(SizeOf(ErrorMessageRec)));
		errMsgPtr^.theError := errorNum;
		errMsgPtr^.rowNumber := rowNumber;
		errMsgPtr^.columnNumber := columnNumber;
		errMsgPtr^.fileName := fileName;
		errMsgPtr^.message := message;
		errMsgPtr^.next := nil;

{Länka in SIST}
		
		count := 0; // ???
		if gErrorMessageList = nil then
			begin
				gErrorMessageList := errMsgPtr;
//				count := 1;
			end
		else
			begin
//				count := 0;
				aMsg := gErrorMessageList;
				while aMsg^.next <> nil do
				begin
					aMsg := aMsg^.next;
					count := count + 1;
				end;
				aMsg^.next := errMsgPtr;
//				count := count + 1; // Man vill väl räkna upp för den sista också?
			end;
		
// Länk även in i gErrorDisplayList!
		CopyToDisplay(errMsgPtr);
	
		GetPort(savePort); // ???
		SetPortWindowPort(theErrorMsgWind);
		windBounds := GetWindowPortBounds(theErrorMsgWind, windBounds)^;
		SetRect(r, 0, count * kSpacing, windBounds.right, (count + 1) * kSpacing);
//		InvalWindowRect(theErrorMsgWind, r); {InvalRect(r);}
		// Uppdatera genast?
		HIViewSetNeedsDisplay(SkelGetContentView(theErrorMsgWind), true);
//		WindUpdate(false);
		//QDFlushPortBuffer(GetWindowPort(theErrorMsgWind), nil);
		SetPort(savePort);
		
		// Update scroll bar value
		r := GetWindowPortBounds(theErrorMsgWind, r)^;
		visLines := (r.bottom - r.top) div kSpacing;
// FEL - count är inte giltig här!
		count := GetDisplayCount;
		if count - visLines > 0 then // Doesn't fit in window
			SetControl32BitMaximum(errScroll, count - visLines)
		else
			SetControl32BitMaximum(errScroll, 0);
		
		RecalcScrollbar;
		
		// 081030: Bring error window to front on errors
		if errorNum in [kError, kFatal] then
			SelectWindow(theErrorMsgWind);
		ShowWindow(theErrorMsgWind);
WriteLn('StoreErrorMessage done');
if gErrorMessageList = nil then
	WriteLn('StoreErrorMessage exiyts with no list!');
	end;

	procedure ClearErrorMessages;
		var
			aMsg, deleteMe: ErrorMessagePtr;
			//windBounds: Rect;
			//err: OSErr;
	begin
		//gSelectedMessage := -1;
		aMsg := gErrorMessageList;
		while aMsg <> nil do
			begin
				deleteMe := aMsg;
				aMsg := aMsg^.next;
				DisposePtr(Ptr(deleteMe));
			end;
		gErrorMessageList := nil;
		
		RecalcDisplayList; // Tömmer även denna
		
		SetControl32BitValue(errScroll, 0);
		SetControl32BitMaximum(errScroll, 0);
		halfPage := 0;
		gSelectedMessage := 0;
		gSelectedMessage := -1;
		
		expectsLinkError := false;
		
		// 0.9.1: counters to limit number of messages
		errCount := 0;
		warningCount := 0;
		noteCount := 0;
		beepCount := 0;
		searchCount:=0;
		
		// InvalRect
		//windBounds := GetWindowPortBounds(theErrorMsgWind, windBounds)^;
		//err := InvalWindowRect(theErrorMsgWind, windBounds); {InvalRect(r);}
		HIViewSetNeedsDisplay(SkelGetContentView(theErrorMsgWind), true);
		
		HideWindow(theErrorMsgWind);
	end;
	
	procedure ParseGCCErrorMessage(msg: AnsiString);
	var
		i, start: Integer;
		firstToken, secondToken, thirdToken, fourthToken, fifthToken: String;
		rowNumber, columnNumber: Longint;
	begin
		msg := UTF8ToMacRoman(msg);
		
		// NEW 0.8.0: Report link errors
		if msg = '/usr/bin/ld: Undefined symbols:' then
		begin
			expectsLinkError := true;
//			StoreErrorMessage(kError, 1, 0, '', 'LINK ERROR DETECTED TEST 1');
		end;
		
		i := 1;
		if Length(msg) = 0 then Exit(ParseGCCErrorMessage);
		
//		while not (msg[i] in [':']) and (i < Length(msg)) do
		while not (msg[i] in [':', '(']) and (i < Length(msg)) do
			i := i + 1;
			firstToken := Copy(msg, 1, i - 1);

// ":" most likely. CUDA will send "(".
		
		if i >= Length(msg) then
		begin
			DoBeep;
			if expectsLinkError then
			begin
				StoreErrorMessage(kError, 1, 0, '', 'Undefined symbol: ' + msg);
				Exit(ParseGCCErrorMessage);
			end
			else
			begin
				Exit(ParseGCCErrorMessage);
			end;
		end;
		i := i + 1;
		if not (msg[i] in ['0'..'9']) then
			if expectsLinkError then // Should not catch that here
			begin
//				StoreErrorMessage(kError, 1, 0, '', 'Link error: ' + msg);
				Exit(ParseGCCErrorMessage);
			end
			else
			begin
				Exit(ParseGCCErrorMessage);
			end;
//			Exit(ParseGCCErrorMessage);

// If we make it here, it can not be a link error.
		expectsLinkError := false;
		
		start := i;
		while (msg[i] in ['0'..'9']) and (i < Length(msg)) do
			i := i + 1;
		secondToken := Copy(msg, start, i - start); {Radnummer}
		
		if i >= Length(msg) then
			Exit(ParseGCCErrorMessage);
		if not (msg[i] in [':', ')']) then // ')' for CUDA
			Exit(ParseGCCErrorMessage);
		
		i := i + 1;
		if msg[i] in ['0'..'9'] then
		begin
			start := i;
			while (msg[i] in ['0'..'9']) and (i < Length(msg)) do
 				i := i + 1;
				thirdToken := Copy(msg, start, i - start); {Pos på raden}
			end
		else
			thirdToken := '0';
		
		start := i + 1;
		i := i + 1;
		while not (msg[i] in [':']) and (i < Length(msg)) do
			i := i + 1;
			
		if i < Length(msg) then
		begin
			fourthToken := Copy(msg, start, i - start); {msg type}	
			fifthToken := Copy(msg, i + 1, Length(msg) - i);
		end
		else
		begin // Ada
			fourthToken := 'error';
			fifthToken := Copy(msg, start, Length(msg) - start+1);
		end;
		
		StringToNum(secondToken, rowNumber);
		StringToNum(thirdToken, columnNumber);
		if (fourthToken = ' error') or (fourthToken = 'error') then
		begin
			StoreErrorMessage(kError, rowNumber, columnNumber, firstToken, fifthToken);
		end
		else if (fourthToken = ' warning') or (fourthToken = 'warning') then
		begin
			StoreErrorMessage(kWarning, rowNumber, columnNumber, firstToken, fifthToken);
		end
		else if (fourthToken = ' note') or (fourthToken = 'note') then
		begin
			StoreErrorMessage(kNote, rowNumber, columnNumber, firstToken, fifthToken);
		end
		else
		begin
			StoreErrorMessage(kError, rowNumber, columnNumber, firstToken, '"'+fourthToken+'"' + fifthToken); // Needed for Ada!
		end;
		
//		WriteLn('1:', firstToken);
//		WriteLn('2:', secondToken);
//		WriteLn('3:', thirdToken);
//		WriteLn('4:', fourthToken);
//		WriteLn('5:', fifthToken);
		
	end;

(*
	procedure ParseGCCErrorMessage(msg: AnsiString);
	var
		i, start: Integer;
		firstToken, secondToken, thirdToken, fourthToken, fifthToken: Str255;
		rowNumber, columnNumber: Longint;
	begin
		i := 1;
		if Length(msg) = 0 then Exit(ParseGCCErrorMessage);

		while not (msg[i] in [':']) and (i < Length(msg)) do
			i := i + 1;
			firstToken := Copy(msg, 1, i - 1);

		if i >= Length(msg) then
			Exit(ParseGCCErrorMessage);
		i := i + 1;
		if not (msg[i] in ['0'..'9']) then
			Exit(ParseGCCErrorMessage);
		
		start := i;
		while (msg[i] in ['0'..'9']) and (i < Length(msg)) do
			i := i + 1;
		secondToken := Copy(msg, start, i - start); {Radnummer}
		
		if i >= Length(msg) then
			Exit(ParseGCCErrorMessage);
		if not (msg[i] in [':']) then
			Exit(ParseGCCErrorMessage);
		
		i := i + 1;
		if msg[i] in ['0'..'9'] then
		begin
			start := i;
			while (msg[i] in ['0'..'9']) and (i < Length(msg)) do
 				i := i + 1;
				thirdToken := Copy(msg, start, i - start); {Pos på raden}
			end
		else
			thirdToken := '1';
		
		start := i + 1;
		i := i + 1;
		while not (msg[i] in [':']) and (i < Length(msg)) do
			i := i + 1;
		fourthToken := Copy(msg, start, i - start); {Pos på raden}
		
		fifthToken := Copy(msg, i + 1, Length(msg) - i);
		
		StringToNum(secondToken, rowNumber);
		StringToNum(thirdToken, columnNumber);
		if (fourthToken = ' error') or (fourthToken = 'error') then
		begin
			StoreErrorMessage(kError, rowNumber, columnNumber, firstToken, fifthToken);
		end
		else if (fourthToken = ' warning') or (fourthToken = 'warning') then
		begin
			StoreErrorMessage(kWarning, rowNumber, columnNumber, firstToken, fifthToken);
		end
		else
			StoreErrorMessage(kError, rowNumber, columnNumber, firstToken, fifthToken); // Needed for Ada!
		
//		HelpAppendLn(firstToken);
//		HelpAppendLn(secondToken);
//		HelpAppendLn(thirdToken);
//		HelpAppendLn(fourthToken);
//		HelpAppendLn(fifthToken);
		
	end;
*)

var
	nowCompiling: AnsiString;

	procedure ParseErrorMessage(msg: AnsiString); // Var Str255 - alla strängar borde väl vara AnsiString?!
		var
			i, start: Integer;
			firstToken, secondToken, thirdToken, fourthToken, fifthToken: Str255;
			rowNumber, columnNumber: Longint;
	begin
		// NEW 0.8.0: Report link errors
		if msg = '/usr/bin/ld: Undefined symbols:' then
		begin
			expectsLinkError := true;
			Exit(ParseErrorMessage);
		end;

//		WriteLn('ParseErrorMessage parsing "', msg, '"');
		if Length(msg) = 0 then Exit(ParseErrorMessage);
		
{Search for first space OR start parenthesis.}
		i := 1;
		while not (msg[i] in [' ', '(']) and (i < Length(msg)) do
			i := i + 1;
		firstToken := Copy(msg, 1, i - 1);
		
		if i >= Length(msg) then
			if expectsLinkError then
			begin
				StoreErrorMessage(kError, 1, 0, nowCompiling, 'Undefined symbol: ' + msg);
				Exit(ParseErrorMessage);
			end;
		
{Check for known first keywords.}
		if firstToken = 'Compiling' then
			begin
				secondToken := Copy(msg, i + 1, Length(msg) - i);
				//WriteLn('Found "compiling" of ', secondToken);
				nowCompiling := secondToken; // Saved for later!
			end
		else if firstToken = 'Target' then
			begin
				//WriteLn('Found "Target"');
			end
		else if firstToken = 'Copyright' then
			begin
				//WriteLn('Found "Copyright"');
			end
		else if firstToken = 'Free' then
			begin
				//WriteLn('Found "Free"');
			end
		else if firstToken = 'Assembling' then
			begin
				secondToken := Copy(msg, i + 1, Length(msg) - i);
				// WriteLn('Found "assembling" of ', secondToken);
			end
		else if firstToken = 'Linking' then
			begin
				secondToken := Copy(msg, i + 1, Length(msg) - i);
				// WriteLn('Found "linking" of ', secondToken);
			end
		// NEW in 0.8.0: Error without specific file
		else if firstToken = 'Error:' then
			begin
				secondToken := Copy(msg, i + 1, Length(msg) - i);
				StoreErrorMessage(kError, 1, 0, nowCompiling, secondToken);
			end
		// NEW in 0.8.0: Fatal error
		else if firstToken = 'Fatal:' then
			begin
				secondToken := Copy(msg, i + 1, Length(msg) - i);

				StoreErrorMessage(kError, 1, 0, nowCompiling, secondToken);
			end
		else
			begin
{Annat, troligen felmeddelande. Sök till startparentes om vi inte redan är där}
				while not (msg[i] in ['(']) and (i < Length(msg)) do
					i := i + 1;
				firstToken := Copy(msg, 1, i - 1); {Hela namnet fram till parentesen - även om det finns mellanslag}
												{(är väl inte tillåtet men ändå)}
				start := i + 1;
				while not (msg[i] in [',']) and (i < Length(msg)) do
					i := i + 1;
				secondToken := Copy(msg, start, i - start); {Radnummer}
				start := i + 1;
				while not (msg[i] in [')']) and (i < Length(msg)) do
					i := i + 1;
				thirdToken := Copy(msg, start, i - start); {Position på raden}
				i := i + 2;
				start := i; {i är på slutparentes. Hoppa fram förbi parentes och mellanslag}
				while not (msg[i] in [' ']) and (i < Length(msg)) do
					i := i + 1;
				fourthToken := Copy(msg, start, i - start - 1); {-1 för att slippa kolonet}
				fifthToken := Copy(msg, i + 1, Length(msg) - i);

//				WriteLn('Found message: "', fourthToken, '" stating "', fifthToken, '" at ', secondToken, ' ', thirdToken);
//				WriteLn('Radnummer: "', secondToken, '"');
//				WriteLn('Radposition: "', thirdToken, '"');
//				WriteLn('Filnamn: "', firstToken, '"');
				StringToNum(secondToken, rowNumber);
				StringToNum(thirdToken, columnNumber);

				if fourthToken = 'Error' then
					begin
						StoreErrorMessage(kError, rowNumber, columnNumber, firstToken, fifthToken);
					end
				else if fourthToken = 'Fatal' then
					begin
						StoreErrorMessage(kFatal, rowNumber, columnNumber, firstToken, fifthToken);
					end
				else if fourthToken = 'Warning' then
					begin
						StoreErrorMessage(kWarning, rowNumber, columnNumber, firstToken, fifthToken);
					end
				else if fourthToken = 'Note' then
					begin
						StoreErrorMessage(kNote, rowNumber, columnNumber, firstToken, fifthToken);
					end;
				{Formatet stämde inte. Då är det nåt okänt som vi ignorerar!}
			end;
//WriteLn('ParseErrorMessage done');
	end;
{Free Pascal Compiler version 2.0.4-rc3 [2006/08/13] for powerpc}
{Copyright (c) 1993-2006 by Florian Klaempfl}
{Target OS: Darwin for PowerPC}
{Compiling LillHelp.p}
{LillHelp.p(141,12) Note: Local variable "ignore" is assigned but never used}
{LightWeightPascalIDE.p(260,15) Warning: unreachable code}
{LightWeightPascalIDE.p(116,13) Error: Identifier not found "TProcess"}
{LightWeightPascalIDE.p(398,4) Fatal: Syntax error, ";" expected but "ELSE" found}
{LightWeightPascalIDE.p(398,4) Fatal: Compilation aborted}
{Assembling lightweightpascalide}
{Linking LightWeightPascalIDE}
{Hur ser länkfel ut?}

{Bryt ner, trigga på kända startord, är det okänt så är det nog filnamn!}
{Först: Leta efter mellanslag eller startparentes}

procedure DoErrorEditMenu(item: Integer);
var
	clipData: AnsiString;
	err: OSErr;
	scrap: ScrapRef;
	aMsg: ErrorMessagePtr;
	count: Longint;
	rowStr, colStr: String;
begin
	case item of
		kEditUndo, kEditRedo, kEditCut, kEditPaste:
			DoBeep;
		kEditCopy:
		begin
			// Copy selected error to clipboard!
			if gSelectedMessage >= 0 then
			begin
				clipData := '';
				count := 0;
				aMsg := gErrorDisplayList; // gErrorMessageList; // gErrorDisplayList?
				// Hitta nummer gSelectedMessage
				while aMsg <> nil do
				begin
					if count = gSelectedMessage then
					begin
					    NumToString(aMsg^.rowNumber, rowStr);
					    NumToString(aMsg^.columnNumber, colStr);
					    if aMsg^.columnNumber = 0 then
							clipData := aMsg^.fileName + ' ' + rowStr + ' ' + aMsg^.message
					    else
							clipData := aMsg^.fileName + ' ' + rowStr+','+colStr + ' ' + aMsg^.message;
					end;
					count := count + 1;
					aMsg := aMsg^.next;
				end;
				if clipData <> '' then
				begin
					WriteLn('Copying: ', clipData);
					ClearCurrentScrap();
					err := GetCurrentScrap(scrap);
					WriteLn('CopyText GetCurrentScrap');
					if err <> noErr then
						WriteLn('GetCurrentScrap: ', err);
					if err = noErr then
						err := PutScrapFlavor(scrap, 'TEXT', kScrapFlavorMaskNone, Length(clipData), @clipData[1]);
				end;
			end;
		end;
		kEditClear:
			; // Rensa?
		kEditSelectAll, kEditShiftLeft, kEditShiftRight, kFormatCode, kToggleDual, kEditBlockCommentIn, kEditBlockCommentOut:
			DoBeep;
	end;
end;

{ -------------------------------------------------------------------- }
{					Window handling procedures							}
{ -------------------------------------------------------------------- }

	procedure WindActivate (active: Boolean);
	begin
		// Recalc scroll bar
		
		DrawGrowIcon(theErrorMsgWind);	{ make grow box reflect new window state }
	end;

procedure RecalcScrollbar;
		var
			r: MACOSALL.Rect;
//			aMsg: ErrorMessagePtr;
			count, {localCount,} visLines: Longint;
begin
WriteLn('RecalcScrollbar');
		count := GetDisplayCount;
//		aMsg := gErrorDisplayList; // gErrorMessageList;
//		count := 0;
//		while aMsg <> nil do
//			begin
//				count := count + 1;
//				aMsg := aMsg^.next;
//
//				ForeColor(blackColor); {Restore}
//			end;

		HideControl(errScroll);
		r := GetWindowPortBounds(theErrorMsgWind, r)^;

		visLines := (r.bottom - r.top) div kSpacing;
		if count - visLines > 0 then {/ / Doesn 't fit in window}
			SetControl32BitMaximum(errScroll, count - visLines)
		else
			SetControl32BitMaximum(errScroll, 0);
WriteLn('RecalcScrollbar ', count - visLines);

		halfPage := visLines div 2; { pageup / pagedown}

		r.left := r.right - 15;
		r.bottom := r.bottom - 14;
		r.top := r.top - 1;
		r.right := r.right + 1;
		SizeControl(errScroll, r.right - r.left, r.bottom - r.top);
		MoveControl(errScroll, r.left, r.top);
{if nLines - visLines < 0 then}
{ignore := 0 }
{    else}
{ ignore := nLines - vislines;}
{SetControl32BitMaximum ( errScroll , ignore );}
{ SetControlValue ( errScroll , errLine );}
		ShowControl(errScroll);
//WriteLn('RecalcScrollbar done');
end;

function MakeRGBColor(r, g, b: SInt16 {Integer}): RGBColor;
var
	color: RGBColor;
begin
	color.red := r;
	color.green := g;
	color.blue := b;
	MakeRGBColor := color;
 end;

function MakeRGBColorf(r, g, b: Real): RGBColor;
var
	color: RGBColor;
begin
	color.red := Trunc(r * 65535);
	color.green := Trunc(g * 65535);
	color.blue := Trunc(b * 65535);
	MakeRGBColorf := color;
 end;

procedure DoErrorMenu(where: MacOSAll.Point); forward;



{--- dragbar view ---}
var
	errView: HIViewRef;
{type
	ViewDataRec = record
		view: HIViewRef;
		window: WindowPtr;
		editIndex: Longint;
		icon: IconRef;
	end;
	ViewDataPtr = ^ViewDataRec;}

procedure BarWidgetFrame(inWindow: WindowRef; var outBounds: HIRect);
const
	kWidgetSize	= 16;	// width and height of the widget
	kWidgetSpace	= 8;		// space on the left and right of the widget
var
	rTitle, rStructure: MacOSALL.Rect;
//	outBounds: HIRect;
begin
	GetWindowBounds( inWindow, kWindowTitleTextRgn, rTitle );
	GetWindowBounds( inWindow, kWindowStructureRgn, rStructure );
	
	outBounds.origin.x := rTitle.right + kWidgetSpace - rStructure.left;
	outBounds.origin.y := rTitle.top - rStructure.top; {Should this center by kWidgetSize?}
	outBounds.size.width := kWidgetSize;
	outBounds.size.height := kWidgetSize;
end;

(*
	procedure DoViewUpdateQD(theView: HIViewRef; r: MacOSAll.Rect; userData: Pointer);
	var
//		err: OSErr;
//		vd: ViewDataPtr;
		pol: PolyHandle;
		curPort: MacOSAll.GrafPtr;
	begin
//		vd := ViewDataPtr(userData);
		MacOSAll.GetPort(curPort);
		CreatePortQDPort(curPort);

		EraseRect(MacRectToRect(r));
		FrameRect(MacRectToRect(r));
		MoveTo(2, 9);
		TextSize(9);
		DrawString('V');

		pol := OpenPoly;
		MoveTo(6, 10);
		LineTo(10, 14);
		LineTo(14, 10);
		ClosePoly;
		PaintPoly(pol);
		KillPoly(pol);

		FinishPort;

// Varför påverkas ritandet av vad man ritat innan? Transfer mode?	
//		err := PlotIconRef (r, kAlignNone, kTransformNone, kPlotIconRefNormalFlags, vd^.icon);
	end;

	procedure DoMouseQD (theView: HIViewRef; where: MacOSAll.Point; mods: Longint; userData: Pointer);
	var
//		vd: ViewDataPtr;
		b1: HIRect;
		thePort: GrafPtr;
	begin
		GetPort(thePort);
//		vd := ViewDataPtr(userData);

		SetPortWindowPort(theErrorMsgWind); // För att LocalToGlobal skall funka?
		BarWidgetFrame(theErrorMsgWind, b1);
		
		where.h := 0;
		where.v := 0;
		LocalToGlobal(where);
		
		where.h := where.h + Trunc(b1.origin.x)-0{32}; // Where does this offset come from???
		where.v := where.v + Trunc(b1.origin.y)-0{24};
		DoErrorMenu(where);
		
		SetPort(thePort);
	end;
	*)

//	procedure DoViewUpdateQDCG(theView: HIViewRef; r: MacOSAll.Rect; userData: Pointer);
	procedure DoViewUpdateQDCG(theView: HIViewRef; r: QDCG.Rect; userData: Pointer);
	var
//		err: OSErr;
//		vd: ViewDataPtr;
		pol: PolyHandle;
//		curPort: MacOSAll.GrafPtr;
	begin
//		vd := ViewDataPtr(userData);
//		MacOSAll.GetPort(curPort);
//		CreatePortQDPort(curPort);

		EraseRect(r);
		FrameRect(r);
		MoveTo(2, 9);
		TextSize(9);
		DrawString('V');

		pol := OpenPoly;
		MoveTo(6, 10);
		LineTo(10, 14);
		LineTo(14, 10);
		ClosePoly;
		PaintPoly(pol);
		KillPoly(pol);

		FinishPort;

// Varför påverkas ritandet av vad man ritat innan? Transfer mode?	
//		err := PlotIconRef (r, kAlignNone, kTransformNone, kPlotIconRefNormalFlags, vd^.icon);
	end;

procedure LocalToGlobal(var p: Point; theView: HIViewRef);
var
	{r,} rr: MacOSAll.Rect;
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

//	procedure DoMouseQD (theView: HIViewRef; where: MacOSAll.Point; mods: Longint; userData: Pointer);
	procedure DoMouseQDCG(theView: HIViewRef; where: QDCG.Point; mods, button: Longint; userData: Pointer);
	var
//		vd: ViewDataPtr;
		b1: HIRect;
	//	thePort: GrafPtr;
	begin
//		GetPort(thePort);
//		vd := ViewDataPtr(userData);

//		SetPortWindowPort(theErrorMsgWind); // För att LocalToGlobal skall funka?
		BarWidgetFrame(theErrorMsgWind, b1);
		
		where.h := 0;
		where.v := 0;
		LocalToGlobal(where, theView);
		
		where.h := where.h + b1.origin.x;
		where.v := where.v + b1.origin.y;
		DoErrorMenu(PointToMacPoint(where));
	end;

{--- end of dragbar view code ---}


{	On update event, can ignore the resizing information, since the whole}
{	window is always redrawn in terms of the current size, anyway.}
{	Content area is dark gray except scroll bar areas, which are white.}
{	Draw grow box as well.}

procedure WindUpdate(theView: ControlRef; viewRect: QDCG.Rect; userData: Pointer);
// procedure WindUpdate (resized: Boolean);
  var
   windBounds: MacOSAll.Rect;
   count, localCount {, visLines}: Longint;
   aMsg: ErrorMessagePtr;
   rowStr, colStr: Str255;
   color: RGBColor;
  // b1: HIRect;
   r: QDCG.Rect;
  const
   kScrollWidth = 15;
 begin
//WriteLn('WindUpdate');
//  QDCG.CreatePortWindowPort(theErrorMsgWind);
  windBounds := GetWindowPortBounds(theErrorMsgWind, windBounds)^;
  QDCG.BackColor(QDCG.whiteColor);
  QDCG.EraseRect(MacRectToRect(windBounds));
  aMsg := gErrorDisplayList; // gErrorMessageList;
  count := 0;
//  if aMsg = nil then
//  	WriteLn('No list!');
  while aMsg <> nil do
   begin
{/ / localCount is the position in which it is drawn}
    localCount := count - GetControlValue(errScroll);

    SetRect(r, 0, localCount * kSpacing, windBounds.right - kScrollWidth, (localCount + 1) * kSpacing);
//    FrameRect(r);

	if count = gSelectedMessage then
		color := MakeRGBColorf(1, 0.8, 0.5)
	else if count mod 2 = 0 then
		color := MakeRGBColorf(1, 1, 1)
	else
		color := MakeRGBColorf(0.95, 0.95, 0.95);
	RGBForeColor(color);
	PaintRect(r);
	ForeColor(blackColor);

// Gamla: markera med svart
//    if count = gSelectedMessage then
//     PaintRect(r);

    SetRect(r, 0, localCount * kSpacing, kSpacing, (localCount + 1) * kSpacing);
//    WriteLn('Draw ', aMsg^.theError);
	if aMsg^.theError >= 1 then
	if aMsg^.theError <= kMaxIcon  then
	if picIcon[aMsg^.theError] <> nil then
	begin
		QDCGPictUnit.DrawPicture( picIcon[aMsg^.theError], r);
//	    WriteLn('Drawn icon for ', aMsg^.theError);
	end;

//    if count = gSelectedMessage then
//     ForeColor(whiteColor);

    TextSize(9);

    MoveTo(40, localCount * kSpacing + 12);
    DrawString(GetLastToken(aMsg^.fileName));
    Move(10, 0);
    NumToString(aMsg^.rowNumber, rowStr);
    NumToString(aMsg^.columnNumber, colStr);

 	if aMsg^.columnNumber = 0 then
		DrawString(ConCat(rowStr))
	else
		DrawString(ConCat(rowStr, ',', colStr));
	MoveTo(40, localCount * kSpacing + 22);
	DrawString(aMsg^.message);

    count := count + 1;
    aMsg := aMsg^.next;

    ForeColor(blackColor); {Restore}
   end;

(*	if resized then
	begin
		RecalcScrollbar;

		BarWidgetFrame(theErrorMsgWind, b1);
		HIViewSetFrame( errView,  b1);
	end;
	*)
	
  QDCG.FinishPort;

  DrawControls(theErrorMsgWind);	{ redraw scroll bar }
  DrawGrowIcon(theErrorMsgWind);

//WriteLn('WindUpdate done');
 end;


// Find the selected message and pass it to EditOpenAndSelect (in LWPEdit)
	procedure OpenSelectSelected;
	var
		aMsg: ErrorMessagePtr;
		count: Longint;
		//tempStr: Str255;
	begin
		if gSelectedMessage < 0 then Exit(OpenSelectSelected);
		aMsg := gErrorDisplayList; // gErrorMessageList;
		count := 0;
		// Hitta nummer gSelectedMessage
		while aMsg <> nil do
		begin
			if count = gSelectedMessage then
			begin
//DebugStr('Försöker öppna meddelande ' + aMsg^.fileName);
				EditOpenAndSelect(aMsg^.fileName, aMsg^.rowNumber, aMsg^.columnNumber);
				Exit(OpenSelectSelected);
			end;
			count := count + 1;
			aMsg := aMsg^.next;
		end;

//NumToString(gSelectedMessage, tempStr);
//DebugStr('Inget meddelande valt, sökte nummer ' + tempStr);
		DoBeep;
	end;

	procedure WindHalt;
	begin
		{CloseWindow(theErrorMsgWind);}
		{Dispose?}
	end;


// Only used from mouse wheel.
	procedure DoScroll (lDelta: integer);
	var
		newLine: integer;
		savePort: GWorldPtr;
	begin
		newLine := errLine + lDelta;
		if newLine < 0 then
			newLine := 0;
		if newline > GetControl32BitMaximum(errScroll) then
			newline := GetControl32BitMaximum(errScroll);
		SetControl32BitValue(errScroll, newLine);
		lDelta := (errLine - newLine) * (kSpacing);
		//TEScroll(0, lDelta, teHelp);
		errLine := newLine;
		
		// savePort := GetPort; Hur kunde detta gå?
		GetPort(savePort); // ???
		SetPortWindowPort(theErrorMsgWind);
		HIViewSetNeedsDisplay(SkelGetContentView(theErrorMsgWind), true);
//		WindUpdate(false);
		SetPort(savePort);
	end;

var scrollPos: Longint;

function DidScroll(scroll: ControlRef; userData: Pointer): Boolean;
begin
	WriteLn('DidScroll ', scrollPos);
	HIViewSetNeedsDisplay(SkelGetContentView(theErrorMsgWind), true);
	DidScroll := false;
end;


{	Filter proc for tracking mousedown in scroll bar.  The part code}
{	of the part originally hit is stored as the control's reference}
{	value.}

	procedure TrackScroll (theScroll: ControlHandle; partCode: Integer);MWPascal; // ????? StdCall;
	var
		lDelta: integer;
	begin
	WriteLn('TrackScroll');
		if (partCode = GetControlReference(theScroll)) then
			begin
				case partCode of
					kControlUpButtonPart{inUpButton}
					: 
						lDelta := -1;
					kControlDownButtonPart{inDownButton}
					: 
						lDelta := 1;
					kControlPageUpPart{inPageUp}
					: 
						lDelta := -halfPage;
					kControlPageDownPart{inPageDown}
					: 
						lDelta := halfPage;
					otherwise
						;
				end;
				DoScroll(lDelta);
			end;
	end;



{Felet återuppstår när WindMouse använder scrollbaren? Men inte kan felet bara vara här?}
{Det verkar ändå så. Det funkade länge, tills jag använde OpenSelectSelected. Leta där?}
//	procedure WindMouse (thePt: MacOSAll.Point; t: UInt32; mods: integer);
	procedure WindMouse(theView: HIViewRef; thePt: QDCG.Point; mods, button: Longint; userData: Pointer);
		var
			newSelection, oldSelection: Longint;
			localNewSelection, localOldSelection: Longint;
			windBounds, r: MacOSALL.Rect;
//			err:OSErr;

			thePart{, ignore}: integer;
			trackScrollProc: ControlActionUPP;
			localThePt: MacOSALL.Point;
			t: Longint;
	begin
		t := TickCount;
		localThePt:= PointToMacPoint(thePt); // Just in case it is better to have a local copy. Not likely to matter.

// OBSOLETE
		thePart := TestControl(errScroll, localThePt);
//		thePart := 0;
		if thePart <> 0 then
		begin
		if thePart = kControlIndicatorPart then{inThumb}
			begin
				{ignore :=} TrackControl(errScroll, localThePt, nil);
				DoScroll(GetControlValue(errScroll) - errline);
			end
		else
			begin
				SetControlReference(errScroll, longint(thePart));
				trackScrollProc := NewControlActionUPP(TrackScroll);
				{ignore :=} TrackControl(errScroll, localThePt, trackScrollProc);
				DisposeControlActionUPP(trackScrollProc);
			end;
		Exit(WindMouse);
		end;
		
		
		// Locals for invalrect
		localNewSelection := localThePt.v div kSpacing;
		localOldSelection := gSelectedMessage - GetControlValue(errScroll);
		
		newSelection := localThePt.v div kSpacing + GetControlValue(errScroll);
		oldSelection := gSelectedMessage;
		if newSelection <> gSelectedMessage then
			begin
{Gör InvalRect på gamla och nya!}
				windBounds := GetWindowPortBounds(theErrorMsgWind, windBounds)^;
//				SetRect(r, 0, localNewSelection * kSpacing, windBounds.right, (localNewSelection + 1) * kSpacing);
				{err :=} InvalWindowRect(theErrorMsgWind, r); {InvalRect(r);}
//				SetRect(r, 0, localOldSelection * kSpacing, windBounds.right, (localOldSelection + 1) * kSpacing);
				{err :=} InvalWindowRect(theErrorMsgWind, r); {InvalRect(r);}
				HIViewSetNeedsDisplay(SkelGetContentView(theErrorMsgWind), true);

				gSelectedMessage := newSelection;
			end;
		if Longint(t) - gWhenClicked < 20 then
			if oldSelection = newSelection then {Borde egentligen bara ta om båda klicken i samma ruta!}
				begin
{Doubleclick!}
{Räkna fram till meddelande gSelectedMessage}
{OpenAndSelectFile(fileName, rowNumer, columnNumber)}
{- Öppna om den inte redan är öppen, sök i giltiga sökvägar}
{- Gå till felet}
//DebugStr('Försöker öppna');
					OpenSelectSelected;
					//SysBeep(1);
				end;

		gWhenClicked := Longint(t); {Kolla tid mot förra, anger dubbelklick!}
	end;

{WindKey skall kolla piltangenter, flytta valt meddelande upp och ner!}
	procedure WindKey (ch: char; mods: integer);
	var
		r, windBounds: MacOSAll.Rect;
		newSelection: Longint;
		//err: OSErr;
	begin
		if ch = Char(13) then
		begin
			// som dubbelklick
			OpenSelectSelected;
		end
		else
		if Ord(ch) in [30, 31] then {30 = upp, 31 = ner}
		begin
			if Ord(ch) = 30 then
				newSelection := gSelectedMessage - 1;
			if Ord(ch) = 31 then
				newSelection := gSelectedMessage + 1;
				
				{Kolla om vi kom utanför intervallet!}
				if newSelection < 0 then Exit(WindKey);
				
{Gör InvalRect på gamla och nya!}
				windBounds := GetWindowPortBounds(theErrorMsgWind, windBounds)^;
				SetRect(r, 0, newSelection * kSpacing, windBounds.right, (newSelection + 1) * kSpacing);
//				{err :=} InvalWindowRect(theErrorMsgWind, r); {InvalRect(r);}
				SetRect(r, 0, gSelectedMessage * kSpacing, windBounds.right, (gSelectedMessage + 1) * kSpacing);
//				{err :=} InvalWindowRect(theErrorMsgWind, r); {InvalRect(r);}
				HIViewSetNeedsDisplay(SkelGetContentView(theErrorMsgWind), true);
				
				gSelectedMessage := newSelection;
				
				// Kolla att den valda syns! Scrolla vid behov!
		end;
	end;


// DENNA SKALL INTE BEHÖVAS!!!
procedure DoClose;
var
	thePort: MacOSAll.GrafPtr;
begin
	GetPort(thePort);			{ grafport of window to be closed }
	HideWindow(GetWindowFromPort(thePort));
end;


				procedure DoErrorMenu(where: MacOSAll.Point);
				var
					menuSelection: Longint;
					goodIndex: Longint;
				begin
					goodIndex := 0;
					InsertMenu(errorMenu, -1);
					menuSelection := PopUpMenuSelect(errorMenu, where.v, where.h, goodIndex);
					
					// Handle selection
					// Reprocess the errors list, redisplay
					case Lo(menuSelection) of
						1: // All
						begin
							showError[1] := true;
							showError[2] := true;
							showError[3] := true;
							showError[4] := true;
						end;
						2: // Errors
						begin
							showError[1] := not showError[1];
							showError[2] := not showError[2];
						end;
						3: // Warnings
						begin
							showError[3] := not showError[3];
						end;
						4: // Notes
						begin
							showError[4] := not showError[4];
						end;
					end;
					//CheckMenuItem(errorMenu, 1, showError[1]);
					CheckMenuItem(errorMenu, 2, showError[2]);
					CheckMenuItem(errorMenu, 3, showError[3]);
					CheckMenuItem(errorMenu, 4, showError[4]);
					RecalcDisplayList;
					
					DeleteMenu(kPathMenuID); // kPathMenuID eller nåt sånt
				end;


	// Internal handler of Carbon Events in classic dialogs
	function DragClickEventHandler (nextHandler: EventHandlerCallRef; inEvent: EventRef;
							userData: Pointer): OSStatus; MWPascal;
	var
		eventClass: UInt32;
		eventKind: UInt32;
		//classicEvent: EventRecord;
		//success: Boolean;
		theMods: Longint;
		thePort: CGrafPtr;
		foundWindow: WindowRef;
		//editIndex: Longint;
		//startOffset, endOffset: TXNOffset;
		//i, iItemCount, item
		//goodIndex, menuSelection: Longint;
		//pathMenu: MenuRef;
		//path: AnsiString;
		//itemString: Str255;
		where: MacOSAll.Point;
		err: OSErr;
		//saveMainFile: FSSpecOBSOLETE;
		
	// Auto-indent:
		//chars: Handle;
		//theString: Str255;
		//key: Char;
		//dataPtr: CharsPtr;
		//pos: Longint;
		//oStartOffset, oEndOffset: TXNOffset;
//		wheelDelta: Longint;		
//		axis: EventMouseWheelAxis;
	begin
//		WriteLn('Custom proc edit wind?');
		eventClass := GetEventClass(inEvent);
		eventKind  := GetEventKind(inEvent);
		
		GetPort(thePort); // Fel sätt att hitta eventfönster!!
		//foundWindow := GetWindowFromPort(thePort);
		
		err := GetEventParameter (inEvent, kEventParamDirectObject,
			typeWindowRef, nil, SizeOf(WindowPtr), nil, @foundWindow);
		
//		if ((eventClass = kEventClassMouse) and (eventKind = kEventMouseWheelMoved)) then
//		begin
//			if GetEventParameter(inEvent, kEventParamMouseWheelDelta, typeLongInteger,
//								nil, sizeof(Longint), nil, @wheelDelta ) = noErr then
//			begin
//					WriteLn('Wheel applied!');
//					err := GetEventParameter(inEvent, kEventParamMouseWheelAxis, typeMouseWheelAxis,
//								nil, sizeof(EventMouseWheelAxis), nil, @axis);
//					if axis = kEventMouseWheelAxisY then
//					begin
//						DoScroll (wheelDelta);
//					end;
//			end;
//		end;

		if err = noErr then
		if foundWindow <> FrontWindow then
		begin
			Return eventNotHandledErr;
		end;

// Borde även testa altmusknapp:
// kEventMouseButtonSecondary
// Kräver att musklick i content testas.
// Altmusknapp skulle kunna vara intressant för editorn också.

		if ((eventClass = kEventClassWindow) and (eventKind = kEventWindowClickDragRgn)) then
		begin
			theMods := GetCurrentEventKeyModifiers;
			
			err := GetEventParameter (inEvent, kEventParamMouseLocation,
				typeQDPoint, nil, SizeOf(MacOSAll.Point), nil, @where);
			
//			if BitAnd(theMods, optionKey) <> 0 then // option-click in drag bar - function menu
//			begin
//			end
//			else if BitAnd(theMods, cmdKey) <> 0 then	// command-click in drag bar
//			begin
//			end
//			else if BitAnd(theMods, shiftKey) <> 0 then
//			begin
//			end
//			else
			if BitAnd(theMods , controlKey or optionKey or cmdKey) <> 0 then
			begin
//	function BuildMenuFromUses(teWind: WindowPtr; teEdit: TXNObject): MenuHandle;
				if errorMenu <> nil then
				begin
					DoErrorMenu(where);
					return noErr;
				end;
			end
			else
			return eventNotHandledErr; {Ask TransSkel to handle dragging}
			
		end;
		
		return eventNotHandledErr;
	end; {DragClickEventHandler}


procedure WindResize(theEditWind: WindowRef);
var
	b1: HIRect;
begin
WriteLn('Resize err');
	RecalcScrollbar;

	BarWidgetFrame(theErrorMsgWind, b1);
	HIViewSetFrame( errView,  b1);

// Adjust scroll bar position
	RecalcScrollbar;

	BarWidgetFrame(theErrorMsgWind, b1);
	HIViewSetFrame( errView,  b1);
end;

procedure ErrorScroll(affectedWindow: WindowPtr; axis: EventMouseWheelAxis; wheelDelta: Longint);
begin
	if axis = kEventMouseWheelAxisY then
	begin
		DoScroll(-wheelDelta);
	end;
end;

{	Read window from resource file and install handler for it.  Mouse}
{	and key clicks are ignored.  There is no close proc since the window}
{	doesn't have a close box.  There is no idle proc since nothing is}
{	done while the window is in front (all the things that are done are}
{	handled by TransSkel).}

	procedure ErrorMsgWindInit;
	var
		//dummy: Boolean;
		//i: Integer;
		r: MacOSAll.Rect;
		root: HIViewRef;
		b1: HIRect;
		
		p:Array [1..5] of AnsiString;
		windframe: Rect;
		//control: HIViewRef;
		layout: HILayoutInfo;
		
	const
{Custom handler for click in window drag bar}
		customEventTypes: array [0..1] of EventTypeSpec =
		(
			( eventClass: kEventClassWindow; eventKind: kEventWindowClickDragRgn ),
			( eventClass: kEventClassMouse; eventKind: kEventMouseWheelMoved )
		);
	begin
//ClearErrorMessages; // TEST

		{for i := 1 to kMaxIcon do
			if bugIcon[i] = nil then
				bugIcon[i] := GetCIcon(127+i);}
		
		//function GetPicture(fileName: AnsiString): PicHandle;
		p[1]:= 'icons/bug.png';
		PicIcon[1]:= GetPicture(p[1]);
		p[2]:= 'icons/fatal.png';
		PicIcon[2]:= GetPicture(p[2]);
		p[3]:= 'icons/warning.png';
		PicIcon[3]:= GetPicture(p[3]);
		p[4]:= 'icons/note.png';
		PicIcon[4]:= GetPicture(p[4]);
		p[5]:= 'icons/search.png';
		PicIcon[5]:= GetPicture(p[5]);
(*
		theErrorMsgWind := GetNewWindow(theErrorMsgWindRes, nil, WindowPtr(-1));
		SetPortWindowPort(theErrorMsgWind);
//		dummy := SkelWindow(theErrorMsgWind, @WindMouse, @WindKey, @WindUpdate, @WindActivate, @DoClose, @WindHalt, nil, false);
		SkelCustomWindow(theErrorMsgWind, WindMouse, WindKey, WindUpdate,
					WindActivate, DoClose, WindHalt, nil, false, 0, @DragClickEventHandler,
					@customEventTypes[0], Length(customEventTypes));
*)
		SetRect(windframe, 2, 355, 353, 640);
		theErrorMsgWind := SkelNewWindow(windframe, 'Error messages');
		SkelWindow(theErrorMsgWind, WindUpdate, WindMouse, WindKey, nil,
					WindActivate, DoClose, WindHalt,
					nil, false,
					0, @DragClickEventHandler,
					@customEventTypes[0], Length(customEventTypes), @WindResize);
//					0, @DragClickEventHandler,
//					@customEventTypes[0], Length(customEventTypes), @WindResize);
		SkelSetScrollProc(theErrorMsgWind, @ErrorScroll);
		HideWindow(theErrorMsgWind);

//		SkelCustomWindow(theErrorMsgWind, WindMouse, WindKey, WindUpdate,
//					WindActivate, DoClose, WindHalt, nil, false, 0, @DragClickEventHandler,
//					@customEventTypes[0], Length(customEventTypes));

//	control := SkelGetContentView(theErrorMsgWind);
//	GetControlBounds(control, r);
//	r.right -= 15;
//	r.bottom -= 15;
//	SetControlBounds(control, r);

		r := GetWindowPortBounds(theErrorMsgWind, r)^;
		r.left := r.right - 15;
		r.bottom := r.bottom - 14;
		r.top := r.top - 1;
		r.right := r.right + 1;
{	Build the scroll bar.  Don't need to bother testing whether to}
{	highlight it or not, since that will be done in response to the}
{	activate event.}
//		errScroll := NewControl(theErrorMsgWind, r, '', true, 0, 0, 0, scrollBarProc, 0);
		CreateScrollBarControl (theErrorMsgWind, r, {value}0, {minimum}0, {maximum}0-0, {viewSize}1, true, nil{LiveScrollProcV}, errScroll);
		VMInstallDefaultScrollHandler(errScroll, 1, 10);

		InstallNumViewHandler(theErrorMsgWind, errScroll, scrollPos, DidScroll);

		// Autosize scrollbar!
				// Auto-position
		layout.version := kHILayoutInfoVersionZero;
		layout.binding.top.toView := HIViewGetRoot(theErrorMsgWind);
		layout.binding.top.kind := kHILayoutBindTop;
		layout.binding.left.toView := HIViewGetRoot(theErrorMsgWind);
		layout.binding.left.kind := kHILayoutBindRight;
		layout.binding.right.toView := HIViewGetRoot(theErrorMsgWind);
		layout.binding.right.kind := kHILayoutBindRight;
		layout.binding.bottom.toView := HIViewGetRoot(theErrorMsgWind);
		layout.binding.bottom.kind := kHILayoutBindBottom;
		layout.scale.x.kind := kHILayoutBindNone;
		layout.scale.y.kind := kHILayoutBindNone;
		layout.position.x.kind := kHILayoutBindNone;
		layout.position.y.kind := kHILayoutBindNone;
		HIViewSetLayoutInfo(errScroll, layout);		
		
		// Onödiga nollställningar?
		gErrorMessageList := nil;
		gErrorDisplayList := nil;
		halfPage := 0;
		gSelectedMessage := 0;

		//gMaxErrorLevel := 4;
		errorMenu := NewMenu(kPathMenuID, 'ErrMenu');
		AppendMenu(errorMenu, 'Show all;Errors;Warnings;Notes');
		showError[1] := true;
		showError[2] := true;
		showError[3] := true;
		showError[4] := true;
		showError[5] := true;
		CheckMenuItem(errorMenu, 2, showError[2]);
		CheckMenuItem(errorMenu, 3, showError[3]);
		CheckMenuItem(errorMenu, 4, showError[4]);
		
		// Install custom dragbar view
		SetRect(r, 0, 0, 32, 32);
		CreateUserPaneControl(theErrorMsgWind, r, {features} 0, errView);
		root := HIViewGetRoot( theErrorMsgWind );
		HIViewAddSubview( root, errView );
		InstallQDCGSkelViewHandler(theErrorMsgWind, errView, DoViewUpdateQDCG, DoMouseQDCG, nil, nil);
		BarWidgetFrame(theErrorMsgWind, b1);
		HIViewSetFrame( errView,  b1);
	end;

end.
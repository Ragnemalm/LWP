// Lightweight Pascal IDE, message window unit = console
// © Ingemar Ragnemalm 2006-2007

{Originally based on TransSkel MultiSkel Help module. Rewritten for MLTE. Rewritten again for Halda.}
{Some comments from TextEdit days may remain.}
// Was called LWPHelp in the past (until 160115)
// Calls are still prefixed Help but should be Console.

{$mode macpas}
unit Console;

interface

	uses
		MacOSAll, LWPGlobals, TransSkel4, strutils, ProcessUtils,
		TXNUtilities, Settings, LWPColorGlobals, Halda, HaldaTypes, QDCG;

	procedure HelpWindInit;
	procedure HelpClear;
	procedure HelpForceUpdate;
	procedure HelpAppendLn(s: Ansistring);
	procedure HelpAppend(s: Ansistring);
	procedure DoHelpEditMenu (item: integer);
	procedure HelpMainMessage(s: Ansistring);
	function BarWidgetFrame(inWindow: WindowRef): HIRect;

implementation

	var
//		teHelp: TXNObject;		{ handle to help window TextEdit record }
		barWheelView: HIViewRef;

// Place a spinning wheel in the drag bar when a process is running!
function BarWidgetFrame(inWindow: WindowRef): HIRect;
const
	kWidgetSize	= 16;	// width and height of the widget
	kWidgetSpace	= 8;		// space on the left and right of the widget
var
	rTitle, rStructure: MacOSAll.Rect;
	outBounds: HIRect;
begin
	GetWindowBounds( inWindow, kWindowTitleTextRgn, rTitle );
	GetWindowBounds( inWindow, kWindowStructureRgn, rStructure );
	
	outBounds.origin.x := rTitle.right + kWidgetSpace - rStructure.left;
	outBounds.origin.y := rTitle.top - rStructure.top; {Borde väl centrera med kWidgetSize???}
	outBounds.size.width := kWidgetSize;
	outBounds.size.height := kWidgetSize;
	return outBounds;
end;

procedure HelpMainMessage(s: Ansistring);
begin
	if Length(s) > 0 then
	begin
		SetWTitle(helpWind, s);
		HIViewSetFrame( barWheelView, BarWidgetFrame(helpWind) );
		HIViewSetVisible(barWheelView, true);
	end
	else
	begin
		SetWTitle(helpWind, 'Lightweight IDE');
		HIViewSetVisible(barWheelView, false);
	end;
end;

	procedure DoHelpEditMenu (item: integer);
//	var
		//ignore: integer;
		//editIndex: Integer;
//		err: OSErr;
	const // Copied from Edit! Should be only one set!
		kEditUndo = 1;
		kEditRedo = 2;
		kEditCut = 4;
		kEditCopy = 5;
		kEditPaste = 6;
		kEditClear = 7;
		kEditSelectAll = 8;
	begin
//WriteLn('DoHelpEditMenu ', item);
			case item of
				kEditUndo:
					;
				kEditRedo: 
					;
				kEditCut: 
					{err :=} HCopy(teHelp^.h);
				kEditCopy: 
					{err :=} HCopy(teHelp^.h);
				kEditPaste: 
					;
				kEditClear: 
					HelpClear;
				kEditSelectAll:
					HSelectAll(teHelp^.h);
				otherwise
					;
			end; {case}
	end;


{	When the window comes active, disable the Edit menu and highlight}
{	the scroll bar if there are any lines not visible in the content}
{	region.  When the window is deactivated, enable the Edit menu and}
{	un-highlight the scroll bar.}

	procedure Activate (active: Boolean);
	begin
//		DrawGrowBox(helpWind); // Obsolete!
	end;

//	var
//		err: OSErr;

	procedure HelpClear;
	begin
//		err := TXNClear(teHelp); // Funkar inte???
//		{err :=} TXNSetData(teHelp, kTXNTextData, nil, 0, kTXNStartOffset, kTXNEndOffset);
		// Funkar inte heller???
		HSetText(teHelp^.h, '');
		// Inval?
	end;
	
	procedure HelpAppendLn(s: Ansistring);
	begin
	// Add a CR
		s := s + Char(13);
		HelpAppend(s);
	end;

	procedure HelpAppend(s: Ansistring);
	var
		i, limit: Longint;
	const
		FF = Char(12); // Form feed = ASCII(12).
		BEL = Char(7); // Bell
	begin
	// Process BEL and FF (moved from Lightbugs 160402 - why was it ever there?)
		i := 1;
		while i < Length(s) do
		begin
			if s[i] = FF then
			begin
				HelpClear;
				s := Copy(s, i+1, Length(s));
				i := 1;
			end
			else
			if s[i] = BEL then
			begin
				SysBeep(1);
				s := Copy(s, 1, i-1) + Copy(s, i+1, Length(s));
			end
			else
				i += 1;
		end;
		
		HSetSelection(teHelp^.h, Length(teHelp^.h^.text)+1, Length(teHelp^.h^.text)+1); // +1?
		HInsert(teHelp^.h, s);
		HShowSelection(teHelp, false);
	end;
	
	procedure HelpDeleteTail(count: Longint);
	begin
		// TAG BORT I SLUTET
//		HGetSelection(teHelp^.h, so, eo);
		HSetSelection(teHelp^.h, Length(teHelp^.h^.text)-count, Length(teHelp^.h^.text));
		HInsert(teHelp^.h, '');
//		{err :=} TXNSetData(teHelp^.h, kTXNTextData, nil, 0, eo-count, kTXNEndOffset);
	end;

	procedure Halt;
	begin
		HDispose(teHelp^.h);
		HDisposeView(teHelp);
		teHelp := nil;
	end;


//	procedure Mouse (thePt: MacOSAll.Point; t: UInt32; mods: integer);
//	var
//		thePart, ignore: integer;
//		trackScrollProc: ControlActionUPP;
//	begin
		// Möjligen
		// TXNClick(teEdit[editIndex], theEvent);
//	end;


// Input buffer for "terminal", used by HelpKey below:
	var
		buffer: AnsiString;

	procedure HelpKey (ch: char; mods: integer);
	const
		CR = Char(13);
		LF = Char(10);
		BS = Char(8);
	begin
		if BitAnd(mods, cmdKey)<>0 then Exit(HelpKey); // Fix for cmd-keys going into the window
	
		if gCurrentProcess <> nil then
			if ProcessRunning(gCurrentProcess) then
			begin
				if ch in [CR, LF] then
				begin
{$ifc defined CPUPOWERPC}
// Echo is on - delete before writing
					HelpDeleteTail(Length(buffer));
{$elsec}
					HelpAppend(CR);
{$endc}
					ProcessWrite(gCurrentProcess, buffer + LF);
					buffer := '';
				end
				else
				if ch = BS then
				begin
					if Length(buffer) > 0 then
					begin
						SetLength(buffer, Length(buffer)-1);
						HelpDeleteTail(1);
					end;
				end
				else
				begin
					HelpAppend(ch);
					buffer := buffer + ch;
				end;
				
//				ProcessWrite(gCurrentProcess, ch);
//				HelpAppend(ch);
//				if ch = CR then
//					ProcessWrite(gCurrentProcess, LF); // Försök att få ordning på CLI-beteendet
			end;
		//begin
		//	ProcessWriteChar(gCurrentProcess.theProcess, ch);
//			gCurrentProcess.theProcess.Input.Write(ch, 1);
		//end;
		
// ALSO: Detect command-period! ESC?
	end;


//	procedure Update (resized: Boolean);
	procedure Update(theView: ControlRef; viewRect: QDCG.Rect; userData: Pointer);
//	var
//		r, ignoreRect: MacOSAll.Rect;
//		result: OSStatus;
	begin
//WriteLn('Help/console update');
	// Helt onödig, väl?
//		if resized then
//		begin
//			HIViewSetFrame( barWheelView, BarWidgetFrame(helpWind) );
//			HAdjustTextEditArea(teHelp^.h, 0,0,0,0,0,0,0,0);
//			HAdjustScrollBarRange(teHelp);
//		end;
//WriteLn('Help/console update sets port');
//		if GetQDGlobalsThePort = nil then
//			WriteLn('No damn port???');
//		CreatePortQDPort(GetQDGlobalsThePort);
//		SetTextToNormal(teHelp^.h, true, GetSettingsTextSize); // and all black
		teHelp^.invalLevel := 2; // Full redraw
		HDraw(teHelp);
		DrawControls(teHelp^.window);
		DrawGrowIcon(teHelp^.window);
		FinishPort;
//WriteLn('Help/console update done');
	end;
	
	procedure Resize(theEditWind: WindowRef);
	begin
		HIViewSetFrame( barWheelView, BarWidgetFrame(helpWind) );
		HAdjustTextEditArea(teHelp^.h, 0,0,0,0,0,0,0,0);
		HAdjustScrollBarRange(teHelp);
		HIViewSetNeedsDisplay(SkelGetContentView(helpWind), true);
	end;

	{Needed to make an appended message visible immediately}
	procedure HelpForceUpdate;
	begin
		HIViewSetNeedsDisplay(SkelGetContentView(helpWind), true);
//		SetPortWindowPort(helpWind);
//		Update(true); // fake resize to recalc scroll bars
//		MacOSAll.QDFlushPortBuffer(GetWindowPort(helpWind), nil);
//		SetPortWindowPort(FrontWindow);
	end;

// DENNA SKALL INTE BEHÖVAS!!!
procedure DoClose;
var
	thePort: MacOSAll.GrafPtr;
begin
	GetPort(thePort);			{ grafport of window to be closed }
	HideWindow(GetWindowFromPort(thePort));
end;

var
	pressed: Boolean; // Behövs inte? Jo, för att ersätta mouseUp-events!

procedure Mouse(theView: HIViewRef; thePt: QDCG.Point; mods, button: Longint; userData: Pointer);
//procedure Mouse (thePt: MacOSAll.Point; t: LongWord; mods: integer);
begin
WriteLn('Help mouse');
WriteLn('Text length = ', Length(teHelp^.h^.text));
WriteLn('Font size = ', teHelp^.h^.fontSize);
WriteLn('Row start length = ', Length(teHelp^.h^.rowStarts));
WriteLn('Row height = ', teHelp^.h^.rowHeight);
WriteLn('tabWidth = ', teHelp^.h^.tabWidth);
	if PtInRect(thePt, teHelp^.viewRect) then
	begin
		HMouse(teHelp, thePt, mods);
WriteLn('Done HMouse');
//		HInvalView(teEdit[editIndex]);
		pressed := true; // Behövs inte? Jo!
	end
	else
		SkelCallNextEventHandler;
WriteLn('Exits mouse');
end;
	
var
	count: Longint;
	savedp: MacOSAll.Point;
		
procedure Idle;
var
	p: MacOSAll.Point;
begin
{PosToPoint, blinka vid punkten}
	count := (count + 1) mod 5; // För att detta demo behöver högre frekvens för resten
										// Hellre kolla klockan i HIdle?
	if count = 0 then
		HIdle(teHelp);

		GetMouse(p);
		
		if not Button then
		begin
			if pressed then
				HMouseUp(teHelp, MacPointToPoint(p), 0);
			pressed := false; // Behövs inte?
		end;
		
		if (p.h <> savedp.h) or (p.v <> savedp.v) then
			if pressed then
				HMouseDrag(teHelp, MacPointToPoint(p));
		savedp := p;
end;

// Scroll wheel support
	function HelpWindowEventHandler (nextHandler: EventHandlerCallRef; inEvent: EventRef;
							userData: Pointer): OSStatus; MWPascal;
	var
		eventClass: UInt32;
		eventKind: UInt32;
		editIndex: Longint;
		err: OSErr;
		wheelDelta: Longint;
		max, value: Longint;
		axis: EventMouseWheelAxis;
		scrollbar: HIViewRef;
	begin
		eventClass := GetEventClass(inEvent);
		eventKind  := GetEventKind(inEvent);
//		WriteLn('Event ', eventClass, ',', eventKind);
		
		// 140625: Mouse wheel!
		if ((eventClass = kEventClassMouse) and (eventKind = kEventMouseWheelMoved)) then
		begin
			if GetEventParameter(inEvent, kEventParamMouseWheelDelta, typeLongInteger,
								nil, sizeof(Longint), nil, @wheelDelta ) = noErr then
			begin
				// Scroll by wheelDelta!
				WriteLn('Wheel ', wheelDelta);
				
				// Scroll the active view!
				begin
					WriteLn('Wheel applied!');
					err := GetEventParameter(inEvent, kEventParamMouseWheelAxis, typeMouseWheelAxis,
								nil, sizeof(EventMouseWheelAxis), nil, @axis);
					if axis = kEventMouseWheelAxisY then
						scrollbar := teHelp^.vScroll
					else
						scrollbar := teHelp^.hScroll;
					if scrollbar <> nil then
					begin
						value := VMGetNumValue(scrollbar) - wheelDelta;
						max := GetControl32BitMaximum(scrollbar);
						if value <= max then
							VMSetNumValue(scrollbar, value);
					end;
				end;
			end;

		end
		else
			return eventNotHandledErr;
	end; {EditWindowEventHandler}
	

function NewEditWindow(r: Rect; title: AnsiString; var h: HaldaPtr): WindowPtr;
var
	w: WindowPtr;
	control: ControlHandle;
	rr: MacOSAll.Rect;
	view: HaldaViewPtr;
begin
	w := SkelNewWindow(r, title);
	h := HNew;
	HSetText(h, '');
	view := HNewView(w, h, true, true);
	view^.hiview := SkelGetContentView(w);
	view^.window := nil;
	
	control := SkelGetContentView(w);
	GetControlBounds(control, rr);
	rr.right -= 15;
	rr.bottom -= 15;
//	SetControlBounds(control, rr);
	
	NewEditWindow := w;
end;

procedure ConsoleScroll(affectedWindow: WindowPtr; axis: EventMouseWheelAxis; wheelDelta: Longint);
var
	scrollbar: HIViewRef;
	max, value: Longint;
begin
	if axis = kEventMouseWheelAxisY then
		scrollbar := teHelp^.vScroll
	else
		scrollbar := teHelp^.hScroll;
	if scrollbar <> nil then
	begin
		value := VMGetNumValue(scrollbar) - wheelDelta;
		max := GetControl32BitMaximum(scrollbar);
		if value <= max then
		begin
			VMSetNumValue(scrollbar, value);
			HIViewSetNeedsDisplay(SkelGetContentView(affectedWindow), true);
		end;
	end;
end;

	procedure HelpWindInit;
	var
		r{, ignoreRect}: MacOSAll.Rect;
		//visLines, scrollLines: integer;
		//result: OSStatus;
		//fontNum: Integer;
//		teFrameID: TXNFrameID;
		root: HIViewRef;
		h: HaldaPtr;
		windframe: Rect;
		view: HaldaViewPtr;
//	const
{Custom handler for mouse wheel}
//		customEventTypes: array [0..0] of EventTypeSpec =
//		(
//			( eventClass: kEventClassMouse; eventKind: kEventMouseWheelMoved )
//		);
	begin
WriteLn('Console');
		SetRect(windframe, 2, 46, 353, 331);

		helpWind := NewEditWindow(windframe, 'Lightweight IDE', h);
//		helpWind := GetNewCWindow(helpWindRes, nil, WindowPtr(-1));
//		SkelCustomWindow(helpWind, @Mouse, @HelpKey, @Update, @Activate, @DoClose, nil{@Halt}, @Idle, true,
//							kEventDurationSecond/5,
//							@HelpWindowEventHandler, @customEventTypes[0], Length(customEventTypes));
//		SkelWindow(helpWind, @Update, @Mouse, @HelpKey, nil, @Activate, @DoClose, nil{@Halt}, @Idle, true,
//							kEventDurationSecond/5,
//							@HelpWindowEventHandler, @customEventTypes[0], Length(customEventTypes),
//							@Resize);
		SkelWindow(helpWind, @Update, @Mouse, @HelpKey, nil, @Activate, @DoClose, nil{@Halt}, @Idle, true,
							kEventDurationSecond/5,
							nil, nil, 0,
							@Resize);
		SkelSetScrollProc(helpWind, @ConsoleScroll);


(*helpWind := SkelNewWindow(windframe, 'Lightweight IDE', @Update, @Mouse, @HelpKey, nil,
		@Activate, @DoClose, nil, @Idle, true, kEventDurationSecond/5,
		@HelpWindowEventHandler, @customEventTypes[0], Length(customEventTypes), @Resize);
	h := HNew;
	HSetText(h, '');
	view := HNewView(helpWind, h, true, true);
	view^.hiview := SkelGetContentView(helpWind);
	view^.window := nil;*)
	
WriteLn('Console done');

//							nil, nil, 0);
		// Halt was incorrect and unnecessary
		
		SetPortWindowPort(helpWind);
//		h := HNew; // Kraschar
//		HSetFont(h, 'Monaco', GetSettingsTextSize);

		// KRÄVER ATT QDCG HAR EN PORT!
//		MacOSAll.GetPortBounds(GetWindowPort(helpWind), viewRect);
//		teHelp := HNewView(helpWind, h, true, true);
		teHelp := h^.views[0];

		HSetText(h, 'Welcome to Lightweight IDE! This is the console window.');

		//08/06/21 DS For font and sintax coloring     	
//		SetSintaxColor(gFlags^^.textSaveColorCoding);
//		FontCode:=gFlags^^.textSaveFont;

//WriteLn('Ajuto 3');

// Crashes!
		SetTextToNormal(teHelp^.h, true, GetSettingsTextSize); // and all black
		root := HIViewGetRoot( helpWind );
		MacOSAll.SetRect(r, 0, 0, 16, 16);
		CreateChasingArrowsControl(nil, r, barWheelView);
		HIViewSetVisible(barWheelView, false);
		HIViewAddSubview( root, barWheelView );
		HIViewSetFrame( barWheelView, BarWidgetFrame(helpWind) );

	WriteLn('Console done done');
end;
end.

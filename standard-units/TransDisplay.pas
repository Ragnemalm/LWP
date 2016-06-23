{	TransDisplay version 2.0.1 - TransSkel plug-in module supporting}
{	an arbitrary number of generic display windows with memory.}

{	TransSkel and TransDisplay are public domain, and are written by:}

{			Paul DuBois}
{			Wisconsin Regional Primate Research Center}
{			1220 Capital Court}
{			Madison WI  53706  USA}

{	UUCP:		[allegra,ihnp4,seismo]!uwvax !uwmacc !dubois }
{	ARPA : 	dubois @ unix.macc.wisc.edu }
{				dubois @ rhesus.primate.wisc.edu }

{	The Pascal Version of TransSkel is public domain and was ported by		}

{			Owen Hartnett			}
{			½hm Software			}
{			163 Richard Drive		}
{			Tiverton, RI 02878		}

{	CSNET:	omh@cs.brown.edu.CSNET 											}
{	ARPA:		omh%cs.brown.edu@relay.cs.net-relay.ARPA						}
{	UUCP:		[ihnp4,allegra]!brunix !omh											}

{	Psychic Wavelength:  182.2245 Meters  (sorry, couldn't resist)	}

{	This version of TransDisplay written for Lightspeed Pascal.  Lightspeed Pascal}
{	is a trademark of:}
{			THINK Technologies, Inc}
{			420 Bedford Street  Suite 350}
{			Lexington, MA  02173  USA}


 { History}
{  08/25/86	Genesis.  Beta version.}
{  09/15/86	Changed to allow arbitrary number of windows.  Changed}
{ 			version number to 1.0.}
{  01/10/87	Ported to LightSpeed Pascal by Owen Hartnett				}
{	½hm Software, 163 Richard Drive, Tiverton, RI 02878				}
{  12/2/88    Made changes to add conditional compiling if you only need }
{			one TransDisplay window.  Set the following cond variable		}
{			singleDisplay to true if you want only one TransDisplay window }
{			and want smaller code size.	Made adjustments for LSP 2.0	}

{dec -94: Two serious bugs fixed by Ingemar R, both causing problems with multiple TransDisplay windows:}
{Ð Mouse events could be sent to the wrong display window.}
{Ð SyncGlobals didn't check dispInfo for nil, which could cause crashes.}
{aug -97: TS 2.6, UPI interfaces, PowerMac native}
{april 2002: Carbonization mostly working!}
// 2006 removed CarbonExtras dependency. CarbonExtras is good, but TransSkel should not depend on it.

// November 2007 Major rewrite for using MLTE and TransSkel 4, by Ingemar Ragnemalm.
// News in this version:
// Rewritten to use MLTE, no more 32k limit.
// All the TextEdit handling code was removed.
// No globals, only the list root and currentDisplay pointers.
// No single-window option.
// Added display to HIView.
// No more singleDisplay, no more globals to synch with.
// New demo: MiniDisplayView
// This is the first major change since 1.0. I call this version 2.0.

// 080216: Some minor corrections.
// 090421: Moved to MacOSAll.

{$mode macpas}
unit TransDisplay;
interface

	uses
		MacOSAll, TransSkel4;

	procedure SetDWindow (theWind: WindowPtr);
	procedure DisplayString (theStr: str255);overload;
	procedure DisplayString (theStr: AnsiString);overload;
	procedure DisplayHexLong (l: longint);
	procedure DisplayHexInt (i: integer);
	procedure DisplayHexChar (c: char);
	procedure DisplayBoolean (b: Boolean);
	procedure DisplayChar (c: char);
	procedure DisplayInt (i: integer);
	procedure DisplayLong (l: longint);
	procedure DisplayLn;
	procedure DisplayText (theText: Ptr; len: longint);
	function GetNewDWindow (resourceNum: integer; behind: WindowPtr): WindowPtr;
	function NewDWindow (bounds: Rect; title: Str255; visible: Boolean; behind: WindowPtr; goAway: Boolean; refcon: longint): WindowPTr;
//	procedure FlushDWindow (theWind: WindowPtr; byteCount: longint);
	procedure FlushDisplay;
	procedure GetDWindow (var theWind: WindowPtr);
//	procedure SetDWindowFlush (theWind: WindowPtr; maxText, flushAmt: longint);
//	procedure SetDWindowNotify (theWind: WindowPTr; p: TSBooleanProcPtr);
	procedure SetDWindowPos (theWind: WindowPtr; lineNum: integer);
	procedure SetDWindowStyle (theWind: WindowPtr; font, size, wrap, just: integer);
	function GetDWindowTE (theWind: WindowPtr): TXNObject;
	function IsDWindow (theWind: WindowPtr): Boolean;
	procedure TransDisplayInit;
	
	// ADDITIONS for 2.0:
	// TransDisplay HIViews!
	procedure NewDView(view: HIViewRef);overload; // Install HIView to be used as displayview
	procedure NewDView(window: WindowPtr; signature:OSType; id:SInt32);overload;
	procedure SetDView(view: HIViewRef);overload; // Set a HIView as current display
	procedure SetDView(window:WindowRef; signature:OSType; id:SInt32);overload;	// + overloaded versions for passing a view spec

implementation

	type
		DIPtr = ^DisplayInfo;
//		DIHandle = ^DIPtr;
		DisplayInfo = record
			dWind: WindowPtr;			{ display window (can be nil) }
			dTE: TXNObject;				{ window text            }
//			dActivate: TSBooleanProcPtr;	{ notification procedure }
			dMaxText: Longint;			{ max text length        }
			dFlushAmt: Longint;			{ amount to autoflush    }
			dNext: DIPtr;			{ next window structure  }
			
			dHIView: HIViewRef;			{ display HIView (can be nil) }
		end;

var
	currentDisplay: DIPtr;
	dwList: DIPtr; // Root to list of displays

	function GetDIFromWindow(theWind: WindowPtr): DIPtr;
	var
		di: DIPtr;
	begin
		// Search list for window
		di := dwList;
		while di <> nil do
		begin
			if di^.dWind <> nil then
				if di^.dWind = theWind then
					return di;
			di := di^.dNext;
		end;
		return nil;;
	end;

	procedure SetDWindow (theWind: WindowPtr);
	var
		di: DIPtr;
	begin
		di := GetDIFromWindow(theWind);
		if di <> nil then
			currentDisplay := di;
	end;

	function IsDWindow (theWind: WindowPtr): Boolean;
	begin
		return (GetDIFromWindow(theWind) <> nil);
	end;

	function GetDIFromView(view: HIViewRef): DIPtr;
	var
		di: DIPtr;
	begin
		// Search list for view
		di := dwList;
		while di <> nil do
		begin
			if di^.dHIView <> nil then
				if di^.dHIView = view then
					return di;
			di := di^.dNext;
		end;
		return nil;
	end;

	procedure SetDView(view: HIViewRef);overload; // Set a HIView as current display
	var
		di: DIPtr;
	begin
		di := GetDIFromView(view);
		if di <> nil then
			currentDisplay := di;
	end;

	function IsDView (view: HIViewRef): Boolean;overload;
	begin
		return (GetDIFromView(view) <> nil);
	end;

	procedure SetDView(window:WindowRef; signature:OSType; id:SInt32);overload; // Set a HIView as current display
	var
		theControlID: ControlID;
		view: HIViewRef;
		err: OSErr;
	begin
		theControlID.id		:= id;
		theControlID.signature	:= signature;
		err := GetControlByID( window, theControlID, view );
		SetDView(view);
	end;

	procedure TransDisplayInit;
	begin
		{No longer needed}
	end;



{	Remove the display window from the list, and dispose of it.}
{	Since the clobber procedure is never called except for real display}
{	windows, and since the list must therefore be non-empty, it is}
{	not necessary to check the legality of the window or that the}
{	window's in the list.}

{	Must do SetDWindow (nil) to turn output off, if the window being}
{	clobbered is the current output window.}

	procedure Clobber;
		var
			h, h2: DIPtr;
			keepgoing: Boolean;
			disposableWindow: WindowPtr;
			curPort: CGrafPtr;
	begin
//		SyncGlobals(nil);					{ sync to current port }

		GetPort(curPort);
		disposableWindow := GetWindowFromPort(curPort);
		if disposableWindow = nil then
			Exit(Clobber); /// ERROR!
		
		if disposableWindow = currentDisplay^.dWind then	{ is it the first window in list? }
			SetDWindow(nil);
		if dwList^.dWind = disposableWindow then	{ found it }
			begin
				h2 := dwList;
				dwList := dwList^.dNext;
			end
		else
			begin
				h := dwList;
				keepgoing := true;
				while (h <> nil) and keepgoing do
					begin
						h2 := h^.dNext;
						if h2^.dWind = disposableWindow then
							begin
								h^.dNext := h2^.dNext;
								keepgoing := false;
							end;
						h := h2;
					end;
			end;
		TXNDeleteObject(h2^.dTE);			{ toss text record }
		DisposeWindow(disposableWindow);		{ toss window and scroll bar }
		DisposePtr(Ptr(h2));				{ get rid of information structure }
	end;


{	Create and initialize a display window and the associated data}
{	structures, and return the window pointer.  Install window in}
{	list of display windows.}

	procedure SetupDWindow(dispWind: WindowPtr);
	var
		//savePort: GrafPtr;
		dInfo: DIPtr;
		dummy: Boolean;
		err: OSErr;
		dispTE: TXNObject;
		ignore: LongWord;
	begin
		dispTE := nil;
		
		dummy := SkelWindow(dispWind, nil{@Mouse}, nil, nil{@Update}, nil{@Activate}, nil, @Clobber, nil, false);
	{ the window }
		{ mouse click handler }
		{ key clicks are ignored }
		{ window updating procedure }
		{ window activate/deactivate procedure }
		{ TransSkel hides window if no close proc }
		{ (generates deactivate event) }
		{ window disposal procedure }
		{ no idle proc }
		{ irrelevant since no idle proc }

{	Create the TE record used for text display.  Use defaults for}
{	display characteristics.  Setting window style overhauls}
{	display, so can cancel and update event pending for the window.}

		err := TXNNewObject(nil,  dispWind, nil, // defaults to window bounds
				kTXNWantVScrollBarMask or kTXNDrawGrowIconMask,
				kTXNTextEditStyleFrameType, 'TEXT',
				kTXNSystemDefaultEncoding, dispTE, ignore, nil);

{	Get new information structure, attach to list of known display}
{	windows.}

		dInfo := DIPtr(NewPtr(sizeof(DisplayInfo)));

		dInfo^.dNext := dwList;
		dwList := dInfo;
		dInfo^.dWind := dispWind;
		dInfo^.dHIView := nil;
		dInfo^.dTE := dispTE;

//		SetDWindowStyle(dispWind, d_font, d_size, d_wrap, d_just);

{	Make window current display output window}

		SetDWindow(dispWind);
	end;

{	Create and initialize a display window and the associated data}
{	structures, and return the window pointer.  Install window in}
{	list of display windows.}

{	The parameters are similar to those for NewWindow.  See Inside}
{	Macintosh.}

//	function NewDWindow: WindowPtr;
	function NewDWindow (bounds: Rect; title: Str255; visible: Boolean; behind: WindowPtr; goAway: Boolean; refcon: longint): WindowPTr;
	var
		dispWind: WindowPtr;
	begin
		dispWind := NewWindow(nil, bounds, title, visible, documentProc, behind, goAway, refCon);
		SetUpDWindow(dispWind);
		NewDWindow := dispWind;
	end;

//	function GetDWindowTE: TXNObject;
	function GetDWindowTE (theWind: WindowPtr): TXNObject;
		var
			dInfo: DIPtr;
	begin
		dInfo := GetDIFromWindow(theWind);

		if dInfo = nil then {GetDInfo(theWind)}
			GetDWindowTE := nil
		else
			GetDWIndowTE := dInfo^.dTE;
	end;

	procedure NewDView(view: HIViewRef);overload; // Install HIView to be used as displayview
	var
		dInfo: DIPtr;
	begin
		if view <> nil then
		begin
			dInfo := DIPtr(NewPtr(sizeof(DisplayInfo)));
	
			dInfo^.dNext := dwList;
			dwList := dInfo;
			dInfo^.dWind := nil;
			dInfo^.dHIView := view;
			dInfo^.dTE := HITextViewGetTXNObject(view);

//		SetDWindowStyle(dispWind, d_font, d_size, d_wrap, d_just);

{	Make view current display output view}

			SetDView(view);
		end;
	end;

	procedure NewDView(window: WindowPtr; signature:OSType; id:SInt32);overload; // Install HIView to be used as displayview
	var
		theControlID: ControlID;
		view: HIViewRef;
		err: OSErr;
	begin
		theControlID.id		:= id;
		theControlID.signature	:= signature;
		err := GetControlByID( window, theControlID, view );
		if err = noErr then
			NewDView(view);
	end;

	// Flushes the current display. Replaces FlushDWindow.
	procedure FlushDisplay;
	var
		err: OSErr;
	begin
		if currentDisplay <> nil then
			err := TXNSetData(currentDisplay^.DTE, kTXNTextData, nil, 0, kTXNStartOffset, kTXNEndOffset);
	end;

	procedure SetDWindowStyle (theWind: WindowPtr; font, size, wrap, just: integer);
	var
		te: TXNObject;
		typeAttr: array [1..4] of TXNTypeAttributes;
		err: OSErr;
	begin
		te := GetDWindowTE;
		if te = nil then Exit(SetDWindowStyle);
		
		typeAttr[1].tag := kTXNQDFontStyleAttribute;
		typeAttr[1].size := kTXNQDFontStyleAttributeSize;
		typeAttr[1].data.dataValue := Longint(0); // normal
		
		typeAttr[2].tag := kTXNQDFontFamilyIDAttribute;
		typeAttr[2].size := kTXNQDFontFamilyIDAttributeSize;
		typeAttr[2].data.dataValue := font shl 16;
	
		typeAttr[3].tag := kTXNQDFontSizeAttribute; // Storlek
		typeAttr[3].size := kTXNQDFontSizeAttributeSize;
		typeAttr[3].data.dataValue := size shl 16; // 9! 12?
		err := TXNSetTypeAttributes(te, 3, @typeAttr[1], kTXNStartOffset, kTXNEndOffset);
	end;

{ ------------------------------------------------------------ }
{						Output Routines							}
{ ------------------------------------------------------------ }

{}
{	Write text to display area if output is on (curDispWind != nil).}
{	DisplayText is the fundamental output routine.  All other}
{	output calls map (eventually) to it.}

{	First check whether the insertion will cause overflow and flush}
{	out some stuff if so.  Insert new text at the end, then test}
{	whether lines must be scrolled to get the new stuff to show up.}
{	If yes, then do the scroll.  Set values of scroll bar properly}
{	and highlight as appropriate.}

{	The current port is preserved.  Since all output calls end up}
{	here, it's the only output routine that has to save the port}
{	and check whether output is on.}

	procedure DisplayText (theText: Ptr; len: longint);
	var
		err: OSErr;
	begin
		if currentDisplay <> nil then
			err := TXNSetData(currentDisplay^.dTE, kTXNTextData, theText, len, kTXNEndOffset, kTXNEndOffset);
	end;

{	Derived output routines:}

{	DisplayString	Write (Pascal) string}

{	DisplayLong		Write value of long integer}
{	DisplayInt		Write value of integer}
{	DisplayChar		Write character}

{	DisplayHexLong	Write value of long integer in hex (8 digits)}
{	DisplayHexInt	Write value of integer in hex (4 digits)}
{	DisplayHexChar	Write value of character in hex (2 digit)}

{	DisplayBoolean	Write boolean value}
{	DisplayLn		Write carriage return}

	procedure DisplayString(theStr: Str255);overload;
		var
			myPtr: Ptr;
	begin
		myPtr := Ptr(longint(@theStr) + 1);
		DisplayText(myPtr, longint(length(theSTr)));
	end;

	procedure DisplayString(theStr: AnsiString);overload;
	begin
		DisplayText(@theStr[1], Length(theStr));
	end;

	procedure DisplayLong;
		var
			s: Str255;
	begin
		NumToString(l, s);
		DisplayString(s);
	end;

	procedure DisplayInt;
	begin
		DisplayLong(longint(i));
	end;

	procedure DisplayChar;
	begin
		DisplayString(AnsiString(c));
// Avoid 16-bit char problems!
//		myPtr := @c;
//		myPtr := Ptr(longint(myPtr) + 1);
//		DisplayText(myPtr, longint(1));
	end;

	procedure DisplayLn;
	begin
		DisplayChar(char(13));
	end;

	procedure DisplayBoolean;
	begin
		if b then
			DisplayString('True')
		else
			DisplayString('False');
	end;

	procedure HexByte (value: integer);	{value should be 0..15}
	begin
		if value < 10 then
			DisplayChar(char(value + integer('0')))
		else
			DisplayChar(char(value + (integer('a') - 10)));
	end;

	procedure DisplayHexChar;
	begin
		HexByte(integer(BitAnd(BitShift(longint(c), -4), $0000000f)));
		HexByte(integer(BitAnd(longint(c), $0000000f)));
	end;

	procedure DisplayHexInt;
	begin
		DisplayHexChar(char(BitAnd(BitShift(longint(i), -8), $000000ff)));
		DisplayHexChar(char(BitAnd(longint(i), $000000ff)));
	end;

	procedure DisplayHexLong;
	begin
		DisplayHexInt(Integer(BitAnd(BitShift(l, -16), $0000ffff)));
		DisplayHexInt(integer(Lo(l)));
	end;


end.






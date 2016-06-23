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

// 091229: TransSkel5/Cocoa version.
// TransDisplay 3.0a1

// 101210: Minor synchronizations with changes in TS.
// 110411: Renamed one of the DisplayString functions to DisplayShortString,
// because the two were too similar to be overloaded.

{$mode objfpc}
{$modeswitch objectivec1}

unit TransDisplay5;
interface

	uses
		CocoaAll, MacOSAll, TransSkel5, SkelViewUnit;

	procedure SetDWindow (theWind: NSWindow);
	procedure DisplayString (theStr: String);
	procedure DisplayShortString (theStr: AnsiString);
	procedure DisplayHexLong (l: longint);
	procedure DisplayHexInt (i: integer);
	procedure DisplayHexChar (c: char);
	procedure DisplayBoolean (b: Boolean);
	procedure DisplayChar (c: char);
	procedure DisplayInt (i: integer);
	procedure DisplayLong (l: longint);
	procedure DisplayLn;
	procedure DisplayText (theText: Pointer; len: longint);
//	function GetNewDWindow (resourceNum: integer; behind: NSWindow): NSWindow;
	function NewDWindow (x, y, w, h: Real; title: String; front: Boolean): NSWindow;
//	function NewDWindow (bounds: Rect; title: Str255; visible: Boolean; behind: NSWindow; goAway: Boolean; refcon: longint): WindowPTr;
//	procedure FlushDWindow (theWind: NSWindow; byteCount: longint);
	procedure FlushDisplay;
//	procedure GetDWindow (var theWind: NSWindow);
//	procedure SetDWindowFlush (theWind: NSWindow; maxText, flushAmt: longint);
//	procedure SetDWindowNotify (theWind: WindowPTr; p: TSBooleanProcPtr);
//	procedure SetDWindowPos (theWind: NSWindow; lineNum: integer);
	procedure SetDStyle (font: AnsiString; size, wrap, just: integer);
//	procedure SetDWindowStyle (theWind: NSWindow; font: AnsiString; size, wrap, just: integer);
//	procedure SetDViewStyle (te: NSTextView; font: AnsiString; size, wrap, just: integer);
//	procedure SetDWindowStyle (theWind: NSWindow; font, size, wrap, just: integer);
	function GetDWindowTE (theWind: NSWindow): NSTextView{TXNObject};
	function IsDWindow (theWind: NSWindow): Boolean;
	procedure TransDisplayInit;
	
	// ADDITIONS for 2.0:
	// TransDisplay HIViews!
	procedure NewDView(view: NSTextView);overload; // Install HIView to be used as displayview
	procedure NewDView(window: NSWindow; signature: String; id:SInt32);overload;
	procedure SetDView(view: NSTextView);overload; // Set a HIView as current display
	procedure SetDView(window: NSWindow; signature: String; id:SInt32);overload; // + overloaded versions for passing a view spec

implementation

	type
		DIPtr = ^DisplayInfo;
//		DIHandle = ^DIPtr;
		DisplayInfo = record
			dWind: NSWindow;			{ display window (can be nil) }
//			dTE: TXNObject;				{ window text            }
//			dActivate: TSBooleanProcPtr;	{ notification procedure }
			dMaxText: Longint;			{ max text length        }
			dFlushAmt: Longint;			{ amount to autoflush    }
			dNext: DIPtr;			{ next window structure  }
			
			dHIView: NSTextView;			{ display HIView (can be nil) }
		end;

var
	currentDisplay: DIPtr;
	dwList: DIPtr; // Root to list of displays

	function GetDIFromWindow(theWind: NSWindow): DIPtr;
	var
		di: DIPtr;
	begin
		// Search list for window
		di := dwList;
		while di <> nil do
		begin
			if di^.dWind <> nil then
				if di^.dWind = theWind then
				begin
					result := di;
					Exit;
				end;
			di := di^.dNext;
		end;
		result := nil;
	end;

	procedure SetDWindow (theWind: NSWindow);
	var
		di: DIPtr;
	begin
		di := GetDIFromWindow(theWind);
		if di <> nil then
			currentDisplay := di;
	end;

	function IsDWindow (theWind: NSWindow): Boolean;
	begin
		result := (GetDIFromWindow(theWind) <> nil);
	end;

	function GetDIFromView(view: NSTextView): DIPtr;
	var
		di: DIPtr;
	begin
		// Search list for view
		di := dwList;
		while di <> nil do
		begin
			if di^.dHIView <> nil then
				if di^.dHIView = view then
				begin
					result := di;
					Exit;
				end;
			di := di^.dNext;
		end;
		result := nil;
	end;

	procedure SetDView(view: NSTextView);overload; // Set a HIView as current display
	var
		di: DIPtr;
	begin
		di := GetDIFromView(view);
		if di <> nil then
			currentDisplay := di;
	end;

	function IsDView (view: NSTextView): Boolean;overload;
	begin
		result := (GetDIFromView(view) <> nil);
	end;

	procedure SetDView(window: NSWindow; signature:String; id:SInt32);overload; // Set a HIView as current display
	var
		view: NSTextView;
	begin
		view := NSTextView(SkelGetIndView(window, signature, id));
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

	procedure Clobber(w: NSWindow; userData: Pointer);
//		var
//			h, h2: DIPtr;
//			keepgoing: Boolean;
//			curPort: CGrafPtr;
	begin
(*		
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
		disposableWindow^.close;
		DisposePtr(Ptr(h2));				{ get rid of information structure }
*)
	end;


{	Create and initialize a display window and the associated data}
{	structures, and return the window pointer.  Install window in}
{	list of display windows.}

	procedure SetupDWindow(dispWind: NSWindow);
	var
//		r: Rect;
		//savePort: GrafPtr;
		dInfo: DIPtr;
//		dummy: Boolean;
//		err: OSErr;
//		dispTE: TXNObject;
//		ignore: LongWord;
		f: NSRect;
		v: NSTextView;
	begin
//		dispTE := nil;

		dInfo := DIPtr(NewPtr(sizeof(DisplayInfo)));
		
		SkelSetWindowHandler(dispWind, nil{mouse}, nil{key}, SkelDrawProcPtr(nil){draw}, nil{close}, @Clobber, dInfo);
		
//		dummy := SkelWindow(dispWind, nil{@Mouse}, nil, nil{@Update}, nil{@Activate}, nil, @Clobber, nil, false);
		
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
		
//		err := TXNNewObject(nil,  dispWind, nil, // defaults to window bounds
//				kTXNWantVScrollBarMask or kTXNDrawGrowIconMask,
//				kTXNTextEditStyleFrameType, 'TEXT',
//				kTXNSystemDefaultEncoding, dispTE, ignore, nil);
		
		v := NSTextView(GetWindowContentView(dispWind));
		f := GetViewFrame(v);
		dInfo^.dHIView := SkelNewTextView(v, f.origin.x,f.origin.y,f.size.width,f.size.height, '');
//	    dInfo^.dHIView.setAutoresizingMask(NSViewHeightSizable or NSViewWidthSizable);
		
{	Get new information structure, attach to list of known display windows.}
		
		dInfo^.dNext := dwList;
		dwList := dInfo;
		dInfo^.dWind := dispWind;
//		dInfo^.dHIView := nil;
//		dInfo^.dTE := dispTE;

//		SetDWindowStyle(dispWind, d_font, d_size, d_wrap, d_just);

{	Make window current display output window}

		SetDWindow(dispWind);
	end;

{	Create and initialize a display window and the associated data}
{	structures, and return the window pointer.  Install window in}
{	list of display windows.}

	function NewDWindow (x, y, w, h: Real; title: String; front: Boolean): NSWindow;
	var
		dispWind: NSWindow;
	begin
		dispWind := SkelNewWindow(x, y, w, h, title, front);
//		dispWind := NewWindow(nil, bounds, title, visible, documentProc, behind, goAway, refCon);
		SetUpDWindow(dispWind);
		NewDWindow := dispWind;
	end;

	function GetDWindowTE (theWind: NSWindow): NSTextView;
		var
			dInfo: DIPtr;
	begin
		dInfo := GetDIFromWindow(theWind);
		
		if dInfo = nil then {GetDInfo(theWind)}
			GetDWindowTE := nil
		else
			GetDWIndowTE := dInfo^.dHiView; // TE;
	end;

	procedure NewDView(view: NSTextView);overload; // Install HIView to be used as displayview
	var
//		err: OSErr;
		dInfo: DIPtr;
	begin
		if view <> nil then
		begin
			dInfo := DIPtr(NewPtr(sizeof(DisplayInfo)));
			
			dInfo^.dNext := dwList;
			dwList := dInfo;
			dInfo^.dWind := nil;
			dInfo^.dHIView := view;
//			dInfo^.dTE := HITextViewGetTXNObject(view);

//		SetDWindowStyle(dispWind, d_font, d_size, d_wrap, d_just);
			dInfo^.dHiView.setEditable(false);

{	Make view current display output view}

			SetDView(view);
		end;
	end;

	procedure NewDView(window: NSWindow; signature: String; id:SInt32);overload; // Install HIView to be used as displayview
	var
		view: NSTextView; // HIViewRef;
	begin
		view := NSTextView(SkelGetIndView(window, signature, id));
		if view <> nil then
			NewDView(view);
	end;

	// Flushes the current display. Replaces FlushDWindow.
	procedure FlushDisplay;
	begin
		if currentDisplay <> nil then
			currentDisplay^.dHiView.replaceCharactersInRange_withString(NSMakeRange(0, currentDisplay^.dHiView.textStorage.length), StringToNSString(''));
	end;

	procedure SetDStyle (font: AnsiString; size, wrap, just: integer);
	var
//		err: OSErr;
		f: NSFont;
	begin
		if currentDisplay <> nil then
		begin
			currentDisplay^.dHiView.replaceCharactersInRange_withString(NSMakeRange(0, currentDisplay^.dHiView.textStorage.length), StringToNSString(''));
			f := NSFont.fontWithName_size(StringToNSString(font), size);
			if f = nil then Exit;
			currentDisplay^.dHiView.setFont(f);
		end;
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

	procedure DisplayText (theText: Pointer; len: longint);
	var
//		err: OSErr;
		saveSelection: NSRange;
		s: AnsiString;
	begin
		if currentDisplay <> nil then
//			err := TXNSetData(currentDisplay^.dTE, kTXNTextData, theText, len, kTXNEndOffset, kTXNEndOffset);
		begin
			saveSelection := currentDisplay^.dHiView.selectedRange;
			
			currentDisplay^.dHiView.setEditable(true);
			currentDisplay^.dHiView.setSelectedRange(NSMakeRange(currentDisplay^.dHiView.textStorage.length, 0));
			SetLength(s, len);
			BlockMoveData(theText, @s[1], len);
			currentDisplay^.dHiView.scrollRangeToVisible(NSMakeRange(currentDisplay^.dHiView.textStorage.length, 0));
			
			currentDisplay^.dHiView.insertText(StringToNSString(s));
			currentDisplay^.dHiView.setEditable(false);
//			currentDisplay^.dHiView.replaceCharactersInRange_withString(range: NSRange; aString: NSString); message 'replaceCharactersInRange:withString:';
			
			currentDisplay^.dHiView.setSelectedRange(saveSelection);
		end;
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

	procedure DisplayString(theStr: String);
		var
			myPtr: Pointer;
	begin
		myPtr := @theStr[1]; // Ptr(longint(@theStr) + 1);
		DisplayText(myPtr, longint(Length(theStr)));
	end;

	procedure DisplayShortString(theStr: AnsiString);
	begin
		DisplayText(@theStr[1], Length(theStr));
	end;

	procedure DisplayLong (l: longint);
		var
			s: Str255;
	begin
		NumToString(l, s);
		DisplayString(s);
	end;

	procedure DisplayInt (i: integer);
	begin
		DisplayLong(longint(i));
	end;

	procedure DisplayChar (c: char);
//		var
//			myPtr: Ptr;
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

	procedure DisplayBoolean (b: Boolean);
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

	procedure DisplayHexChar (c: char);
	begin
		HexByte(integer(BitAnd(BitShift(longint(c), -4), $0000000f)));
		HexByte(integer(BitAnd(longint(c), $0000000f)));
	end;

	procedure DisplayHexInt (i: integer);
	begin
		DisplayHexChar(char(BitAnd(BitShift(longint(i), -8), $000000ff)));
		DisplayHexChar(char(BitAnd(longint(i), $000000ff)));
	end;

	procedure DisplayHexLong (l: longint);
	begin
		DisplayHexInt(Integer(BitAnd(BitShift(l, -16), $0000ffff)));
		DisplayHexInt(integer(Lo(l)));
	end;

end.

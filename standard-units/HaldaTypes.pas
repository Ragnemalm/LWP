unit HaldaTypes;
interface
	uses
		MacOSAll, QDCG;

// SysDept types
type
	HaldaScrollbar = ControlRef;
	HaldaWindow = WindowPtr;
	HaldaControl = HIViewRef;
	HaldaRect = QDCG.Rect;
	HaldaPoint = QDCG.Point;
	HaldaRegion = array of HaldaRect;

	type
		HaldaColor = packed record
			case SInt16 of
//				0: (red, green, blue, alpha: UnsignedByte;);
				0: (alpha, blue, green, red: UnsignedByte;); // Intel order?
				1: (col: UInt32;);
		end;
		
		FormatRec = record
			style: Integer;
			color: HaldaColor; {Eller annan färgspec?}
			formatEnd: Longint;
		end;

		HaldaViewPtr = ^HaldaViewRec;
		HaldaInvalProc = procedure(view: HaldaViewPtr);
		HaldaScrollProc = procedure(view: HaldaViewPtr);
		HaldaRec = record
				text: AnsiString; {Skall bli AnsiString}
				selStart, selEnd: Longint;
				
//				numFormats: Longint;
				format: array of FormatRec; {Dynamisk i FPC}
				
//				numRowStarts: Longint; {Kan elimineras i FPC, Length(rowStarts)}
				rowStarts: array of Longint; {Dynamisk i FPC}
				
				fontName: AnsiString;
				fontSize: Longint;
				
				rowHeight: Longint; // Can be integer
				charWidth: Real; // Must be real
				ascent: Longint; // Can be integer
				mouseClickPos: Longint;
				prevClickTime: Longint;
				clickCount: Longint;
				singleWaiting: Boolean; {Possible single click done}
				doubleClickLimit, singleClickLimit: Longint;
				
				tabWidth: Longint; {# of characters}
				
				// Array of HaldaViews?
				views: array of HaldaViewPtr;
				focusedView: Longint;
								
				{Callbacks}
				{Key filter (for HIView)?}
				{Draw row callback}
				{Draw string callback}
				{System dependent features by callbacks? Timer, inval?}
			end;
		HaldaPtr = ^HaldaRec;

		HaldaViewRec = record
				viewRect: HaldaRect;
				leftMargin: Longint; // Extra space
				topRow: Longint;
				leftCol: Longint;
				window: HaldaWindow; // Change to HIView?
				hiview: HaldaControl;
				h: HaldaPtr;
				bgColor: HaldaColor;
				
				flashOn: Boolean; // Is cursor visible?
				lastp1, lastp2: HaldaPoint; // Last points for flashing cursor
				pressed: Boolean; // Has this gotten a click in it with no mouseup since?
				invalLevel: Longint;
				
				// Scroll bars?
				hScroll, vScroll: HaldaScrollbar;
				// callbacks?
				additionalInval: HaldaInvalProc;
				liveScrollProc: HaldaScrollProc;
//				hScrollbarChanged, vScrollbarChanged: XXProc;
				auxDataPtr: Pointer;
			end;

		
implementation

end.

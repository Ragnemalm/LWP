{Halda, monospaced text editor for programming}
{A "typewriter" text editor}
{DRAFT}
{Supports, so far:}
{255 characters - will be 2 gig when moved to AnsiString DONE}
{selection}
{all arrow keys, with shift and alt}
{drawing in a rectangular view}
{tabs, clicks, double-clicks}
{No wrap at edge; you don't want that when programming}
{To do:}
{• cmd-arrow to end of line or top/bottom}
{• callbacks for special needs, including every drawn line}
{• multiple styles and colors (still monospaced and single size)}
{• utilities for load/save including CR/CRLF/LF conversions}
{• click-and-drag on selections}
{• double-click, triple-click}
{• Drag Manager}
{• optimizations}
{• hidden rows (for code folding)}
{• convert to FPC/AnsiStrings}
{• Support for other drawing systems than QuickDraw (CG etc).}
{• Multi-button support}
{• Timing for caret}
{• Support for scrolling}
{• Built-in in a scrolling HIView}
{• Auto-indent option}
{• Auto-format option}
{• Proper selection color}
{• More error checks}

// Original draft feb-march 2008.
// First written in Think Pascal, then ported to FPC.
// Abandoned since I ran into problems with the FPC port.
// 120319: Alive again. Found a problem in TransSkel.
// Demo with 1/10 second events instead of 1 second.
// Worked on a future version, using QDCG.
// Considering making an OpenGL version instead.
// 131127: Continuing on QuickDraw (and planning for OpenGL). Added HGetVisibleRows. Added HSetStyle.
// 131209: Added HSelectAll, HNew, HNewView, HDispose, HDisposeView... and more.
// 131211: PosToPoint renamed to HOffsetToPoint, and exported
// 131212: Separated system dependent code, as preparation for cross-platform solutions.
// Created HaldaTypes and HaldaSysDept for this purpose. Many new calls for glue.
// 131213: Added left margin, and it was soo much easier than... the others.
// 131213: Corrected tab support to be position dependent, not always tabSize blanks.
// 131223: Exports HOffsetToRow (former HGetRow). Variant of HInsert.
// 131224: Added built-in scrollbar support.
// 131225: Support for scrolling during click-and-drag
// 140103: Lots of bug fixes... Many small changes. Fixed remaining tab problems in HDraw!
// 140223: Fixed fatal bug in HKeyCustom!
// 140310: Floating-point width changed to more accurate values.!
// 140311: Live scrolling fixed!
// 140416: HInvalSelection now works on all views.

// Current:
// Error in HOffsetToPoint, 1 space wrong sometimes. Even 2.
// Click to right of text does not return end of line FIXED
// Still not supporting variable width (fairly hard)
// Still not used with split view
// Clipping missing
// Selection below upper end -> crash. Also beyond end. Range checking problem at both ends! FIXED
// Big bad error (including crashes and no flashing insertion point) which was caused by a missing init of col in HOffsetToPoint FIXED
// Character width no longer integer. Must be accounted for, causes slight errors in selection and caret.
// Caret is sometimes left unerased.

{$R+}
{$mode fpc}

unit Halda;
interface
	uses
		//MacOSAll,
		HaldaTypes, HaldaSysDept, Math; // , TransSkel4;

	procedure HSetup (h: HaldaPtr);
	procedure HSetFont(h: HaldaPtr; fontName: AnsiString; fontSize: Longint);
	procedure HSetTextGeometry(h: HaldaPtr; rowHeight, charWidth, ascent: Longint);
	procedure HDraw (theView: HaldaViewPtr);
	procedure HInsertAt(h: HaldaPtr; s: AnsiString; selStart, selEnd: Longint);
	procedure HInsert(h: HaldaPtr; s: AnsiString);
	procedure HInvalView(theView: HaldaViewPtr);
	procedure HInvalViews(h: HaldaPtr);
	procedure HInvalSelection (h: HaldaPtr);
	procedure HSetSelection (h: HaldaPtr; selStart, selEnd: Longint);
	procedure HGetSelection (h: HaldaPtr; var selStart, selEnd: Longint);
	procedure HSelectAll (h: HaldaPtr);
//	procedure HKey (h: HaldaPtr; theKey: Char; mods: Longint);
	type
		HInsertProc = procedure (h: HaldaPtr; s: AnsiString);
	procedure HKeyCustom (theView: HaldaViewPtr; theKey: Char; mods: Longint; MyHInsert: HInsertProc);
	procedure HKey (theView: HaldaViewPtr; theKey: Char; mods: Longint);
	procedure HMouse (theView: HaldaViewPtr; p: HaldaPoint; mods: integer);
//	procedure HMouseDown (h: HaldaPtr; theView: HaldaViewPtr; p: HaldaPoint; mods: integer);
//	procedure HMouseDrag (h: HaldaPtr; theView: HaldaViewPtr; p: HaldaPoint; mods: integer);
	procedure HMouseUp(theView: HaldaViewPtr; p: HaldaPoint; mods: integer);
	procedure HIdle (theView: HaldaViewPtr);
	procedure HMouseDrag(theView: HaldaViewPtr; p: HaldaPoint);
	
	procedure HSetText (h: HaldaPtr; s: AnsiString);
	function HGetText (h: HaldaPtr): AnsiString; overload;
	function HGetText (h: HaldaPtr; fromPos, count: Longint): AnsiString; overload;
	procedure HGetVisibleRows(theView: HaldaViewPtr; var firstRow, lastRow: Longint);

	procedure HSetStyle(h: HaldaPtr; selStart, selEnd: Longint; theStyle: Integer; color: HaldaColor);

	// Straight from TXN. Relevant?
	procedure HFocus(h: HaldaViewPtr; gotFocus: Boolean);
	procedure HSetScrollbarState(h: HaldaViewPtr; active: Boolean);
	procedure HShowSelection(theView: HaldaViewPtr; flag: Boolean); overload;
	procedure HShowSelection(h: HaldaPtr; flag: Boolean); overload;
	function HOffsetToPoint (theView: HaldaViewPtr; pos: Longint): HaldaPoint;
	function HPointToOffset (v: HaldaViewPtr; p: HaldaPoint): Longint;
	function HOffsetToRow (h: HaldaPtr; pos: Longint): Longint;

	procedure HDispose(h: HaldaPtr);
	procedure HDisposeView(view: HaldaViewPtr);
	function HNew: HaldaPtr;
	function HNewView(viewRect: HaldaRect; window: HaldaWindow; te: HaldaPtr): HaldaViewPtr;overload;
	function HNewView(window: HaldaWindow; te: HaldaPtr; hScroll, vScroll: Boolean): HaldaViewPtr;overload;
	procedure HSetBackgroundColor(view: HaldaViewPtr; bgColor: HaldaColor);
	procedure HAdjustScrollBarRange(theView: HaldaViewPtr);
	procedure HAdjustScrollBarRanges(h: HaldaPtr);
	procedure HMakeVisible(theView: HaldaViewPtr; pos: Longint);
//	procedure HAdjustTextEditArea(theView: HaldaViewPtr);
	procedure HAdjustTextEditArea(h: HaldaPtr; hScrollLeftMargin, hScrollRightMargin, vScrollTopMargin, vScrollBottomMargin,
																			textLeftMargin, textRightMargin, textTopMargin, textBottomMargin: Longint);

	function HMakeColor(col: Cardinal): HaldaColor;
	function HCopy(h: HaldaPtr): Integer;
	function HPaste(h: HaldaPtr): Integer;

implementation

{Om jag har rowStarts/rowHeight, NÄR uppdateras de?}

	const
		bs = Char(8);
		cr = Char(13);
		lf = Char(10);
		tab = Char(9);
		esc = Char(27);
		left = Char(28);
		right = Char(29);
		up = Char(30);
		down = Char(31);
		pup = Char(11);
		pdown = Char(12);
		del = Char(127);
		
		ctrlA = Char(1);
		ctrlE = Char(5);
		ctrlK = Char(11);
	
	procedure HSetFont(h: HaldaPtr; fontName: AnsiString; fontSize: Longint);
	begin
//		WriteLn('HSetFont to ', fontName);
		HSDSetFont(fontName, fontSize); // Needed when using QD or QDCG font info
		if h = nil then exit; // WriteLn('FANKEN');
		HSDGetSystemFontInfo(h^.rowHeight, h^.ascent, h^.charWidth, h^.prevClickTime);
		if h^.rowHeight = 0 then 
		begin
			WriteLn('Oh no, ROWHEIGHT ZERO!');
			WriteLn('fontName ', fontName);
			WriteLn('fontSize ', fontSize);
		end;
		h^.fontName := fontName;
		h^.fontSize := fontSize;
	end;
	
	procedure HSetup (h: HaldaPtr);
	begin
		{Systemberoende, textgeometri:}
		{Bör IFC-as, med bra default för olika system.}
		
		// Default font
		h^.fontName := 'Monaco';
		h^.fontSize := 10;
		HSDSetFont(h^.fontName, h^.fontSize);
		
		HSDGetSystemFontInfo(h^.rowHeight, h^.ascent, h^.charWidth, h^.prevClickTime);
				
		h^.text := ''; // #13;
		h^.selStart := 1;
		h^.selEnd := 1;
		h^.tabWidth := 4;
		h^.clickCount := 0;
		h^.singleWaiting := false;
		h^.doubleClickLimit := 40; {ticks, GetDblTime in old MacOS}
		h^.singleClickLimit := 50; {ticks}
		h^.focusedView := 0;
		
{Sätt arrayer till tomma}
		
		SetLength(h^.format, 1);
//		h^.numFormats := 1;
		h^.format[0].style := 0;
		h^.format[0].color.red := 0;
		h^.format[0].color.green := 0;
		h^.format[0].color.blue := 0;
		h^.format[0].color.alpha := 1;
		h^.format[0].formatEnd := 1;
	end;

	procedure HSetTextGeometry(h: HaldaPtr; rowHeight, charWidth, ascent: Longint);
	begin
		h^.rowHeight := rowHeight;
		if rowHeight = 0 then WriteLn('ROWHEIGHT is ZERO in HSetTextGeometry!');
		if h^.rowHeight = 0 then h^.rowHeight := 1;
		h^.ascent := charWidth; // Why are these swapped?
		h^.charWidth := ascent;
	end;

{Beräkna radstarter för HELA texten ≈ TECalText}
	procedure FindRowStarts (h: HaldaPtr);
		var
			i: Longint;
			numRowStarts: Longint;
	begin
		numRowStarts := 0;

{Första raden:}
		numRowStarts := 1;
		// Put some significant length on it so we don't have to re-alloc all the time
		if Length(h^.rowStarts) < 1024 then
			SetLength(h^.rowStarts, 1024);
		h^.rowStarts[0] := 1;

		for i := 1 to Length(h^.text) do
			begin
				if h^.text[i] = #13 then
					begin
						if numRowStarts > High(h^.rowStarts) then
							SetLength(h^.rowStarts, Length(h^.rowStarts)*2);
						h^.rowStarts[numRowStarts] := i + 1;
						numRowStarts := numRowStarts + 1;
					end;
			end;
		// Är det bättre att ha en större storlek och spara max separat?
// Vi skall INTE ha en ickerad på slutet!
// Denna motsvarar inte en CR och rör till HDraw!!
//		SetLength(h^.rowStarts, numRowStarts+1);
//		h^.rowStarts[numRowStarts] := Length(h^.text);
		
		if Length(h^.text) > 0 then
		begin
			if h^.text[Length(h^.text)] <> #13 then
			begin
				SetLength(h^.rowStarts, numRowStarts+1);
				h^.rowStarts[numRowStarts] := Length(h^.text) + 2 // Modifierad
			end
			else
			begin
				SetLength(h^.rowStarts, numRowStarts);
			end;
		end
		else
		begin
			SetLength(h^.rowStarts, 1);
			h^.rowStarts[0] := 2;
		end;
		
(*		
		SetLength(h^.rowStarts, numRowStarts+1);
		if Length(h^.text) > 0 then
			if h^.text[Length(h^.text)] = #13 then
				h^.rowStarts[numRowStarts] := Length(h^.text) + 1 // Modifierad
			else
				h^.rowStarts[numRowStarts] := Length(h^.text) + 2 // Modifierad
		else
			h^.rowStarts[numRowStarts] := 2;
*)
	end;

	function PrevWordLimit (h: HaldaPtr; pos: Longint): Longint;
	begin
		if pos <= 1 then
			Exit(1);
		pos := pos - 1;

// Better double-click: Judge by alphas vs non-alphas!
//		if not (h^.text[pos] in ['A'..'Z', 'a'..'z']) then

		while pos > 1 do
		begin
			pos := pos - 1;
			if not (h^.text[pos] in ['A'..'Z', 'a'..'z', '0'..'9', '+', '-', 'å', 'ä', 'ö', 'Å', 'Ä', 'Ö']) then
			begin
				PrevWordLimit := pos + 1;
				Exit(PrevWordLimit);
			end;
		end;
		PrevWordLimit := 1;
		Exit(PrevWordLimit);

// The rest is old version
		if h^.text[pos] in [' ', tab, cr, lf, ',', ';'] then
			while pos > 1 do
				begin
					pos := pos - 1;
					if not (h^.text[pos] in [' ', tab, cr, lf, ',', ';']) then
						begin
							PrevWordLimit := pos + 1;
							Exit(PrevWordLimit);
						end;
				end
		else // Backa till första bryttecken
			while pos > 1 do
				begin
					pos := pos - 1;
					if h^.text[pos] in [' ', tab, cr, lf, ',', ';'] then
						begin
							PrevWordLimit := pos + 1;
							Exit(PrevWordLimit);
						end;
				end;

		PrevWordLimit := 1;
	end;

	function NextWordLimit (h: HaldaPtr; pos: Longint): Longint;
	begin
	
		while pos < Length(h^.text) do
		begin
			pos := pos + 1;
			if not (h^.text[pos] in ['A'..'Z', 'a'..'z', '0'..'9', '+', '-', 'å', 'ä', 'ö', 'Å', 'Ä', 'Ö']) then
			begin
				NextWordLimit := pos;
				Exit(NextWordLimit);
			end;
		end;
		NextWordLimit := Length(h^.text);
		Exit(NextWordLimit);
	
// Rest is old	
		if pos > Length(h^.text) then
			Exit(Length(h^.text));
		if h^.text[pos] in [' ', TAB, CR, LF, ',', ';'] then {case 1: we are at a delimiter char}
			while pos < Length(h^.text) do
				begin
					if not (h^.text[pos] in [' ', TAB, CR, LF, ',', ';']) then {search to non-delimiter}
						begin
							NextWordLimit := pos;
							Exit(NextWordLimit);
						end;
					pos := pos + 1;
				end
		else {case 2: non-delimiter}
			while pos < Length(h^.text) do
				begin
					if h^.text[pos] in [' ', TAB, CR, LF, ',', ';'] then {search to delimiter}
						begin
							NextWordLimit := pos;
							Exit(NextWordLimit);
						end;
					pos := pos + 1;
				end;

		NextWordLimit := Length(h^.text);
	end;

type
	StylesArrType = array of Longint;
	
// New ExpandRow!
// Expand to a string AND array of styles
// Used for both drawing and calculating position
// Can help to calculate strings with in any style?

function GetStyleFromPosition(h: HaldaPtr; pos: Longint): Longint;
var
	start, finish, x: Longint;
begin
	if High(h^.format) < 0 then
	begin
		GetStyleFromPosition := -1;
		Exit(GetStyleFromPosition);
	end;
	if pos >= Length(h^.text) then
	begin
		GetStyleFromPosition := High(h^.format);
		Exit(GetStyleFromPosition);
	end;
	start := 0;
	finish := High(h^.format);
	while start < finish-1 do
	begin
		x := (start + finish) div 2;
		if h^.format[x].formatEnd < pos then
			start := x
		else
			finish := x;
	end;
	while (h^.format[start].formatEnd < pos) and (start < High(h^.format)) do
		start += 1;
	GetStyleFromPosition := start;
end;

function GetStyleFromPositionJUNK(h: HaldaPtr; pos: Longint): Longint;
var
	start, finish, x: Longint;
begin
	start := 0;
	finish := High(h^.format);
	while start < finish do
	begin
		x := (start + finish) div 2;
		if h^.format[x].formatEnd < pos then
			start := x
		else
			finish := x;
	end;
	GetStyleFromPositionJUNK := start;
end;

(*
// startingStyle is used to avoid searching for styles all the time.
function ExpandRowWithStyles(h: HaldaPtr; row: Longint; var styles: StylesArrType; startingStyle: Longint): AnsiString;
var
	styleIx: Longint;
begin
	styleIx := GetStyleFromPosition(h, h^.rowStarts[row]);
	
end;
*)

// Note: Looking up styles for a position must be speeded up! Binary search? Table?
// Or local search from previous start (top of screen)




// Take a row, convert TABs to spaces
// To use for StringWidth for finding clicks etc
// (Not for drawing; then we lose track of styles.)
// NOT USED! CharWidth from StringWidth of space works!
(*
function ExpandRow(h: HaldaPtr; row: Longint): AnsiString;
var
	i, actualTabSize, next, start, drawnSoFar: Longint;
	theString: AnsiString;
begin
			if row < High(h^.rowStarts) then
				next := h^.rowStarts[row + 1] - 2 // last character before row break
			else
				next := Length(h^.text);
			start := 1; // scroll-läge?
			drawnSoFar := 0; // Probably unnecessary here

			theString := Copy(h^.text, start, next - start + 1);

// Expand TABs. Note that "drawnSoFar" is needed to keep track of expansion of earlier parts of the same line
			i := 1;
			while i <= Length(theString) do
			begin
				if theString[i] = tab then
				begin
					actualTabSize := h^.tabWidth - (drawnSoFar + i-1) mod h^.tabWidth;
					if actualTabSize <= 0 then actualTabSize := h^.tabWidth;
					theString := ConCat(Copy(theString, 1, i - 1),
										Copy('        ', 1, actualTabSize),
										Copy(theString, i + 1, Length(theString) - i)); {testas}
				end;
				i := i + 1;
			end;
			ExpandRow := theString;
end;
*)

// Enklare än ExpandRow: Expandera det som är utanför, kolla om resultatet sticker in pga tabs
procedure CalcLeftChars(theView: HaldaViewPtr; line: Longint; var charsToLeft, tabSpill: Longint);
var
	s: AnsiString;
	pos, actualTabSize: Longint;
begin
//WriteLn('CalcLeftChars line ', line);
	pos := theView^.h^.rowStarts[line];
	s := '';
	while Length(s) < theView^.leftCol do
	begin
		if theView^.h^.text[pos] = tab then
		begin
			actualTabSize := theView^.h^.tabWidth - Length(s) mod theView^.h^.tabWidth;
			s := s + Copy('        ', 1, actualTabSize);
		end
		else
			s := s + theView^.h^.text[pos];
		pos := pos + 1;
		if pos > Length(theView^.h^.text) then
			Break;
		if theView^.h^.text[pos] = cr then
			Break;
	end;
	
//	if theView^.leftCol > 0 then
//		WriteLn('CalcLeftChars ', theView^.leftCol, ' gave ', Length(s))
//	else
//		WriteLn('leftCol is zero');
	
	charsToLeft := pos - theView^.h^.rowStarts[line]; // Number of actual chars not visible
	tabSpill := Length(s) - theView^.leftCol; // Number of steps into visible are made by tabs
//WriteLn('CalcLeftChars done');
end;



	// Förbättra så vi klarar ej monospace?
	function HPointToOffset (v: HaldaViewPtr; p: HaldaPoint): Longint;
		var
			row, col: Longint;
			r: HaldaRect;
			firstRow: Longint;
			firstChar: Longint;
			c, i, rowStart, rowEnd, actualTabSize: Longint;
			h: HaldaPtr;
	begin
		h := v^.h;
		r := v^.viewRect;
		r.left += v^.leftMargin; // Always adjust with left margin
		firstRow := v^.topRow;
		firstChar := v^.leftCol;

//		row := p.v - r.top; {Avstånd från topp}
//		row := row div h^.rowHeight; {Antal rader från toppen}
//		row := row + firstRow;
		row := Trunc((p.v - r.top) / h^.rowHeight) + firstRow;
		if row < 0 then
		begin
//			row := 0;
			HPointToOffset := 1;
			Exit(HPointToOffset);
		end;
		if row > High(h^.rowStarts) then
		begin
//			row := High(h^.rowStarts);
			HPointToOffset := Length(h^.text)+1;
			Exit(HPointToOffset);
		end;
		
//		col := p.h - r.left; {Avstånd från kant}
//		if col < 0 then col := 0; // Margin
//		col := (col + h^.charWidth div 2) div h^.charWidth; {Antal tecken från kant - avrundar}
//		col := col + firstChar; {Scroll-läge horisontellt}
		if h^.charWidth < 0.001 then
			h^.charWidth := 1;
		col := Trunc((p.h - r.left + h^.charWidth / 2) / h^.charWidth) + firstChar; {Antal tecken från kant - avrundar}
		// Behövs "margin" ovan?
		
		// New col
		// Start from right end, measure StringWidth per style!
		rowStart := h^.rowStarts[row]; // Start of row
		// Get style for position

		rowStart := h^.rowStarts[row];
		if rowStart < 1 then rowStart := 1; //??? Shouldn't rowStarts fix this?
		if row >= High(h^.rowStarts) then
			rowEnd := Length(h^.text)+1
		else
		begin
			rowEnd := h^.rowStarts[row + 1] - 1;
//			if rowEnd > Length(h^.text) then
//				rowEnd := Length(h^.text);
		end;
		

{tab support: a tab counts as several characters}
		c := 0;
		for i := rowStart to rowEnd do
			begin
				if i <= Length(h^.text) then // Might be at end of text
					if h^.text[i] = tab then
					begin
						actualTabSize := h^.tabWidth - c mod h^.tabWidth;
	//					if actualTabSize = 0 then actualTabSize := h^.tabWidth;
						c := c + actualTabSize;
	//					c := ((c div h^.tabWidth) + 1) * h^.tabWidth;
	//					c := c + h^.tabWidth; // FIXAS så tabs beror på radpos
					end
					else
						c := c + 1
				else
					c := c + 1;
				if c > col then
					begin
						HPointToOffset := i; {rowStart + c;}
						Exit(HPointToOffset);
					end;
			end;
		HPointToOffset := i; {rowStart + c;}
	end;

//HGetRow
	function HOffsetToRow (h: HaldaPtr; pos: Longint): Longint;
		var
			first, last, middle: Longint; {Binärsökningsvariabler}
	begin
{Hitta rad med positionen. Binärsökning i rowStarts!}
		first := 0;
		last := High(h^.rowStarts);

		while last > first do
			begin
				middle := (last + first) div 2;

				if h^.rowStarts[middle] <= pos then
					begin
						if first = middle then {denna eller nästa!}
							if h^.rowStarts[last] <= pos then
								middle := middle + 1
							else
								last := last - 1;

						first := middle;
					end
				else
					last := middle - 1;
			end;

{WriteLn(first : 1, ' ', pos : 1);}
		HOffsetToRow := first;
	end;


	function HOffsetToPoint (theView: HaldaViewPtr; pos: Longint): HaldaPoint;
		var
			row, col, add, i: Longint;
			p: HaldaPoint;
			r: HaldaRect;
			ph, pv: Real;
			actualTabSize: Longint;
			h: HaldaPtr;
	begin
//WriteLn('HOffsetToPoint');
		h := theView^.h;
		if h = nil then
			WriteLn('WARNING! NO H IN VIEW!');
		row := HOffsetToRow(h, pos);
		r := theView^.viewRect;
		r.left += theView^.leftMargin;

//WriteLn('HOffsetToPoint 2');
		
{BUGG: Saknar stöd för theView^.topRow och leftCol FIXAT? NEJ! Jo?}
	
//		if pos > Length(h^.text) then
//			pos := Length(h^.text);
// NEJ för pos får vara Length+1!

		col := 0;
		if row > High(h^.rowStarts) then
			col := pos - h^.rowStarts[High(h^.rowStarts)] {bara exakt sista skall kunna hända}
		else
			col := pos - h^.rowStarts[row];

//WriteLn('HOffsetToPoint 3, col = ', col);
	
{tab support: count tabs as several characters:}
		add := 0;
		if Length(h^.text) > 0 then // Avoid range error
		for i := h^.rowStarts[row] to h^.rowStarts[row] + col - 1 do
		begin
//WriteLn('HOffsetToPoint 3+, i = ', i);
// CRASH HERE
//WriteLn('loop from ', h^.rowStarts[row], ' to ', h^.rowStarts[row] + col - 1);
			if i <= Length(h^.text) then // Avoid error!
			if h^.text[i] = tab then // Range check error!!!
			begin
				// Width of tab depends on position
				actualTabSize := h^.tabWidth - (i - h^.rowStarts[row] + add) mod h^.tabWidth;
				add += actualTabSize - 1;
			end;
		end
		else // slight adjustment to make an empty file put the caret in the proper place!
			add := 1;
		col := col + add;

// CRASH BEFORE HERE
//WriteLn('HOffsetToPoint 4');
		
		ph := col * h^.charWidth + r.left - theView^.leftCol * h^.charWidth;
		pv := (row - theView^.topRow) * h^.rowHeight + r.top;
		// Keep in range for Point
//		if ph < -32768 then ph := -32768;
//		if ph > 32767 then ph := 32767;
//		if pv < -32768 then pv := -32768;
//		if pv > 32767 then pv := 32767;
// Lite extra marginal:
		if ph < -32568 then ph := -32568;
		if ph > 32567 then ph := 32567;
		if pv < -32568 then pv := -32568;
		if pv > 32567 then pv := 32567;
		p.h := ph;
		p.v := pv;
		HOffsetToPoint := p;
//WriteLn('HOffsetToPoint DONE');
	end;

//	function GetFirstStyle (h: HaldaPtr; pos: Longint): Longint;
//	begin
{Search style list for first style that reaches pos}
//	end;

// Size in # of chars
function SizeWithTabs (h: HaldaPtr; rowStart, fromPos, toPos: Longint): Longint;
	var
		theSize, i, rowSize: Longint;
begin
	theSize := 0; // Size of selection
	rowSize := 0; // Size from start of row (necessary for tabs to be correct)
	if rowStart < 1 then rowStart := 1;
	if toPos > Length(h^.text) then
		toPos := Length(h^.text);
	for i := rowStart {fromPos} to toPos do
	begin
{Check for tabs and make them the proper size!}
		if i >= fromPos then
			if h^.text[i] = tab then
				theSize := theSize + h^.tabWidth - rowSize mod h^.tabWidth
			else
				theSize := theSize + 1;
		if h^.text[i] = tab then
			rowSize := rowSize + h^.tabWidth - rowSize mod h^.tabWidth
		else
			rowSize := rowSize + 1;
	end;
	SizeWithTabs := theSize;
end;

{Regions are not system independent... but can they be?}
{YES if they are arrays of rectangles. Good idea.}
	function GetSelectionRegion (h: HaldaPtr; theView: HaldaViewPtr): HaldaRegion;
		var
			rangeStart, rangeEnd, v: Real;
			rangeStartChars, rangeEndChars, rangeStartPos, rangeEndPos, i, row1, row2: Longint;
			box: HaldaRect;
			selRgn: HaldaRegion;
			firstRow, lastRow: Longint;
	begin
		HGetVisibleRows(theView, firstRow, lastRow);

		row1 := HOffsetToRow(h, h^.selStart);
		row2 := HOffsetToRow(h, h^.selEnd);
		
		if row1 < firstRow then row1 := firstRow;
		if row2 > lastRow then row2 := lastRow;

		SetLength(selRgn, 0);

//		WriteLn('GetSelectionRegion selStart = ', h^.selStart);
//		WriteLn('GetSelectionRegion selEnd = ', h^.selEnd);
		if row2 > High(h^.rowStarts) then
			row2 := High(h^.rowStarts);
		
		if h^.selStart < h^.selEnd then
			for i := row1 to row2 do
				begin
					rangeStartPos := h^.rowStarts[i];
					if i < High(h^.rowStarts) then
						rangeEndPos := h^.rowStarts[i + 1] {lägg till -1 så funkar HOffsetToPoint-lösningen - men man kan inte markera CR!}
					else
						rangeEndPos := Length(h^.text);
					if h^.selStart > rangeStartPos then
						rangeStartPos := h^.selStart;
					if h^.selEnd < rangeEndPos then
						rangeEndPos := h^.selEnd;

//WriteLn('conv to tabs');

{Nya: Kräver egen tab-fixare (SizeWithTabs) men verkar funka bra}
//					CalcLeftChars(theView, i, charsToLeft, xxx);
					rangeStartChars := SizeWithTabs(h, h^.rowStarts[i], h^.rowStarts[i], rangeStartPos - 1); {chars from start of row}
					rangeEndChars := SizeWithTabs(h, h^.rowStarts[i], rangeStartPos, rangeEndPos - 1); {chars in range}
{Calc pixel widths}
//WriteLn('pixel width');
//					rangeStart := theView^.viewRect.left + theView^.leftMargin + (rangeStartChars - (charsToLeft + xxx)) * h^.charWidth;
//					rangeStart := theView^.viewRect.left + theView^.leftMargin + (rangeStartChars - xxx-theView^.leftCol) * h^.charWidth;
					rangeStart := theView^.viewRect.left + theView^.leftMargin + (rangeStartChars - theView^.leftCol) * h^.charWidth;
					rangeEnd := rangeStart + rangeEndChars * h^.charWidth;
{Gamla: Snygg men klarar inte tab}
{rangeStart := (rangeStart - h^.rowStarts[i]) * h^.charWidth + r.left - theView^.leftCol * h^.charWidth;}
{rangeEnd := (rangeEnd - h^.rowStarts[i]) * h^.charWidth + r.left - theView^.leftCol * h^.charWidth;}
{Must be within r too.}
					v := theView^.viewRect.top + h^.rowHeight * (i - theView^.topRow);
					HSDSetRect(box, rangeStart, v, rangeEnd, v + h^.rowHeight);
					if HSDSectRect(box, theView^.viewRect, box) then
					begin
//						FrameRect(box);
//						WriteLn('Adding rect to rgn ', Length(selRgn));
						HSDAddRectToRgn(selRgn, box);
					end;

{Lösning med HOffsetToPoint: enkelt men markerar inte CR}
{box.topLeft := HOffsetToPoint(h, theView, rangeStart);}
{box.botRight := HOffsetToPoint(h, theView, rangeEnd);}
{if box.right > r.right then}
{box.right := r.right;}
{v := r.top + h^.rowHeight * i;}
{box.top := v;}
{box.bottom := v + h^.rowHeight;}

{Blend selection}
{HMarkRect(box);}
{FrameRect(box);}

{ATT FIXA:}
{Borde göra en "selection region"-skapare, som sedan kan användas för att rita, dra runt, och invalidera.}

				end;
//		CloseRgn(selRgn);
		GetSelectionRegion := selRgn;
//		WriteLn('Final rgn is ', Length(selRgn));
	end;


// This is now ONLY done from HIdle. No, from HDraw
procedure FlashCaret(view: HaldaViewPtr; tempPort: Boolean);
var
	p, p2: HaldaPoint;
	// TEMPORARY
	fgColor: HaldaColor = (alpha: 255; blue: 0; green: 0; red: 0);
	whiteColor: HaldaColor = (alpha: 255; blue: 255; green: 255; red: 255);
begin
	// Ej efter scroll, väl?
//	WriteLn('COME ON, FLASH');

	HSDForeColor(view^.bgColor);
	HSDForeColor(whiteColor);
	HSDLine(view, view^.lastp1, view^.lastp2, tempPort, whiteColor);
	if view^.h^.selStart <> view^.h^.selEnd then
	begin
//		WriteLn('Big selection - NO FLASH');
		Exit;
	end;

	if not view^.flashOn then
	begin
//		WriteLn('FLASH');
		HSDForeColor(fgColor);
	end
	else
	begin
//		WriteLn('FLASH was on, turn off');
		view^.flashOn := not view^.flashOn;
		Exit; // Don't bother, the old caret is already erased!
	end;
	p := HOffsetToPoint(view, view^.h^.selStart);
	p2.h := p.h;
	p2.v := p.v + view^.h^.rowHeight;
	HSDLine(view, p, p2, tempPort, fgColor);
	view^.lastp1 := p;
	view^.lastp2 := p2;
	view^.flashOn := not view^.flashOn;

	Exit;
	
(*
	if view^.flashOn then
	begin
//		Write('/');
		HSDXORLine(view, view^.lastp1, view^.lastp2, tempPort)
	end
	else
	if view^.h^.selStart <> view^.h^.selEnd then
		Exit
	else
	begin
//Write('.');
		p := HOffsetToPoint(view, view^.h^.selStart);
//Write('.');
		p2.h := p.h;
		p2.v := p.v + view^.h^.rowHeight;
		HSDXORLine(view, p, p2, tempPort);
//Write('.');
		view^.lastp1 := p;
		view^.lastp2 := p2;
	end;
	view^.flashOn := not view^.flashOn;
*)
end;

// This is never done any more - InvalViews instead
(*procedure UnflashCaret(view: HaldaViewPtr; tempPort: Boolean);
begin
Halt;
//	if view^.flashOn then FlashCaret(view);
	if view^.flashOn then
	begin
//		WriteLn('UnFlash');
		HSDXORLine(view, view^.lastp1, view^.lastp2, tempPort);
	end;
//	else
//		WriteLn('No unflash needed');
	view^.flashOn := false;
end;*)


	procedure HDraw (theView: HaldaViewPtr);
		var
			line: Longint;
			r: HaldaRect;

			next: Longint;
			v: Real;
			styleIndex: Longint;
			actualTabSize, drawnSoFar: Longint;

			selRgn: HaldaRegion;
//			p, p2: HaldaPoint;
			h: HaldaPtr;
			charsToLeft: Longint; // Instead of view^.leftCol
{Drawing a string}
{procedure HDrawString (h: HaldaPtr; posh, posv, line: Longint);}
{Draws one FULL ROW! But can skip the start if it is scrolled out of view.}
		procedure MyDrawString;
			var
				theString: AnsiString;
				next: Longint;
				start, upto, i: Longint;

		begin
			HSDSetFont(theView^.h^.fontName, theView^.h^.fontSize);
//			WriteLn(theView^.leftCol);

//WriteLn('MyDrawString');
			if line < High(h^.rowStarts) then
				next := h^.rowStarts[line + 1] - 2 // last character before line break
			else
				next := Length(h^.text);

// Make sure that the last style ends at the end.
			h^.format[High(h^.format)].formatEnd := Length(h^.text);

{Get the first style in line - even if not visible}
// LINEAR SEARCH CAN BE SLOW FOR LARGE DOCUMENTS!
			while (h^.format[styleIndex].formatEnd < h^.rowStarts[line]) and (styleIndex < High(h^.format)) do
				styleIndex := styleIndex + 1;
			if styleIndex > High(h^.format) then
				styleIndex := High(h^.format);

			start := h^.rowStarts[line]; // Start at start of line, skip all before theView^.leftCol after expansion
			upto := start;
			drawnSoFar := 0; // Includes skipped at left

{for the entire line (in view)}
			while start <= next do
				begin
{get string up to the next style edge OR end of line}
					if (styleIndex < 0) or (styleIndex > High(h^.format)) then
						WriteLn('Who''s afraid of the big bad error? ', styleIndex);

					upto := h^.format[styleIndex].formatEnd;
					if upto > next then
						upto := next;
					if upto < start then
						upto := start; // Should not happen

					theString := Copy(h^.text, start, upto - start + 1);
					//WriteLn('Drawing part "', theString, '" length ', upto - start + 1); // Börjar ibland MYCKET för tidigt! Trasig rowStarts? leftCol?

					HSDForeColor(h^.format[styleIndex].color);
					HSDTextFace(h^.format[styleIndex].style);

// Expand TABs. Note that "drawnSoFar" is needed to keep track of expansion of earlier parts of the same line					
					i := 1;
					while i <= Length(theString) do
					begin
						if theString[i] = tab then
						begin
							actualTabSize := h^.tabWidth - (drawnSoFar + i-1) mod h^.tabWidth;
							if actualTabSize <= 0 then actualTabSize := h^.tabWidth;
							theString := ConCat(Copy(theString, 1, i - 1),
												Copy('        ', 1, actualTabSize),
												Copy(theString, i + 1, Length(theString) - i)); {testas}
						end;
						i := i + 1;
					end;
					//WriteLn('Expand to "', theString, '"');
					
					{Cut away any part of the expanded string that is left of leftCol}
					if drawnSoFar < theView^.leftCol then
						if Length(theString) < theView^.leftCol - drawnSoFar then
						begin // skip all
							drawnSoFar += Length(theString);
							theString := '';
						end
					else
						begin
							theString := Copy(theString, theView^.leftCol - drawnSoFar + 1, Length(theString));
							drawnSoFar := theView^.leftCol;
						end;
					
					{Draw the string through the system dependent glue.}
//					WriteLn('Actually drawn "', theString, '"');
					if Length(theString) > 0 then
						HSDDrawString(theString);
					drawnSoFar += Length(theString);
					
					// Did the style end?
					if h^.format[styleIndex].formatEnd = upto then
						if styleIndex < High(h^.format) then
						begin
							styleIndex += 1;
//							WriteLn('Moving to style ', styleIndex, ' at ', upto);
						end;

					start := upto+1; {step to next}
				end;

		end; {MyDrawString}

	var
		extraLeft: Longint;
		
	begin
		h := theView^.h;
		if theView^.invalLevel >= 2 then // Full redraw
		begin
//			WriteLn('FULL REDRAW (', theView^.invalLevel, ')');
			HSDSetFont(theView^.h^.fontName, theView^.h^.fontSize);

			// Some health checks
			if theView^.topRow > High(theView^.h^.rowStarts) then
				theView^.topRow := High(theView^.h^.rowStarts);
			if theView^.topRow < 0 then
				theView^.topRow := 0;

			r := theView^.viewRect;
			r.left += theView^.leftMargin;
			line := theView^.topRow;
			
	{v := r.top + h^.rowHeight; För mycket? Bara ASCENT?}
			v := r.top + h^.ascent; {Ja, bara ascent är snyggare.}

			styleIndex := 0;
			// Don't search linearly for the first!
			styleIndex := GetStyleFromPosition(h, h^.rowStarts[line]);

			HSDForeColor(blueColor);
			HSDFrameRect(theView^.viewRect); // erase with margin
//WriteLn(theView^.viewRect.left:1:0, ',', theView^.viewRect.top:1:0, ',', theView^.viewRect.right:1:0, ',', theView^.viewRect.bottom:1:0);
			HSDBackColor(theView^.bgColor);
			HSDEraseRect(theView^.viewRect); // erase with margin
			HSDBackColor(whiteColor);
			HSDForeColor(h^.format[0].color);

	{Draw text}
			repeat
	{if line < Length(rowStarts) then}
				if line < High(h^.rowStarts) then
					next := h^.rowStarts[line + 1] - 1
				else
					next := Length(h^.text); // last character
				// Varför görs detta igen i MyDrawString?

				CalcLeftChars(theView, line, charsToLeft, extraLeft); // Obsolete?
//				HSDMoveTo(r.left + extraLeft * h^.charWidth, v);
				HSDMoveTo(r.left, v);

				MyDrawString;

				line := line + 1;
				v := v + h^.rowHeight;
			until (v > r.bottom) or (next >= Length(h^.text));
//			WriteLn('Loop done');

	{Draw selection!}

	{Insertion point}
			if h^.selStart = h^.selEnd then
			begin
//				theView^.flashOn := false;
//		WriteLn('Force FlashCaret');
				FlashCaret(theView, false);
//		WriteLn('Done FlashCaret');
			end
			else
{Region glue needed}
			begin
//		WriteLn('GetSelectionRegion');
				selRgn := GetSelectionRegion(h, theView);
//		WriteLn('GetSelectionRegion done');
				HSDMarkRgn(selRgn);
//		WriteLn('HSDMarkRgn done');
			end;
	//		DisposeRgn(selRgn);
		end
		else // Low inval level, just flash caret
			if h^.selStart = h^.selEnd then
			begin
//					WriteLn('REDRAW CARET ONLY (', theView^.invalLevel, ')');
				FlashCaret(theView, false);
			end
			else
				WriteLn('NO REDRAW OF CARET: end <> start : ', h^.selStart, ',', h^.selEnd);
		
		theView^.invalLevel := 0; // No redraw need known
//		theView^.invalLevel -= 1; // Reduce redraw level (this allows multiple full redraws which is an attempt to fix the caret dropping problem.)
//		WriteLn('HDraw done');
		
		// Restore colors
	end; {HDraw}
	
	function EqualStyle(f1, f2: FormatRec): Boolean;
	begin
		if f1.style <> f2.style then
			EqualStyle := false
		else
			EqualStyle := f1.color.col = f2.color.col;			
	end;

	procedure HInsertAt (h: HaldaPtr; s: AnsiString; selStart, selEnd: Longint);
	var
		i, insertedCRs, deletedCRs, offset: Longint;
	begin
		for i := 1 to Length(s) do
			if s[i] = #10 then s[i] := #13;
//		HInvalViews(h);
		//for i := 0 to High(h^.views) do
		//	UnflashCaret(h^.views[i], true);

//		WriteLn('HInsert "', s, '" at ', selStart, ',', selEnd);
{Enkel lösning: Tag bort selection, sätt in s i stället, räkna om radstarter}
{Lite mer sofistikerat: Räkna CR i s och i det som tas bort. Räkna om radstarter lokalt.}

// Sanity
		if selStart < 1 then selStart := 1;
		if selEnd < selStart then selEnd := selStart;
		if selEnd > Length(h^.text)+1 then
			selEnd := Length(h^.text)+1;

		insertedCRs := 0;
		for i := 1 to Length(s) do
			if s[i] = #13 then
				insertedCRs += 1;
		deletedCRs := 0;
		for i := selStart to selEnd do // Ingår selEnd?
			if i <= Length(h^.text) then // Might be beyond the text
				if h^.text[i] = #13 then
					deletedCRs += 1;
		offset := Length(s) - (selEnd - selStart);
		
		// Modifiera offset efter identiska stilar
		
{TO DO: Update rowStarts smarter, count CRs and lengths in deleted and inserted part}

		// Insert text
		if (selEnd > selStart) and (Length(s) = 0) then // delete
			begin
				{h^.text := Omit(h^.text, selStart, selEnd - selStart);}
				h^.text := ConCat(Copy(h^.text, 1, selStart - 1), Copy(h^.text, selEnd, Length(h^.text) - selEnd + 1));

				selEnd := selStart;
			end
		else
			begin // insert
				h^.text := ConCat(Copy(h^.text, 1, selStart - 1), s, Copy(h^.text, selEnd, Length(h^.text) - selEnd + 1));

				selStart := selStart + Length(s);
				selEnd := selStart;
			end;

//		WriteLn('HInsert FindRowStarts');

		FindRowStarts(h); // BYT TILL INKREMENTELL! insertedCRs + deletedCRs

//		WriteLn('HInsert Uppdatera stilar');
		
		// Uppdatera stilar också
		for i := 0 to High(h^.format) do
			if h^.format[i].formatEnd > selStart then
			begin
				if offset >= 0 then
				h^.format[i].formatEnd += offset
				else // Farligt - en stil kan sluta innan den förra!
				begin
					if h^.format[i].formatEnd + offset < selStart then
						h^.format[i].formatEnd := selStart
					else
						h^.format[i].formatEnd += offset;
					// Bättre men inte helt rätt.
				end;
			end;

//		WriteLn('HInsert Rensa stilar');

		// Ev eliminerade stilar borde tas bort!
		// clean styles:
		// no length - remove
		// same as previous - merge
		i := 1;
		offset := 0; // New meaning: number of deleted styles!
		while i <= High(h^.format) do
		begin
			if h^.format[i].formatEnd <= h^.format[i-1 - offset].formatEnd then // No length
			begin
//				WriteLn(i, ': no length');
				offset := offset + 1;
			end
			else
			if EqualStyle(h^.format[i-1 - offset], h^.format[i]) then // Same as previous
			begin
//				WriteLn(i, ': Same style');
				h^.format[i-1 - offset].formatEnd := h^.format[i].formatEnd;
				offset := offset + 1;
			end
			else // Style not deleted - copy to the right place!
			begin
//				WriteLn(i, ': preserve, shift by ', offset);
				h^.format[i - offset] := h^.format[i];
			end;
			i := i + 1;
		end;
		SetLength(h^.format, Length(h^.format) - offset);

//		UpdateRowStarts(h, s);???

//		WriteLn('HInsert done');
		HInvalViews(h);
		HAdjustScrollBarRanges(h);
	end;

procedure HInsert(h: HaldaPtr; s: AnsiString);
begin
	HInsertAt (h, s, h^.selStart, h^.selEnd);
	h^.selStart := h^.selStart + Length(s);
	h^.selEnd := h^.selStart;
end;

// NOTE! Inval on windows questionable if you embed in HIViews!
	
	procedure HInvalView(theView: HaldaViewPtr);
	begin
		if theView^.window <> nil then
			HSDInvalRect(theView^.window, theView^.viewRect);
		if theView^.hiview <> nil then
			HSDViewSetNeedsDisplay(theView, true);
		if theView^.additionalInval <> nil then
			theView^.additionalInval(theView);
		theView^.invalLevel := 2; // Redraw all
//		err := InvalWindowRect(theView^.window, theView^.viewRect);
	end;
	
	procedure HInvalViews(h: HaldaPtr);
	var
		i: Longint;
	begin
		for i := 0 to High(h^.views) do
		begin
			HInvalView(h^.views[i]);
//			HAdjustScrollBarRange(view^.h^.views[i]); // Endast vid delete, CR, cut, paste
		end;
	end;
	
	procedure HInvalCaret(h: HaldaPtr);
	var
		i, lvl: Longint;
	begin
		for i := 0 to High(h^.views) do
		begin
			lvl := h^.views[i]^.invalLevel;
			HInvalView(h^.views[i]);
			if lvl = 0 then
				h^.views[i]^.invalLevel := 1; // Don't change if it is >1
		end;
	end;
	
	procedure HInvalSelection (h: HaldaPtr); // theView: HaldaViewPtr);
	var
//		err: OSStatus;
		selRgn: HaldaRegion;
		i: Longint;
	begin
		for i := 0 to High(h^.views) do
		begin
			selRgn := GetSelectionRegion(h, h^.views[i]);
//		InvalRgn(selRgn);
			if h^.views[i]^.window <> nil then
				HSDInvalRegion(h^.views[i]^.window, selRgn);
			HSDViewSetNeedsDisplay(h^.views[i], true);
			h^.views[i]^.invalLevel := 2; // Redraw all
//		DisposeRgn(selRgn);
		end;
	end;

{Back to portable stuff}
	procedure HSetSelection (h: HaldaPtr; selStart, selEnd: Longint);
	begin
		if selStart < 1 then 
			h^.selStart := 1
		else
			h^.selStart := selStart;
		if selEnd > Length(h^.text)+1 then
			selEnd := Length(h^.text)+1
		else
			h^.selEnd := selEnd;
	end;

	procedure HGetSelection (h: HaldaPtr; var selStart, selEnd: Longint);
	begin
		selStart := h^.selStart;
		selEnd := h^.selEnd;
	end;

	procedure HSelectAll (h: HaldaPtr);
	begin
		h^.selStart := 1;
		h^.selEnd := Length(h^.text)+1; // After the last character
		HInvalViews(h);
	end;

	procedure HKeyCustom (theView: HaldaViewPtr; theKey: Char; mods: Longint; MyHInsert: HInsertProc);
		var
			shift, alt, control, command: Boolean;
			row, col, next: Longint;
			h: HaldaPtr;
	begin
//		UnflashCaret(theView, true);
//		HInvalSelection(theView^.h, theView);
		HInvalViews(theView^.h);
		
		h := theView^.h;
		HSDGetModsBits(mods, shift, alt, control, command);
//		shift := (mods and shiftKey) <> 0;
//		shift := BitAnd(mods, shiftKey) <> 0;
//		alt := BitAnd(mods, optionKey) <> 0;
//		control := BitAnd(mods, controlKey) <> 0;

{shift: fixed funcionality, one character at a time}
{alt/control: callback controlled? Defaults?}
{Optional callbacks for ALL keys, to override the behavior below, as a pre-filter!}
		
		// Some ctrl-keys map on others
		case theKey of
			ctrlA:
			begin
				theKey := left;
				command := true;
			end;
			ctrlE:
			begin
				theKey := right;
				command := true;
			end;
			ctrlK:
			begin
				// More complicated:
				// If there is a selection, cut.
				// If no selection, cut to end of line!
				// But if there are several ctrl-K in succession...
			end;
		end;

		case theKey of
			cr, tab: 
				begin
{callback för CR åtminstone}

//					WriteLn('calling MyHInsert');
					MyHInsert(h, theKey);
//					WriteLn('returned from MyHInsert');
//					FindRowStarts(h);
					HMakeVisible(theView, h^.selStart);
				end;
			bs: 
				begin
{Specialare för shift/alt/control?}

					if h^.selStart >= h^.selEnd then {om ingen selection}
						if h^.selStart > 1 then
							h^.selStart := h^.selEnd - 1; {Välj bakåt}

					MyHInsert(h, '');
//					FindRowStarts(h);
					HMakeVisible(theView, h^.selStart);
				end;
			del: 
				begin
{Specialare för shift/alt/control?}

					if h^.selStart >= h^.selEnd then {om ingen selection}
						if h^.selEnd < Length(h^.text)+1 then
							h^.selEnd := h^.selStart + 1; {Välj framåt}

					MyHInsert(h, '');
//					FindRowStarts(h);
					HMakeVisible(theView, h^.selStart);
				end;

			left: 
				begin
					if alt then
						begin
{row := GetRow(h, h^.selStart);}
							h^.selStart := PrevWordLimit(h, h^.selStart);
{Backa ett ord}
{Callback för varianter!}
						end
					else
					if command then
					begin
						// To end of line
						row := HOffsetToRow(h, h^.selStart);
						h^.selStart := h^.rowStarts[row];
					end
					else
						begin {Vanlig vänsterpil}
							if h^.selStart > 1 then
								h^.selStart := h^.selStart - 1;
						end;
					if not shift then
						h^.selEnd := h^.selStart
					else
						HInvalViews(h);
					HMakeVisible(theView, h^.selStart);
				end;
{Saknas: Cmd-pil, gå till radslut/radstart!}

			right: 
				begin
					if alt then
						begin
{Fram ett ord}
{Callback för varianter!}
							h^.selEnd := NextWordLimit(h, h^.selEnd);
						end
					else
					if command then
					begin
						row := HOffsetToRow(h, h^.selStart);
						if row < High(h^.rowStarts) then
							h^.selEnd := h^.rowStarts[row+1]-1
						else
							h^.selEnd := Length(h^.text)+1;
					end
					else
						begin {Vanlig vänsterpil}
							if h^.selEnd < Length(h^.text)+1 then
								h^.selEnd := h^.selEnd + 1;
						end;
					if not shift then
						h^.selStart := h^.selEnd
					else
						HInvalViews(h);
					HMakeVisible(theView, h^.selEnd);
				end; {right}
{Saknas: Alt-pil, gå till radslut eller radstart!}

			up: 
				begin
					row := HOffsetToRow(h, h^.selStart);
					col := h^.selStart - h^.rowStarts[row];
					if command then
					begin
						h^.selStart := 1;
					end
					else
					if row > 0 then {not first row}
						begin
							if h^.rowStarts[row] - h^.rowStarts[row - 1] > col then {Fits in previous row?}
								begin
									h^.selStart := h^.rowStarts[row - 1] + col;
								end
							else {Does not fit}
								begin
									h^.selStart := h^.rowStarts[row] - 1;
								end;

						end
					else {first row}
						begin
							h^.selStart := 1;
						end;

					if not shift then
						h^.selEnd := h^.selStart
					else
						HInvalViews(h);
					HMakeVisible(theView, h^.selStart);
					//HAdjustScrollBarRange(theView);
				end; {up}

			down: 
				begin
{Hellre gå från selEnd? Och kopiera fram selStart om man inte håller shift? Eller kom ihåg vilken av dem som är "aktuell"?}
					row := HOffsetToRow(h, h^.selEnd);
					col := h^.selEnd - h^.rowStarts[row];

					if command then
					begin
						h^.selEnd := Length(h^.text)+1;
					end
					else
					if row < High(h^.rowStarts) then {not last row}
						begin
{OBS! En del obehagligt adresserande med +2. Testas! Borde jag ha specialfallet att man är en rad ovanför slutet? Ja!}
							if row = High(h^.rowStarts)-1 then {second to last row}
							begin
								next := Length(h^.text)+1;
							end
							else
								next := h^.rowStarts[row + 2];
							if next - h^.rowStarts[row + 1] > col then {Fits in next row}
								begin
									h^.selEnd := h^.rowStarts[row + 1] + col;
								end
							else {Does not fit}
								begin
								// Correction 141027
									h^.selEnd := next; // - 1;
									if h^.selEnd > Length(h^.text)+1 then
										h^.selEnd := Length(h^.text)+1;
								end;

						end
					else {last row}
						begin
							h^.selEnd := Length(h^.text)+1;
						end;

					if not shift then
						h^.selStart := h^.selEnd
					else
						HInvalViews(h);
					HMakeVisible(theView, h^.selEnd);
					//HAdjustScrollBarRange(theView);
				end; {down}

			otherwise
				if not command then
				if theKey <> Char(27) then // no ESCAPES please
				begin
					MyHInsert(h, theKey);
//					FindRowStarts(h);
					HMakeVisible(theView, h^.selEnd);
				end;
		end;
		HSDSetScrollbarValue(theView^.vScroll, theView^.topRow);
//		if h^.selStart = h^.selEnd then FlashCaret(theView, true);
		HInvalViews(h);
	end;

	procedure HKey (theView: HaldaViewPtr; theKey: Char; mods: Longint);
	begin
		HKeyCustom (theView, theKey, mods, @HInsert);
	end;


// BYT TILL MOUSEUP!
// MouseDown - registrera var
// stilldown/drag - visa drag-mellan-resultat
// MouseUp - hantera resultat!
	procedure HMouse (theView: HaldaViewPtr; p: HaldaPoint; mods: integer);
		var
			pos, mid, theTime, row, i: Longint;
			shift, alt, control, command: Boolean;
			h: HaldaPtr;
	begin
{shift-klick: extend selection}
{short click: set selection}
{quick click-and-drag: selection drag}
{click, hold, drag: drag selection NOT DONE YET}
{This means that a short click can NOT be done here, but in HIdle: a click that ends at the same}
{place is a selection click}
		
		h := theView^.h;
		theView^.pressed := true;
		
		// Set focused view
		for i := 0 to High(h^.views) do
		if h^.views[i] = theView then
			h^.focusedView := i;
		
//		UnflashCaret(theView, true);
//		HInvalViews(theView^.h);
		HSDGetModsBits(mods, shift, alt, control, command);
//		shift := BitAnd(mods, shiftKey) <> 0;
		
		pos := HPointToOffset(theView, p); {sätt selection}
		
		if shift then
			begin
{Move the closest edge to pos}
{TP-style: Move selStart if pos < selStart, otherwise selEnd}
{TextEdit-style: always entire words}
{I take closest, seems reasonable}
				mid := (h^.selStart + h^.selEnd) div 2;
				if pos < mid then
					begin
						h^.selStart := pos;
						h^.mouseClickPos := h^.selEnd; {Gör det någon skillnad om man vet att man fått klicket?}
//						HInvalSelection(h, theView);
					end
				else
					begin
						h^.selEnd := pos;
						h^.mouseClickPos := h^.selStart; {Gör det någon skillnad om man vet att man fått klicket?}
//						HInvalSelection(h, theView);
					end
			end
		else if pos > 0 then {Just a click}
			begin
{Double-click?}
{OBS! Double-click-and-drag not supported yet. Must save both start and end of selection inn that case (splitting mouseClickPos in two)}
				theTime := HSDTickCount;
				if (theTime - h^.prevClickTime < h^.doubleClickLimit) and (pos = h^.mouseClickPos) then
					begin
						h^.clickCount := h^.clickCount + 1;
{Double or triple click!}
						case h^.clickCount of
							1: 
								begin {Select word}
//									HInvalSelection(h, theView);
									h^.selStart := PrevWordLimit(h, pos);
									h^.selEnd := NextWordLimit(h, pos);
//									HInvalSelection(h, theView);
									h^.singleWaiting := false;
								end;
							2: 
								begin {Select row}
									row := HOffsetToRow(h, pos);
									if row < High(h^.rowStarts) then
									begin
//										HInvalSelection(h, theView);
										HSetSelection(h, h^.rowStarts[row], h^.rowStarts[row + 1]);
//										HInvalSelection(h, theView);
									end;
									h^.singleWaiting := false;
								end;
							otherwise
						end; {case}
						h^.singleWaiting := false;
					end {if double}
				else {if not double-click}
					begin
						h^.clickCount := 0; {0 = first click at stored time}
						h^.prevClickTime := HSDTickCount;

{HSetSelection(h, pos, pos);}
						h^.mouseClickPos := pos;
						h^.singleWaiting := true;
						
//						FlashCaret(theView, true);
					end;
			end;
		HInvalViews(theView^.h);
	end;
	
	procedure HMouseUp(theView: HaldaViewPtr; p: HaldaPoint; mods: integer);
	var
		theTime: Longint;
		h: HaldaPtr;
	begin
		theView^.pressed := false; // Help HIdle to do right
		h := theView^.h;
		if h^.singleWaiting then
		begin
		// Denna förstår jag inte riktigt.
		// Enkelklick som sätter selection?
		// Varför görs den inte på mouse?
		// Och hur görs shift-klick? (Som funkar!)
		// Om detta kommenteras ut så sätts aldrig selection
		// vid enkel-klick.
		// Dvs singleWaiting måste avslutas någon gång.
			theTime := HSDTickCount;
//if false then
			if theTime - h^.prevClickTime < h^.singleClickLimit then
				begin
					HInvalSelection(h);

					HSetSelection(h, h^.mouseClickPos, h^.mouseClickPos);
					h^.singleWaiting := false;

					HInvalSelection(h);
				end;
		end
	end;

	procedure HIdle (theView: HaldaViewPtr);
		var
			p: HaldaPoint;
			firstRow, lastRow, row, pos: Longint;
		h: HaldaPtr;
	begin
		if theView = nil then Exit;
		if theView^.h = nil then Exit;
		h := theView^.h;
{Detektera kort markering:}
//		cursorOn := not cursorOn;
		
		if HSDButton then
		if theView^.pressed then // If not then this might be a resize etc
		begin
				p := HSDGetMouse;
//				WriteLn('idle mouse at ', p.h, ',', p.v);
				pos := HPointToOffset(theView, p); {sätt selection}
				HGetVisibleRows(theView, firstRow, lastRow);
				row := HOffsetToRow (h, pos);
//				om ej synlig, ändra topRow
				if row < firstRow then begin theView^.topRow -= firstRow - row; HSDSetScrollbarValue(theView^.vScroll, theView^.topRow); HInvalView(theView); end;
				if row > lastRow then begin theView^.topRow += row - lastRow; HSDSetScrollbarValue(theView^.vScroll, theView^.topRow); HInvalView(theView); end;
// Hellre räkna pixlar över gränsen?
		end;

		if not HSDButton then
		begin
			if h^.singleWaiting then
				begin
				end
			else if h^.selStart = h^.selEnd then
				begin
{Blinka cursorn}
//					idleCount += 1;
//					if idleCount mod 10 = 0 then
					begin
						HInvalCaret(theView^.h);
//						WriteLn('INVAL CARET (IDLE)! ', theView^.invalLevel);
//						FlashCaret(theView, true);
					end;
				end;
			theView^.pressed := false; // Should be unnecessary, noted by HMouseUp!
		end;
	end;
	
	procedure HMouseDrag(theView: HaldaViewPtr; p: HaldaPoint);
	var
		pos, start, finish, row1, row2: Longint;
		h: HaldaPtr;
	begin
		h := theView^.h;
//				UnflashCaret(theView, true);
				HInvalViews(theView^.h);
	// Click-and-drag: Make selection.
	// We could also support dragging text blocks. (Not yet.)
				pos := HPointToOffset(theView, p); {set selection}
				
//				WriteLn('Something happens at ', p.h, ' ', p.v);
				
				if pos <> h^.mouseClickPos then {Any kind of dragging done - not a single click marking}
					h^.singleWaiting := false;
				
				HInvalSelection(h);
								
//				WriteLn('Drag with ', pos, ' and ', h^.mouseClickPos);

				if pos > h^.mouseClickPos then
				begin
					start := h^.mouseClickPos;
					finish := pos;
				end
				else
				begin
					start := pos;
					finish := h^.mouseClickPos;
				end;
				
				case h^.clickCount of
				1:
					begin
						start := PrevWordLimit(h, start);
						finish := NextWordLimit(h, finish);
					end;
				2:
					begin
						row1 := HOffsetToRow(h, start);
						row2 := HOffsetToRow(h, finish);
						start := h^.rowStarts[row1];
						if row2 < High(h^.rowStarts) then
							finish := h^.rowStarts[row2 + 1]
						else
							finish := Length(h^.text);
					end;
				otherwise
//					WriteLn('clickCount = ', h^.clickCount, ' No expansion');
				end;
				HSetSelection(h, start, finish);

//				if pos > h^.mouseClickPos then
//					HSetSelection(h, h^.mouseClickPos, pos)
//				else
//					HSetSelection(h, pos, h^.mouseClickPos);
				
				HInvalSelection(h);
	end;

{Dela upp Mouse/Idle mer? Separata ingångar för dubbelklick, klick-och-dra med mera?}
{Kan man anta att man kan få sånt från systemet?}

	procedure HSetText (h: HaldaPtr; s: AnsiString);
	var
		i: Longint;
	begin
		// Search CRLF
		for i := Length(s) downto 1 do // Reduce CRLF to CR
			if s[i] = #10 then
			begin
				if i > 1 then
					if s[i-1] = #13 then
					begin
						// CRLF found, remove the LF
//						s := Copy(s, 1, i-1) + Copy(s, i+1, Length(s));
						s := Copy(s, 1, i-2) + Copy(s, i, Length(s));
					end;
			end
			else
			if s[i] = #13 then
				if i > 1 then
					if s[i-1] = #10 then
					begin
						// LFCR found, remove the CR
//						s := Copy(s, 1, i-1) + Copy(s, i+1, Length(s));
						s := Copy(s, 1, i-2) + Copy(s, i, Length(s));
					end;
		for i := 1 to Length(s) do // LF to CR
			if s[i] = #10 then
				s[i] := #13;
		h^.text := s;
		FindRowStarts(h);
		// Initiera stilar
		HInvalViews(h);
		HAdjustScrollBarRanges(h);
	end;

// The HGetText calls are trivial, exists primarily to make it possible to
// intercept/detect readouts when  debugging, if necessary.
	function HGetText (h: HaldaPtr): AnsiString; overload;
	begin
		HGetText := h^.text;
	end;

	function HGetText (h: HaldaPtr; fromPos, count: Longint): AnsiString; overload;
	begin
		HGetText := Copy(h^.text, fromPos, count);
	end;

	// Unnecessary utility? Trivial as long as the structures are transparent.
	procedure HGetVisibleRows(theView: HaldaViewPtr; var firstRow, lastRow: Longint);
	begin
		if theView^.topRow > High(theView^.h^.rowStarts) then
		begin 
			theView^.topRow := High(theView^.h^.rowStarts);
			HSDSetScrollbarValue(theView^.vScroll, theView^.topRow);
		end;
		firstRow := theView^.topRow;
		lastRow := firstRow + Trunc(theView^.viewRect.bottom - theView^.viewRect.top) div theView^.h^.rowHeight - 1;
		if lastRow > High(theView^.h^.rowStarts) then
			lastRow := High(theView^.h^.rowStarts);
	end;
	
	var
		gLatestStyleIndex: Longint;

	// SetStyle borde slå ihop stilar som är lika
	// Kolla om ny stil är lika med en granne?
	procedure HSetStyle(h: HaldaPtr; selStart, selEnd: Longint; theStyle: Integer; color: HaldaColor);
	var
		styleIndex, newIndex, i, minIx, maxIx, newStyleIndex: Longint;
		styleStart, styleEnd: Longint;
		lengthChange: Longint;
		done: Boolean;
	begin
//		selEnd -= 1;
		if Length(h^.format) < 1 then
		begin
			WriteLn('No styles! Fatal error!');
			Exit;
		end;
		
		// Some sanity checks
		h^.format[High(h^.format)].formatEnd := Length(h^.text); // Standard repair
		if selEnd > Length(h^.text)+1 then selEnd := Length(h^.text)+1;
		if selStart > Length(h^.text) then Exit;
		if selEnd < 0 then Exit;
		
		// Only one style
		if Length(h^.format) < 2 then
		begin
			styleIndex := 0;
//			WriteLn('Single style');
		end
		else
		// Case 1: Change in the LAST style. Very likely case for color coding
		// At least two are guaranteed to exist
		if selStart > h^.format[High(h^.format) - 1].formatEnd then // Format start
		begin
			styleIndex := High(h^.format);
//			WriteLn('Last style');
		end
		else
		// Case 2: Work with bidirectional binary search.
		// selStart should be in the style!
		// still NOT correct!
		// Stega fram till den FÖRSTA där styleEnd (formatEnd) >= selStart
		// Detta är den FÖRSTA som överlappar intervallet!
		begin
//			styleIndex := gLatestStyleIndex;
//			if styleIndex > High(h^.format) then styleIndex := High(h^.format);
			done := false;
			maxIx := High(h^.format);
			minIx := 0;
			
//			minIx - styleIndex - maxIx
//			Jämför selStart med formatEnd!
//			[styleIndex]formatEnd < selStart -> flytta fram minIx
//			[styleIndex]formatEnd >= selStart -> flytta ner maxIx
			
			repeat
				styleIndex := minIx + (maxIx-minIx) div 2;
				if selStart <= h^.format[styleIndex].formatEnd then
					maxIx := styleIndex
				else
					minIx := styleIndex;
			until minIx >= maxIx-1;
//			if selStart > h^.format[minIx].formatEnd then
//				styleIndex := maxIx
//			else
//				styleIndex := minIx;
			styleIndex := minIx;
		end;
//		gLatestStyleIndex := styleIndex;
		styleStart := 0;
		if styleIndex > 0 then
			styleStart := h^.format[styleIndex-1].formatEnd;
		styleEnd := h^.format[styleIndex].formatEnd;
//		WriteLn('Found style ', styleIndex);
		newStyleIndex := styleIndex;
		
(*
// Old: linear search. Dumb as global solution.	But is used for the last step.
//		styleIndex := 0;
//		styleStart := 0;
//		styleEnd := h^.format[styleIndex].formatEnd;
		
		// Stega fram tills vi har överlapp
		while styleEnd < selStart do
		begin
			styleIndex += 1;
			styleStart := styleEnd+1;
			if styleIndex > High(h^.format) then
			begin
//				WriteLn('Shit appends 1');
			end;
			styleEnd := h^.format[styleIndex].formatEnd;
		end;
//		WriteLn('OLD Found style ', styleIndex);
		newStyleIndex := styleIndex;
*)
		


		
// Old: linear search. Dumb as global solution.	But is used for the last step.
// Here used as fix for bug above.
//		styleIndex := 0;
		styleIndex := styleIndex - 2;
		if styleIndex < 0 then styleIndex := 0;
		styleStart := 0;
		styleEnd := h^.format[styleIndex].formatEnd;
		
		// Stega fram tills vi har överlapp
		while styleEnd < selStart do
		begin
			styleIndex += 1;
			styleStart := styleEnd+1;
			if styleIndex > High(h^.format) then
			begin
//				WriteLn('Shit appends 1');
			end;
			styleEnd := h^.format[styleIndex].formatEnd;
		end;
//		WriteLn('OLD Found style ', styleIndex);
		
//		if newStyleIndex <> styleIndex then
//			WriteLn('OLD Found style ', styleIndex, ' NEW ', newStyleIndex);
		
		// Nu har vi överlapp!
		// Korta den första till selStart
//		h^.format[styleIndex].formatEnd := selStart;
		newIndex := styleIndex + 1;
		if styleStart = selStart then // hela första intervallet täcks!
			newIndex := styleIndex;
		
		// Stega fram till den sista som överlappar
		while styleEnd < selEnd do
		begin
			styleIndex += 1; // Kan vara utanför arrayen!
			styleStart := styleEnd+1;
			if styleIndex > High(h^.format) then
			begin
				styleIndex := High(h^.format); // Kan detta gå snett???
//				WriteLn('Shit appends 2'); // Detta skall inte hända - den sista skall gå till slutet
				selEnd := styleEnd; // Nu skall det funka! 140923
//				styleEnd := h^.format[styleIndex].formatEnd; // KAN GÅ FEL!
				break;
			end;
			styleEnd := h^.format[styleIndex].formatEnd; // KAN GÅ FEL!			
		end;
		
		if styleEnd = selEnd then // Hela sista intervallet överlappar!
			styleIndex += 1;
		
		// Allt mellan styleIndex och newIndex skall bort!
		// Om de är lika skall en läggas till!
		
//		oldLength := Length(h^.format);
//		newLength := oldLength + (newIndex - styleIndex + 1);
		lengthChange := newIndex - styleIndex + 1;
//		WriteLn('Old length was ', Length(h^.format));
//		WriteLn('New length is ', Length(h^.format)+lengthChange);

		if lengthChange < 0 then
		begin
			if styleIndex + lengthChange < 0 then
				WriteLn('FUBAR length');
			for i := styleIndex to High(h^.format) do
				h^.format[i + lengthChange] := h^.format[i];
			SetLength(h^.format, Length(h^.format) + lengthChange);
		end
		else
		if lengthChange > 0 then
		begin
			SetLength(h^.format, Length(h^.format) + lengthChange);
			for i := High(h^.format)-lengthChange downto styleIndex do
				h^.format[i + lengthChange] := h^.format[i];
		end;
		
		// Nu har vi ett format på newIndex som skall sättas
		h^.format[newIndex].style := theStyle;
{$R-}
		h^.format[newIndex].color := color;
{$R+}
		h^.format[newIndex].formatEnd := selEnd;
		
		if newIndex > 0 then
			h^.format[newIndex-1].formatEnd := selStart-1;
		
//		for i := 0 to High(h^.format) do
//		begin
//			WriteLn('--- Format ', i);
//			WriteLn('ends at ', h^.format[i].formatEnd);
//			WriteLn('style ', h^.format[i].style);
//			WriteLn('color ', h^.format[i].color.red, ', ', h^.format[i].color.green, ', ', h^.format[i].color.blue);
//			WriteLn;
//		end;
//		WriteLn('---');
//	HInvalViews(h);
// Inval NOT done here for performance reasons!
	end;

	procedure HFocus(h: HaldaViewPtr; gotFocus: Boolean);
	begin
		// Flash cursor or not
	end;
	procedure HSetScrollbarState(h: HaldaViewPtr; active: Boolean);
	begin
		// Activate/deactivate scroll bars?
	end;
	
	procedure HShowSelection(theView: HaldaViewPtr; flag: Boolean); overload;
	var
		firstRow, lastRow: Longint;
		firstSel, lastSel, midVis, midSel: Longint;
		h, visibleChars, selectionCenter: Longint;
		startPoint, endPoint: HaldaPoint;
	begin
//		WriteLn('HShowSelection');
		// Find selection line number
		// Is it on screen?
		// Otherwise change scroll position
		// Must have access to scroll bars!
		
		// HMakeVisible? On beginning or end?
		if not flag then
			HMakeVisible(theView, theView^.h^.selStart)
		else // center
		begin
//			WriteLn('HShowSelection on true');
		
			HGetVisibleRows(theView, firstRow, lastRow);
			firstSel := HOffsetToRow(theView^.h, theView^.h^.selStart);
			lastSel := HOffsetToRow(theView^.h, theView^.h^.selEnd);
			midVis := (firstRow + lastRow) div 2;
			midSel := (firstSel + lastSel) div 2;
//			WriteLn('HShowSelection 1');
			theView^.topRow -= midVis - midSel;
			if theView^.topRow < 0 then theView^.topRow := 0;
			if theView^.topRow > High(theView^.h^.rowStarts) then theView^.topRow := High(theView^.h^.rowStarts);
			HSDSetScrollbarValue(theView^.vScroll, theView^.topRow);
//			WriteLn('v scroll to ', theView^.topRow);

//			WriteLn('HShowSelection 2');
			
			// Also adjust horizontally
			startPoint := HOffsetToPoint(theView, theView^.h^.selStart);
//			WriteLn('HShowSelection startPoint ', startPoint.h, startPoint.v);
// KRASCH HERE!
			endPoint := HOffsetToPoint(theView, theView^.h^.selEnd);
//			WriteLn('HShowSelection startPoint ', endPoint.h, endPoint.v);

//			Writeln(theView^.h^.charWidth);
			if theView^.h^.charWidth < 0.001 then // Avoid divide by zero!
				theView^.h^.charWidth := 1;
			visibleChars := Trunc((theView^.viewRect.right - theView^.viewRect.left) / theView^.h^.charWidth);

//			WriteLn('HShowSelection 3');
			selectionCenter := Trunc((endPoint.h - startPoint.h)/theView^.h^.charWidth);
			h := selectionCenter - visibleChars div 2;
//			WriteLn('H at ', h);
			if h < 0 then h := 0;
			HSDSetScrollbarValue(theView^.hScroll, h);

// Incomplete: Scan all lines to find left and right extremes of selection
//			for i := firstRow to lastRow do
//			begin
//				if selStart < theView^.h^.rowStarts[i] then // Multiple lines - left edge must be left end of selection
//					left := 0;
//				if i < High(theView^.h^.rowStarts) then //  Multiple lines - right edge possible right
//					if selEnd > theView^.h^.rowStarts[i+1] then
//			end;
		end;
	end;
	
	procedure HShowSelection(h: HaldaPtr; flag: Boolean); overload;
	begin
		HShowSelection(h^.views[h^.focusedView], true);
	end;

	// Hur kan man uppdatera ALLA vyer för en HaldaRec?
	procedure HForceUpdate(h: HaldaViewPtr);
	begin
	// HInvalViews
	end;
	
	procedure HDispose(h: HaldaPtr);
	var
		i: Longint;
	begin
	// Also dispose all views?
		for i := 0 to High(h^.views) do
			HDisposeView(h^.views[i]);
		Dispose(h);
	end;
	
	procedure HDisposeView(view: HaldaViewPtr);
	var
		i, x: Longint;
	begin
		// Remove from view list
		x := -1;
		for i := 0 to High(view^.h^.views) do
		begin
			if x > -1 then
				view^.h^.views[i] := view^.h^.views[i-1]
			else
				if view^.h^.views[i] = view then
					x := i;
		end;
		if x > -1 then
			SetLength(view^.h^.views, Length(view^.h^.views)-1);
		
		// Dispose scrollbars!
		if view^.hScroll <> nil then
			HDSDisposeScrollbar(view^.hScroll);
		if view^.vScroll <> nil then
			HDSDisposeScrollbar(view^.vScroll);
		
		// Dispose view
		Dispose(view);
	end;

	function HNew: HaldaPtr;
	var
		h: HaldaPtr;
	begin
		New(h);
		HSetup(h);
		HNew := h;
	end;
	
	function HNewView(viewRect: HaldaRect; window: HaldaWindow; te: HaldaPtr): HaldaViewPtr;overload;
	var
		view: HaldaViewPtr;
	begin
		New(view);
		view^.window := window;
		view^.hiview := nil;
		view^.h := te;
		view^.viewRect := viewRect;
		view^.topRow := 0;
		view^.leftCol := 0;
		view^.leftMargin := 3;
		view^.bgColor.col := whiteColor;
		view^.additionalInval := nil; // No callback
		view^.liveScrollProc := nil; // No callback
		
		// Save view in array in HaldaRec
		if te <> nil then
		begin
			SetLength(te^.views, Length(te^.views)+1);
			te^.views[High(te^.views)] := view;
		end;
		
		HNewView := view;
	end;
	
	procedure HSetBackgroundColor(view: HaldaViewPtr; bgColor: HaldaColor);
	begin
		view^.bgColor.col := bgColor.col;
	end;

// Scroll bar handlers
function HitVScroll(hitView: HaldaScrollbar; myPtr: Pointer): Boolean;
var
	value: Longint;
	theView: HaldaViewPtr;
begin
	theView := HaldaViewPtr(myPtr);
//	WriteLn('HitVScroll');
	value := HSDGetScrollbarValue(hitView);
	theView^.topRow := value;
	HInvalView(theView);
	if theView^.liveScrollProc <> nil then
		theView^.liveScrollProc(theView);
	
//		HSDSetPortWindowPort(theView);
//		HDraw(theView);
//		HSDFinishPort;
	HitVScroll := false;
//	WriteLn('Done HitVScroll');
end;

function HitHScroll(hitView: HaldaScrollbar; myPtr: Pointer): Boolean;
var
	theView: HaldaViewPtr;
begin
	theView := HaldaViewPtr(myPtr);
//	WriteLn('HitHScroll');
	theView^.leftCol := HSDGetScrollbarValue(hitView);
	HInvalView(theView);
	if theView^.liveScrollProc <> nil then
		theView^.liveScrollProc(theView);
	HitHScroll := false;
end;

procedure HAdjustScrollBarRange(theView: HaldaViewPtr);
var
	top, bot: Longint;
	firstRow, lastRow, i, r, maxLength: Longint;
begin
	if theView^.vScroll = nil then Exit;

	// Set scroll range to number of row out of view
	HGetVisibleRows(theView, top, bot);
//	WriteLn('Visible rows: ', top, ',', bot);
//	WriteLn(Length(theView^.h^.rowStarts) - (bot-top));
	if Length(theView^.h^.rowStarts) - (bot-top) > 1 then
		HSDSetScrollbarMax(theView^.vScroll, Length(theView^.h^.rowStarts) - (bot-top))
	else
		HSDSetScrollbarMax(theView^.vScroll, 0);

	// Tell the scrollbar the visible range (for the thumb size)
	HSDSetControlViewSize(theView^.vScroll, bot-top);
//	SetControlViewSize (ControlRef theControl, bot-top);

//	VMSetScrollSteps(testScroll, 1, (bot - top) div 2);
//	VMInstallDefaultScrollHandler(testScroll, 1, (bot - top) div 2);
	HSDSetScrollSteps(theView^.vScroll, 1, (bot - top) div 2);
	
// Also set scrollbar value
	HSDSetScrollbarValue(theView^.vScroll, theView^.topRow);

// Adjust the horizontal one!
	HGetVisibleRows(theView, firstRow, lastRow);
// Measure length of each row - with tabs!
	maxLength := 0;
	for i := firstRow to min(lastRow, High(theView^.h^.rowStarts)) do
	begin
		if i = High(theView^.h^.rowStarts) then // Last one
			r := SizeWithTabs (theView^.h, theView^.h^.rowStarts[i], theView^.h^.rowStarts[i], Length(theView^.h^.text))
//			r := Length(theView^.h^.text) - theView^.h^.rowStarts[i]
		else
			r := SizeWithTabs (theView^.h, theView^.h^.rowStarts[i], theView^.h^.rowStarts[i], theView^.h^.rowStarts[i+1]-1);
//			r := theView^.h^.rowStarts[i+1] - theView^.h^.rowStarts[i];
		maxLength := max(r, maxLength);
	end;
//	WriteLn('Max length = ', maxLength);
//	maxLength := maxLength + 1 - Trunc((theView^.viewRect.right - theView^.viewRect.left - theView^.leftMargin - 15) / theView^.h^.charWidth);
	if theView^.h^.charWidth < 0.001 then
		theView^.h^.charWidth := 1; // Range check error on division by 0!
	maxLength := maxLength - Trunc((theView^.viewRect.right - theView^.viewRect.left - theView^.leftMargin) / theView^.h^.charWidth);
	
//	WriteLn('Max length after adjustment = ', maxLength);
	if maxLength < 0 then maxLength := 0;
//	i := HSDGetScrollbarValue(theView^.hScroll);
	HSDSetScrollbarMax(theView^.hScroll, maxLength);
//	HSDSetScrollbarValue(theView^.hScroll, i);
end;

procedure HAdjustScrollBarRanges(h: HaldaPtr);
var
	i: Longint;
begin
	for i := 0 to High(h^.views) do
	begin
		HAdjustScrollBarRange(h^.views[i]); // Endast vid delete, CR, cut, paste
	end;
end;

procedure HMakeVisible(theView: HaldaViewPtr; pos: Longint);
var
	firstRow, lastRow, row: Longint;
	startPoint: HaldaPoint;
	visibleChars, h, selectionCenter, current: Longint;
begin
	HGetVisibleRows(theView, firstRow, lastRow);
	row := HOffsetToRow(theView^.h, pos);
	if firstRow > row then
	begin
		theView^.topRow := row; // ScrollTo function! Or adjust scrollbars to position!
		HInvalView(theView);
		HSDSetScrollbarValue(theView^.vScroll, theView^.topRow);
	end;
	if lastRow < row then
	begin
		theView^.topRow += row - lastRow;
		HInvalView(theView);
		HSDSetScrollbarValue(theView^.vScroll, theView^.topRow);
	end;
	
	// Also adjust horizontally
	startPoint := HOffsetToPoint(theView, pos); // Nuvarande pos på skärmen
	if theView^.h^.charWidth < 0.001 then
		theView^.h^.charWidth := 1;
	visibleChars := Trunc((theView^.viewRect.right - theView^.viewRect.left) / theView^.h^.charWidth); // Hur mycket ryms på skärmen?
	selectionCenter := Trunc(startPoint.h/theView^.h^.charWidth); // Hur många tecken in på skärmen?
	h := selectionCenter - visibleChars div 2;
//	WriteLn('H at ', h);
	
	current := HSDGetScrollbarValue(theView^.hScroll);
	if (startPoint.h < theView^.viewRect.left) or (startPoint.h > theView^.viewRect.right) then // Only change if needed
		HSDSetScrollbarValue(theView^.hScroll, current + h);
end;

// Resize text editing area (default, full-window)
//procedure HAdjustTextEditArea(theView: HaldaViewPtr);
// If more than one view, distribute them
// (Scroll bar size is slightly off!)
procedure HAdjustTextEditArea(h: HaldaPtr; hScrollLeftMargin, hScrollRightMargin, vScrollTopMargin, vScrollBottomMargin,
							textLeftMargin, textRightMargin, textTopMargin, textBottomMargin: Longint);
var
	fr, sr, r: HaldaRect;
	count, i: Longint;
	theView: HaldaViewPtr;
begin
	// Resize scrollbars
		fr :=  HDSGetViewPortRect(h^.views[0]);
//		if h^.views[0]^.window <> nil then
//			fr := HDSGetWindowPortRect(h^.views[0]^.window); // frame rect
//		if h^.views[0]^.hiview <> nil then
//		fr := HDSGetWindowPortRect(HIViewGetWindow(h^.views[0]^.hiview)); // frame rect

		sr := fr; // scroll rect
		sr.left := sr.right - 15;
		sr.bottom := sr.bottom - 14 - vScrollBottomMargin;
		sr.top := sr.top - 1 + vScrollTopMargin;
		sr.right := sr.right + 1;
		
		count := Length(h^.views);
		for i := 0 to High(h^.views) do
		begin
			theView := h^.views[i];
			// Get the full rectangle fr
			// Create a local part
			r := fr;
			r.bottom := (fr.bottom - fr.top)/count*(i+1) + fr.top;
			r.top := (fr.bottom - fr.top)/count*i + fr.top;
			// Cut down by margins for scrollbars
			if theView^.vScroll <> nil then
				r.right := r.right - 15 - textRightMargin;
			if theView^.hScroll <> nil then
				r.bottom := r.bottom - 15 - textBottomMargin;
			r.left := r.left + textLeftMargin; // Lite marginal (borde vara inställbar = kTextBoxLeftMargin)
			r.top := r.top + textTopMargin; // Lite marginal (borde vara inställbar)
			theView^.viewRect := r;
			sr := r;
			sr.left := sr.right;
			sr.right := sr.left + 15;

			// Horizontal scroll bar
			if theView^.hScroll <> nil then
			begin
				r.top := r.bottom;
				r.bottom := r.top + 15;
				if i = High(h^.views) then
					r.left := {r.left +} hScrollLeftMargin // extra for the row number area
				else
					r.right := r.right + 15;
//				r.bottom := r.bottom - hScrollRightMargin;
//				r.top := r.bottom - 15;
//				r.right := r.right - 15;
				HSDSetScrollbarRect(theView^.hScroll, r);
			end;
			
			// Vertical scroll bar
			r := sr;
//			r.bottom := (sr.bottom - sr.top)/count*(i+1) + sr.top;
//			r.top := (sr.bottom - sr.top)/count*i + sr.top;
			if theView^.vScroll <> nil then
			begin
				HSDSetScrollbarRect(theView^.vScroll, r);
			end;
			
		end;
end;

(*
// Resize text editing area (default, full-window)
//procedure HAdjustTextEditArea(theView: HaldaViewPtr);
procedure HAdjustTextEditAreaOLD(theView: HaldaViewPtr; hScrollLeftMargin, hScrollRightMargin, vScrollTopMargin, vScrollBottomMargin,
																			textLeftMargin, textRightMargin, textTopMargin, textBottomMargin: Longint);
var
	r: HaldaRect;
begin
	// Resize scrollbars
	if theView^.vScroll <> nil then
	begin
		r := HDSGetWindowPortRect(theView^.window);
		r.left := r.right - 15;
		r.bottom := r.bottom - 14 - vScrollBottomMargin;
		r.top := r.top - 1 + vScrollTopMargin;
		r.right := r.right + 1;
		// Set size
		HSDSetScrollbarRect(theView^.vScroll, r);
	end;
	
	if theView^.hScroll <> nil then
	begin
		r := HDSGetWindowPortRect(theView^.window);
		r.left := r.left + hScrollLeftMargin;
		r.bottom := r.bottom - hScrollRightMargin;
		r.top := r.bottom - 15;
		r.right := r.right - 15;
		HSDSetScrollbarRect(theView^.hScroll, r);
	end;

	r := HDSGetWindowPortRect(theView^.window);
	r.right := r.right - 15 - textRightMargin;
	r.bottom := r.bottom - 15 - textBottomMargin;
	r.left := r.left + textLeftMargin; // Lite marginal (borde vara inställbar = kTextBoxLeftMargin)
	r.top := r.top + textTopMargin; // Lite marginal (borde vara inställbar)
	theView^.viewRect := r;
end;*)

// Variant with built-in scroll bar creation
function HNewView(window: HaldaWindow; te: HaldaPtr; hScroll, vScroll: Boolean): HaldaViewPtr;overload;
var
	view: HaldaViewPtr;
	r: HaldaRect;
	viewRect: HaldaRect;
begin
	viewRect := HDSGetWindowPortRect(window);
	viewRect.right -= 15;
	viewRect.bottom -= 15;
	view := HNewView(viewRect, window, te);
	r := viewRect;
	r.left := r.right;
	r.right := r.right + 15;
	view^.vScroll := HDSCreateScrollbar(view, window, r, @HitVScroll);
	r := viewRect;
	r.top := r.bottom;
	r.bottom := r.bottom + 15;
	view^.hScroll := HDSCreateScrollbar(view, window, r, @HitHScroll);
	view^.invalLevel := 2; // Full redraw first time
	HNewView := view;
end;
	
function HMakeColor(col: Cardinal): HaldaColor;
var
	c: HaldaColor;
begin
{$R-}
	c.col := col;
{$R+}
	HMakeColor := c;
end;

// Copy and paste, currently WITHOUT undo!
function HCopy(h: HaldaPtr): Integer;
var
	selectionStart, selectionEnd: Longint;
	clipData: AnsiString;
begin
	HGetSelection(h, selectionStart, selectionEnd);
//	clipData := GetTXNDataToString(index, selectionStart, selectionEnd);
	clipData := Copy(h^.text, selectionStart, selectionEnd - selectionStart);
	HCopy := HDSSetScrapString(clipData);
end;

function HPaste(h: HaldaPtr): Integer;
var
	clipData: AnsiString;
	err: Integer;
begin
	err := HDSGetScrapString(clipData);
	if err = 0 then
		HInsert(h, clipData);
//		SetTXNDataUndoable(index, clipData);	
	HPaste := err;
end;

end.

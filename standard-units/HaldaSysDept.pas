// Mac/QuickDraw version of system dependent Halda parts/glue.
{$mode macpas}

// Bättre att helt enkelt använda QDCG/QDGL?
unit HaldaSysDept; // System dependent stuff for Halda
interface
uses
	MacOSAll, TransSkel4, HaldaTypes, Math, QDCG, QDCGPictUnit;

type
	HaldaScrollbar = HIViewRef;
	HSDPointerProcPtr = FUNCTION(theView: HaldaScrollbar; myPtr: Pointer): Boolean;

//procedure HSDGetSystemFontInfo(rowHeight: Longint; ascent: Longint; charWidth, prevClickTime: Longint);
//procedure HSDGetSystemFontInfo(var rowHeight, ascent, charWdh: Real; var prevClickTime: Longint);
procedure HSDGetSystemFontInfo(var rowHeight, ascent: Longint; var charWdh: Real; var prevClickTime: Longint);
procedure HSDSetFont(fontName: AnsiString; fontSize: Longint);
procedure HSDForeColor(color: HaldaColor);overload;
procedure HSDForeColor(color: UInt32);overload;
procedure HSDBackColor(color: HaldaColor);overload;
procedure HSDBackColor(color: UInt32);overload;
function HSDRGBColorToHaldaColor(color: RGBColor): HaldaColor;
function HSDMakeHaldaColor(color: UInt32): HaldaColor; overload;
function HSDMakeHaldaColor(red, green, blue: Real): HaldaColor; overload;

procedure HSDTextFace(theStyle: StyleParameter);
procedure HSDDrawString(s: AnsiString);
function HSDStringWidth(s: AnsiString): Longint;
procedure HSDInvalRect(theWindow: HaldaWindow; badRect: HaldaRect);
//procedure HSDInvalRect(badRect: Rect);
procedure HSDInvalRegion(theWindow: HaldaWindow; theRgn: HaldaRegion);
//procedure HSDInvalSelection(badRect: Rect);
procedure HSDViewSetNeedsDisplay(theView: HaldaViewPtr; doRedisplay: Boolean);

procedure HSDMarkRect (box: HaldaRect);
procedure HSDMarkRgn (rgn: HaldaRegion);

procedure HSDSetRect(var r: HaldaRect; left, top, right, bottom: Real);
function HSDSectRect({const} rect1, rect2: HaldaRect; var dstRect: HaldaRect):Boolean;
procedure HSDFrameRect(r: HaldaRect);
procedure HSDEraseRect(r: HaldaRect);

procedure HSDAddRectToRgn(var rgn: HaldaRegion; r: HaldaRect);
procedure HSDSetRgnToRect(var rgn: HaldaRegion; r: HaldaRect);
procedure HSDMoveTo(h, v: Real);
procedure HSDLineTo(h, v: Real);

//procedure HSDGetModsBits(mods: Longint; var shift, alt, control: Boolean);
procedure HSDGetModsBits(mods: Longint; var shift, alt, control, command: Boolean);
function HSDTickCount: Longint;
procedure HSDXORLine(theView: HaldaViewPtr; p1, p2: HaldaPoint; tempPort: Boolean);
function HSDButton: Boolean;
function HSDGetMouse: HaldaPoint;

function HDSGetScrapString(var clipData: AnsiString): Integer;
function HDSSetScrapString(clipData: AnsiString): Integer;

function HDSCreateScrollbar(theView: HaldaViewPtr; w: HaldaWindow; r: HaldaRect; scrollHandler: HSDPointerProcPtr): HaldaScrollbar;
procedure HDSDisposeScrollbar(s: HaldaScrollbar);
procedure HDSSetScrollbarRange(s: HaldaScrollbar; maxValue: Longint);
function HSDGetScrollbarValue(s: HaldaScrollBar): Longint;
procedure HSDSetScrollbarValue(s: HaldaScrollBar; value: Longint);
procedure HSDSetScrollbarMax(s: HaldaScrollBar; value: Longint);
procedure HSDSetControlViewSize(s: HaldaScrollBar; range: Longint);
function HDSGetWindowPortRect(w: HaldaWindow): HaldaRect;
function HDSGetViewPortRect(view: HaldaViewPtr): HaldaRect;
procedure HSDSetScrollbarRect(scroll: HaldaScrollbar; r: HaldaRect);
procedure HSDSetScrollSteps(scroll: HaldaScrollbar; buttonStep, pageStep: Integer);
//procedure HSDSetPortWindowPort(view: HaldaViewPtr); // Needed?
procedure HSDFinishPort; // QDCG finish port - needed???

// Stolen from QDCG
const
	blackColor = $000000ff;
	whiteColor = $ffffffff;
	redColor = $ff0000ff;
	greenColor = $00ff00ff;
	blueColor = $0000ffff;
	yellowColor = $ffff00ff;
	cyanColor = $00ffffff;
	magentaColor = $ff00ffff;
	greyColor = $7f7f7fff;

procedure HSDLine(theView: HaldaViewPtr; p1, p2: HaldaPoint; tempPort: Boolean; col: HaldaColor);

implementation

uses
	Halda;

// System dependent setup
//procedure HSDGetSystemFontInfo(var rowHeight, ascent, charWdh, prevClickTime: Longint);
procedure HSDGetSystemFontInfo(var rowHeight, ascent: Longint; var charWdh: Real; var prevClickTime: Longint);
var
	finfo: FontInfo;
	tempPort, savePort: QDCG.GWorldPtr;
	r: Rect;
begin
//WriteLn('HSDGetSystemFontInfo');
	
	savePort := thePort;
// QDCG bug! You can't save the current port! This is bad and should be fixed!

	QDCG.GetFontInfo(fInfo);
	rowHeight := Trunc(finfo.ascent + finfo.descent + finfo.leading);
	ascent := Trunc(finfo.ascent);
//	charWdh := Trunc(CharWidth(' ')); // Asking for trouble here?
//WriteLn('Char width = ', finfo.maxWidth);
//WriteLn('Avg char width = ', finfo.avgWidth);
	charWdh := finfo.maxWidth;

// The char width is incorrect! But this works:	

	SetRect(r, 0,0,16,16);
	NewGWorld(tempPort, r);
	SetPort(tempPort);
	charWdh := StringWidth(' ');	
	DisposeGWorld(tempPort);
	
	SetPort(savePort);
	
	{Ovanstående kan sättas utifrån, HSetTextGeometry}
	prevClickTime := TickCount; {En tidsfunktion skulle också kunna vara extern.}
end;

procedure HSDSetFont(fontName: AnsiString; fontSize: Longint);
begin
	TextFont(fontName);
	TextSize(fontSize);
end;

procedure HSDForeColor(color: HaldaColor);overload;
var
	c: RGBColor;
begin
	c.red := Trunc(color.red SHL 8);
	c.green := Trunc(color.green SHL 8);
	c.blue := Trunc(color.blue SHL 8);
	RGBForeColor(c);
end;

procedure HSDForeColor(color: UInt32);overload;
var
	c: HaldaColor;
begin
	c.col := color;
	HSDForeColor(c);
end;

procedure HSDBackColor(color: HaldaColor);overload;
var
	c: RGBColor;
begin
	c.red := Trunc(color.red SHL 8);
	c.green := Trunc(color.green SHL 8);
	c.blue := Trunc(color.blue SHL 8);
	RGBBackColor(c);
end;

procedure HSDBackColor(color: UInt32);overload;
var
	c: HaldaColor;
begin
	c.col := color;
	HSDBackColor(c);
end;

function HSDRGBColorToHaldaColor(color: RGBColor): HaldaColor;
var
	c: HaldaColor;
begin
	c.red := Trunc(color.red SHR 8);
	c.green := Trunc(color.green SHR 8);
	c.blue := Trunc(color.blue SHR 8);
	HSDRGBColorToHaldaColor := c;
end;

function HSDMakeHaldaColor(color: UInt32): HaldaColor; overload;
var
	c: HaldaColor;
begin
	c.col := color;
	HSDMakeHaldaColor := c;
end;

function HSDMakeHaldaColor(red, green, blue: Real): HaldaColor; overload;
var
	c: HaldaColor;
begin
	c.red := Trunc(red * 255);
	c.green := Trunc(green * 255);
	c.blue := Trunc(blue * 255);
	HSDMakeHaldaColor := c;
end;

var
	gBold: Boolean;

procedure HSDTextFace(theStyle: StyleParameter);
begin
	if (bold and theStyle) <> 0 then
	begin
		theStyle := theStyle xor bold;
		gBold := true;
	end
	else
		gBold := false;
	TextFace(theStyle);
	// Ev särbehandling av bold
end;

procedure HSDDrawString(s: AnsiString);
var
	pt: Point;
begin
	if gBold then
	begin
		GetPen(pt);
		MoveTo(pt.h+1, pt.v);
		DrawString(s);
		MoveTo(pt.h, pt.v);
		DrawString(s);
	end
	else
		DrawString(s);
end;

function HSDStringWidth(s: AnsiString): Longint;
begin
	HSDStringWidth := Trunc(StringWidth(s)); // Depends on port!
end;

procedure HSDInvalRect(theWindow: HaldaWindow; badRect: HaldaRect);
var
	err: OSErr;
begin
	if theWindow <> nil then
		err := InvalWindowRect(theWindow, RectToMacRect(badRect));
end;

procedure HSDInvalRegion(theWindow: HaldaWindow; theRgn: HaldaRegion);
var
	i: Longint;
begin
	for i := 0 to High(theRgn) do
		HSDInvalRect(theWindow, theRgn[i]);
end;

procedure HSDViewSetNeedsDisplay(theView: HaldaViewPtr; doRedisplay: Boolean);
begin
	if theView^.hiview <> nil then
			HIViewSetNeedsDisplay(theView^.hiview, doRedisplay);
end;


// Regioner är ett problem?
// Njae, om det finns i QD/QDCG/QDGL så fixar det sig.

{Rect marking procedure, isolates system dependent code}
{SHOULD BE A CALLBACK, or IFC-ed for different systems}
	procedure HSDMarkRect (box: HaldaRect);
	var
		ps: PenState;
	begin
		GetPenState(ps);
		QDCG.ForeColor(cyanColor);
		PenMode(subPin);
		PaintRect(box);
		ForeColor(blackColor);
		BackColor(whiteColor);
//		PaintRect(MacRectToRect(box));
//		ForeColor(MacOSAll.blackColor);
//		BackColor(MacOSAll.whiteColor);
		SetPenState(ps);
	end;

	procedure HSDMarkRgn (rgn: HaldaRegion);
		var
			ps: PenState;
			i: Longint;
	begin
		GetPenState(ps);
		QDCG.ForeColor(cyanColor);
		QDCG.PenMode(subPin);
		QDCG.PenMode(srcBic);
		QDCG.PenMode(srcOr);
//		CGContextSetBlendMode(thePort^.ctx, kCGBlendModeXOR);
//		WriteLn('Drawing rgn of size ', Length(rgn));
		for i := 0 to High(rgn) do
		begin
			PaintRect(rgn[i]);
		end;
//		PaintRgn(rrgn);
		ForeColor(MacOSAll.blackColor);
		BackColor(MacOSAll.whiteColor);
		QDCG.PenMode(srcCopy);
		SetPenState(ps);
	end;

procedure HSDAddRectToRgn(var rgn: HaldaRegion; r: HaldaRect);
begin
	SetLength(rgn, Length(rgn)+1);
	rgn[High(rgn)] := r;
end;

procedure HSDSetRgnToRect(var rgn: HaldaRegion; r: HaldaRect);
begin
	SetLength(rgn, 1);
	rgn[0] := r;
end;

procedure HSDSetRect(var r: HaldaRect; left, top, right, bottom: Real);
begin
	r.left := left;
	r.top := top;
	r.right := right;
	r.bottom := bottom;
end;

function HSDSectRect({const} rect1, rect2: HaldaRect; var dstRect: HaldaRect):Boolean;
begin
	dstRect.left := Max(rect1.left, rect2.left);
	dstRect.top := Max(rect1.top, rect2.top);
	dstRect.right := Min(rect1.right, rect2.right);
	dstRect.bottom := Min(rect1.bottom, rect2.bottom);
	if (dstRect.left > dstRect.right) or (dstRect.top > dstRect.bottom) then
	begin
		HSDSectRect := false;
		HSDSetRect(dstRect, 0,0,0,0);
	end
	else
		HSDSectRect := true;
end;

procedure HSDFrameRect(r: HaldaRect);
begin
	FrameRect(r);
end;

procedure HSDEraseRect(r: HaldaRect);
begin
	EraseRect(r);
end;

procedure HSDMoveTo(h, v: Real);
begin
	MoveTo(h, v);
end;
procedure HSDLineTo(h, v: Real);
begin
	MoveTo(h, v);
end;

procedure HSDGetModsBits(mods: Longint; var shift, alt, control, command: Boolean);
begin
		shift := (mods and shiftKey) <> 0;
//		shift := BitAnd(mods, shiftKey) <> 0;
		alt := BitAnd(mods, optionKey) <> 0;
		control := BitAnd(mods, controlKey) <> 0;
		command := BitAnd(mods, cmdKey) <> 0;
end;

function HSDTickCount: Longint;
begin
	HSDTickCount := TickCount;
end;

var
	xorFlag: Boolean;

procedure HSDXORLine(theView: HaldaViewPtr; p1, p2: HaldaPoint; tempPort: Boolean);
var
	ps: PenState;
	r: Rect;
begin
//	GetPort(savePort);
//	SetPortWindowPort(theView^.window);
	if tempPort then
		CreatePortWindowPort(theView^.window);
	GetPenState(ps);
//	if xorFlag then
//		ForeColor(MacOSAll.blackColor)
//	else
//		ForeColor(MacOSAll.whiteColor);
	QDCG.ForeColor(whiteColor);
	xorFlag := not xorFlag;
	QDCG.PenMode(patXor);
//	p := HOffsetToPoint(theView, h^.selStart);
//	MoveTo(p.h, p.v);
//	Line(0, h^.rowHeight);
//	QDCG.MoveTo(p1.h-1, p1.v);
//	QDCG.LineTo(p2.h-1, p2.v);
	p1.h := Trunc(p1.h); // Trunc forces on-pixel, which avoids interpolated lines
	p1.v := Trunc(p1.v);
	p2.h := Trunc(p2.h);
	p2.v := Trunc(p2.v);
	SetRect(r, p1.h, p1.v, p1.h+1, p2.v);
	PaintRect(r);
	
	QDCG.ForeColor(blackColor);
	SetPenState(ps);
	QDCG.PenMode(patCopy);
	if tempPort then
		FinishPort;

//	MacOSAll.PenMode(patXor);
//	MacOSAll.MoveTo(p1.h-1, p1.v);
//	MacOSAll.LineTo(p2.h-1, p2.v);
//	MacOSAll.QDFlushPortBuffer(GetWindowPort(theView^.window), nil);
//	SetPort(savePort);
end;

procedure HSDLine(theView: HaldaViewPtr; p1, p2: HaldaPoint; tempPort: Boolean; col: HaldaColor);
var
	ps: PenState;
	r: Rect;
begin
	if tempPort then
		CreatePortWindowPort(theView^.window);
	HSDForeColor(col);
	QDCG.PenMode(patCopy);
	p1.h := Trunc(p1.h); // Trunc forces on-pixel, which avoids interpolated lines
	p1.v := Trunc(p1.v);
	p2.h := Trunc(p2.h);
	p2.v := Trunc(p2.v);
	SetRect(r, p1.h, p1.v, p1.h+1, p2.v);
	PaintRect(r);
	
	if tempPort then
		FinishPort;
end;

function HSDButton: Boolean;
begin
	HSDButton := Button;
end;

function HSDGetMouse: HaldaPoint;
var
	mp: MacOSAll.Point;
begin
	GetMouse(mp);
	HSDGetMouse := MacPointToPoint(mp);
end;

function HDSGetScrapString(var clipData: AnsiString): Integer;
var
	scrap: ScrapRef;
	err: OSErr;
	byteCount: Longint;
begin
	clipData := '';
	err := GetCurrentScrap(scrap);
	if err = noErr then
		err := GetScrapFlavorSize(scrap, 'TEXT', byteCount);
	if err = noErr then
	begin
		SetLength(clipData, byteCount);
		err := GetScrapFlavorData(scrap, 'TEXT', byteCount, @clipData[1]);
	end;
	HDSGetScrapString := err;
end;

function HDSSetScrapString(clipData: AnsiString): Integer;
var
	scrap: ScrapRef;
	err: OSErr;
begin
	ClearCurrentScrap();
	err := GetCurrentScrap(scrap);
	if err <> noErr then
		WriteLn('GetCurrentScrap: ', err);
	if err = noErr then
		err := PutScrapFlavor(scrap, 'TEXT', kScrapFlavorMaskNone, Length(clipData), @clipData[1]);
	if err <> noErr then
		WriteLn('PutScrapFlavor: ', err);
		// -100 no scrap exists
	HDSSetScrapString := err;
end;

function HDSCreateScrollbar(theView: HaldaViewPtr; w: HaldaWindow; r: HaldaRect; scrollHandler: HSDPointerProcPtr): HaldaScrollbar;
var
	scrollbar: HaldaScrollbar;
begin
	CreateScrollBarControl (w, RectToMacRect(r),
			0, 0, 100, Trunc(r.bottom-r.top), true, nil{LiveScrollProcV}, scrollbar);
	VMInstallDefaultScrollHandler(scrollbar, 1, 10);
	// Borde auto-resize med fönstret
//	InstallNumViewHandler(w, scrollbar, theView^.topRow, @HitVScroll);
	InstallViewHandlerByRef(w, scrollbar, kViewDataCustom, theView, scrollHandler, nil);
	
	HDSCreateScrollbar := scrollbar;
end;

procedure HDSDisposeScrollbar(s: HaldaScrollbar);
begin
	DisposeControl(s);
end;

procedure HDSSetScrollbarRange(s: HaldaScrollbar; maxValue: Longint);
begin
	SetControl32BitMaximum(s, maxValue);
	HIViewSetNeedsDisplay(s, true); // Hjälpte inte
end;

function HSDGetScrollbarValue(s: HaldaScrollBar): Longint;
begin
	HSDGetScrollbarValue := GetControl32BitValue(s);
end;
	
procedure HSDSetScrollbarValue(s: HaldaScrollBar; value: Longint);
begin
	SetControl32BitValue(s, value);
end;

procedure HSDSetScrollbarMax(s: HaldaScrollBar; value: Longint);
begin
	SetControl32BitMaximum(s, value);
end;

procedure HSDSetControlViewSize(s: HaldaScrollBar; range: Longint);
begin
	SetControlViewSize(s, range);
end;

function HDSGetWindowPortRect(w: HaldaWindow): HaldaRect;
var
	r: MacOSAll.Rect;
begin
	r := MacOSAll.GetWindowPortBounds(w, r)^;
	HDSGetWindowPortRect := MacRectToRect(r);
end;

function HDSGetViewPortRect(view: HaldaViewPtr): HaldaRect;
var
	r: MacOSAll.Rect;
begin
	if view^.window <> nil then
		r := MacOSAll.GetWindowPortBounds(view^.window, r)^;
	if view^.hiview <> nil then
//		MacOSAll.GetControlBounds(view^.hiview, r);
		r := RectToMacRect(HDSGetWindowPortRect(HIViewGetWindow(view^.hiview))); // frame rect
	HDSGetViewPortRect := MacRectToRect(r);
end;

procedure HSDSetScrollbarRect(scroll: HaldaScrollbar; r: HaldaRect);
begin
	SizeControl(scroll, Trunc(r.right - r.left), Trunc(r.bottom - r.top));
	MoveControl(scroll, Trunc(r.left), Trunc(r.top));
end;

procedure HSDSetScrollSteps(scroll: HaldaScrollbar; buttonStep, pageStep: Integer);
begin
	VMInstallDefaultScrollHandler(scroll, buttonStep, pageStep);
end;

// Inaktuell? Ser skum ut!
//procedure HSDSetPortWindowPort(view: HaldaViewPtr);
//begin
//	if view^.window <> nil then
//		SetPortWindowPort(view^.window);
//	CreatePortQDPort(MacOSAll.GrafPtr(nil));
//end;

procedure HSDFinishPort; // QDCG finish port
begin
	FinishPort;
end;

end.

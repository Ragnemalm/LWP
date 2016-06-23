// QDCG by Ingemar Ragnemalm 2009-2014

// This unit implements a subset of QuickDraw through CG. It is a small "proof-of-concept"
// showing how a modernization of QuickDraw could have been done if there had been any
// respect at all for the need to support old QuickDraw-based code, and not least to
// modernize it with reasonable effort. Converting thousands and tens of thousands lines
// of QuickDraw code takes a lot more time than writing glue code like this!
// So it not only can be done, it is pretty easy (up to a certain point). Making an all-new
// API like CG is easy. Making the move from the old API easy it more challenging,
// but it is not harder that this!

// Thus, there are three goals here:
// ¥ Showing that this kind of glue is quite easy to do.
// ¥ Making glue that makes QD code run even without QD.
// ¥ Creating a path ahead where new features can be added to QD without
// spoiling the concept; simply add what extra you need. So far, floating-
// point precision as well as alpha channel support have been added.
// And as a bonus, I have found this file to be a good reference to CG as well.

// QuickDraw features covered so far:
// SetPort
// Rectangles, rounded rectangles, ovals, lines, polygons and strings.
// A limited set of drawing operations.
// Splines!
// Line width.
// Patterns
// Regions (partially done)
// PDF recording and display
// Clipping
// Text styles

// Additional CG features supported:
// Shadows
// Shadings/gradients
// Double underlines, strikethrough, overline.
// Arrow ends.
// Vector and matrix operations

// CG features to add:
// Blend modes
// Masking
// Layers (partially supported)

// This is designed to work for window ports as well as views. (Views have QD-style
// coordinate system done wrong. The problems have been fixed through QDCG.)

// NOTE THAT MANY FUNCTIONS ARE MERELY PLACEHOLDERS!

// Updated 090427: Text position, font number, rotated text, stroke color on text.
// 090429: Flipping disabled for views. Splines. Rounded-corner rects. HIView demo.
// This is version 1.0!

// 091229: Used succcessfully in TransSkel 5! No changes to speak of.
// 100401: Changed to ObjP mode, added UseNSViewContext.
// 100414: Important change: No change of coordinate system in RectToCGRect
// Also added QDGetPictureBounds. RGBBackColor expanded.
// Erase, Invert, Fill of Rect, Oval, RoundRect, Polygon, CubicPolygon
// 100415: Text drawing revised. Pattern phase correct. Demo updated.
// Problems remain with rounded rects! Removed SetPortWindowPort and
// UseWindowContext since we no longer support unflipped contexts nor
// Carbon. Added QDCGContext.
// Somewhere here should be considered 1.1

// 100416: CGPointMake
// 100417: Regions, incomplete. Can build a region from LineTo's.
// 100418: MakePt. Gradients. PNG loader for "pictures". More region support.
// Pictures revised for future PDF support.
// 100420: PtInRgn, ClipRgn, ClipRect, SaveState, RestoreState
// 100426: OffsetRect. PDF support is looking good! PDF and GWorlds are supported
// by two separate units.
// This is version 1.2!

// 100427: PenDash, PenCap added. ForeColor, BackColor with its old constants
// (and a few new ones) + StrokeColor. CreatePoly. Preliminary arrow support.
// GetPen.
// 100505: Integrated arrow rendering in LineTo, PenCap etc.
// This is version 1.3!

// 100511: Fixed bugs in GetPicture that made it sensitive to non-ASCII paths.
// 1005??: Added a variant of FillRect (to be added to other Fill operations)
// where a pattern offset is included.
// 100606: Added a bunch of additional Rect operations

// 101223: Removed all "Stroke" calls. The stroke is now controlled by TextFace -
// as "outline". Instead, TextFillColor/RGBTextFillColor is added for controlling
// the fill of outlined text. It defaults to transparent, which makes this
// solution more QuickDraw compatible. Updated QDCGDemo to use this.
// 101224: Implemented GetFontInfo using ATSUI calls, and prepared for future
// CoreText/CGFont support
// 101225: TextFace now supports underline, strikethru, double underline, overline,
// condense, extend, shadow. Bold and italic are only preliminary.
// 101230: Some qd globals added. InitGraf for initializing those globals.
// This is version 1.4!
// 110402: Updated for new versions of ObjP.
// 110815: Added overloaded variants of of SetRect, LineTo, MoveTo, Line, Move
// - and then changed them to LineToPoint etc since overloading didn't work well
// due to confusion with original QuickDraw calls.
// New variants of the Point and Rect records.
// 111103: Less transforming back, more CGContextSaveGState/CGContextRestoreGState (drawing pictures).
// Replacement function of deprecated CGContextDrawPDFDocument.
// DrawPictureRotated renamed to DrawPicture.
// DrawStringRotated renamed to DrawString.
// This is version 1.5!
// 120101: Fixed bugs in OpenPicture/PictureNextPage/ClosePicture:
// No previous drawing context needed.
// Subsequent pages properly flipped (not upside-down).
// Replaced CGPDFPageGetDrawingTransform with translations and scaling.
// 120102: Arcs implemented, pretty much working but not tested much.
// 120226: Added InternalUpdateFont to StringWidth. (Otherwise the font may not exist or be current.)
// Added DisposeRgn.

// MISSING?
// 120428: Added many new color names, light and dark versions of all.
// 120615: Added variants of SetPort
// 120621: Added GetPortBounds. Added SetPortWindowPort

// 130402: Added light and dark colors. Gives a rather rich palette of named colors!
// 130631: Vector and matrix operations added (preliminary, not tested much)
// 131002: More named transparent colors
// Added RectToNSRect, NSRectToRect, MakeRect
// 131025: Added RectToMacRect, MacRectToRect, PointToMacPoint, MacPointToPoint
// 131105: PtInRect with CGPoint
// 140129: GetPortBounds, UsePortContext, UseWindowContext, made it compatible with TS4/Carbon again.
// Made SetPort general, overloaded, then renamed it to CreatePort in order to avoid confusion.
// 140205: Removed CreatePort for GrafPtr and WindowPtr, since it caused problems.
// Are GrafPtr and WindowPtr still declared as equivalent despite not being that any more?
// So use CreatePortWindowPort and CreatePortQDPort instead.
// 140814: Added * operator for scaling of Rect.
// 141208: Added fields in port as reference to window/qd port/view, in order to inspect size (and maybe more).
// Corrected byte order for some color settings calls. (Works for Intel order only.)
// 160209: Now works for FPC 3.0! (Some new type casts were needed.)


{$mode objfpc}
{$modeswitch objectivec1}
{ $mode macpas}

unit QDCG;
interface
uses
	MacOSAll, CocoaAll, Math; // MacOSAll included for CG

//procedure SetPortWindowPort(wind: WindowPtr);
procedure FinishPort; // Same as QDFlushPortBuffer
procedure QDFlushPortBuffer; // assume current port
// When using an available CGC, i.e. update events, use this:
//procedure UseWindowContext(cgc: CGContextRef; portHeight: Real);
procedure UsePortContext(curPort: MacOSAll.GrafPtr; createPort: Boolean);
//procedure UseWindowContext(w: MacOSAll.WindowPtr; createPort: Boolean); Use CreatePort instead?
procedure UseViewContext(cgc: CGContextRef; portHeight: Real);
procedure UseNSViewContext(view: NSView); // Assumes use in draw method!
function QDCGContext: CGContextRef;

// Replacement of above: SetPort for everything!
// See below!


// Optional initialization only for initializing some qd. globals.
procedure InitGraf;

// Handle update events?
// Should old-style events be considered? BeginUpdate/EndUpdate?
// How about update CarbonEvents? kEventWindowDrawContent
// works fine with old SetPortWindowPort. Could possibly work
// just as well here? Needs testing!
// Set port to view? Response to view redraw event?

// Old types redefined.
type
	Point = record
		case SInt16 of
			0:(v,h: Real;);
			1:(vh: array[0..1] of Real;);
			2:(y,x: Real;);
	end;
	PointPtr = ^Point;
	Rect = record
		case SInt16 of
			0:(top,left,bottom, right: Real;);
			1:(topLeft, botRight: Point;);
			2:(arr: array[0..3] of Real;);
	end;
	RectPtr = ^Rect;

type
	FontInfoPtr = ^FontInfo;
	FontInfo = record
		ascent:		Real;
		descent:		Real;
		widMax:		Real;
		leading:		Real;
		// Added:
		underlinePos, underlineThickness: Real;
		maxWidth, avgWidth: Real;
	end;

type
	Picture = record
		picFrame: Rect;
//		picType: Integer;
// CGImage version:
		picImage: CGImageRef;
// PDF version:
		picPDF: CGPDFDocumentRef;
		picPDFDataRef: CFDataRef;
//		picPDFData: Pointer;
//		picPDFLength: Longint;
	end;
	PicHandle = ^Picture; // CGImageRef;

	FakeBitMap = record
		bounds: Rect;
	end;
var
	qd: record
		screenBits: FakeBitMap;
		black, white, dkGray, ltGray: PicHandle;
		// thePort? Meaningful?
	end;
	

// Utilities

// CGRectMake should be in FPCMacOSAll/RTL in some way
function CGRectMake(x: Single; y: Single; width: Single; height: Single): CGRect;
function RectToCGRect(r: Rect): CGRect;
function CGRectToRect(viewRect: CGRect): Rect;
function CGPointMake(x, y: Real): CGPoint;

// Classic integer Rect <-> QDCG floating-point Rect and the same for Point
function RectToMacRect(r: Rect): MacOSAll.Rect;
function MacRectToRect(r: MacOSAll.Rect): Rect;
function PointToMacPoint(r: Point): MacOSAll.Point;
function MacPointToPoint(r: MacOSAll.Point): Point;

// CG Points, NS Rects
function PointToCGPoint(p: Point): CGPoint;
function CGPointToPoint(p: CGPoint): Point;
function RectToNSRect(r: Rect): NSRect;
function NSRectToRect(viewRect: NSRect): Rect;

// Extra TextFace options
// The ordinary:
const
	normal = 0;
	bold = 1;
	italic = 2;
	underline = 4;
	outline = 8;
	shadow = $10;
	condense = $20;
	extend = $40;
//extra
	strikethru = $80;
	strikethrough = $80;
	doubleunderline = $100;
	overline = $200;

// Old functions redefined

procedure TextFont(font: SInt16);overload; // Not recommended
procedure TextFont(fontName: AnsiString); overload;
procedure TextFace(face: StyleParameter);
procedure TextExtend(extendValue: Real); // New call, complement to condense and extend that allows more freedom
procedure TextMode(mode: SInt16);
procedure TextSize(size: Real);
procedure DrawChar(ch: CharParameter);
procedure DrawString(const (*var*) s: Str255); overload;
procedure DrawString(const (*var*) s: Str255; angle: Real); overload;
function CharWidth(ch: CharParameter): Real;
function StringWidth(const (*var*) s: Str255): Real;
procedure GetFontInfo(var info: FontInfo);

procedure SetRect(var r: Rect; left, top, right, bottom: Real); overload;
procedure SetRect(var r: Rect; topLeft, botRight: Point); overload;
function MakePt(h, v: Real): Point;
function MakeRect(left, top, right, bottom: Real): Rect;

// Old constants for ForeColor/BackColor, redefined to be more generally usable
const
	blackColor: UInt32 = $000000ff;
	whiteColor: UInt32 = $ffffffff;
	redColor = $ff0000ff;
	greenColor = $00ff00ff;
	blueColor = $0000ffff;
	yellowColor = $ffff00ff;
	cyanColor = $00ffffff;
	magentaColor = $ff00ffff;
// A few new ones that may come in handy
	greyColor = $7f7f7fff;
	lightGreyColor = $afafafff;
	darkGreyColor = $3f3f3fff;
	grayColor = $7f7f7fff;
	lightGrayColor = $afafafff;
	darkGrayColor = $3f3f3fff;
	orangeColor = $ff7f00ff;
	brownColor = $7f3f00ff;

// Basic transparent colors
	transpGreyColor = $7f7f7f7f;
	transpGrayColor = $7f7f7f7f;
	transpBlackColor = $0000007f;
	transpWhiteColor = $ffffff7f;
	transpRedColor = $ff00007f;
	transpGreenColor = $00ff007f;
	transpBlueColor = $0000ff7f;
	transpYellowColor = $ffff007f;
	transpCyanColor = $00ffff7f;
	transpMagentaColor = $ff00ff7f;

// light and dark colors
	lightRedColor = $ff8080ff;
	lighterRedColor = $ffc0c0ff;
	darkRedColor = $800000ff;
	lightGreenColor = $80ff80ff;
	lighterGreenColor = $c0ffc0ff;
	darkGreenColor = $008000ff;
	lightBlueColor = $A0A0ffff;
//	lightBlueColor = $8080ffff;
	lighterBlueColor = $D0D0ffff; // Blue needs an adjustment since the eyes are non-linear
//	lighterBlueColor = $c0c0ffff;
	darkBlueColor = $0000C0ff;
//	darkBlueColor = $000080ff;
	lightYellowColor = $ffff80ff;
	lighterYellowColor = $ffffc0ff;
	darkyellowColor = $C0C000ff; // Not really so dark
	darkeryellowColor = $808000ff; // Not really so dark
	lightCyanColor = $80ffffff;
	lighterCyanColor = $C0ffffff;
	darkcyanColor = $00C0C0ff;
	darkercyanColor = $008080ff;
	lightmagentaColor = $ff80ffff;
	lightermagentaColor = $ffC0ffff;
	darkmagentaColor = $C000C0ff;
	darkermagentaColor = $800080ff; // purple
	purpleColor = darkermagentaColor;
	lightPurpleColor = darkmagentaColor;
	lighterPurpleColor = magentaColor;
	darkpurpleColor = $400040ff;
	lightorangeColor = $ffC080ff;
	lighterorangeColor = $ffE0C0ff;
	darkorangeColor = $804000ff; // =brown
	lightbrownColor = $ff7f00ff; // =orange
	darkbrownColor = $402000ff;
	lighterGreyColor = $cfcfcfff;
	lighterGrayColor = $cfcfcfff;

	yelloworangeColor = $ffC000ff;
	yellowgreenColor = $7fff00ff;
	
procedure ForeColor(color: SInt32); overload;
procedure BackColor(color: SInt32); overload;
procedure TextFillColor(color: SInt32); overload;
procedure ForeColor(color: SInt32; alpha: Real); overload;
procedure BackColor(color: SInt32; alpha: Real); overload;
procedure TextFillColor(color: SInt32; alpha: Real); overload;

procedure RGBForeColor(r,g,b,a: Real); overload;
procedure RGBForeColor(r,g,b: Real); overload;
procedure RGBForeColor(col: RGBColor); overload;
procedure RGBBackColor(r,g,b,a: Real); overload;
procedure RGBBackColor(r,g,b: Real); overload;
procedure RGBBackColor(const (*var*) col: RGBColor); overload;

procedure RGBTextFillColor(r,g,b,a: Real); overload;
procedure RGBTextFillColor(r,g,b: Real); overload;
procedure RGBTextFillColor(const (*var*) col: RGBColor); overload;


procedure PaintRect(r: Rect);
procedure FrameRect(const (*var*) r: Rect);
procedure EraseRect(const (*var*) r: Rect);
procedure InvertRect(const (*var*) r: Rect);
procedure FillRect(const (*var*) r: Rect; const (*var*) pat: PicHandle);overload;
procedure FillRect(const (*var*) r: Rect; const (*var*) pat: PicHandle; offsetx, offsety: Real);overload;
procedure FrameOval(const (*var*) r: Rect);
procedure PaintOval(const (*var*) r: Rect);
procedure EraseOval(const (*var*) r: Rect);
procedure InvertOval(const (*var*) r: Rect);
procedure FillOval(const (*var*) r: Rect; const (*var*) pat: PicHandle);
procedure FrameRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real);
procedure PaintRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real);
procedure EraseRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real);
procedure InvertRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real);
procedure FillRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real; const (*var*) pat: PicHandle);
// Arcs, use paths
procedure FrameArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16);
procedure PaintArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16);
procedure EraseArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16);
procedure InvertArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16);
procedure FillArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16; const (*var*) pat: PicHandle);

// Regions? Paths again? HIShape?
type
	Region = record
		thePath: CGMutablePathRef;
	end;
	RgnHandle = ^Region;
function NewRgn: RgnHandle;
procedure OpenRgn;
procedure CloseRgn(dstRgn: RgnHandle);
function PtInRgn(pt: Point; rgn: RgnHandle): boolean;
function PointInRegion(pt: Point; rgn: RgnHandle): boolean;
procedure FrameRgn(rgn: RgnHandle);
procedure PaintRgn(rgn: RgnHandle);
procedure DisposeRgn(rgn: RgnHandle);
// Etc

type
//	PointArray = array of Point;
	MacPolygonPtr = ^MacPolygon;
	MacPolygon = record
		polySize:				SInt16; // Number of points (old meaning not relevant)
		polyBBox:				Rect;
		polyPoints:				{PointArray;} array of Point;
	end;
	PolyHandle = ^MacPolygonPtr;


function OpenPoly: PolyHandle;
procedure ClosePoly;
procedure KillPoly(poly: PolyHandle);
function CreatePoly(points: array of Point): PolyHandle; // Easier polygon creation
//procedure OffsetPoly(poly: PolyHandle; dh: SInt16; dv: SInt16);
procedure FramePoly(poly: PolyHandle);//overload;
procedure PaintPoly(poly: PolyHandle);
procedure ErasePoly(poly: PolyHandle);
procedure InvertPoly(poly: PolyHandle);
procedure FillPoly(poly: PolyHandle; const (*var*) pat: PicHandle);

// Cubic splines
procedure FrameCubicPoly(poly: PolyHandle);//overload;
procedure PaintCubicPoly(poly: PolyHandle);
procedure EraseCubicPoly(poly: PolyHandle);
procedure InvertCubicPoly(poly: PolyHandle);
procedure FillCubicPoly(poly: PolyHandle; const (*var*) pat: PicHandle);

// Quadratic splines
procedure FrameQuadraticPoly(poly: PolyHandle); // overload;
procedure PaintQuadraticPoly(poly: PolyHandle);
procedure EraseQuadraticPoly(poly: PolyHandle);
procedure InvertQuadraticPoly(poly: PolyHandle);
procedure FillQuadraticPoly(poly: PolyHandle; const (*var*) pat: PicHandle);

// Lines/curves with arrows: (obsolete - use PenCap)
procedure ArrowNockStyle(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
procedure ArrowHeadStyle(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
procedure ArrowStyle(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
procedure ArrowNockStyle(theStyle: Integer);overload;
procedure ArrowHeadStyle(theStyle: Integer);overload;
procedure ArrowStyle(theStyle: Integer);overload;

//procedure ArrowTo(h,v: Real{; arrowAtStart, arrowAtEnd: Boolean});
//procedure ArrowCubicPoly(poly: PolyHandle{; arrowAtStart, arrowAtEnd: Boolean});
//procedure ArrowPoly(poly: PolyHandle{; arrowAtStart, arrowAtEnd: Boolean});

const
	kButt = -1; // No arrow
	kPlainArrowHead = 0; // Simple triangle
	kPlainArrowNock = 1;
	kSlantedArrowHead = 2; // Nice
	kSlantedArrowNock = 3;
	kRoundArrowHead = 4; // A shovel?
	kRoundArrowNock = 5;
	kTightArrowHead = 6; // Just a triangular tip
	kTightArrowNock = 7;
	kHeartArrowHead = 8; // Heart or spade, as you like it
	kHeartArrowNock = 9;
	kPolyArrowHead = 10; // A bit more squarish
	kPolyArrowNock = 11;
	kDropArrowHead = 12; // Drop-shaped
	kDropArrowNock = 13;
	kSpearArrowHead = 14;
	kSpearArrowNock = 15;
	kSpear2ArrowHead = 16;
	kCrossbowArrowHead = 17;

(*
procedure SetOrigin(h: SInt16; v: SInt16);
procedure InsetRect(var r: Rect; dh: SInt16; dv: SInt16);*)
// SectRect, UnionRect, EqualRect,

procedure PenMode(mode: SInt16);

//procedure OffsetRect(var r: Rect; dh: Real; dv: Real);
procedure OffsetRect(var r: Rect; dh: Real; dv: Real);overload;
procedure OffsetRect(var r: NSRect; dh: Real; dv: Real);overload;
procedure OffsetRect(var r: CGRect; dh: Real; dv: Real);overload;

// Added in 1.4:
function SectRect({const} rect1, rect2: Rect; var dstRect: Rect):Boolean;overload;
function SectRect({const} rect1, rect2: Rect):Boolean;overload;
procedure UnionRect({const} var rect1, rect2: Rect; var dstRect: Rect);
function EqualRect({const} rect1, rect2: Rect):Boolean;
procedure MapRect(var r: Rect; {const}var srcRect, dstRect: Rect);
procedure InsetRect(var r:Rect; dh,dv: Real);

// Constants for FitRectInRect
const
	kFitLeft = 0;
	kFitUp = 0;
	kFitCenter = 1;
	kFitRight = 2;
	kFitDown = 2;
function FitRectInRect(fitThis, inThis: Rect; lrMapping, udMapping: Integer): Rect;
function FitRectInRect(fitThis, inThis: Rect): Rect;


procedure GetPen(var pt: Point);
procedure MoveTo(h,v: Real); // overload;
procedure MoveToPoint(p: Point); // overload;
procedure Move(dh, dv: Real); // overload;
procedure MoveByPoint(p: Point); // overload;
procedure LineTo(h,v: Real); // overload;
procedure LineToPoint(p: Point); // overload;
procedure Line(dh, dv: Real); // overload;
procedure LineByPoint(p: Point); // overload;

// Pen style:
procedure PenSize(width, height: Real); overload;
procedure PenSize(width: Real); overload;

// Advanced pen style: caps and dashes

(*
procedure PenCap(cap: CGLineCap);
// PenDash, wrapper on the CGContextSetLineDash call.
// The first two are convenient calls with no explicit array.
// The third is a Pascal-style array-based call, and the fourth
// is equivalent to the original call, thereby the clumsiest
// and probably superfluous here.
// Note: Phase can usually be zero. You should call PenCap(kCGLineCapButt)
// to avoid that the line segments float into each other.
procedure PenDash(phase, len1, len2: Float32); overload;
procedure PenDash(phase, len1, len2, len3, len4: Float32); overload;
procedure PenDash(phase: Float32; lenList: array of Float32); overload;
procedure PenDash(phase: Float32; lenList: Float32Ptr; lenLength: Longint); overload;
*)

function MakeRGB(r, g, b: UInt16): RGBColor;

// Shadow support
procedure QDCGSetShadow(offsetX, offsetY, blur: Real);overload;
procedure QDCGSetShadow(offsetX, offsetY, blur, r, g, b, a: Real);overload;
procedure QDCGBeginTransparencyLayer;
procedure QDCGEndTransparencyLayer;

// Gradients
type
	ShadingProc = procedure(arg: Float32; var r,g,b,a: Float32);

procedure LinearGradient(p1, p2: Point; r1, g1, b1, a1, r2, g2, b2, a2: Real; callback: ShadingProc);
procedure RadialGradient(p1, p2: Point; radius1, radius2,
						r1, g1, b1, a1, r2, g2, b2, a2: Real;
						callback: ShadingProc);
procedure RadialGradientRgn(rgn: RgnHandle; p1, p2: Point; radius1, radius2,
						r1, g1, b1, a1, r2, g2, b2, a2: Real;
						callback: ShadingProc);
procedure LinearGradientRgn(rgn: RgnHandle; p1, p2: Point;
						r1, g1, b1, a1, r2, g2, b2, a2: Real;
						callback: ShadingProc);

// Clipping
procedure ClipRect(const (*var*) r: Rect);
procedure ClipRgn(rgn: RgnHandle);
procedure SaveState;
procedure RestoreState;

type
	GWorld = record // This is also OK as GrafPtr!
		ctx: CGContextRef;
		height: Real;
		
		portRect: Rect; // Bitmap context only! Other ports may not have this.
		// Pixelsize is always 32 bits per pixel!
// Include a PixMap for bitmap contexts? Include the pixel address?
		temporaryPort: Boolean; // Port that expects FinishPort when done
		isQDPort: Boolean; // Port created from QD or NS port? NS ports should destroy the context.
							// Well... except for offscreen ones! So keep temporaryPort as well!
		
		// References to underlying structure (added 141208):
		theNSView: NSView; // NSView, if the port is that
		qdPort: MacOSAll.GrafPtr; // GrafPtr; // QD port, if that is what it is
		qdWind: MacOSAll.WindowPtr; // QD window, if applicable
		qdView: MacOSAll.Controlref; // QD HIView/Control, if applicable
		// If neither, we just got a cgContext.
		flip: Boolean;
	end;
	GWorldPtr = ^GWorld;
	GrafPtr = GWorldPtr;

var
	thePort: GWorldPtr = nil;
//	gCurrentCGC: CGContextRef;
// gPortHeight byts mot thePort^.height

// SetPort/SetGWorld sets an *existing* QDCG port to current
procedure SetGWorld(gw: GWorldPtr);
procedure SetPort(gw: GWorldPtr);
function GetPort: GrafPtr;overload;
procedure GetPort(var port: GrafPtr);overload;
function GetPortBounds(aPort: GrafPtr): Rect;

//function CreatePort(curPort: MacOSAll.GrafPtr): GWorldPtr;overload;
function CreatePortQDPort(curPort: MacOSAll.GrafPtr): GWorldPtr;
//function CreatePort(w: MacOSAll.WindowPtr): GWorldPtr;overload;
function CreatePort(cgc: CGContextRef; portHeight: Real): GWorldPtr;overload;
function CreatePort(view: HIViewRef; cgc: CGContextRef): GWorldPtr;overload;
function CreatePort(view: NSView): GWorldPtr;overload;
function CreatePort(w: NSWindow): GWorldPtr;overload;
function CreatePortWindowPort(w: NSWindow): GWorldPtr;overload;
function CreatePortWindowPort(w: MacOSAll.WindowPtr): GWorldPtr;overload;

// I do NOT use SetPort, but renamed all port creation calls to
// "CreatePort". Reason: The classic GetPort/SetPort for restoring the
// old port will not work with QDCG! CG contexts must be
// finished, released, and SetPort will create a new one
// instead of just setting it! No good!
// Maybe CreatePort when creating? All CreatePort returns a
// "new" GrafPtr/GWorldPtr!

procedure Normalize(var p: Point);
function VectorAdd(a, b: Point): Point;
function VectorSub(a, b: Point): Point;
function DotProduct(a, b: Point): Real;
function ScalarMult(a: Point; s: Real): Point;

procedure AddPt(src: Point; var dst: Point); 
procedure SubPt(src: Point; var dst: Point);
function PtInRect(pt: Point; const (*var*) r: Rect): Boolean;overload;
function PtInRect(pt: CGPoint; const (*var*) r: Rect): Boolean;overload;

procedure PenCapHead(cap: Longint{CGLineCap});overload;
procedure PenCapNock(cap: Longint{CGLineCap});overload;
procedure PenCap(cap: Longint{CGLineCap});overload;

procedure PenCapHead(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
procedure PenCapNock(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
procedure PenCap(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;


procedure PenDash(phase, len1, len2: Float32; round: Boolean); overload;
procedure PenDash(phase, len1, len2, len3, len4: Float32; round: Boolean); overload;
procedure PenDash(phase: Float32; lenList: array of Float32; round: Boolean); overload;
procedure PenDash(phase: Float32; lenList: Float32Ptr; lenLength: Longint; round: Boolean); overload;


// Vector and matrix operations
// Added june 2013 (preliminary, not tested much)

operator + (a, b: Point) c: Point;
operator - (a, b: Point) c: Point;
operator * (a, b: Point) c: Real; // Dot product
operator * (a: Real; b: Point) c: Point;
operator * (a: Point; b: Real) c: Point;
operator / (a: Point; b: Real) c: Point;
operator / (a, b: Point) c: Real; // Cross product in 2D produces a Real
operator + (a: Point; b: Rect) c: Rect;
operator + (b: Rect; a: Point) c: Rect;
//procedure Normalize(var a: Point);
operator * (a: Real; b: Rect) c: Rect;
operator * (b: Rect; a: Real) c: Rect;

// Add matrix mult? 3x3 matrix with homogenous coordinates.
type
	mat3 = array[0..8] of Real;

function MakeMatrix(a,b,c,d,e,f,g,h,i: Real): mat3;
function MultMat3(a, b: mat3): mat3;
function MultMat3ByVector (a: mat3; b: Point): Point;
function IdentityMatrix: mat3;
function RotationMatrix(a: GLfloat): mat3;
function TranslationMatrix(a, b: GLfloat): mat3;
function ScaleMatrix(sx, sy: GLfloat): mat3;
operator * (a, b: mat3) c: mat3;
operator * (a: mat3; b: Point) c: Point;

procedure ClosePort(var aPort: GrafPtr); // Has to be declared after GrafPtr


implementation

procedure ArrowPoly(poly: PolyHandle; mode: Integer); {0 = linear, 1 = quadratic, 2 = cubic}
	forward;
procedure ArrowTo(h,v: Real{; arrowAtStart, arrowAtEnd: Boolean});
	forward;

// Ports! Important!
// Two cases: temporary CGC or provided by the system.
// Temporary: Use SetPortWindowPort and QDFlushPortBuffer. Note that the latter closes the port!
// System-provided: Use SetContext!

var
//	gCurrentCGC: CGContextRef;
//	gCurrentWindow: WindowPtr;
//	gPortHeight: Real;
	gCurrentFontName: AnsiString = 'Geneva';
	gCurrentFontSize: Real = 12;
	gCurrentPoint: Point;
	gRecordingPolygon: Boolean = false;
	gPenSize: Real; // needed for arrows
	gArrowAtStart: Boolean = false;
	gArrowAtEnd: Boolean = false;
	gTextFace: StyleParameter;
	gExtendValue: Real = 1.5;
	
	gRecordingRgn: Boolean = false;
	gRgnInRecording: CGMutablePathRef;
	
	gPolygonInProgress: PolyHandle; // MacPolygon;
	gFlipMode: Integer;
	gFontExists: Boolean = false;
//	gTemporaryPort: Boolean = false;

	gBackColorRed: Real = 1; gBackColorGreen: Real = 1; gBackColorBlue: Real = 1; gBackColorAlpha: Real = 1;
	gForeColorRed, gForeColorGreen, gForeColorBlue, gForeColorAlpha: Real;
//	gTextFillColorRed, gTextFillColorGreen, gTextFillColorBlue, gTextFillColorAlpha: Real;
	gTextFillColorRed: Real = 0; gTextFillColorGreen: Real = 0; gTextFillColorBlue: Real = 0; gTextFillColorAlpha: Real = 0;

	gQDInitialized: Boolean = false; // If the qd global record is initialized

//	gCurrentView: NSView;
	
const
	kWindowMode = 0;
	kViewMode = 1;


// Initialize QD Globals
// Required only if you need to access any qd. globals.
// Not otherwise needed by QDCG itself.
// Auto-initialized by some calls but you may need it if you need
// qd globals before calling other QDCG calls.
procedure InitGraf;
var
	screenRect: NSRect;
begin
	if not gQDInitialized then
	begin
		gQDInitialized := true;
		
		screenRect := NSScreen.mainScreen.frame;
		qd.screenBits.bounds := CGRectToRect(CGRect(screenRect));
	end;
end;

procedure ClosePort(var aPort: GrafPtr);
begin
	if aPort = nil then Exit;
	if thePort^.temporaryPort then
		QDEndCGContext(GetQDGlobalsThePort, aPort^.ctx); // Wrong! QuickDraw ports only!
	if thePort = aPort then
		thePort := nil;
	Dispose(aPort);
	aPort := nil;
end;

procedure FinishPort; // Same as QDFlushPortBuffer
begin
//	WriteLn('FinishPort');
	if thePort = nil then Exit;
	if gFlipMode = kViewMode then
	begin
		CGContextScaleCTM(thePort^.ctx, 1, -1);
//		CGContextTranslateCTM(thePort^.ctx, 0, -thePort^.height);
	end;

	if thePort^.temporaryPort then
		QDEndCGContext(GetQDGlobalsThePort, thePort^.ctx);
	// Warning: What happens if the wrong port is passed?
	//thePort^.temporaryPort := false;
	// Dispose thePort?
	Dispose(thePort);
	thePort := nil;
end;

procedure QDFlushPortBuffer; // assume current port
begin
//	QDEndCGContext(GetWindowPort(gCurrentWindow), thePort^.ctx);
end;

// When using an available CGC, i.e. update events, use this for windows:
// Use only for Carbon!
// Causes big problems with Cocoa - for so far unknown reasons!
(*procedure UseWindowContext(cgc: CGContextRef; portHeight: Real);
begin
	thePort^.ctx := cgc;
	thePort^.height := portHeight;
	gFlipMode := kWindowMode;
	gTemporaryPort := false;
	gFontExists := false;
	CGContextSetTextMatrix(cgc, CGAffineTransformIdentity); // Added 0912
	
	gTemporaryPort := false; // Do not QDEndCGContext
end;*)

// To update a Carbon window
// Allocated a port!
procedure UsePortContext(curPort: MacOSAll.GrafPtr; createPort: Boolean);
var
	r: MacOSAll.Rect;
	cgc: CGContextRef;
	err: OSErr;
begin
//	WriteLn('UsePortContext');
	if curPort = nil then
		curPort := GetQDGlobalsThePort;
	r := MacOSAll.GetPortBounds(curPort, r)^;
	err := QDBeginCGContext(curPort, cgc);
	if err <> noErr then
		WriteLn('QDBeginCGContext ', err);

//	UseViewContext(cgc, r.bottom - r.top);

	if not gQDInitialized then
		InitGraf;

//WriteLn('UsePortContext should be OK now');
if cgc = nil then
	WriteLn('But it isnt');
	
//	CGContextTranslateCTM(cgc, 0, portHeight);
//	CGContextScaleCTM(cgc, 1, -1);
	CGContextSetTextMatrix(cgc, CGAffineTransformIdentity); // Added 0912
	
//	if NSGraphicsContext.currentContext.isFlipped then
//		CGContextSetTextMatrix(cgc, CGAffineTransformMakeScale(1, -1));

	CGContextTranslateCTM(cgc, 0, r.bottom - r.top);
	CGContextScaleCTM(cgc, 1, -1);
	
	if createPort or (thePort = nil) then
		thePort := New(GWorldPtr);
	thePort^.height := r.bottom - r.top;
	thePort^.ctx := cgc;
	gFlipMode := kWindowMode;
	gFontExists := false;
//	gTemporaryPort := false; // Do not QDEndCGContext

	// Must be finished with QDEndCGContext!
	thePort^.temporaryPort := true;
end;
//procedure UseWindowContext(w: MacOSAll.WindowPtr);
//var
//	thePort: MacOSAll.GrafPtr;
//begin
//	thePort := GetWindowPort(w);
//	UsePortContext(thePort);
//end;

// ...and this for views. Works well with TransSkel5!
// But only for window? Not for views? But it is the same!
// Changes in VERY strange ways! Must be debugged further!
// It works when it shouldn't! Total madness!
procedure CreatePortInternal(cgc: CGContextRef; portHeight: Real;
		qdWindow: WindowPtr; qdPort: MacOSAll.GrafPtr; nsView: NSView; qdView: HIViewRef; flip, temporary: Boolean);
begin
	if not gQDInitialized then
		InitGraf;

// Now, this is what I think:
// For a window's main view, don't flip.
// For a subview, flip!
// For NSview, who knows? Later!

	if flip then
	begin
		CGContextTranslateCTM(cgc, 0, portHeight);
		CGContextScaleCTM(cgc, 1, -1);
	end;
	CGContextSetTextMatrix(cgc, CGAffineTransformIdentity); // Added 0912
	
//	if NSGraphicsContext.currentContext.isFlipped then
//		CGContextSetTextMatrix(cgc, CGAffineTransformMakeScale(1, -1));
	
//	if createPort or (thePort = nil) then
	thePort := New(GWorldPtr);
	thePort^.theNSView := nsView;
	thePort^.qdPort := qdPort;
	thePort^.qdWind := qdWindow;
	thePort^.qdView := qdView; // Shouldn't we pass this?
	thePort^.flip := flip;
// Set parameters
	thePort^.height := portHeight;
	thePort^.ctx := cgc;
	thePort^.temporaryPort := temporary;
	gFlipMode := kViewMode;
	gFontExists := false;
//	gTemporaryPort := false; // Do not QDEndCGContext
end;


// ...and this for views. Works well with TransSkel5!
// But only for window? Not for views? But it is the same!
// Changes in VERY strange ways! Must be debugged further!
// It works when it shouldn't! Total madness!

// Only kept for compatibility! Should be removed!
// Should be replaced by CreatePort/CreatePortInternal and SetPort/UseExistingPort!
procedure UseViewContext(cgc: CGContextRef; portHeight: Real);
begin
	if not gQDInitialized then
		InitGraf;

// Now, this is what I think:
// For a window's main view, don't flip.
// For a subview, flip!
// For NSview, who knows? Later!

//	if flip then
//	begin
//		CGContextTranslateCTM(cgc, 0, portHeight);
//		CGContextScaleCTM(cgc, 1, -1);
//	end;
	CGContextSetTextMatrix(cgc, CGAffineTransformIdentity); // Added 0912
	
//	if NSGraphicsContext.currentContext.isFlipped then
//		CGContextSetTextMatrix(cgc, CGAffineTransformMakeScale(1, -1));
	
//	if createPort or (thePort = nil) then
	thePort := New(GWorldPtr);
// Don't know about these here
	thePort^.theNSView := nil;
	thePort^.qdPort := nil;
	thePort^.qdWind := nil;
	thePort^.qdView := nil; // Shouldn't we pass this?
//	thePort^.flip := flip;
// Set parameters
	thePort^.height := portHeight;
	thePort^.ctx := cgc;
	gFlipMode := kViewMode;
	gFontExists := false;
//	gTemporaryPort := false; // Do not QDEndCGContext
end;

// Maybe use this from SetPort? DRAFT!
procedure UseExistingPort(aPort: GrafPtr);
begin
	if not gQDInitialized then
		InitGraf;

	if aPort^.flip then
	begin
		CGContextTranslateCTM(aPort^.ctx, 0, aPort^.height);
		CGContextScaleCTM(aPort^.ctx, 1, -1);
	end;
	CGContextSetTextMatrix(aPort^.ctx, CGAffineTransformIdentity); // Added 0912
	
//	if NSGraphicsContext.currentContext.isFlipped then
//		CGContextSetTextMatrix(cgc, CGAffineTransformMakeScale(1, -1));
	
	thePort := aPort;
end;



procedure UseNSViewContext(view: NSView);
var
	ctx: NSGraphicsContext;
	cgc: CGContextRef;
begin
	if not gQDInitialized then
		InitGraf;
	
	ctx := NSGraphicsContext.currentContext; // Assume we are in a draw method
	cgc := CGContextRef(ctx.graphicsPort);
//	UseViewContext(cgc, view.bounds.size.height);

	if NSGraphicsContext.currentContext.isFlipped then
		CGContextSetTextMatrix(cgc, CGAffineTransformMakeScale(1, -1));

//	gCurrentView := view;
//	if createPort or (thePort = nil) then
// No, always create a new one, never destroy the old!
	thePort := New(GWorldPtr);
// Don't know about these
	thePort^.qdPort := nil;
	thePort^.qdWind := nil;
	thePort^.qdView := nil;
	
	thePort^.height := view.frame.size.height;
	thePort^.ctx := cgc;
	thePort^.temporaryPort := false; // NS port, don't dispose context
	thePort^.theNSView := view;
	gFlipMode := kViewMode;
	gFontExists := false;
//	gTemporaryPort := false; // Do not QDEndCGContext
end;


// All the above may be replaced by SetPort! Easier to remember, right?
// Or make a similar name, QDCGSetPort?
// Removed since it causes confusion GrafPtr-WindowPtr
//function CreatePort(curPort: MacOSAll.GrafPtr): GWorldPtr;overload;
//begin
//	UsePortContext(curPort, true);
//	CreatePort := thePort;
//end;
function CreatePortQDPort(curPort: MacOSAll.GrafPtr): GWorldPtr;
begin
//	WriteLn('CreatePortQDPort');
	UsePortContext(curPort, true);
	thePort^.qdPort := curPort;
	CreatePortQDPort := thePort;
end;
// Removed since it causes confusion GrafPtr-WindowPtr
//function CreatePort(w: MacOSAll.WindowPtr): GWorldPtr;overload;
//var
//	theQDPort: MacOSAll.GrafPtr;
//begin
//	theQDPort := GetWindowPort(w);
//	UsePortContext(theQDPort, true);
////	UseWindowContext(w, true);
//	CreatePort := thePort;
//end;
function CreatePortWindowPort(w: MacOSAll.WindowPtr): GWorldPtr;overload;
var
	theQDPort: MacOSAll.GrafPtr;
begin
//	WriteLn('CreatePortWindowPort');
	theQDPort := GetWindowPort(w);
	UsePortContext(theQDPort, true);
	thePort^.qdWind := w;
//	UseWindowContext(w, true);
	CreatePortWindowPort := thePort;
end;
// Should this be avoided, since it gives no information about the source?
function CreatePort(cgc: CGContextRef; portHeight: Real): GWorldPtr;overload;
begin
	thePort := nil;
	UseViewContext(cgc, portHeight);
	CreatePort := thePort;
end;
function CreatePort(view: HIViewRef; cgc: CGContextRef): GWorldPtr;overload;
var
	r: MacOSAll.Rect;
begin
	thePort := nil;
	MacOSAll.GetControlBounds(view, r);
//	UseViewContext(cgc, r.bottom-r.top);
WriteLn('Calling CreatePortInternal');
	CreatePortInternal(cgc, r.bottom-r.top,
		nil, nil, nil, view, true{flip}, false{temporary});
//	thePort^.qdView := view;
	CreatePort := thePort;
end;
function CreatePort(view: NSView): GWorldPtr;overload;
begin
	UseNSViewContext(view);
	CreatePort := thePort;
end;

//procedure CreatePort(gw: GWorldPtr);overload;
//begin
//	UseViewContext(gw^.ctx, gw^.height);
//end;
function CreatePort(w: NSWindow): GWorldPtr;overload;
begin
	UseNSViewContext(w.contentView);
	CreatePort := thePort;
end;
function CreatePortWindowPort(w: NSWindow): GWorldPtr;overload;
begin
	UseNSViewContext(w.contentView);
	CreatePortWindowPort := thePort;
end;




function QDCGContext: CGContextRef;
begin
	if not gQDInitialized then
		InitGraf;
	
	QDCGContext := thePort^.ctx;
end;

// Good old SetPort/GetPort are back
// UseViewContext is harmless since it works on an EXISTING context
// No! It allocates a new port!

// This is NOT good! You can't save the current port and reuse it!
// This should be fixed!

procedure SetGWorld(gw: GWorldPtr);
begin
	thePort := gw;
	if gw <> nil then
		UseViewContext(gw^.ctx, gw^.height);
end;
procedure SetPort(gw: GWorldPtr);
begin
	thePort := gw;
	if gw <> nil then
		UseViewContext(gw^.ctx, gw^.height);
end;

// Added at synch with KK version
// Replaced by exported SetPort above.
(*
procedure SetPort(gw: GWorldPtr);overload;
begin
	UseViewContext(gw^.ctx, gw^.height);
end;
procedure SetPort(w: NSWindow);overload;
begin
	UseNSViewContext(w.contentView);
end;
procedure SetPort(view: NSView);overload;
begin
	UseNSViewContext(view);
end;
procedure SetPortWindowPort(w: NSWindow);
begin
	UseNSViewContext(w.contentView);
end;*)


function GetPort: GrafPtr;overload;
begin
	// QDCG borde packa info i struktur, returnera pekare!
	GetPort := thePort;
end;
procedure GetPort(var port: GrafPtr);overload;
begin
	// QDCG borde packa info i struktur, returnera pekare!
	port := thePort;
end;

// Synk
// This was not working at all before! Now... hopefully, if there is something to measure.
// 141208
function GetPortBounds(aPort: GrafPtr): Rect;
var
	bounds: MacOSAll.Rect;
begin
	if aPort = nil then
		aPort := thePort;
	if aPort^.theNSView <> nil then
	begin
//		aPort^.portRect := NSRectToRect(aPort^.theNSView.contentSize);
//		aPort^.portRect := NSRectToRect(aPort^.theNSView.frame);
//		aPort^.portRect := NSRectToRect(aPort^.theNSView.contentSize);
//		aPort^.portRect := NSRectToRect(aPort^.theNSView.contentSize);
//		aPort^.portRect := NSRectToRect(aPort^.theNSView.contentSize);
	end;
	if aPort^.qdPort <> nil then
	begin
//		bounds := GetPortBounds(aPort^.qdPort);
		aPort^.portRect := MacRectToRect(bounds); // This looks wrong!!!
	end;
	if aPort^.qdWind <> nil then
	begin
		GetWindowPortBounds(aPort^.qdWind, bounds);
		aPort^.portRect := MacRectToRect(bounds);
	end;
	if aPort^.qdView <> nil then
	begin
		GetControlBounds(aPort^.qdView, bounds);
		aPort^.portRect := MacRectToRect(bounds);
	end;
	
	GetPortBounds := aPort^.portRect;

// QuickDraw ports
//	r := MacOSAll.GetPortBounds(curPort, r)^;

end;

// This seems to be pointless now.
function FlipY(y: Real): Real; inline;
begin
//	if gFlipMode = kWindowMode then
//		y := thePort^.height - y;
//	if gFlipMode = kViewMode then
	
//	if gFlipMode <> kFlippedViewMode then
//		y := thePort^.height - y;
	
	result := y;
end;

// Utilities

// CGRectMake and CGPointMake should really be in MacOSAll/RTL in some way
function CGRectMake(x: Single; y: Single; width: Single; height: Single): CGRect;
var
	cgr: CGRect;
begin
	cgr.origin.x := x;
	cgr.origin.y := y;
	cgr.size.width := width;
	cgr.size.height := height;
	result := cgr;
end;

function CGPointMake(x, y: Real): CGPoint;
begin
	CGPointMake.x := x;
	CGPointMake.y := y;
end;

// Important change 100414: No change of coordinate system here!
function RectToCGRect(r: Rect): CGRect;
begin
	result := CGRectMake(r.left, r.top, r.right-r.left, r.bottom-r.top);
end;

function CGRectToRect(viewRect: CGRect): Rect;
var
	r: Rect;
begin
	SetRect(r, viewRect.origin.x, viewRect.origin.y, viewRect.origin.x + viewRect.size.width, viewRect.origin.y + viewRect.size.height);
	result := r;
end;

// Conversion for old QuickDraw (integer <-> floating point)
function RectToMacRect(r: Rect): MacOSAll.Rect;
var
	rr: MacOSAll.Rect;
begin
	MacOSAll.SetRect(rr, Trunc(r.left), Trunc(r.top), Trunc(r.right), Trunc(r.bottom));
	RectToMacRect := rr;
end;

function MacRectToRect(r: MacOSAll.Rect): Rect;
var
	rr: Rect;
begin
	SetRect(rr, r.left, r.top, r.right, r.bottom);
	MacRectToRect := rr;
end;

function PointToMacPoint(r: Point): MacOSAll.Point;
var
	rr: MacOSAll.Point;
begin
	rr.h := Trunc(r.h);
	rr.v := Trunc(r.v);
	PointToMacPoint := rr;
end;

function MacPointToPoint(r: MacOSAll.Point): Point;
var
	rr: Point;
begin
	rr.h := r.h;
	rr.v := r.v;
	MacPointToPoint := rr;
end;



function PointToCGPoint(p: Point): CGPoint;
begin
	PointToCGPoint.x := p.h;
	PointToCGPoint.y := p.v;
end;

function CGPointToPoint(p: CGPoint): Point;
begin
	CGPointToPoint.h := p.x;
	CGPointToPoint.v := p.y;
end;

function RectToNSRect(r: Rect): NSRect;
//var
//	nr: NSRect;
begin
	RectToNSRect := NSRect(RectToCGRect(r));
//	nr.size.height := r.bottom-r.top;
//	nr.size.width := r.right-r.left;
//	nr.origin.x := r.left;
//	nr.origin.y := r.top;
//	RectToNSRect := nr;
end;

function NSRectToRect(viewRect: NSRect): Rect;
//var
//	r: Rect;
begin
//	SetRect(r, viewRect.origin.x, viewRect.origin.y, viewRect.origin.x + viewRect.size.width, viewRect.origin.y + viewRect.size.height);
//	NSRectToRect := r;
	NSRectToRect := CGRectToRect(CGRect(viewRect));
end;


// Old functions redefined

// Only a few of these are truly useful. Use font names instead.
const
	kFontNames: array [1..24] of AnsiString =
		(
		'Chicago', // No longer standard
		'NewYork', // No longer standard
		'Geneva',
		'Monaco',
		'Venice', // No longer standard
		'London', // No longer standard
		'Athens', // No longer standard
		'SanFrancisco', // No longer standard
		'Toronto', // No longer standard
		'',
		'Cairo', // No longer standard
		'LosAngeles', // No longer standard
		'',
		'',
		'',
		'',
		'',
		'',
		'',
		'Times',
		'Helvetica',
		'Courier',
		'Symbol',
		'Mobile' // No longer standard
		);


// Preliminary font selection based on strings only. THIS WILL FAIL SOMETIMES.
// Should be replaced by a formal font selector. This is preliminary.
procedure InternalUpdateFont;
var
	fn: AnsiString;
begin
	fn := gCurrentFontName;
	if (gTextFace and bold) <> 0 then
		fn := fn + ' Bold';
	if (gTextFace and italic) <> 0 then
		fn := fn + ' Italic';
	
	if thePort <> nil then
	begin
//	WriteLn('Set font (InternalUpdateFont) to ', fn);
		CGContextSelectFont(thePort^.ctx, @fn[1], gCurrentFontSize, kCGEncodingMacRoman);
		gFontExists := true;
	end
	else
		gFontExists := true; // Why?
end;

// Old-style font number
procedure TextFont(font: SInt16);overload;
begin
//	WriteLn('Set font (TextFont) by ', font, ' to ', kFontNames[font]);
	if kFontNames[font] <> '' then
		gCurrentFontName := kFontNames[font];
//	else
//		WriteLn('Warning: Empty font name');
	gFontExists := false;
	
//	if (font >= Low(kFontNames)) and (font <= High(kFontNames)) then
//		CGContextSelectFont(thePort^.ctx, @kFontNames[font][1], 20, kCGEncodingMacRoman);
//	gFontExists := true;
end;

// New call, using the name directly
procedure TextFont(fontName: AnsiString); overload;
begin
//	WriteLn('Set font (TextFont) to ', fontName);
//	if fontName <> '' then
//		gCurrentFontName := fontName;
//	else
//		WriteLn('Warning: Empty font name');
	if fontName <> '' then
	if fontName <> gCurrentFontName then
	begin
		WriteLn('Set font (TextFont) to ', fontName);
		gCurrentFontName := fontName;
		gFontExists := false;
	end;
	
//	CGContextSelectFont(thePort^.ctx, @gCurrentFontName[1], gCurrentFontSize, kCGEncodingMacRoman);
//	gFontExists := true;
end;
procedure TextFace(face: StyleParameter);
begin
// Use this info to add Bold or Oblique to font name - if supported!
// Hos can this be made safe?
	if face <> gTextFace then
	begin
		gTextFace := face;
		gFontExists := false;
	end;
end;
procedure TextExtend(extendValue: Real);
begin
	gExtendValue := extendValue;
end;
procedure TextMode(mode: SInt16);
begin
end;
procedure TextSize(size: Real);
begin
	if size <> gCurrentFontSize then
	begin
		gCurrentFontSize := size;
//WriteLn(size:1:1);
//	CGContextSelectFont(thePort^.ctx, @gCurrentFontName[1], gCurrentFontSize, kCGEncodingMacRoman);
// CGContextSetFontSize
		gFontExists := false;
	end;
end;

procedure DrawString(const (*var*) s: Str255); overload;
begin
	DrawString(s, 0.0);
end;

procedure DrawString(const (*var*) s: Str255; angle: Real);overload;
var
	myTextTransform: CGAffineTransform;
	where: CGPoint;
	wherefrom: Point;
	finfo: FontInfo;
	savePenSize: Real;
	underlineOffset: Point;
	textExtend: Real;
begin
// How to draw a plain string in CG:
//	CGContextSelectFont (context, "Helvetica", 60, kCGEncodingMacRoman);
//	CGContextSetCharacterSpacing (context, 10);
//	CGContextSetTextDrawingMode (context, kCGTextFill);
//	CGContextSetGrayFillColor(context, 0.0, 1.0);

	if not gFontExists then
	begin
		InternalUpdateFont;
//		CGContextSelectFont(thePort^.ctx, @gCurrentFontName[1], gCurrentFontSize, kCGEncodingMacRoman);
//		gFontExists := true;
	end;

//	if gHasStroke then
	if (gTextFace and outline) <> 0 then
	begin
		CGContextSetTextDrawingMode(thePort^.ctx, kCGTextFillStroke);

		CGContextSetRGBStrokeColor(thePort^.ctx, gForeColorRed, gForeColorGreen, gForeColorBlue, gForeColorAlpha);
		CGContextSetRGBFillColor(thePort^.ctx, gTextFillColorRed, gTextFillColorGreen, gTextFillColorBlue, gTextFillColorAlpha);
	end
	else
		CGContextSetTextDrawingMode(thePort^.ctx, kCGTextFill);

	wherefrom := gCurrentPoint; // CGContextGetTextPosition(thePort^.ctx);

	textExtend := 1.0;
	if (gTextFace and extend) <> 0 then
		textExtend := gExtendValue;
	if (gTextFace and condense) <> 0 then
		textExtend := 1/gExtendValue;

	CGContextSaveGState (thePort^.ctx);
	
	// A simple default shadow for text
	// I could add customization of this, but the host can just as well use
	// QDCGSetShadow directly.
	if (gTextFace and shadow) <> 0 then
		QDCGSetShadow(gCurrentFontSize/8, gCurrentFontSize/8, 4, 0, 0, 0, 1);

// Scale for extend/condense, rotate, scale for axis flipping	
	myTextTransform := CGAffineTransformScale(
		CGAffineTransformRotate(
		CGAffineTransformScale(CGAffineTransformIdentity, 1.0, -1.0 ),angle / 180 * Pi),
						textExtend, 1.0);
// Old version, no extend/condense:
//	myTextTransform := CGAffineTransformRotate(
//		CGAffineTransformScale(CGAffineTransformIdentity, 1.0, -1.0 ),angle / 180 * Pi);
	CGContextSetTextMatrix (thePort^.ctx, myTextTransform);
	CGContextShowTextAtPoint (thePort^.ctx, gCurrentPoint.h, gCurrentPoint.v, @s[1], Length(s));
	
	where := CGContextGetTextPosition(thePort^.ctx);
	where.y := FlipY(where.y);
	gCurrentPoint.h := where.x;
	gCurrentPoint.v := where.y;
	
	// Text underlining
	if (gTextFace and underline) <> 0 then
	begin
		GetFontInfo(finfo);
//		WriteLn('underlinepos = ', finfo.underlinePos);
		// How far down should the underline be? Top or middle at underlinePos?
		// This looks better but may not be correct.
		underlineOffset.h := (finfo.underlinePos+finfo.underlineThickness/2) * sin(angle / 180 * Pi);
		underlineOffset.v := (finfo.underlinePos+finfo.underlineThickness/2) * cos(angle / 180 * Pi);
//		WriteLn('underlineoffset.h = ', underlineOffset.h);
//		WriteLn('underlineoffset.v = ', underlineOffset.v);
		
		CGContextRestoreGState (thePort^.ctx);
		CGContextSaveGState (thePort^.ctx);
		
		savePenSize := gPenSize;
		PenSize(finfo.underlineThickness);

		MoveTo(wherefrom.h+underlineOffset.h, wherefrom.v+underlineOffset.v);
		LineTo(where.x+underlineOffset.h, where.y+underlineOffset.v);
//		ForeColor(blackColor);
//		MoveTo(wherefrom.h, wherefrom.v);
//		LineTo(where.x, where.y);
		
		gPenSize := savePenSize;
		
		gCurrentPoint.h := where.x;
		gCurrentPoint.v := where.y;
	end;
	// Text strikethru
	if (gTextFace and strikethru) <> 0 then
	begin
		GetFontInfo(finfo);
		// What is the right strikethrough height? ascent/3 looks OK.
		underlineOffset.h := -finfo.ascent * sin(angle / 180 * Pi)/3;
		underlineOffset.v := -finfo.ascent * cos(angle / 180 * Pi)/3;
		
		CGContextRestoreGState (thePort^.ctx);
		CGContextSaveGState (thePort^.ctx);
		
		savePenSize := gPenSize;
		PenSize(finfo.underlineThickness);
		
		MoveTo(wherefrom.h+underlineOffset.h, wherefrom.v+underlineOffset.v);
		LineTo(where.x+underlineOffset.h, where.y+underlineOffset.v);
//		ForeColor(blackColor);
//		MoveTo(wherefrom.h, wherefrom.v);
//		LineTo(where.x, where.y);
		
		gPenSize := savePenSize;
		
		gCurrentPoint.h := where.x;
		gCurrentPoint.v := where.y;
	end;
	// Text overline
	if (gTextFace and overline) <> 0 then
	begin
		GetFontInfo(finfo);
		underlineOffset.h := -finfo.ascent * sin(angle / 180 * Pi);
		underlineOffset.v := -finfo.ascent * cos(angle / 180 * Pi);
		
		CGContextRestoreGState (thePort^.ctx);
		CGContextSaveGState (thePort^.ctx);
		
		savePenSize := gPenSize;
		PenSize(finfo.underlineThickness);

		MoveTo(wherefrom.h+underlineOffset.h, wherefrom.v+underlineOffset.v);
		LineTo(where.x+underlineOffset.h, where.y+underlineOffset.v);
		
		gPenSize := savePenSize;
		
		gCurrentPoint.h := where.x;
		gCurrentPoint.v := where.y;
	end;
	// Text double underlining
	if (gTextFace and doubleunderline) <> 0 then
	begin
		GetFontInfo(finfo);
		// The finfo.underlineThickness/2 step down may be incorrect but it looks good.
		underlineOffset.h := (finfo.underlinePos+finfo.underlineThickness/2) * sin(angle / 180 * Pi);
		underlineOffset.v := (finfo.underlinePos+finfo.underlineThickness/2) * cos(angle / 180 * Pi);
		
		CGContextRestoreGState (thePort^.ctx);
		CGContextSaveGState (thePort^.ctx);

		savePenSize := gPenSize;
		PenSize(finfo.underlineThickness);
		
		MoveTo(wherefrom.h+underlineOffset.h, wherefrom.v+underlineOffset.v);
		LineTo(where.x+underlineOffset.h, where.y+underlineOffset.v);

		underlineOffset.h := (finfo.underlinePos+finfo.underlineThickness*2) * sin(angle / 180 * Pi);
		underlineOffset.v := (finfo.underlinePos+finfo.underlineThickness*2) * cos(angle / 180 * Pi);

		MoveTo(wherefrom.h+underlineOffset.h, wherefrom.v+underlineOffset.v);
		LineTo(where.x+underlineOffset.h, where.y+underlineOffset.v);
		
		gPenSize := savePenSize;
		
		gCurrentPoint.h := where.x;
		gCurrentPoint.v := where.y;
	end;
	
	CGContextRestoreGState (thePort^.ctx);
end;

function StringWidth(const (*var*) s: Str255): Real;
// http://developer.apple.com/documentation/graphicsimaging/conceptual/drawingwithquartz2d/dq_text/dq_text.html#//apple_ref/doc/uid/TP30001066-CH213-DontLinkElementID_44
//	1.	Call the function CGContextGetTextPosition to obtain the current text position.
//	2.	Set the text drawing mode to kCGTextInvisible using the function CGContextSetTextDrawingMode.
//	3.	Draw the text by calling the function CGContextShowText to draw the text at the current text position.
//	4.	Determine the final text position by calling the function CGContextGetTextPosition.
//	5.	Subtract the starting position from the ending position to determine the width of the text.
var
	where1, where2, v: CGPoint;
begin
	if not gFontExists then
		InternalUpdateFont;
	where1 := CGContextGetTextPosition(thePort^.ctx);
	CGContextSetTextDrawingMode(thePort^.ctx, kCGTextInvisible);
	CGContextShowText(thePort^.ctx, @s[1], Length(s));
	where2 := CGContextGetTextPosition(thePort^.ctx);
//	return where2.x - where1.x; // Assuming horizontal
	v.x := where2.x - where1.x;
	v.y := where2.y - where1.y;
	result := sqrt(v.x*v.x + v.y*v.y);
end;
procedure DrawChar(ch: CharParameter);
begin
	DrawString(ch);
end;
function CharWidth(ch: CharParameter): Real;
begin
	result := StringWidth(ch);
end;

// Duplicate from TransSkel, but it is so simple it doesn't matter
function StringToCFString(input: AnsiString): CFStringRef;
begin
	result :=  CFStringCreateWithCString(nil, @input[1], kCFStringEncodingUTF8);
	// Must be released
end;

procedure GetFontInfo(var info: FontInfo);
var
//	fr: CGFontRef;
//	stringNameCF: CFStringRef;
	
	cfstr: CFStringRef;
	ats_font: ATSFontRef;
	ats_metrics: ATSFontMetrics;
begin
// This is a PROBLEM! Under 10.4 we should ATS (which is hairy) and 10.5 and up we should use Core Text.

// 10.4 solution:
// Is this OK with 10.5 and 10.6 for now?
//WriteLn('GetFontInfo 1 with font "', gCurrentFontName, '"');
	cfstr := StringToCFString(gCurrentFontName);
//WriteLn('GetFontInfo 1.1');
	ats_font := ATSFontFindFromPostScriptName(cfstr, kATSOptionFlagsDefault);
//WriteLn('GetFontInfo 1.2');
	CFRelease(cfstr);
//WriteLn('GetFontInfo 2');
	if ats_font = kATSFontRefUnspecified then
	begin
		WriteLn('kATSFontRefUnspecified');
		info.ascent := 0;
		info.descent := 0;
		info.leading := 0;
		info.underlinePos := 0;
		info.underlineThickness := 0;

// WHY DOES THIS HAPPEN NOW? Fake it! (Better than crashing!)
		info.ascent := gCurrentFontSize;
		info.descent := gCurrentFontSize * 0.2;
		info.leading := gCurrentFontSize * 0.2;
		info.underlinePos := gCurrentFontSize * 0.2;
		info.underlineThickness := 1;
		Exit;
	end;
	ATSFontGetHorizontalMetrics(ats_font, kATSOptionFlagsDefault, ats_metrics);
	info.ascent := ats_metrics.ascent * gCurrentFontSize;
	info.descent := -ats_metrics.descent * gCurrentFontSize; // Minus to make descent positive, as in QuickDraw
	info.leading := ats_metrics.leading * gCurrentFontSize;
	info.underlinePos := -ats_metrics.underlinePosition * gCurrentFontSize; // Minus to make positive, not necessarily right but makes it agree with descent
	info.underlineThickness := ats_metrics.underlineThickness * gCurrentFontSize;
	// Added to replace StringWidth(' '):
	// BUT THIS WON'T WORK!
//	info.maxWidth := ats_metrics.maxAdvanceWidth; // * gCurrentFontSize?
//	info.avgWidth := ats_metrics.avgAdvanceWidth; // * gCurrentFontSize
	info.maxWidth := ats_metrics.maxAdvanceWidth * gCurrentFontSize;
	info.avgWidth := ats_metrics.avgAdvanceWidth * gCurrentFontSize; // Broken!

//WriteLn('GetFontInfo 3');

	// Release ats_font?

// 10.5: CGFont/CTFont, should be tested and then used for 10.5+
(*
	cg_font: CGFontRef;
	ct_font: CTFontRef;
	
	cg_font := CGFontCreateWithPlatformFont (ats_font);
	ct_font := CTFontCreateWithGraphicsFont(cg_font, gCurrentFontSize, nil, nil);
	info.ascent := CTFontGetAscent(ct_font);
	info.descent := -CTFontGetDescent(ct_font);
	info.leading := CTFontGetLeading(ct_font);
	info.underlinePos = CTFontGetUnderlinePosition(ct_font);
	info.underlineThickness = CTFontGetUnderlineThickness(ct_font);
	Release?
*)
end;


procedure SetRect(var r: Rect; left, top, right, bottom: Real);overload;
begin
	r.left := left;
	r.top := top;
	r.right := right;
	r.bottom := bottom;
end;

procedure SetRect(var r: Rect; topLeft, botRight: Point); overload;
begin
	r.topLeft := topLeft;
	r.botRight := botRight;
end;

function MakePt(h, v: Real): Point;
begin
	MakePt.h := h;
	MakePt.v := v;
end;
	
function MakeRect(left, top, right, bottom: Real): Rect;
var
	r: Rect;
begin
	SetRect(r, left, top, right, bottom);
	MakeRect := r;
end;

procedure ForeColor(color: SInt32);
type
	BytesQuad = packed array [0..3] of Byte;
var
	colBytes: BytesQuad;
begin
	colBytes := BytesQuad(color);
// Byte order issue here!
	RGBForeColor(colBytes[3]/255, colBytes[2]/255, colBytes[1]/255, colBytes[0]/255);
end;
procedure BackColor(color: SInt32);
type
	BytesQuad = packed array [0..3] of Byte;
var
	colBytes: BytesQuad;
begin
	colBytes := BytesQuad(color);
// Byte order issue here!
	RGBBackColor(colBytes[3]/255, colBytes[2]/255, colBytes[1]/255, colBytes[0]/255);
end;
procedure TextFillColor(color: SInt32);
type
	BytesQuad = packed array [0..3] of Byte;
var
	colBytes: BytesQuad;
begin
	colBytes := BytesQuad(color);
// Byte order issue here!
	RGBTextFillColor(colBytes[3]/255, colBytes[2]/255, colBytes[1]/255, colBytes[0]/255);
end;

procedure ForeColor(color: SInt32; alpha: Real); overload;
type
	BytesQuad = packed array [0..3] of Byte;
var
	colBytes: BytesQuad;
begin
	colBytes := BytesQuad(color);
// Byte order issue here!
	RGBForeColor(colBytes[3]/255, colBytes[2]/255, colBytes[1]/255, alpha);
end;
procedure BackColor(color: SInt32; alpha: Real); overload;
type
	BytesQuad = packed array [0..3] of Byte;
var
	colBytes: BytesQuad;
begin
	colBytes := BytesQuad(color);
// Byte order issue here!
	RGBBackColor(colBytes[3]/255, colBytes[2]/255, colBytes[1]/255, alpha);
end;
procedure TextFillColor(color: SInt32; alpha: Real); overload;
type
	BytesQuad = packed array [0..3] of Byte;
var
	colBytes: BytesQuad;
begin
	colBytes := BytesQuad(color);
// Byte order issue here!
	RGBTextFillColor(colBytes[3]/255, colBytes[2]/255, colBytes[1]/255, alpha);
end;

// Stroke and fill are set to the same. It works well for QD calls,
// For CG commands that use both, variants in QD-style should be added.
procedure RGBForeColor(r,g,b,a: Real); overload;
begin
	CGContextSetRGBFillColor(thePort^.ctx, r, g, b, a);
	CGContextSetRGBStrokeColor(thePort^.ctx, r, g, b, a);
	gForeColorRed := r;
	gForeColorGreen := g;
	gForeColorBlue := b;
	gForeColorAlpha := a;
end;
procedure RGBForeColor(r,g,b: Real); overload;
begin
	CGContextSetRGBFillColor(thePort^.ctx, r, g, b, 1);
	CGContextSetRGBStrokeColor(thePort^.ctx, r, g, b, 1);
	gForeColorRed := r;
	gForeColorGreen := g;
	gForeColorBlue := b;
	gForeColorAlpha := 1;
end;
procedure RGBForeColor(col: RGBColor); overload;
begin
	CGContextSetRGBFillColor(thePort^.ctx, UInt16(col.red)/65535, UInt16(col.green)/65535, UInt16(col.blue)/65535, 1);
	CGContextSetRGBStrokeColor(thePort^.ctx, UInt16(col.red)/65535, UInt16(col.green)/65535, UInt16(col.blue)/65535, 1);
	gForeColorRed := UInt16(col.red)/65535;
	gForeColorGreen := UInt16(col.green)/65535;
	gForeColorBlue := UInt16(col.blue)/65535;
	gForeColorAlpha := 1;
end;

procedure RGBBackColor(r,g,b,a: Real); overload;
begin
	gBackColorRed := r;
	gBackColorGreen := g;
	gBackColorBlue := b;
	gBackColorAlpha := a;
end;
procedure RGBBackColor(r,g,b: Real); overload;
begin
	gBackColorRed := r;
	gBackColorGreen := g;
	gBackColorBlue := b;
	gBackColorAlpha := 1.0;
end;
procedure RGBBackColor(const (*var*) col: RGBColor); overload;
begin
	gBackColorRed := UInt16(col.red)/65535;
	gBackColorGreen := UInt16(col.green)/65535;
	gBackColorBlue := UInt16(col.blue)/65535;
	gBackColorAlpha := 1.0;
end;

procedure RGBTextFillColor(r,g,b,a: Real); overload;
begin
	gTextFillColorRed := r;
	gTextFillColorGreen := g;
	gTextFillColorBlue := b;
	gTextFillColorAlpha := a;
end;
procedure RGBTextFillColor(r,g,b: Real); overload;
begin
	gTextFillColorRed := r;
	gTextFillColorGreen := g;
	gTextFillColorBlue := b;
	gTextFillColorAlpha := 1.0;
end;
procedure RGBTextFillColor(const (*var*) col: RGBColor); overload;
begin
	gTextFillColorRed := UInt16(col.red)/65535;
	gTextFillColorGreen := UInt16(col.green)/65535;
	gTextFillColorBlue := UInt16(col.blue)/65535;
	gTextFillColorAlpha := 1.0;
end;

// NEW 100414: Pattern support

// Pattern globals
var
	hPatternSize, vPatternSize: Real;
	patImage: CGImageRef;

procedure InternalDrawImagePattern (info: Pointer; myContext: CGContextRef); MWPascal;
var
	myRect: CGRect;
begin
	myRect := CGRectMake(0,0, hPatternSize, vPatternSize);

	CGContextSetRGBFillColor (myContext, 1, 1, 1, 1);
	CGContextDrawImage(myContext, myRect, patImage);
end;

procedure QDCGSetPatternToImage(myContext: CGContextRef; image: CGImageRef; offsetx, offsety: Real);
var
	pattern: CGPatternRef; // 1
	patternSpace: CGColorSpaceRef; // 2
	alpha: Float32 = 1;
//	width, height: real; // 4
	s: CGSize;
const
	callbacks: CGPatternCallbacks = (version: 0; drawPattern: @InternalDrawImagePattern; releaseInfo: nil);
begin
	// Can these go with the pattern data? Yes!
	hPatternSize := CGImageGetWidth(image);
	vPatternSize := CGImageGetHeight(image);
	patImage := image;
	
	CGContextSaveGState (myContext);
	patternSpace := CGColorSpaceCreatePattern (nil); // 6
	CGContextSetFillColorSpace (myContext, patternSpace); // 7
	CGColorSpaceRelease (patternSpace); // 8
	
	// Make sure the pattern is stationary
	s.height := thePort^.height - offsety; // gCurrentView.frame.size.height;
	s.width := offsetx; // 0;
	CGContextSetPatternPhase(myContext, s);
	
	pattern := CGPatternCreate (nil,  // 9
					CGRectMake (0, 0, hPatternSize, vPatternSize),
					CGAffineTransformMake (1, 0, 0, 1, 0, 0), // 11
					hPatternSize,  // 12
					vPatternSize,  // 13
					kCGPatternTilingConstantSpacing, // 14
					1,  // 15
					callbacks); // 16
	
	CGContextSetFillPattern (myContext, pattern, @alpha); // 17
	CGPatternRelease (pattern); // 18
end;

// End of pattern support


procedure PaintRect(r: Rect);
var
	cgr: CGRect;
begin
	cgr := RectToCGRect(r);
	CGContextFillRect(thePort^.ctx, cgr);
end;

procedure FrameRect(const (*var*) r: Rect);
var
	cgr: CGRect;
begin
	cgr := RectToCGRect(r);
	if gRecordingRgn then
	begin
		CGPathAddRect(gRgnInRecording, @CGAffineTransformIdentity, cgr);
	end
	else
		CGContextStrokeRect(thePort^.ctx, cgr);
end;

procedure EraseRect(const (*var*) r: Rect);
var
	cgr: CGRect;
begin
	if thePort = nil then
	begin
		WriteLn('EraseRect: No port!');
		Exit;
	end;
	CGContextSaveGState (thePort^.ctx);
	cgr := RectToCGRect(r);
	CGContextSetRGBFillColor (thePort^.ctx, gBackColorRed, gBackColorGreen, gBackColorBlue, gBackColorAlpha);
	CGContextFillRect(thePort^.ctx, cgr);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure InvertRect(const (*var*) r: Rect);
var
	cgr: CGRect;
begin
	cgr := RectToCGRect(r);
	CGContextSaveGState (thePort^.ctx);
	CGContextSetBlendMode(thePort^.ctx, kCGBlendModeDifference);
	CGContextSetRGBFillColor (thePort^.ctx, 1.0, 1.0, 1.0, 1.0);
	CGContextFillRect (thePort^.ctx, cgr);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure FillRect(const (*var*) r: Rect; const (*var*) pat: PicHandle; offsetx, offsety: Real);overload;
var
	cgr: CGRect;
begin
	CGContextSaveGState (thePort^.ctx);
	QDCGSetPatternToImage(thePort^.ctx, pat^.picImage, offsetx, offsety);
	cgr := RectToCGRect(r);
	CGContextFillRect(thePort^.ctx, cgr);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure FillRect(const (*var*) r: Rect; const (*var*) pat: PicHandle);overload;
begin
	FillRect(r, pat, 0, 0);
end;
procedure FrameOval(const (*var*) r: Rect);
var
	cgr: CGRect;
begin
	cgr := RectToCGRect(r);
	if gRecordingRgn then
	begin
		CGPathAddEllipseInRect(gRgnInRecording, @CGAffineTransformIdentity, cgr);
	end
	else
		CGContextStrokeEllipseInRect(thePort^.ctx, cgr);
end;
procedure PaintOval(const (*var*) r: Rect);
var
	cgr: CGRect;
begin
	cgr := RectToCGRect(r);
	CGContextFillEllipseInRect(thePort^.ctx, cgr);
end;
procedure EraseOval(const (*var*) r: Rect);
var
	cgr: CGRect;
begin
	CGContextSaveGState (thePort^.ctx);
	cgr := RectToCGRect(r);
	CGContextSetRGBFillColor (thePort^.ctx, gBackColorRed, gBackColorGreen, gBackColorBlue, gBackColorAlpha);
	CGContextFillEllipseInRect(thePort^.ctx, cgr);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure InvertOval(const (*var*) r: Rect);
var
	cgr: CGRect;
begin
	cgr := RectToCGRect(r);
	CGContextSaveGState (thePort^.ctx);
	CGContextSetBlendMode(thePort^.ctx, kCGBlendModeDifference);
	CGContextSetRGBFillColor (thePort^.ctx, 1.0, 1.0, 1.0, 1.0);
	CGContextFillEllipseInRect (thePort^.ctx, cgr);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure FillOval(const (*var*) r: Rect; const (*var*) pat: PicHandle);
var
	cgr: CGRect;
begin
	CGContextSaveGState (thePort^.ctx);
	QDCGSetPatternToImage(thePort^.ctx, pat^.picImage, 0, 0); // add offsetx, offsety?
	cgr := RectToCGRect(r);
	CGContextFillEllipseInRect(thePort^.ctx, cgr);
	CGContextRestoreGState (thePort^.ctx);
end;


procedure RoundRectPath(r: Rect; ovalWidth, ovalHeight: Real);
var
	cgr: CGRect;
begin
	cgr := RectToCGRect(r);
	
	// Scale to counter scaling transform
	cgr.size.height := cgr.size.height * ovalWidth/ovalHeight;
	cgr.origin.y := cgr.origin.y * ovalWidth/ovalHeight;
	
	// Scale to allow non-circular corners
	CGContextScaleCTM(thePort^.ctx, 1, ovalHeight/ovalWidth);
	
	CGContextBeginPath(thePort^.ctx);
	
	// Lower left inside corner
	CGContextMoveToPoint(thePort^.ctx, cgr.origin.x + ovalWidth, cgr.origin.y);
	// Line to upper right inside corner
// CGContextAddLineToPoint(thePort^.ctx, cgr.origin.x + cgr.size.width - ovalWidth, cgr.origin.y);
// These are not needed.
	// Arc to upper right below corner
	
	CGContextAddArc(thePort^.ctx,
		cgr.origin.x + cgr.size.width - ovalWidth, cgr.origin.y + ovalWidth,
		ovalWidth,
		-Pi/2, 0, 0);

	CGContextAddArc(thePort^.ctx,
		cgr.origin.x + cgr.size.width - ovalWidth, cgr.origin.y + cgr.size.height - ovalWidth,
		ovalWidth, 0, Pi/2 {-?}, 0);

	CGContextAddArc(thePort^.ctx,
		cgr.origin.x + ovalWidth, cgr.origin.y + cgr.size.height - ovalWidth,
		ovalWidth, Pi/2, Pi, 0);

	CGContextAddArc(thePort^.ctx,
		cgr.origin.x + ovalWidth, cgr.origin.y + ovalWidth,
		ovalWidth, Pi, Pi/2, 0);

	// Restore scaling
	CGContextScaleCTM(thePort^.ctx, 1, ovalWidth/ovalHeight);
end;


// Round rects is pretty easy with paths with arcs on corners
procedure FrameRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real);
begin
	CGContextSaveGState (thePort^.ctx);
	RoundRectPath(r, ovalWidth, ovalHeight);
	
	CGContextStrokePath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure PaintRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real);
begin
	CGContextSaveGState (thePort^.ctx);
	RoundRectPath(r, ovalWidth, ovalHeight);

	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure EraseRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real);
begin
	CGContextSaveGState (thePort^.ctx);
	RoundRectPath(r, ovalWidth, ovalHeight);
	CGContextSetRGBFillColor (thePort^.ctx, gBackColorRed, gBackColorGreen, gBackColorBlue, gBackColorAlpha);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure InvertRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real);
begin
	CGContextSaveGState (thePort^.ctx);
	RoundRectPath(r, ovalWidth, ovalHeight);
	CGContextSetBlendMode(thePort^.ctx, kCGBlendModeDifference);
	CGContextSetRGBFillColor (thePort^.ctx, 1.0, 1.0, 1.0, 1.0);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure FillRoundRect(const (*var*) r: Rect; ovalWidth, ovalHeight: Real; const (*var*) pat: PicHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	RoundRectPath(r, ovalWidth, ovalHeight);
	QDCGSetPatternToImage(thePort^.ctx, pat^.picImage, 0, 0); // offsetx, offsety?
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;

// Arcs, using CGContextAddArc
procedure BuildArcPath(r:Rect; startAngle, endAngle: Real; arcOnly: Boolean);
var
	ovalHeight, ovalWidth, centerH, centerV: Real;
begin
	startAngle := startAngle - 90;
	endAngle := endAngle - 90;
	
	ovalHeight := r.bottom - r.top;
	ovalWidth := r.right - r.left;
	centerH := (r.right + r.left)/2;
	centerV := (r.top + r.bottom)/2;
	CGContextScaleCTM(thePort^.ctx, 1, ovalHeight/ovalWidth);
	
	CGContextBeginPath(thePort^.ctx);	
//	CGContextMoveToPoint(thePort^.ctx, centerH + cos(startAngle*Pi/180)*ovalWidth/2, centerV + sin(startAngle*Pi/180)*ovalWidth/2);
	CGContextAddArc(thePort^.ctx,
		centerH, centerV, ovalWidth/2,
		startAngle*Pi/180, endAngle*Pi/180, 0);
	
	if not arcOnly then
	begin
		// Back to starting point
		CGContextAddLineToPoint(thePort^.ctx, centerH, centerV);
		CGContextAddLineToPoint(thePort^.ctx, centerH + cos(startAngle*Pi/180)*ovalWidth/2, centerV + sin(startAngle*Pi/180)*ovalWidth/2);
	end;
	
	CGContextScaleCTM(thePort^.ctx, 1, ovalWidth/ovalHeight);
end;

procedure FrameArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16);
begin
	BuildArcPath(r, startAngle, startAngle + arcAngle, true);
	CGContextStrokePath(thePort^.ctx);
end;
procedure PaintArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16);
begin
	BuildArcPath(r, startAngle, startAngle + arcAngle, false);
//	CGContextStrokePath(thePort^.ctx);
	CGContextFillPath(thePort^.ctx);

//set the fill or stroke color
//CGContextSetRGBFillColor(thePort^.ctx, 0.5, 0.5, 0.5, 1.0);
//CGContextSetRGBStrokeColor(thePort^.ctx, 0.5, 0.5, 0.5, 1.0);

//fill or draw the path
// Use these or CGContextStrokePath/CGContextFillPath?
//CGContextDrawPath(thePort^.ctx, kCGPathStroke);
//CGContextDrawPath(thePort^.ctx, kCGPathFill);

end;
procedure EraseArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16);
begin
	CGContextSaveGState (thePort^.ctx);
	BuildArcPath(r, startAngle, startAngle + arcAngle, false);
	CGContextSetRGBFillColor (thePort^.ctx, gBackColorRed, gBackColorGreen, gBackColorBlue, gBackColorAlpha);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure InvertArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16);
begin
	CGContextSaveGState (thePort^.ctx);
	BuildArcPath(r, startAngle, startAngle + arcAngle, false);
	CGContextSetBlendMode(thePort^.ctx, kCGBlendModeDifference);
	CGContextSetRGBFillColor (thePort^.ctx, 1.0, 1.0, 1.0, 1.0);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure FillArc(const (*var*) r: Rect; startAngle: SInt16; arcAngle: SInt16; const (*var*) pat: PicHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	BuildArcPath(r, startAngle, startAngle + arcAngle, false);
	QDCGSetPatternToImage(thePort^.ctx, pat^.picImage, 0, 0); // offsetx, offsety?
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;


// Regions? Paths again? HIShape?
function NewRgn: RgnHandle;
begin
	NewRgn := New(RgnHandle);
end;
procedure OpenRgn;
begin
	gRgnInRecording := CGPathCreateMutable;
	gRecordingRgn := true;
end;
procedure CloseRgn(dstRgn: RgnHandle);
begin
	CGPathCloseSubpath(gRgnInRecording);
	if dstRgn <> nil then
		dstRgn^.thePath := gRgnInRecording;
	gRecordingRgn := false;
end;

function PtInRgn(pt: Point; rgn: RgnHandle): boolean;
begin
	PtInRgn := 0 <> CGPathContainsPoint(CGPathRef(rgn^.thePath), @CGAffineTransformIdentity, // ERROR?!
                            PointToCGPoint(pt), 0); // 0 or 1 as fill rule?
end;

function PointInRegion(pt: Point; rgn: RgnHandle): boolean;
begin
	PointInRegion := PtInRgn(pt, rgn);
end;

procedure FrameRgn(rgn: RgnHandle);
begin
	if rgn <> nil then
		CGContextAddPath(thePort^.ctx, CGPathRef(rgn^.thePath)); // ERROR?!
	CGContextStrokePath(thePort^.ctx);
end;

procedure PaintRgn(rgn: RgnHandle);
begin
	if rgn <> nil then
		CGContextAddPath(thePort^.ctx, CGPathRef(rgn^.thePath)); // ERROR?!
	CGContextFillPath(thePort^.ctx);
end;

procedure DisposeRgn(rgn: RgnHandle);
begin
	if rgn <> nil then
	begin
		if rgn^.thePath <> nil then
			CGPathRelease(CGPathRef(rgn^.thePath)); // ERROR?!
		Dispose(rgn);
	end;
end;

// Etc

// CopyBits doesn't exist, hard to replace.
// GWorlds? CGBitmapContextCreate, I think.

// Picture, tricky. What kind of metafile can be used? PDF?

// Polygons are easier. Should be pretty easy with paths.

// Much QD vs CG info in http://www.oreilly.com/pub/a/mac/2004/11/02/quartz.html

	
// Fšr linje- och polygonstšd:

procedure PolyAddPoint(h,v: Real);
begin
	if Length(gPolygonInProgress^^.polyPoints) < gPolygonInProgress^^.polySize + 1 then
		SetLength(gPolygonInProgress^^.polyPoints, Length(gPolygonInProgress^^.polyPoints)+16);
		// Must not add one at a time, will cause memory fragmentation
	gPolygonInProgress^^.polyPoints[gPolygonInProgress^^.polySize].h := h;
	gPolygonInProgress^^.polyPoints[gPolygonInProgress^^.polySize].v := v;
	gPolygonInProgress^^.polySize := gPolygonInProgress^^.polySize + 1;
end;

function OpenPoly: PolyHandle;
var
	ph: PolyHandle;
begin
	ph := PolyHandle(NewHandleClear(SizeOf(MacPolygon)));
	gRecordingPolygon := true;
	gPolygonInProgress := ph;
	result := ph;
end;
procedure ClosePoly;
var
	i: Longint;
	
//	debugpol: PolyHandle;
//	debugptr: MacPolygonPtr;
//	debugpolygon: MacPolygon;
begin
//	debugpol := gPolygonInProgress;
//	debugptr := gPolygonInProgress^;
//	debugpolygon := gPolygonInProgress^^;
	gRecordingPolygon := false;
	// BerŠkna polyBBox
	with gPolygonInProgress^^ do
	begin
		polyBBox.left := polyPoints[0].h;
		polyBBox.top := polyPoints[0].v;
		polyBBox.right := polyPoints[0].h;
		polyBBox.bottom := polyPoints[0].v;
		for i := 1 to polySize-1 do
		begin
			if polyBBox.left > polyPoints[i].h then
				polyBBox.left := polyPoints[i].h;
			if polyBBox.top > polyPoints[i].v then
				polyBBox.top := polyPoints[i].v;
			if polyBBox.right < polyPoints[i].h then
				polyBBox.right := polyPoints[i].h;
			if polyBBox.bottom < polyPoints[i].v then
				polyBBox.bottom := polyPoints[i].v;
		end;
	end;
	// Make the length accurate
	SetLength(gPolygonInProgress^^.polyPoints, gPolygonInProgress^^.polySize);
	
	gPolygonInProgress := nil;
end;


function CreatePoly(points: array of Point): PolyHandle;
var
	i: Longint;
begin
	result := PolyHandle(NewHandleClear(SizeOf(MacPolygon)));
	SetLength(result^^.polyPoints, Length(points)); // +16???
	for i := 0 to High(points) do
		result^^.polyPoints[i] := points[i];
//	result^^.polyPoints := points;

	with result^^ do
	begin
		polyBBox.left := polyPoints[0].h;
		polyBBox.top := polyPoints[0].v;
		polyBBox.right := polyPoints[0].h;
		polyBBox.bottom := polyPoints[0].v;
		for i := 1 to Length(polyPoints)-1 do
		begin
			if polyBBox.left > polyPoints[i].h then
				polyBBox.left := polyPoints[i].h;
			if polyBBox.top > polyPoints[i].v then
				polyBBox.top := polyPoints[i].v;
			if polyBBox.right < polyPoints[i].h then
				polyBBox.right := polyPoints[i].h;
			if polyBBox.bottom < polyPoints[i].v then
				polyBBox.bottom := polyPoints[i].v;
		end;
	end;
	result^^.polySize := Length(points);
end;


(*
// NEW: Create polygon directly from an array
function CreatePoly(points: PointArray): PolyHandle;
begin
end;

// NEW: Create curve (polygon with optional smooth corners) from arrays
CreateSmoothPoly?
function CreateCurve(points: PointArray; options: PolyOptionsArray): PolyHandle;
begin
end;
*)

procedure KillPoly(poly: PolyHandle);
begin
	SetLength(poly^^.polyPoints, 0); // Garbage collection does not work with DisposeHandle
	DisposeHandle(Handle(poly));
end;

procedure PolyPath(poly: PolyHandle);
var
	i: Longint;
begin
	if poly^^.polySize < 2 then Exit;
	CGContextBeginPath(thePort^.ctx);
	CGContextMoveToPoint(thePort^.ctx, poly^^.polyPoints[0].h, FlipY(poly^^.polyPoints[0].v));
	for i := 1 to poly^^.polySize - 1 do
	begin
		CGContextAddLineToPoint(thePort^.ctx, poly^^.polyPoints[i].h, FlipY(poly^^.polyPoints[i].v));
	end;
end;

procedure AddPolyToPath(path: CGMutablePathRef; poly: PolyHandle);
var
	i: Longint;
begin
	CGPathMoveToPoint(path, @CGAffineTransformIdentity, poly^^.polyPoints[0].h, FlipY(poly^^.polyPoints[0].v));
	for i := 1 to poly^^.polySize - 1 do
	begin
		CGPathAddLineToPoint(path, @CGAffineTransformIdentity,
			poly^^.polyPoints[i].h, FlipY(poly^^.polyPoints[i].v));
	end;
end;

//procedure OffsetPoly(poly: PolyHandle; dh: SInt16; dv: SInt16);
procedure FramePoly(poly: PolyHandle);//overload;
begin
	if gRecordingRgn then
	begin
		AddPolyToPath(gRgnInRecording, poly);
	end
	else
	begin
		ArrowPoly(poly, 0);
//		PolyPath(poly);
//		CGContextStrokePath(thePort^.ctx);
	end;
end;

procedure PaintPoly(poly: PolyHandle);
begin
	PolyPath(poly);
	CGContextFillPath(thePort^.ctx);
end;

procedure ErasePoly(poly: PolyHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	PolyPath(poly);
	CGContextSetRGBFillColor (thePort^.ctx, gBackColorRed, gBackColorGreen, gBackColorBlue, gBackColorAlpha);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure InvertPoly(poly: PolyHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	PolyPath(poly);
	CGContextSetBlendMode(thePort^.ctx, kCGBlendModeDifference);
	CGContextSetRGBFillColor (thePort^.ctx, 1.0, 1.0, 1.0, 1.0);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure FillPoly(poly: PolyHandle; const (*var*) pat: PicHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	PolyPath(poly);
	QDCGSetPatternToImage(thePort^.ctx, pat^.picImage, 0, 0); // offsetx, offsety
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;

procedure CubicPolyPath(poly: PolyHandle);
var
	i: Longint;
begin
	CGContextBeginPath(thePort^.ctx);
	CGContextMoveToPoint(thePort^.ctx, poly^^.polyPoints[0].h, FlipY(poly^^.polyPoints[0].v));
	for i := 0 to (poly^^.polySize-1) div 3 - 1 do
	begin
		CGContextAddCurveToPoint(thePort^.ctx,
			poly^^.polyPoints[i*3+1].h, FlipY(poly^^.polyPoints[i*3+1].v),
			poly^^.polyPoints[i*3+2].h, FlipY(poly^^.polyPoints[i*3+2].v),
			poly^^.polyPoints[i*3+3].h, FlipY(poly^^.polyPoints[i*3+3].v));
	end;
end;

procedure QuadraticPolyPath(poly: PolyHandle);
var
	i: Longint;
begin
	CGContextBeginPath(thePort^.ctx);
	CGContextMoveToPoint(thePort^.ctx, poly^^.polyPoints[0].h, FlipY(poly^^.polyPoints[0].v));
	for i := 0 to (poly^^.polySize-1) div 2 - 1 do
	begin
		CGContextAddQuadCurveToPoint(thePort^.ctx,
			poly^^.polyPoints[i*2+1].h, FlipY(poly^^.polyPoints[i*2+1].v),
			poly^^.polyPoints[i*2+2].h, FlipY(poly^^.polyPoints[i*2+2].v));
	end;
end;

procedure AddCubicPolyToPath(path: CGMutablePathRef; poly: PolyHandle);
var
	i: Longint;
begin
	CGPathMoveToPoint(gRgnInRecording, @CGAffineTransformIdentity, poly^^.polyPoints[0].h, FlipY(poly^^.polyPoints[0].v));
	for i := 0 to (poly^^.polySize-1) div 3 - 1 do
	begin
		CGPathAddCurveToPoint(path, @CGAffineTransformIdentity,
			poly^^.polyPoints[i*3+1].h, FlipY(poly^^.polyPoints[i*3+1].v),
			poly^^.polyPoints[i*3+2].h, FlipY(poly^^.polyPoints[i*3+2].v),
			poly^^.polyPoints[i*3+3].h, FlipY(poly^^.polyPoints[i*3+3].v));
	end;
end;

procedure FrameCubicPoly(poly: PolyHandle); // overload;
begin
	if gRecordingRgn then
	begin
		AddCubicPolyToPath(gRgnInRecording, poly);
	end
	else
	begin
		ArrowPoly(poly, 2);
//		CubicPolyPath(poly);
//		CGContextStrokePath(thePort^.ctx);
	end;
end;

procedure PaintCubicPoly(poly: PolyHandle);
begin
	CubicPolyPath(poly);
	CGContextFillPath(thePort^.ctx);
end;

procedure EraseCubicPoly(poly: PolyHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	CubicPolyPath(poly);
	CGContextSetRGBFillColor (thePort^.ctx, gBackColorRed, gBackColorGreen, gBackColorBlue, gBackColorAlpha);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure InvertCubicPoly(poly: PolyHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	CubicPolyPath(poly);
	CGContextSetBlendMode(thePort^.ctx, kCGBlendModeDifference);
	CGContextSetRGBFillColor (thePort^.ctx, 1.0, 1.0, 1.0, 1.0);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure FillCubicPoly(poly: PolyHandle; const (*var*) pat: PicHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	CubicPolyPath(poly);
	QDCGSetPatternToImage(thePort^.ctx, pat^.picImage, 0, 0); // offsetx, offsety
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;


// TEST - Quadratic polygons

// UNTESTED AND LOOKS WRONG!
procedure AddQuadraticPolyToPath(path: CGMutablePathRef; poly: PolyHandle);
var
	i: Longint;
begin
	CGPathMoveToPoint(gRgnInRecording, @CGAffineTransformIdentity, poly^^.polyPoints[0].h, FlipY(poly^^.polyPoints[0].v));
	for i := 0 to (poly^^.polySize-1) div 2 - 1 do
	begin
		CGContextAddQuadCurveToPoint(thePort^.ctx,
//		CGContextAddQuadCurveToPoint(path,
			poly^^.polyPoints[i*2+1].h, FlipY(poly^^.polyPoints[i*2+1].v),
			poly^^.polyPoints[i*2+2].h, FlipY(poly^^.polyPoints[i*2+2].v));
	end;
end;

procedure FrameQuadraticPoly(poly: PolyHandle); // overload;
begin
	if gRecordingRgn then
	begin
		AddQuadraticPolyToPath(gRgnInRecording, poly);
	end
	else
	begin
		ArrowPoly(poly, 1);
//		QuadraticPolyPath(poly);
//		CGContextStrokePath(thePort^.ctx);
	end;
end;

procedure PaintQuadraticPoly(poly: PolyHandle);
begin
	QuadraticPolyPath(poly);
	CGContextFillPath(thePort^.ctx);
end;

procedure EraseQuadraticPoly(poly: PolyHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	QuadraticPolyPath(poly);
	CGContextSetRGBFillColor (thePort^.ctx, gBackColorRed, gBackColorGreen, gBackColorBlue, gBackColorAlpha);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure InvertQuadraticPoly(poly: PolyHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	QuadraticPolyPath(poly);
	CGContextSetBlendMode(thePort^.ctx, kCGBlendModeDifference);
	CGContextSetRGBFillColor (thePort^.ctx, 1.0, 1.0, 1.0, 1.0);
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;
procedure FillQuadraticPoly(poly: PolyHandle; const (*var*) pat: PicHandle);
begin
	CGContextSaveGState (thePort^.ctx);
	QuadraticPolyPath(poly);
	QDCGSetPatternToImage(thePort^.ctx, pat^.picImage, 0, 0); // offsetx, offsety
	CGContextFillPath(thePort^.ctx);
	CGContextRestoreGState (thePort^.ctx);
end;


// Lines

procedure GetPen(var pt: Point);
begin
	pt := gCurrentPoint;
end;


// NOTE: Overloading works poorly here, because if I overload,
// calls tend to go to QuickDraw!
// Instead, the versions I indended to overload are called "MoveToPoint"
// and similar.

procedure MoveTo(h,v: Real); // overload;
begin
	if gRecordingPolygon then
	begin
		PolyAddPoint(h,v);
	end;
	if gRecordingRgn then
	begin
		CGPathMoveToPoint(gRgnInRecording, @CGAffineTransformIdentity, h, v);
	end;
	gCurrentPoint.h := h;
	gCurrentPoint.v := v;
end;
procedure MoveToPoint(p: Point);
begin
	MoveTo(p.h, p.v);
end;

procedure Move(dh, dv: Real);
begin
	if gRecordingPolygon then
	begin
		PolyAddPoint(gCurrentPoint.h + dh, gCurrentPoint.v + dv);
	end;
	if gRecordingRgn then
	begin
		CGPathMoveToPoint(gRgnInRecording, @CGAffineTransformIdentity, gCurrentPoint.h + dh, gCurrentPoint.v + dv);
	end;
	gCurrentPoint.h := gCurrentPoint.h + dh;
	gCurrentPoint.v := gCurrentPoint.v + dv;
end;
procedure MoveByPoint(p: Point);
begin
	Move(p.h, p.v);
end;

procedure LineTo(h,v: Real);
begin
	if gRecordingPolygon then
	begin
		PolyAddPoint(h,v);
		gCurrentPoint.h := h;
		gCurrentPoint.v := v;
	end
	else
	if gRecordingRgn then
	begin
		CGPathAddLineToPoint(gRgnInRecording, @CGAffineTransformIdentity, h, FlipY(v));
		gCurrentPoint.h := h;
		gCurrentPoint.v := v;
	end
	else
	begin
//		CGContextBeginPath(thePort^.ctx);
//		CGContextMoveToPoint(thePort^.ctx, gCurrentPoint.h, FlipY(gCurrentPoint.v));
//		CGContextAddLineToPoint(thePort^.ctx, h, FlipY(v));
//		CGContextStrokePath(thePort^.ctx);
//		MoveTo(h, v);
		
		ArrowTo(h,v);
	end;
end;
procedure LineToPoint(p: Point);
begin
	LineTo(p.h, p.v);
end;

procedure Line(dh, dv: Real);
begin
	if gRecordingPolygon then
	begin
		PolyAddPoint(gCurrentPoint.h + dh, gCurrentPoint.v + dv);
		gCurrentPoint.h := gCurrentPoint.h + dh;
		gCurrentPoint.v := gCurrentPoint.v + dv;
	end
	else
	begin
		ArrowTo(gCurrentPoint.h + dh, gCurrentPoint.v + dv);
//		CGContextBeginPath(thePort^.ctx);
//		CGContextMoveToPoint(thePort^.ctx, gCurrentPoint.h, FlipY(gCurrentPoint.v));
//		CGContextAddLineToPoint(thePort^.ctx, gCurrentPoint.h + dh, FlipY(gCurrentPoint.v + dv));
//		CGContextStrokePath(thePort^.ctx);
//		Move(dh, dv);
	end;
end;
procedure LineByPoint(p: Point); // Was Line, overload;
begin
	Line(p.h, p.v);
end;


(*
procedure SetOrigin(h: SInt16; v: SInt16);
procedure PenSize(width: SInt16; height: SInt16);
procedure ForeColor(color: SInt32);
procedure BackColor(color: SInt32);
procedure InsetRect(var r: Rect; dh: SInt16; dv: SInt16);*)
// SectRect, UnionRect, EqualRect, 

// PenMode only supports srcCopy, srcOr, srcBic and (in 10.5 and up) srcXor
procedure PenMode(mode: SInt16);
begin
	mode := mode and 63; // Ignore ditherCopy
	if mode <= 16 then
		mode := mode and 7; // Make all pat*** into src***
//	if mode = gMode then Exit; // Ignore if same

	case mode of
		srcCopy:
			CGContextSetBlendMode(thePort^.ctx, kCGBlendModeNormal);
		srcOr:
			CGContextSetBlendMode(thePort^.ctx, kCGBlendModeMultiply);
		srcXor:
			CGContextSetBlendMode(thePort^.ctx, kCGBlendModeExclusion);
//			CGContextSetBlendMode(thePort^.ctx, kCGBlendModeXOR);		
		srcBic:
			CGContextSetBlendMode(thePort^.ctx, kCGBlendModeScreen);
	end;
end;

procedure OffsetRect(var r: Rect; dh: Real; dv: Real);overload;
begin
	r.left += dh;
	r.right += dh;
	r.top += dv;
	r.bottom += dv;
end;

// Overloading the same function for NSRect and CGRect seems interesting,
// but it is too much work to do for all cases.
// Use RectToNSRect/NSRectToRect instead.
// But it is more useful for Rect *manipulating* calls, so
// some may be supported (SectRect, UnionRect, MapRect, InsetRect...).
procedure OffsetRect(var r: NSRect; dh: Real; dv: Real);overload;
begin
	r.origin.x += dh;
	r.origin.y += dv;
end;

procedure OffsetRect(var r: CGRect; dh: Real; dv: Real);overload;
begin
	r.origin.x += dh;
	r.origin.y += dv;
end;

// Added in 1.4 (mostly copied from the old Win32 QD port):
function EqualRect({const} rect1, rect2: Rect):Boolean;
begin
	EqualRect := (rect1.top = rect2.top) and (rect1.bottom = rect2.bottom) and
					(rect1.left = rect2.left) and (rect1.right = rect2.right);
end; {}
function SectRect({const} rect1, rect2: Rect; var dstRect: Rect):Boolean;overload;
//var
//	r1, r2, d: RECT;
begin
	dstRect.left := Max(rect1.left, rect2.left);
	dstRect.top := Max(rect1.top, rect2.top);
	dstRect.right := Min(rect1.right, rect2.right);
	dstRect.bottom := Min(rect1.bottom, rect2.bottom);
	if (dstRect.left > dstRect.right) or (dstRect.top > dstRect.bottom) then
	begin
		SectRect := false;
		SetRect(dstRect, 0,0,0,0);
	end
	else
		SectRect := true;
	
//	r1 := MacRectToRect(rect1);
//	r2 := MacRectToRect(rect2);
//	d := MacRectToRect(dstRect);
//	SectRect := 0 <> IntersectRect(@d, @r1, @r2);
end; {}
function SectRect({const} rect1, rect2: Rect):Boolean;overload;
var dstRect: Rect;
begin
	SectRect := SectRect(rect1, rect2, dstRect);
end;
procedure UnionRect({const} var rect1, rect2: Rect; var dstRect: Rect);
//var
//	r1, r2, d: RECT;
begin
	dstRect.left := Min(rect1.left, rect2.left);
	dstRect.top := Min(rect1.top, rect2.top);
	dstRect.right := Max(rect1.right, rect2.right);
	dstRect.bottom := Max(rect1.bottom, rect2.bottom);
//	r1 := MacRectToRect(rect1);
//	r2 := MacRectToRect(rect2);
//	d := MacRectToRect(dstRect);
//	UnionRect(@d, @r1, @r2);
end; {}
procedure MapRect(var r: Rect; {const}var srcRect, dstRect: Rect);
begin
	r.top := (r.top - srcRect.top) * (dstRect.bottom - dstRect.top) / (srcRect.bottom-srcRect.top) + dstRect.top;
	r.bottom := (r.bottom - srcRect.top) * (dstRect.bottom - dstRect.top) / (srcRect.bottom-srcRect.top) + dstRect.top;
	r.left := (r.left - srcRect.left) * (dstRect.right - dstRect.left) / (srcRect.right-srcRect.left) + dstRect.left;
	r.right := (r.right - srcRect.left) * (dstRect.right - dstRect.left) / (srcRect.right-srcRect.left) + dstRect.left;
end; {}

procedure InsetRect(var r:Rect; dh,dv: Real);
begin
	r.top := r.top + dv;
	r.bottom:=r.bottom-dv;
	r.left:=r.left+dh;
	r.right:=r.right-dh;
end; {InsetRect}



// FitRectInRect: Useful addition for proportional scaling.

// Rescale a rectangle while keeping proportions
function FitRectInRect(fitThis, inThis: Rect; lrMapping, udMapping: Integer): Rect;
var
	w1, h1, w2, h2, q1, q2: Real;
begin
	w1 := fitThis.right - fitThis.left;
	h1 := fitThis.bottom - fitThis.top;
	q1 := w1 / h1;
	w2 := inThis.right - inThis.left;
	h2 := inThis.bottom - inThis.top;
	q2 := w2 / h2;
	
	// Scale size of rectangle
	if q1 < q2 then // fitThis higher than inThis
	begin
		fitThis := fitThis * (h2 / h1);
	end
	else // fitThis wider than inThis
	begin
		fitThis := fitThis * (w2 / w1);
	end;
	
	// Align with inThis
	case lrMapping of
		kFitLeft:
			OffsetRect(fitThis, inThis.left - fitThis.left, 0);
		kFitCenter:
			OffsetRect(fitThis, (inThis.left+inThis.right)/2 - (fitThis.left+fitThis.right)/2, 0);
		kFitRight:
			OffsetRect(fitThis, inThis.right - fitThis.right, 0);
	end;
	case udMapping of
		kFitUp:
			OffsetRect(fitThis, 0, inThis.top - fitThis.top);
		kFitCenter:
			OffsetRect(fitThis, 0, (inThis.top+inThis.bottom)/2 - (fitThis.top+fitThis.bottom)/2);
		kFitDown:
			OffsetRect(fitThis, 0, inThis.bottom - fitThis.bottom);
	end;
	FitRectInRect := fitThis;
end;

function FitRectInRect(fitThis, inThis: Rect): Rect;
begin
	FitRectInRect := FitRectInRect(fitThis, inThis, kFitCenter, kFitCenter);
end;



procedure PenSize(width, height: Real); overload;
begin
	CGContextSetLineWidth(thePort^.ctx, width);
	gPenSize := width; // needed for arrows
end;

procedure PenSize(width: Real); overload;
begin
	CGContextSetLineWidth(thePort^.ctx, width);
	gPenSize := width; // needed for arrows
end;

// PenCap/PenDash moved to arrow generation


// Utility function that should have been in QD (less important here)

function MakeRGB(r, g, b: UInt16): RGBColor;
var
	c: RGBColor;
begin
	c.red := r;
	c.green := g;
	c.blue := b;
	result := c;
end;

// Shadow support
// Shadows is yet another feature that doesn't work properly in
// flipped views, so the offsetY below must be flipped.

procedure QDCGSetShadow(offsetX, offsetY, blur: Real);overload;
var
	s: CGSize;
begin
	s.width := offsetX;
	s.height := -offsetY;
	CGContextSetShadow(thePort^.ctx, s, blur);
end;

procedure QDCGSetShadow(offsetX, offsetY, blur, r, g, b, a: Real);overload;
var
	s: CGSize;
	c: CGColorRef;
	colorSpace: CGColorSpaceRef;
begin
	colorSpace := CGColorSpaceCreateDeviceRGB();
	c := CGColorCreate(colorspace, @r);
	s.width := offsetX;
	s.height := -offsetY;
	CGContextSetShadowWithColor(thePort^.ctx, s, blur, c);
	CGColorRelease(c);
end;

procedure QDCGBeginTransparencyLayer;
begin
	CGContextBeginTransparencyLayer(thePort^.ctx, nil);
end;

procedure QDCGEndTransparencyLayer;
begin
    CGContextEndTransparencyLayer(thePort^.ctx);
end;

// Gradients

type
	RealArr = array [0..3] of Float32;
	RealArrPtr = ^RealArr;
var
	startColor, endColor: array[0..3] of Float32;

procedure myCalculateShadingValues (info: Pointer;
					inVal: Float32Ptr;
					outPtr: Float32Ptr); MWPascal;
var
	k: size_t;
	outArr: RealArrPtr;
	callback: ShadingProc;
begin
	outArr := RealArrPtr(outPtr);
	if info <> nil then
	begin
		callback := ShadingProc(info);
		callback(inVal^, outArr^[0], outArr^[1], outArr^[2], outArr^[3]);
	end
	else
	begin
		for k := 0 to 3 do
			outArr^[k] := startColor[k] * (1-inVal^) + endColor[k] * inVal^;
	end;
end;

var
	input_value_range: array [0..1] of Float32 = ( 0, 1 );
	output_value_ranges: array [0..7] of Float32 = ( 0, 1, 0, 1, 0, 1, 0, 1 );
	callbacks: CGFunctionCallbacks = (version:0; evaluate: @myCalculateShadingValues; releaseInfo: nil);

procedure InternalRenderGradient (
							p1, p2: Point;
							radius1, radius2,
							r1, g1, b1, a1,
							r2, g2, b2, a2: Real;
							callback: ShadingProc; radial, extend: Boolean);
var
	colorspace: CGColorSpaceRef;
	myShadingFunction: CGFunctionRef;
	shading: CGShadingRef;

	startPoint, endPoint: CGPoint;
	startRadius, endRadius: Real;
//	myTransform: CGAffineTransform;
begin
	CGContextSaveGState (thePort^.ctx); 

	startPoint := CGPointMake(p1.h, p1.v);  // 2
	startRadius := radius1;  // 3
	endPoint := CGPointMake(p2.h, p2.v);  // 4
	endRadius := radius2;  // 5 
	
	startColor[0] := r1;
	startColor[1] := g1;
	startColor[2] := b1;
	startColor[3] := a1;
	endColor[0] := r2;
	endColor[1] := g2;
	endColor[2] := b2;
	endColor[3] := a2;
	
	colorspace := CGColorSpaceCreateDeviceRGB();  // 6
	myShadingFunction := CGFunctionCreate (callback,  // 4
							1,  // 5
							@input_value_range[0], // 6
							1 + CGColorSpaceGetNumberOfComponents (colorspace),  // 7
							@output_value_ranges[0],  // 8
							callbacks); // 9
	
	if radial then
	    shading := CGShadingCreateRadial (colorspace,  // 8
							startPoint, startRadius,
							endPoint, endRadius,
							myShadingFunction,
							Ord(extend), Ord(extend)) // Extension booleans
	else
	    shading := CGShadingCreateAxial (colorspace,  // 8
							startPoint, endPoint,
							myShadingFunction,
							Ord(extend), Ord(extend)); // Extension booleans
	
	CGContextDrawShading (thePort^.ctx, shading); // 13
	CGColorSpaceRelease (colorspace); // 14
	CGShadingRelease (shading);
	CGFunctionRelease (myShadingFunction);
	
	CGContextRestoreGState (thePort^.ctx); 
// 15
end;

procedure LinearGradient(p1, p2: Point; r1, g1, b1, a1, r2, g2, b2, a2: Real; callback: ShadingProc);
begin
	InternalRenderGradient (
							p1, p2,
							0, 0, // No radii
							r1, g1, b1, a1,
							r2, g2, b2, a2,
							callback, false, false);
end;

procedure RadialGradient(p1, p2: Point; radius1, radius2,
						r1, g1, b1, a1, r2, g2, b2, a2: Real;
						callback: ShadingProc);
begin
	InternalRenderGradient (
							p1, p2,
							radius1, radius2,
							r1, g1, b1, a1,
							r2, g2, b2, a2,
							callback, true, false);
end;

procedure LinearGradientRgn(rgn: RgnHandle; p1, p2: Point;
						r1, g1, b1, a1, r2, g2, b2, a2: Real;
						callback: ShadingProc);
begin
	CGContextSaveGState (thePort^.ctx); 
// Clip to region
	if rgn <> nil then
		CGContextAddPath(thePort^.ctx, CGPathRef(rgn^.thePath)); // ERROR?!
	CGContextClip(thePort^.ctx );
// Draw
	InternalRenderGradient (
							p1, p2,
							0, 0, // No radii
							r1, g1, b1, a1,
							r2, g2, b2, a2,
							callback, false, true);
// Restore
	CGContextRestoreGState (thePort^.ctx); 
end;

procedure RadialGradientRgn(rgn: RgnHandle; p1, p2: Point; radius1, radius2,
						r1, g1, b1, a1, r2, g2, b2, a2: Real;
						callback: ShadingProc);
begin
	CGContextSaveGState (thePort^.ctx); 
// Clip to region
	if rgn <> nil then
		CGContextAddPath(thePort^.ctx, CGPathRef(rgn^.thePath)); // ERROR?!
	CGContextClip(thePort^.ctx );
// Draw
	InternalRenderGradient (
							p1, p2,
							radius1, radius2,
							r1, g1, b1, a1,
							r2, g2, b2, a2,
							callback, true, true);
// Restore
	CGContextRestoreGState (thePort^.ctx); 
end;

procedure ClipRect(const (*var*) r: Rect);
begin
	CGContextBeginPath(thePort^.ctx);
	CGContextAddRect(thePort^.ctx, RectToCGRect(r));
	CGContextClip(thePort^.ctx );
end;

// Note that there is no way to get the current clip rgn! That's a CG limitation.
procedure ClipRgn(rgn: RgnHandle);
begin
	CGContextBeginPath(thePort^.ctx);
	if rgn <> nil then
		CGContextAddPath(thePort^.ctx, CGPathRef(rgn^.thePath)); // ERROR?!
	CGContextClip(thePort^.ctx );
end;

// The following simple wrappers are needed for saving and restoring the clip
procedure SaveState;
begin
	CGContextSaveGState (thePort^.ctx);
end;
procedure RestoreState;
begin
	CGContextRestoreGState (thePort^.ctx);
end;

// Simple vector algebra

	procedure Normalize(var p: Point);
	var
		len: Real;
	begin
		len := sqrt(p.h*p.h + p.v*p.v);
		p.h *= 1/len;
		p.v *= 1/len;
	end;

	function VectorAdd(a, b: Point): Point;
	var
		resultat: Point;
	begin
		resultat.h := a.h + b.h;
		resultat.v := a.v + b.v;
		VectorAdd := resultat;
	end;
	
	function VectorSub(a, b: Point): Point;
	var
		resultat: Point;
	begin
		resultat.h := a.h - b.h;
		resultat.v := a.v - b.v;
		VectorSub := resultat;
	end;
	
	function DotProduct(a, b: Point): Real;
	begin
		DotProduct := a.h * b.h + a.v * b.v;
	end;

	function ScalarMult(a: Point; s: Real): Point;
	var
		resultat: Point;
	begin
		resultat.h := a.h * s;
		resultat.v := a.v * s;
		ScalarMult := resultat;
	end;

// Old QD calls
procedure AddPt(src: Point; var dst: Point); 
begin
	dst.h += src.h;
	dst.v += src.v;
end;
procedure SubPt(src: Point; var dst: Point);
begin
// Is this correct?
	dst.h -= src.h;
	dst.v -= src.v;
end;
function PtInRect(pt: Point; const (*var*) r: Rect): Boolean;
begin
	PtInRect := false;
	if pt.h > r.left then
		if pt.h < r.right then
			if pt.v > r.top then
				if pt.v < r.bottom then
					PtInRect := true;
end;
function PtInRect(pt: CGPoint; const (*var*) r: Rect): Boolean;overload;
begin
	PtInRect := false;
	if pt.x > r.left then
		if pt.x < r.right then
			if pt.y > r.top then
				if pt.y < r.bottom then
					PtInRect := true;
end;

// Lines with arrows

// This is inspired by this nice arrow creation package:
//http://www.dncompute.com/blog/2008/07/17/graphicsutil-a-utility-class-for-drawing-arrows.html
// Improvements over the package above:
// Scales by the line width
// Another pair of control points, at the back, for more freedom.
// 7-parameter system:
// length, width, base position, relative length and width position of two control points

type
	ArrowStyleRec = record
		alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real;
		smooth: Boolean;
	end;

var
	gHead, gNock: ArrowStyleRec;
	gArrowStyles: array[0..17] of ArrowStyleRec = (
								// kPlainArrowHead
								(alength: 4; width: 3; basePosition: 0.0;
								cpLength: 0; cpWidth: 1; cp2Length: 0; cp2Width: 1; smooth: false),
								// kPlainArrowNock
								(alength: -4; width: 3; basePosition: 0.0;
								cpLength: 0; cpWidth: 1; cp2Length: 0; cp2Width: 1; smooth: false),
								// kSlantedArrowHead
								(alength: 4; width: 3; basePosition: 0.2;
								cpLength: 0; cpWidth: 1; cp2Length: -0.2; cp2Width: 1; smooth: true),
								// kSlantedArrowNock
								(alength: -4; width: 3; basePosition: 0.2;
								cpLength: 0; cpWidth: 1; cp2Length: -0.2; cp2Width: 1; smooth: true),
								// kRoundArrowHead
								(alength: 4; width: 3; basePosition: 0.2;
								cpLength: 1; cpWidth: 1; cp2Length: -0.2; cp2Width: 0.6; smooth: true),
								// kRoundArrowNock
								(alength: -4; width: 3; basePosition: 0.2;
								cpLength: 1; cpWidth: 1; cp2Length: -0.2; cp2Width: 0.6; smooth: true),
								// kTightArrowHead
								(alength: 2; width: 1; basePosition: 0;
								cpLength: 0; cpWidth: 1; cp2Length: -0.2; cp2Width: 0; smooth: true),
								// kTightArrowNock
								(alength: -2; width: 1; basePosition: 0.0;
								cpLength: 0; cpWidth: 1; cp2Length: -0.4; cp2Width: 1; smooth: false),
								// kHeartArrowHead
								(alength: 3; width: 3; basePosition: 0;
								cpLength: 0.3; cpWidth: 1; cp2Length: -0.4; cp2Width: 1; smooth: true),
								// kHeartArrowNock
								(alength: -3; width: 3; basePosition: 0.0;
								cpLength: 0.3; cpWidth: 1; cp2Length: -0.4; cp2Width: 1; smooth: true),
								// kPolyArrowHead
								(alength: 3; width: 3; basePosition: 0;
								cpLength: 0.3; cpWidth: 1; cp2Length: -0.4; cp2Width: 1; smooth: false),
								// kPolyArrowNock
								(alength: -3; width: 3; basePosition: 0.0;
								cpLength: 0.3; cpWidth: 1; cp2Length: -0.4; cp2Width: 1; smooth: false),
								// kDropArrowHead
								(alength: 4; width: 3; basePosition: -0.3;
								cpLength: 0.4; cpWidth: 1.2; cp2Length: -0.3; cp2Width: 0.7; smooth: true),
								// kDropArrowNock
								(alength: -4; width: 3; basePosition: -0.3;
								cpLength: 0.4; cpWidth: 1.2; cp2Length: -0.3; cp2Width: 0.7; smooth: true),
								// kSpearArrowHead
								(alength: 4.5; width: 2.6; basePosition: 0.2;
								cpLength: 0.4; cpWidth: 0.3; cp2Length: -0.3; cp2Width: 1.5; smooth: true),
								// kSpearArrowNock
								(alength: -7; width: 2.6; basePosition: 0.2;
								cpLength: 0.4; cpWidth: 0.3; cp2Length: -0.3; cp2Width: 1.5; smooth: true),
								// kSpear2ArrowHead
								(alength: 4.5; width: 3; basePosition: -0.3;
								cpLength: 0; cpWidth: 0.5; cp2Length: 0.2; cp2Width: 1.2; smooth: true),
								// kCrossbowArrowHead
								(alength: 3.5; width: 5.4; basePosition: 1;
								cpLength: 1; cpWidth: 1.2; cp2Length: 0.3; cp2Width: 1.6; smooth: true)
								);

// Pretty general 7-parameter arrow
function DrawArrow(base, direction: Point; theLength, theWidth, basePosition, cpLength, cpWidth,
					cp2Length, cp2Width: Real; smooth: Boolean): Point;
var
	side{, corner, edge}: Point;
	tri: array of Point;
	ph: PolyHandle;
	sidev, lengthVector, baseVector, cpSide, cpLen, cpSideRev, cp2Len, cp2Side: Point;
	baseD{, baseL}: Point; // Base to draw, base to line
	
	offset: Real;
	lineWidth: Real;
begin
	lineWidth := gPenSize;

	Normalize(direction);
	side.h := -direction.v;
	side.v := direction.h;

// Move arrow and line end how much?
	result := base;
	offset := 0;
	
	// Move the whole head so the tip is at the line end
	if theLength > 0 then
	begin
		offset := theLength * (1 - basePosition) * lineWidth;
		baseD := VectorAdd(base, ScalarMult(direction, -offset));
	end
	else
	begin
		offset := theLength * (-basePosition) * lineWidth; // Make the wings match the end
		if offset < 0 then offset := 0; // Makes the base match the end
		baseD := VectorAdd(base, ScalarMult(direction, -offset));
	end;
	result := baseD;
	
	// Adjust the line length
	if theLength > 0 then
	begin
		if basePosition < 0 then
		// Move forward to the wings, by -basePosition
			result := VectorAdd(baseD, ScalarMult(direction, -theLength*basePosition*lineWidth))
		else
			if theWidth > 1.01 then
		// Minimal step forward to avoid crack
				result := VectorAdd(baseD, ScalarMult(direction, 2)); // 2 pixels
	end
	else
	begin
		if basePosition < 0 then // Move back to the wings
			result := VectorAdd(baseD, ScalarMult(direction, basePosition*lineWidth));		
	end;
	
	sidev := ScalarMult(side, theWidth/2 * lineWidth); // 1 = as wide as the line
	lengthVector := ScalarMult(direction, theLength * lineWidth); // normalt positiv, negativ fšr "pilstjŠrt"
	baseVector := ScalarMult(direction, theLength * lineWidth * basePosition);
	
	cpSide := ScalarMult(sidev, cpWidth); // Fel
	cpSideRev := VectorSub(sidev, cpSide); // Fel
	cpLen := ScalarMult(lengthVector, cpLength);

	cp2Side := ScalarMult(sidev, cp2Width);
	cp2Len := ScalarMult(lengthVector, cp2Length);

	SetLength(tri, 9);
	tri[0] := baseD;
	tri[1] := VectorAdd(baseD, VectorAdd(cp2Len, cp2Side));
	tri[2] := VectorAdd(baseD, VectorSub(sidev, baseVector)); // Side corner
	tri[3] := VectorAdd(tri[2], VectorSub(cpLen, cpSideRev)); // Control point
	tri[4] := VectorAdd(baseD, VectorSub(lengthVector, baseVector)); // Tip
	tri[6] := VectorSub(baseD, VectorAdd(sidev, baseVector)); // Side corner
	tri[5] := VectorAdd(tri[6], VectorAdd(cpLen, cpSideRev)); // Control point
	tri[7] := VectorAdd(baseD, VectorSub(cp2Len, cp2Side));
	tri[8] := baseD;

// This is restored by the caller, since all callers save and restore state.
	CGContextSetRGBFillColor(thePort^.ctx, gForeColorRed, gForeColorGreen, gForeColorBlue, gForeColorAlpha);

	ph := CreatePoly(tri);
	if smooth then
		PaintQuadraticPoly(ph)
	else
		PaintPoly(ph);
	KillPoly(ph);

// Possible improvement:
// Make the arrow fit the corners of the line instead of fitting to the end point
end;

// OBSOLETE - replaced by PenCap
procedure ArrowNockStyle(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
begin
// Set globals
	gNock.alength := alength;
	gNock.width := width;
	gNock.basePosition := basePosition;
	gNock.cpLength := cpLength;
	gNock.cpWidth := cpWidth;
	gNock.cp2Length := cp2Length;
	gNock.cp2Width := cp2Width;
	gNock.smooth := smooth;
end;
procedure ArrowHeadStyle(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
begin
// Set globals
	gHead.alength := alength;
	gHead.width := width;
	gHead.basePosition := basePosition;
	gHead.cpLength := cpLength;
	gHead.cpWidth := cpWidth;
	gHead.cp2Length := cp2Length;
	gHead.cp2Width := cp2Width;
	gHead.smooth := smooth;
end;
procedure ArrowStyle(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
begin
// Set globals for both
	ArrowNockStyle(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width, smooth);
	ArrowHeadStyle(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width, smooth);
end;

// Obsolete, replaced by PenCap
procedure ArrowNockStyle(theStyle: Integer);overload;
begin
	if theStyle >= Low(gArrowStyles) then
	if theStyle <= High(gArrowStyles) then
	begin
		gNock := gArrowStyles[theStyle];
	end;
end;
procedure ArrowHeadStyle(theStyle: Integer);overload;
begin
	if theStyle >= Low(gArrowStyles) then
	if theStyle <= High(gArrowStyles) then
	begin
		gHead := gArrowStyles[theStyle];
	end;
end;
procedure ArrowStyle(theStyle: Integer);overload;
begin
	ArrowNockStyle(theStyle);
	ArrowHeadStyle(theStyle);
end;

procedure ArrowTo(h,v: Real);
var
	gCurrentPoint: Point; // TEMP
	startP, endP: Point;
begin
//	SaveState; // Save old line cap
//	PenCap(kCGLineCapButt); // Set to Butt
//	CGContextSetLineCap(thePort^.ctx, kCGLineCapButt);

	GetPen(gCurrentPoint); // TEMP
	// save gCurrentPoint?
	startP := gCurrentPoint;
	endP := MakePt(h, v);
	
	if gArrowAtStart then
		startP := DrawArrow(gCurrentPoint, VectorSub(gCurrentPoint, MakePt(h,v)),
							gNock.alength, gNock.width, gNock.basePosition, gNock.cpLength,
							gNock.cpWidth, gNock.cp2Length, gNock.cp2Width, gNock.smooth);
	if gArrowAtEnd then
		endP := DrawArrow(MakePt(h,v), VectorSub(MakePt(h,v), gCurrentPoint),
							gHead.alength, gHead.width, gHead.basePosition, gHead.cpLength,
							gHead.cpWidth, gHead.cp2Length, gHead.cp2Width, gHead.smooth);
	
	MoveTo(startP.h, startP.v);
	
//	LineTo(endP.h, endP.v);
	CGContextBeginPath(thePort^.ctx);
	CGContextMoveToPoint(thePort^.ctx, startP.h, FlipY(startP.v));
	CGContextAddLineToPoint(thePort^.ctx, endP.h, FlipY(endP.v));
	CGContextStrokePath(thePort^.ctx);
	MoveTo(h, v);

	MoveTo(h, v);
//	RestoreState; // Restore line cap
end;


// NOTE: The three polygon arrow drawing functions are easily merged to one with a simple linear/quadratic/cubic switch

procedure ArrowPoly(poly: PolyHandle; mode: Integer); {0 = linear, 1 = quadratic, 2 = cubic}
var
//	gCurrentPoint: Point; // TEMP
	startP, endP, savedStart, savedEnd: Point;
begin
	SaveState; // Save old line cap
//	CGContextSetLineCap(thePort^.ctx, kCGLineCapButt);
//	PenCap(kCGLineCapButt); // Set to Butt

	savedStart := poly^^.polyPoints[0];
	savedEnd := poly^^.polyPoints[poly^^.polySize-1];

	if gArrowAtStart then
		startP := DrawArrow(poly^^.polyPoints[0], VectorSub(poly^^.polyPoints[0], poly^^.polyPoints[1]),
							gNock.alength, gNock.width, gNock.basePosition, gNock.cpLength,
							gNock.cpWidth, gNock.cp2Length, gNock.cp2Width, gNock.smooth)
	else
		startP := savedStart;
	if gArrowAtEnd then
		endP := DrawArrow(poly^^.polyPoints[poly^^.polySize-1], VectorSub(poly^^.polyPoints[poly^^.polySize-1], poly^^.polyPoints[poly^^.polySize-2]),
							gHead.alength, gHead.width, gHead.basePosition, gHead.cpLength,
							gHead.cpWidth, gHead.cp2Length, gHead.cp2Width, gHead.smooth)
	else
		endP := savedEnd;
	
	poly^^.polyPoints[0] := startP;
	poly^^.polyPoints[poly^^.polySize-1] := endP;
	case mode of
		0: PolyPath(poly);
		1: QuadraticPolyPath(poly);
		2: CubicPolyPath(poly);
	end;
		CGContextStrokePath(thePort^.ctx);
//	FrameCubicPoly(poly);
	poly^^.polyPoints[0] := savedStart;
	poly^^.polyPoints[poly^^.polySize-1] := savedEnd;
//	FrameCubicPoly(poly);
	RestoreState; // Restore line cap
end;

//const
//	kCGLineCapButt = 0;
//	kCGLineCapRound = 1;
//	kCGLineCapSquare = 2;

// PenCap:
// Tre inbyggda i CG
// Alla QDCG-pilar kombinerade med kCGLineCapButt
// Alla QDCG-pilar kombinerade med kCGLineCapRound (fšr PenDash)
// Eller Šr detta en switch i PenDash?
// Skippa kCGLineCapSquare - den Šr šverflšdig! Eller?
// Den Šr i alla fall šverflšdig i PenDash!

// SŒ hŠr dŒ:
// PenCap innehŒller Butt och Round samt en samling pilar.
// PenDash innehŒller en flagg fšr Butt eller Round.
// Square skippas helt? Tveksamt! Kan den gšras som arrow,
// genom att CPLength Šr mer Šn 1? Nix. Men om man har ett
// lŠngdoffset fšr att gšra Square sŒvŠl som Round runt Šndpunkten
// snarare Šn till den sŒ gŒr det. DET FR V€NTA.
// En konflikt kvar: rundad linje med kantig line cap. Kan lšsas
// med arrow! Men skall man alltid gšra det? Detektera detta fall?

// En konflikt till: Skall Round sŠttas av bŒde Cap och Dash?
// Det kan bli konstigt! Om vi i stŠllet spolar alla CG-varianter
// och gšr dem sjŠlva sŒ blir det bŠttre. Round dash get round
// cap. Men dŒ skall jag inte pilla pŒ saker i arrow-ritningen...
// Detta Šr knepigt!

// Om vi gšr sŒ hŠr dŒ:
// BARA Butt frŒn PenCap
// SŠtt en flagga om round dash, sŠtter kCGLineCapRound vid kurvritningen.
// Men ger inte detta problem vid Frame? Det hŒller inte! Men om vi sŠtter
// round vid PenDash dŒ sŒ den hŠnger med?

procedure PenCapHead(cap: Longint{CGLineCap});
begin
	gArrowAtEnd := false;
	if cap >= Low(gArrowStyles) then
	if cap <= High(gArrowStyles) then
	begin
		gHead := gArrowStyles[cap];
		gArrowAtEnd := true;
	end;
end;

procedure PenCapNock(cap: Longint{CGLineCap});
begin
	gArrowAtStart := false;
	if cap >= Low(gArrowStyles) then
	if cap <= High(gArrowStyles) then
	begin
		gNock := gArrowStyles[cap];
		gArrowAtStart := true;
	end;
	
//	CGContextSetLineCap(thePort^.ctx, kCGLineCapButt);
//	CGContextSetLineCap(thePort^.ctx, cap);
//	kCGLineCapButt, kCGLineCapRound, kCGLineCapSquare
end;

procedure PenCap(cap: Longint{CGLineCap});
begin
	PenCapHead(cap);
	PenCapNock(cap);

//	CGContextSetLineCap(thePort^.ctx, kCGLineCapButt);
//	CGContextSetLineCap(thePort^.ctx, cap);
//	kCGLineCapButt, kCGLineCapRound, kCGLineCapSquare
end;

procedure PenCapHead(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
begin
	gArrowAtEnd := true;
// Set globals
	gHead.alength := alength;
	gHead.width := width;
	gHead.basePosition := basePosition;
	gHead.cpLength := cpLength;
	gHead.cpWidth := cpWidth;
	gHead.cp2Length := cp2Length;
	gHead.cp2Width := cp2Width;
	gHead.smooth := smooth;
end;

procedure PenCapNock(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
begin
	gArrowAtStart := true;
// Set globals
	gNock.alength := alength;
	gNock.width := width;
	gNock.basePosition := basePosition;
	gNock.cpLength := cpLength;
	gNock.cpWidth := cpWidth;
	gNock.cp2Length := cp2Length;
	gNock.cp2Width := cp2Width;
	gNock.smooth := smooth;
end;

procedure PenCap(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width: Real; smooth: Boolean);overload;
begin
// Set globals for both
	PenCapHead(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width, smooth);
	PenCapNock(alength, width, basePosition, cpLength, cpWidth, cp2Length, cp2Width, smooth);
end;

procedure PenDash(phase, len1, len2: Float32; round: Boolean); overload;
var
	lp: array of Float32;
begin
	SetLength(lp, 2);
	lp[0] := len1;
	lp[1] := len2;
	CGContextSetLineDash(thePort^.ctx, phase, @lp[0], 2);
	
	if round then
		CGContextSetLineCap(thePort^.ctx, kCGLineCapRound)
	else
		CGContextSetLineCap(thePort^.ctx, kCGLineCapButt);
end;
procedure PenDash(phase, len1, len2, len3, len4: Float32; round: Boolean); overload;
var
	lp: array of Float32;
begin
	SetLength(lp, 4);
	lp[0] := len1;
	lp[1] := len2;
	lp[2] := len3;
	lp[3] := len4;
	CGContextSetLineDash(thePort^.ctx, phase, @lp[0], 4);

	if round then
		CGContextSetLineCap(thePort^.ctx, kCGLineCapRound)
	else
		CGContextSetLineCap(thePort^.ctx, kCGLineCapButt);
end;
procedure PenDash(phase: Float32; lenList: array of Float32; round: Boolean); overload;
begin
	CGContextSetLineDash(thePort^.ctx, phase, @lenList[0], Length(lenList));

	if round then
		CGContextSetLineCap(thePort^.ctx, kCGLineCapRound)
	else
		CGContextSetLineCap(thePort^.ctx, kCGLineCapButt);
end;
procedure PenDash(phase: Float32; lenList: Float32Ptr; lenLength: Longint; round: Boolean); overload;
begin
	CGContextSetLineDash(thePort^.ctx, phase, lenList, lenLength);

	if round then
		CGContextSetLineCap(thePort^.ctx, kCGLineCapRound)
	else
		CGContextSetLineCap(thePort^.ctx, kCGLineCapButt);
end;


// Operator overloading for simple vector operations on Point
// and 3x3 matrix operations
// Matrices are defined row-wise! (I.e. index 0, 1, 2 are the first row.)

operator + (a, b: Point) c: Point;
begin
	c.h := a.h + b.h;
	c.v := a.v + b.v;
end;

operator - (a, b: Point) c: Point;
begin
	c.h := a.h - b.h;
	c.v := a.v - b.v;
end;

operator * (a, b: Point) c: Real; // Dot product
begin
	c := a.h * b.h + a.v * b.v;
end;

operator * (a: Real; b: Point) c: Point;
begin
	c.h := a * b.h;
	c.v := a * b.v;
end;

operator * (a: Point; b: Real) c: Point;
begin
	c.h := a.h * b;
	c.v := a.v * b;
end;

operator / (a: Point; b: Real) c: Point;
begin
	c.h := a.h / b;
	c.v := a.v / b;
end;

operator / (a, b: Point) c: Real; // Cross product in 2D produces a Real
begin
	c := a.h*b.v - a.v*b.h;
end;

operator + (a: Point; b: Rect) c: Rect;
begin
	c.top := b.top + a.v;
	c.bottom := b.bottom + a.v;
	c.left := b.left + a.h;
	c.right := b.right + a.h;
end;

operator + (b: Rect; a: Point) c: Rect;
begin
	c.top := b.top + a.v;
	c.bottom := b.bottom + a.v;
	c.left := b.left + a.h;
	c.right := b.right + a.h;
end;

operator * (a: Real; b: Rect) c: Rect;
begin
	c.top := b.top * a;
	c.bottom := b.bottom * a;
	c.left := b.left * a;
	c.right := b.right * a;
end;

operator * (b: Rect; a: Real) c: Rect;
begin
	c.top := b.top * a;
	c.bottom := b.bottom * a;
	c.left := b.left * a;
	c.right := b.right * a;
end;

//procedure Normalize(var a: Point);
//begin
//	a := a / sqrt(a * a);
//end;

// Rect / Rect = scale, fit into?


// Add matrix mult? 3x3 matrix with homogenous coordinates.

function MakeMatrix(a,b,c,d,e,f,g,h,i: Real): mat3;
var
	m: mat3;
begin
	m[0] := a;
	m[1] := b;
	m[2] := c;
	m[3] := d;
	m[4] := e;
	m[5] := f;
	m[6] := g;
	m[7] := h;
	m[8] := i;
	MakeMatrix := m;
end;

function MultMat3(a, b: mat3): mat3;
var
	m: mat3;
	x, y: Longint;
begin
	for x := 0 to 2 do
		for y := 0 to 2 do
			m[x + y*3] :=	a[y*3+0] * b[0*3+x] +
									a[y*3+1] * b[1*3+x] +
									a[y*3+2] * b[2*3+x];
//	for x := 0 to 2 do
//		for y := 0 to 2 do
//			m[x*3 + y] :=	a[y+3*0] * b[0+3*x] +
//									a[y+3*1] * b[1+3*x] +
//									a[y+3*2] * b[2+3*x];
	MultMat3 := m;
end;

{HELT OTESTAD!}
function MultMat3ByVector (a: mat3; b: Point): Point;
var
	p: Point;
	h: Real;
begin
// Row-wise
	p.h := a[0] * b.h + a[1] * b.v + a[2];
	p.v := a[3] * b.h + a[4] * b.v + a[5];
	h := a[6] + a[7] + a[8];
	p.h := p.h / h;
	p.v := p.v / h;
	MultMat3ByVector := p;
end;

// T, R, invert...
	function IdentityMatrix: mat3;
	var
		m: mat3;
		i: Longint;
	begin
		for i := 0 to 8 do
			m[i] := 0;
		for i := 0 to 2 do
			m[i * 4] := 1; {0,4,8}
		IdentityMatrix := m;
	end;
	
	function RotationMatrix(a: GLfloat): mat3;
	var
		m: mat3;
	begin
		m := IdentityMatrix;
		m[0] := cos(a);
		m[1] := -sin(a);
		m[3] := sin(a);
		m[4] := cos(a);
		RotationMatrix := m;
	end;
	
	function TranslationMatrix(a, b: GLfloat): mat3;
	var
		m: mat3;
	begin
		m := IdentityMatrix;
		m[2] := a;
		m[5] := b;
		TranslationMatrix := m;
	end;
	
	function ScaleMatrix(sx, sy: GLfloat): mat3;
	var
		m: mat3;
	begin
		m := IdentityMatrix;
		m[0] := sx;
		m[4] := sy;
		ScaleMatrix := m;
	end;
	


operator * (a, b: mat3) c: mat3;
begin
	c := MultMat3(a, b);
end;

operator * (a: mat3; b: Point) c: Point;
begin
	c := MultMat3ByVector (a, b);
end;


end.


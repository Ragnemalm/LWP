// Split view. Needs DragView for dragging the spitter view. (Could be replaced by a SkelView.)
// 2013-03-25
// 130418: Added "ratio" parameter to avoid unwanted rescaling
// 150505: Rewrote for QDCG and thereby Retina friendly.

// Annan bra embedding: Split view. Tre views i en stor toppvy, två
// "contents" och en mellan dem, dragbar.

// Borde vara lätt att fixa - om man inte tappar musevents om muspekaren lämnar view.

unit Splitter;
interface
uses
	MacOSAll, TransSkel4, CarbonUtils, {SkelView,} DragView, QDCG;

type
	SplitData = record
		masterView, upper, lower, splitter: ControlRef;
		verticalSplit: Boolean;
		ratio: Real;
	end;
	SplitDataPtr = ^SplitData;

procedure ResizeSplitter(theSplitData: SplitDataPtr; newSize: Rect);
function EmbedInSplitter(parent: ControlRef; var masterView: ControlRef; isVertical: Boolean; ratio: Real): SplitDataPtr;

implementation

const
	kSplitHeight = 10;
	kMinimumSize = 0;

// The draw proc could need the user data
procedure DrawSplitter(theView: HIViewRef; cgContext: CGContextRef; viewRect: CGRect; userData: Pointer);
var
	r: Rect;
	col: RGBColor;
begin
	CreatePort(cgContext, viewRect.size.height);

	SetRect(r, 0,0,Trunc(viewRect.size.width),Trunc(viewRect.size.height));
//	ForeColor(yellowColor);
	col.red := $e000;	col.green := $e000;	col.blue := $e400;
	RGBForeColor(col);
	PaintRect(r);

	if viewRect.size.width < viewRect.size.height then
	begin
		col.red := $c000;	col.green := $c000;	col.blue := $c000;
		RGBForeColor(col);
		MoveTo((r.left + r.right)/ 2-1, r.top + 10);
		LineTo((r.left + r.right)/ 2-1, r.bottom - 10);
		col.red := $8000;	col.green := $8000;	col.blue := $8000;
		RGBForeColor(col);
		MoveTo((r.left + r.right)/ 2+1, r.top + 10);
		LineTo((r.left + r.right)/ 2+1, r.bottom - 10);
	end
	else
	begin
		col.red := $c000;	col.green := $c000;	col.blue := $c000;
		RGBForeColor(col);
		MoveTo(r.left + 10, (r.top + r.bottom)/ 2-1);
		LineTo(r.right - 10, (r.top + r.bottom)/ 2-1);
		col.red := $8000;	col.green := $8000;	col.blue := $8000;
		RGBForeColor(col);
		MoveTo(r.left + 10, (r.top + r.bottom)/ 2+1);
		LineTo(r.right - 10, (r.top + r.bottom)/ 2+1);
	end;
	
	FinishPort;
end;

// Resize all views proportionally
procedure ResizeSplitter(theSplitData: SplitDataPtr; newSize: Rect);
var
	oldSize, oldUpper, oldLower: MacOSAll.Rect;
//	dUpper, dLower: Real;
	qUpper, qLower: Real;
	h, hu{, hl}: Real;
	upperRect, splitterRect, lowerRect: Rect;
begin
	GetControlBounds(theSplitData^.masterView, oldSize);
	GetControlBounds(theSplitData^.upper, oldUpper);
	GetControlBounds(theSplitData^.lower, oldLower);
	
	SetControlBounds(theSplitData^.masterView, RectToMacRect(newSize));

// Change to local 
	OffsetRect(newSize, -newSize.left, -newSize.top);

	qUpper := theSplitData^.ratio;
	qLower := 1.0 - theSplitData^.ratio;

	if theSplitData^.verticalSplit then
	begin
//		qUpper := (oldUpper.right - oldUpper.left) / (oldUpper.right - oldUpper.left + oldLower.right - oldLower.left);
//		qLower := (oldLower.right - oldLower.left) / (oldUpper.right - oldUpper.left + oldLower.right - oldLower.left);

		h := newSize.right-newSize.left;
		hu := Trunc((h - kSplitHeight) * qUpper);
//		hl := Trunc(h - (h - kSplitHeight) * qLower);

		SetRect(upperRect, newSize.left, newSize.top, newSize.left + hu, newSize.bottom);
		SetRect(splitterRect, newSize.left + hu, newSize.top, newSize.left + hu + kSplitHeight, newSize.bottom);
		SetRect(lowerRect, newSize.left + hu + kSplitHeight, newSize.top, newSize.right, newSize.bottom);
	end
	else
	begin
//		qUpper := (oldUpper.bottom - oldUpper.top) / (oldUpper.bottom - oldUpper.top + oldLower.bottom - oldLower.top);
//		qLower := (oldLower.bottom - oldLower.top) / (oldUpper.bottom - oldUpper.top + oldLower.bottom - oldLower.top);

		h := newSize.bottom-newSize.top;
		hu := Trunc((h - kSplitHeight) * qUpper);
//		hl := Trunc(h - (h - kSplitHeight) * qLower);

		SetRect(upperRect, newSize.left, newSize.top, newSize.right, newSize.top + hu);
		SetRect(splitterRect, newSize.left, newSize.top + hu, newSize.right, newSize.top + hu + kSplitHeight);
		SetRect(lowerRect, newSize.left, newSize.top + hu + kSplitHeight, newSize.right, newSize.bottom);
	end;
	
	SetControlBounds(theSplitData^.upper, RectToMacRect(upperRect));
	SetControlBounds(theSplitData^.splitter, RectToMacRect(splitterRect));
	SetControlBounds(theSplitData^.lower, RectToMacRect(lowerRect));
end; // ResizeSplitter

function MasterViewEventHandlerProc(nextHandler: EventHandlerCallRef; inEvent: EventRef; inUserData: Pointer ):OSStatus; MWPascal;
var
	myViewDataPtr: SplitDataPtr;
	
	eventClass: UInt32;
	eventKind: UInt32;
	r: MacOSAll.Rect;
begin
	eventClass := GetEventClass(inEvent);
	eventKind  := GetEventKind(inEvent);
	myViewDataPtr := SplitDataPtr(inUserData);
	
	// Standard bye-bye
	if eventClass = kEventClassControl then
		if eventKind = kEventControlDispose then
		begin
			DisposePtr(inUserData);
		end;
	// Resize
	if eventClass = kEventClassControl then
		if eventKind = kEventControlBoundsChanged then
		begin
			// Get bounds
			GetControlBounds(myViewDataPtr^.masterView, r);
			// Call resize
			ResizeSplitter(myViewDataPtr, MacRectToRect(r));
		end;	
	MasterViewEventHandlerProc := noErr;
end;

procedure DragSplitter(theView: HIViewRef; diff: MacOSAll.Point; userData: Pointer);
var
	theSplitData: SplitDataPtr;
	upperRect, lowerRect, splitterRect, totalRect: MacOSAll.Rect;
begin
//	WriteLn('DragSplitter', diff.h, ',', diff.v);
	theSplitData := SplitDataPtr(userData);
	GetControlBounds(theSplitData^.upper, upperRect);
	GetControlBounds(theSplitData^.lower, lowerRect);
	GetControlBounds(theSplitData^.masterView, totalRect);

// Change to local 
	OffsetRect(totalRect, -totalRect.left, -totalRect.top);

	if theSplitData^.verticalSplit then
	begin
		upperRect.right += Trunc(diff.h);
		lowerRect.left := upperRect.right + kSplitHeight;
		if upperRect.right < totalRect.right - kSplitHeight - kMinimumSize then
		if lowerRect.left > totalRect.left + kSplitHeight + kMinimumSize then
		begin
			SetRect(splitterRect, upperRect.right, upperRect.top, lowerRect.left, upperRect.bottom);
			SetControlBounds(theSplitData^.upper, upperRect);
			SetControlBounds(theSplitData^.splitter, splitterRect);
			SetControlBounds(theSplitData^.lower, lowerRect);
		end;
		if totalRect.right - totalRect.left > 0 then
			theSplitData^.ratio := (upperRect.right - upperRect.left) / (totalRect.right - totalRect.left);
	end
	else
		begin
		upperRect.bottom += Trunc(diff.v);
		lowerRect.top := upperRect.bottom + kSplitHeight;
		if upperRect.bottom < totalRect.bottom - kSplitHeight - kMinimumSize then
		if lowerRect.top > totalRect.top + kSplitHeight + kMinimumSize then
		begin
			SetRect(splitterRect, upperRect.left, upperRect.bottom, upperRect.right, lowerRect.top);
			SetControlBounds(theSplitData^.upper, upperRect);
			SetControlBounds(theSplitData^.splitter, splitterRect);
			SetControlBounds(theSplitData^.lower, lowerRect);
		end;
		if totalRect.bottom - totalRect.top > 0 then
			theSplitData^.ratio := (upperRect.bottom - upperRect.top) / (totalRect.bottom - totalRect.top);
	end;

end;

// Get a view, create a new view with scrollbars
function EmbedInSplitter(parent: ControlRef; var masterView: ControlRef; isVertical: Boolean; ratio: Real): SplitDataPtr;
var
	mySplitData: SplitDataPtr;
	mainFrame: MacOSAll.Rect;
	upperRect, splitterRect, lowerRect: MacOSAll.Rect;
	h, hu{, hl}: Longint;
	layout: HILayoutInfo;
	
	viewEvents: array [0..1] of EventTypeSpec =
	(
		( eventClass: kEventClassControl; eventKind: kEventControlBoundsChanged ), // Fångar resize
		( eventClass: kEventClassControl; eventKind: kEventControlDispose )
	);
begin
	mySplitData := SplitDataPtr(NewPtr(SizeOf(SplitData)));
	mySplitData^.verticalSplit := isVertical;
	mySplitData^.ratio := ratio;

	if parent = nil then
		parent := HIViewGetSuperview(masterView);

// Kill old view and replace by a new one that supports embedding (since IB views don't)
// Or can I check whether it does?
	GetControlBounds(masterView, mainFrame); // In parent coordinates!
	layout.version := kHILayoutInfoVersionZero;
	HIViewGetLayoutInfo (masterView, layout); // Get layout
	HIViewRemoveFromSuperview(masterView);
	DisposeControl(masterView); // ??
	CreateUserPaneControl(nil, mainFrame, kControlSupportsEmbedding, mySplitData^.masterView); // feature: kControlSupportsEmbedding
	HIViewSetLayoutInfo (mySplitData^.masterView, layout); // Put back layout
	masterView := mySplitData^.masterView; // Update source reference

// Change to local 
	OffsetRect(mainFrame, -mainFrame.left, -mainFrame.top);
	
	if mySplitData^.verticalSplit then
	begin
	// Split mainFrame in three parts: Upper (left), lower (right) and splitter
		h := mainFrame.right-mainframe.left;
		hu := Trunc((h - kSplitHeight) * ratio);
//		hl := Trunc((h - kSplitHeight) * (1-ratio));

		SetRect(upperRect, mainframe.left, mainFrame.top, mainFrame.left + hu, mainFrame.bottom);
		SetRect(splitterRect, mainFrame.left + hu, mainFrame.top, mainframe.left + hu + kSplitHeight, mainFrame.bottom);
		SetRect(lowerRect, mainframe.left + hu + kSplitHeight, mainFrame.top, mainFrame.right, mainFrame.bottom);
	end
	else
	begin
	// Split mainFrame in three parts: Upper, lower and splitter
		h := mainFrame.bottom-mainframe.top;
		hu := Trunc((h - kSplitHeight) * ratio);
//		hl := Trunc((h - kSplitHeight) * (1-ratio));

		MacOSAll.SetRect(upperRect, mainframe.left, mainFrame.top, mainFrame.right, mainFrame.top + hu);
		MacOSAll.SetRect(splitterRect, mainframe.left, mainFrame.top + hu, mainFrame.right, mainFrame.top + hu + kSplitHeight);
		MacOSAll.SetRect(lowerRect, mainframe.left, mainFrame.top + hu + kSplitHeight, mainFrame.right, mainFrame.bottom);
	end;

// Make views
	CreateUserPaneControl(nil, upperRect, kControlSupportsEmbedding, mySplitData^.upper);
	CreateUserPaneControl(nil, splitterRect, kControlSupportsEmbedding, mySplitData^.splitter);
	CreateUserPaneControl(nil, lowerRect, kControlSupportsEmbedding, mySplitData^.lower);

	HIViewAddSubview (parent, mySplitData^.masterView);
	HIViewAddSubview (mySplitData^.masterView, mySplitData^.upper);
	HIViewAddSubview (mySplitData^.masterView, mySplitData^.splitter);
	HIViewAddSubview (mySplitData^.masterView, mySplitData^.lower);

// Handler for splitter
//	InstallQDSkelViewHandler(nil, mySplitData^.splitter, DrawSplitter, MouseSplitter, nil, nil);
	InstallDragViewHandler(mySplitData^.splitter, false, @DrawSplitter, @DragSplitter, mySplitData);

// Handler for master view
	InstallEventHandler( GetControlEventTarget(mySplitData^.masterView), @MasterViewEventHandlerProc, Length(viewEvents), viewEvents, mySplitData, nil );

	EmbedInSplitter := mySplitData;
end; // EmbedInSplitter

end.

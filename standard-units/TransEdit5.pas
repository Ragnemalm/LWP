// TransEdit for TransSkel 5 by Ingemar Ragnemalm 2010
// API based on TransEdit for earlier TransSkel version, written by Paul DuBois.
// Extended with more customization than the old package.
// First version 100621, part of TransSkel 5.0a8.
// Please note that this is brand new sources which surely contains many bugs.
// Be careful with your data and please report any bugs you find.
// 100813 (b2): Added mouse proc for sidebar.
// 100916: Added OpenEWindow. Adapted to SaveTextUnit.
// 100917: More revisions, added ESetFileTypes to specify allowed file types.
// 101003: Moved OpenFile and SaveFile into this unit.
// 120229: Removed itemAtIndex dure to interface bugs (same change in TransSkel long ago)

{$mode objfpc}
{$modeswitch objectivec1}
unit TransEdit5;
interface
uses
	CocoaAll, MacOSAll, TransSkel5, SkelViewUnit, QDCG, SkelFileDialogs;

// A textview with an extra view connected (sideBarView) intended as a kind of ruler,
// file reference and some useful callbacks.
type
	TransEditView = objcclass;
	TransEditFilterProc = function(textView: TransEditView; userPtr: Pointer;
				var theString: AnsiString; theRange: NSRange): Boolean;
	TransEditChangedProc = procedure(textView: TransEditView; userPtr: Pointer);
	TransEditDrawRectProc = procedure(theView: TransEditView; lineRect: NSRect;
				lineNumber, charNumber: Longint);
	TransEditPaintRectProc = procedure(theView: TransEditView; lineRect: NSRect);
	TransEditMouseProc = procedure(theView: TransEditView;
				lineNumber, charNumber: Longint;
				where: NSPoint);
	TransEditOpenedProc = procedure(w: NSWindow);

	TransEditView = objcclass(NSTextView, NSTextDelegateProtocol)
		sideBarView: SkelView; // Ruler replacement
		dirty: Boolean;
		filterProc: TransEditFilterProc;
		changedProc: TransEditChangedProc;
		drawSidebarRectProc: TransEditDrawRectProc;
		drawSidebarPaintProc: TransEditPaintRectProc;
		sidebarMouseProc: TransEditMouseProc;
		userDataPtr: Pointer;
		boundToFile: Boolean;
		textLeftMargin: Double;
		red, green, blue: Real; // Sidebar bg color
		// There seems to be a bug preventing us from using AnsiString
		// in an objcclass? So I must use workarounds.
//		fref: FSRef; This seems to work
//		fileNamePtr: Pointer; // Really pointer to AnsiString
		nsFileName: NSString; // Same filename as NSString
//		replyPtr: Pointer; // Really pointer to StandardFileReply - should work but I go for the filename
		// All this will NOT work (why?):
//		fileName: System.AnsiString;
		latestEditedRange: NSRange; // Only added just on case so far.
	public
		function textContainerOrigin: NSPoint; override;
		procedure drawRect (arect: NSRect); override;
//		procedure createSizeView(parentView: NSView); message 'createSizeView:';
		procedure updateMargin; message 'updateMargin';
		
//		// Delegate functions delegated to self
		procedure textDidChange (note: NSNotification); message 'textDidChange:';
		function textView_shouldChangeTextInRange_replacementString (aTextView: TransEditView;
					affectedRange: NSRange; replString: NSString): Boolean;
					message 'textView:shouldChangeTextInRange:replacementString:';
	end;

	function EWindowClose (theWind: NSWindow): boolean;
	function IsEWindow (theWind: NSWindow): Boolean;
	function IsEWindowDirty (theWind: NSWindow): Boolean;
//	function GetEWindowTE (theWind: NSWindow): TEHandle;
//	function GetEWindowFile (theWind: NSWindow; fileInfo: SFReplyPtr): Boolean;
	procedure SetEWindowProcs (theWind: NSWindow; pKey, pActivate, pClose: ProcPtr);
	procedure SetEWindowStyle (theWind: NSWindow; font, size, wrap, just: integer);
//	procedure EWindowOverhaul (theWind: NSWindow; showCaret, recalc, dirty: Boolean);
	procedure EWindowEditOp (item: integer);
//	procedure SetEWindowCreator (creat: OSType);
	function EWindowSave (theWind: NSWindow): Boolean;
	function EWindowSaveAs (theWind: NSWindow): Boolean;
	function EWindowSaveCopy (theWind: NSWindow): Boolean;
	function EWindowRevert (theWind: NSWindow): Boolean;
//	function NewEWindow (bounds: Rect; title: Str255; visible: Boolean; behind: NSWindow; goAway: Boolean; refNum: longint; bindToFile: Boolean): NSWindow;
//	function NewEWindow (wX, wY, wWidth, wHeight, leftMargin, topMargin,
//						rightMargin, bottomMargin: Double; title: Str255;
//						front: Boolean; bindToFile: Boolean): NSWindow;

// Main function
	function NewEWindow (wX, wY, wWidth, wHeight,
					leftMargin, topMargin, rightMargin, bottomMargin,
					sidebarWidth, middleMargin, textLeftMargin: Double;
					sideBarProc: TransEditDrawRectProc; sideBarPaintProc: TransEditPaintRectProc;
					mouseDownProc: TransEditMouseProc;
					title: Str255; front: Boolean; bindToFile: Boolean; openThisFile: AnsiString): NSWindow;overload;
// Without sidebar
	function NewEWindow (wX, wY, wWidth, wHeight,
					leftMargin, topMargin, rightMargin, bottomMargin: Double;
					title: Str255; openThisFile: AnsiString): NSWindow; overload;
// Close to old version
//	function NewEWindow (wX, wY, wWidth, wHeight: Double; title: Str255;
//					visible: Boolean; behind: NSWindow; goAway: Boolean;
//					refNum: longint; bindToFile: Boolean): NSWindow; overload;

// Using defaults
	function NewEWindow (fileName, openThisFile: AnsiString): NSWindow; overload;
	procedure SetEWindowDefaults(wX, wY, wWidth, wHeight,
				leftMargin, topMargin, rightMargin, bottomMargin,
				sidebarWidth, middleMargin, textLeftMargin: Double;
				sideBarProc: TransEditDrawRectProc; sideBarPaintProc: TransEditPaintRectProc;
				mouseDownProc: TransEditMouseProc;
				title: Str255; front: Boolean;
				// Special variables only for defaults
				staggerWindows: Boolean;
				staggerLeft, staggerTop, staggerWidth, staggerHeight,
				staggerStepH, staggerStepV: Double);
//	function OpenEWindow (openThisFile: AnsiString): NSWindow; overload;
procedure OpenEWindow({w: NSWindow;} openedProc: TransEditOpenedProc; allowRTF, allowRTFD: Boolean); overload;
procedure OpenEWindow({w: NSWindow;} openedProc: TransEditOpenedProc; typeList: StringArr); overload;

	function ClobberEWindows: Boolean;
//	procedure TransEditInit;
	
	function ETextViewClose (theView: TransEditView): boolean;
	function IsETextView (theView: NSTextView): Boolean;
	function IsETextViewDirty (theView: TransEditView): Boolean;
	procedure SetETextViewProcs (theView: TransEditView; pKey, pActivate, pClose: ProcPtr);
	procedure SetETextViewStyle (theView: TransEditView; font, size, wrap, just: integer);
	procedure ETextViewEditOp (item: integer);
	function ETextViewSave (theView: TransEditView): Boolean;
	function ETextViewSaveAs (theView: TransEditView): Boolean;
	function ETextViewSaveCopy (theView: TransEditView): Boolean;
	function ETextViewRevert (theView: TransEditView): Boolean;
//	function NewETextView (AX, AY, AWidth, AHeight: Double; parent: NSWindow;
//						bindToFile: Boolean): TransEditView;
//	function NewETextView (AX, AY, AWidth, AHeight: Double;
//						sideBarX, sideBarWidth,
//						parent: NSWindow;
//						bindToFile: Boolean): TransEditView;

// Saving type lists
	procedure ESetFileTypes(typeList, typeNamesList: StringArr);overload;
	procedure ESetFileTypes(allowRTF, allowRTFD: Boolean);overload;

	
	function GetEWindowText (theWind: NSWindow): AnsiString;
	function GetETextViewText (view: TransEditView): AnsiString;
	function GetEWindowTextView (theWind: NSWindow): TransEditView;
	procedure SetEWindowType (creat: OSType);
	procedure SetETextViewText(view: TransEditView; value: AnsiString);
	procedure SetEWindowText(theWind: NSWindow; value: AnsiString);
	
	procedure SkelSetTextViewHandler(textView: TransEditView;
		filterProc: TransEditFilterProc;
		changedProc: TransEditChangedProc;
		userDataPtr: Pointer);

// Customize
procedure SetETextViewSidebarColor(view: TransEditView; red, green, blue: Real);
procedure SetEWindowSidebarColor(theWind: NSWindow; red, green, blue: Real);

// Convert a text view (usually from a nib) to a TransEdit view
function SkelCloneEditView(oldView: NSView): TransEditView;

// Automatic updating of save and save as.
procedure ESetSaveItem(menu: NSMenu; index: Longint);overload;
procedure ESetSaveItem(menu: NSMenu; title: AnsiString);overload;
procedure ESetSaveItem(item: NSMenuItem);overload;
procedure ESetSaveAsItem(menu: NSMenu; index: Longint);overload;
procedure ESetSaveAsItem(menu: NSMenu; title: AnsiString);overload;
procedure ESetSaveAsItem(item: NSMenuItem);overload;
procedure ESetRevertItem(menu: NSMenu; index: Longint);overload;
procedure ESetRevertItem(menu: NSMenu; title: AnsiString);overload;
procedure ESetRevertItem(item: NSMenuItem);overload;
procedure ESetDefaultItemState(enableSave, enableSaveAs, enableRevert: Boolean);

	function GetLastToken(s: AnsiString): AnsiString;

implementation

const
//	left_margin_width = 5; // Extra click margin
	kBarWidth = 25; // Size of bar

var
	gTypeList, gTypeNamesList: StringArr;

procedure SynchMenus; forward;

// Simple subclass of NSTextContainer to modify the left edge
// Here used only to get extra space for easier clicks to the left
type
	TransEditViewContainer = objcclass(NSTextContainer)
		textLeftMargin: Double;
		public
			function lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect(proposedRect: NSRect; sweepDirection: NSLineSweepDirection; movementDirection: NSLineMovementDirection; remainingRect: NSRectPointer): NSRect; override;
	end;

// To modify the left margin.
function TransEditViewContainer.lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect(proposedRect: NSRect; sweepDirection: NSLineSweepDirection; movementDirection: NSLineMovementDirection; remainingRect: NSRectPointer): NSRect;
begin
    proposedRect.origin.x := textLeftMargin; // left_margin_width;
    result := inherited lineFragmentRectForProposedRect_sweepDirection_movementDirection_remainingRect(proposedRect, sweepDirection, movementDirection, remainingRect);
end;

procedure TransEditView.textDidChange (note: NSNotification);
begin
	// Uppdatera color coding?
	
	dirty := true;
//	w.setDocumentEdited(true);
	SynchMenus;
//	WriteLn('Dirty');

	if self.changedProc <> nil then
		self.changedProc(self, self.userDataPtr);
end;

// Filter function for incoming text!
function TransEditView.textView_shouldChangeTextInRange_replacementString (
	aTextView: TransEditView; affectedRange: NSRange; replString: NSString): Boolean;
var
	theString, theOldString: AnsiString;
begin
	result := true;
//	i := FindTextViewHandler(aTextView);
//	if i >= 0 then
//		if skelTextViews[i].filterProc <> nil then
//			result := skelTextViews[i].filterProc(aTextView, skelTextViews[i].userDataPtr, CFStringToString(CFStringRef(replString)), affectedRange);
	
	aTextView.latestEditedRange := affectedRange; // Save where the latest change took place
	
// Detta vore snyggare
	if self.filterProc <> nil then
	begin
//		theString := CFStringToString(CFStringRef(replString));
		theString := NSStringToString(replString);
		theOldString := theString;
		result := self.filterProc(aTextView, self.userDataPtr, theString, affectedRange);
//		if result then
		if result and (theString <> theOldString) then
		begin
//			replString := StringToNSString(theString);
// Mark that the next call should not be filtered here!		
			replaceCharactersInRange_withString(affectedRange, StringToNSString(theString));
			result := false;
		end;
	end;
end;

	procedure TransEditView.updateMargin;
	begin
		if sideBarView <> nil then
			sideBarView.setNeedsDisplay_(true);
	end;

//	procedure TransEditView.createSizeView(parentView: NSView);
//	begin
//		sideBarView := SkelNewView(nil, 0, 20, kBarWidth, bounds.size.height);
//		parentView.addSubview(sideBarView);
//		sideBarView.setAutoresizingMask(NSViewHeightSizable); // Funkar inte
//	end;

	function TransEditView.textContainerOrigin: NSPoint;
	begin
		result := inherited textContainerOrigin;
		result.x += textLeftMargin; // left_margin_width;
	end;

	procedure TransEditView.drawRect (arect: NSRect);
	begin
		// Uppdatera color coding i rektangel här
		
		inherited drawRect(arect);
//		WriteLn('PING');
		
		if sideBarView <> nil then
			sideBarView.setNeedsDisplay_(true);
	end;

// SaveFile and OpenFile - formerly in separate file, can easily live as
// a separate unit if you like.

procedure OpenFile(theTextView: NSTextView; filename: NSString);
var
	rtfData: NSData;
	fileContents: NSString;
begin
	if fileName.pathExtension.isEqualToString(NSSTR('rtfd')) then
	begin
		theTextView.readRTFDFromFile(fileName);
//		theFormat = RTFD;
	end
	else if fileName.pathExtension.isEqualToString(NSSTR('rtf')) then
	begin
		rtfData := NSData.dataWithContentsOfFile(fileName);
		// Error check? rtfData = nil?
		theTextView.replaceCharactersInRange_withRTF(NSMakeRange(0, theTextView.string_.length), rtfData);
//		theFormat = RichText;
	end
	else
	begin
		fileContents := NSString.stringWithContentsOfFile(fileName);
//		theTextView.setString_range(fileContents, NSMakeRange(0, theTextView.string_.length));
		theTextView.replaceCharactersInRange_withString(NSMakeRange(0, theTextView.string_.length), fileContents);

//		theFormat = PlainText;
	end;
end;

function SaveFile(theTextView: NSTextView; filename: NSString): Boolean;
//var
//	err: NSError;
begin
	if fileName.pathExtension.isEqualToString(NSSTR('rtfd')) then
	begin
		Exit(theTextView.writeRTFDToFile_atomically(filename, true));
	end
	else if fileName.pathExtension.isEqualToString(NSSTR('rtf')) then
	begin
		Exit(theTextView.RTFFromRange(NSMakeRange(0, theTextView.string_.length)).
			writeToFile_atomically(filename, true))
	end
	else
	begin
//		WriteLn('Trying to save to ', NSSTringToString(filename));
		Exit(theTextView.string_.writeToFile_atomically(filename, true)); // Returns a boolean that is true even if the save fails!?
//		if theTextView.string_.writeToFile_atomically_encoding_error(filename, true, NSUTF8StringEncoding, @err) then

		if not theTextView.string_.writeToFile_atomically(filename, true) then // Returns a boolean that is true even if the save fails!?
			WriteLn('SAVE FAILED GODDAMNIT');
		Exit(false);
	end;
end;

// ---

procedure Mouse(theView: NSView; where: HIPoint; mods: Longint; userData: Pointer);
var
	start_index: Longint;
	textView: TransEditView;
	scrollPos: Real;
	lineRange: NSRange;
	
	lineRect{, cr}: NSRect;
	index, lineNumber: Longint;
begin
	textView := TransEditView(userData); // Get the NSTextView, passed as user data pointer
	if textView <> nil then
	if textView.sidebarMouseProc <> nil then
	begin
// Convert "where" to line number and character index.
// First offset with scrolling position
		scrollPos := textView.enclosingScrollView.contentView.bounds.origin.y;
		where.y += scrollPos;
		where.x := 0;

// Get the index of first character
		start_index := textView.layoutManager.glyphIndexForPoint_inTextContainer(NSPoint(where), textView.textContainer);
		index := 0;
		lineNumber := 1;

// Skip all lines that are visible at the top of the text view (if any)
		while index < start_index do
		begin
			lineRect := textView.layoutManager.lineFragmentRectForGlyphAtIndex_effectiveRange(index, @lineRange);
			index := NSMaxRange( lineRange );
			lineNumber += 1;
		end;
		WriteLn(lineNumber);
		
// Send result to callback!
		textView.sidebarMouseProc(textView, lineNumber, start_index, NSPoint(where));
	end;
end;

// This routine is based on "LineNumbering" by Koen van der Drift.
procedure Draw(theView: NSView; cgContext: CGContextRef; viewRect: CGRect; userData: Pointer);
var
	r: Rect;
	start_index, end_index: Longint;
	glyphRange: NSRange;
	visRect: NSRect;
	textView: TransEditView;
	bounds: NSRect;
	
	lineRange: NSRange;
//	s: String;
	scrollPos: Real;
	
	lineRect, cr: NSRect;
	index, lineNumber: Longint;
begin
	UseNSViewContext(theView);
	
	textView := TransEditView(userData); // Get the NSTextView, passed as user data pointer
	bounds := theView.bounds;
	
// Translate for scroll position
	scrollPos := textView.enclosingScrollView.contentView.bounds.origin.y;
//	WriteLn('sp = ', scrollPos);
	CGContextTranslateCTM(cgContext, 0, -scrollPos);
	
// Paint the line number area
//	RGBForeColor(0.85, 0.9, 0.9); // Set to customizable sidebar color
	if TransEditView(textView).drawSidebarPaintProc <> nil then
	begin
		cr.origin.x := 0;
		cr.origin.y := scrollPos;
		cr.size.width := theView.frame.size.width;
		cr.size.height := bounds.size.height;
		TransEditView(textView).drawSidebarPaintProc(textView, cr);
	end
	else
		begin
			RGBForeColor(textView.red, textView.green, textView.blue); // Set to customizable sidebar color
			SetRect(r, 0, 0 + scrollPos, kBarWidth, bounds.size.height + scrollPos);
			PaintRect(r);
			RGBForeColor(0, 0, 0);
		end;

// Get the visible part of the text
	visRect := textView.enclosingScrollView.documentVisibleRect;
	glyphRange := textView.layoutManager.glyphRangeForBoundingRect_inTextContainer(visRect, textView.textContainer);
	textView.layoutManager.glyphRangeForBoundingRect_inTextContainer(visRect, textView.textContainer);
	start_index := glyphRange.location;
	end_index := glyphRange.location + glyphRange.length;
	
	index := 0;
	lineNumber := 1;

// Skip all lines that are visible at the top of the text view (if any)
	while index < start_index do
	begin
		lineRect := textView.layoutManager.lineFragmentRectForGlyphAtIndex_effectiveRange(index, @lineRange);
		index := NSMaxRange( lineRange );
		lineNumber += 1;
	end;

// Draw all visible lines
	index := start_index;
	while index < end_index do
	begin
		lineRect := textView.layoutManager.lineFragmentRectForGlyphAtIndex_effectiveRange(index, @lineRange);
		index := NSMaxRange( lineRange );
		
// CALLBACK TO SIDEBAR RECT DRAWING
		if TransEditView(textView).drawSidebarRectProc <> nil then
			TransEditView(textView).drawSidebarRectProc(textView, lineRect, lineNumber, index);
//		MoveTo(0, lineRect.origin.y + lineRect.size.height);
//		Str(lineNumber, s);
//		DrawString(s);
			
		lineNumber += 1;
	end;
	
// Draw "extra" line number
	lineRect := textView.layoutManager.extraLineFragmentRect;
//	MoveTo(0, lineRect.origin.y + lineRect.size.height);
//	Str(lineNumber, s);
//	DrawString(s);
// CALLBACK TO SIDEBAR RECT DRAWING
	if TransEditView(textView).drawSidebarRectProc <> nil then
		TransEditView(textView).drawSidebarRectProc(textView, lineRect, lineNumber, index);

	FinishPort;
end;



// Close handling

// Sheet callback
// Reply for "save before close?"
// Fires up a save dialog if needed
procedure TheSkelSheetProc(returnCode: NSInteger; userData: Pointer);
var
	theWindow: NSWindow;
begin
	theWindow := NSWindow(userData);
//	WriteLn('Sheet replied ', returnCode);
	if returnCode = 1 then // Yes
	begin
		EWindowSave(theWindow);
	end
	else
		if returnCode = -1 then // Cancel
			SkelCancelCloseWindow(theWindow)
		else
			SkelCloseWindow(theWindow, false);
end;

// Removal of window - can be either due to close command or quit.
function CloseWindow(theWindow: NSWindow; userData: Pointer): Longint; {1 = OK, 0 = later, -1 = cancel}
var
	textView: TransEditView;
begin
//	WriteLn('Nice close question');
	
	// Any saving need?
	textView := TransEditView(GetEWindowTextView(theWindow));
	if textView <> nil then
		if not textView.dirty then
		begin
//			WriteLn('NOT dirty! Close!');
			result := 1; // OK to close!
			Exit(CloseWindow);
		end;
	
	if not SkelWindowHasBeenAskedToClose(theWindow) then // have we been asked before? If not, ask.
		SkelSheetAlert('Save before closing?', 'Yes', 'No', 'Cancel', 'Unsaved data is lost',
			theWindow, @TheSkelSheetProc, theWindow);
	
	result := 0; // Don't know yet - wait!
end;

type
	FileNamePtr = ^AnsiString;

function GetFilePath(textView: TransEditView): AnsiString;
begin
	if textView.nsFilename <> nil then
		GetFilePath := NSStringToString(textView.nsFilename)
		//FileNamePtr(textView.fileNamePtr)^
	else
		GetFilePath := '';
end;

function GetFileName(textView: TransEditView): AnsiString;
begin
		GetFileName := GetLastToken(GetFilePath(textView));
end;

procedure SetFilePath(textView: TransEditView; fileNameAndPath: AnsiString);
//var
//	fnp: FileNamePtr;
begin
//	if textView.fileNamePtr <> nil then
//		fnp := FileNamePtr(textView.fileNamePtr)
//	else
//		fnp := New(FileNamePtr);
//	fnp^ := fileName;
//	textView.fileNamePtr := Pointer(fnp);
	textView.nsFileName := StringToNSString(fileNameAndPath);
	textView.nsFileName.retain;
	textView.boundToFile := true;
end;


function RTFIsAllowed: Boolean;
var
	i: Longint;
begin
	RTFIsAllowed := false;
	for i := Low(gTypeList) to High(gTypeList) do
		if gTypeList[i] = 'rtf' then // även rtfd?
			RTFIsAllowed := true;
end;

function GraphicsIsAllowed: Boolean;
var
	i: Longint;
begin
	GraphicsIsAllowed := false;
	for i := Low(gTypeList) to High(gTypeList) do
		if gTypeList[i] = 'rtfd' then
			GraphicsIsAllowed := true;
end;

procedure ActivateTEWindow(active: Boolean);forward;


// Builds a scrollable text view plus custom container view but no sideBarView
function NewETextView(x,y,w,h: Real;
						textLeftMargin: Real;
						parentView: NSView;
						initialText: AnsiString{; openThisFile: AnsiString}): TransEditView; overload;
var
	CFMessage: NSString;
	textViewRect, scrollViewRect: NSRect;
	textView: TransEditView;
	scrollView: NSScrollView;
	
	// Lilla extramarginalen:
	myContainer: TransEditViewContainer;
	frame: NSRect;
//	sidebarView: SkelView;
//	l: LongWord;
const
	LargeNumberForText = 1.0e7;
begin
	CFMessage := StringToNSString(initialText);
	scrollViewRect.origin.x := x;
	scrollViewRect.origin.y := y;
	scrollViewRect.size.width := w;
	scrollViewRect.size.height := h;
	
	scrollView := NSScrollView.alloc;
	scrollView.initWithFrame(scrollViewRect);
	scrollView.setHasVerticalScroller(true);
//	if hasHScroll then
		scrollView.setHasHorizontalScroller(true);	
	scrollView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
	scrollView.contentView.setAutoresizesSubviews(true);
	
	// Set up the text view
	textViewRect.size := scrollView.contentSize;
	textView := TransEditView(TransEditView.alloc);
	textView.initWithFrame(textViewRect);
	textView.insertText(id(CFMessage)); // Funkar!
	textView.setAutoresizingMask({NSViewWidthSizable or} NSViewHeightSizable);
	textView.setDelegate(textView); // So delegate methods are called

	// Clear some fields
//	textView.fileNamePtr := nil;
	textView.nsFileName := nil;
	textView.boundToFile := false;
	textView.filterProc := nil;
	textView.changedProc := nil;
	textView.drawSidebarRectProc := nil;
	textView.drawSidebarPaintProc := nil;
	textView.sidebarMouseProc := nil;

	textView.userDataPtr := nil;
	textView.dirty := false;
	
// Create my modified TextViewContainer
	frame := NSMakeRect(0, 0, scrollView.contentSize.width, scrollView.contentSize.height);
	myContainer := TransEditViewContainer(TransEditViewContainer.allocWithZone_(textView.zone)).
							initWithContainerSize(NSMakeSize(frame.size.width, 100000));
	myContainer.textLeftMargin := textLeftMargin; //textLeftMargin;
	textView.textLeftMargin := textLeftMargin; //textLeftMargin; // Must both know this number?
	myContainer.setWidthTracksTextView(true);
	myContainer.setHeightTracksTextView(false);
// This controls the inset of our text away from the margin.
	textView.replaceTextContainer(myContainer);
	myContainer.release;
	
// Make sure we get long lines with scroll
	textView.textContainer.setContainerSize(NSMakeSize(LargeNumberForText, LargeNumberForText));
	textView.textContainer.setWidthTracksTextView(false);
	textView.textContainer.setHeightTracksTextView(false);
	textView.setAutoresizingMask(NSViewNotSizable);
	textView.setMaxSize(NSMakeSize(LargeNumberForText, LargeNumberForText));
	textView.setHorizontallyResizable(true);
	textView.setVerticallyResizable(true);
	
	NSNotificationCenter(NSNotificationCenter.defaultCenter).addObserver_selector_name_object(textView,
						sel_registerName(PChar('updateMargin')),
						NSWindowDidUpdateNotification, textView.window);
	
	parentView.addSubview(scrollView);
	scrollView.addSubview(textView);
	scrollView.setDocumentView(textView);
	
	SkelSetFocus(textView);

// Finally, auto-init type arrays if they don't exist to avoid errors.
	if Length(gTypeList) = 0 then
		ESetFileTypes(false, false);
		
// Set RTD and graphics allowance
	textView.setRichText(RTFIsAllowed);
	textView.setImportsGraphics(GraphicsIsAllowed);

	result := textView;
end;

procedure TEAddSidebar(textView: TransEditView; parentView: NSView;
					sideBarProc: TransEditDrawRectProc; sideBarPaintProc: TransEditPaintRectProc;
					mouseDownProc: TransEditMouseProc;
					sideBarX, sideBarWidth: Real);
begin
	textView.sideBarView := SkelNewView(nil, sideBarX, textView.enclosingScrollView.frame.origin.y, sideBarWidth, textView.bounds.size.height);
	parentView.addSubview(textView.sideBarView);
	textView.sideBarView.setAutoresizingMask(NSViewHeightSizable); // Funkar inte
	InstallSkelViewHandler(textView.sideBarView, @Mouse, nil, @Draw, textView);

	textView.drawSidebarRectProc := sideBarProc;
	textView.drawSidebarPaintProc := sideBarPaintProc;
	textView.sidebarMouseProc := mouseDownProc;
end;

// Extended NewETextView which also creates the sidebar
function NewETextView(x,y,w,h: Real;
					textLeftMargin: Real;
					hasHScroll: Boolean;
					sideBarX, sideBarWidth: Double;
					sideBarProc: TransEditDrawRectProc; sideBarPaintProc: TransEditPaintRectProc;
					mouseDownProc: TransEditMouseProc;
					parentView: NSView;
					initialText: AnsiString; openThisFile: AnsiString): TransEditView; overload;
var
	textView: TransEditView;
//	s: AnsiString;
	theSuperView: SkelView;
begin
// 101218: An enclosing view (SkelView in case we want any extra functionality)
// in order to make it more friendly to e.g. SplitView.
	theSuperView := SkelNewView(nil, x-sideBarWidth, y, w+20, h);
	parentView.addSubview(theSuperView);
	theSuperView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);

//	SkelViewSetColor(theSuperView, 1,0,0); // NOP?

//	textView := NewETextView(x,y,w,h, textLeftMargin, parentView, initialText);
	textView := NewETextView(x,y,w,h, textLeftMargin, theSuperView, initialText);
	textView.enclosingScrollView.setHasHorizontalScroller(hasHScroll);	
	
//	TEAddSidebar(textView, parentView, sideBarProc, sideBarPaintProc, mouseDownProc, sideBarX, sideBarWidth);
	TEAddSidebar(textView, theSuperView, sideBarProc, sideBarPaintProc, mouseDownProc, sideBarX, sideBarWidth);
	// Read in this file
	if openThisFile <> '' then
	begin
		OpenFile(textView, StringToNSString(openThisFile));
//		s := ReadEntireFile(openThisFile);
//		SetETextViewText(textView, s);
		SetFilePath(textView, openThisFile);
		textView.boundToFile := true;
	end;
	
	NewETextView := textView;
end;

function NewEWindow (wX, wY, wWidth, wHeight,
					leftMargin, topMargin, rightMargin, bottomMargin,
					sidebarWidth, middleMargin, textLeftMargin: Double;
					sideBarProc: TransEditDrawRectProc; sideBarPaintProc: TransEditPaintRectProc;
					mouseDownProc: TransEditMouseProc;
					title: Str255; front: Boolean; bindToFile: Boolean; openThisFile: AnsiString): NSWindow; overload;
// The sidebar is assumed to be to the left of the text view.
// Whole set of horizontal ranges:
// leftMargin - sidebarWidth - middleMargin - textMargin(part of wWidth?) - wWidth - rightMargin
var
	w: NSWindow;
	textView: TransEditView;
begin
	w := SkelNewWindow(wX, wY, wWidth, wHeight, title, front);
	
	textView := NewETextView(leftMargin+sidebarWidth+middleMargin,
				topMargin, wWidth - (leftMargin+sidebarWidth+middleMargin) - rightMargin,
				wHeight - topMargin - bottomMargin,
				textLeftMargin,
				true, {hasHScroll}
				leftMargin, sideBarWidth,
				sideBarProc, sideBarPaintProc, mouseDownProc,
				GetWindowContentView(w),
				'', openThisFile);

	w.setReleasedWhenClosed(true);
	SkelSetWindowHandler(w, nil, nil, SkelDrawProcPtr(nil), @CloseWindow, nil, nil);
	SkelSetWindowActivateProc(w, @ActivateTEWindow);
	
	if openThisFile <> '' then
		if w <> nil then
			SkelSetWindowTitle(w, GetLastToken(openThisFile));

	SkelBringToFront(w);
	SynchMenus;
	
	NewEWindow := w;
	
//	if bindToFile then
//		Open file dialog
// NO - bad idea?
// We must be able to open from file name/path since that will come from many sources.
end;


// A variant with no sidebar but margins. No text margin.
// Not very interesting - remove?
function NewEWindow (wX, wY, wWidth, wHeight,
					leftMargin, topMargin, rightMargin, bottomMargin: Double;
					title: Str255; openThisFile: AnsiString): NSWindow; overload;
begin
	NewEWindow := NewEWindow (wX, wY, wWidth, wHeight,
					leftMargin, topMargin, rightMargin, bottomMargin,
					0, 0, 0 {text margin}, nil, nil, nil,
					title, true, false, openThisFile);
end;

// A variant as close to the original as possible (without using Rect)
// No margins and no sidebar
// Only for backwards compatibility and even that is marginal - remove?
(*
Commented out since modal file dialogs are no longer supported.
Rewrite to use a modeless one?

function NewEWindow (wX, wY, wWidth, wHeight: Double; title: Str255;
					visible: Boolean; behind: NSWindow; goAway: Boolean;
					refNum: longint; bindToFile: Boolean): NSWindow; overload;
var
	fileName: AnsiString;
begin
	fileName := '';
	if bindToFile then
		fileName := OldFileName;

	NewEWindow := NewEWindow (wX, wY, wWidth, wHeight,
					0, 0, 0, 0,
					0, 0, 0 {text margin}, nil, nil, nil,
					title, true, false, fileName);
	// visible, behind, goAway, bindToFile...
end;
*)

procedure OpenDialogCompletion(reply: SkelFileReply; userData: Pointer);
var
	theWindow: NSWindow;
	openedProc: TransEditOpenedProc;
begin
	if reply.sfGood then
	begin
		theWindow := NewEWindow(reply.fileName, reply.fileNameAndPath);
		if userData <> nil then
		begin
			openedProc := TransEditOpenedProc(userData);
			openedProc(theWindow);
		end;
	end;
end;

// OpenEWindow is exomplitly for creating a new window.
// How about loading into an existing one?
// Can this be generalized? It is of course possible to do this
// by calling SkelGetFile from the host program.
// But shouldn't TransEdit improve its tabbed editing capabilities?

function CreateTextfileTypelist(allowRTF, allowRTFD: Boolean): StringArr;
var
	typeList: StringArr;
begin
	SetLength(typeList, 1);
	typeList[0] := 'txt';
	if allowRTF then
	begin
		SetLength(typeList, 2);
		typeList[1] := 'rtf';
	end;
	if allowRTFD then
	begin
		SetLength(typeList, Length(typeList)+1);
		typeList[High(typeList)] := 'rtfd';
	end;
	CreateTextfileTypelist := typeList;
end;

// These strings should be customizable
function CreateTextfileTypeNameslist(allowRTF, allowRTFD: Boolean): StringArr;
var
	typeList: StringArr;
begin
	SetLength(typeList, 1);
	typeList[0] := 'Text file';
	if allowRTF then
	begin
		SetLength(typeList, 2);
		typeList[1] := 'RTF file';
	end;
	if allowRTFD then
	begin
		SetLength(typeList, Length(typeList)+1);
		typeList[High(typeList)] := 'RTFD file';
	end;
	CreateTextfileTypeNameslist := typeList;
end;

// Set type lists for file dialogs
procedure ESetFileTypes(typeList, typeNamesList: StringArr);overload;
begin
	gTypeList := typeList;
	gTypeNamesList := typeNamesList;
end;
procedure ESetFileTypes(allowRTF, allowRTFD: Boolean);overload;
begin
	gTypeList := CreateTextfileTypelist(allowRTF, allowRTFD);
	gTypeNamesList := CreateTextfileTypeNameslist(allowRTF, allowRTFD);
end;

procedure OpenEWindow({w: NSWindow;} openedProc: TransEditOpenedProc; allowRTF, allowRTFD: Boolean); overload;
var
	typeList: StringArr;
begin
	typeList := CreateTextfileTypelist(allowRTF, allowRTFD);
	SkelGetFile(typeList, nil{w}, @OpenDialogCompletion, openedProc);	
end;

procedure OpenEWindow({w: NSWindow;} openedProc: TransEditOpenedProc; typeList: StringArr); overload;
begin
	SkelGetFile(typeList, nil{w}, @OpenDialogCompletion, openedProc);	
end;

var
	gWindowDefaults: record
		wX, wY, wWidth, wHeight,
		leftMargin, topMargin, rightMargin, bottomMargin,
		sidebarWidth, middleMargin, textLeftMargin: Double;
		sideBarProc: TransEditDrawRectProc; sideBarPaintProc: TransEditPaintRectProc;
		mouseDownProc: TransEditMouseProc;
		title: Str255; front: Boolean;
		// Special variables only for defaults
		staggerWindows: Boolean;
		staggerLeft, staggerTop, staggerWidth, staggerHeight,
		staggerStepH, staggerStepV: Double;
		
		currentLeft, currentTop: Double;
	end;

// New window using defaults - preferred
function NewEWindow (fileName, openThisFile: AnsiString): NSWindow; overload;
begin
	NewEWindow := NewEWindow (
					gWindowDefaults.currentLeft, gWindowDefaults.currentTop, gWindowDefaults.wWidth, gWindowDefaults.wHeight,
					gWindowDefaults.leftMargin, gWindowDefaults.topMargin, gWindowDefaults.rightMargin, gWindowDefaults.bottomMargin,
					gWindowDefaults.sidebarWidth, gWindowDefaults.middleMargin, gWindowDefaults.textLeftMargin,
					gWindowDefaults.sideBarProc, gWindowDefaults.sideBarPaintProc,
					gWindowDefaults.mouseDownProc,
					{gWindowDefaults.title}fileName, gWindowDefaults.front, false, openThisFile);
	// visible, behind, goAway, bindToFile...

	if gWindowDefaults.staggerWindows then		// Make a step
	begin
		gWindowDefaults.currentLeft += gWindowDefaults.staggerStepH;
		gWindowDefaults.currentTop += gWindowDefaults.staggerStepV;
		if gWindowDefaults.currentLeft + gWindowDefaults.wWidth >
			gWindowDefaults.staggerLeft + gWindowDefaults.staggerWidth then
				gWindowDefaults.currentLeft := gWindowDefaults.staggerLeft;
		if gWindowDefaults.currentTop + gWindowDefaults.wHeight >
			gWindowDefaults.staggerTop + gWindowDefaults.staggerHeight then
				gWindowDefaults.currentTop := gWindowDefaults.staggerTop;
	end;
end;

// Set the defaults for the NewEWindow version using defaults
procedure SetEWindowDefaults(wX, wY, wWidth, wHeight,
				leftMargin, topMargin, rightMargin, bottomMargin,
				sidebarWidth, middleMargin, textLeftMargin: Double;
				sideBarProc: TransEditDrawRectProc; sideBarPaintProc: TransEditPaintRectProc;
				mouseDownProc: TransEditMouseProc;
				title: Str255; front: Boolean;
				// Special variables only for defaults
				staggerWindows: Boolean;
				staggerLeft, staggerTop, staggerWidth, staggerHeight,
				staggerStepH, staggerStepV: Double);
begin
	gWindowDefaults.wX := wX; // Unnecessary
	gWindowDefaults.wY := wY; // Unnecessary
	gWindowDefaults.wWidth := wWidth;
	gWindowDefaults.wHeight := wHeight;
	gWindowDefaults.leftMargin := leftMargin;
	gWindowDefaults.topMargin := topMargin;
	gWindowDefaults.rightMargin := rightMargin;
	gWindowDefaults.bottomMargin := bottomMargin;
	gWindowDefaults.sidebarWidth := sidebarWidth;
	gWindowDefaults.middleMargin := middleMargin;
	gWindowDefaults.textLeftMargin := textLeftMargin;
	gWindowDefaults.sideBarProc := sideBarProc;
	gWindowDefaults.sideBarPaintProc := sideBarPaintProc;
	gWindowDefaults.mouseDownProc := mouseDownProc;
	gWindowDefaults.title := title;
	gWindowDefaults.front := front;
	gWindowDefaults.staggerWindows := staggerWindows;
	gWindowDefaults.staggerLeft := staggerLeft;
	gWindowDefaults.staggerTop := staggerTop;
	if staggerHeight > 0 then
		gWindowDefaults.staggerHeight := staggerHeight
	else
		gWindowDefaults.staggerHeight := NSScreen.mainScreen.frame.size.height - staggerLeft;
	if staggerWidth > 0 then
		gWindowDefaults.staggerWidth := staggerWidth
	else
		gWindowDefaults.staggerWidth := NSScreen.mainScreen.frame.size.width - staggerTop;
	gWindowDefaults.staggerStepH := staggerStepH;
	gWindowDefaults.staggerStepV := staggerStepV;
	
	gWindowDefaults.currentLeft := wX;
	gWindowDefaults.currentTop := wY;
end;

procedure SkelSetTextViewHandler(textView: TransEditView;
		filterProc: TransEditFilterProc;
		changedProc: TransEditChangedProc;
		// mouse
		// mouseInLineRect
		// drawRowMarker
		userDataPtr: Pointer);
		// samt sätt avstånden
begin
	textView.filterProc := filterProc;
	textView.changedProc := changedProc;
	textView.userDataPtr := userDataPtr;
end;


function ETextViewClose (theView: TransEditView): boolean;
begin
	if theView.nsFileName <> nil then
		theView.nsFileName.release;
	ETextViewClose := false;
end;

function EWindowClose (theWind: NSWindow): boolean;
var
	theView: TransEditView;
begin
	theView := GetEWindowTextView(theWind);
	if theView.nsFileName <> nil then
		theView.nsFileName.release;
	if theView <> nil then
		EWindowClose := ETextViewClose(theView)
	else
		EWindowClose := false;
end;

function IsETextView (theView: NSTextView): Boolean;
begin
	IsETextView := 'TransEditView' = CFStringToString(CFStringRef(NSStringFromClass(theView.classForCoder)));
end;

function IsEWindow (theWind: NSWindow): Boolean;
begin
	WriteLn('IsEWindow on window entitled ', SkelGetWindowTitle(theWind));
	if nil <> GetEWindowTextView(theWind) then WriteLn('Is E Window') else WriteLn('Is not E Window');
	IsEWindow := nil <> GetEWindowTextView(theWind);
end;

function IsETextViewDirty (theView: TransEditView): Boolean;
begin
	IsETextViewDirty := TransEditView(theView).dirty;
end;

function IsEWindowDirty (theWind: NSWindow): Boolean;
var
	theView: TransEditView;
begin
	theView := GetEWindowTextView(theWind);
	if theView <> nil then
		IsEWindowDirty := IsETextViewDirty(theView)
	else
		IsEWindowDirty := false;
end;

// TO DO
// and to be modified.
procedure SetETextViewProcs (theView: TransEditView; pKey, pActivate, pClose: ProcPtr);
begin
end;
procedure SetEWindowProcs (theWind: NSWindow; pKey, pActivate, pClose: ProcPtr);
var
	theView: TransEditView;
begin
	theView := GetEWindowTextView(theWind);
	// Call TextView version
	SetETextViewProcs(theView, pKey, pActivate, pClose);
end;

procedure SetETextViewStyle (theView: TransEditView; font, size, wrap, just: integer);
begin
	// To do
end;

procedure SetEWindowStyle (theWind: NSWindow; font, size, wrap, just: integer);
var
	theView: TransEditView;
begin
	theView := GetEWindowTextView(theWind);
	if theView <> nil then
		SetETextViewStyle(theView, font, size, wrap, just);
	// Call TextView version
end;

// What are these supposed to do?
procedure ETextViewEditOp (item: integer);
begin
end;
procedure EWindowEditOp (item: integer);
//var
//	theView: TransEditView;
begin
//	theView := GetEWindowTextView(theWind);
	// Call TextView version
end;

//	procedure SetEWindowCreator (creat: OSType);

// Plain text file save
function SaveTheFile(textView: TransEditView): Boolean;
var
//	s: AnsiString;
//	fileName: AnsiString;
	theWindow: NSWindow;
//	success: Boolean;
begin
	if SaveFile(textView, textView.nsFileName) then
	begin
		theWindow := textView.window;
		if theWindow <> nil then
			SkelSetWindowTitle(theWindow, GetLastToken(NSStringToString(textView.nsfileName)));
		TransEditView(textView).dirty := false;
		result := true;
	end
	else // FAILED!
	begin
		theWindow := textView.window;
		if theWindow <> nil then
			SkelSetWindowTitle(theWindow, GetLastToken(NSStringToString(textView.nsfileName)) + ' SAVE FAILED');
		result := false;
	end;
end;

procedure SaveDialogCompletion(reply: SkelFileReply; userData: Pointer);
var
	textView: TransEditView;
	theWindow: NSWindow;
//	fnp: FileNamePtr;
	success: Boolean = false;
begin
	theWindow := NSWindow(userData);
//	textView := NSTextView(Skel GetIndView(theWindow, 'NSTextView', 1));
	textView := GetEWindowTextView(theWindow);
	if reply.sfGood then
	begin
		SetFilePath(textView, reply.fileNameAndPath);
		WriteLn('Saving from SaveDialogCompletion');
		success := SaveTheFile(textView);
		if success then
		begin
// Tedious trick to save a string to an objcobject
//			if textView.fileNamePtr <> nil then
//				fnp := FileNamePtr(textView.fileNamePtr)
//			else
//				fnp := New(FileNamePtr);
//			fnp^ := reply.fileName;
//			textView.fileNamePtr := Pointer(fnp);
			textView.boundToFile := true;
			textView.nsFileName := NSString(reply.fileNameAndPathCF);
			textView.nsFileName.retain; // ??
			// NOTE: Needs disposing!
		
			SkelSetWindowTitle(theWindow, reply.fileName);
			WriteLn('SAVE OK!!!');
		end
		else
		begin
			WriteLn('SAVE FAILED!!!');
// This alert crashes! Maybe because the file dialog is not gone yet?
//			SkelAlert('Failed to save "' + reply.fileName + '"!', '', '', '', '');
		end;
	end;
	
	if success then
	if SkelWindowHasBeenAskedToClose(theWindow) then
	begin
		WriteLn('CLOSING WINDOW! SkelWindowHasBeenAskedToClose was true.');
// Both these work?
//		SkelDisposeWindow(theWindow);
		SkelCloseWindow(theWindow, false); // Close unconditionally
	end
	else
		WriteLn('SkelWindowHasBeenAskedToClose was false')
	else
		WriteLn('No success');
end;

function ETextViewSaveAs (theView: TransEditView): Boolean;
var
	theWindow: NSWindow;
begin
	// If nil is passed, find the right view from the front window.
	if theView = nil then
	begin
		theWindow := SkelFrontWindow;
		if theWindow <> nil then
			theView := GetEWindowTextView(theWindow);
	end;
	if theView = nil then
	begin
		ETextViewSaveAs := false; // Failed
		Exit(ETextViewSaveAs);
	end;
	
	// Start the save dialog
	
	theWindow := theView.window;
	if theView.boundToFile then
		SkelPutFile(gTypeList, gTypeNamesList, 'Save as', GetFileName(theView), theWindow,
					@SaveDialogCompletion, theWindow)
	else
		SkelPutFile(gTypeList, gTypeNamesList, 'Save as', 'untitled.txt', theWindow,
					@SaveDialogCompletion, theWindow);
	ETextViewSaveAs := true; // OK so far
end;

function ETextViewSave (theView: TransEditView): Boolean;
var
	theWindow: NSWindow;
begin
	// If nil is passed, find the right view from the front window.
	if theView = nil then
	begin
		theWindow := SkelFrontWindow;
		if theWindow <> nil then
			theView := GetEWindowTextView(theWindow);
	end;
	if theView = nil then
	begin
		ETextViewSave := false; // Failed
		Exit(ETextViewSave);
	end;
	
	// Save or save as
	if not theView.boundToFile or (theView.nsFileName = nil) then
		ETextViewSave := ETextViewSaveAs(theView)
	else
		ETextViewSave := SaveTheFile(theView);
//		SaveTheFile(fileName: AnsiString; textView: NSTextView; theWindow: NSWindow);

//	ETextViewSave := true; // OK so far
end;

function EWindowSave (theWind: NSWindow): Boolean;
var
	theView: TransEditView;
begin
	theView := GetEWindowTextView(theWind);
	// Call TextView version
	if theView <> nil then
		EWindowSave := ETextViewSave(theView)
	else
		EWindowSave := false;
end;

function EWindowSaveAs (theWind: NSWindow): Boolean;
var
	theView: TransEditView;
begin
	theView := GetEWindowTextView(theWind);
	// Call TextView version
	if theView <> nil then
		EWindowSaveAs := ETextViewSaveAs(theView)
	else
		EWindowSaveAs := false;
end;

function ETextViewSaveCopy (theView: TransEditView): Boolean;
begin
	// TO DO
	ETextViewSaveCopy := false;
end;

function EWindowSaveCopy (theWind: NSWindow): Boolean;
var
	theView: TransEditView;
begin
	theView := GetEWindowTextView(theWind);
	// Call TextView version
	if theView <> nil then
		EWindowSaveCopy := ETextViewSaveCopy(theView)
	else
		EWindowSaveCopy := false;
end;

function ETextViewRevert (theView: TransEditView): Boolean;
begin
// TO DO
	ETextViewRevert := false;
end;

function EWindowRevert (theWind: NSWindow): Boolean;
var
	theView: TransEditView;
begin
	theView := GetEWindowTextView(theWind);
	// Call TextView version
	if theView <> nil then
		EWindowRevert := ETextViewRevert(theView)
	else
		EWindowRevert := false;
end;

function ClobberEWindows: Boolean;
begin
	ClobberEWindows := false;
end;

function GetETextViewText (view: TransEditView): AnsiString;
var
	cfText: CFStringRef;
begin
	if view <> nil then
	begin
		cfText := CFStringRef(view.string_);
		
		GetETextViewText := CFStringToString(cfText);
	end
	else
		GetETextViewText := '';
end;

function GetEWindowText (theWind: NSWindow): AnsiString;
var
	view: TransEditView; // TransEditView?
begin
	view := GetEWindowTextView(theWind);
	GetEWindowText := GetETextViewText(view);
end;

procedure SetETextViewText(view: TransEditView; value: AnsiString);
var
	CFText: NSString;
begin
	CFText := StringToNSString(value);
	view.setString(CFText);
end;

procedure SetEWindowText(theWind: NSWindow; value: AnsiString);
var
	view: TransEditView; // TransEditView?
begin
	view := GetEWindowTextView(theWind);
	SetETextViewText(view, value);
end;


function GetEWindowTextView (theWind: NSWindow): TransEditView;
begin
// We assume that only one TransEditView exists per window.
	GetEWindowTextView := TransEditView(SkelGetIndView(theWind, 'TransEditView', 1));
end;

procedure SetEWindowType (creat: OSType);
begin
end;

procedure SetETextViewSidebarColor(view: TransEditView; red, green, blue: Real);
begin
	view.red := red;
	view.green := green;
	view.blue := blue;
end;

procedure SetEWindowSidebarColor(theWind: NSWindow; red, green, blue: Real);
var
	view: TransEditView;
begin
	view := GetEWindowTextView(theWind);
	if view <> nil then
		SetETextViewSidebarColor(view, red, green, blue);
end;


// Create a SkelView by replacing an existing view
// This can be handy when using custom views in nibs.
// Variant of the SkelView cloning call.
function SkelCloneEditView(oldView: NSView): TransEditView;
var
	viewRect: NSRect;
	autoSub: Boolean;
	mask: NSUInteger;
begin
    autoSub := oldView.autoresizesSubviews; // Not yet meaningful
    mask := oldView.autoresizingMask;

	// Get the position for the NSView.
	viewRect := oldView.frame;
	// Create a new one with the same viewRect.
	// Can't use SkelNewView since we don't know the window!
	result := NewETextView(viewRect.origin.x,viewRect.origin.y,viewRect.size.width,viewRect.size.height,
						2, {Little default margin} oldView.superView,
						'');
	oldView.removeFromSuperview;
	
    result.setAutoresizesSubviews(autoSub);
    result.setAutoresizingMask(mask);
//	result.flipped := true; // Default
//	result.backgroundColor := NSColor.whiteColor;
	
	// Note: no subview support yet
end;


// --------------- Menu support ------------------

// Edit menus are expected to be automatic through Cocoa,
// but "save as" and "save" are not.
// TE can handle enabling and disabling of menu items.
// But what happens when no text window is in front?
// Then a specified default can be used.

// Missing feature: How about toolbar items? They should be disabled in synch.

var
	gSaveItem: NSMenuItem = nil;
	gSaveAsItem: NSMenuItem = nil;
	gRevertItem: NSMenuItem = nil;
	gEnableSave: Boolean = false;
	gEnableSaveAs: Boolean = false;
	gEnableRevert: Boolean = false;

procedure ESetSaveItem(menu: NSMenu; index: Longint);overload;
begin
//	gSaveItem := menu.itemAtIndex(index);
	gSaveItem := menu.itemArray.objectAtIndex(index);
end;
procedure ESetSaveItem(menu: NSMenu; title: AnsiString);overload;
begin
	gSaveItem := menu.itemWithTitle(StringToNSString(title));
end;
procedure ESetSaveItem(item: NSMenuItem);overload;
begin
	gSaveItem := item;
end;
procedure ESetSaveAsItem(menu: NSMenu; index: Longint);overload;
begin
//	gSaveAsItem := menu.itemAtIndex(index);
	gSaveAsItem := menu.itemArray.objectAtIndex(index);
end;
procedure ESetSaveAsItem(menu: NSMenu; title: AnsiString);overload;
begin
	gSaveAsItem := menu.itemWithTitle(StringToNSString(title));
end;
procedure ESetSaveAsItem(item: NSMenuItem);overload;
begin
	gSaveAsItem := item;
end;
procedure ESetRevertItem(menu: NSMenu; index: Longint);overload;
begin
//	gRevertItem := menu.itemAtIndex(index);
	gRevertItem := menu.itemArray.objectAtIndex(index);
end;
procedure ESetRevertItem(menu: NSMenu; title: AnsiString);overload;
begin
	gRevertItem := menu.itemWithTitle(StringToNSString(title));
end;
procedure ESetRevertItem(item: NSMenuItem);overload;
begin
	gRevertItem := item;
end;
procedure ESetDefaultItemState(enableSave, enableSaveAs, enableRevert: Boolean);
begin
	gEnableSave := enableSave;
	gEnableSaveAs := enableSaveAs;
	gEnableRevert := enableRevert;

	SkelSetInternalOpenApp(@SynchMenus);
end;

procedure SynchMenus;
var
	w: NSWindow;
	te: TransEditView;
begin
	w := SkelFrontWindow;
	if w = nil then WriteLn('NO FRONT');
	w := SkelKeyWindow;
	if w = nil then WriteLn('NO KEY');
	if IsEWindow(w) then
	begin
		WriteLn('SynchMenus on TE window');
		if gSaveItem <> nil then
			EnableMenuItem(gSaveItem, IsEWindowDirty(w));
		if gSaveAsItem <> nil then
			EnableMenuItem(gSaveAsItem, true);
		if gRevertItem <> nil then
		begin
			te := GetEWindowTextView(w);
			if te <> nil then
				EnableMenuItem(gRevertItem, IsEWindowDirty(w) and (te.nsFileName <> nil));
		end;
	end
	else
	begin
		WriteLn('SynchMenus on non-TE window');
//		WriteLn('Edit window deactivated', gEnableSave, gEnableSaveAs);
		if gSaveItem <> nil then
			EnableMenuItem(gSaveItem, gEnableSave);
		if gSaveAsItem <> nil then
			EnableMenuItem(gSaveAsItem, gEnableSaveAs);
		if gRevertItem <> nil then
			EnableMenuItem(gRevertItem, gEnableRevert);
	end;
end;

procedure ActivateTEWindow(active: Boolean);
begin
	// Lots of calls to SynchMenus...
	SynchMenus;
//	if active then
//	begin
//		SynchMenus;
//	end
//	else
//	begin
//		SynchMenus;
//		WriteLn('Edit window deactivated', gEnableSave, gEnableSaveAs);
//		if gSaveItem <> nil then
//			EnableMenuItem(gSaveItem, gEnableSave);
//		if gSaveAsItem <> nil then
//			EnableMenuItem(gSaveAsItem, gEnableSaveAs);
//		if gRevertItem <> nil then
//			EnableMenuItem(gRevertItem, gEnableRevert);
//	end;
end;


	{Get the file name from a path}
	{Useful when you have the path and want the file name}
	function GetLastToken(s: AnsiString): AnsiString;
	var
		i: Longint;
	begin
		i := Length(s);
		while i > 0 do
		begin
			if s[i] = '/' then
			begin
//				SetLength(s, i-1);
				Exit(Copy(s, i+1, Length(s) - i));
			end;
			i := i - 1;
		end;
		Exit(s);
	end;


end.

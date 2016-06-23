// New undo system 131010
// A better, more general solution would have to pack the TXNObject and undo data
// in one structure, and make calls using that information for all calls (effectively
// making a subclass of TXN). This is based on the Lightweight IDE window data arrays
// and is therefore based on the edit window indices.

// 131114: Now also is resposible of telling ColorCoding when a buffer changes
// Late 13 or 14 somewhere: Now works 100% against Halda.

// 141118: Renamed some calls from TXN. SetTXNDataUndoable -> SetTextUndoable.
// GetTXNDataToString -> GetTextDataToString
// The rest by replacing TXN with Text.

{$mode macpas}
{$I-}

unit UndoUnit;
interface
uses
	MacOSAll, LWPGlobals, Settings, AlertsUtils, TXNUtilities,
	{CustomTXNUnit,} LWPColorGlobals, Halda, HaldaTypes;

procedure InitUndoForIndex(index: Longint); // At open
procedure SetTextUndoable(index: Longint; data: AnsiString); overload; // Replaces TXNSetData
procedure SetTextUndoable(index: Longint; selectionStart, selectionEnd: LongWord; data: AnsiString); overload;
procedure UndoText(index: Longint); // Replaces TXNUndo
procedure RedoText(index: Longint); // Replaces TXNRedo
function CanUndoText(index: Longint): Boolean; // Replaces TXNCanUndoAction
function CanRedoText(index: Longint): Boolean; // Replaces TXNCanRedoAction

function GetTextDataToString(index: Longint; selectionStart, selectionEnd: Longint): AnsiString;

procedure CutText(index: Longint);
function CopyText(index: Longint): OSErr;
procedure PasteText(index: Longint);

// Unecessary unless I reactivate dragging
//procedure CheckMouseDragForUndoFix(eventKind: UInt32);

implementation

uses
	LWPEdit, ColorCoding;

type
	UndoRec = record
		selectionStart, selectionEnd: Longint; // selection before change
		oldData, newData: AnsiString;
	end;

var
	undoBuffer: array [1..kMaxEditWindows] of array of UndoRec;
	undoIndex: array [1..kMaxEditWindows] of Longint; // Aktuell undo

procedure InitUndoForIndex(index: Longint);
begin
	SetLength(undoBuffer[index], 0);
	undoIndex[index] := -1;
end;


function GetTextDataToString(index: Longint; selectionStart, selectionEnd: Longint): AnsiString;
var
	err: OSErr;
	chars: Handle;
	buffer: AnsiString;
begin
//	err := TXNGetDataEncoded(teEdit[index], selectionStart, selectionEnd, Handle(chars), kTXNTextData);
//	SetLength(buffer, GetHandleSize(chars));
//	BlockMoveData(chars^, @buffer[1], GetHandleSize(chars));
//	DisposeHandle(chars);
//	GetTextDataToString := buffer;

	GetTextDataToString := Copy(teEdit[index]^.text, selectionStart, selectionEnd - selectionStart);
//	GetTextDataToString := teEdit[index]^.h^.text;
end;

procedure SetTextUndoable(index: Longint; data: AnsiString); overload;
var
	selectionStart, selectionEnd: Longint;
	err: OSErr;
begin
	HGetSelection(teEdit[index], selectionStart, selectionEnd);
	SetTextUndoable(index, selectionStart, selectionEnd, data);
end;

procedure SetTextUndoable(index: Longint; selectionStart, selectionEnd: LongWord; data: AnsiString); overload;
var
	err: OSErr;
	oldEnd: Longint;
begin
	if index < 0 then
	begin
		MessageAlert('Serious error in the undo system! Window number is < 0', 'This should not happen. If it happens often, please report!');
		Exit(SetTextUndoable);
	end;
	if undoIndex[index] >= 0 then
		oldEnd := undoBuffer[index][undoIndex[index]].selectionEnd
	else
		oldEnd := -1;
	
	// Decide if this is a new undo or whether it can be appended to the previous one.
	if (undoIndex[index] < 0) or (Length(data) <> 1) or
	(selectionStart <> oldEnd+1) or
	(selectionStart <> selectionEnd) or
	(data = #13) then
	begin
//		if undoIndex[index] < 0 then WriteLn('undoIndex[index] < 0 = ', undoIndex[index] < 0);
//		if selectionStart <> oldEnd then WriteLn('selectionStart <> oldEnd = ', selectionStart, ',', oldEnd);
//		if selectionStart <> selectionEnd then WriteLn('Length(dataselectionStart <> selectionEnd) = ', selectionStart, ',', selectionEnd);
//		if data = #13 then WriteLn('data = #13');
//		if Length(data) <> 1 then WriteLn('Length(data) = ', Length(data));
		
		SetLength(undoBuffer[index], undoIndex[index]+2);
		undoIndex[index] += 1;

		undoBuffer[index][undoIndex[index]].selectionStart := selectionStart;
		undoBuffer[index][undoIndex[index]].selectionEnd := selectionEnd;
		undoBuffer[index][undoIndex[index]].oldData := GetTextDataToString(index, selectionStart, selectionEnd);
//		WriteLn('Old data "', undoBuffer[index][undoIndex[index]].oldData, '"');
//		WriteLn('New data "', data, '"');
		undoBuffer[index][undoIndex[index]].newData := data;
	end
	else
	// If immediately after previous, - concatenate!
//	if (Length(data) > 0) and (selectionStart = undoBuffer[index][undoIndex[index]].selectionEnd) and (selectionStart = selectionEnd) then
	begin
//		WriteLn('Extending old undo data');
		undoBuffer[index][undoIndex[index]].selectionEnd += Length(data);
		undoBuffer[index][undoIndex[index]].newData += data;
		WriteLn('to ', undoBuffer[index][undoIndex[index]].newData);
	end;
	HInsertAt(teEdit[index], data, selectionStart, selectionEnd); // Should this change the selection?

// €ndra:
// - Skapa plats i listan, Šndra index
// - HŠmta selection TXNGetDataEncoded?
// - Spara gamla data och nya data
// - SŠtt nya data med TXNSetData

// Right place to change selection?
	HSetSelection(teEdit[index], selectionStart+Length(data), selectionStart+Length(data));	
	
	dirty[index] := true;
	autodirty[index] := true;
	TouchColorCoder(index, selectionStart-50);
end;

procedure UndoText(index: Longint);
var
	selectionStart, selectionEnd, prevStart: Longint;
	data: AnsiString;
	err: OSErr;
begin
	WriteLn('UndoText');
	if undoIndex[index] < 0 then Exit(UndoText);
	
	selectionStart := undoBuffer[index][undoIndex[index]].selectionStart;
	selectionEnd := undoBuffer[index][undoIndex[index]].selectionStart + Length(undoBuffer[index][undoIndex[index]].newData);
	HSetSelection(teEdit[index], selectionStart, selectionEnd);
	data := undoBuffer[index][undoIndex[index]].oldData;
	WriteLn('undo data = ', data);
	HInsertAt(teEdit[index], data, selectionStart, selectionEnd);
	
	undoIndex[index] -= 1;
	
// Undo:
// - SŠtt selection med enbart selectionStart + lŠngd av newData
// - SŠtt in oldData
// - Backa undoIndex
	
	dirty[index] := true;
	autodirty[index] := true;
	WriteLn('will do HShowSelection');
	HShowSelection(teEdit[index], true); // Crash??? Fixed!
	WriteLn('HShowSelection done');
	TouchColorCoder(index, selectionStart);
	WriteLn('UndoText done');
end;

procedure RedoText(index: Longint);
var
	selectionStart, selectionEnd: Longint;
	data: AnsiString;
	err: OSErr;
begin
//	if undoIndex[index] < High(undoIndex) then // €r detta verkligen rŠtt? €r det inte lŠngden av undoBuffer[index] jag vill kolla?
	if undoIndex[index] < High(undoBuffer[index]) then // €r detta bŠttre?
	begin
		undoIndex[index] += 1;

		selectionStart := undoBuffer[index][undoIndex[index]].selectionStart;
		selectionEnd := undoBuffer[index][undoIndex[index]].selectionEnd;
		data := undoBuffer[index][undoIndex[index]].newData;
		HInsertAt(teEdit[index], data, selectionStart, selectionEnd); // Fatal error fixed (synched)!
// IMPORTANT: This is NOT OK yet! Redo loses data!
// But exactly when?
// Also, the selection is not restored!
	end;

// Redo:
// Som €ndra men tag data och selection ur undobuffern
	
	dirty[index] := true;
	autodirty[index] := true;
	HShowSelection(teEdit[index], true);
	TouchColorCoder(index, selectionStart);
end;

function CanUndoText(index: Longint): Boolean; // Replaces TXNCanUndoAction
begin
	CanUndoText := undoIndex[index] >= 0;
end;

function CanRedoText(index: Longint): Boolean; // Replaces TXNCanRedoAction
begin
	CanRedoText := undoIndex[index] < High(undoBuffer[index]); // HŠr var det ocksŒ fel
end;

procedure CutText(index: Longint);
var
	err: OSErr;
begin
	err := CopyText(index);
	if err = noErr then
		SetTextUndoable(index, '');	
end;
function CopyText(index: Longint): OSErr;
var
	selectionStart, selectionEnd: Longint;
	clipData: AnsiString;
	scrap: ScrapRef;
	err: OSErr;
begin
	HGetSelection(teEdit[index], selectionStart, selectionEnd);
	clipData := GetTextDataToString(index, selectionStart, selectionEnd);
	
	WriteLn('CopyText: "', clipData, '"'); // Funkar hit fram
	
	ClearCurrentScrap();
	err := GetCurrentScrap(scrap);
	WriteLn('CopyText GetCurrentScrap');
	if err <> noErr then
		WriteLn('GetCurrentScrap: ', err);
	if err = noErr then
		err := PutScrapFlavor(scrap, 'TEXT', kScrapFlavorMaskNone, Length(clipData), @clipData[1]);
	WriteLn('CopyText PutScrapFlavor');
	if err <> noErr then
		WriteLn('PutScrapFlavor: ', err);
		// -100 no scrap exists
		
	WriteLn('Done CopyText');
	
	CopyText := err;
end;
procedure PasteText(index: Longint); // Funkar bra
var
	clipData: AnsiString;
	scrap: ScrapRef;
	byteCount: Longint;
	err: OSErr;
	j: Longint;
begin
	err := GetCurrentScrap(scrap);
	err := GetScrapFlavorSize(scrap, 'TEXT', byteCount);
	if err = noErr then
	begin
		SetLength(clipData, byteCount);
		err := GetScrapFlavorData(scrap, 'TEXT', byteCount, @clipData[1]);

		WriteLn('PasteText: ', clipData);
		
		// Zap gremlins! That is the non-breaking space that GCC hates!
		for j := 1 to Length(clipData) do
			if clipData[j] = Char($CA) then
				clipData[j] := ' ';
		
		SetTextUndoable(index, clipData);	
	end;
end;




end.


// No problem if dragging disabled - includes within document.

	// Data for CheckMouseDragForUndoFix collected in a record
	var
		undoDrag: record
			selectionStartOnMouseDown, selectionEndOnMouseDown: LongWord;
			selectedOnMouseDown: AnsiString;
			mouseDraggingHasOccurred: Boolean;
			editIndex: Longint;
		end;
	
	// For new undo system we must detect dragging of text blocks!
	procedure CheckMouseDragForUndoFix(eventKind: UInt32);
	var
		editIndex: Longint;
		selectionStartOnMouseUp, selectionEndOnMouseUp: Longint;
	begin
		editIndex := GetWRefCon(FrontWindow);
		if editIndex < 1 then
			Exit(CheckMouseDragForUndoFix);
		case eventKind of 
			kEventMouseDown:
			begin
				HGetSelection(teEdit[editIndex]^.h, undoDrag.selectionStartOnMouseDown, undoDrag.selectionEndOnMouseDown);
				undoDrag.selectedOnMouseDown := GetTextDataToString(editIndex, undoDrag.selectionStartOnMouseDown, undoDrag.selectionEndOnMouseDown); // What was selected?
				undoDrag.mouseDraggingHasOccurred := false; // No sign or danger yet
				undoDrag.editIndex := editIndex; // Remember which window this was
			end;
			kEventMouseUp:
			begin
				if editIndex <> undoDrag.editIndex then // Must be same!
					Exit(CheckMouseDragForUndoFix);
				if not undoDrag.mouseDraggingHasOccurred then // No dragging, no problem
					Exit(CheckMouseDragForUndoFix);
				HGetSelection(teEdit[editIndex]^.h, selectionStartOnMouseUp, selectionEndOnMouseUp);
				if undoDrag.selectionStartOnMouseDown = selectionStartOnMouseUp then // Same start - can't be bad, probably just selection
					Exit(CheckMouseDragForUndoFix);
				if undoDrag.selectionEndOnMouseDown = selectionEndOnMouseUp then // Same end - can't be bad, probably just selection
					Exit(CheckMouseDragForUndoFix);
				
				// Fake deleting old selection
				// Fake writing in new selection
			end;
			kEventMouseDragged:
			begin
				undoDrag.mouseDraggingHasOccurred := true;
			end;
		end; {case}
	end;

end.


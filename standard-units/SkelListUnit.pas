// SkelList
// This is a list management unit for TransSkel, a layer on top
// of NSTableView. A shown by the demo, the API allows very compact code.

// Regrettably, this first version does not allow dynamic arrays and has
// therefore a size limit determined at compilation time by constants
// below. I wish to remove this limitation but Objective Pascal
// currently has problems with dynamic arrays.

{$mode objfpc}
{$modeswitch objectivec1}

unit SkelListUnit;
interface
uses
	ctypes, MacOSAll, CocoaAll, TransSkel5, SkelViewUnit;

const
	kMaxRows = 1000;
	kMaxCols = 30;
type
	EditProc = function(row, column: Longint; var s: String): Boolean;
	SelectProc = procedure(selectedRow: Longint);
	ListItem = record
		itemType: Integer; // 0 = rownumber, 1 = text, 2 = number
		textData: array [0..kMaxRows] of String;
		numberData: array [0..kMaxRows] of Longint;
	end;

type
  SkelList = objcclass(NSObject, NSTableViewDataSourceProtocol)
  public
	theList: NSTableView;
	theScroll: NSScrollView;
	rows, columns: Longint;
	
	listData: array[0..kMaxCols] of ListItem;
	
	editCallback: EditProc;
	selectedCallback: SelectProc;
	
// Data source methods
    function numberOfRowsInTableView(tableView: NSTableView): Longint; message 'numberOfRowsInTableView:';
    function tableView_objectValueForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: Longint): id; message 'tableView:objectValueForTableColumn:row:';
// Table view delegate methods
    procedure tableViewSelectionDidChange(notification: NSNotification); message 'tableViewSelectionDidChange:';
    procedure tableView_setObjectValue_forTableColumn_row(aTableView: NSTableView;
	  anObject: id; aTableColumn: NSTableColumn; rowIndex: Longint);
	   message 'tableView:setObjectValue:forTableColumn:row:';
  end;

function SkelNewList(parentView: NSView; x,y,w,h: Real): SkelList;
procedure SkelAddListColumn(theList: SkelList; title: AnsiString; colType, width: Integer; editable: Boolean);
procedure SkelAddListTextColumn(theList: SkelList; title: AnsiString; width: Integer; editable: Boolean);
procedure SkelAddListNumberColumn(theList: SkelList; title: AnsiString; width: Integer; editable: Boolean);
procedure SkelAddListIndexColumn(theList: SkelList; title: AnsiString; width: Integer);
procedure SkelAddListRows(theList: SkelList; howMany, after: Integer);
procedure SkelAddListRow(theList: SkelList; after: Integer);
procedure SkelDisposeList(theList: SkelList);
procedure SkelDeleteListRows(theList: SkelList; howMany, fromrow: Integer);
procedure SkelDeleteListRow(theList: SkelList; row: Integer);
procedure SkelSetListCell(theList: SkelList; row, column: Integer; value: AnsiString); overload;
procedure SkelSetListCell(theList: SkelList; row, column: Integer; value: Longint); overload;
procedure SkelSetListProcs(theList: SkelList; select: SelectProc; edit: EditProc);
procedure SkelSetListHeaderHeight(theList: SkelList; height: Longint);
function SkelGetListSelect(theList: SkelList): Longint;

implementation

function SkelList.numberOfRowsInTableView(tableView: NSTableView): Longint;
begin
//	WriteLn('numberOfRowsInTableView');
	result := rows;
end;

function SkelList.tableView_objectValueForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: Longint): id;
var
	ident, s: String;
	column, value: Longint;
begin
	result := nil;
	if tableColumn.identifier = nil then
		Exit(tableView_objectValueForTableColumn_row);
	ident := NSStringToString(tableColumn.identifier);

	if row <= rows then // High(theListData^.items[column].textData) then
	if row >= 0 then // Low(theListData^.items[column].textData) then
	begin
		if ident = '#' then // Row number
		begin
			Str(row+1 {OR RANKBASE?}, s);
			result := StringToNSString(s);
//			result := NSSTR('INDEX');
		end;
		if ident[1] in ['A'..'Z'] then // Name
		begin
			column := Ord(ident[1]) - Ord('A');
			s := listData[column].textData[row];
			result := StringToNSString(s);
		end;
		if ident[1] in ['0'..'9'] then // Score
		begin
			column := Ord(ident[1]) - Ord('0');
			value := listData[column].numberData[row];
			Str(value, s);
			result := StringToNSString(s);
		end;
	end;
end;

// Delegate method
procedure SkelList.tableViewSelectionDidChange(notification: NSNotification);
begin
//	WriteLn('Selected row = ', theList.selectedRow);
	if selectedCallback <> nil then
		selectedCallback(theList.selectedRow);
end;

// Editing message
procedure SkelList.tableView_setObjectValue_forTableColumn_row(aTableView: NSTableView;
	anObject: id; aTableColumn: NSTableColumn; rowIndex: Longint);
var
	ident, s: String;
	ii: Longint;
	column: Longint;
	ok: Boolean;
begin
//	WriteLn('EDIT at row ', rowIndex);
//	WriteLn('col ', NSStringToString(aTableColumn.identifier));
//	WriteLn(NSStringToString(NSString(anObject)));

	ident := NSStringToString(aTableColumn.identifier);

	if ident[1] in ['A'..'Z'] then
		column := Ord(ident[1]) - Ord('A');
	if ident[1] in ['0'..'9'] then
		column := Ord(ident[1]) - Ord('0');

	s := NSStringToString(NSString(anObject));
	ok := true;
	if editCallback <> nil then
		ok := editCallback(rowIndex, column, s);
	if not ok then
		Exit;

	if ident[1] in ['A'..'Z'] then
	begin
		column := Ord(ident[1]) - Ord('A');
		listData[column].textData[rowIndex] := s; // NSStringToString(NSString(anObject));
	end;
	if ident[1] in ['0'..'9'] then
	begin
		column := Ord(ident[1]) - Ord('0');
//		s := NSStringToString(NSString(anObject));
		Val(s, ii);
		listData[column].numberData[rowIndex] := ii;
	end;
	
end;

// ----------- API part -------------

// Argument order does unfortunately not conform with List Manager, but with TransSkel 5 calls.
function SkelNewList(parentView: NSView; x,y,w,h: Real): SkelList;
var
	container: NSScrollView;
	table: NSTableView;
	theSkelList: SkelList;
begin
	// Set up a scroll view
	container := NSScrollView.alloc.initWithFrame(NSMakeRect(x,y,w,h));
	container.setHasVerticalScroller(true);
	container.setHasHorizontalScroller(false);
	parentView.addSubview(container);
//	container.setAutoresizesSubviews(true);
	container.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
	container.release;
	
	// Create the table, put in the scroll view
	table := NSTableView.alloc.initWithFrame(NSMakeRect(12, 11, 386, 153));
	table.setGridStyleMask(NSTableViewSolidVerticalGridLineMask+NSTableViewSolidHorizontalGridLineMask);
	container.setDocumentView(table);
	
	// Create SkelList
	theSkelList := SkelList.alloc;
	// Connect SkelList to table
	table.setDataSource(theSkelList);
	table.setDelegate(NSTableViewDelegateProtocol(theSkelList));
	
	theSkelList.theList := table;
	theSkelList.theScroll := container;
	// Zero some fields just in case
	theSkelList.rows := 0;
	theSkelList.columns := 0;
	theSkelList.editCallback := nil;
	theSkelList.selectedCallback := nil;
	
	result := theSkelList;
end;

procedure SkelAddListColumn(theList: SkelList; title: AnsiString; colType, width: Integer; editable: Boolean);
var
	col: NSTableColumn;
begin
	case colType of
	0: // Row number
		col := NSTableColumn.alloc.initWithIdentifier(NSSTR('#'));
	1: // Text
	begin
		col := NSTableColumn.alloc.initWithIdentifier(StringToNSString(Char(Ord('A')+theList.columns)));
	end;
	2: // Number
	begin
		col := NSTableColumn.alloc.initWithIdentifier(StringToNSString(Char(Ord('0')+theList.columns)));
	end;
	otherwise
	end;
	theList.listData[theList.columns].itemType := colType;
	col.setWidth(width);
	NSTableHeaderCell(col.headerCell).setStringValue(StringToNSString(title));
	NSCell(col.dataCell).setEditable(editable);
	NSCell(col.dataCell).setSelectable(true);
	theList.theList.addTableColumn(col);
	theList.columns += 1;
	
	// Activate sideways scroll if more than 2 columns.
	if theList.columns > 2 then
		theList.theScroll.setHasHorizontalScroller(false);
end;
procedure SkelAddListTextColumn(theList: SkelList; title: AnsiString; width: Integer; editable: Boolean);
begin
	SkelAddListColumn(theList, title, 1, width, editable);
end;
procedure SkelAddListNumberColumn(theList: SkelList; title: AnsiString; width: Integer; editable: Boolean);
begin
	SkelAddListColumn(theList, title, 2, width, editable);
end;
procedure SkelAddListIndexColumn(theList: SkelList; title: AnsiString; width: Integer);
begin
	SkelAddListColumn(theList, title, 0, width, false);
end;

procedure SkelAddListRows(theList: SkelList; howMany, after: Integer);
var
	i, ii, li: Longint;
begin
// Add a row before table.selectedRow, or at the end

// 1. Modify data arrays
	if after >= 0 then
		ii := after
	else
		ii := theList.rows; // ???
	
	theList.rows += howMany;
	for li := 0 to theList.columns do // Low(theListData^.items) to High(theListData^.items) do
	begin
		for i := theList.rows-howMany downto ii do
		begin
			theList.listData[li].textData[i+howMany] := theList.listData[li].textData[i];
			theList.listData[li].numberData[i+howMany] := theList.listData[li].numberData[i];
//			tableScores[i+1] := tableScores[i];

			theList.listData[li].textData[i] := 'NEW';
			theList.listData[li].numberData[i] := 0;
		end;
	end;
	
// 2. Tell the table that things changed
	theList.theList.noteNumberOfRowsChanged;
	theList.theList.setNeedsDisplay_(true);
end;

procedure SkelAddListRow(theList: SkelList; after: Integer);
begin
	SkelAddListRows(theList, 1, after);
end;

procedure SkelDisposeList(theList: SkelList);
begin
// Not needed?
end;

procedure SkelDeleteListRows(theList: SkelList; howMany, fromrow: Integer);
var
	i, li: Longint;
begin
// Add a row before table.selectedRow, or at the end

// 1. Modify data arrays

// Delete at end?
	if fromrow+howMany >= theList.rows then
	begin
		theList.rows := fromrow;
	end
	else
	begin
		theList.rows -= howMany;
		for li := 0 to theList.columns do // Low(theListData^.items) to High(theListData^.items) do
		begin
			for i := fromrow to fromrow+howMany do
			begin
				theList.listData[li].textData[i] := theList.listData[li].textData[i+howMany];
				theList.listData[li].numberData[i] := theList.listData[li].numberData[i+howMany];
			end;
		end;
	end;
	
// 2. Tell the table that things changed
	theList.theList.noteNumberOfRowsChanged;
	theList.theList.setNeedsDisplay_(true);
end;

procedure SkelDeleteListRow(theList: SkelList; row: Integer);
begin
	SkelDeleteListRows(theList, 1, row);
end;

procedure SkelSetListCell(theList: SkelList; row, column: Integer; value: AnsiString); overload;
var
	i: Longint;
begin
	// Auto-add rows
	if row >= theList.rows then
		SkelAddListRows(theList, row - theList.rows+1, -1);

	// Auto-add columns (should be avoided if you have a header)
	if column >= theList.columns then
	for i := 0 to column - theList.columns do
		SkelAddListColumn(theList, '', 1, 200, true);
	
	theList.listData[column].textData[row] := value;
end;

procedure SkelSetListCell(theList: SkelList; row, column: Integer; value: Longint); overload;
var
	i: Longint;
begin
	// Auto-add rows
	if row >= theList.rows then
		SkelAddListRows(theList, row - theList.rows+1, -1);
	
	// Auto-add columns (should be avoided if you have a header)
	if column >= theList.columns then
	for i := 0 to column - theList.columns do
		SkelAddListColumn(theList, '', 2, 100, true);
	
	theList.listData[column].numberData[row] := value;
end;

procedure SkelSetListProcs(theList: SkelList; select: SelectProc; edit: EditProc);
begin
	theList.editCallback := edit;
	theList.selectedCallback := select;
end;

// Hide header by passing 0 to this
procedure SkelSetListHeaderHeight(theList: SkelList; height: Longint);
begin
	theList.theList.headerView.setFrameSize(NSMakeSize(theList.theList.frame.size.width,height));
end;

function SkelGetListSelect(theList: SkelList): Longint;
begin
	result := theList.theList.selectedRow;
	// Add support for multiple selection?
end;

procedure SkelSetListSelect(theList: SkelList; selection: Longint);
begin
// How?
end;

end.

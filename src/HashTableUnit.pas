// Simple hash table unit. The key value is always a string, while each
// entry can contain a string and/or an integer.
// By Ingemar 2012.


// Some thoughts while developing:
// What should I add beyond the obvious?
// Dynamic resizing on overflow! OK!
// Multiple type support? Integer + strings (could be anything - pointer?) OK!
// Seems pretty OK...

unit HashTableUnit;
interface

type
	HashRec = record
		hashTableKeys: array of AnsiString; // Keys
		hashTableValues: array of Longint; // Values
		hashTableStringValues: array of AnsiString; // String values
		hashMembers: Longint; // Number of occupied spaces
		failureValue: Integer; failureString: AnsiString; // Values to return if lookup fails
	end;

// function HashGetIndex(fileName: AnsiString; hashLength: Longint): Longint;
procedure HashInitTable(var h: HashRec; failureValue: Integer; failureString: AnsiString; hashLength: Longint);overload;
procedure HashInitTable(var h: HashRec; failureValue: Integer; failureString: AnsiString);overload;
procedure HashInitTable(var h: HashRec);overload;
//procedure HashExpandTable(var h: HashRec);
procedure HashAddEntry(var h: HashRec; keyValue: AnsiString; intValue: Longint; stringValue: AnsiString); overload;
procedure HashAddEntry(var h: HashRec; keyValue: AnsiString; intValue: Longint); overload;
procedure HashAddEntry(var h: HashRec; keyValue: AnsiString; stringValue: AnsiString); overload;
procedure HashLookUp(var h: HashRec; keyValue: AnsiString; var intValue: Longint; var stringValue: AnsiString);
function HashLookUpInteger(var h: HashRec; keyValue: AnsiString): Longint;
function HashLookUpString(var h: HashRec; keyValue: AnsiString): AnsiString;
procedure HashDeleteEntry(var h: HashRec; keyValue: AnsiString);
procedure PrintHashTable(var h: HashRec);

implementation

const
	kHashLength = 1024;

function HashGetIndex(key: AnsiString; hashLength: Longint): Longint;
var
	i, index: Longint;
begin
	index := 0;
	// Generate hash index
	for i := 1 to Length(key) do
	begin
		index := index * 2 + Ord(key[i]);
	end;
//	WriteLn('Start index before mod for ', key, ', ', index);
	index := abs(index) mod hashLength;
//	WriteLn('Start index for ', key, ' is ', index);
	
	HashGetIndex := index;
end;

procedure HashInitTable(var h: HashRec; failureValue: Integer; failureString: AnsiString; hashLength: Longint);overload;
var
	i: Longint;
begin
	h.hashMembers := 0;
	SetLength(h.hashTableKeys, hashLength);
	SetLength(h.hashTableValues, hashLength);
	SetLength(h.hashTableStringValues, hashLength);
	for i := 0 to High(h.hashTableKeys) do
	begin
		h.hashTableKeys[i] := ''; // Empty key = free space
		h.hashTableValues[i] := 0;
		h.hashTableStringValues[i] := '';
	end;
	h.failureValue := failureValue;
	h.failureString := failureString;
end;

procedure HashInitTable(var h: HashRec; failureValue: Integer; failureString: AnsiString);overload;
begin
	HashInitTable(h, failureValue, failureString, kHashLength);
end;

procedure HashInitTable(var h: HashRec);overload;
begin
	HashInitTable(h, 0, '', kHashLength);
end;

procedure HashExpandTable(var h: HashRec);
var
	i, l: Longint;
	h2: HashRec; // Temp table
begin
// Clear h2 but with double length
	l := Length(h.hashTableKeys);
	WriteLn('Expanding table to ', l*2);
	SetLength(h2.hashTableKeys, l * 2);
	SetLength(h2.hashTableValues, l * 2);
	SetLength(h2.hashTableStringValues, l * 2);
	for i := 0 to High(h2.hashTableKeys) do
	begin
		h2.hashTableKeys[i] := ''; // Empty key = free space
		h2.hashTableValues[i] := 0;
		h2.hashTableStringValues[i] := '';
	end;
	h2.hashMembers := 0;

// Take all entries in h and put in h2	
	for i := 0 to High(h.hashTableKeys) do
	begin
		if Length(h.hashTableKeys[i]) > 0 then
		begin
			HashAddEntry(h2, h.hashTableKeys[i], h.hashTableValues[i], h.hashTableStringValues[i]);
		end;
	end;

// Copy h2 to h. Is it this easy?
	h := h2;
end;

procedure HashAddEntry(var h: HashRec; keyValue: AnsiString; intValue: Longint; stringValue: AnsiString); overload;
var
	index: Longint;
begin
	index := HashGetIndex(keyValue, Length(h.hashTableKeys));
	
	if h.hashMembers > Length(h.hashTableKeys) div 2 then
		HashExpandTable(h);
	
	while true do
	begin
		if (Length(h.hashTableKeys[index]) = 0) or (h.hashTableKeys[index] = keyValue) then {Ledig plats eller fanns redan, skall uppdateras}
		begin
			h.hashTableKeys[index] := keyValue;
			h.hashTableValues[index] := intValue;
			h.hashTableStringValues[index] := stringValue;
			h.hashMembers := h.hashMembers + 1;
			Exit;
		end;
		index := (index + 1) mod Length(h.hashTableKeys); // and kHashLength;
	end;
end;

procedure HashAddEntry(var h: HashRec; keyValue: AnsiString; intValue: Longint); overload;
var
	i: Longint;
begin
	HashAddEntry(h, keyValue, intValue, '');
	
	// TEST
	i := HashLookUpInteger(h, keyValue);
	if intValue <> i then
	begin
		WriteLn(keyValue, ' entered ', intValue, ' and got back ', i);
		WriteLn('***** ERROR!!! *****');
	end;
end;

procedure HashAddEntry(var h: HashRec; keyValue: AnsiString; stringValue: AnsiString); overload;
begin
	HashAddEntry(h, keyValue, 0, stringValue);
end;

procedure HashLookUp(var h: HashRec; keyValue: AnsiString; var intValue: Longint; var stringValue: AnsiString);
var
	index: Longint;
begin
	index := HashGetIndex(keyValue, Length(h.hashTableKeys));
	
// This should not happen but it does:
	if (index < 0) or (index >= Length(h.hashTableKeys)) then
	begin
		WriteLn('ERROR: index = ', index, ' length = ', Length(h.hashTableKeys), ' for key value ', keyValue);
		intValue := h.failureValue;
		stringValue := h.failureString;
		Exit;
	end;
	
	while true do
	begin
		if Length(h.hashTableKeys[index]) = 0 then {Ledig plats - fanns inte}
		begin
//			return kOtherToken; {Fanns inte i listan innan!}
			intValue := h.failureValue;
			stringValue := h.failureString;
			Exit;
		end;
		if h.hashTableKeys[index] = keyValue then
		begin
//			return h.hashTableValues[index];
			intValue := h.hashTableValues[index];
			stringValue := h.hashTableStringValues[index];
			Exit;
		end;
		index := (index + 1) mod Length(h.hashTableKeys); // kHashLength;
		// eller and med Length(h.hashTableKeys)-1 för lite bättre fart?
	end;
end;

function HashLookUpInteger(var h: HashRec; keyValue: AnsiString): Longint;
var
	intValue: Longint;
	stringValue: AnsiString;
begin
	HashLookUp(h, keyValue, intValue, stringValue);
	HashLookUpInteger := intValue;
end;

function HashLookUpString(var h: HashRec; keyValue: AnsiString): AnsiString;
var
	intValue: Longint;
	stringValue: AnsiString;
begin
	HashLookUp(h, keyValue, intValue, stringValue);
	HashLookUpString := stringValue;
end;

procedure HashDeleteEntry(var h: HashRec; keyValue: AnsiString);
// May be nontrivial if someone has skipped the space
// Loop through all following entries until we find a space,
// test these against position of new space?
var
	index: Longint;
//	deletedIndex: Longint;
	tmpKeyValue: AnsiString;
	tmpIntValue: Longint;
	tmpStringValue: AnsiString;
begin
	index := HashGetIndex(keyValue, Length(h.hashTableKeys));
	
	if h.hashMembers > Length(h.hashTableKeys) div 2 then
		HashExpandTable(h);
	
	while true do
	begin
		if h.hashTableKeys[index] = keyValue then
		begin
//			deletedIndex := index;
			h.hashTableKeys[index] := '';
			h.hashTableValues[index] := 0;
			h.hashTableStringValues[index] := '';
			h.hashMembers := h.hashMembers - 1;
			break;
		end;
		index := (index + 1) mod Length(h.hashTableKeys);
	end;
	
	// Sök framåt till nästa lediga. Finns någon i den sekvensen som hellre vill ha den nu lediga?
	// MEN tar vi bort den upprepar sig problemet!
	// Enkel lösning? Delete på alla i sekvens efter och lägg in dem igen? Rekursivt borde väl funka?

	index := (index + 1) mod Length(h.hashTableKeys);
	while true do
	begin
		if Length(h.hashTableKeys[index]) = 0 then {Ledig plats}
			Exit;
		begin
			tmpKeyValue := h.hashTableKeys[index];
			tmpIntValue := h.hashTableValues[index];
			tmpStringValue := h.hashTableStringValues[index];
			HashDeleteEntry(h, tmpKeyValue);
			HashAddEntry(h, tmpKeyValue, tmpIntValue, tmpStringValue);
		end;
		index := (index + 1) mod Length(h.hashTableKeys);
	end;
	
end;

procedure PrintHashTable(var h: HashRec);
var
	i: Longint;
begin
	for i := 0 to High(h.hashTableKeys) do
	begin
		if Length(h.hashTableKeys[i]) = 0 then
			WriteLn('-')
		else
			WriteLn(h.hashTableKeys[i], ': ', h.hashTableValues[i]:1, ', "', h.hashTableStringValues[i], '"')
	end;
end;

end.

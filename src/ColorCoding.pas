// Lightweight Pascal IDE, color coding and pop-up menu unit
// © Ingemar Ragnemalm 2006-2013

// 0904??: RW added new symbols in Pascal coder.
// 090416: Many comments changed to english. Some old junk removed.
// 090418: SH color coder added.
// 090510: Reworked the C color coder and BuildMenuFromUses. Frameworks are extracted
// and uses is generated to an array.
// 120221: Added CheckSpecialComment, adds a linklib/linkframework feature for C
// 121106: Replaced hash functions by the improved and isolated HashTableUnit.
// 121109: Added support for individual styles in color schemes
// 121115: Prototype for auto-formatting of Pascal code.
// 131113: Introduced SnapShot for storing CC data, allowing for threaded CC, color coding a large file in small steps

// Uncomment this to compile with auto-formatter
//{$define LW_PASCAL_FORMATTER}

{$mode macpas} 
unit ColorCoding;
interface

	uses
		MacOSAll, LWPGlobals, FileUtils, Settings, TXNUtilities,
		LWPColorGlobals, UtilsTypes, baseunix, AboutWindowUnit, ProcessUtils,
		HashTableUnit, UndoUnit, TransSkel4, Halda, HaldaTypes, HaldaSysDept, ClassParser;
	
//procedure ColorCodeThis(teRec: TXNObject);
procedure ResetColorCoder(teRec: HaldaPtr; editIndex: Integer);
procedure TouchColorCoder(editIndex, pos: Longint);
procedure InitColorCodingTables; 
function GetTokenInCase(teRec: HaldaPtr; tokenStart, tokenEnd: Longint):AnsiString;

function BuildMenuFromUses(teWind: WindowPtr): MenuHandle;

//function UpdateRowFromCharOffset(editIndex, pos: Longint): Longint;

//type
//	StrArr = array of Str255; // OBSOLETE
function GetUsesListFromText(data: AnsiString; bufferLength: Longint; theTitle: Str255): AnsiStringArray;



// TEST:
//procedure PascalFormatter(teRec: TXNObject; editIndex: Integer);
// Temporarily disabled

// For other units using the parser here:

// Token types
const
	kFunctionToken = 1;		// function, procedure
	kBeginEndToken = 2;		// begin/end/if/else/case/of - anything that affects structure)
	kReservedToken = 3;		// Reserved words that do NOT affect structure analysis
	kKnownTypeToken = 4;	// Integer, Longint, Real...
	kCommentToken = 5;		// {} (* *) // Can span several rows!
	kStringToken = 6;		// 'string' "c"
	kSpecialToken = 7;		// ( ) * / + - etc
	kSingleCharToken = 8;	// Any unknown token (identifiers etc)
	kOtherToken = 9;				// Any unknown token (identifiers etc)
	kPreprocessorToken = 10;// For the C parser
	kStartParenToken = 11;	// For the C parser
	kEndParenToken = 12;		// For the Java parser
	kClassToken = 13;		// For the Java parser
	kLibraryToken = 14;		// For Library functions
	kCarbonToken = 15;		// For Apple Carbon functions
	kCocoaToken = 16;		// For Apple Cocoa functions
	kCompilerDirectiveToken = 17;	// For compiler directives
	kCRLFToken = 18;			// At this time only used in code formatter
	kLinkageToken = 19;
	
	kObjectToken=50; // For kKnownClassNames, which will be used for object recognization in C coding.
	
var
classType:LongInt;

// Hash tables for color coding
var
	pascalHashTable, cHashTable, javaHashTable, shHashTable, adaHashTable: HashRec;

//procedure GetToken(dataPtr: CharsPtr; bufferLength: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);
procedure GetToken(data: AnsiString; bufferLength: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);

procedure GetCToken(data: AnsiString; bufferLength: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);Overload;
procedure GetCToken(data: AnsiString; bufferLength, editIndex: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);Overload;
procedure FinishCBrackets(chars:AnsiString; var pos:LongInt; bracketCount, untilHere, bufferLength:LongInt);

type
	SnapShot = record
		pos: Longint;
		bufferLength: Longint;
		// Pascal (much of this is also used by others)
		hasSeenProgram, hasSeenUnit, hasSeenInterface, hasSeenImplementation: Boolean;
		previousTokenType: Longint; {Ett steg tillbaka}
		previousTokenValue: AnsiString;
		secondPreviousTokenType: Longint; {Två steg tillbaka}
		secondPreviousTokenValue: AnsiString;
		// C
		currentParenIsFunction: Boolean;
		{hasSeenImplementation,} hasSeenObjCMethod{, hasSeenInterface}: Boolean;
		startOfObjCMethod: Longint; // Where did we seen + or - at top level?
		parenBalance: Longint; // () balance
		objCMethodName: AnsiString; 
		objCClassName: AnsiString;
		// Java
		possibleFunctionTokenStart, possibleFunctionTokenEnd: Longint;
		possibleFunctionTokenValue: AnsiString;
		possibleFunctionParenBalance: Longint;
		
		// Snapshots of these global lists
		usesList: AnsiStringArray;
		frameworkList: AnsiStringArray;
		// Copied out to the global list at end of update? Or just access the last?
		
		editFunctionNameStart: array of Longint;
		editFunctionNameEnd: array of Longint;
		editFunctionName: array of AnsiString;
		
		editGlobalFunctions: array of AnsiString;
		
		FunctionFileName: array of AnsiString;
		extraOptions: AnsiString;
		extraLinkOptions: AnsiString;

		editWindIsMain: Boolean;
		editMainWindIsProgram: Boolean;
		editWindIsCocoaProgram: Boolean;
		editWindIsiOSProgram: Boolean;
		editWindIsJVMProgram, editWindIsAndroidProgram: Boolean; // JVM targets added 20160303
									
		//Code Completion variables
   		lRecordVariables: Array of RecordVariables; 
   		lVariableMembers: VariableMembers;  		
   		lRecordPointers: array of RecordPointers;
   		functionParsing:Boolean;
		count:LongInt;
		SSfileName:AnsiString;
   		//lPointerVariableName, lPointerVariableType: array of AnsiString;
   		

		
// More info that should be in the SnapShot
//		editWindIsMain[editIndex] := false; // Added 130925
//		editMainWindIsProgram[editIndex] := false; // Added 130925 - means "applet or main" for Java.
	end; 
	
procedure FinishBracketsinLoops(chars:AnsiString; var pos:LongInt; bracketCount, untilHere, bufferLength:LongInt);

procedure SearchVariableTypeInPointersList(s:snapShot; editIndex:LongInt);

procedure InitSnapShot(var s: SnapShot; editIndex: Longint);

function SearchForInheritedObjInRVList(var s:snapshot; editIndex:LongInt; inheritedObjectName:AnsiString ):AnsiStringArray;


implementation

uses
	IncludeAnalyzer, LWPEdit, ClosedFilesDataManager;

const
	CR = Char(13);
	LF = Char(10);
	TAB = Char(9);
	
const
//	kMaxSizeForColorCoding = 1000000;
	kMaxLinesForColorCoding = 20000; // More than this and no color coding is applied - it would be too slow

//	kCCStepLength = 4096; // 65536; // 256; // Something like 16k
	kCCStepLength = 16384; // 256; // Something like 16k

var
// Main global data for color coding!
//		chars: array[1..kMaxEditWindows] of Handle; // nil = reload
		snap: array [1..kMaxEditWindows] of array of SnapShot;
// Data for line numbers
var
	lastRow: array[1..kMaxEditWindows] of Longint;
	positionOfLastRow: array[1..kMaxEditWindows] of Longint;
	
// Fix for stupid error caused by old Toolbox functions.
// LowerCaseText doesn't work above 16k!
// Note: For strings there is a "LowerCase" call.
procedure MyLowerCaseText(pt: Ptr; dataSize: Longint; ignored: Integer);
var
	i, s: Longint;
	p: CharsPtr;
begin
	p := CharsPtr(pt);
	s := dataSize-1;
	for i := 0 to s do
		if p^[i] in ['A'..'Z'] then
			p^[i] := Char(Ord(p^[i]) + 32);
//		p^[i] := Char((Ord(p^[i])>>1 and $20) or Ord(p^[i]));
end;

procedure InitSnapShot(var s: SnapShot; editIndex: Longint);
begin
	with s do
	begin
		pos := 1;
		
		hasSeenProgram := false;
		hasSeenUnit := false;
		hasSeenInterface := false;
		hasSeenImplementation := false;
		previousTokenType := 0;
		previousTokenValue := '';
		secondPreviousTokenType := 0;
		secondPreviousTokenValue := '';
		// C
		currentParenIsFunction := false;
		hasSeenObjCMethod := false;
		startOfObjCMethod := 0;
		parenBalance := 0;
		objCMethodName := '';
		objCClassName := '';
		// Java
		possibleFunctionTokenStart := 0;
		possibleFunctionTokenEnd := 0;
		possibleFunctionTokenValue := '';
		possibleFunctionParenBalance := 0;
		// Snapshots of these global lists
		usesList := nil;
		frameworkList := nil; 
		editFunctionNameStart := nil;
		editFunctionNameEnd := nil;
		editFunctionName := nil;
		FunctionFileName:= nil;
		editGlobalFunctions:= nil;
		SSfileName:='';
		lRecordVariables:= nil; 
   		lVariableMembers.variableName:= nil;  
   		lVariableMembers.variableScope:=nil;
   		lVariableMembers.variableType:=nil;
   		
   		lRecordPointers:= nil;
		{lRecordPointers.pointerVariableName:= nil;
		lRecordPointers.PointerVariableType:=nil;
		lRecordPointers.pointerToPointer:=false;}
		
		extraOptions := '';
		extraLinkOptions := '';
		
		editWindIsMain := false;
		editMainWindIsProgram := false;
		editWindIsCocoaProgram := false;
		editWindIsiOSProgram := false;
		editWindIsJVMProgram := false;
		editWindIsAndroidProgram := false; // JVM targets added 20160303
		
		functionParsing:=false;
		count:=0;
	end;

	with s do
	if teEdit[editIndex] <> nil then
	begin
//		if chars[editIndex] <> nil then
//			DisposeHandle(chars[editIndex]);
		
		// Get the text from the text field
//		{err :=} TXNGetDataEncoded(teEdit[editIndex], kTXNStartOffset, kTXNEndOffset, Handle(chars[editIndex]), kTXNTextData);
//		HLock(chars[editIndex]);
		
		// Change chars to lower case
//		MyLowercaseText(chars[editIndex]^, GetHandleSize(Handle(chars[editIndex])), smRoman);
		
		// Set the entire text to normal
		SetTextToNormal(teEdit[editIndex], true, GetSettingsTextSize); // and all black	
	end;
	
end;



{------------------------ PASCAL COLOR CODER ----------------------------}

procedure InitColorCodingTable;
begin
	HashAddEntry(pascalHashTable, 'program', kReservedToken);
	HashAddEntry(pascalHashTable, 'library', kReservedToken); // 110311
	HashAddEntry(pascalHashTable, 'exports', kReservedToken); // 110311
	HashAddEntry(pascalHashTable, 'array', kReservedToken);
	HashAddEntry(pascalHashTable, 'unit', kReservedToken);
	HashAddEntry(pascalHashTable, 'interface', kReservedToken);
	HashAddEntry(pascalHashTable, 'implementation', kReservedToken);
	HashAddEntry(pascalHashTable, 'initialization', kReservedToken); // Ingemar 090629
	HashAddEntry(pascalHashTable, 'finalization', kReservedToken); // Ingemar 110424
	HashAddEntry(pascalHashTable, 'const', kReservedToken);
	HashAddEntry(pascalHashTable, 'type', kReservedToken);
	HashAddEntry(pascalHashTable, 'var', kReservedToken);
	HashAddEntry(pascalHashTable, 'record', kReservedToken);
	HashAddEntry(pascalHashTable, 'uses', kReservedToken);

	HashAddEntry(pascalHashTable, 'of', kReservedToken);
	HashAddEntry(pascalHashTable, 'set', kReservedToken);
	HashAddEntry(pascalHashTable, 'variant', kReservedToken);
	HashAddEntry(pascalHashTable, 'packed', kReservedToken);
	HashAddEntry(pascalHashTable, 'overload', kReservedToken);
	HashAddEntry(pascalHashTable, 'class', kReservedToken);
	HashAddEntry(pascalHashTable, 'object', kReservedToken);
	HashAddEntry(pascalHashTable, 'objcclass', kReservedToken); // 131105
	
	HashAddEntry(pascalHashTable, 'abstract', kClassToken);
	HashAddEntry(pascalHashTable, 'generic', kClassToken);
	//HashAddEntry(pascalHashTable, 'object', kClassToken);
	//HashAddEntry(pascalHashTable, 'class', kClassToken); // 131105
	//HashAddEntry(pascalHashTable, 'objcclass', kClassToken); // 131105
	HashAddEntry(pascalHashTable, 'virtual', kClassToken); 
	HashAddEntry(pascalHashTable, 'override', kClassToken); 
	HashAddEntry(pascalHashTable, 'reintroduce', kClassToken); 
	HashAddEntry(pascalHashTable, 'public', kBeginEndToken); // 121113 kBeginEndToken for formatting? kClassToken
	HashAddEntry(pascalHashTable, 'private', kBeginEndToken); // 121113 kBeginEndToken for formatting? kClassToken
	HashAddEntry(pascalHashTable, 'protected', kClassToken); 
	HashAddEntry(pascalHashTable, 'published', kClassToken); 
	HashAddEntry(pascalHashTable, 'as', kClassToken); 
	HashAddEntry(pascalHashTable, 'is', kClassToken); 

	HashAddEntry(pascalHashTable, 'and', kSpecialToken); 
	HashAddEntry(pascalHashTable, 'or', kSpecialToken); 
	HashAddEntry(pascalHashTable, 'not', kSpecialToken); 
	HashAddEntry(pascalHashTable, 'xor', kSpecialToken); 
	HashAddEntry(pascalHashTable, 'in', kSpecialToken);
	HashAddEntry(pascalHashTable, 'div', kSpecialToken);
	HashAddEntry(pascalHashTable, 'mod', kSpecialToken);
	HashAddEntry(pascalHashTable, 'nil', kSpecialToken);
	HashAddEntry(pascalHashTable, 'true', kSpecialToken);
	HashAddEntry(pascalHashTable, 'false', kSpecialToken);

	HashAddEntry(pascalHashTable, 'procedure', kFunctionToken);
	HashAddEntry(pascalHashTable, 'function', kFunctionToken);
	HashAddEntry(pascalHashTable, 'operator', kFunctionToken); // 110314
	HashAddEntry(pascalHashTable, 'constructor', kFunctionToken);
	HashAddEntry(pascalHashTable, 'destructor', kFunctionToken);

	HashAddEntry(pascalHashTable, 'begin', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'end', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'end.', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'if', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'then', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'else', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'case', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'otherwise', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'with', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'downto', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'for', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'to', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'do', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'while', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'repeat', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'until', kBeginEndToken); 
	HashAddEntry(pascalHashTable, 'goto', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'try', kBeginEndToken); // Ingemar 090629
	HashAddEntry(pascalHashTable, 'except', kBeginEndToken); // Ingemar 090629

	HashAddEntry(pascalHashTable, 'exit', kBeginEndToken);	// these are actually system library routines
	HashAddEntry(pascalHashTable, 'halt', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'runerror', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'assert', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'break', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'continue', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'longjump', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'setjump', kBeginEndToken);
	HashAddEntry(pascalHashTable, 'return', kBeginEndToken); 
	
	HashAddEntry(pascalHashTable, 'pointer', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'longint', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'integer', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'real', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'char', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'shortint', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'smallInt', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'longword', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'int64', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'byte', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'word', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'cardinal', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'qword', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'boolean', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'bytebool', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'longbool', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'single', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'double', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'extended', kKnownTypeToken); 
	HashAddEntry(pascalHashTable, 'comp', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'currency', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'string', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'ansistring', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'widestring', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'pchar', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'file', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'dword', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'uint64', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'widechar', kKnownTypeToken);
	HashAddEntry(pascalHashTable, 'wchar', kKnownTypeToken);
		
//	HashAddEntry(pascalHashTable, '(', kSingleCharToken);	// these are hardwired
															// - because they are not alphanumeric /Ingemar
//	HashAddEntry(pascalHashTable, ')', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '[', kSingleCharToken);
//	HashAddEntry(pascalHashTable, ']', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '+', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '-', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '*', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '/', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '@', kSingleCharToken);
//	HashAddEntry(pascalHashTable, ':=', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '+=', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '-=', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '/=', kSingleCharToken);
//	HashAddEntry(pascalHashTable, '*=', kSingleCharToken);
		
	HashAddEntry(pascalHashTable, 'tclass', kLibraryToken); 
	HashAddEntry(pascalHashTable, 'tobject', kLibraryToken); 
	HashAddEntry(pascalHashTable, 'tcollection', kLibraryToken); 
	HashAddEntry(pascalHashTable, 'create', kLibraryToken); 
	HashAddEntry(pascalHashTable, 'destroy', kLibraryToken); 
	HashAddEntry(pascalHashTable, 'free', kLibraryToken); 
	HashAddEntry(pascalHashTable, 'freeandnil', kLibraryToken); 
	
	HashAddEntry(pascalHashTable, 'text', kLibraryToken);
	HashAddEntry(pascalHashTable, 'append', kLibraryToken);
	HashAddEntry(pascalHashTable, 'assign', kLibraryToken);
	HashAddEntry(pascalHashTable, 'blockread', kLibraryToken);
	HashAddEntry(pascalHashTable, 'blockwrite', kLibraryToken);
	HashAddEntry(pascalHashTable, 'close', kLibraryToken);
	HashAddEntry(pascalHashTable, 'eof', kLibraryToken);
	HashAddEntry(pascalHashTable, 'eoln', kLibraryToken);
	HashAddEntry(pascalHashTable, 'erase', kLibraryToken);
	HashAddEntry(pascalHashTable, 'filepos', kLibraryToken);
	HashAddEntry(pascalHashTable, 'filesize', kLibraryToken);
	HashAddEntry(pascalHashTable, 'flush', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ioresult', kLibraryToken);
	HashAddEntry(pascalHashTable, 'read', kLibraryToken);
	HashAddEntry(pascalHashTable, 'readln', kLibraryToken);
	HashAddEntry(pascalHashTable, 'rename', kLibraryToken);
	HashAddEntry(pascalHashTable, 'reset', kLibraryToken);
	HashAddEntry(pascalHashTable, 'rewrite', kLibraryToken);
	HashAddEntry(pascalHashTable, 'seek', kLibraryToken);
	HashAddEntry(pascalHashTable, 'seekeof', kLibraryToken);
	HashAddEntry(pascalHashTable, 'seekeoln', kLibraryToken);
	HashAddEntry(pascalHashTable, 'settextbuf', kLibraryToken);
	HashAddEntry(pascalHashTable, 'truncate', kLibraryToken);
	HashAddEntry(pascalHashTable, 'write', kLibraryToken); 
	HashAddEntry(pascalHashTable, 'writeln', kLibraryToken);

	HashAddEntry(pascalHashTable, 'chdir', kLibraryToken);
	HashAddEntry(pascalHashTable, 'getdir', kLibraryToken);
	HashAddEntry(pascalHashTable, 'paramcount', kLibraryToken);
	HashAddEntry(pascalHashTable, 'paramstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'mkdir', kLibraryToken);
	HashAddEntry(pascalHashTable, 'rmdir', kLibraryToken);
	HashAddEntry(pascalHashTable, 'rewrite', kLibraryToken);

	HashAddEntry(pascalHashTable, 'abs', kLibraryToken);
	HashAddEntry(pascalHashTable, 'arctan', kLibraryToken);
	HashAddEntry(pascalHashTable, 'cos', kLibraryToken);
	HashAddEntry(pascalHashTable, 'dec', kLibraryToken);
	HashAddEntry(pascalHashTable, 'exp', kLibraryToken);
	HashAddEntry(pascalHashTable, 'frac', kLibraryToken);
	HashAddEntry(pascalHashTable, 'hi', kLibraryToken);
	HashAddEntry(pascalHashTable, 'lo', kLibraryToken);
	HashAddEntry(pascalHashTable, 'odd', kLibraryToken);
	HashAddEntry(pascalHashTable, 'pi', kLibraryToken);
	HashAddEntry(pascalHashTable, 'power', kLibraryToken);
 	HashAddEntry(pascalHashTable, 'random', kLibraryToken);
	HashAddEntry(pascalHashTable, 'randomize', kLibraryToken);
	HashAddEntry(pascalHashTable, 'round', kLibraryToken);
	HashAddEntry(pascalHashTable, 'sin', kLibraryToken);
	HashAddEntry(pascalHashTable, 'sqr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'sqrt', kLibraryToken);
	HashAddEntry(pascalHashTable, 'swap', kLibraryToken);
	HashAddEntry(pascalHashTable, 'trunc', kLibraryToken);
	
	HashAddEntry(pascalHashTable, 'ord', kLibraryToken);
	HashAddEntry(pascalHashTable, 'pred', kLibraryToken);
	HashAddEntry(pascalHashTable, 'succ', kLibraryToken);
	HashAddEntry(pascalHashTable, 'sizeof', kLibraryToken);
	HashAddEntry(pascalHashTable, 'sqrt', kLibraryToken);
	HashAddEntry(pascalHashTable, 'include', kLibraryToken);
	HashAddEntry(pascalHashTable, 'exclude', kLibraryToken);
	
	HashAddEntry(pascalHashTable, 'binstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'chr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'concat', kLibraryToken);
	HashAddEntry(pascalHashTable, 'copy', kLibraryToken);
	HashAddEntry(pascalHashTable, 'delete', kLibraryToken);
	HashAddEntry(pascalHashTable, 'hexstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'insert', kLibraryToken);
	HashAddEntry(pascalHashTable, 'length', kLibraryToken);
	HashAddEntry(pascalHashTable, 'lowercase', kLibraryToken);
	HashAddEntry(pascalHashTable, 'octstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'pos', kLibraryToken);
	HashAddEntry(pascalHashTable, 'setlength', kLibraryToken);
	HashAddEntry(pascalHashTable, 'setstring', kLibraryToken);
	HashAddEntry(pascalHashTable, 'str', kLibraryToken);
	HashAddEntry(pascalHashTable, 'stringofchar', kLibraryToken);
	HashAddEntry(pascalHashTable, 'upcase', kLibraryToken);
	HashAddEntry(pascalHashTable, 'val', kLibraryToken);
	

	HashAddEntry(pascalHashTable, 'worddelimiters', kLibraryToken);
	
	HashAddEntry(pascalHashTable, 'addchar', kLibraryToken);
	HashAddEntry(pascalHashTable, 'addcharr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansicontainsstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansicontainstext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansiendsstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansiendstext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansiindexstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansiindextext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansileftstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansimatchstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansimatchtext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansimidstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansipropercase', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansireplacestr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansireplacetext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansiresemblestext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansireversestring', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansirightstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansistartsstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansistartstext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'bintohex', kLibraryToken);
	HashAddEntry(pascalHashTable, 'copy2space', kLibraryToken);
	HashAddEntry(pascalHashTable, 'copy2spacedel', kLibraryToken);
	HashAddEntry(pascalHashTable, 'copy2symb', kLibraryToken);
	HashAddEntry(pascalHashTable, 'copy2symbdel', kLibraryToken);
	HashAddEntry(pascalHashTable, 'dec2numb', kLibraryToken);
	HashAddEntry(pascalHashTable, 'decodesoundexint', kLibraryToken);
	HashAddEntry(pascalHashTable, 'decodesoundexword', kLibraryToken);
	HashAddEntry(pascalHashTable, 'delchars', kLibraryToken);
	HashAddEntry(pascalHashTable, 'delspace', kLibraryToken);
	HashAddEntry(pascalHashTable, 'delspace1', kLibraryToken);
	HashAddEntry(pascalHashTable, 'dupestring', kLibraryToken);
	HashAddEntry(pascalHashTable, 'extractdelimited', kLibraryToken);
	HashAddEntry(pascalHashTable, 'extractsubstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'extractword', kLibraryToken);
	HashAddEntry(pascalHashTable, 'extractwordpos', kLibraryToken);
	HashAddEntry(pascalHashTable, 'findpart', kLibraryToken);
	HashAddEntry(pascalHashTable, 'getcmdlineArg', kLibraryToken);
	HashAddEntry(pascalHashTable, 'hex2dec', kLibraryToken);
	HashAddEntry(pascalHashTable, 'hextobin', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ifthen', kLibraryToken);
	HashAddEntry(pascalHashTable, 'inttobin', kLibraryToken);
	HashAddEntry(pascalHashTable, 'inttoroman', kLibraryToken);
	HashAddEntry(pascalHashTable, 'isemptystr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'iswild', kLibraryToken);
	HashAddEntry(pascalHashTable, 'iswordpresent', kLibraryToken);
	HashAddEntry(pascalHashTable, 'leftbstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'leftstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'midbstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'midstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'npos', kLibraryToken);
	HashAddEntry(pascalHashTable, 'numb2dec', kLibraryToken);
	HashAddEntry(pascalHashTable, 'numb2UsA', kLibraryToken);
	HashAddEntry(pascalHashTable, 'padcenter', kLibraryToken);
	HashAddEntry(pascalHashTable, 'padleft', kLibraryToken);
	HashAddEntry(pascalHashTable, 'padright', kLibraryToken);
	HashAddEntry(pascalHashTable, 'posex', kLibraryToken);
	HashAddEntry(pascalHashTable, 'posset', kLibraryToken);
	HashAddEntry(pascalHashTable, 'possetex', kLibraryToken);
	HashAddEntry(pascalHashTable, 'randomfrom', kLibraryToken);
	HashAddEntry(pascalHashTable, 'removeleadingchars', kLibraryToken);
	HashAddEntry(pascalHashTable, 'removepadchars', kLibraryToken);
	HashAddEntry(pascalHashTable, 'removetrailingchars', kLibraryToken);
	HashAddEntry(pascalHashTable, 'reversestring', kLibraryToken);
	HashAddEntry(pascalHashTable, 'rightbstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'rightstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'romantoint', kLibraryToken);
	HashAddEntry(pascalHashTable, 'rpos', kLibraryToken);
	HashAddEntry(pascalHashTable, 'rposex', kLibraryToken);
	HashAddEntry(pascalHashTable, 'searchbuf', kLibraryToken);
	HashAddEntry(pascalHashTable, 'soundex', kLibraryToken);
	HashAddEntry(pascalHashTable, 'soundexcompare', kLibraryToken);
	HashAddEntry(pascalHashTable, 'soundexint', kLibraryToken);
	HashAddEntry(pascalHashTable, 'soundexproc', kLibraryToken);
	HashAddEntry(pascalHashTable, 'soundexsimilar', kLibraryToken);
	HashAddEntry(pascalHashTable, 'soundexword', kLibraryToken);
	HashAddEntry(pascalHashTable, 'stringsreplace', kLibraryToken);
	HashAddEntry(pascalHashTable, 'stuffstring', kLibraryToken);
	HashAddEntry(pascalHashTable, 'tab2space', kLibraryToken);
	HashAddEntry(pascalHashTable, 'trimleftset', kLibraryToken);
	HashAddEntry(pascalHashTable, 'trimrightset', kLibraryToken);
	HashAddEntry(pascalHashTable, 'trimset', kLibraryToken);
	HashAddEntry(pascalHashTable, 'wordcount', kLibraryToken);
	HashAddEntry(pascalHashTable, 'wordposition', kLibraryToken);
	HashAddEntry(pascalHashTable, 'xordecode', kLibraryToken);
	HashAddEntry(pascalHashTable, 'xorencode', kLibraryToken);
	HashAddEntry(pascalHashTable, 'xorstring', kLibraryToken);


	HashAddEntry(pascalHashTable, 'bcdtoint', kLibraryToken);
	HashAddEntry(pascalHashTable, 'comparemem', kLibraryToken);
	HashAddEntry(pascalHashTable, 'floattostrf', kLibraryToken);
	HashAddEntry(pascalHashTable, 'floattostr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'floattotext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'formatfloat', kLibraryToken);
	HashAddEntry(pascalHashTable, 'getdirs', kLibraryToken);
	HashAddEntry(pascalHashTable, 'inttohex', kLibraryToken);
	HashAddEntry(pascalHashTable, 'inttostr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strtointdef', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strtoint', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strtofloat', kLibraryToken);
	HashAddEntry(pascalHashTable, 'texttofloat', kLibraryToken);

	HashAddEntry(pascalHashTable, 'ansicomparestr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansicomparetext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansiextractquotedstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansilastchar', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansilowercase', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansiquotedstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansistrcomp', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansistricomp', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansistrlcomp', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansistrlicomp', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansistrlastchar', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansistrlower', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansistrupper', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ansiuppercase', kLibraryToken);
	HashAddEntry(pascalHashTable, 'appendstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'assignstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'comparestr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'comparetext', kLibraryToken);
	HashAddEntry(pascalHashTable, 'disposestr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'isvalidident', kLibraryToken);
	HashAddEntry(pascalHashTable, 'lastdelimiter', kLibraryToken);
	HashAddEntry(pascalHashTable, 'leftstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'loadstr', kLibraryToken);
//	HashAddEntry(pascalHashTable, 'lowercase', kLibraryToken);
	HashAddEntry(pascalHashTable, 'newstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'rightstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'stralloc', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strbufsize', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strdispose', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strpas', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strpcopy', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strplcopy', kLibraryToken);
	HashAddEntry(pascalHashTable, 'uppercase', kLibraryToken);
	
	HashAddEntry(pascalHashTable, 'adjustlinebreaks', kLibraryToken);
	HashAddEntry(pascalHashTable, 'formatbuf', kLibraryToken);
	HashAddEntry(pascalHashTable, 'format', kLibraryToken);
	HashAddEntry(pascalHashTable, 'fmtstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'quotedstr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strfmt', kLibraryToken);
	HashAddEntry(pascalHashTable, 'strlfmt', kLibraryToken);
	HashAddEntry(pascalHashTable, 'trimleft', kLibraryToken);
	HashAddEntry(pascalHashTable, 'trimright', kLibraryToken);
	HashAddEntry(pascalHashTable, 'trim', kLibraryToken);	
 
	HashAddEntry(pascalHashTable, 'new', kLibraryToken);
	HashAddEntry(pascalHashTable, 'dispose', kLibraryToken);
	HashAddEntry(pascalHashTable, 'move', kLibraryToken);
	HashAddEntry(pascalHashTable, 'fillbyte', kLibraryToken);
	HashAddEntry(pascalHashTable, 'fillchar', kLibraryToken);
	HashAddEntry(pascalHashTable, 'movechar', kLibraryToken);
	HashAddEntry(pascalHashTable, 'ofs', kLibraryToken);
	HashAddEntry(pascalHashTable, 'sptr', kLibraryToken);
	HashAddEntry(pascalHashTable, 'fillbyte', kLibraryToken);
	
	HashAddEntry(pascalHashTable, 'maxint', kLibraryToken);
	HashAddEntry(pascalHashTable, 'maxsmallint', kLibraryToken);
	HashAddEntry(pascalHashTable, 'maxlongint', kLibraryToken);
	HashAddEntry(pascalHashTable, 'maxuintvalue', kLibraryToken);
	
	HashAddEntry(pascalHashTable, 'macosall', kCarbonToken);
	HashAddEntry(pascalHashTable, 'fpcmacosall', kCarbonToken);
	HashAddEntry(pascalHashTable, 'math', kCarbonToken);
	HashAddEntry(pascalHashTable, 'transskel4', kCarbonToken);
	HashAddEntry(pascalHashTable, 'transskel', kCarbonToken);
	HashAddEntry(pascalHashTable, 'carbonstandardfile', kCarbonToken);
	HashAddEntry(pascalHashTable, 'transdisplay', kCarbonToken);
	HashAddEntry(pascalHashTable, 'viewmanager', kCarbonToken);
end;


procedure GetToken(data: AnsiString; bufferLength: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);
var
	s: ansistring;
begin
	
	//pick previous and secondPreviousTokenValue/Type
	secondPrevTokenValue:= prevTokenValue;
	secondPrevTokenType:= prevTokenType;
	secondPrevTokenStart:= prevTokenStart;
	secondPrevTokenEnd:= prevTokenEnd;
	
	prevTokenValue:= tokenValue;
	prevTokenType:= tokenType;
	prevTokenStart:= tokenStart;
	prevTokenEnd:= tokenEnd;

	
	//writeln('*previousTokenValue*', prevTokenValue);
	
	// Set defaults (150111)
	tokenValue := '';
	tokenType := kOtherToken;
	tokenStart := pos;
	tokenEnd := pos;

	// Guard against bad input
	if Length(data) <> bufferLength then
	begin
		pos := bufferLength + 1;
		Exit;
	end;
	if pos >= bufferLength then
		Exit;

	while (data[pos] in [CR, LF, TAB, ' ']) and (pos < bufferLength) do pos := pos + 1;
	tokenStart := pos;
	if data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
	begin
{Change 070326: Added '.' so Object Pascal code will get methods nicely listed in the function menu}
		while (data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.']) and (pos < bufferLength) do pos := pos + 1;
		tokenEnd := pos - 1;
		{Find out what alphanumerical symbol it is by looking it up in the hash table!}
//		SetLength(s, pos - tokenStart);
//		BlockMove(@data[tokenStart], @s[1], pos - tokenStart);
		// MoveBytes
//		WriteLn('*** Old s = "', s, '"');
		s := Copy(data, tokenStart, pos - tokenStart);
//		WriteLn('*** New s = "', s, '"');
		s := LowerCase(s);
//		WriteLn('*** New LC s = ', s);
		
		tokenType := HashLookUpInteger(pascalHashTable, s);
		tokenValue := s;
	end
	else
	begin {strings, comments, compiler directives, math operators, brackets, & other singletons }
	
		if (data[pos] = '{') and (data[pos+1] = '$') then // compiler directive
		begin
			pos := pos + 2;
			while (data[pos] <> '}') and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos;
			pos := pos+1;
			tokenType := kCompilerDirectiveToken;
		end
		else
		if data[pos] = '{' then // Bracket-comment
		begin
			while (data[pos] <> '}') and (pos < bufferLength) do pos := pos + 1;
			pos := pos+1;
			tokenEnd := pos-1;
			tokenType := kCommentToken;
			//tokenValue := 'comment';
//			pos := pos + 1; // Test, try to avoid the last character to be caught by someone else
			// NOTE! This is most likely needed for other cases!
		end
		else
		if (data[pos] = '/') and (data[pos+1] = '/') then // Line-comment
		begin
			while not (data[pos] in [CR, LF]) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos - 1;
			tokenType := kCommentToken;
		end
		else
		if (data[pos] = '(') and (data[pos+1] = '*') then // Block-comment
		begin
			pos := pos + 2;
			while ((data[pos-1] <> '*') or (data[pos] <> ')')) and (pos < bufferLength) do pos := pos + 1;
	pos := pos+1;
			tokenEnd := pos-1;
			tokenType := kCommentToken;
//			pos := pos + 1; // Test, try to avoid the last character to be caught by someone else
			// NOTE! This is most likely needed for other cases!
		end
		else
		if (data[pos] = '''') then // String
		begin
// Funkar INTE - varför?
			pos := pos + 1;
//			while (data[pos] <> '''') and (pos < bufferLength) do pos := pos + 1;
			while not (data[pos] in ['''', CR, LF]) and (pos < bufferLength) do
			begin
				//WriteLn(data[pos], ' Char(', Ord(data[pos]), ') is not a quote, Char(', Ord(''''), ')');
				pos := pos + 1;
			end;
			//WriteLn('Found a quote, end of comment');
			// Borde också ha stöd för '' i sträng
			tokenEnd := pos;
			pos := pos + 1; // Skip '
			tokenType := kStringToken;
			// Should we set tokenValue here?
		end
		else
		begin
		// Otherwise skip the symbol
		// Must do special case for most symbols! Or assume that they are single char!
		//	if data[pos] in ['+','-', ':', '=', '<', '>', '/', '*'] then // Single char token
		//	begin
		//	end;
		tokenEnd := tokenStart;
//				tokenEnd := pos;
				tokenType := kSingleCharToken;
				tokenValue := data[pos]; // Ord(data[pos]);
				//writeln('SingleChar token: ', tokenValue);
				pos := pos + 1;
			//while not (dataPtr in ['a'..'z', 'A'..'Z', '0'..'9', '_', CR, LF, ' ']) do pos := pos + 1;
		end;
	end;
end;

{procedure GetToken(data: AnsiString; bufferLength: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);overload;
begin
	GetToken(data, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue, true);
end;}


// The Pascal coder works in lower case. This routine fetches a token in original capitalization.
function GetTokenInCase(teRec: HaldaPtr; tokenStart, tokenEnd: Longint):AnsiString;
begin
	GetTokenInCase := Copy(teRec^.text, tokenStart, tokenEnd-tokenStart+1);
end;

procedure AddToEditFunctionArrays(var s: SnapShot; tokenValue: AnsiString; tokenStart, tokenEnd: Longint);
var
	iItemCount: Longint;
begin
	with s do
	begin
		iItemCount := Length(editFunctionName);
		if iItemCount < kMaxFunctionMenuItems then
		begin
			// Add to popup menu!
			// Get the function name in proper case!
			SetLength(editFunctionNameStart, Length(editFunctionName)+1);
			editFunctionNameStart[High(editFunctionNameStart)] := tokenStart;
			SetLength(editFunctionNameEnd, Length(editFunctionName)+1);
			editFunctionNameEnd[High(editFunctionNameEnd)] := tokenEnd;
			SetLength(editFunctionName, Length(editFunctionName)+1);
			editFunctionName[High(editFunctionName)] := tokenValue;
			
		end;
	end;
end;

//FinishBracketsinLoops parses from bracket open positon to bracket end position.
procedure FinishBracketsinLoops(chars:AnsiString; var pos:LongInt; bracketCount, untilHere, bufferLength:LongInt);
var
	pos2:LongInt;
	tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
	bracketFound:Boolean;
begin
	pos2:=pos;
	repeat
		repeat GetToken(chars, bufferLength, pos2, tokenStart, tokenEnd, tokenType, tokenValue);
		until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos2 >= bufferLength) or (pos2 >= untilHere); //Skip comments, kCRLF
		bracketFound:= true;
		//writeln('For: tokenValue: ', tokenValue, ' bracketCount: ', bracketCount);
		if tokenValue='(' then
		begin
			bracketCount+=1;
		end;

		if tokenValue=')' then
		begin	
			bracketCount-=1;
			if bracketCount<=0 then
				bracketFound:= false;
			
		end;

	until not (bracketFound) or (pos2 >= bufferLength) or (pos2 >= untilHere); 
	pos:=pos2;
end;

procedure SearchVariableTypeInPointersList(s:snapshot;editIndex:LongInt);//Rename SearchVariableTypeInPointersList
var
i:LongInt;
foundType:Boolean;
begin
	with s do
	begin
		with lVariableMembers do
		begin
		{SetLength(variabletype, Length(variableType)+1);
		if (prevTokenValue)='' then
			variableType[High(variableType)]:= secondPrevTokenValue
		else
			variableType[High(variableType)]:= prevTokenValue;}
			
			//Here variableType should be searched in pointerVariableName
			//If matched then append variableName by '^'											
			//First search in snapshot variables,
			foundType:=false;
			
			for i:=0 to High(lRecordPointers) do
			begin
				with lRecordPointers[i] do
					begin
					if (variableType[High(variableType)]=pointerVariableName) then
					begin
						if High(variableType) <= High(VariableName) then
							variableName[High(variableType)]:= variableName[High(variableType)] + '^';
						if pointerToPointer then
							if High(variableType) <= High(VariableName) then
								variableName[High(variableType)]:= variableName[High(variableType)] + '^';
					
						variableType[High(variableType)]:= pointerVariableType;
						//writeln('modified Var Name2: ', variableName[High(variableType)]);
						foundType:=true;
						break;
					end;
				end;
			end;
			if not foundType then
				for i:=0 to High(editRecordPointers[editIndex]) do
				begin
					if (variableType[High(variableType)]=editRecordPointers[editIndex][i].pointerVariableName) then
					begin
						if High(variableType) <= High(VariableName) then
							variableName[High(variableType)]:= variableName[High(variableType)] + '^';
						if editRecordPointers[editIndex][i].pointerToPointer then
							if High(variableType) <= High(VariableName) then
								variableName[High(variableType)]:= variableName[High(variableType)] + '^';
						variableType[High(variableType)]:= editRecordPointers[editIndex][i].pointerVariableType;
						//writeln('modified Var Name2: ', variableName[High(variableType)]);
						foundType:=true;
						break;
					end;
				end;
		end;
	end;
end; 

{procedure GetVariableList(chars:AnsiString; var s:SnapShot; tokenValue:AnsiString; editIndex:LongInt; var pos1:LongInt; untilHere, bufferLength: LongInt );
var
pos2:LongInt;
bracketFound:Boolean;
bracketCount:LongInt;
//count:LongInt=0;
gotVariableType :boolean = false;
begin
	with s do
	begin
		with lVariableMembers do
		begin
			//Skip brackets
			if (tokenValue= '(') then
			begin
				pos2:=pos1;
				
				bracketFound:= False;
				bracketCount:=0;
				bracketCount+=1;
				//writeln('bracketCount: ', bracketCount, ' pos: ', pos);
				FinishBracketsinLoops(chars, pos2, bracketCount, untilHere, bufferLength);
				pos1:=pos2;
			end;
				writeln('**High of VariableName: ', High(variableName));
				writeln('**High of Variabletype: ', High(variableType));
				writeln('**secondPrevTokenValue: ', secondPreviousTokenValue, ' prevTokenValue: ', previousTokenValue,' tokenValue: ', tokenValue);
			//pick variable names
			
				if (tokenValue=':') or (tokenValue=',') then
				begin
					if (tokenValue=',') then
					begin
						count+=1;
					end;
					SetLength(variableName, Length(variableName)+1);
					SetLength(variableScope, Length(variableScope)+1);
					if (prevTokenValue)='' then// (prevTokenValue)='' means a comment, in that case, variable name should be secondprevTokenValue
						variableName[High(variableName)]:= secondPreviousTokenValue//GetTokenInCase(teRec, secondPrevTokenStart, secondPrevTokenEnd)//secondPrevTokenValue //Should be replaced by GetTokenInCase(teRec, SecondPrevtokenStart, SecondPrevtokenEnd);
					else
						variableName[High(variableName)]:= previousTokenValue;//GetTokenInCase(teRec, prevtokenStart, prevtokenEnd);//prevTokenValue; //Should be replaced by GetTokenInCase(teRec, prevtokenStart, prevtokenEnd);
					//writeln('pTV: ', prevTokenValue);
					writeln('varibleName: ', variableName[High(variableName)]);
					writeln('secondPrevTokenValue: ', secondPreviousTokenValue, 'prevTokenValue: ', previousTokenValue);
					//Add scope here
					if not hasSeenImplementation then
						variableScope[High(variableScope)]:= 'global'
					else
						if not functionParsing then
							variableScope[High(variableScope)]:= 'local'
						else
							variableScope[High(variableScope)]:= editFunctionName[High(editFunctionName)];
						
					writeln('variableScope: ', variableScope[High(variableScope)]);
				end;
			
			
			if (tokenValue='=' ) then
			begin
				gotVariableType:=true;
				
				SetLength(variabletype, Length(variableType)+1);
				if (prevTokenValue)='' then
					variableType[High(variableType)]:= secondPreviousTokenValue
				else
					variableType[High(variableType)]:= previousTokenValue;
					
					
				SearchVariableTypeInPointersList(s, editIndex);
				writeln(' variableType: ', variableType[High(variableType)]);
				//writeln('gotVariableType: ', gotVariableType);
			end;                                         
			
			
			if (tokenValue=';') then
			begin
				if not gotVariableType then
				begin
					if count>0 then
					begin
						count+=1;
						repeat
							SetLength(variabletype, Length(variableType)+1);

							if (prevTokenValue)='' then
								variableType[High(variableType)]:= secondPreviousTokenValue
							else
								variableType[High(variableType)]:= previousTokenValue;
								
							SearchVariableTypeInPointersList(s, editIndex);
							writeln('count: ', count);
							writeln(' variableType: ', variableType[High(variableType)]);
							writeln('High of VariableName: ', High(variableName));
							writeln('High of Variabletype: ', High(variableType));
							writeln('secondPrevTokenValue: ', secondPrevTokenValue, 'prevTokenValue: ', prevTokenValue);
							if (secondPrevTokenValue= '^') then
							begin
								if High(variableType) <= High(VariableName) then
									variableName[High(variableType)]:= variableName[High(variableType)] + secondPrevTokenValue;
								writeln('modified Var Name1: ', variableName[High(variableType)]);
							end;
							count-=1;
						until count=0;	
					end
					else
					begin
						SetLength(variabletype, Length(variableType)+1);
						if (prevTokenValue)='' then
							variableType[High(variableType)]:= secondPreviousTokenValue
						else
							variableType[High(variableType)]:= previousTokenValue;
							
						writeln(' variableType: ', variableType[High(variableType)]);
						writeln('High of VariableName: ', High(variableName));
						writeln('High of Variabletype: ', High(variableType));
						writeln('secondPrevTokenValue: ', secondPrevTokenValue, 'prevTokenValue: ', prevTokenValue);

						SearchVariableTypeInPointersList(s, editIndex);
						
						if (secondPrevTokenValue= '^') then
						begin
							writeln('variableName[High(variableType)]: ', variableName[High(variableType)] );
							if High(variableType) <= High(VariableName) then
								variableName[High(variableType)]:= variableName[High(variableType)] + secondPrevTokenValue;
							writeln('modified Var Name1: ', variableName[High(variableType)]);
						end;
						
					end;
				end;
				
				if gotVariableType then
					gotVariableType:= false;
			end;
							
		end;
	end;
end;}
	
function SearchForInheritedObjInRVList(var s:snapshot; editIndex:LongInt; inheritedObjectName:AnsiString ):AnsiStringArray;
var
	eIOpenFile,frontMainEditIndex:LongInt;
	i,j,l,m:LongInt;
	list:AnsiStringArray;
	frontMain: WindowPtr;
	
begin
	with s do
	begin
		for i:=Low(lRecordVariables) to High(lRecordVariables) do
		begin
			with lRecordVariables[i] do
			begin
				if (LowerCase(recordName) = LowerCase(inheritedObjectName)) then
				begin
					SearchForInheritedObjInRVList := recordMembers;
					Exit(SearchForInheritedObjInRVList);
				end;
			end;
		end;
	end;
	
	for i:=Low(editRecordVariables[editIndex]) to High(editRecordVariables[editIndex]) do
	begin
		with editRecordVariables[editIndex][i] do
		begin
			if (LowerCase(recordName) = LowerCase(inheritedObjectName)) then
			begin
				SearchForInheritedObjInRVList := recordMembers;
				Exit(SearchForInheritedObjInRVList);
			end;
		end;
	end;
	
	if editIndex > 0 then
		if editIndex <= kMaxEditWindows then
			list := BuildJTFNUsesList(editWind[editIndex], true {withPath}); //usesList[editIndex];
			
			//for i := Low(list) to High(list) do
			 //writeln('list: ', list[i]);

		//Get editIndex of frontMain file.
		frontMain := GetFrontMainWindow;
		frontMainEditIndex:= getWRefCon(frontMain);
		
	for i := low(list) to High(list) do
	begin
		eIOpenFile := FileIsOpen(list[i]); // If the file is open then it returns it's editIndex.
		//isOpenFlags[i] := eIOpenFile <> 0; //isOpenFlags is true If file is open; 
		//writeln('list ', i, ' ', list[i]);
	    if eIOpenFile > 0 then
	    begin
	    	if Length(editRecordVariables) > 0 then // Possible bug?
	    	if High(editRecordVariables) < eIOpenFile then // Possible bug?
			for j:= low(editRecordVariables[eIOpenFile]) to High(editRecordVariables[editIndex]) do
			begin
				with editRecordVariables[eIOpenFile][j] do
				begin
					if (LowerCase(recordName) = LowerCase(inheritedObjectName)) then
					begin
						SearchForInheritedObjInRVList := recordMembers;
						Exit(SearchForInheritedObjInRVList);
					end;
				end;
			end;
		end
		else
		begin
			for m:= Low(gClosedFilesVariablesData[frontMainEditIndex]) to High(gClosedFilesVariablesData[frontMainEditIndex]) do
			begin
				if (list[i] = gClosedFilesVariablesData[frontMainEditIndex][m].fileName) then
				begin
					for l:= low(gClosedFilesVariablesData[frontMainEditIndex][m].fileRecordVariables) to High(gClosedFilesVariablesData[frontMainEditIndex][m].fileRecordVariables) do
					begin
						with gClosedFilesVariablesData[frontMainEditIndex][m].fileRecordVariables[l] do
						begin
							if (LowerCase(recordName) = LowerCase(inheritedObjectName)) then
							begin
								SearchForInheritedObjInRVList := recordMembers;
								Exit(SearchForInheritedObjInRVList);
							end;
						end;
					end;
				end;
			end;
		end;
	end;
end;

procedure PascalColorCoder(var s: SnapShot; teRec: HaldaPtr; editIndex: Integer);
var
//	data: AnsiString;
	//err: OSErr;
	tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
	
//	betterTokenHandle: Handle; {Token value in normal case}
//	iItemCount: Integer;
	untilHere: Longint;
	pos1, pos2, pos3: LongInt;
	count: LongInt =0;
	recName, recMember: AnsiString;
	recIndex,i: LongInt;
	foundrecName, foundRecMember, foundPointer, foundType, bracketFound: Boolean;
	bracketCount: LongInt;
	chars, toBeParsed: AnsiString;
	gotVariableType: boolean = false;
	startPos, endPos: LongInt;
	functionParsing: boolean = false;
	levelCount: LongInt =0;
	exitVar: boolean=false;
	methodParameters, methodName: AnsiString;
	theTitle: String;
	
	//gMethods: array of MethodRec;

	//This recognises which one is variable name, or variableType. 	
	procedure GetVariableList(tokenValue:AnsiString; pos1, untilHere, bufferLength: LongInt );
	begin
		with s do
		begin
			with lVariableMembers do
			begin
				//Skip brackets, for example, brackets in arrays declaration.
				if (tokenValue= '(') then
				begin
					pos2:=pos1;
					
					bracketFound:= False;
					bracketCount:=0;
					bracketCount+=1;
					//writeln('bracketCount: ', bracketCount, ' pos: ', pos);
					FinishBracketsinLoops(teRec^.text, pos2, bracketCount, untilHere, bufferLength);
					pos1:=pos2;
				end;

				//pick variable names
					//if tokenValue = ':' or ',' then its previousTOkenValue should be variable Name.			
					if (tokenValue=':') or (tokenValue=',') then
					begin
						//tokenValue=',' then need to be counted no. of commas to use is it for storing tokenTypes. 
						if (tokenValue=',') then
						begin
							count+=1;
						end;
						SetLength(variableName, Length(variableName)+1);
						SetLength(variableScope, Length(variableScope)+1);
						if (prevTokenValue)='' then// (prevTokenValue)='' means a comment, in that case, variable name should be secondprevTokenValue
							variableName[High(variableName)]:= secondPrevTokenValue//GetTokenInCase(teRec, secondPrevTokenStart, secondPrevTokenEnd)//secondPrevTokenValue //Should be replaced by GetTokenInCase(teRec, SecondPrevtokenStart, SecondPrevtokenEnd);
						else
							variableName[High(variableName)]:= prevTokenValue;//GetTokenInCase(teRec, prevtokenStart, prevtokenEnd);//prevTokenValue; //Should be replaced by GetTokenInCase(teRec, prevtokenStart, prevtokenEnd);
						//writeln('pTV: ', prevTokenValue);
						//writeln('varibleName: ', variableName[High(variableName)]);
						
						//Add scope here
						if not hasSeenImplementation then
							variableScope[High(variableScope)]:= 'global'
						else
							if not functionParsing then
								variableScope[High(variableScope)]:= 'local'
							else
								variableScope[High(variableScope)]:= editFunctionName[High(editFunctionName)];
							
						//writeln('variableScope: ', variableScope[High(variableScope)]);
					end;
				
				//While initialization tokenType should be tokenValue, previous to '=', which can be found before ';'
				if (tokenValue='=' ) then
				begin
					gotVariableType:=true;
					
					SetLength(variabletype, Length(variableType)+1);
					if (prevTokenValue)='' then
						variableType[High(variableType)]:= secondPrevTokenValue
					else
						variableType[High(variableType)]:= prevTokenValue;
						
						
					SearchVariableTypeInPointersList(s, editIndex);
					//writeln(' variableType: ', variableType[High(variableType)]);
					//writeln('gotVariableType: ', gotVariableType);
				end;                                         
				
				//TokenType is the tokenValue before ';' 
				if (tokenValue=';') then
				begin
					if not gotVariableType then
					begin
						if count>0 then
						begin
							count+=1;
							repeat //repeat no. of count times.
								SetLength(variabletype, Length(variableType)+1);

								if (prevTokenValue)='' then
									variableType[High(variableType)]:= secondPrevTokenValue
								else
									variableType[High(variableType)]:= prevTokenValue;
									
								SearchVariableTypeInPointersList(s, editIndex);
								//writeln('count: ', count);
								//writeln(' variableType: ', variableType[High(variableType)]);
								//writeln('secondPrevTokenValue: ', secondPrevTokenValue, 'prevTokenValue: ', prevTokenValue);
								if (secondPrevTokenValue= '^') then
								begin
									if High(variableType) <= High(VariableName) then
										variableName[High(variableType)]:= variableName[High(variableType)] + secondPrevTokenValue;
									//writeln('modified Var Name1: ', variableName[High(variableType)]);
								end;
								count-=1;
							until count=0;	
						end
						else
						begin
							SetLength(variabletype, Length(variableType)+1);
							if (prevTokenValue)='' then
								variableType[High(variableType)]:= secondPrevTokenValue
							else
								variableType[High(variableType)]:= prevTokenValue;
								
							//writeln(' variableType: ', variableType[High(variableType)]);
							//writeln('High of VariableName: ', High(variableName));
							//writeln('High of Variabletype: ', High(variableType));
							SearchVariableTypeInPointersList(s, editIndex);
							
							if (secondPrevTokenValue= '^') then
							begin
								//writeln('variableName[High(variableType)]: ', variableName[High(variableType)] );
								if High(variableType) <= High(VariableName) then
									variableName[High(variableType)]:= variableName[High(variableType)] + secondPrevTokenValue;
								//writeln('modified Var Name1: ', variableName[High(variableType)]);
							end;
							
						end;
					end;
					
					if gotVariableType then
						gotVariableType:= false;
				end;
								
			end;
		end;
	end;
	
	
begin
	SetLength(editGlobalFunctions[editIndex], 0);
	untilHere := s.pos + kCCStepLength;
	
	
	with s do
	begin
	// Turn off redraw while color coding
//		SetTXNVisibility(teRec, false); 

//		dataPtr := CharsPtr(chars[editIndex]^);
		bufferLength := Length(teRec^.text);
		SetLength(editMethods[editIndex], 0);	
		SetLength(editClassList[editIndex], 0);
		repeat
			GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
			
			// History
			if tokenValue = 'program' then
			begin
				hasSeenProgram := true;		// Mark window as gogo-able and runnable
				editWindIsMain := true;
				editMainWindIsProgram := true;

				editWindIsCocoaProgram := false;
				editWindIsiOSProgram := false;
				editWindIsJVMProgram := false;
				editWindIsAndroidProgram := false; // JVM targets added 20160303				

				InvalFrontMainWindow;
			end;
			if tokenValue = 'library' then
			begin
				hasSeenProgram := true;		// Mark window as buildable
				editWindIsMain := true;
				editMainWindIsProgram := false; // Not runnable!
			end;
			if tokenValue = 'unit' then
				hasSeenUnit := true;		// Mark window as compileable without building bundle
			if tokenValue = 'interface' then
				hasSeenInterface := true;
			if tokenValue = 'implementation' then	// Signals that "procedure" and "function" may be candidates for function menu
				hasSeenImplementation := true;
			
			case tokenType of
			kFunctionToken:
			begin
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VFunctionColor, LWPSintax.VFunctionStyle);

			end;
			kBeginEndToken:
			begin
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
				
				if functionParsing then
				begin
					if tokenValue = 'begin' then
						levelCount+=1
					else 
						if tokenValue = 'end' then
						begin
							levelCount-=1;
							if levelCount <= 0 then
								functionParsing:= false;
						end;
				end;
				
			end;
			kKnownTypeToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VKnowTypeColor, LWPSintax.VKnowTypeStyle);
			kReservedToken:
			begin
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
				if functionParsing then
					if (tokenValue = 'record') or (tokenValue = 'object') or (tokenValue = 'objcclass') then
						levelCount+=1; //level should be incremented?
					
				//Get the variable names of record, by checking type followd by record.
				if (tokenValue='record') or (tokenValue = 'object') or (tokenValue = 'objcclass') then
				begin
					//Get record name and Search for it If it exist in editRecordVariables[editIndex][0..High]. If It doesn't has, then increase length of editRecordVariables.
					SetLength(lRecordVariables, Length(lRecordVariables)+1);
					
					with lRecordVariables[High(lRecordVariables)] do 
					begin
						//pick record Name
						//if not foundrecName then	
						if exitVar then
							recordName := GetTokenInCase(teRec, secondPrevTokenStart, secondPrevTokenEnd)//secondPrevTokenValue //prevTokenValue
						else
							recordName := GetTokenInCase(teRec, secondPrevTokenStart, secondPrevTokenEnd);//secondPrevTokenValue;
						//writeln('recordName: ', recordName);
						//Add record scope here
						if not hasSeenImplementation then
							recordScope:= 'global'
						else
							if not functionParsing then
								recordScope:= 'local'
							else
								//recordScope:= editFunctionName[High(editFunctionName)];
								if High(editFunctionName) >= 0 then // Added by Ingemar 151105
									recordScope:= editFunctionName[High(editFunctionName)]
								else
									recordScope := '???';
								
						//writeln('***recordScope:*** ', recordScope);
						//GetWTitle(editWind[editIndex], theTitle);
						theTitle:=GetPathFromIndex(editIndex);

						if (tokenValue = 'object') or (tokenValue = 'class') or (tokenValue = 'objcclass') then // objcclass, class?
						begin
							SetLength(editClassList[editIndex], Length(editClassList[editIndex])+1);
							//writeln('Length(editClassList[editIndex]): ', Length(editClassList[editIndex]));
							//writeln('className: ', recordName);
							editClassList[editIndex][High(editClassList[editIndex])].className := recordName;
							SetLength(editClassList[editIndex][High(editClassList[editIndex])].inheritedClasses, 0); // No inherited classes found yet
							editClassList[editIndex][High(editClassList[editIndex])].definedInFileName := GetLastToken(theTitle);
							editClassList[editIndex][High(editClassList[editIndex])].definedInFilePath := theTitle;
							editClassList[editIndex][High(editClassList[editIndex])].definedInFilePosition := secondPrevTokenStart; // tokenStart;
							editClassList[editIndex][High(editClassList[editIndex])].classScope := recordScope;
							
							//WriteLn('Done storing to list');
							//writeln('editClassList[editIndex].className: ', editClassList[editIndex][High(editClassList[editIndex])].className);
						end;
						
						//Parse until end of record to collect record members. Note that we ignore type of recordmembers.
						pos1:= pos;
						repeat
							repeat GetToken(teRec^.text, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
								
							//To support inheritance:
							if false then // DISABLED since it CRASHES!
							if (tokenValue='(') and (prevTokenValue='object') then
							begin
								repeat GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
								until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
								
								repeat GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
								until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
								
								inheritedObjectName := tokenValue;
								
								SetLength(editClassList[editIndex][High(editClassList[editIndex])].inheritedClasses, Length(editClassList[editIndex][High(editClassList[editIndex])].inheritedClasses)+1); // No inherited classes found yet
								editClassList[editIndex][High(editClassList[editIndex])].inheritedClasses[High(editClassList[editIndex][High(editClassList[editIndex])].inheritedClasses)] := GetTokenInCase(teRec, tokenStart, tokenEnd);//tokenValue;
								
								//writeln('editClassList[editIndex].inheritedClasses[High(editClassList[editIndex].inheritedClasses)]: ', editClassList[editIndex][High(editClassList[editIndex])].inheritedClasses[High(editClassList[editIndex][High(editClassList[editIndex])].inheritedClasses)]);
								//writeln('inheritedObjectName: ', inheritedObjectName);
								inheritedList := SearchForInheritedObjInRVList(s, editIndex, inheritedObjectName);//Search for inherited objectName in record variable list, and get it's members list If found.
								{for i:= low(inheritedList) to High(inheritedList) do
								begin
									writeln('inheritedList: ',i, ' ', inheritedList[i]);
								end;}
								
								//Add the members list to the current object or record.
								recordMembers:=inheritedList;
							end;
							
							
							
							//pick record variables
							//writeln('tokenValue: ', tokenValue);
							//writeln('**previousTokenValue**', prevTokenValue);
							if (tokenValue=':') or (tokenValue=',') or (tokenValue='procedure') or (tokenValue='function') then
							begin
								SetLength(recordMembers, Length(recordMembers)+1);
								if (prevTokenValue)='' then
									recordMembers[High(recordMembers)] {recMember}:= GetTokenInCase(teRec, secondPrevTokenStart, secondPrevTokenEnd)//secondPrevTokenValue
								else
									recordMembers[High(recordMembers)] {recMember}:= GetTokenInCase(teRec, prevtokenStart, prevtokenEnd);//prevTokenValue;
								
								if (tokenValue='procedure') or (tokenValue='function') then
								begin
									//Get function name
									methodName:='';
									methodParameters:='';
									repeat GetToken(teRec^.text, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
									until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
									recordMembers[High(recordMembers)] := GetTokenInCase(teRec, tokenStart, tokenEnd);//tokenValue;
									methodName:=recordMembers[High(recordMembers)];
									//writeln('recordMembers[High(recordMembers)]: ', recordMembers[High(recordMembers)] );
									//writeln('@#$%methodName: ', methodName);
									//Get one more takenValue for '('
									
									SetLength(editMethods[editIndex], Length(editMethods[editIndex])+1);
									editMethods[editIndex][High(editMethods[editIndex])].className := recordName;
									editMethods[editIndex][High(editMethods[editIndex])].name := methodName;
									editMethods[editIndex][High(editMethods[editIndex])].position := tokenStart;
									editMethods[editIndex][High(editMethods[editIndex])].fileName := theTitle;
									//writeln('theTitle: ', theTitle);
									
									repeat GetToken(teRec^.text, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
									until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
									
									if (tokenValue= '(') then
									begin
										pos2:=pos1;
										startPos:= pos1-1;
										bracketFound:= False;
										bracketCount:=0;
										bracketCount+=1;
										//writeln( 'b4- pos2: ', pos2);
										FinishBracketsinLoops(teRec^.text, pos2, bracketCount, untilHere, bufferLength);
										//writeln( 'after- pos2: ', pos2);
										endPos:=pos2;
										pos1:=pos2;
										
										methodParameters:=GetTokenInCase(teRec, startPos, endPos);
										SetLength(recordMembers, Length(recordMembers)+1);
										recordMembers[High(recordMembers)] := methodName + methodParameters;
										//writeln('methodParameters: ', methodParameters);
									end;
								end;
								//writeln(' recordMember: ', recordMembers[High(recordMembers)]);
							end;
						until (tokenValue='end') or (pos1 >= bufferLength) or (pos1 >= untilHere);
					end; //with
				end;
				
				//Collect variables list 
				if (tokenValue= 'var') then
				begin
					exitVar:= false;
					with lVariableMembers do
					begin
						pos1:=pos;
						count:=0;
						repeat
							repeat GetToken(teRec^.text, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
							//writeln('tokenValue: ', tokenValue, ' previousTokenValue: ', previousTokenValue, 'secondPreviousTokenValue: ', secondPrevTokenValue);
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
							
							//This is for special case var X:record. Precisly record declared inside var.
							if tokenValue = 'record' then
							begin
								//SetLength(variableName, Length(variableName)-1);
								//save this record name in to variable name. VarableType also should be the recordName in order to use this for code completion. And the exit Var loop by exitVar.
								if High(variableName) >= 0 then // If nothing in variableName, it will crash!
								begin
									SetLength(variableType, Length(variableType)+1);
									variableType[High(variableType)]:= variableName[High(variableName)];
									exitVar:= true;
								end;
							end;
							If not exitVar then
							begin
								pos2:=pos1;
								//GetVariableList(teRec^.text, s, tokenValue, editIndex, pos2, untilHere, bufferLength);
								GetVariableList(tokenValue, pos2, untilHere, bufferLength);

								pos1:=pos2;
							end;		
									
									
						until (exitVar) or (tokenvalue= 'type') or (tokenvalue= 'var') or (tokenvalue= 'begin') or (tokenvalue= 'const') or (tokenvalue= 'procedure') or
						(tokenvalue= 'function') or (tokenvalue= 'implementation') or (tokenValue = 'constructor') or (pos1 >= bufferLength) or (pos1 >= untilHere);
						
						tokenType:= kReservedToken;// previous token type--> var
						
					end;//with
				end;
				
				if (tokenValue = 'class') or (tokenValue = 'objcclass') then
				begin
				// Förrförra = klassnamn
				// Nästa = (
				// Nästnästa = ärvd klass
				// Bygg på klasslista här? (För class browser!)
//					WriteLn('--- Class detected! ---');
//					WriteLn('Class name: ', secondPreviousTokenValue);
					GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
					GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
//					WriteLn('Inherits: ', tokenValue);
				end;
				
	// Parse "uses" clause
	//				if tokenType = kReservedToken then
				if tokenValue = 'uses' then
				// Found "uses", read everything until a ";" is found.
				begin
					repeat
						GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
						// NOTE! This may need to include more types. Some recognised words may be accepted as unit names.
						if tokenType in [kOtherToken, kCarbonToken] then
						begin
							if hasSeenProgram then // only check for main program
							// Should I add an advanced setting to select this manually?
							begin
								if tokenValue = 'cocoaall' then
									editWindIsCocoaProgram := true;
								if tokenValue = 'iphoneall' then
									editWindIsiOSProgram := true;
								// NOTE! These are hard-coded for the current version. 
								if tokenValue = 'jdk15' then
									editWindIsJVMProgram := true;
								if tokenValue = 'androidr14' then
									editWindIsAndroidProgram := true;
							end;

							// Append findings to list.
							tokenValue := GetTokenInCase(teRec, tokenStart, tokenEnd);
							SetLength(usesList, Length(usesList)+1);
//							usesList[editIndex][Length(usesList[editIndex])-1] := tokenValue;
							usesList[Length(usesList)-1] := tokenValue;
	//								AppendMenu(usesMenu, tokenValue + extension); // + file extension								
						end
						else
						// Allow comments in "uses" - same as below
						if tokenType = kCommentToken then
							SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VCommentColor, LWPSintax.VCommentStyle);
						// And compiler directives! 121109
						if tokenType = kCompilerDirectiveToken then
							SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VCompilerDirectiveColor, LWPSintax.VCompilerDirectiveStyle);
					until (pos >= bufferLength) or (tokenType = kSingleCharToken) and (tokenValue = ';');
				end;
			end;
			kCommentToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VCommentColor, LWPSintax.VCommentStyle);
			kStringToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VStringColor, LWPSintax.VStringStyle);
			kSpecialToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VSpecialColor, LWPSintax.VSpecialStyle);
			kLibraryToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VLibraryColor, LWPSintax.VLibraryStyle);
			kClassToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VClassColor, LWPSintax.VClassStyle);
			kCarbonToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VCarbonColor, LWPSintax.VCarbonStyle);
			kCocoaToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VCocoaColor, LWPSintax.VCocoaStyle);
	//		kSingleCharToken:
	//			SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VSingleCharColor, LWPSintax.VSingleCharStyle);
			kCompilerDirectiveToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VCompilerDirectiveColor, LWPSintax.VCompilerDirectiveStyle);
			kOtherToken, kSingleCharToken:
			begin
				//writeln('previousTokenType: ', previousTokenType);
				if previousTokenType = kFunctionToken then
				begin
					SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VOtherColor, LWPSintax.VOtherStyle);
					{Good base for a function menu, but some items should be filtered out.}
					{- Only after implementation or program}
					{- Not "forward" ("forward" could remove the latest?)}
					{- Not at function type declaration}
					
					if not hasSeenImplementation then
						if secondPreviousTokenValue <> ':' then // function type
								if secondPreviousTokenValue <> '=' then // function type
									if not (tokenType = kSingleCharToken) then
									begin
										//writeln('Global Function: ',  GetTokenInCase(teRec, tokenStart, tokenEnd));
										SetLength(editGlobalFunctions, Length(editGlobalFunctions)+1);
										editGlobalFunctions[High(editGlobalFunctions)] := GetTokenInCase(teRec, tokenStart, tokenEnd);
									end;
					
					if editIndex > 0 then
					if hasSeenImplementation or hasSeenProgram then // This is pretty easy, others are harder
						if secondPreviousTokenValue <> ':' then // function type
							if secondPreviousTokenValue <> '=' then // function type
								begin
									if tokenType = kSingleCharToken then // operator
										tokenValue := 'operator ' + tokenValue
									else
										tokenValue := GetTokenInCase(teRec, tokenStart, tokenEnd);
									functionParsing:=true;
									AddToEditFunctionArrays(s, tokenValue, tokenStart, tokenEnd);
								end;
					// Hur ska jag sortera bort de andra? Ta bort en rad vid forward? Men funktionstyper då? Kolla token före procedure/funktion? = eller : betyder funktiontyp?
					
					//If it is function, Collect functionname and functionType and store it in varName and varType respectively. 
					//Skip brackets of function, procedure, operator, destructor
					//pos1:= pos;
					
					//if tV='(' then parse brackets, as this is special case type Procedure(...);
					if tokenValue = '(' then
					begin
						pos2:=pos;
						startPos:= pos;
						bracketFound:= False;
						bracketCount:=0;
						bracketCount+=1;
						//writeln( 'b4- pos2: ', pos2);
						FinishBracketsinLoops(teRec^.text, pos2, bracketCount, untilHere, bufferLength);
						//writeln( 'after- pos2: ', pos2);
						//writeln('untilHere: ', untilHere);
						endPos:=pos2-1;
						pos:=pos2;
					end;
					if hasSeenImplementation then
					begin
						if (prevTokenValue= 'function') then
						begin
							{repeat GetToken(teRec^.text, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF}
							
							//Colect function name as varName
							with lVariableMembers do
							begin
								SetLength(variableName, Length(variableName)+1);
								SetLength(variableScope, Length(variableScope)+1);
								variableName[High(variableName)]:= tokenValue; //GetTokenInCase(teRec, prevtokenStart, prevtokenEnd);//prevTokenValue; //Should be replaced by GetTokenInCase(teRec, prevtokenStart, prevtokenEnd);
								//writeln('pTV: ', prevTokenValue, ' TV: ', tokenValue);
								//writeln(' varibleName: ', variableName[High(variableName)]);

								
								//Add scope here
								if not hasSeenImplementation then
									variableScope[High(variableScope)]:= 'global'
								else
									if not functionParsing then
										variableScope[High(variableScope)]:= 'local'
									else
										variableScope[High(variableScope)]:= editFunctionName[High(editFunctionName)];
									
								//writeln('variableScope: ', variableScope[High(variableScope)]);
								
							end;
							repeat GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
							
							if (tokenValue= '(') then
							begin
								pos2:=pos;
								startPos:= pos;
								bracketFound:= False;
								bracketCount:=0;
								bracketCount+=1;
								//writeln( 'b4- pos2: ', pos2);
								FinishBracketsinLoops(teRec^.text, pos2, bracketCount, untilHere, bufferLength);
								//writeln( 'after- pos2: ', pos2);
								endPos:=pos2-1;
								pos:=pos2;
			
							end;
							
							repeat GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
							
							//Collect function type and strore into the variableType.
							if (tokenValue= ':') then
							begin
								repeat GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
								until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
								with lVariableMembers do
								begin
									SetLength(variabletype, Length(variableType)+1);
									variableType[High(variableType)]:= tokenValue;
									//writeln(' varibleType: ', variableType[High(variableType)]);
								end;
								//ReturnType can be also pointer.
								{if (secondPrevTokenValue= '^') then
								begin
									//writeln('variableName[High(variableType)]: ', variableName[High(variableType)] );
									if High(variableType) <= High(VariableName) then
										variableName[High(variableType)]:= variableName[High(variableType)] + secondPrevTokenValue;
									writeln('modified Var Name: ', variableName[High(variableType)]);

								end;}
								
							end;
							
							//Collect the variablesList inside the function parethesis.
							toBeParsed:= Copy(teRec^.text, startPos, endPos-startPos) + ';';
							//writeln('toBeParsed: ', toBeParsed);
							pos1:=1;
							//get the variables in text 'toBeParsed' by parsing.
							repeat
								repeat GetToken(toBeParsed, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
								//writeln('tokenValue: ', tokenValue, ' previousTokenValue: ', previousTokenValue, 'secondPreviousTokenValue: ', secondPrevTokenValue);
								until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= length(toBeParsed)); //Skip comments, kCRLF
								
								pos2:=pos1;
								secondPrevTokenValue:='';
								previousTokenValue:='';
								//GetVariableList(teRec^.text, s, tokenValue, editIndex, pos2, length(toBeParsed), length(toBeParsed));
								GetVariableList(tokenValue, pos2, length(toBeParsed), length(toBeParsed));
								pos1:=pos2;
								
							until pos1 >= (length(toBeParsed)+1);	
						end
						else //This is for collecting the procedure variables inside parethesis.
						begin
							repeat GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
							
							if (tokenValue= '(') then
							begin
								pos2:=pos;
								startPos:= pos;
								bracketFound:= False;
								bracketCount:=0;
								bracketCount+=1;
								//writeln( 'b4- pos2: ', pos2);
								FinishBracketsinLoops(teRec^.text, pos2, bracketCount, untilHere, bufferLength);
								//writeln( 'after- pos2: ', pos2);
								endPos:=pos2-1;
								pos:=pos2;
								
								toBeParsed:= Copy(teRec^.text, startPos, endPos-startPos) + ';';
								//writeln('toBeParsed: ', toBeParsed);
								pos1:=1;
								repeat
									repeat GetToken(toBeParsed, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
									//writeln('tokenValue: ', tokenValue, ' previousTokenValue: ', previousTokenValue, 'secondPreviousTokenValue: ', secondPrevTokenValue);
									until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= length(toBeParsed)); //Skip comments, kCRLF
									
									pos2:=pos1;
									secondPrevTokenValue:='';
									previousTokenValue:='';
									//GetVariableList(teRec^.text, s, tokenValue, editIndex, pos2, length(toBeParsed), length(toBeParsed));
									GetVariableList(tokenValue, pos2, length(toBeParsed), length(toBeParsed));
									pos1:=pos2;
									
								until pos1 >= (length(toBeParsed)+1);
								end;
							
						end;
					end
					else //Skip data inside of () for functions/procedures before implementations. Otherwise, those variables stored as normal variables if it has 'var' in ().
					begin
					
						repeat GetToken(teRec^.text, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
						until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
						
						if (tokenValue= '(') then
						begin
							pos2:=pos;
							startPos:= pos;
							bracketFound:= False;
							bracketCount:=0;
							bracketCount+=1;
							//writeln( 'b4- pos2: ', pos2);
							FinishBracketsinLoops(teRec^.text, pos2, bracketCount, untilHere, bufferLength);
							//writeln( 'after- pos2: ', pos2);
							//writeln('untilHere: ', untilHere);
							endPos:=pos2-1;
							pos:=pos2;
						end;
					end;
						
						//writeln('**TV**', tokenValue);
					
				end
				else
					if tokenType = kSingleCharToken then
					begin
						SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VSingleCharColor, LWPSintax.VSingleCharStyle);
						
						//To collect the recordPointersName and Type. Copy them in to snapshotvariables, i.e. lRecordPointers.
						//lRecordPointers (record) has three variables, 1.pointerVariableName stores the pointer name 2. pointerToPointer (boolean): stores whether it is a pointerToPointer or not.
						//3.pointerVariableType stores the pointer type.
						if (tokenValue='^') and (prevTokenValue='=') then
						begin
							//writeln('**tokenValue**', tokenValue, ' prevTV: ', prevTokenValue, 'secondPTV: ', secondPrevTokenValue);
							//writeln('*Length of lPointerVariableName*: ', Length(lRecordPointers));

							//setLength(lRecordPointers.pointerVariableName, Length(lRecordPointers.pointerVariableName)+1);
							//setLength(lRecordPointers.pointerVariableType, Length(lRecordPointers.pointerVariableType)+1);
							setLength(lRecordPointers, Length(lRecordPointers)+1);
							with lRecordPointers[High(lRecordPointers)] do
							begin
							//lRecordVariables[High(lRecordPointers)].pointerToPointer:=false;
							//lRecordPointers[High(lRecordPointers)].pointerVariableName:= secondPrevTokenValue;
								pointerToPointer:=false;
								pointerVariableName:= secondPrevTokenValue;
							end;
							pos1:=pos;
							repeat GetToken(teRec^.text, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
							until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
							foundPointer:=false;
							//writeln('**tokenValue**', tokenValue);
							//writeln('Length of lPointerVariableName: ', Length(lRecordPointers.pointerVariableName));
							
							//tokenValue (which supposed to be pointerType), is searched in all snapShot pointerVariableNames. If it found, the it should be pointerToPointer.							
							for i:=0 to High(lRecordPointers) do
							begin					
								if (tokenValue=lowercase(lRecordPointers[i].pointerVariableName)) then
								begin
									lRecordPointers[High(lRecordPointers)].pointerVariableType:= lRecordPointers[i].pointerVariableType;
									lRecordPointers[High(lRecordPointers)].pointerToPointer:=true;
									//lPointerVariableName[High(lPointerVariableType)]:= lPointerVariableName[i]+'^';
									foundPointer:=true;
									break;
								end;
							end;
							
							//tokenValue (which supposed to be pointerType), is searched in all pointerVariableNames. If it found, the it should be pointerToPointer.							
							if not foundPointer then
								for i:=0 to High(editRecordPointers[editIndex]) do
								begin
									if (tokenValue=lowercase(editRecordPointers[editIndex][i].pointerVariableName)) then
									begin
										lRecordPointers[High(lRecordPointers)].pointerVariableType:= editRecordPointers[editIndex][i].pointerVariableType;
										lRecordPointers[High(lRecordPointers)].pointerToPointer:=true;
										//lPointerVariableName[High(lPointerVariableType)]:= gPointerVariableName[editIndex][i]+'^';
										foundPointer:=true;
										break;
									end;
								end;
							//If not found then it is not a pointerToPointer.
							if not foundPointer then
									lRecordPointers[High(lRecordPointers)].pointerVariableType:= tokenValue;
							//writeln('lRecordVariables.pointerToPointer: ', lRecordPointers[High(lRecordPointers)].pointerToPointer);
							
							//writeln('**lPointerVariableName:** ', lPointerVariableName[High(lPointerVariableName)]);
							//writeln('**lPointerVariableType**', lPointerVariableType[High(lPointerVariableType)]);
							
						end;

						 
					end;
			end;
				
			otherwise
			end;

			// Remember previous two tokens - except comments
			// HJÄLPER INTE???
			if tokenType <> kCommentToken then
			begin
				secondPreviousTokenValue := previousTokenValue;
				secondPreviousTokenType := previousTokenType;
				
				previousTokenValue := tokenValue;
				previousTokenType := tokenType;
			end;

			until (pos >= bufferLength) or (pos >= untilHere);
			
	end; // with
end; {PascalColorCoder}


// Ada skipped for now - maybe forever, since nobody uses it

{------------------------ ADA COLOR CODER ----------------------------}

procedure InitAdaColorCodingTable;
begin
	HashAddEntry(adaHashTable, 'package', kReservedToken);
	HashAddEntry(adaHashTable, 'array', kReservedToken);
//	HashAddEntry(adaHashTable, 'unit', kReservedToken);
	HashAddEntry(adaHashTable, 'interface', kReservedToken);
	HashAddEntry(adaHashTable, 'implementation', kReservedToken);
	HashAddEntry(adaHashTable, 'initialization', kReservedToken); // Ingemar 090629
	HashAddEntry(adaHashTable, 'const', kReservedToken);
	HashAddEntry(adaHashTable, 'type', kReservedToken);
	HashAddEntry(adaHashTable, 'declare', kReservedToken);
	HashAddEntry(adaHashTable, 'record', kReservedToken);
	HashAddEntry(adaHashTable, 'uses', kReservedToken);
	HashAddEntry(adaHashTable, 'use', kReservedToken);

	HashAddEntry(adaHashTable, 'of', kReservedToken);
	HashAddEntry(adaHashTable, 'set', kReservedToken);
	HashAddEntry(adaHashTable, 'variant', kReservedToken);
	HashAddEntry(adaHashTable, 'packed', kReservedToken);
	HashAddEntry(adaHashTable, 'overload', kReservedToken);
	HashAddEntry(adaHashTable, 'class', kReservedToken);
	
	HashAddEntry(adaHashTable, 'abstract', kClassToken); 
	HashAddEntry(adaHashTable, 'generic', kClassToken); 
	HashAddEntry(adaHashTable, 'object', kClassToken); 
	HashAddEntry(adaHashTable, 'virtual', kClassToken); 
	HashAddEntry(adaHashTable, 'override', kClassToken); 
	HashAddEntry(adaHashTable, 'reintroduce', kClassToken); 
	HashAddEntry(adaHashTable, 'public', kClassToken); 
	HashAddEntry(adaHashTable, 'private', kClassToken); 
	HashAddEntry(adaHashTable, 'protected', kClassToken); 
	HashAddEntry(adaHashTable, 'published', kClassToken); 
	HashAddEntry(adaHashTable, 'as', kClassToken); 
	HashAddEntry(adaHashTable, 'is', kClassToken); 

	HashAddEntry(adaHashTable, 'and', kSpecialToken); 
	HashAddEntry(adaHashTable, 'or', kSpecialToken); 
	HashAddEntry(adaHashTable, 'not', kSpecialToken); 
	HashAddEntry(adaHashTable, 'xor', kSpecialToken); 
	HashAddEntry(adaHashTable, 'in', kSpecialToken);
	HashAddEntry(adaHashTable, 'div', kSpecialToken);
	HashAddEntry(adaHashTable, 'mod', kSpecialToken);
	HashAddEntry(adaHashTable, 'nil', kSpecialToken);
	HashAddEntry(adaHashTable, 'true', kSpecialToken);
	HashAddEntry(adaHashTable, 'false', kSpecialToken);

	HashAddEntry(adaHashTable, 'procedure', kFunctionToken);
	HashAddEntry(adaHashTable, 'function', kFunctionToken);
	HashAddEntry(adaHashTable, 'constructor', kFunctionToken);
	HashAddEntry(adaHashTable, 'destructor', kFunctionToken);

	HashAddEntry(adaHashTable, 'begin', kBeginEndToken);
	HashAddEntry(adaHashTable, 'end', kBeginEndToken);
	HashAddEntry(adaHashTable, 'if', kBeginEndToken);
	HashAddEntry(adaHashTable, 'then', kBeginEndToken);
	HashAddEntry(adaHashTable, 'else', kBeginEndToken);
	HashAddEntry(adaHashTable, 'case', kBeginEndToken); 
	HashAddEntry(adaHashTable, 'when', kBeginEndToken); 
	HashAddEntry(adaHashTable, 'others', kBeginEndToken);
	HashAddEntry(adaHashTable, 'with', kReservedToken);
	HashAddEntry(adaHashTable, 'downto', kBeginEndToken);
	HashAddEntry(adaHashTable, 'loop', kBeginEndToken); 
	HashAddEntry(adaHashTable, 'for', kBeginEndToken); 
	HashAddEntry(adaHashTable, 'to', kBeginEndToken); 
	HashAddEntry(adaHashTable, 'do', kBeginEndToken); 
	HashAddEntry(adaHashTable, 'while', kBeginEndToken); 
	HashAddEntry(adaHashTable, 'repeat', kBeginEndToken); 
	HashAddEntry(adaHashTable, 'until', kBeginEndToken); 
	HashAddEntry(adaHashTable, 'goto', kBeginEndToken);
	HashAddEntry(adaHashTable, 'try', kBeginEndToken); // Ingemar 090629
	HashAddEntry(adaHashTable, 'except', kBeginEndToken); // Ingemar 090629

	HashAddEntry(adaHashTable, 'exit', kBeginEndToken);	// these are actually system library routines
	HashAddEntry(adaHashTable, 'halt', kBeginEndToken);
	HashAddEntry(adaHashTable, 'runerror', kBeginEndToken);
	HashAddEntry(adaHashTable, 'assert', kBeginEndToken);
	HashAddEntry(adaHashTable, 'break', kBeginEndToken);
	HashAddEntry(adaHashTable, 'continue', kBeginEndToken);
	HashAddEntry(adaHashTable, 'longjump', kBeginEndToken);
	HashAddEntry(adaHashTable, 'setjump', kBeginEndToken);
	HashAddEntry(adaHashTable, 'return', kBeginEndToken); 
	
	HashAddEntry(adaHashTable, 'pointer', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'longint', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'integer', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'real', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'char', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'shortint', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'smallInt', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'longword', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'int64', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'byte', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'word', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'cardinal', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'qword', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'boolean', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'bytebool', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'longbool', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'single', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'double', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'extended', kKnownTypeToken); 
	HashAddEntry(adaHashTable, 'comp', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'currency', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'string', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'ansistring', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'widestring', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'pchar', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'file', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'dword', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'uint64', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'widechar', kKnownTypeToken);
	HashAddEntry(adaHashTable, 'wchar', kKnownTypeToken);
		
//	HashAddEntry(adaHashTable, '(', kSingleCharToken);	// these are hardwired
															// - because they are not alphanumeric /Ingemar
//	HashAddEntry(adaHashTable, ')', kSingleCharToken);
//	HashAddEntry(adaHashTable, '[', kSingleCharToken);
//	HashAddEntry(adaHashTable, ']', kSingleCharToken);
//	HashAddEntry(adaHashTable, '+', kSingleCharToken);
//	HashAddEntry(adaHashTable, '-', kSingleCharToken);
//	HashAddEntry(adaHashTable, '*', kSingleCharToken);
//	HashAddEntry(adaHashTable, '/', kSingleCharToken);
//	HashAddEntry(adaHashTable, '@', kSingleCharToken);
//	HashAddEntry(adaHashTable, ':=', kSingleCharToken);
//	HashAddEntry(adaHashTable, '+=', kSingleCharToken);
//	HashAddEntry(adaHashTable, '-=', kSingleCharToken);
//	HashAddEntry(adaHashTable, '/=', kSingleCharToken);
//	HashAddEntry(adaHashTable, '*=', kSingleCharToken);
		
	HashAddEntry(adaHashTable, 'tclass', kLibraryToken); 
	HashAddEntry(adaHashTable, 'tobject', kLibraryToken); 
	HashAddEntry(adaHashTable, 'tcollection', kLibraryToken); 
	HashAddEntry(adaHashTable, 'create', kLibraryToken); 
	HashAddEntry(adaHashTable, 'destroy', kLibraryToken); 
	HashAddEntry(adaHashTable, 'free', kLibraryToken); 
	HashAddEntry(adaHashTable, 'freeandnil', kLibraryToken); 
	
	HashAddEntry(adaHashTable, 'text', kLibraryToken);
	HashAddEntry(adaHashTable, 'append', kLibraryToken);
	HashAddEntry(adaHashTable, 'assign', kLibraryToken);
	HashAddEntry(adaHashTable, 'blockread', kLibraryToken);
	HashAddEntry(adaHashTable, 'blockwrite', kLibraryToken);
	HashAddEntry(adaHashTable, 'close', kLibraryToken);
	HashAddEntry(adaHashTable, 'eof', kLibraryToken);
	HashAddEntry(adaHashTable, 'eoln', kLibraryToken);
	HashAddEntry(adaHashTable, 'erase', kLibraryToken);
	HashAddEntry(adaHashTable, 'filepos', kLibraryToken);
	HashAddEntry(adaHashTable, 'filesize', kLibraryToken);
	HashAddEntry(adaHashTable, 'flush', kLibraryToken);
	HashAddEntry(adaHashTable, 'ioresult', kLibraryToken);
	HashAddEntry(adaHashTable, 'read', kLibraryToken);
	HashAddEntry(adaHashTable, 'readln', kLibraryToken);
	HashAddEntry(adaHashTable, 'rename', kLibraryToken);
	HashAddEntry(adaHashTable, 'reset', kLibraryToken);
	HashAddEntry(adaHashTable, 'rewrite', kLibraryToken);
	HashAddEntry(adaHashTable, 'seek', kLibraryToken);
	HashAddEntry(adaHashTable, 'seekeof', kLibraryToken);
	HashAddEntry(adaHashTable, 'seekeoln', kLibraryToken);
	HashAddEntry(adaHashTable, 'settextbuf', kLibraryToken);
	HashAddEntry(adaHashTable, 'truncate', kLibraryToken);
	HashAddEntry(adaHashTable, 'write', kLibraryToken); 
	HashAddEntry(adaHashTable, 'writeln', kLibraryToken);

	HashAddEntry(adaHashTable, 'chdir', kLibraryToken);
	HashAddEntry(adaHashTable, 'getdir', kLibraryToken);
	HashAddEntry(adaHashTable, 'paramcount', kLibraryToken);
	HashAddEntry(adaHashTable, 'paramstr', kLibraryToken);
	HashAddEntry(adaHashTable, 'mkdir', kLibraryToken);
	HashAddEntry(adaHashTable, 'rmdir', kLibraryToken);
	HashAddEntry(adaHashTable, 'rewrite', kLibraryToken);

	HashAddEntry(adaHashTable, 'abs', kLibraryToken);
	HashAddEntry(adaHashTable, 'arctan', kLibraryToken);
	HashAddEntry(adaHashTable, 'cos', kLibraryToken);
	HashAddEntry(adaHashTable, 'dec', kLibraryToken);
	HashAddEntry(adaHashTable, 'exp', kLibraryToken);
	HashAddEntry(adaHashTable, 'frac', kLibraryToken);
	HashAddEntry(adaHashTable, 'hi', kLibraryToken);
	HashAddEntry(adaHashTable, 'lo', kLibraryToken);
	HashAddEntry(adaHashTable, 'odd', kLibraryToken);
	HashAddEntry(adaHashTable, 'pi', kLibraryToken);
	HashAddEntry(adaHashTable, 'power', kLibraryToken);
 	HashAddEntry(adaHashTable, 'random', kLibraryToken);
	HashAddEntry(adaHashTable, 'randomize', kLibraryToken);
	HashAddEntry(adaHashTable, 'round', kLibraryToken);
	HashAddEntry(adaHashTable, 'sin', kLibraryToken);
	HashAddEntry(adaHashTable, 'sqr', kLibraryToken);
	HashAddEntry(adaHashTable, 'sqrt', kLibraryToken);
	HashAddEntry(adaHashTable, 'swap', kLibraryToken);
	HashAddEntry(adaHashTable, 'trunc', kLibraryToken);
	
	HashAddEntry(adaHashTable, 'ord', kLibraryToken);
	HashAddEntry(adaHashTable, 'pred', kLibraryToken);
	HashAddEntry(adaHashTable, 'succ', kLibraryToken);
	HashAddEntry(adaHashTable, 'sizeof', kLibraryToken);
	HashAddEntry(adaHashTable, 'sqrt', kLibraryToken);
	HashAddEntry(adaHashTable, 'include', kLibraryToken);
	HashAddEntry(adaHashTable, 'exclude', kLibraryToken);
	
	HashAddEntry(adaHashTable, 'binstr', kLibraryToken);
	HashAddEntry(adaHashTable, 'chr', kLibraryToken);
	HashAddEntry(adaHashTable, 'concat', kLibraryToken);
	HashAddEntry(adaHashTable, 'copy', kLibraryToken);
	HashAddEntry(adaHashTable, 'delete', kLibraryToken);
	HashAddEntry(adaHashTable, 'hexstr', kLibraryToken);
	HashAddEntry(adaHashTable, 'insert', kLibraryToken);
	HashAddEntry(adaHashTable, 'length', kLibraryToken);
	HashAddEntry(adaHashTable, 'lowercase', kLibraryToken);
	HashAddEntry(adaHashTable, 'octstr', kLibraryToken);
	HashAddEntry(adaHashTable, 'pos', kLibraryToken);
	HashAddEntry(adaHashTable, 'setlength', kLibraryToken);
	HashAddEntry(adaHashTable, 'setstring', kLibraryToken);
	HashAddEntry(adaHashTable, 'str', kLibraryToken);
	HashAddEntry(adaHashTable, 'stringofchar', kLibraryToken);
	HashAddEntry(adaHashTable, 'upcase', kLibraryToken);
	HashAddEntry(adaHashTable, 'val', kLibraryToken);
end;


procedure GetAdaToken(data: AnsiString; bufferLength: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);
var
	s: ansistring;
begin
	while (data[pos] in [CR, LF, TAB, ' ']) and (pos < bufferLength) do pos := pos + 1;
	tokenStart := pos;
	if data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
	begin
{Change 070326: Added '.' so Object Pascal code will get methods nicely listed in the function menu}
		while (data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.']) and (pos < bufferLength) do pos := pos + 1;
		tokenEnd := pos - 1;
		{Find out what alphanumerical symbol it is by looking it up in the hash table!}
		SetLength(s, pos - tokenStart);
		BlockMove(@data[tokenStart], @s[1], pos - tokenStart);
		// MoveBytes
		
		tokenType := HashLookUpInteger(adaHashTable, s);
		tokenValue := s;
	end
	else
	begin {strings, comments, compiler directives, math operators, brackets, & other singletons }
	
		if (data[pos] = '{') and (data[pos+1] = '$') then // compiler directive
		begin
			pos := pos + 2;
			while (data[pos] <> '}') and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos;
			pos := pos+1;
			tokenType := kCompilerDirectiveToken;
		end
		else
		if data[pos] = '{' then // Bracket-comment
		begin
			while (data[pos] <> '}') and (pos < bufferLength) do pos := pos + 1;
			pos := pos+1;
			tokenEnd := pos;
			tokenType := kCommentToken;
//			pos := pos + 1; // Test, try to avoid the last character to be caught by someone else
			// NOTE! This is most likely needed for other cases!
		end
		else
		if (data[pos] = '-') and (data[pos+1] = '-') then // Line-comment
		begin
			while not (data[pos] in [CR, LF]) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos - 1;
			tokenType := kCommentToken;
		end
		else
		if (data[pos] = '(') and (data[pos+1] = '*') then // Block-comment
		begin
			pos := pos + 2;
			while ((data[pos-1] <> '*') or (data[pos] <> ')')) and (pos < bufferLength) do pos := pos + 1;
			pos := pos+1;
			tokenEnd := pos;
			tokenType := kCommentToken;
//			pos := pos + 1; // Test, try to avoid the last character to be caught by someone else
			// NOTE! This is most likely needed for other cases!
		end
		else
		if (data[pos] = '"') then // String
		begin
// Funkar INTE - varför?
			pos := pos + 1;
//			while (data[pos] <> '''') and (pos < bufferLength) do pos := pos + 1;
			while not (data[pos] in ['"', CR, LF]) and (pos < bufferLength) do
			begin
				//WriteLn(data[pos], ' Char(', Ord(data[pos]), ') is not a quote, Char(', Ord(''''), ')');
				pos := pos + 1;
			end;
			//WriteLn('Found a quote, end of comment');
			// Borde också ha stöd för '' i sträng
			tokenEnd := pos;
			pos := pos + 1; // Skip '
			tokenType := kStringToken;
			// Should we set tokenValue here?
		end
		else
		begin
		// Otherwise skip the symbol
		// Must do special case for most symbols! Or assume that they are single char!
		//	if data[pos] in ['+','-', ':', '=', '<', '>', '/', '*'] then // Single char token
		//	begin
		//	end;
		tokenEnd := tokenStart;
//				tokenEnd := pos;
				tokenType := kSingleCharToken;
				tokenValue := data[pos]; // Ord(data[pos]);
				pos := pos + 1;
			//while not (dataPtr in ['a'..'z', 'A'..'Z', '0'..'9', '_', CR, LF, ' ']) do pos := pos + 1;
		end;
	end;
end;

procedure AdaColorCoder(var s: SnapShot; teRec: HaldaPtr; editIndex: Integer);
var
	data: AnsiString;
	tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
	iItemCount: Integer;
	untilHere: Longint;
begin
	untilHere := s.pos + kCCStepLength;
	with s do
	begin
	// Turn off redraw while color coding
//		SetTXNVisibility(teRec, false);

		data := teRec^.text; // CharsPtr(chars[editIndex]^);
		bufferLength := Length(data); // bufferLength borde skippas?
			
		repeat
			GetAdaToken(data, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
			
			// History
			if (not hasSeenUnit) and (not hasSeenProgram) then
			if (tokenValue = 'procedure') or (tokenValue = 'function') then
			begin
				hasSeenProgram := true;		// Mark window as gogo-able and runnable
				editWindIsMain := true;
			end;
			if tokenValue = 'package' then
				hasSeenUnit := true;		// Mark window as compileable without building bundle
	//		if tokenValue = 'interface' then
	//			hasSeenInterface := true;
	//		if tokenValue = 'implementation' then	// Signals that "procedure" and "function" may be candidates for function menu
	//			hasSeenImplementation := true;
			
			case tokenType of
			kFunctionToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VFunctionColor, LWPSintax.VFunctionStyle);
			kBeginEndToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
			kKnownTypeToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VKnowTypeColor, LWPSintax.VKnowTypeStyle);
			kReservedToken:
			begin
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
				
	// Parse "with" clause
					if tokenType = kReservedToken then
					if not hasSeenProgram and not hasSeenUnit then // Only at top
						if tokenValue = 'with' then
						// Found "uses", read everything until a ";" is found.
						begin
							repeat
								GetToken(data, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
								// NOTE! This may need to include more types. Some recognised words may be accepted as unit names.
								if tokenType in [kOtherToken, kCarbonToken] then
								begin
									// Append findings to list.
									tokenValue := GetTokenInCase(teRec, tokenStart, tokenEnd);
									SetLength(usesList, Length(usesList)+1);
									usesList[Length(usesList)-1] := tokenValue;
	//								AppendMenu(usesMenu, tokenValue + extension); // + file extension
								end;
							until (pos >= bufferLength) or (tokenType = kSingleCharToken) and (tokenValue = ';');
						end;

			end;
			kCommentToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VCommentColor, LWPSintax.VCommentStyle);
			kStringToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VStringColor, LWPSintax.VStringStyle);
			kSpecialToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VSpecialColor, LWPSintax.VSpecialStyle);
			kLibraryToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VLibraryColor, LWPSintax.VLibraryStyle);
			kClassToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VClassColor, LWPSintax.VClassStyle);
			kCarbonToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VCarbonColor, LWPSintax.VCarbonStyle);
			kCocoaToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VCocoaColor, LWPSintax.VCocoaStyle);
			kSingleCharToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VSingleCharColor, LWPSintax.VSingleCharStyle);
			kCompilerDirectiveToken:
				SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VCompilerDirectiveColor, LWPSintax.VCompilerDirectiveStyle);
			kOtherToken:
				if previousTokenType = kFunctionToken then
				begin
					SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VOtherColor, LWPSintax.VOtherStyle);
					{Good base for a function menu, but some items should be filtered out.}
					{- Only after implementation or program}
					{- Not "forward" ("forward" could remove the latest?)}
					{- Not at function type declaration}
					
					if editIndex > 0 then
						if hasSeenUnit or hasSeenProgram then // Den här är hyfsat lätt, de andra är knepigare
						begin
							AddToEditFunctionArrays(s, tokenValue, tokenStart, tokenEnd);	
						end
						else
							WriteLn(possibleFunctionTokenValue, ' NOT added to function menu arrays (Java)');
					// Hur ska jag sortera bort de andra? Ta bort en rad vid forward? Men funktionstyper då? Kolla token före procedure/funktion? = eller : betyder funktiontyp?
				end;
				
			otherwise
			end;

		secondPreviousTokenValue := previousTokenValue;
		secondPreviousTokenType := previousTokenType;
		
		previousTokenValue := tokenValue;
		previousTokenType := tokenType;

		until (pos >= bufferLength) or (pos >= untilHere);
	end; // with
	
//	SetTXNVisibility(teRec, true);
	// Redraw!
//	TXNForceUpdate(teRec); done by caller
//	DisposeHandle(s.chars);
end; {AdaColorCoder}



{------------------------ C COLOR CODER ----------------------------}

procedure InitCColorCodingTable;
begin
	HashAddEntry(cHashTable, 'if', kReservedToken); // Was kBeginEndToken  Ingemar 080626
	HashAddEntry(cHashTable, 'else', kReservedToken); // Was kBeginEndToken  Ingemar 080626
	HashAddEntry(cHashTable, 'struct', kReservedToken); // Ingemar 080626
	HashAddEntry(cHashTable, 'class', kReservedToken); 
	HashAddEntry(cHashTable, 'goto', kReservedToken); // Ingemar 080626
	HashAddEntry(cHashTable, 'for', kReservedToken);
	HashAddEntry(cHashTable, 'do', kReservedToken);
	HashAddEntry(cHashTable, 'while', kReservedToken); // kBeginEndToken?
	HashAddEntry(cHashTable, 'repeat', kReservedToken); // kBeginEndToken?
	HashAddEntry(cHashTable, 'break', kReservedToken); // kBeginEndToken?
	HashAddEntry(cHashTable, 'switch', kReservedToken);
	HashAddEntry(cHashTable, 'case', kReservedToken);
	HashAddEntry(cHashTable, 'default', kReservedToken); // Ingemar 080626
	HashAddEntry(cHashTable, 'return', kReservedToken); // kBeginEndToken?
	HashAddEntry(cHashTable, 'typedef', kReservedToken);
	
	HashAddEntry(cHashTable, 'enum', kKnownTypeToken); // Ingemar 080626
	HashAddEntry(cHashTable, 'long', kKnownTypeToken);
	HashAddEntry(cHashTable, 'int', kKnownTypeToken);
	HashAddEntry(cHashTable, 'float', kKnownTypeToken);
	HashAddEntry(cHashTable, 'double', kKnownTypeToken);
	HashAddEntry(cHashTable, 'void', kKnownTypeToken);
	HashAddEntry(cHashTable, 'char', kKnownTypeToken);
	HashAddEntry(cHashTable, 'signed', kKnownTypeToken); // Ingemar 080626
	HashAddEntry(cHashTable, 'unsigned', kKnownTypeToken); // Ingemar 080626
	HashAddEntry(cHashTable, 'register', kKnownTypeToken); // Ingemar 080626
	HashAddEntry(cHashTable, 'pascal', kKnownTypeToken); // Ingemar 080626
	HashAddEntry(cHashTable, 'static', kKnownTypeToken); // Ingemar 091126
	
	// First try to add some Cuda-specific color coding:
	HashAddEntry(cHashTable, '__shared__', kReservedToken); // Cuda 100222
	HashAddEntry(cHashTable, '__global__', kReservedToken); // Cuda 100222
	HashAddEntry(cHashTable, '__device__', kReservedToken); // Cuda 100222
	HashAddEntry(cHashTable, '__host__', kReservedToken); // Cuda 100222
	HashAddEntry(cHashTable, '__constant__', kReservedToken); // Cuda 100222
	HashAddEntry(cHashTable, 'blockIdx', kLibraryToken); // Cuda 100222
	HashAddEntry(cHashTable, 'threadIdx', kLibraryToken); // Cuda 100222
	HashAddEntry(cHashTable, 'blockDim', kLibraryToken); // Cuda 100222
	HashAddEntry(cHashTable, 'gridDim', kLibraryToken); // Cuda 100222
	HashAddEntry(cHashTable, 'float2', kReservedToken); // Cuda 100222
	HashAddEntry(cHashTable, 'float3', kReservedToken); // Cuda 100222
	HashAddEntry(cHashTable, 'float4', kReservedToken); // Cuda 100222
	HashAddEntry(cHashTable, '__synchthreads', kLibraryToken); // Cuda 100222
	HashAddEntry(cHashTable, 'cudaThreadSynchonize', kLibraryToken); // Cuda 100222
	HashAddEntry(cHashTable, 'cudaMalloc', kLibraryToken); // Cuda 100222
	HashAddEntry(cHashTable, 'cudaMemcpy', kLibraryToken); // Cuda 100222
	HashAddEntry(cHashTable, 'cudaFree', kLibraryToken); // Cuda 100222

	// Some OpenCL-specific color coding:
	HashAddEntry(cHashTable, '__kernel', kReservedToken); // OpenCL 100309
	HashAddEntry(cHashTable, '__global', kReservedToken); // OpenCL 100309
	HashAddEntry(cHashTable, '__local', kReservedToken); // OpenCL 100309
	HashAddEntry(cHashTable, '__constant', kReservedToken); // OpenCL 100309
	HashAddEntry(cHashTable, '__private', kReservedToken); // OpenCL 100309
	HashAddEntry(cHashTable, '__attribute__', kReservedToken); // OpenCL 100309
	HashAddEntry(cHashTable, 'half', kKnownTypeToken); // OpenCL 100309
	HashAddEntry(cHashTable, 'imaginary', kKnownTypeToken); // OpenCL 100309
	HashAddEntry(cHashTable, 'complex', kKnownTypeToken); // OpenCL 100309
	// Some GLSL
	HashAddEntry(cHashTable, 'uniform', kKnownTypeToken); // GLSL 100309
	HashAddEntry(cHashTable, 'attribute', kKnownTypeToken); // GLSL 100309
	HashAddEntry(cHashTable, 'varying', kKnownTypeToken); // GLSL 100309
	HashAddEntry(cHashTable, 'vec3', kKnownTypeToken); // GLSL 100309
	HashAddEntry(cHashTable, 'vec4', kKnownTypeToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_ModelViewMatrix', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_ProjectionMatrix', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_NormalMatrix', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_LightSource', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_Color', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_Normal', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_Vertex', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_MultiTexCoord0', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_MultiTexCoord1', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_MultiTexCoord2', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_MultiTexCoord3', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_FrontColor', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_TexCoord', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_FragCoord', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'gl_FragColor', kReservedToken); // GLSL 100309
	HashAddEntry(cHashTable, 'dot', kLibraryToken); // GLSL 100309
	HashAddEntry(cHashTable, 'max', kLibraryToken); // GLSL 100309
	HashAddEntry(cHashTable, 'min', kLibraryToken); // GLSL 100309
	HashAddEntry(cHashTable, 'sin', kLibraryToken); // GLSL 100309
	HashAddEntry(cHashTable, 'cos', kLibraryToken); // GLSL 100309
	HashAddEntry(cHashTable, 'ftransform', kLibraryToken); // GLSL 100309
	
	// Should we really add these manually or parse them? There are so many!
	// Hard to decide which ones are truly important.
	HashAddEntry(cHashTable, 'free', kLibraryToken); // C lib 100222
	HashAddEntry(cHashTable, 'malloc', kLibraryToken); // C lib 100222
	HashAddEntry(cHashTable, 'printf', kLibraryToken); // C lib 100222
	HashAddEntry(cHashTable, 'scanf', kLibraryToken); // C lib 100222
	HashAddEntry(cHashTable, 'sscanf', kLibraryToken); // C lib 100222
	HashAddEntry(cHashTable, 'calloc', kLibraryToken); // C lib 100222
	HashAddEntry(cHashTable, 'memcpy', kLibraryToken); // C lib 100222
	HashAddEntry(cHashTable, 'strcmp', kLibraryToken); // C lib 100222
	
	HashAddEntry(cHashTable, 'extern', kLinkageToken); // C Linkage 160129
end;
{Parser that parses C}
{Comments // and /**/ must be handled}

procedure GetCToken(data: AnsiString; bufferLength, editIndex: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);Overload;
var
	s: AnsiString;
	i,k : LongInt;
	filesList: AnsiStringArray;
	frontMain: WindowPtr;
	frontMainEditIndex, eIOpenFile: LongInt;
begin
	//pick previous and secondPreviousTokenValue/Type
	secondPrevTokenValue:= prevTokenValue;
	secondPrevTokenType:= prevTokenType;
	secondPrevTokenStart:= prevTokenStart;
	secondPrevTokenEnd:= prevTokenEnd;
	
	prevTokenValue:= tokenValue;
	prevTokenType:= tokenType;
	prevTokenStart:= tokenStart;
	prevTokenEnd:= tokenEnd;
	classType:=0;
	 //writeln('tokenValue: ', tokenValue, 'tokenStart: ', tokenStart, ' tokenEnd: ', tokenEnd, ' prevTokenValue: ', prevTokenValue, ' prevTokenStart: ', prevTokenStart, ' prevTokenEnd: ', prevTokenEnd);
	if Length(data) = 0 then Exit(GetCToken);
	
	while (data[pos] in [CR, LF, TAB, ' ']) and (pos <= bufferLength) do pos := pos + 1;
	tokenStart := pos;
	if data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
	begin
		// 101006: Allows : for better C++ navigation.
		while (data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_'{, ':'}]) and (pos < bufferLength) do 
		pos := pos + 1;
// Maybe I should do the following too, to avoid inclusion of ":" in other cases? It will at least catch most irrelevant cases.
// But should NOT affect ObjC methods!
//		if data[pos-1] = ':' then pos := pos - 1; // ":" last is skipped
		tokenEnd := pos - 1;
		{Look up alphanumerical symbol}
		//SetLength(s, pos - tokenStart);
		//BlockMove(@data[tokenStart], @s[1], pos - tokenStart);
		// MoveBytes
		s := Copy(data, tokenStart, pos - tokenStart);
//		WriteLn('*** New s = "', s, '"');
//		s := LowerCase(s);
		
		tokenType := HashLookUpInteger(cHashTable, s);

		//Look for classHashtables of project files (both opened files and closed files).
		// 160229: SKIPPED by Ingemar since this makes the C parser horribly slow.
		// It makes double loops over all dependent files for ALL alphanumeric identifiers!
		// This is unreasonable. Many of these tests are made multiple times for the same
		// file - and it is still just two layers rather than the complete list of files.
		if false then
		if (editIndex>0) then
		begin
			classType := HashLookUpInteger(classNamesHashTable[editIndex], s);
			//writeln('$classType: ', classType, ' tokenValue: ', s, ' editIndex: ', editIndex);
			//Get editIndex of frontMain file.	
			frontMain := GetFrontMainWindow;
			frontMainEditIndex:= getWRefCon(frontMain);
			filesList:= gProjectFiles[editIndex];
			if not (classType =kObjectToken) then
				for i := Low(filesList) to High(filesList) do 
				begin
					//writeln('$filesList: ', filesList[i]);
					eIOpenFile := FileIsOpen(filesList[i]);
					if eIOpenFile > 0 then
					begin
						if not (classType =kObjectToken) then
							classType := HashLookUpInteger(classNamesHashTable[eIOpenFile], s);
						//writeln('****classType: ', classType, 'tokenValue: ', tokenValue);
					end
					else
					begin
						if not (classType =kObjectToken) then
							if frontMainEditIndex>0 then
							for k:=low(gClosedFilesVariablesData[frontMainEditIndex])  to High(gClosedFilesVariablesData[frontMainEditIndex]) do
							begin
								//writeln('$$filesList: ', filesList[i]);
								//writeln('$$$filesList: ', gClosedFilesVariablesData[frontMainEditIndex][k].fileName);
								if (filesList[i] = gClosedFilesVariablesData[frontMainEditIndex][k].fileName) then
								begin
									classType := HashLookUpInteger(gClosedFilesVariablesData[frontMainEditIndex][k].fileClassNamesHashTable, s);
									//writeln('$classType: ', classType);
								end;
							end;	
					end;
				end;		
		end;
		
		tokenValue := s;
	end
	else
	begin {Other token, (, ), }
	// Test if { /* or //
		if data[pos] in ['{', '}'] then // Begin, end
		begin
			tokenEnd := tokenStart; //  + 1;
			tokenType := kBeginEndToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = '#' then // preprocessor junk
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kPreprocessorToken;
			//tokenValue := data[pos]; // Ord(data[pos]);

			while not (data[pos] in [CR, LF]) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos;//- 1;
			
			//SetLength(tokenValue, tokenEnd - tokenStart);
			//BlockMove(@data[tokenStart], @tokenValue[1], tokenEnd - tokenStart);
			
			tokenValue := Copy(data, tokenStart, tokenEnd - tokenStart);
			// MoveBytes
			
			pos := pos + 1;
		end
		else
		if data[pos] = '(' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kStartParenToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = ')' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kEndParenToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = '[' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kStartParenToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = ']' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kEndParenToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
			//writeln('tV:', tokenValue);
		end
		else
		if (data[pos] = '/') and (data[pos+1] = '/') then // Line-comment
		begin
			while not (data[pos] in [CR, LF]) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos - 1;
			tokenType := kCommentToken;
			
			// Check for LWP specials
			if tokenEnd - tokenStart > 3 then
			begin
				//SetLength(tokenValue, tokenEnd - tokenStart + 1);
				//BlockMove(@data[tokenStart], @tokenValue[1], tokenEnd - tokenStart + 1);
				tokenValue := Copy(data, tokenStart, tokenEnd - tokenStart+1);


			end;
		end
		else
		if (data[pos] = '/') and (data[pos+1] = '*') then // Comment
		begin
			pos := pos + 2;
			while ((data[pos-1] <> '*') or (data[pos] <> '/')) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos;
			tokenType := kCommentToken;
		end
		else
		if (data[pos] = '"') then // String
		begin
			pos := pos + 1;
// Might get problems with \? Should handle that
			while not (data[pos] in ['"', CR, LF]) and (pos < bufferLength) do
			begin
				//WriteLn(data[pos], ' Char(', Ord(data[pos]), ') is not a quote, Char(', Ord(''''), ')');
				if data[pos] = '\' then
					pos := pos + 1;
				pos := pos + 1;
			end;
			tokenEnd := pos;
			pos := pos + 1; // Skip "
			tokenType := kStringToken;

			tokenStart := tokenStart; // + 1;
			{Must get string contents (for #include menu)}
			//SetLength(tokenValue, tokenEnd - tokenStart-1);
			//BlockMove(@data[tokenStart+1], @tokenValue[1], tokenEnd - tokenStart-1);
			tokenValue := Copy(data, tokenStart+1, tokenEnd - tokenStart-1);

		end
		else
		if (data[pos] = '''') then // String (character)
		begin
			pos := pos + 1;
			while not (data[pos] in ['''', CR, LF]) and (pos < bufferLength) do
			begin
				//WriteLn(data[pos], ' Char(', Ord(data[pos]), ') is not a quote, Char(', Ord(''''), ')');
				if data[pos] = '\' then
					pos := pos + 1;
				pos := pos + 1;
			end;
			tokenEnd := pos;
			pos := pos + 1; // Skip '
			tokenType := kStringToken;

			tokenStart := tokenStart; // + 1;
			{Get string contents (not necessary for ')}
			//SetLength(tokenValue, tokenEnd - tokenStart+1);
			//BlockMove(@data[tokenStart+1], @tokenValue[1], tokenEnd - tokenStart-1);
			tokenValue := Copy(data, tokenStart+1, tokenEnd - tokenStart-1);

		end
		else
		begin
		// Otherwise skip the symbol
		// Must do special case for most symbols! Or assume that they are single char!
		//	if data[pos] in ['+','-', ':', '=', '<', '>', '/', '*'] then // Single char token
		//	begin
		//	end;
			tokenEnd := tokenStart + 1;
			tokenType := kSingleCharToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
			//while not (dataPtr in ['a'..'z', 'A'..'Z', '0'..'9', '_', CR, LF, ' ']) do pos := pos + 1;
		end;
	end;
end;

procedure GetCToken(data: AnsiString; bufferLength: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);Overload;
begin
	GetCToken(data, bufferLength, 0, pos, tokenStart, tokenEnd, tokenType, tokenValue);
end;

function FrameWorkExists(frameworkName: AnsiString): Boolean;
var
	err: OSErr;
	info: stat;
begin
	err:=fpStat('/System/Library/Frameworks/' + frameworkName + '.framework', info);
	if err <> noErr then
		err:=fpStat('/Library/Frameworks/' + frameworkName + '.framework', info);
// Borde även söka HEMDIR/Library/Frameworks!
// samt LOKALA frameworks - FILNAMN.frameworks-mapp?
	return err = noErr;
end;

function iOSFrameworkExists(frameworkName: AnsiString): Boolean;
var
	err: OSErr;
	info: stat;
begin
	if (giOSSDK = '') or (giOSSDK = kProcessUtilsFailedString) then
		CheckiOSSDKVersion;
				
	err:=fpStat(kiOSSDKPath + giOSSDK + '.sdk/System/Library/Frameworks/' + frameworkName + '.framework', info);
	return err = noErr;
// /Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS4.0.sdk/System/Library/Frameworks
end;

// For the new //uses framework etc, new system for C files.
procedure CheckSpecialComment(line: AnsiString; var usesList, frameworkList: AnsiStringArray; var extraOptions, extraLinkOptions: AnsiString); // ; editIndex: Integer);
var
	pos, pos2: Longint;
	tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
	frameworkName: AnsiString;
	i: Longint;
begin
//	line := line + ' ';
	pos := 3;
//	WriteLn('Checking "', line, '"');
	GetCToken(line, Length(line), pos, tokenStart, tokenEnd, tokenType, tokenValue);
//	WriteLn('tokenValue = ', tokenValue);
	if (tokenvalue = 'use') or (tokenvalue = 'uses') then
	begin
//		WriteLn('This is a "uses" special comment!');
		GetCToken(line, Length(line), pos, tokenStart, tokenEnd, tokenType, tokenValue);
//		WriteLn('Next tokenValue = "', tokenValue, '"');
		if tokenValue = 'framework' then
		begin
			GetCToken(line, Length(line), pos, tokenStart, tokenEnd, tokenType, tokenValue);
//			WriteLn('special comment framework ', tokenvalue);
			if FrameWorkExists(tokenValue) then
			begin
//				WriteLn('framework "', tokenValue, '" exists');				
				// Se efter om den redan finns
				for i := 0 to High(frameworkList) do
					if frameworkList[i] = tokenValue then Exit(CheckSpecialComment);
				SetLength(frameworkList, Length(frameworkList)+1);
				frameworkList[High(frameworkList)] := tokenValue;
//				WriteLn('Added framework "', tokenValue, '"');
			end
			else
				WriteLn('framework "', tokenValue, '" does not exist!');
		end
		else
		if (tokenValue = 'library') or (tokenValue = 'lib') then
		begin
		// Borde ta allt fram till mellanslag, eller i alla fall klara minustecken.
			pos2 := pos + 1;
			while not (line[pos2] in ['''', CR, LF, ' ']) and (pos2 < Length(line)-1) do
				pos2 += 1;
			tokenValue := Copy(line, pos, pos2-pos);
//			GetCToken(@line[1], Length(line), pos, tokenStart, tokenEnd, tokenType, tokenValue);
//			WriteLn('Library ', tokenValue, ' wanted');
			if not (Copy(tokenValue, 1, 2) = '-l') then
				tokenValue := '-l' + tokenValue;
			extraLinkOptions := extraLinkOptions + ' ' + tokenValue;
		end
		else
		if (tokenValue = 'option') or (tokenValue = 'options') then
		begin
//			GetCToken(@line[1], Length(line), pos, tokenStart, tokenEnd, tokenType, tokenValue);
			// Or rest of line? Yes!
			tokenValue := Copy(line, pos+1, Length(line) - pos);
//			WriteLn('Option ', tokenValue, ' wanted');
			extraOptions := extraOptions + ' ' + tokenValue;
		end
		else
		begin
			// FIXA: Fastnar på "." i filnamn!
			while not (line[tokenEnd] in ['''', CR, LF, ' ']) and (tokenEnd < Length(line)-1) do
				tokenEnd += 1;
			tokenValue := Copy(line, tokenStart+1, tokenEnd-tokenStart+1);
			
			// Se efter om den redan finns
			for i := 0 to High(usesList) do
				if usesList[i] = tokenValue then
				begin
//					WriteLn('DUPLICATE NOT ADDED TO USES LIST: "', tokenValue, '"');
					Exit(CheckSpecialComment);
				end;
			SetLength(usesList, Length(usesList)+1);
			usesList[High(usesList)] := tokenValue;
//			WriteLn('ADDED TO USES LIST: "', tokenValue, '"');
		end;
	end;
end;

procedure AnalyzePreprocessorLine(line: AnsiString; var usesList, frameworkList: AnsiStringArray; editIndex: Integer);
var
	pos, i: Longint;
	tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
	frameworkName: AnsiString;
begin
	if Length(line) < 1 then Exit(AnalyzePreprocessorLine);
	//WriteLn('*** AnalyzePreprocessorLine ***');
	//WriteLn(line);
	pos := 2;
	GetCToken(line, Length(line), pos, tokenStart, tokenEnd, tokenType, tokenValue);
	// Has to be include or import or we are not interested
	// (Pragma mark would be fun too though.)
	if tokenValue <> 'include' then
		if tokenValue <> 'import' then
		begin
//			WriteLn(tokenValue, ' unknown #');
			Exit(AnalyzePreprocessorLine);
		end;
//	WriteLn('OK: ', tokenValue);
	GetCToken(line, Length(line), pos, tokenStart, tokenEnd, tokenType, tokenValue);
	if tokenType = kStringToken then
	begin
		for i := 0 to High(usesList) do
			if usesList[i]=tokenValue then Exit(AnalyzePreprocessorLine);
//		WriteLn('Local include: "', tokenValue, '"');
		// Relevant for uses menu! Add to it!
		// Append to a list of strings?
		SetLength(usesList, Length(usesList)+1);
		usesList[High(usesList)] := tokenValue;
		Exit(AnalyzePreprocessorLine);
	end;
	if tokenValue <> '<' then Exit(AnalyzePreprocessorLine); // We don't know what it is
	GetCToken(line, Length(line), pos, tokenStart, tokenEnd, tokenType, tokenValue);
	// This might be the framework name!
	frameworkName := tokenValue;
	// If the next is "/" then it is a framework!
	GetCToken(line, Length(line), pos, tokenStart, tokenEnd, tokenType, tokenValue);
	if tokenValue = '/' then
	begin
//		WriteLn('Found framework: "', frameworkName, '"');
		// Append To Framework List
		if FrameWorkExists(frameworkName) then
		begin
//			WriteLn('framework ', frameworkName, ' exists for Mac');
			SetLength(frameworkList, Length(frameworkList)+1);
			frameworkList[High(frameworkList)] := frameworkName;
		end
		else
		if iOSFrameworkExists(frameworkName) then
		begin
//			WriteLn('framework ', frameworkName, ' exists for iPhone');
			SetLength(frameworkList, Length(frameworkList)+1);
			frameworkList[High(frameworkList)] := frameworkName;
			if editIndex >= 0 then
//				if editMainWindIsProgram[editIndex] then
					editWindIsiOSProgram[editIndex] := true;
		end;
	end;
//	WriteLn('*** AnalyzePreprocessorLine END ***');
end;

procedure FinishCBrackets(chars:AnsiString; var pos:LongInt; bracketCount, untilHere, bufferLength:LongInt);
var
	pos1:LongInt;
	tokenStart, tokenEnd, tokenType: Longint;
	tokenValue: AnsiString;
	bracketFound:Boolean;
begin
	pos1:=pos;
		//writeln('Entered FinishCBrackets');
	repeat
		//writeln('AAA');	
		repeat GetCToken(chars, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
		until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
		bracketFound:= true;
		//writeln('For$$$: tokenValue: ', tokenValue, ' bracketCount: ', bracketCount);
		if (tokenValue='(') or (tokenValue='[') or (tokenValue='{') then
		begin
			bracketCount+=1;
			//writeln('bracketCount: ', bracketCount);
		end;

		if (tokenValue=')') or (tokenValue=']') or (tokenValue='}')  then
		begin	
			bracketCount-=1;
			if bracketCount<=0 then
				bracketFound:= false;	
			//writeln('For1: tokenValue: ', tokenValue, ' bracketCount: ', bracketCount);
		end;
	//writeln(' bracketCount: ', bracketCount, ' pos1: ', pos1, ' bufferLength: ', bufferLength, 'untilHere: ', untilHere);
	until not (bracketFound) or (pos1 >= bufferLength) or (pos1 >= untilHere); 
	pos:=pos1;
end;


procedure CColorCoder(var s: SnapShot; teRec: HaldaPtr; editIndex: Integer);
var
	data: AnsiString;
	tokenStart, tokenEnd, tokenType: Longint;
	{prevTokenStart, prevTokenEnd,} balance: Longint;
	tokenValue: AnsiString;
	
	iItemCount: Integer;
	untilHere: Longint;
	
	pos1, pos2:LongInt;
	bracketCount:LongInt;
	bracketFound: Boolean;
	tempClassMember, tempType: AnsiString;
	gotClassMember:Boolean;
	functionParsing:Boolean;
	dataInParen:AnsiString;
	startBracketPos, endBracketPos:LongInt;
	dataInParenParsing, gotRecordName: Boolean;
	i:LongInt;
	externParsing: Boolean;
	
	procedure GetCVariablesForParensData(data:AnsiString);// posData:LongInt{; scope:AnsiString});
	var
	posData:LongInt;
	begin
		posData:=0;
		repeat
			GetCToken(data, Length(data), posData, tokenStart, tokenEnd, tokenType, tokenValue);
			with s do
			begin
				with lVariableMembers do
				begin
					if Length(editFunctionName) > 0 then // BUG!!!
					if (prevTokenType=kKnownTypeToken) then
					begin
						SetLength(variableName, Length(variableName)+1);
						variableName[High(variableName)]:= tokenValue;
						SetLength(variableType, Length(variableType) + 1);
						variableType[High(variableType)]:= prevTokenValue;
						SetLength(variableScope, Length(variableScope)+1);
						variableScope[High(variableScope)] :=  editFunctionName[High(editFunctionName)];
					end;
				end;
			end;
		until (tokenValue=';') or (posData>=Length(data));
	end;
	
	
	procedure GetCVariableList(data:AnsiString; pos1, untilHere, bufferLength: LongInt; tempType:AnsiString);
	begin
		//writeln('Entered GetCVariableList')
		with s do
		begin
			with lVariableMembers do
			begin
				repeat		
					GetCToken(data, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
					
					if (tokenValue=',') or (tokenValue='=') or (tokenValue=';') or (tokenType=kStartParenToken) then
					begin
						SetLength(variableName, Length(variableName)+1);
						variableName[High(variableName)]:= prevTokenValue;
						
						SetLength(variableScope, Length(variableScope)+1);
						if (balance=0) then
							variableScope[High(variableScope)] := 'global'
						else
						begin
							if High(editFunctionName) > 0 then
								variableScope[High(variableScope)] :=  editFunctionName[High(editFunctionName)];
						end;
					
						SetLength(variableType, Length(variableType) + 1);
						variableType[High(variableType)]:= tempType;
						//writeln('variableName: ', variableName[High(variableName)], ' variableType ', variableType[High(variableType)], ' variableScope: ', variableScope[High(variableScope)]);
					end;
					
				until (tokenType=kStartParenToken) or (tokenValue=';') or (tokenValue='=') or (pos1 >= bufferLength) or (pos1 >= untilHere);
			end;
		end;
	end;
begin
	untilHere := s.pos + kCCStepLength;
	with s do
	begin
		// Turn off redraw while color coding
//		SetTXNVisibility(teRec, false);
		
		data := teRec^.text;
		bufferLength := Length(data);
		pos := 1;
		
		// Special history variables
		tokenValue:='';
		tokenType:=kOtherToken;
		tokenStart:=0;
		tokenEnd:=0;
		previousTokenType := kOtherToken;
		previousTokenValue := '';
		secondPreviousTokenType := kOtherToken;
		secondPreviousTokenValue := '';
		prevTokenStart := 0;
		prevTokenEnd := 0;
		balance := 0;
		hasSeenImplementation := false;
		hasSeenInterface := false;
		hasSeenObjCMethod := false;
		parenBalance := 0;
		
		editWindIsiOSProgram := false;
		functionParsing:=false;
		gotClassMember:=false;
		//For C code completion
		HashInitTable(classNamesHashTable[editIndex]);
		dataInParenParsing:=false;
		gotRecordName:=false;
		
		externParsing:= false; // Too time consuming (needs to be fixed)
		
		{prevTokenValue:='';
		prevTokenStart:=0;
		prevTokenEnd:=0;
		secondPrevTokenValue:='';
		secondPrevTokenStart:=0;
		secondPrevTokenEnd:=0;}
		//writeln('data: ', data);
		//writeln('teRec^.text: ', teRec^.text);

		repeat
			GetCToken(teRec^.text, bufferLength, editIndex, pos, tokenStart, tokenEnd, tokenType, tokenValue);
			//writeln('tokenValue: ', tokenValue, ' pos: ',pos);
	// C is awkward to parse.
	// How do I find procedures?
	// sequence of 1 or more unknown alphanumeric tokens before "("?
	// Not enough. But when outside {}!
	// Requires more lookback than Pascal.
	
			{if ((balance=0) or (functionParsing)) then	
			if classType >0 then
				tokenType:=ClassType;}
			//writeln('classType: ', classType, ' tokenValue: ', tokenValue);
			if (classType=kObjectToken) then
			if not ((prevTokenValue='class') or (prevTokenValue='struct') or (prevTokenValue='private') or (prevTokenValue='public') or (prevTokenValue='protected')) then
			begin
				if not (dataInParenParsing) then
				begin
					//writeln('###*tokenValue: ',tokenValue);
					//writeln('###*prevTokenValue: ', prevTokenValue);
					if ((balance=0) or (functionParsing)) then
					begin
						//collect variable name after ‘,’. Sol: parse until ';’ or ‘(‘ or’[‘ (startparenToken) or ‘=‘.
						//Store the type in tempType.
						//repeat GetCToken until ';’ or ‘(‘ or ’[‘ (startparenToken) or ‘=‘.
						//if TV=',' or startParenToken or '=' then vn:=prevTokenValue, 
						tempType:=tokenValue;
						pos1:=pos;
						GetCVariableList(teRec^.text, pos1, untilHere, bufferLength, tempType);
					end;
				end;
			end;
			
			case tokenType of
			kStartParenToken:
				begin
					parenBalance := parenBalance + 1; // För ObjC-metoder
					
					if balance = 0 then // Balance refers to {} level
						currentParenIsFunction := false;
					
					// C functions
					if previousTokenType = kOtherToken then
					if not hasSeenObjCMethod then // Not in an ObjC method
					if not hasSeenInterface then // Not in an ObjC interface
					if balance = 0 then
					begin
//						WriteLn('C function ', previousTokenValue);
						SetInterval(teRec, prevTokenStart, prevTokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
//						previousTokenValue:=GetTokenInCase(teRec, prevTokenStart, prevTokenEnd);
// Unnecessary if the token is never damaged by LowerCase!
						AddToEditFunctionArrays(s, previousTokenValue, prevTokenStart, prevTokenEnd);

						currentParenIsFunction := true;
						if tokenValue='(' then
						begin
							startBracketPos:=pos;
							dataInParenParsing:=true;
						end;
						//writeln('###previousTokenValue: ', previousTokenValue);
						//writeln('###tokenValue: ', tokenValue);
						
						//ToDo: Top most variable name, type should be removed here, because It is a function, like void fun(). 
						//fun should be removed from variable list. Or may be not?
						with lVariableMembers do
						begin
							if (Length(variableName)>0) then
							begin
								SetLength(variableName, Length(variableName)-1);
								SetLength(variableScope, Length(variableScope)-1);
								SetLength(variableType, Length(variableType)-1);
							end;
						end;
						
						if previousTokenValue = 'main' then
						begin
							editWindIsMain := true;
							editMainWindIsProgram := true; // Needed to avoid confusion in CompilationDone (in BuildWithPascal), whichis common for both C and Pascal builds.
						end;
					end;
				end;
			kEndParenToken:
			begin
				parenBalance := parenBalance - 1; // För ObjC-metoder
				if (tokenValue=')') and (dataInParenParsing) then
				begin
					dataInParenParsing:=false;
				end;
				
			end;
			kSingleCharToken:
			begin
//				WriteLn('kSingleCharToken "', tokenValue, '"');
				// Check if a function definition ends by ");"
				if tokenValue = ';' then
				if previousTokenValue = ')' then
				if currentParenIsFunction then
				begin
//						WriteLn('Detected function prototype, should be removed');
					
//						iItemCount := CountMenuItems(editFunctionMenu[editIndex]);
//						DeleteMenuItem(editFunctionMenu[editIndex], iItemCount);
					
					//AddToGlobalFunctionsList
					
					if (Length(editFunctionNameStart)>0) then
					begin
						writeln('editCFunctionName: ', editFunctionName[High(editFunctionName)]);
						SetLength(editGlobalFunctions, Length(editGlobalFunctions)+1);
						editGlobalFunctions[High(editGlobalFunctions)] := editFunctionName[High(editFunctionName)]; 
		
						SetLength(editFunctionNameStart, Length(editFunctionNameStart)-1);
						SetLength(editFunctionNameEnd, Length(editFunctionNameEnd)-1);
						SetLength(editFunctionName, Length(editFunctionName)-1);
					end;
					
				end;
				
				// Detect start of ObjC methods
				if balance = 0 then // must be top level
				if hasSeenImplementation then // Has detected @implementation in the file
				if (tokenValue = '+') or (tokenValue = '-') then
				begin
					// Remember the start of the whole range plus the fact that we are in
					// an ObjC method (so we won't incorrectly detect parts of it as C functions)
					SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
					hasSeenObjCMethod := true;
					startOfObjCMethod := tokenEnd;
//					objCMethodName := objCClassName + ' '; // Prefix all methods with class name
					objCMethodName := '  '; // Just indent all methoids (class name will be at start of implementation)
				end;
			end;
			kPreprocessorToken:
			begin
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
				//writeln('kP tokenValue: ', tokenValue);
				AnalyzePreprocessorLine(tokenValue, usesList, frameworkList, editIndex);
			end;
			kFunctionToken:
			// Do we EVER get here?
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
			kBeginEndToken:
			begin
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
				if tokenValue = '{' then
				begin	
					if not externParsing then
						balance := balance + 1;
				end;
				if tokenValue = '}' then 
				begin
					if not externParsing then
						balance := balance - 1
					else
						externParsing:= false;
				end;
				
				//If currentParenIsFunction is still true, It means, It is a function as present TV is '{'.
				if currentParenIsFunction then
				begin
					endBracketPos:= pos-4;
					//dataInParenParsing:= false;
					dataInParen:= GetTokenInCase(teRec, startBracketPos, endBracketPos)+';';
					//writeln('dataInParen: ', dataInParen);
					GetCVariablesForParensData(dataInParen);//, posData:LongInt{; scope:AnsiString});
					functionParsing:= true; 
					//Collect data inside 
				end;
				
				
				if (balance=0) then
					functionParsing:=false;
				
				currentParenIsFunction := false; // If a function definition has been found, it has ended by now.

					// End of ObjC method
					if hasSeenObjCMethod then
					if tokenValue = '{' then
					begin
						hasSeenObjCMethod := false;
						if previousTokenType = kOtherToken then
						begin
							// Similar to C function above but with some significant changes
							SetInterval(teRec, prevTokenStart, prevTokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
//							WriteLn('ObjC function ', objCMethodName);
							AddToEditFunctionArrays(s, objCMethodName, startOfObjCMethod, prevTokenEnd);
						end;
					end;
			end;
			kKnownTypeToken {or kObjectToken}:// or (classType=kObjectToken): //To Do: or classNameToken; The hash table should ne unique to each project. This might be tricky or pretty simple.
			begin
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VKnowTypeColor, LWPSintax.VKnowTypeStyle);
				//writeln('dataInParenParsing: ', dataInParenParsing);
				if not (dataInParenParsing) then
				begin
					if ((balance=0) or (functionParsing)) then
					begin
						//collect variable name after ‘,’. Sol: parse until ';’ or ‘(‘ or’[‘ (startparenToken) or ‘=‘.
						//Store the type in tempType.
						//repeat GetCToken until ';’ or ‘(‘ or ’[‘ (startparenToken) or ‘=‘.
						//if TV=',' or startParenToken or '=' then vn:=prevTokenValue, 
						tempType:=tokenValue;
						pos1:=pos;
						GetCVariableList(data, pos1, untilHere, bufferLength, tempType);

						{SetLength(variableName, Length(variableName)+1);
						SetLength(variableScope, Length(variableScope)+1);
						SetLength(variableType, Length(variableType)+1);
						variableName[High(variableName)]:= tokenValue;
						variableType[High(variableType)]:= prevTokenValue;
						if (balance=0) then
							variableScope[High(variableScope)] := 'global'
						else
							variableScope[High(variableScope)] :=  editFunctionName[High(editFunctionName)];}

					end;
				end;
				
			end;
			kReservedToken:
			begin
				//TODO: Here C Code completon code should be developed. 
				
				if (tokenValue = 'struct') or (tokenValue = 'class') then
				begin
				//writeln('Entered kReservedToken');
				//Get record name and Search for it If it exist in editRecordVariables[editIndex][0..High]. If It doesn't has, then increase length of editRecordVariables.
					SetLength(lRecordVariables, Length(lRecordVariables)+1);
					
					with lRecordVariables[High(lRecordVariables)] do 
					begin
						pos1:=pos;
						GetCToken(data, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
						//writeln('tokenValue: ', tokenValue, ' pos1: ', pos1);
						//Get one more tokenValue
						GetCToken(data, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
						
						
						//Support for inheritance
						if false then // DISABLED since it CRASHES!
						If not (tokenValue='{'){(tokenValue=':')} then
						begin
							recordName := prevTokenValue;
							//Add ClassName to corresponding Hash table.
							HashAddEntry(classNamesHashTable[editIndex], recordName, kObjectToken);
							gotRecordName:=true;
							//writeln('**$$className: ', prevTokenValue, ' recordName: ', recordName, ' editIndex: ', editIndex);
							//WriteLn('**$$myClassName returns: ', HashLookUpInteger(classNamesHashTable[editIndex], recordName));
							repeat 
								GetCToken(data, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
								//writeln('tokenValue in inheritance section: ', tokenValue);
								if (tokenValue=',') or (tokenValue='{') then
								begin
									inheritedObjectName:= prevTokenValue;
									//writeln('inheritedObjectName: ', inheritedObjectName);
									inheritedList:= SearchForInheritedObjInRVList(s, editIndex, inheritedObjectName);//Search for inherited objectName in record variable list, and get it's members list If found.
									{for i:= low(inheritedList) to High(inheritedList) do
									begin
										writeln( 'inheritance list: ',i, ' ', inheritedList[i]);
									end;}
									
									//Add the members list to the current object or record.
									for i:= low(inheritedList) to High(inheritedList) do
									begin
										SetLength(recordMembers, Length(recordMembers)+1);
										recordMembers[High(recordMembers)] := inheritedList[i];
									end;
									//recordMembers := inheritedList;
								end;
								
							until (tokenValue='{') or (pos1>untilHere) or (pos1>bufferLength);
							
						end;
						
						//writeln('tokenValue: ', tokenValue, ' pos1: ', pos1);
						//Collect className and class members
						If tokenValue='{' then
						begin
							//className:=pTV;
							if not gotRecordName then
							begin
								recordName:=prevTokenValue;
								//Add ClassName to corresponding Hash table.
								HashAddEntry(classNamesHashTable[editIndex], recordName, kObjectToken);
								//writeln('**className: ', prevTokenValue);
								//WriteLn('**myClassName returns ', HashLookUpInteger(classNamesHashTable[editIndex], recordName));
							end
							else
								gotRecordName:=false;
							
							//writeln('className: ', prevTokenValue);
							//WriteLn('myClassName returns ', HashLookUpInteger(classNamesHashTable[editIndex], recordName));
							if (balance=0) then
								recordScope:= 'global'
							else
								if Length(editFunctionName) > 0 then // Ingemar found yet another case that breaks! 160310
									recordScope:= editFunctionName[High(editFunctionName)];
							repeat 
								GetCToken(data, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
								//writeln('$$$$tokenValue: ', tokenValue);
								
								if (tokenValue='(') or (tokenValue='[')  or (tokenValue='{')then
								begin
									if (tokenValue='(') or (tokenValue='[') then
									begin
										bracketFound := true;
										tempClassMember := GetTokenInCase(teRec, prevTokenStart, prevTokenEnd);//prevTokenValue;
										//writeln('tempClassMember: ', tempClassMember, ' prevTokenValue: ', prevTokenValue, ' prevTokenStart: ', prevTokenStart, ' prevTokenEnd: ', prevTokenEnd);
									end
									else if (tokenValue='{') then
										if gotClassMember then
											gotClassMember:=false;
									(*if (tokenValue='{') then //For methods eg: functioname () {return value}; 
									begin
										writeln('classMember: ', prevTokenValue);
									end;*)
									bracketCount:=0;
									bracketCount+=1;
									pos2:=pos1;
									FinishCBrackets(data, pos2, bracketCount, untilHere, bufferLength);
									pos1:=pos2;
									//writeln('pos2: ', pos2, 'bracketCount: ', bracketCount);
								end;
									
								if (tokenValue=';') or (tokenValue=',') or (tokenValue='('{for constructor or functions})then
								//classMember:=pTV:
								begin
									SetLength(recordMembers, Length(recordMembers)+1);
									if {(tokenValue=';') and} (bracketFound) then
									begin
										recordMembers[High(recordMembers)] {recMember}:= tempClassMember; //GetTokenInCase(data, secondPrevTokenStart, secondPrevTokenEnd)
										//writeln('classMember bcoz of brackets: ', tempClassMember, ' ', recordMembers[High(recordMembers)]);
										bracketFound := false;
										if (tokenValue='('{for constructor or functions}) then
											gotClassMember:=true;
									end
									else
										if not gotClassMember then
										begin
											recordMembers[High(recordMembers)]:= GetTokenInCase(teRec, prevTokenStart, prevTokenEnd);//prevTokenValue;
											//writeln('classMember: ', prevTokenValue, ' ', recordMembers[High(recordMembers)]);
										end
										else
											gotClassMember:= false;
								end;
							until( tokenValue='}') or (pos1>untilHere) or (pos1>bufferLength);
							
							//Should parse little more till ';' for object If exists
							pos2:=pos1;
							repeat
								GetCToken(data, bufferLength, pos2, tokenStart, tokenEnd, tokenType, tokenValue);
								//writeln('&***tokenValue: ', tokenValue);
									with lVariableMembers do
									begin
									if ((tokenValue=';') and not (prevTokenValue='}'))then
									begin
										SetLength(variableName, Length(variableName)+1);
										variableName[High(variableName)]:= prevTokenValue;
										
										SetLength(variableScope, Length(variableScope)+1);
										if (balance=0) then
											variableScope[High(variableScope)] := 'global'
										else
											if Length(editFunctionName) > 0 then // Yet another range error! 160310
											variableScope[High(variableScope)] :=  editFunctionName[High(editFunctionName)];
									
										SetLength(variableType, Length(variableType) + 1);
										variableType[High(variableType)]:= recordName;
										//writeln('&&&Object Name from class: ', variableName[High(variableName)], 'ObjectType: ', variableType[High(variableType)]);

									end;
								end;
							until( tokenValue=';') or (pos2>untilHere) or (pos2>bufferLength);
							tokenValue:= 'class';
							
							//objName := tV;
							//objType := pTV;
						end;//If tokenValue='{' then
					end;//with lRecordVariables
				end;//if struct

				
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
			end;
			kCommentToken:
			begin
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VCommentColor, LWPSintax.VCommentStyle);
				if length(tokenValue) > 3 then { Check for //uses, new special LWP preprocessor }
					CheckSpecialComment(tokenValue + CR, usesList, frameworkList, extraOptions, extraLinkOptions); // editIndex);
			end;
			kStringToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VStringColor, LWPSintax.VStringStyle);
			kOtherToken:
	//			if previousTokenType = kPreprocessorToken then
				begin
	//				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VOtherColor, LWPSintax.VOtherStyle);

					if previousTokenValue = '@' then
					begin
						if tokenValue = 'implementation' then
						begin
							hasSeenImplementation := true;
							SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
						end;
						if tokenValue = 'interface' then
						begin
							hasSeenImplementation := false;
							hasSeenInterface := true;
							SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
						end;
						if tokenValue = 'end' then
						begin
							hasSeenImplementation := false;
							hasSeenInterface := false;
							SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
						end;
					end;
					
					// ObjC class name
					if previousTokenValue = 'implementation' then
					begin
						tokenValue:= GetTokenInCase(teRec, tokenStart, tokenEnd);
						objCClassName := tokenValue;
						SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
						
						AddToEditFunctionArrays(s, tokenValue + ' implementation', prevTokenStart, tokenEnd);
					end;
					if previousTokenValue = 'interface' then
					begin
						SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
						// Put interface start in menu?
						
						AddToEditFunctionArrays(s, tokenValue + ' interface', prevTokenStart, tokenEnd);
					end;
					
					if hasSeenObjCMethod then
						if parenBalance = 0 then
						begin
							SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
							// Del av ObjC-metod
							objCMethodName := objCMethodName + tokenValue;
						end;
				end;
				
			kLinkageToken:
			begin
				if tokenValue = 'extern' then
					externParsing := true;
			end;
				
			otherwise
			end;
			
			//writeln();
			secondPreviousTokenValue := previousTokenValue;
			secondPreviousTokenType := previousTokenType;
			
			previousTokenValue := tokenValue;
			previousTokenType := tokenType;
			
			prevTokenStart := tokenStart;
			prevTokenEnd := tokenEnd;
			
		until (pos >= bufferLength) or (pos >= untilHere);
	end; // with
	
//	SetTXNVisibility(teRec, true);
end;




{------------------------ JAVA COLOR CODER ----------------------------}

procedure InitJavaColorCodingTable;
begin
	HashAddEntry(javaHashTable, 'if', kBeginEndToken);
	HashAddEntry(javaHashTable, 'else', kBeginEndToken);
	HashAddEntry(javaHashTable, 'typedef', kReservedToken);
	HashAddEntry(javaHashTable, 'for', kReservedToken);
	HashAddEntry(javaHashTable, 'do', kReservedToken);
	HashAddEntry(javaHashTable, 'while', kReservedToken); // kBeginEndToken?
	HashAddEntry(javaHashTable, 'repeat', kReservedToken); // kBeginEndToken?
	HashAddEntry(javaHashTable, 'switch', kReservedToken);
	HashAddEntry(javaHashTable, 'case', kReservedToken);
	HashAddEntry(javaHashTable, 'return', kReservedToken);
	HashAddEntry(javaHashTable, 'try', kReservedToken);
	HashAddEntry(javaHashTable, 'catch', kReservedToken);
	HashAddEntry(javaHashTable, 'long', kKnownTypeToken);
	HashAddEntry(javaHashTable, 'int', kKnownTypeToken);
	HashAddEntry(javaHashTable, 'float', kKnownTypeToken);
	HashAddEntry(javaHashTable, 'double', kKnownTypeToken);
	HashAddEntry(javaHashTable, 'void', kKnownTypeToken);
	HashAddEntry(javaHashTable, 'char', kKnownTypeToken);

	HashAddEntry(javaHashTable, 'static', kReservedToken);
	HashAddEntry(javaHashTable, 'public', kReservedToken);
	HashAddEntry(javaHashTable, 'class', kClassToken);
	HashAddEntry(javaHashTable, 'import', kReservedToken);
end;
{Parser som parsar C}
{Men hur?}
{Kommentarer av typ // och /**/ måste hanteras}

procedure GetJavaToken(data: AnsiString; bufferLength: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);
var
	s: AnsiString;
begin
	while (data[pos] in [CR, LF, TAB, ' ']) and (pos < bufferLength) do pos := pos + 1;
	tokenStart := pos;
	if data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
	begin
		while (data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) and (pos < bufferLength) do pos := pos + 1;
		tokenEnd := pos - 1;
		{Analysera vilken alfanumerisk symbol det är!}
		SetLength(s, pos - tokenStart);
		BlockMove(@data[tokenStart], @s[1], pos - tokenStart);
		//tokenValue := Copy(data, tokenStart, pos - tokenStart);
		
		tokenType := HashLookUpInteger(javaHashTable, s);
//		if s = 'main' then editWindIsMain[] := true;
		
		tokenValue := s;
	end
	else
	begin {Other token, (, ), }
	// Test if { /* or //
		if data[pos] in ['{', '}'] then // Begin, end
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kBeginEndToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = '(' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kStartParenToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = ')' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kEndParenToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if (data[pos] = '/') and (data[pos+1] = '/') then // Line-comment
		begin
			while not (data[pos] in [CR, LF]) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos - 1;
			tokenType := kCommentToken;
		end
		else
		if (data[pos] = '/') and (data[pos+1] = '*') then // Comment
		begin
			pos := pos + 2;
			while ((data[pos-1] <> '*') or (data[pos] <> '/')) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos;
			tokenType := kCommentToken;
		end
		else
		if (data[pos] = '"') then // String
		begin
			pos := pos + 1;
// Might get problems with \? Should handle that
			while not (data[pos] in ['"', CR, LF]) and (pos < bufferLength) do
			begin
				//WriteLn(data[pos], ' Char(', Ord(data[pos]), ') is not a quote, Char(', Ord(''''), ')');
				pos := pos + 1;
			end;
			//WriteLn('Found a quote, end of comment');
			// Borde också ha stöd för '' i sträng
			tokenEnd := pos;
			pos := pos + 1; // Skip '
			tokenType := kStringToken;

			tokenStart := tokenStart + 1;
			{Must get string contents (for #include menu)}
			SetLength(tokenValue, tokenEnd - tokenStart);
			BlockMove(@data[tokenStart], @tokenValue[1], tokenEnd - tokenStart);
			// MoveBytes

		end
		else
		begin
		// Otherwise skip the symbol
		// Must do special case for most symbols! Or assume that they are single char!
		//	if data[pos] in ['+','-', ':', '=', '<', '>', '/', '*'] then // Single char token
		//	begin
		//	end;
			tokenEnd := tokenStart + 1;
			tokenType := kSingleCharToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
			//while not (dataPtr in ['a'..'z', 'A'..'Z', '0'..'9', '_', CR, LF, ' ']) do pos := pos + 1;
		end;
	end;
end;



procedure JavaColorCoder(var s: SnapShot; teRec: HaldaPtr; editIndex: Integer);
var
	data: AnsiString;
//	err: OSErr;
//	typeAttr: array [1..2] of TXNTypeAttributes;
//	color: RGBColor;
	tokenStart, tokenEnd, tokenType: Longint;
	prevTokenStart, prevTokenEnd, balance: Longint;
	tokenValue: AnsiString;
//	iItemCount: Integer;
	untilHere: Longint;
begin
//WriteLn('JavaColorCoder starts at ', s.pos);
	untilHere := s.pos + kCCStepLength;
	with s do
	begin
	// Turn off redraw while color coding
//		SetTXNVisibility(teRec, false);
		
		data := teRec^.text;
		bufferLength := Length(data);
		pos := 0;
		
		// Special history variables
		previousTokenType := kOtherToken;
		previousTokenValue := '';
		secondPreviousTokenType := kOtherToken;
		secondPreviousTokenValue := '';
		prevTokenStart := 0;
		prevTokenEnd := 0;
		balance := 0;
		parenBalance := 0;

		editWindIsMain := false; // Added 130925
		editMainWindIsProgram := false; // Added 130925 - means "applet or main" for Java.
		
		repeat
			if pos > 16490 then
				WriteLn('GetJavaToken at ', pos);
			GetJavaToken(data, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
			if pos > 16490 then
				WriteLn('GetJavaToken OK, value = ', tokenValue);
			
			case tokenType of
			kStartParenToken:
	// Vid STARTPARENTES efter kOtherToken: Förra elementet möjlig funktionsdefinition.
	// Efter SLUTPARENTES kommer oftast semikolon eller startfiskmås. I fallet startfiskmås
	// så har vi en funktionsdefinition!
	// Parentesbalans måste kollas på vägen. Måste vara på samma parentesnivå!
				begin
					if previousTokenType = kOtherToken then
	//				if balance = 0 then
					begin
						possibleFunctionTokenStart := prevTokenStart;
						possibleFunctionTokenEnd := prevTokenEnd;
						possibleFunctionTokenValue := previousTokenValue;
						possibleFunctionParenBalance := parenBalance;
					end;

					parenBalance := parenBalance + 1;
				end;
			kEndParenToken:
				begin
					// No special action here.
					parenBalance := parenBalance - 1;
				end;
	//		kPreprocessorToken:
	//		begin
	//			SetInterval(teRec, tokenStart, tokenEnd, kColorBlue, kStyleNormal);
	//		end;
			kFunctionToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VFunctionColor, LWPSintax.VFunctionStyle);
			kBeginEndToken:
			begin
				if previousTokenType = kEndParenToken then
					if parenBalance = possibleFunctionParenBalance then // Måste ha samma parentesnivå
					begin
						SetInterval(teRec, possibleFunctionTokenStart, possibleFunctionTokenEnd, LWPSintax.VFunctionColor, LWPSintax.VFunctionStyle);
						AddToEditFunctionArrays(s, ' '+possibleFunctionTokenValue, possibleFunctionTokenStart, possibleFunctionTokenEnd);
//						AppendMenu(editFunctionMenu[editIndex], ' '+possibleFunctionTokenValue); // Extra space to separate functions from classes
						
						if previousTokenValue = 'main' then
						begin
							editWindIsMain := true;
							editMainWindIsProgram := true; // main = program, not applet
						end;
					end;

				WriteLn('Danger?4 tokenEnd = ', tokenEnd, ' Length = ', Length(data));
				// Range error här:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
				WriteLn('Danger?5');
				if tokenValue = '{' then balance := balance + 1;
				if tokenValue = '}' then balance := balance - 1;
			end;
			kKnownTypeToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VKnowTypeColor, LWPSintax.VKnowTypeStyle);
			kReservedToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
			kCommentToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VCommentColor, LWPSintax.VCommentStyle);
			kStringToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VStringColor, LWPSintax.VStringStyle);
			kOtherToken:
				if previousTokenType = kClassToken then
				// Try catching "class" declarations too
				begin
						SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
						AddToEditFunctionArrays(s, tokenValue, tokenStart, tokenEnd);
				end
				else
				if (tokenValue = 'Applet') or (tokenValue = 'java.applet.Applet') then // 160323: Added check for 'java.applet.Applet' /Ingemar
					if previousTokenValue = 'extends' then
					begin // Applet detected!
						editWindIsMain := true;
						editMainWindIsProgram := false; // Applet - not program
					end;
				
			otherwise
			end;

			secondPreviousTokenValue := previousTokenValue;
			secondPreviousTokenType := previousTokenType;
		
			previousTokenValue := tokenValue;
			previousTokenType := tokenType;
		
			prevTokenStart := tokenStart;
			prevTokenEnd := tokenEnd;

		until (pos >= bufferLength) or (pos >= untilHere);
	end; // with
	
//	SetTXNVisibility(teRec, true);
	// Redraw!
//	TXNForceUpdate(teRec); done by caller
WriteLn('JavaColorCoder done');
end;




{------------------------ SH COLOR CODER ----------------------------}

procedure InitShColorCodingTable;
begin
	HashAddEntry(shHashTable, 'if', kBeginEndToken);
	HashAddEntry(shHashTable, 'then', kBeginEndToken);
	HashAddEntry(shHashTable, 'else', kBeginEndToken);
	HashAddEntry(shHashTable, 'fi', kBeginEndToken);
	HashAddEntry(shHashTable, 'echo', kReservedToken);
	HashAddEntry(shHashTable, 'for', kReservedToken);
	HashAddEntry(shHashTable, 'done', kReservedToken);
	HashAddEntry(shHashTable, 'do', kReservedToken);
//	HashAddEntry(shHashTable, 'while', kReservedToken); // kBeginEndToken?
//	HashAddEntry(shHashTable, 'repeat', kReservedToken); // kBeginEndToken?
	HashAddEntry(shHashTable, 'case', kReservedToken);
	HashAddEntry(shHashTable, 'in', kReservedToken);
	HashAddEntry(shHashTable, 'esac', kReservedToken);
	HashAddEntry(shHashTable, 'return', kReservedToken);
//	HashAddEntry(shHashTable, 'try', kReservedToken);
//	HashAddEntry(shHashTable, 'catch', kReservedToken);
//	HashAddEntry(shHashTable, 'long', kKnownTypeToken);
//	HashAddEntry(shHashTable, 'int', kKnownTypeToken);
//	HashAddEntry(shHashTable, 'float', kKnownTypeToken);
//	HashAddEntry(shHashTable, 'double', kKnownTypeToken);
//	HashAddEntry(shHashTable, 'void', kKnownTypeToken);
//	HashAddEntry(shHashTable, 'char', kKnownTypeToken);

//	HashAddEntry(shHashTable, 'static', kReservedToken);
//	HashAddEntry(shHashTable, 'public', kReservedToken);
//	HashAddEntry(shHashTable, 'class', kClassToken);
//	HashAddEntry(shHashTable, 'import', kReservedToken);
end;
{Parser som parsar C}
{Men hur?}
{Kommentarer av typ // och /**/ måste hanteras}

procedure GetShToken(data: AnsiString; bufferLength: Longint; var pos, tokenStart, tokenEnd, tokenType: Longint; var tokenValue: AnsiString);
var
	s: AnsiString;
begin
	while (data[pos] in [CR, LF, TAB, ' ']) and (pos < bufferLength) do pos := pos + 1;
	tokenStart := pos;
	if data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] then
	begin
		while (data[pos] in ['a'..'z', 'A'..'Z', '0'..'9', '_']) and (pos < bufferLength) do pos := pos + 1;
		tokenEnd := pos - 1;
		{Analysera vilken alfanumerisk symbol det är!}
		SetLength(s, pos - tokenStart);
		BlockMove(@data[tokenStart], @s[1], pos - tokenStart);
		// MoveBytes
		
		tokenType := HashLookUpInteger(shHashTable, s);
//		if s = 'main' then editWindIsMain[] := true;
		
		tokenValue := s;
	end
	else
	begin {Other token, (, ), }
	// Test if { (* or //
		if data[pos] in ['{', '}'] then // Begin, end
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kBeginEndToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = '(' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kStartParenToken;
			tokenValue := data[pos]; // Ord(dataPtr^[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = ')' then
		begin
			tokenEnd := tokenStart + 1;
			tokenType := kEndParenToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
		end
		else
		if data[pos] = '#' then // Line-comment
		begin
			while not (data[pos] in [CR, LF]) and (pos < bufferLength) do pos := pos + 1;
			tokenEnd := pos - 1;
			tokenType := kCommentToken;
		end
		else
//		if (data[pos] = '/') and (data[pos+1] = '*') then // Comment
//		begin
//			pos := pos + 2;
//			while ((data[pos-1] <> '*') or (data[pos] <> '/')) and (pos < bufferLength) do pos := pos + 1;
//			tokenEnd := pos;
//			tokenType := kCommentToken;
//		end
//		else
		if (data[pos] = '"') then // String 1
		begin
			pos := pos + 1;
// Might get problems with \? Should handle that
			while not (data[pos] in ['"', CR, LF]) and (pos < bufferLength) do
			begin
				//WriteLn(data[pos], ' Char(', Ord(data[pos]), ') is not a quote, Char(', Ord(''''), ')');
				pos := pos + 1;
			end;
			//WriteLn('Found a quote, end of comment');
			// Borde också ha stöd för '' i sträng
			tokenEnd := pos;
			pos := pos + 1; // Skip '
			tokenType := kStringToken;

			tokenStart := tokenStart + 1;
			{Must get string contents (for #include menu)}
			SetLength(tokenValue, tokenEnd - tokenStart);
			BlockMove(@data[tokenStart], @tokenValue[1], tokenEnd - tokenStart);
			// MoveBytes
		end
		else
		if (data[pos] = '''') then // String 2
		begin
			pos := pos + 1;
// Might get problems with \? Should handle that
			while not (data[pos] in ['''', CR, LF]) and (pos < bufferLength) do
			begin
				//WriteLn(data[pos], ' Char(', Ord(data[pos]), ') is not a quote, Char(', Ord(''''), ')');
				pos := pos + 1;
			end;
			//WriteLn('Found a quote, end of comment');
			// Borde också ha stöd för '' i sträng
			tokenEnd := pos;
			pos := pos + 1; // Skip '
			tokenType := kStringToken;

			tokenStart := tokenStart + 1;
			{Must get string contents (for #include menu)}
			SetLength(tokenValue, tokenEnd - tokenStart);
			BlockMove(@data[tokenStart], @tokenValue[1], tokenEnd - tokenStart);
			// MoveBytes
		end
		else
		begin
		// Otherwise skip the symbol
		// Must do special case for most symbols! Or assume that they are single char!
		//	if data[pos] in ['+','-', ':', '=', '<', '>', '/', '*'] then // Single char token
		//	begin
		//	end;
			tokenEnd := tokenStart + 1;
			tokenType := kSpecialToken;
			tokenValue := data[pos]; // Ord(data[pos]);
			pos := pos + 1;
			//while not (dataPtr in ['a'..'z', 'A'..'Z', '0'..'9', '_', CR, LF, ' ']) do pos := pos + 1;
		end;
	end;
end;


procedure ShColorCoder(s: SnapShot; teRec: HaldaPtr; editIndex: Integer);
var
	data: AnsiString;
//	err: OSErr;
//	color: RGBColor;
	tokenStart, tokenEnd, tokenType: Longint;
	prevTokenStart, prevTokenEnd, balance: Longint;
	tokenValue: AnsiString;

//	iItemCount: Integer;
	untilHere: Longint;
begin
	untilHere := s.pos + kCCStepLength;
	with s do
	begin
		data := teRec^.text;
		bufferLength := Length(data);
		pos := 0;
		
		// Special history variables
		previousTokenType := kOtherToken;
		previousTokenValue := '';
		secondPreviousTokenType := kOtherToken;
		secondPreviousTokenValue := '';
		prevTokenStart := 0;
		prevTokenEnd := 0;
		balance := 0;
		parenBalance := 0;
		
		repeat
			GetShToken(data, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
			
			case tokenType of
			kStartParenToken:
	// This is the C/Java function identifier.
	// Must be rewritten for sh!
				begin
					if previousTokenType = kOtherToken then
	//				if balance = 0 then
					begin
						possibleFunctionTokenStart := prevTokenStart;
						possibleFunctionTokenEnd := prevTokenEnd;
						possibleFunctionTokenValue := previousTokenValue;
						possibleFunctionParenBalance := parenBalance;
					end;

					parenBalance := parenBalance + 1;
				end;
			kEndParenToken:
				begin
					// No special action here.
					parenBalance := parenBalance - 1;
				end;
	//		kPreprocessorToken:
	//		begin
	//			SetInterval(teRec, tokenStart, tokenEnd, kColorBlue, kStyleNormal);
	//		end;
			kFunctionToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VFunctionColor, LWPSintax.VFunctionStyle);
			kBeginEndToken:
			begin
				if previousTokenType = kEndParenToken then
					if parenBalance = possibleFunctionParenBalance then // Måste ha samma parentesnivå
					begin
						SetInterval(teRec, possibleFunctionTokenStart, possibleFunctionTokenEnd, LWPSintax.VFunctionColor, LWPSintax.VFunctionStyle);
						AddToEditFunctionArrays(s, ' '+possibleFunctionTokenValue, possibleFunctionTokenStart, possibleFunctionTokenEnd);
						if previousTokenValue = 'main' then editWindIsMain := true;
					end;
				
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
				if tokenValue = '{' then balance := balance + 1;
				if tokenValue = '}' then balance := balance - 1;
			end;
			kKnownTypeToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VKnowTypeColor, LWPSintax.VKnowTypeStyle);
			kReservedToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
			kCommentToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VCommentColor, LWPSintax.VCommentStyle);
			kStringToken:
				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VStringColor, LWPSintax.VStringStyle);
			kOtherToken:
				;
			otherwise
			end;

			secondPreviousTokenValue := previousTokenValue;
			secondPreviousTokenType := previousTokenType;
		
			previousTokenValue := tokenValue;
			previousTokenType := tokenType;
		
			prevTokenStart := tokenStart;
			prevTokenEnd := tokenEnd;

		until (pos >= bufferLength) or (pos >= untilHere);
	end;
end;

// ----------- end of sh color coder ------------

// Run one step of color coding for a certain window and a certain snapshot
procedure RunColorCoderOnWindow(editIndex: Longint; var s: SnapShot);
var
	theTitle: String; // Can be buffered so we don't have to get it every time
	extType: Integer;
	teRec: HaldaPtr;
	i:LongInt;
begin
	if editWind[editIndex] = nil then Exit(RunColorCoderOnWindow);
	if teEdit[editIndex] = nil then Exit(RunColorCoderOnWindow);

//	WriteLn('CC ', editIndex, ' from ', s.pos);
	
// Make sure that chars are loaded
	with s do
	if teEdit[editIndex] <> nil then
	begin
//		if chars[editIndex] = nil then
//		begin
//			TXNGetDataEncoded(teEdit[editIndex], kTXNStartOffset, kTXNEndOffset, Handle(chars[editIndex]), kTXNTextData);
//			HLock(chars[editIndex]);
//		end;
	end;
	teRec := teEdit[editIndex];
//	SetTXNVisibility(teRec, false);
	
		GetWTitle(editWind[editIndex], theTitle);
		extType := GetExtensionType(theTitle);
		case extType of
			kExtTypePascal: 
			begin
{$ifc defined LW_PASCAL_FORMATTER}
				PascalFormatter(teRec, editIndex); // Test - skall vara valbar
{$ENDC}
				PascalColorCoder(s, teRec, editIndex);
			end;
			kExtTypeC, kExtTypeCPP, kExtTypeObjC, kExtTypeCuda:
			begin
				CColorCoder(s, teRec, editIndex);
			end;
			kExtTypeGLSL, kExtTypeCL, kExtTypePHP: // C-style color coding that can not be main program
			begin
				CColorCoder(s, teRec, editIndex);
				editWindIsMain[editIndex] := false;
			end;
			kExtTypeJava:
			begin
				JavaColorCoder(s, teRec, editIndex);
			end;
			kExtTypeScript:
			if GetExtension(theTitle) = '.sh' then
			begin
				ShColorCoder(s, teRec, editIndex); // Only shellscript color coding supported so far
			end
			else
			begin
//				s.pos := max!!!
// This was a bug before: SetTextToNormal was called but the s.pos was not
// set to end, which caused repeated color coding calls as well as repeated SetTextToNormal.
				s.pos := Length(teRec^.text);
				SetTextToNormal(teRec, true, GetSettingsTextSize); // and all black
			end;
			kExtTypeAda:
			begin
				AdaColorCoder(s, teRec, editIndex);
			end;
		otherwise
			begin
//			s.pos := max!!!
// Same as above, fixed bug.
			s.pos := Length(teRec^.text); // Important! Why was this commented out?
			SetTextToNormal(teRec, true, GetSettingsTextSize);
			end;
		end; {case}
			
// Put back global lists
	usesList[editIndex] := s.usesList;
	frameworkList[editIndex] := s.frameworkList;
	editFunctionNameStart[editIndex] := s.editFunctionNameStart; 
	editFunctionNameEnd[editIndex] := s.editFunctionNameEnd;
	editFunctionName[editIndex] := s.editFunctionName;
	editGlobalFunctions[editIndex] := s.editGlobalFunctions;
	
	editRecordVariables[editIndex] := s.lRecordVariables;
	editVariableMembers[editIndex] := s.lVariableMembers;
	editRecordPointers[editIndex] := s.lRecordPointers;
	//writeln('Length of editGlobalFunctions: ', Length(editGlobalFunctions[editIndex]));
	{for i:=low(editGlobalFunctions[editIndex]) to High(editGlobalFunctions[editIndex]) do
	begin
		writeln('editIndex from colorcoding',editIndex);
		writeln('editRecordVariables: ', editGlobalFunctions[editIndex][i]);
	end;}
	
	{writeln('Length of editRecordVariables: ', Length(editRecordVariables[editIndex]));
	for i:=low(editRecordVariables[editIndex]) to High(editRecordVariables[editIndex]) do
	begin
		writeln('editIndex from colorcoding',editIndex);
		writeln('editRecordVariables: ', editRecordVariables[editIndex][i].recordName);
	end;}
	
	{for i:=low(editVariableMembers[editIndex].variableName) to High(editVariableMembers[editIndex].variableName) do
	begin
		writeln('editIndex from colorcoding: ',editIndex);
		writeln('editVariableName: ', editVariableMembers[editIndex].variableName[i]);
		if i<=High(editVariableMembers[editIndex].variableType) then
			writeln('editVariabletype: ', editVariableMembers[editIndex].variableType[i]);
		if i<=High(editVariableMembers[editIndex].variableScope) then
		writeln('editVariableScope: ', editVariableMembers[editIndex].variableScope[i]);
	end;}
	
	
	editWindIsMain[editIndex] := s.editWindIsMain;
	editMainWindIsProgram[editIndex] := s.editMainWindIsProgram;
	editWindIsCocoaProgram[editIndex] := s.editWindIsCocoaProgram;
	editWindIsiOSProgram[editIndex] := s.editWindIsiOSProgram;
	editWindIsJVMProgram[editIndex] := s.editWindIsJVMProgram;
	editWindIsAndroidProgram[editIndex] := s.editWindIsAndroidProgram; // JVM targets
	extraOptions[editIndex] := s.extraOptions;
	extraLinkOptions[editIndex] := s.extraLinkOptions;

//	SetTXNVisibility(teRec, true);
//	HSDViewSetNeedsDisplay(teEdit[editIndex], true); // Behövs bara vid öppna? Skippa här? Behövs när man kodar i synliga delen? Kolla!
	HInvalViews(teEdit[editIndex]);
	SynchPopups(editIndex);
end;

// Should we do more CC for this window?
function CheckIfCCDirty(editIndex: Longint): Boolean;
begin
	CheckIfCCDirty := false;
	// If data not loaded for window, load it
//	if chars[editIndex] = nil then
//	begin
//		TXNGetDataEncoded(teEdit[editIndex], kTXNStartOffset, kTXNEndOffset, Handle(chars[editIndex]), kTXNTextData);
//		HLock(chars[editIndex]);
//	end;
//	if Length(snap[editIndex]) < 1 then
//		if GetHandleSize(chars[editIndex]) > 0 then
//			CheckIfCCDirty := true
//		else
//			CheckIfCCDirty := false
//	else
//		if snap[editIndex][High(snap[editIndex])].pos < GetHandleSize(chars[editIndex]) then // The actual check
//			CheckIfCCDirty := true;

// This is WRONG - how should we check?
	if Length(snap[editIndex]) <= 0 then
	begin
		if Length(teEdit[editIndex]^.text) > 0 then
			CheckIfCCDirty := true;
	end
	else
		if snap[editIndex][High(snap[editIndex])].pos < Length(teEdit[editIndex]^.text) then // The actual check
			CheckIfCCDirty := true;
end;

// Continue on any CC that hasn't finished
procedure UpdateColorCoding;
var
	i, j, target: Longint;
	w, ww: WindowRef;
	//usesList: FileArr;
	//j: JtfnVariables;
	k: Longint;
begin
	//WriteLn('UpdateColorCoding');
	target := -1;
// 1) Is the front window CC'ed to end? If not, do that.
	w := GetFrontEditWindow;
	if w = nil then
		Exit(UpdateColorCoding);
	i := GetWRefCon(w);
	if (i > 0) and (i <= kMaxEditWindows) then
		if CheckIfCCDirty(i) then
		begin
			target := i;
//			WriteLn('Window ', target, ' should be CCed');
		end;
// 2) Pick the first other window (by number) that isn't CC'ed to end
	if target = -1 then
		for i := 1 to kMaxEditWindows do
			if editWind[i] <> nil then
				if CheckIfCCDirty(i) then
				begin
					target := i;
//					WriteLn('Window ', target, ' should be CCed');
				end;
// If we find an editIndex by any of these, run the color coder!
	if target > 0 then
	begin
		// Clone the last entry
		SetLength(snap[target], Length(snap[target])+1); // One more snap
		if Length(snap[target]) > 2 then
			snap[target][High(snap[target])] := snap[target][High(snap[target])-1]
		else
			InitSnapShot(snap[target][High(snap[target])], target); // Clears s, loads chars[] if needed

		// Run the last entry forward
//		WriteLn('Running CC on ', target);
		RunColorCoderOnWindow(target, snap[target][High(snap[target])]);
		
		// Are done with "target"? If so, good time to check if "target" have unknown file references!
		//	function BuildUsesList(teWind: WindowPtr; includePathInFileName: Boolean): FileArr;
		// TO DO: check above criteria, Check If the parsing of whole file has processed.
		if snap[target][High(snap[target])].pos >= Length(teEdit[target]^.text) then
		begin
			//writeln('BuildJTFNUsesList');
			gusesList[target] := BuildJTFNUsesList(editWind[target], true {withPath});
		end;
	end
	else
		UpdateJTFN;
//	  with j do
end;

// Minor change - just offset the data, don't re-color yet
procedure OffsetColorCodingData(editIndex: Longint; pos, change: Longint);
var
	i: Longint;
begin
// Offset if later than pos
	for i := 0 to High(editFunctionNameStart[editIndex]) do
		if editFunctionNameStart[editIndex][i] > pos then
			editFunctionNameStart[editIndex][i] += change;
	for i := 0 to High(editFunctionNameEnd[editIndex]) do
		if editFunctionNameEnd[editIndex][i] > pos then
			editFunctionNameEnd[editIndex][i] += change;
	// Save earliest recorded change to know where to start recoding?
	// Change the buffer size count in the SnapShot? Might cause problems.
end;

// Call to start CC from the beginning of a file!
procedure InitColorCodingForWindow(editIndex: Integer);
begin
	// Window just opened or changed name
	// Clear CC history
	SetLength(snap[editIndex], 0); // Clear CC history
	// Dispose old data
//	if chars[editIndex] <> nil then
//	begin
//		DisposeHandle(Handle(chars[editIndex]));
//		chars[editIndex] := nil;
//	end;
end;

// Mark a change at a certain position. Color coding is redone from that place
procedure TouchColorCoder(editIndex, pos: Longint);
var
	first, i: Longint;
begin
// Dispose old data (no longer valid)
//	if chars[editIndex] <> nil then
//	begin
//		DisposeHandle(Handle(chars[editIndex]));
//		chars[editIndex] := nil;
//	end;
// Clear all snapshots after pos
	first := -1;
	for i := 0 to High(snap[editIndex]) do
		if pos < snap[editIndex][i].pos then
		begin
			first := i;
			Leave;
		end;
	if first = -1 then Exit(TouchColorCoder);
	// Now first is index to the first before pos
	SetLength(snap[editIndex], first);
	// And set everything beyond this point to normal!
	SetInterval(teEdit[editIndex], pos, Length(teEdit[editIndex]^.text), kColorBlack, kStyleNormal);
	// Finally, one run CC! (To avoid flicker.)
//	RunColorCoderOnWindow(editIndex, snap[editIndex][High(snap[editIndex])]);
// Crashes!
	UpdateColorCoding; // Works!
end;

// Plan: Call this to invalidate a window's CC
// ResetColorCoder?
procedure ResetColorCoder(teRec: HaldaPtr; editIndex: Integer);
var
	extType: Integer;
	i, iItemCount: Integer;
//	theSpec: FSSpecString;
	err: OSErr;
	theTitle: Str255;
	lineTotal: ItemCount;
	s: SnapShot;
begin
	if editIndex > 0 then
		if editIndex <= kMaxEditWindows then
		begin
			SetLength(snap[editIndex], 1);
			InitSnapShot(snap[editIndex][0], editIndex); // Clears s, loads chars[] if needed
			
			RunColorCoderOnWindow(editIndex, snap[editIndex][0]); // Run once straight away
			for i := 0 to High(teEdit[editIndex]^.views) do
				HSDViewSetNeedsDisplay(teEdit[editIndex]^.views[i], true);
		end;
end;

(*	
	err := TXNGetLineCount (teRec, lineTotal);
	if (err <> noErr) or (lineTotal > kMaxLinesForColorCoding) then // Corrected 090416
		begin
			SetTXNVisibility(teRec, true);
			TXNForceUpdate(teRec);
			Exit(ResetColorCoder);
		end;
	SetTXNVisibility(teRec, false);

// Detect other extensions
		if editWind[editIndex] = nil then Exit(ResetColorCoder);
		GetWTitle(editWind[editIndex], theTitle);
		extType := GetExtensionType(theTitle);
		case extType of
			kExtTypePascal: 
			begin
{$ifc defined LW_PASCAL_FORMATTER}
				PascalFormatter(teRec, editIndex); // Test - skall vara valbar
{$ENDC}
				repeat
					PascalColorCoder(s, teRec, editIndex);
				until s.pos >= s.bufferLength;
			end;
			kExtTypeC, kExtTypeCPP, kExtTypeObjC, kExtTypeCuda:
			begin
				repeat
					CColorCoder(s, teRec, editIndex);
				until s.pos >= s.bufferLength;
			end;
			kExtTypeGLSL, kExtTypeCL, kExtTypePHP: // C-style color coding that can not be main program
			begin
				repeat
					CColorCoder(s, teRec, editIndex);
				until s.pos >= s.bufferLength;
				editWindIsMain[editIndex] := false;
			end;
			kExtTypeJava:
			begin
				repeat
					JavaColorCoder(s, teRec, editIndex);
				until s.pos >= s.bufferLength
			end;
			kExtTypeScript:
			if GetExtension(theTitle) = '.sh' then
			begin
				repeat
					ShColorCoder(s, teRec, editIndex) // Only shellscript color coding supported so far
				until s.pos >= s.bufferLength
			end
			else
				SetTextToNormal(teRec, true, GetSettingsTextSize); // and all black
			kExtTypeAda:
			begin
				repeat
					AdaColorCoder(s, teRec, editIndex);
				until s.pos >= s.bufferLength;
			end;
		otherwise
			SetTextToNormal(teRec, true, GetSettingsTextSize);
// Pajar undo vid ospecade filer. (Och alla andra förstås.)
//			; // Ignore
		end; {case}
	
// Put back global lists
	usesList[editIndex] := s.usesList;
	frameworkList[editIndex] := s.frameworkList;
	editFunctionNameStart[editIndex] := s.editFunctionNameStart;
	editFunctionNameEnd[editIndex] := s.editFunctionNameEnd;
	editFunctionName[editIndex] := s.editFunctionName;
	editWindIsMain[editIndex] := s.editWindIsMain;
	editMainWindIsProgram[editIndex] := s.editMainWindIsProgram;
	editWindIsCocoaProgram[editIndex] := s.editWindIsCocoaProgram;
	editWindIsiOSProgram[editIndex] := s.editWindIsiOSProgram;
	extraOptions[editIndex] := s.extraOptions;
	extraLinkOptions[editIndex] := s.extraLinkOptions;
	
	SetTXNVisibility(teRec, true);
	TXNForceUpdate(teRec); // Behövs bara vid öppna? Skippa här?
end; {BetterColorCoder}
*)

procedure InitColorCodingTables;
begin
//WriteLn('Initializing hash tables');
	HashInitTable(pascalHashTable, kOtherToken, 'FAIL');
	HashInitTable(cHashTable, kOtherToken, 'FAIL');
	HashInitTable(javaHashTable, kOtherToken, 'FAIL');
	HashInitTable(shHashTable, kOtherToken, 'FAIL');
	HashInitTable(adaHashTable, kOtherToken, 'FAIL');
	
//WriteLn('Filling hash tables');
	InitColorCodingTable;
//WriteLn('Filling hash tables 2');
	InitCColorCodingTable;
//WriteLn('Filling hash tables 3');
	InitJavaColorCodingTable;
//WriteLn('Filling hash tables 4');
	InitShColorCodingTable;
	InitAdaColorCodingTable;

	//WriteLn('Done filling hash tables');
	
	SkelBackgroundWithDuration(UpdateColorCoding, kEventDurationSecond / 10);

	//WriteLn('Done InitColorCodingTables');
end;

// ---------------- line number support -------------------

// This used to be a major performance problem. Now it is a simple lookup.
function GetRowFromCharOffset(editIndex: Longint; offset: Longint): Longint;
var
//	portRect: Rect;
	pos: Longint;
//	pt: Point;
	line: Longint;
//	theString: Str255;
	ch: Char;
	err: OSErr;
	txnObj: HaldaPtr;
const
	LF = Char(10);
	CR = Char(13);
begin
	GetRowFromCharOffset := HOffsetToRow(teEdit[editIndex], offset);
	Exit(GetRowFromCharOffset);

(*	
	if (editIndex < 1) or (editIndex > kMaxEditWindows) then Exit(GetRowFromCharOffset);
	txnObj := teEdit[editIndex];
	if chars[editIndex] = nil then
	begin
		TXNGetDataEncoded(teEdit[editIndex], kTXNStartOffset, kTXNEndOffset, Handle(chars[editIndex]), kTXNTextData);
		HLock(chars[editIndex]);
	end;
	
	pos := 0;
	line := 0;
	ch := Char(10);
	
//	parse for CR
	while pos < GetHandleSize(Handle(chars[editIndex])) do
	begin
		if ch in [CR, LF] then
		begin
			line := line + 1;
		end;	
		ch := CharsHandle(chars[editIndex])^^[pos];
		pos := pos + 1;
		if pos >= offset then
		begin
			return line;
		end;
	end;
	return line;*)
end;

(*
// Isn't this REALLY obsolete?! Use HOffsetToRow
function UpdateRowFromCharOffset(editIndex, pos: Longint): Longint;
var
	row, i: Longint;
begin
	if lastRow[editIndex] > 0 then // Known last row
	begin
		row := GetRowFromCharOffset(editIndex, pos);
	end
	else
	begin
		row := lastRow[editIndex];
		
		// Could make this faster by using rowStarts?
		if pos < positionOfLastRow[editIndex] then
		begin
			for i := pos to positionOfLastRow[editIndex] do
				if teEdit[editIndex]^.h^.text[i] = CR then
					row -= 1;
			// count CR
		end
		else
		begin
			for i := positionOfLastRow[editIndex] to pos do
				if teEdit[editIndex]^.h^.text[i] = CR then
					row += 1;
		end;
	end;
	lastRow[editIndex] := row;
	positionOfLastRow[editIndex] := pos;
	UpdateRowFromCharOffset := row;
end;
*)



// --------------------------------------------------------------------------


// Help function for BuildMenuFromUses etc
// Extracts a list of strings with all "uses" or "include"

// MÅSTE SKRIVAS OM för att klara filer som inte är öppna i editfönster!
// No longer used by BuildMenuFromUses, only from IncludeAnalyzer.
	function GetUsesListFromText(data: AnsiString; bufferLength: Longint; theTitle: Str255 {teWind: WindowPtr; teEdit: TXNObject}): AnsiStringArray;
	var
//		err: OSErr;
		pos, tokenStart, tokenEnd, tokenType: Longint;
		tokenValue, lowerCaseTokenValue: AnsiString;
//		extension: Str255;
//		cleanName: Str255; // För C-delen
		extType: Integer;
		
		list, dummyList: AnsiStringArray;
		dummyExtraOptions, dummyExtraLinkOptions: AnsiString;
		dataCopy: AnsiString;
		hasSeenImplementation: Boolean;
		hasSeenImplementationAt: Longint;
		readCUpToHere: Longint;
	const
		kHowMuchMore = 16384; // Limit on how long we may read
	begin
		SetLength(list, 0);
		SetLength(dummyList, 0);
		// Pascal -  search for uses
		// C/C++ - search for #include with "". (Ignore <> or handle separately.)
		// <> should be a separate feature, so LWP could search frameworks for headers too.
		
		pos := 1;
		hasSeenImplementation := false;
		hasSeenImplementationAt := pos;
		readCUpToHere := bufferLength;
		
		// Inspect file extension.
//		GetWTitle(teWind, theTitle);
//		extension := GetExtension(theTitle); // Används inte längre för test men dock för att lägga till i menyn
		extType := GetExtensionType(theTitle);
		if extType = kExtTypePascal then
		begin
			// Must have a copy in lower case to parse!
//			dataCopy := data;
//			MyLowerCaseText(dataCopy, bufferLength, 0);
			dataCopy := LowerCase(data);

			// Simple Pascal parsing looking for "uses"
			repeat
		// Read tokens throughout the file.
				GetToken(dataCopy, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
				
				if tokenType = kReservedToken then
					if tokenValue = 'implementation' then
					begin
						hasSeenImplementation := true;
						hasSeenImplementationAt := pos;
						if hasSeenImplementationAt + kHowMuchMore < bufferLength then
							bufferLength := hasSeenImplementationAt + kHowMuchMore;
					end
					else
					if tokenValue = 'uses' then
		// Found "uses", read everything until a ";" is found.
					begin
						//WriteLn('USES SEARCH got uses at ', pos);
						repeat
							GetToken(dataCopy, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
							if tokenType in [kOtherToken,	kCarbonToken,kCocoaToken] then
							begin
//								WriteLn('Found uses token = ', tokenValue);
								// Get value in case!
//								MoveBytes(@data[tokenStart], @tokenValue[1], Length(tokenValue));
//								tokenValue := Copy(data, tokenStart, tokenStart + Length(tokenValue)); stupid bug
								tokenValue := Copy(data, tokenStart, Length(tokenValue));
//								WriteLn('Token in case = ', tokenValue);
								// Append findings to list.
								SetLength(list, Length(list)+1);
								list[Length(list)-1] := tokenValue;
//								AppendMenu(usesMenu, tokenValue + extension); // + file extension
							end
							else
								//WriteLn('IGNORED: ', tokenValue, ' since it was a ', tokenType);
						until (pos >= bufferLength) or (tokenType = kSingleCharToken) and (tokenValue = ';');
					end;
			until  pos >= bufferLength;
//			DisposePtr(Ptr(dataCopyPtr)); // Dispose pointer to copy
		end
		else
		if extType in [kExtTypeC, kExtTypeCPP, kExtTypeObjC] then
		begin
			// Similar, but using "GetCToken" (new)
			// Simple parsing of C looking for "#include"
			repeat
			// Read tokens throughout the file.
				GetCToken(data, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
				
				if tokenType = kPreprocessorToken then // Found a "#"
				begin
					AnalyzePreprocessorLine(tokenValue, list, dummyList, -1);
					
(*					GetCToken(dataPtr, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
					if (tokenValue = 'include') or (tokenValue = 'import') then
					begin
						GetCToken(dataPtr, bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
						if tokenType = kStringToken then
						begin
							// Append findings to list.
							SetLength(list, Length(list)+1);
							list[Length(list)-1] := tokenValue;
						end
							else
						;
					end;*)
					readCUpToHere := pos + kHowMuchMore;
					// If no #include for a long time, stop.
				end;
				// BORDE INTE BEHÖVAS HÄR? Jo! Eller?
				if tokenType = kCommentToken then
				begin
					if length(tokenValue) > 3 then { Check for //uses, new special LWP preprocessor }
						CheckSpecialComment(tokenValue + CR, list, dummyList, dummyExtraOptions, dummyExtraLinkOptions);
				end;
				
			until (pos >= bufferLength) or (pos > readCUpToHere);
		end;

		//WriteLn('GetUsesListFromText found ', Length(list), ' entries');
		GetUsesListFromText := list;
	end; {GetUsesListFromText}

//	procedure GetRecursiveUsesList(fileName: AnsiString; var list: StrArr);
//	begin
//		GetUsesListFromFile(fileName);
//	end;	
//	procedure GetRecursiveUsesListFromFile(teWind: WindowPtr; teEdit: TXNObject; var list: StrArr);
//	begin
//	end;

	function BuildMenuFromUses(teWind: WindowPtr): MenuHandle;
	var
		list: AnsiStringArray;
		usesMenu: MenuHandle;
		i: Longint;
		theTitle, extension: Str255;
		cleanName: Str255; // För C-delen
		extType: Integer;
//		chars: Handle;
//		err: OSErr;
//		data: AnsiString;
//		bufferLength: Longint;
		editIndex: Longint;
		homeSpec, foundSpec: FSSpecString;
		fileName: Str255;
		err: OSErr;
		done: Boolean;
		
		front: WindowPtr;
		mainSpec: FSSpecString;
		item: Longint;
	begin
//		err := TXNGetDataEncoded(teEdit, kTXNStartOffset, kTXNEndOffset, chars, kTXNTextData);
//		HLock(chars);
//		dataPtr := CharsPtr(chars^);
//		bufferLength := GetHandleSize(chars);
		
		GetWTitle(teWind, theTitle);
//		list := GetUsesListFromText(dataPtr, bufferLength, theTitle);
//		list := GetUsesListFromFile(teWind, teEdit);
//		DisposeHandle(chars);
		
		editIndex := GetWRefCon(teWind);
		if editIndex > 0 then
			if editIndex <= kMaxEditWindows then
				list := usesList[editIndex];

		// Clean extraOptions and extraLinkOptions before building lists.
		if editIndex > 0 then
		begin
			//WriteLn('CLEARING EXTRA LISTS');
			extraOptions[editIndex] := '';
			extraLinkOptions[editIndex] := '';
		end;

		
		//WriteLn('*** BuildMenuFromUses ***', Length(list));
		//for i := Low(list) to High(list) do
		//	WriteLn(list[i]);
		
		usesMenu := NewMenu(kPathMenuID, 'FcnMenu');
		
//		GetWTitle(teWind, theTitle);
		extension := GetExtension(theTitle); // Används inte längre för test men dock för att lägga till i menyn
		extType := GetExtensionType(theTitle);
		
//WriteLn('List length = ', Length(list));

// Pascal
	if (extType = kExtTypePascal) or (extType = kExtTypeAda) then
		for i := 0 to High(list) do
		begin
			// Old, fast method:
			// AppendMenu(usesMenu, list[i] + extension); // + file extension
			
			// Smarter:
			
			err := GetEditFSSpec(teWind, homeSpec);
			if err <> noErr then
			begin
				//AppendMenu(usesMenu, list[i] + extension); // + file extension
				MyAppendMenu(usesMenu, list[i] + extension, ' '); // + file extension
			end
			else
			begin
				// In order to use FindFileInPaths, we must provide includeanalyzer with a main file!
				// Not good enough? Check GetFrontMainWindow!
				front := GetFrontMainWindow;
				if front <> nil then
					err := GetEditFSSpec(front, mainSpec)
				else
					mainSpec := homeSpec;
				SetFindFileMainSpec(mainSpec);
				
				// Now try all known Pascal extensions
				fileName := list[i] + extension;
				done := FindFileInPaths(fileName, homeSpec, foundSpec);
				// NOTE! The list of Pascal extensions is hard-coded here! Also exists in FileNameUtils!
				if not done then
				begin
					fileName := list[i] + '.p';
					done := FindFileInPaths(fileName, homeSpec, foundSpec);
				end;
				if not done then
				begin
					fileName := list[i] + '.pas';
					done := FindFileInPaths(fileName, homeSpec, foundSpec);
				end;
				if not done then
				begin
					fileName := list[i] + '.dpr';
					done := FindFileInPaths(fileName, homeSpec, foundSpec);
				end;
				if not done then
				begin
					fileName := list[i] + '.pp';
					done := FindFileInPaths(fileName, homeSpec, foundSpec);
				end;
				if not done then
				begin
					fileName := list[i] + '.c';
					done := FindFileInPaths(fileName, homeSpec, foundSpec);
				end;
				if not done then
				begin
					fileName := list[i] + '.cpp';
					done := FindFileInPaths(fileName, homeSpec, foundSpec);
				end;
				if not done then
				begin
					fileName := list[i] + '.c++';
					done := FindFileInPaths(fileName, homeSpec, foundSpec);
				end;
				if not done then
				begin
					fileName := list[i] + '.h';
					done := FindFileInPaths(fileName, homeSpec, foundSpec);
				end;
				if done then
//					AppendMenu(usesMenu, fileName {homeSpec.name})
					MyAppendMenu(usesMenu, fileName {homeSpec.name}, ' ')
					// Should somehow figure out the real capitalization of the file!
				else
				begin
//					AppendMenu(usesMenu, '(' + list[i] + extension);
					item := MyAppendMenu(usesMenu, list[i] + extension, ' ');
					DisableMenuItem(usesMenu, item);
				end;
			end;
		end;

// C

// Try to set a good home for the search
	err := GetEditFSSpec(teWind, homeSpec);
	front := GetFrontMainWindow;
	if front <> nil then
		err := GetEditFSSpec(front, mainSpec)
	else
		mainSpec := homeSpec;
	SetFindFileMainSpec(mainSpec);

// Do the work
	if extType in [kExtTypeC, kExtTypeCPP, kExtTypeObjC, kExtTypeCUDA] then
		for i := 0 to High(list) do
		begin
//			AppendMenu(usesMenu, list[i]); // Finds .h files. .c too?
			MyAppendMenu(usesMenu, list[i], ' '); // Finds .h files. .c too?
			if extension <> '.h' then
			begin
				cleanName := TrimExtension(list[i]);
				if theTitle <> cleanName+extension then // Inte fönstret självt!
				begin
					// For a .h file, does a file exist with any valid extension?
					// NEW 120208

					done := FindFileInPaths(cleanName+extension, homeSpec, foundSpec);
					if done then
						if cleanName+extension <> list[i] then
						MyAppendMenu(usesMenu, cleanName+extension, ' ');
					if not done then
					begin
						done := FindFileInPaths(cleanName+'.c', homeSpec, foundSpec);
						if done then
						if cleanName+'.c' <> list[i] then
							MyAppendMenu(usesMenu, cleanName+'.c', ' ');
					end;
					if not done then
					begin
						done := FindFileInPaths(cleanName+'.cpp', homeSpec, foundSpec);
						if done then
						if cleanName+'.cpp' <> list[i] then
							MyAppendMenu(usesMenu, cleanName+'.cpp', ' ');
					end;
					if not done then
					begin
						done := FindFileInPaths(cleanName+'.cu', homeSpec, foundSpec);
						if done then
						if cleanName+'.cu' <> list[i] then
							MyAppendMenu(usesMenu, cleanName+'.cu', ' ');
					end;
					if not done then
					begin
						done := FindFileInPaths(cleanName+'.m', homeSpec, foundSpec);
						if done then
						if cleanName+'.m' <> list[i] then
							MyAppendMenu(usesMenu, cleanName+'.m', ' ');
					end;
				end;
			end;
		end;
//		SetLength(list, 0); // Dispose, is this needed?
		
		return usesMenu;
	end;

end.

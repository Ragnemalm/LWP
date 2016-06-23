// Richard Ward made this low-level interface to GDB.
// It is now moved to its own unit to clean things up,
// to two units that can now be more focused.

unit DirectGDB;
interface

uses
	strutils, MacOSAll, LightBugs, ProcessUtils, LWPGlobals, TransSkel4, TransDisplay, UtilsTypes, FileUtils, QDCG;

procedure DirectGDBGetStatus;
procedure DirectGDBInit;
procedure BugsGDB;

var
	directgdbWind: WindowPtr;

implementation

//  ********* Modifications start here **************

const
	gVerbose = false;
	
type
	TypeFamily = (tFloat, tChar, tString, tSInt, tUInt, tAddr, tUnknown);
	
	VariableInfoRec = record
		value : ansistring;
		sourceName: ansistring;
		ProcessName : ansistring;
		ProcedureName : ansistring;
		typeName : ansistring;	// will hold GDB type names for Pascal (hardcoded, uppercase)
		typeID : TypeFamily;		//mainly for formatting
		valuFormat : ansistring;// lower level GDB value formatting modifier
		address : ansistring;		//  GDB format
		dereferencedAddress : ansistring; // for what the variable points to if applicable
		isGlobal : boolean;			// global or local
		sourcefile : ansistring;
		color : integer;			// index in your color table
		monitored: boolean;		// if the variable needs to be updated every break
		watchpoint: boolean;	// if the variable is being watched
	end;

VariableInfoArray = array of VariableInfoRec;

function gdb(gdbCmd: ansistring): ansistring;
begin
	return ReadToPrompt(gCurrentProcess, gdbCmd, BugsGetPrompt);
end;

function ExtractGDBAddress(gdbAddress, addrSpecifier: ansistring): ansistring;
var 
	k : longint;
	
begin
	k := pos(addrSpecifier, gdbAddress);	// '0x' is gdb's normal address specifier
	if k > 0 then
	begin
		delete(gdbAddress, 1, k-1);
		return ExtractWord(1, gdbAddress, WordDelimiters); 
	end
	else
		return '???';
end;

		
procedure ExtractGDBIdentifierInfo(gdbIdentifier: ansistring; var IdentifierName, TypeName, ValueFormat: ansistring; var TypeID: TypeFamily);
var 
	k : longint;
	
begin
	k := pos('static', gdbIdentifier); 	// 'static' is in front of main program variables only
	if k > 0 then
		delete(gdbIdentifier, k, 6);			//	delete the word 'static'
	IdentifierName := ExtractWord(1, gdbIdentifier, WordDelimiters); 
	TypeName := ExtractWord(2, gdbIdentifier, WordDelimiters); 
	
	if 			TypeName = 'BOOLEAN'	then begin ValueFormat := '1ub';	TypeID := tUInt; end
	else if		TypeName = 'BYTE' 		then begin ValueFormat := '1ub';	TypeID := tUInt; end
	else if		TypeName = 'SHORTINT'	then begin ValueFormat := '1db';	TypeID := tSInt; end
	else if		TypeName = 'SMALLINT'	then begin ValueFormat := '1dh';	TypeID := tSInt; end
	else if		TypeName = 'INTEGER'	then begin ValueFormat := '1dh';	TypeID := tSInt; end
	else if		TypeName = 'WORD'		then begin ValueFormat := '1uh';	TypeID := tUInt; end
	else if		TypeName = 'LONGINT'	then begin ValueFormat := '1dw';	TypeID := tSInt; end
	else if		TypeName = 'LONGWORD'	then begin ValueFormat := '1uw';	TypeID := tUInt; end
	else if		TypeName = 'CARDINAL'	then begin ValueFormat := '1uw';	TypeID := tUInt; end
	else if		TypeName = 'INT64'		then begin ValueFormat := '1dg';	TypeID := tSInt; end
	else if		TypeName = 'QWORD'		then begin ValueFormat := '1ug';	TypeID := tUInt; end
	else if		TypeName = 'POINTER'	then begin ValueFormat := '1uw';	TypeID := tAddr; end
	else if		TypeName = 'REAL'		then begin ValueFormat := '1fw';	TypeID := tFloat; end
	else if		TypeName = 'SINGLE'		then begin ValueFormat := '1fw';	TypeID := tFloat; end
	else if		TypeName = 'DOUBLE'		then begin ValueFormat := '1fg';	TypeID := tFloat; end
	else if		TypeName = 'EXTENDED'	then begin ValueFormat := '1fg';	TypeID := tFloat; end
	else if		TypeName = 'CHAR'		then begin ValueFormat := 'c ';		TypeID := tChar; end
//	else if		TypeName = 'STRING'		then begin ValueFormat := '10s';		TypeID := tString; end
	else if		TypeName = 'SHORTSTRING'	then begin ValueFormat := 's';		TypeID := tString; end
	else if		TypeName = 'ShortString'	then begin ValueFormat := 's';		TypeID := tString; TypeName := 'SHORTSTRING';end
	else if		TypeName = 'ANSISTRING' 	then begin ValueFormat := 'a';	TypeID := tAddr; end

	else	begin ValueFormat := '???';	TypeID := tUnknown; end;
	
// For some reason, declaring a string[nn] results in a mixed case 'ShortString'!

end;

function ExtractGDBValue(gdbValue, TypeName, valSpecifier: ansistring; TypeID: TypeFamily): ansistring;
var 
	k : longint;
	Val1, Address : ansistring;
//	shortStr : string;
begin
	k := pos(valSpecifier, gdbValue);	
	if k > 0 then
	begin
		delete(gdbValue, 1, k-1);

		if (TypeID = tFloat) then	
		begin
				Val1 := ExtractWord(1, gdbValue, WordDelimiters - ['.', '-', '+']);
				return Val1;
		end;

		if (TypeID = tSInt) then	
				return ExtractWord(1, gdbValue, WordDelimiters - ['-']);

		if (TypeID = tUInt) then
		begin
			Val1 := ExtractWord(1, gdbValue, WordDelimiters);	
			if TypeName = 'BOOLEAN' then
				if		Val1 = '0'	then	return 'false'
											else	return 'true'
			else
				return Val1;
		end;

		if (TypeID = tChar) then
		 		Return ('''' + ExtractWord(2, gdbValue, WordDelimiters) + ''', #' + ExtractWord(1, gdbValue, WordDelimiters)); // 2 is the character value
	end;

	if (TypeID = tString) then
	begin
		if TypeName = 'SHORTSTRING' then
		begin
			k := pos('''', gdbValue);	
			if k > 0 then
			begin
				delete(gdbValue, 1, k-1);
				k := pos('#', gdbValue);
				if k = 0 then
				begin
					k := pos('(gdb)', gdbValue);
					setlength (gdbValue, k-2);
				end
				else					
					setlength (gdbValue, k-1);
				return gdbValue;
			end;
		end;
		return gdbValue+ char(13) + '@@@@@ unknown string @@@@@@@' + char(13);	
	end;

	
	if (TypeID = tAddr) then
	begin
		Address :=  ExtractGDBAddress(gdbValue, '0x');			// '0x' is the end of the pre junk	
		if TypeName = 'ANSISTRING' then	// need to get the address pointing to the string
		begin
			gdbValue := gdb('x/s '  + Address);	// gdb cmd eXamine to get a formatted value @address
			k := pos ('>:', gdbValue);
			delete(gdbValue, 1, k+2);
			k := pos('(gdb)', gdbValue);
			setlength (gdbValue, k-2);
			return gdbValue;
		end
		else
			return Address;		
	end;
			
	return'???';	// unknown
end;


const
	cTabMaxAmt = 20;
	
	
function BuildTab (s : ansiString; width: smallInt) : ansiString;	// for crude formatting
var
		k : smallInt;
begin
	k := length(s);
	if k >= width then
		return '  '
	else
		return stringOfChar(' ', round(2.0*(width - k)));		//  using a factor other than 1 because of non mono width font
end;


function ObtainGDBGlobalIdentifierInfo(gdbIdentifierString: ansiString) : ansiString;
var
	IdentifierName: ansiString;
	TypeName: ansiString;
	ValueFormat: ansiString;
 	Address : ansiString;
	Value : ansiString;
	s : ansistring;
	TypeID : TypeFamily;
	
	s1, s2, s3, s4 : string[cTabMaxAmt];		// for formatting - need mono font width
begin
	ExtractGDBIdentifierInfo(gdbIdentifierString, IdentifierName, TypeName, ValueFormat, TypeID);	

	Address:= gdb('info address ' + IdentifierName);	// gdb cmd to get an address
	Address :=  ExtractGDBAddress(Address, '0x');			// '0x' is the end of the pre junk

	Value := gdb('x/' + ValueFormat + ' ' + Address);	// gdb cmd eXamine to get a formatted value @address
	Value :=  ExtractGDBValue(Value, TypeName, ':', TypeID);		// ':' is the end of the pre junk
	
//		the below is crudeformatting for basic functionality, eliminate for real human interface
//		need mono font to be correct

	s1 := BuildTab(IdentifierName, 9);
	s := IdentifierName + s1 + '= ' + Value;
	if gVerbose then
	begin
		s2 := BuildTab(Address, 9);	
		s3 := BuildTab(ValueFormat, 7);
		s4 := BuildTab(TypeName, 11);
	 	s  := Address + s2 + ValueFormat + s3 + TypeName + s4 + s ;
	 end;

	 return s; 
end;


function gdbGetProgramGlobals: ansistring;
var
	s : ansiString;
	k : longint;
	
begin
	s := gdb('info variables');
	k := pos('Non-debugging', s);	// stuff after this is junk for us
	if k > 0 then
		delete(s, k, length(s) - k);
	return s;
end;

function LDBGetGlobalValues: ansistring;

const
		GlobalsList : StringArr = nil;
var
	s: AnsiString; 
	k, lenList : longint;

begin
{ Need to go up the stack frame in case a local context stores the globals in a register!}

	if GlobalsList = nil then	//  need to set to nil when a new file is debugged.
	begin
		gdb('up 6');		//	need to determine a method for getting the correct nummber of frames to go up
		s := gdbGetProgramGlobals;						// takes awhile to do since gdb returns so much junk
		GlobalsList := SplitStringToList(s);	// saves time to store it in the typed constant
	end;
	
	lenList := Length(GlobalsList) - 2;

	s := '';
	for k := 0 to lenList do
	begin
		if	 ( pos(';', GlobalsList[k]) > 0 ) then  							// gdb vars end with a semicolon
			s := s + ObtainGDBGlobalIdentifierInfo(GlobalsList[k]) + char(13)
		else if	 (pos('File', GlobalsList[k]) = 1) then
			s := s + char(13) + '--- ' + GlobalsList[k] +  ' ---' +char(13)
		else if (pos('All', GlobalsList[k]) = 1) then
			if gVerbose then
				s := s + char(13) + 'Application Globals' +  char(13) + char(13) + ' Address   Format     Type       Name        Value' +  char(13)
			else
				s := s + char(13) + 'Application Globals' +  char(13) + char(13) + ' Name      Value' +  char(13)				
		else 
			s := s + '   -----------------' + GlobalsList[k];
	end;
	return s + char(13);	
end;

function LDBGetLocalValues : ansistring;
begin
	return char(13) + gdb('info locals') + char(13) + gdb('info scope') + char(13);	
end;

function ProcessLDBCommand(cmd: ansistring): ansistring;
var 
	cmdChar : char;	
begin
	cmdChar := cmd[1];
	
	case cmdChar of
//		'h', 'H' : return LDBhelp;
		'g', 'G' : return LDBGetGlobalValues;
		'l', 'L' : return LDBGetLocalValues;
	otherwise
		return GDB(cmd);
	end;
end;

var
	gLastLDBCmd : ansistring; // used to update the status display	

function LDB(cmd : ansistring): ansistring;
begin
	gLastLDBCmd := cmd;
	if (length(cmd) > 0) then
		if (cmd[1] = '-') then
		begin
			Delete(cmd, 1, 1);
			return ProcessLDBCommand(cmd);
		end
		else
			return GDB(cmd)
	else return '';
end;

procedure BugsGDB;
var 
	cmd: ansistring;
begin
	SetDView(directgdbWind, 'Stac', 0);
	FlushDisplay;
	cmd:= VMGetStringValue(directgdbWind,'Etxt', 1); //
	DisplayString('command entered : '+ cmd + char(13) + char(13));
	if gCurrentProcess <> nil then
		DisplayString( LDB(cmd) )
	else
		DisplayString('app is not running');
end;

procedure DirectGDBGetStatus;
begin	
	SetDView(directgdbWind, 'Stac', 0);
	FlushDisplay;	
	DisplayString (LDB(gLastLDBCmd));
end;

procedure DirectGDBInit;
var
	err: OSErr;
	parent: HIViewRef;
	cmdView, resultView: ControlRef;
	rr: Rect;
begin
	// Replace the nib with code?
	err := CreateWindowFromNib(SkelGetMainNib, CFSTR('Debugger Direct'), directgdbWind);
	if err <> noErr then
		WriteLn('DirectGDBInit failed with ', err);
	dummy := SkelWindow(directgdbWind, nil{@Mouse}, nil{@HelpKey}, nil{@Update}, nil{@Activate}, nil{@DoClose}, nil{@Halt}, nil, true);

(*
	Draft for nibless version:
	SetRect(rr, 317, 107, 593+317, 598+107);
	directgdbWind := SkelNewWindow(rr, 'Debugger Direct', nil{Update, theDrawProc: VMQDCGDrawProcPtr},
					nil {theMouseProc: VMQDCGMouseProcPtr}, nil{theKeyProc: TSKeyProcPtr}, nil {userData: Pointer},
					nil {pActivate: TSBooleanProcPtr}, nil{pClose}, nil{pClobber},
					nil {pIdle: TSNoArgProcPtr;}, true,
					0, nil, nil, 0, nil{DirectGDBResize resizeProc: TSResizeProcPtr});

	parent := SkelGetContentView(directgdbWind);

	cmdView := SkelNewTextField(directgdbWind, 58, 43, 559, 16, '');
	resultView := SkelNewStaticTextField(directgdbWind, 20, 68,553,517, '');
	// Should be selectable/copyable!

	VMSetControlId(cmdView, 'Etxt', 1);
	VMSetControlId(resultView, 'Stac', 0);
*)

	NewDView(directgdbWind, 'Stac', 0); // Install HIView to be used as displayview
end;

end.

// *************** modifications end here *******************

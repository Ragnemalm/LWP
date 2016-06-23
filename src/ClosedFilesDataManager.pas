// Most of this written by Sreehari
// Isolated to a unit by Ingemar

unit ClosedFilesDataManager;
//The name of this file should be renamed as 'ClosedFileDataManager.pas' this unit does the not only 'jump to functions' and also 'code completion' and 'class browser'for closed files in the project files. 
interface
uses
	MacOSAll, LWPGlobals, UtilsTypes;

procedure UpdateJTFN;
procedure CloseJTFNForWindow(editIndex: Longint);
procedure PerformJumpToFunction(editIndex: Longint; itemString, theRootSpec: AnsiString);
//procedure BuildJTFNUsesList(target: Longint; includePathInFileName: Boolean);
function BuildJTFNUsesList(teWind: WindowPtr; includePathInFileName: Boolean): FileArr;
Procedure PerformBackJTFN(editIndex: Longint);
function GetTokenInCaseString(teRec:AnsiString; tokenStart, tokenEnd: Longint):AnsiString;

var 
	gusesList:array[1..kMaxEditWindows] of FileArr;
	gclosedFilesGlobalFunctions: array[1..kMaxEditWindows] of array of AnsiString;
	

implementation

uses
	ColorCoding, LWPEdit, FileUtils, IncludeAnalyzer, HashTableUnit, Halda, Console, ClassParser;

const
//	kCCStepLength = 4096; // 65536; // 256; // Something like 16k
	kCCStepLength = 16384; // 256; // Something like 16k	

// Was in globals:
var
	editFunctionFile: array[1..kMaxEditWindows] of AnsiString;
	
	//gProjectFiles: array[1..kMaxEditWindows] of array of AnsiString;
	gisOpenFile:  array[1..kMaxEditWindows] of array of Boolean;
	
	//For the list of all functions of closed files in the project
	gjumpToFunctionNameStart: array[1..kMaxEditWindows] of array of Longint;
	gjumpToFunctionNameEnd: array[1..kMaxEditWindows] of array of Longint;
	gjumpToFunctionName: array[1..kMaxEditWindows] of array of AnsiString;
	gjumpToFunctionFile: array[1..kMaxEditWindows] of array of AnsiString;
	
	//For backtrace
	// - should be called "jump to function history" IMHO /Ingemar
	backTraceStart: array[1..kMaxEditWindows] of array of Longint;
	backTraceEnd: array[1..kMaxEditWindows] of array of Longint;
	backTraceFile: array[1..kMaxEditWindows] of array of AnsiString;

	// Jump to data record declaration
	type
		JtfnVariables = record
		
		bufferFiles: array of AnsiString;
		bufferFilename: FileArr; // Closed files
		openFilename: FileArr; // Open files - not searched by Jtfn since color coding does
		//i: Longint;
		//filesListInitialize: boolean;
		currentBufFile, totalNoOfBufferFiles:Longint;
		goToNextFile, firstFile, initialized: Boolean;
	end;
	
//var
	{closedFileName: Array of AnsiString;
	closedFileRecordVariables: array of Array of RecordVariables; //2D array
 	closedFileVariableMembers: array of VariableMembers;//1 array of [1..kMax]
 	closedFileRecordPointers: array of array of RecordPointers; }
 	
var
	gJtfnVariables: array [1..kMaxEditWindows] of jtfnVariables;


	//For the list of functions of closed files
var
	jumpToFunctionNameStart: array of array of Longint;
	jumpToFunctionNameEnd: array of array of Longint;
	jumpToFunctionName: array of array of AnsiString;
	jumpToFunctionFile:array of array of AnsiString;
	closedFilesGlobalFunctions:array of array of AnsiString;

procedure InitJtfnVariables(var j: JtfnVariables);
begin
	with j do
	begin
		//w:= nil;
		bufferFilename:= nil;
		//i: Longint;
		//filesListInitialize:= true;
		currentBufFile:= 0;
		totalNoOfBufferFiles:= 0;
		goToNextFile:= true;
		firstFile:= true;
		initialized := false;
	end;
end;


// SYNCH?!
procedure AddToJumpToFunctionArrays(var JtnfSnapshot: SnapShot; tokenValue: AnsiString; tokenStart, tokenEnd: Longint; bufferFilename: AnsiString);
//var
	//iItemCount: Longint;
begin
	with JtnfSnapshot do
	begin
		//iItemCount := Length(editFunctionName);
		//if iItemCount < kMaxFunctionMenuItems then
		//begin
			// Add to popup menu!
			// Get the function name in proper case!
			SetLength(editFunctionNameStart, Length(editFunctionName)+1);
			editFunctionNameStart[High(editFunctionNameStart)] := tokenStart;
			SetLength(editFunctionNameEnd, Length(editFunctionName)+1);
			editFunctionNameEnd[High(editFunctionNameEnd)] := tokenEnd;
			SetLength(editFunctionName, Length(editFunctionName)+1);
			editFunctionName[High(editFunctionName)] := tokenValue;
			SetLength(FunctionFileName, Length(FunctionFileName)+1);
			FunctionFileName[High(FunctionFileName)] := bufferFilename;
		//end;
	end;
end;
function GetTokenInCaseString(teRec:AnsiString; tokenStart, tokenEnd: Longint):AnsiString;
begin
	GetTokenInCaseString := Copy(teRec, tokenStart, tokenEnd-tokenStart+1);
end;

Procedure GetCFunctionsList(var s: SnapShot; var j: JtfnVariables; editIndex:LongInt);
var
//	data: AnsiString;
	tokenStart, tokenEnd, tokenType: Longint;
	{prevTokenStart, prevTokenEnd,} balance: Longint;
	tokenValue: AnsiString;
	
//	iItemCount: Integer;
	untilHere: Longint;
	
	pos1, pos2: LongInt;
	bracketCount: LongInt;
	bracketFound: Boolean;
	tempClassMember, tempType: AnsiString;
	gotClassMember: Boolean;
//	functionParsing: Boolean;
	dataInParen: AnsiString;
	startBracketPos, endBracketPos, i: LongInt;
	dataInParenParsing, gotRecordName: Boolean;
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
					if (prevTokenType=kKnownTypeToken) then
					begin
						SetLength(variableName, Length(variableName)+1);
						variableName[High(variableName)]:= tokenValue;
						SetLength(variableType, Length(variableType) + 1);
						variableType[High(variableType)]:= prevTokenValue;
						SetLength(variableScope, Length(variableScope)+1);
						if High(editFunctionName) >= 0 then // Added by Ingemar 151105
							variableScope[High(variableScope)] :=  editFunctionName[High(editFunctionName)]
						else
							variableScope[High(variableScope)] := '???';
						//writeln('**$$variableName: ', variableName[High(variableName)], ' variableType ', variableType[High(variableType)], ' variableScope: ', variableScope[High(variableScope)]);

					end;
				end;
			end;
		until (tokenValue=';') or (posData>=Length(data));
	end;
	
	
	procedure GetCVariableList(data:AnsiString; pos1:LongInt; var s:snapshot; untilHere:LongInt;  bufferLength: LongInt; tempType:AnsiString);
	begin
		//writeln('Entered GetCVariablelist');
		with s do
		begin
			with lVariableMembers do
			begin
				//writeln('data: ', data);
				repeat		
					GetCToken(data, bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
					//writeln('tokenValue: ', tokenValue, ' pos1: ', pos1);
					if (tokenValue=',') or (tokenValue='=') or (tokenValue=';') or (tokenType=kStartParenToken) then
					begin
						SetLength(variableName, Length(variableName)+1);
						variableName[High(variableName)]:= prevTokenValue;
						
						SetLength(variableScope, Length(variableScope)+1);
						if (balance=0) then
							variableScope[High(variableScope)] := 'global'
						else
							if High(editFunctionName) >= 0 then // Added by Ingemar 151105
								variableScope[High(variableScope)] :=  editFunctionName[High(editFunctionName)]
							else
								variableScope[High(variableScope)] := '???';
					
						SetLength(variableType, Length(variableType) + 1);
						variableType[High(variableType)]:= tempType;
						//writeln('**$$variableName: ', variableName[High(variableName)], ' variableType ', variableType[High(variableType)], ' variableScope: ', variableScope[High(variableScope)]);
						
					end;
					
				until (tokenType=kStartParenToken) or (tokenValue=';') or (tokenValue='=') or (pos1 >= bufferLength) or (pos1 >= untilHere);
				//writeln('**Length: ', Length(s.lVariableMembers.variableName));
			end;
		end;
	end;
	
begin
	//writeln('Entered GetCFunctionsList');
	untilHere := s.pos + kCCStepLength;
	with j do
	begin
		//writeln('**bufferFileName: **', bufferFilename[currentBufFile]);
		////Writeln('bufferFile No: and UnitlHere: ', currentBufFile, ', ', untilHere);
		if currentBufFile >= 0 then
		if currentBufFile <= High(bufferFiles) then // 160412, don't access unknown arrays!
		with s do
		begin
			bufferLength := Length(bufferFiles[currentBufFile]);
			//data := bufferFiles[currentBufFile];
			//bufferLength := Length(data);
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
			//HashInitTable(classNamesHashTable[editIndex]);
			dataInParenParsing:=false;
			gotRecordName:=false;
			
			externParsing:= false; // Too time consuming (needs to be fixed)
			
			{prevTokenValue:='';
			prevTokenStart:=0;
			prevTokenEnd:=0;
			secondPrevTokenValue:='';
			secondPrevTokenStart:=0;
			secondPrevTokenEnd:=0;}

			repeat
				GetCToken(bufferFiles[currentBufFile], bufferLength, editIndex, pos, tokenStart, tokenEnd, tokenType, tokenValue);
				//writeln('*tokenValue: ', tokenValue, ' pos: ',pos);
				// C is awkward to parse.
		
				{if ((balance=0) or (functionParsing)) then	
				if classType >0 then
					tokenType:=ClassType;}
				//writeln('classType: ', classType, ' tokenValue: ', tokenValue);
								
				if (classType=kObjectToken) then
				if not ((prevTokenValue='class') or (prevTokenValue='struct') or (prevTokenValue='private') or (prevTokenValue='public') or (prevTokenValue='protected') ) then
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
							tempType:={tokenValue;}GetTokenInCaseString(bufferFiles[currentBufFile], tokenStart, tokenEnd);
							//writeln('###*tokenType: ', tempType);
							pos1:=pos;
							//writeln('tokenValue: ', tokenValue, ' pos1: ', pos, ' untilHere: ', untilHere, ' bufferLength: ', bufferLength, ' tempType: ', tempType);
							GetCVariableList(bufferFiles[currentBufFile], pos1, s, untilHere, bufferLength, tempType);
							//writeln('###Came out from GetCToken');
							//writeln('##**Length: ', Length(s.lVariableMembers.variableName));
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
							//SetInterval(teRec, prevTokenStart, prevTokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
							AddToJumpToFunctionArrays(s, previousTokenValue, prevTokenStart, prevTokenEnd, bufferFilename[currentBufFile]);

							//AddToEditFunctionArrays(s, previousTokenValue, prevTokenStart, prevTokenEnd);

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
								If (Length(variableName)>0) then
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
							//WriteLn('Detected in closed files, function prototype, should be removed');
							//writeln('Length of editFunctionNameStart: ', Length(editFunctionNameStart));
						
	//						iItemCount := CountMenuItems(editFunctionMenu[editIndex]);
	//						DeleteMenuItem(editFunctionMenu[editIndex], iItemCount);
						 
						if (Length(editFunctionNameStart)>0) then
						begin	
							//writeln('Closed file CFunctionName: ', editFunctionName[High(editFunctionName)]);
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
						//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
						hasSeenObjCMethod := true;
						startOfObjCMethod := tokenEnd;
	//					objCMethodName := objCClassName + ' '; // Prefix all methods with class name
						objCMethodName := '  '; // Just indent all methoids (class name will be at start of implementation)
					end;
				end;
				kPreprocessorToken:
				begin
					//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
					//writeln('kP tokenValue: ', tokenValue);
					//AnalyzePreprocessorLine(tokenValue, usesList, frameworkList, editIndex);
				end;
				// kFunctionToken:
				// Do we EVER get here?
					//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
				kBeginEndToken:
				begin
					//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
					//if tokenValue = '{' then balance := balance + 1;
					//if tokenValue = '}' then balance := balance - 1;
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
						dataInParen:= GetTokenInCaseString(bufferFiles[currentBufFile], startBracketPos, endBracketPos)+';';
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
								//SetInterval(teRec, prevTokenStart, prevTokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
	//							WriteLn('ObjC function ', objCMethodName);										AddToJumpToFunctionArrays(s, tokenValue, tokenStart, tokenEnd, currentbufferFilename);
								AddToJumpToFunctionArrays(s, objCMethodName, startOfObjCMethod, prevTokenEnd, bufferFilename[currentBufFile]);

								//AddToEditFunctionArrays(s, objCMethodName, startOfObjCMethod, prevTokenEnd);
							end;
						end;
				end;
				kKnownTypeToken {or kObjectToken}:// or (classType=kObjectToken): //To Do: or classNameToken; The hash table should ne unique to each project. This might be tricky or pretty simple.
				begin
					//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VKnowTypeColor, LWPSintax.VKnowTypeStyle);
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
							GetCVariableList(bufferFiles[currentBufFile], pos1, s, untilHere, bufferLength, tempType);

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
					//TODO: Here C Code completon should be developed.
					//writeln('Entered Reserved Token');
					if SSfileName = '' then
					begin
						SSfileName := bufferFilename[currentBufFile];
						with gClosedFilesVariablesData[editIndex][High(gClosedFilesVariablesData[editIndex])] do
						begin
							fileName:= bufferFilename[currentBufFile];
						end;
						//writeln('SSfileName: ', SSfileName, ' bufferFilename[currentBufFile]: ', bufferFilename[currentBufFile]);
					end;
					
					if (tokenValue = 'struct') or (tokenValue = 'class') then
					begin
					//Get record name and Search for it If it exist in editRecordVariables[editIndex][0..High]. If It doesn't has, then increase length of editRecordVariables.
						SetLength(lRecordVariables, Length(lRecordVariables)+1);
						
						with lRecordVariables[High(lRecordVariables)] do 
						begin
							pos1:=pos;
							GetCToken(bufferFiles[currentBufFile], bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
							//writeln('tokenValue: ', tokenValue, ' pos1: ', pos1);
							//Get one more tokenValue
							GetCToken(bufferFiles[currentBufFile], bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
							//writeln('tokenValue: ', tokenValue, ' pos1: ', pos1);
							
							//Support for inheritance
							If not (tokenValue='{'){(tokenValue=':')} then
							begin
								recordName := prevTokenValue;
								//Add ClassName to corresponding Hash table.
									with gClosedFilesVariablesData[editIndex][High(gClosedFilesVariablesData[editIndex])] do
									begin
										HashAddEntry(fileClassNamesHashTable, recordName, kObjectToken);
										//writeln('*Length: ', Length(gClosedFilesVariablesData[editIndex]));
										//writeln('*file: ', fileName);
										//writeln('*className: ', prevTokenValue);
										//WriteLn('*myClassName returns ', HashLookUpInteger(fileClassNamesHashTable, recordName));
									end;								gotRecordName:=true;
								//writeln('**$$className: ', prevTokenValue, ' recordName: ', recordName, ' editIndex: ', editIndex);
								//WriteLn('**$$myClassName returns: ', HashLookUpInteger(classNamesHashTable[editIndex], recordName));
								repeat 
									GetCToken(bufferFiles[currentBufFile], bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
									//writeln('tokenValue in inheritance section: ', tokenValue);
									if (tokenValue=',') or (tokenValue='{') then
									begin
										inheritedObjectName := prevTokenValue;
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
									end;
									
								until (tokenValue='{') or (pos1>untilHere) or (pos1>bufferLength);
								
							end;
							
							//Collect className and class members
							If tokenValue='{' then
							begin
								//className:=pTV;
								if not gotRecordName then
								begin
									recordName:=prevTokenValue;
									//Add ClassName to corresponding Hash table.
									with gClosedFilesVariablesData[editIndex][High(gClosedFilesVariablesData[editIndex])] do
									begin
										HashAddEntry(fileClassNamesHashTable, recordName, kObjectToken);
										//writeln('*Length: ', Length(gClosedFilesVariablesData[editIndex]));
										//writeln('*file: ', fileName);
										//writeln('*className: ', prevTokenValue);
										//WriteLn('*myClassName returns ', HashLookUpInteger(fileClassNamesHashTable, recordName));
									end;
								end;
								if (balance=0) then
									recordScope:= 'global'
								else
									if High(editFunctionName) >= 0 then // Added by Ingemar 151105
										recordScope:= editFunctionName[High(editFunctionName)]
									else
										recordScope:= '???';
								repeat 
									GetCToken(bufferFiles[currentBufFile], bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
									//writeln('$$$$tokenValue: ', tokenValue);
									
									if (tokenValue='(') or (tokenValue='[')  or (tokenValue='{') then
									begin
										if (tokenValue='(') or (tokenValue='[') then
										begin
											bracketFound := true;
											tempClassMember := GetTokenInCaseString(bufferFiles[currentBufFile], prevTokenStart, prevTokenEnd);//prevTokenValue;
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
										FinishCBrackets(bufferFiles[currentBufFile], pos2, bracketCount, untilHere, bufferLength);
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
											//writeln('**$$classMember bcoz of brackets: ', tempClassMember, ' ', recordMembers[High(recordMembers)]);
											bracketFound := false;
											if (tokenValue='('{for constructor or functions}) then
												gotClassMember:=true;
										end
										else
											if not gotClassMember then
											begin
												recordMembers[High(recordMembers)]:= GetTokenInCaseString(bufferFiles[currentBufFile], prevTokenStart, prevTokenEnd);//prevTokenValue;
												//writeln('**$$classMember: ', prevTokenValue, ' ', recordMembers[High(recordMembers)]);
											end
											else
												gotClassMember:= false;
									end;
								until( tokenValue='}') or (pos1>untilHere) or (pos1>bufferLength);
								
								//Should parse little more till ';' for object If exists
								pos2:=pos1;
								repeat
									GetCToken(bufferFiles[currentBufFile], bufferLength, pos2, tokenStart, tokenEnd, tokenType, tokenValue);
									//writeln('&***tokenValue: ', tokenValue);
										with lVariableMembers do
										begin
										if ((tokenValue=';') and not (prevTokenValue='}')) then
										begin
											SetLength(variableName, Length(variableName)+1);
											variableName[High(variableName)]:= prevTokenValue;
											
											SetLength(variableScope, Length(variableScope)+1);
											if (balance=0) then
												variableScope[High(variableScope)] := 'global'
											else
												if High(editFunctionName) >= 0 then // Added by Ingemar 151105
													variableScope[High(variableScope)] :=  editFunctionName[High(editFunctionName)]
												else
													variableScope[High(variableScope)] := '???';
										
											SetLength(variableType, Length(variableType) + 1);
											variableType[High(variableType)]:= recordName;
											//writeln('&&&Object Name from class: ', variableName[High(variableName)], 'ObjectType: ', variableType[High(variableType)]);

										end;
									end;
								until( tokenValue=';') or (pos2>untilHere) or (pos2>bufferLength);
								tokenValue:= 'class';

							end;//If tokenValue='{' then
						end;//with lRecordVariables
					end;//if struct

				end;
				kOtherToken:
	//			if previousTokenType = kPreprocessorToken then
				begin
	//				SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VOtherColor, LWPSintax.VOtherStyle);

					if previousTokenValue = '@' then
					begin
						if tokenValue = 'implementation' then
						begin
							hasSeenImplementation := true;
							//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
						end;
						if tokenValue = 'interface' then
						begin
							hasSeenImplementation := false;
							hasSeenInterface := true;
							//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
						end;
						if tokenValue = 'end' then
						begin
							hasSeenImplementation := false;
							hasSeenInterface := false;
							//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VReservedColor, LWPSintax.VReservedStyle);
						end;
					end;
					
					// ObjC class name
					if previousTokenValue = 'implementation' then
					begin
						tokenValue:= GetTokenInCaseString(bufferFiles[currentBufFile], tokenStart, tokenEnd);
						objCClassName := tokenValue;
						//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
						AddToJumpToFunctionArrays(s, tokenValue + ' implementation', prevTokenStart, tokenEnd, bufferFilename[currentBufFile]);
						//AddToEditFunctionArrays(s, tokenValue + ' implementation', prevTokenStart, tokenEnd);
					end;
					if previousTokenValue = 'interface' then
					begin
						//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
						// Put interface start in menu?
						AddToJumpToFunctionArrays(s, tokenValue + ' interface', prevTokenStart, tokenEnd, bufferFilename[currentBufFile]);

						//AddToEditFunctionArrays(s, tokenValue + ' interface', prevTokenStart, tokenEnd);
					end;
					
					if hasSeenObjCMethod then
						if parenBalance = 0 then
						begin
							//SetInterval(teRec, tokenStart, tokenEnd, LWPSintax.VBeginEndColor, LWPSintax.VBeginEndStyle);
							// Del av ObjC-metod
							objCMethodName := objCMethodName + tokenValue;
						end;
				end;
				
				kLinkageToken:
				begin
					if tokenValue = 'extern' then
					begin
						externParsing := true;
						//writeln('extern detected');
					end;
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
			{if (pos>=untilHere) or (pos >= bufferLength) then
				writeln('$$$Length: ', Length(s.lVariableMembers.variableName));}

			
			if pos >= bufferLength then
			begin
				goToNextFile:= true;
			//Writeln('goToNextFile in Files list: ', goToNextFile);
			end;
			
		end;
	end;
end;

//This gets functions list, global variables list, global records list of closed files.
procedure GetFunctionsList(var s: SnapShot; var j: JtfnVariables; editIndex:LongInt);
var
//	data: AnsiString;
	//err: OSErr;
	tokenStart, tokenEnd, tokenType: Longint;
	tokenValue,currentBufferFilename: AnsiString;
	
//	betterTokenHandle: Handle; {Token value in normal case}
//	iItemCount: Integer;
	i, untilHere: Longint;
	
	pos1, pos2:LongInt;
	count:LongInt =0;
//	recName, recMember:AnsiString;
//	recIndex,i :LongInt;
	{foundrecName, foundRecMember,} foundPointer, {foundType,} bracketFound:Boolean;
	bracketCount:LongInt;
//	chars, toBeParsed:AnsiString;
	gotVariableType :boolean = false;
	startPos, endPos:LongInt;
	functionParsing:boolean = false;
	levelCount:LongInt =0;
	typeFunction:boolean=false;
	exitVar:boolean=false;
	methodName, methodParameters:AnsiString;
	
	//This recognises which one is variable name, or variableType. 
	procedure GetVariableList(chars:AnsiString; tokenValue:AnsiString; pos1, untilHere, bufferLength: LongInt );
	begin
		with s do
		begin
			with lVariableMembers do
			begin
			
				//Skip brackets, for exaple in arrays declaration.
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
								if High(editFunctionName) >= 0 then
									variableScope[High(variableScope)]:= editFunctionName[High(editFunctionName)]
								else
									variableScope[High(variableScope)]:= '???';
							
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
						
						
					SearchVariableTypeInPointersList(s, editIndex);;
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
									
								SearchVariableTypeInPointersList(s, editIndex);;
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
							SearchVariableTypeInPointersList(s, editIndex);;
							
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
	//Writeln('Entered JumpToFunctionList');
	//SetLength(list, 0);
	
	untilHere := s.pos + kCCStepLength;

	with j do
	begin
		//writeln('**bufferFileName: **', bufferFilename[currentBufFile]);
		//Writeln('bufferFile No: and UnitlHere: ', currentBufFile, ', ', untilHere);
		if High(bufferFiles) >= currentBufFile then // Yet another array error!
		with s do
		begin
				bufferLength := Length(bufferFiles[currentBufFile]);
				//Writeln('**bufferFileName:** ', bufferFilename[currentBufFile]);
				repeat
				GetToken(bufferFiles[currentBufFile], bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
				
				// History
				if tokenValue = 'program' then
				begin
					hasSeenProgram := true;		// Mark window as gogo-able and runnable
					editWindIsMain := true;
					editMainWindIsProgram := true;

					editWindIsCocoaProgram := false;
					editWindIsiOSProgram := false;
				end;
				
				if tokenValue = 'library' then
				begin
					hasSeenProgram := true;		// Mark window as buildable
					editWindIsMain := true;
					editMainWindIsProgram := false; // Not runnable!
				end;
				
				if tokenValue = 'implementation' then	// Signals that "procedure" and "function" may be candidates for function menu
					hasSeenImplementation := true;
				//if (tokenValue='var') or (tokenValue='record') then
				//	writeln('*tokenValue*', tokenValue);
				
				if tokenType = kReservedToken then
				begin
					if SSfileName = '' then
						SSfileName:= bufferFilename[currentBufFile];
					//writeln('Entered kReservedToken');
					if not hasSeenImplementation then
					begin
						//Get the variable names of record, by checking type followed by record.
						if (tokenValue='record') or (tokenValue = 'object') or (tokenValue = 'objcclass') or (tokenValue = 'class')then
						begin
							//Get record name and Search for it If it exist in editRecordVariables[editIndex][0..High]. If It doesn't has, then increase length of editRecordVariables.
							SetLength(lRecordVariables, Length(lRecordVariables)+1);
							
							with lRecordVariables[High(lRecordVariables)] do 
							begin
								//pick record Name
								recordName := GetTokenInCaseString(bufferFiles[currentBufFile], secondPrevTokenStart, secondPrevTokenEnd); //secondPrevTokenValue;
								//writeln('recordName: ', recordName);

								//Add record scope here
								if not hasSeenImplementation then
									recordScope:= 'global'
								else
									if not functionParsing then
										recordScope:= 'local'
									else
										if High(editFunctionName) >= 0 then // Added by Ingemar 151105
											recordScope:= editFunctionName[High(editFunctionName)]
										else
											recordScope := '???';
										
								//writeln('recordScope: ', recordScope);
								
								if (tokenValue = 'object') or (tokenValue = 'class') or (tokenValue = 'objcclass') then // objcclass, class?
								begin
									
									SetLength(closedFilesClassList[editIndex], Length(closedFilesClassList[editIndex])+1);
									closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].className := recordName;
									SetLength(closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].inheritedClasses, 0); // No inherited classes found yet
									closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].definedInFileName := GetLastToken(bufferFilename[currentBufFile]);
									closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].definedInFilePath := bufferFilename[currentBufFile];
									closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].definedInFilePosition := secondPrevTokenStart; // tokenStart;
									closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].classScope := recordScope;
				//					WriteLn('Done storing to list');
								end;
								
								//Parse until end of record to collect record members. Note that we ignore type of recordmembers.
								pos1:= pos;
								repeat
									repeat GetToken(bufferFiles[currentBufFile], bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
									until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
								//pick record variables
									//writeln('tokenValue: ', tokenValue);
									//writeln('**previousTokenValue**', prevTokenValue);
									
									//To support inheritance:
									if (tokenValue='(') and (prevTokenValue='object') then
									begin
										repeat GetToken(bufferFiles[currentBufFile], bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
										until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
										
										repeat GetToken(bufferFiles[currentBufFile], bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
										until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
										
										inheritedObjectName:=tokenValue;
										//writeln('inheritedObjectName: ', inheritedObjectName);
										
										SetLength(closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].inheritedClasses, Length(closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].inheritedClasses)+1); // No inherited classes found yet
										closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].inheritedClasses[High(closedFilesClassList[editIndex][High(closedFilesClassList[editIndex])].inheritedClasses)] := GetTokenInCaseString(bufferFiles[currentBufFile], tokenStart, tokenEnd);//tokenValue;
								
										inheritedList:= SearchForInheritedObjInRVList(s, editIndex, inheritedObjectName);//Search for inherited objectName in record variable list, and get it's members list If found.
										{for i:= low(inheritedList) to High(inheritedList) do
										begin
											writeln('closed inheritedList: ',i, ' ', inheritedList[i]);
										end;}
										
										//Add the members list to the current object or record.
										recordMembers:=inheritedList;
									end;
									
									
									if (tokenValue=':') or (tokenValue=',') or (tokenValue='procedure') or (tokenValue='function') then
									begin
										SetLength(recordMembers, Length(recordMembers)+1);
										if (prevTokenValue)='' then
											recordMembers[High(recordMembers)] {recMember}:= GetTokenInCaseString(bufferFiles[currentBufFile], secondPrevTokenStart, secondPrevTokenEnd)//secondPrevTokenValue
										else
											recordMembers[High(recordMembers)] {recMember}:= GetTokenInCaseString(bufferFiles[currentBufFile], prevtokenStart, prevtokenEnd);//prevTokenValue;
										
										//writeln(' recordMember: ', recordMembers[High(recordMembers)]);
										if (tokenValue='procedure') or (tokenValue='function') then
										begin
											//Get function name
											methodName:='';
											methodParameters:='';
											repeat GetToken(bufferFiles[currentBufFile], bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
											until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
											recordMembers[High(recordMembers)] := GetTokenInCaseString(bufferFiles[currentBufFile], tokenStart, tokenEnd);//tokenValue;
											methodName:=recordMembers[High(recordMembers)];
											//writeln('recordMembers[High(recordMembers)]: ', recordMembers[High(recordMembers)] );
											//writeln('methodName: ', methodName);
											//Get one more takenValue for '('
											SetLength(closedFilesMethods[editIndex], Length(closedFilesMethods[editIndex])+1);
											closedFilesMethods[editIndex][High(closedFilesMethods[editIndex])].className := recordName;
											closedFilesMethods[editIndex][High(closedFilesMethods[editIndex])].name := methodName;
											closedFilesMethods[editIndex][High(closedFilesMethods[editIndex])].position := tokenStart;
											closedFilesMethods[editIndex][High(closedFilesMethods[editIndex])].fileName := bufferFilename[currentBufFile];
											
											repeat GetToken(bufferFiles[currentBufFile], bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
											until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
											
											if (tokenValue= '(') then
											begin
												pos2:=pos1;
												startPos:= pos1-1;
												bracketFound:= False;
												bracketCount:=0;
												bracketCount+=1;
												//writeln( 'b4- pos2: ', pos2);
												FinishBracketsinLoops(bufferFiles[currentBufFile], pos2, bracketCount, untilHere, bufferLength);
												//writeln( 'after- pos2: ', pos2);
												endPos:=pos2;
												pos1:=pos2;
												
												methodParameters:=GetTokenInCaseString(bufferFiles[currentBufFile], startPos, endPos);
												SetLength(recordMembers, Length(recordMembers)+1);
												recordMembers[High(recordMembers)] := methodName + methodParameters;
												//writeln('methodParameters: ', methodParameters);
												
											end;

										end;

										
									end;
			
								
								until (tokenValue='end') or (pos1 >= bufferLength) or (pos1 >= untilHere);
							
							end; //with
						end;
						
						//Collect variables list 
						if (tokenValue= 'var') then
						begin
							with lVariableMembers do
							begin
								pos1:=pos;
								count:=0;
								exitVar:=false;
								repeat
									repeat GetToken(bufferFiles[currentBufFile], bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
									//writeln('tokenValue: ', tokenValue, ' previousTokenValue: ', previousTokenValue, 'secondPreviousTokenValue: ', secondPrevTokenValue);
									until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos1 >= bufferLength) or (pos1 >= untilHere); //Skip comments, kCRLF
									
									//This is for special case var X:record. Precisly record declared inside var.
									if tokenValue = 'record' then
									begin
										//SetLength(variableName, Length(variableName)-1);
										//save this record name in to variable name. VarableType also should be the recordName in order to use this for code completion. And the exit Var loop by exitVar.
										SetLength(variableType, Length(variableType)+1);
										variableType[High(variableType)]:= variableName[High(variableName)];
										exitVar:= true;
									end;
									
									If not exitVar then
									begin
										pos2:=pos1;
										GetVariableList(bufferFiles[currentBufFile], tokenValue, pos2, untilHere, bufferLength);
										pos1:=pos2;
									end;			
											
											
								until (exitVar) or (tokenvalue= 'type') or (tokenvalue= 'var') or (tokenvalue= 'begin') or (tokenvalue= 'const') or (tokenvalue= 'procedure') or
								(tokenvalue= 'function') or (tokenvalue= 'implementation') or (tokenValue = 'constructor') or (pos1 >= bufferLength) or (pos1 >= untilHere);
								
								tokenType:= kReservedToken;// previous token type--> var
								
							end;//with
						end;//if (tokenValue= 'var') then
					end;//if not hasSeenImplementation then
				end //if tokenType = kReservedToken then
				else
				if (tokenType = kOtherToken) or (tokenType =kSingleCharToken) then
				begin
					if prevTokenType = kFunctionToken then
					begin
						//writeln('tokenValue: ', tokenValue);	
						if not hasSeenImplementation then
							if secondPreviousTokenValue <> ':' then // function type
								if secondPreviousTokenValue <> '=' then // function type
									if not (tokenType = kSingleCharToken) then
									begin
										//writeln('Global Function in closed Files: ',   GetTokenInCaseString(bufferFiles[currentBufFile], tokenStart, tokenEnd));
										SetLength(editGlobalFunctions, Length(editGlobalFunctions)+1);
										editGlobalFunctions[High(editGlobalFunctions)] := GetTokenInCaseString(bufferFiles[currentBufFile], tokenStart, tokenEnd);
									end;
								
						if hasSeenImplementation or hasSeenProgram then // This is pretty easy, others are harder
							if secondPreviousTokenValue <> ':' then // function type
								if secondPreviousTokenValue <> '=' then // function type
								begin
									if tokenType = kSingleCharToken then // operator
										tokenValue := 'operator ' + tokenValue
									else
										//tokenValue := GetTokenInCase(bufferFiles[i], tokenStart, tokenEnd);
									begin
										currentBufferFilename:= bufferFilename[currentBufFile];
										AddToJumpToFunctionArrays(s, tokenValue, tokenStart, tokenEnd, bufferFilename[currentBufFile]);
									//AddToEditFunctionArrays(s, tokenValue, tokenStart, tokenEnd);
									//Writeln('currentBufFile: ', currentBufFile, ', and Token Value: ', tokenValue);
									end;
								end;
						typeFunction:=false;	
						//if tV='(' then parse brackets, as this is special case type Procedure(...);
						if tokenValue = '(' then
						begin
							pos2:=pos;
							startPos:= pos;
							bracketFound:= False;
							bracketCount:=0;
							bracketCount+=1;
							//writeln( 'b4- pos2: ', pos2);
							FinishBracketsinLoops(bufferFiles[currentBufFile], pos2, bracketCount, untilHere, bufferLength);
							typeFunction:= true;
							//writeln( 'after- pos2: ', pos2);
							//writeln('untilHere: ', untilHere);
							endPos:=pos2-1;
							pos:=pos2;
						end;
							
						 if not hasSeenImplementation then //skip the brackets
						     if not typeFunction then
							 begin
							 	repeat GetToken(bufferFiles[currentBufFile], bufferLength, pos, tokenStart, tokenEnd, tokenType, tokenValue);
								until (not (tokenType in [kCRLFToken, kCommentToken])) or (pos >= bufferLength) or (pos >= untilHere);
								//writeln('tokenValue: ', tokenValue, ' pos: ', pos);
								
								//parse brackets, otherwise variables inside global functions also collected and we dont need them.
								if (tokenValue= '(') then
								begin
									pos2:=pos;
									startPos:= pos;
									bracketFound:= False;
									bracketCount:=0;
									bracketCount+=1;
									//writeln( 'b4- pos2: ', pos2);
									FinishBracketsinLoops(bufferFiles[currentBufFile], pos2, bracketCount, untilHere, bufferLength);
									//writeln( 'after- pos2: ', pos2);
									endPos:=pos2-1;
									pos:=pos2;
								end;
							end;
					end;//if previousTokenType = kFunctionToken then
					//else
					if tokenType = kSingleCharToken then
					begin
						//SetInterval(teRec, tokenStart, tokenEnd, 	LWPSintax.VSingleCharColor, LWPSintax.VSingleCharStyle);
													
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
							repeat GetToken(bufferFiles[currentBufFile], bufferLength, pos1, tokenStart, tokenEnd, tokenType, tokenValue);
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
							
						end;//	if tokenType = kSingleCharToken then
					end;		
				end;//if tokenType = (kOtherToken) or (kSingleCharToken) then


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
				
				if pos >= bufferLength then
				begin
					goToNextFile:= true;
					hasSeenImplementation:= false;
				//Writeln('goToNextFile in Files list: ', goToNextFile);
				end;
				
		end; // with
	end; // with JtfnVariables
end; {PascalColorCoder}

var
JtnfSnapshot: snapshot;

//This manages the data of variableMembers, recordVariables
//1. stores lVariableMembers, lRecordVariables, lRecordPointers (snapShot) to fileVariableMembers, fileRecordVariables, fileRecordPointers (Each file info.) 
//2. After each file parsed, list of fileVariableMembers, fileRecordVariables, fileRecordPointers should copy to gVariableMembers, gRecordVariables, gRecordPointers.
//procedure StoreCodeCompletionData();
//Note: CC stands for Code Completion. '*' in comments resembles info. RE. code completion.

procedure JumpToFunctions(var j: JtfnVariables); // Rename store functions
var
	err: OSErr;
//	readOnlyFlag: Boolean;
	editIndex, eINewFile: Longint;
	w: WindowRef;
	i, k:Longint;
	found, isOpenFlag: boolean;
	accessFileName:Boolean;

	procedure CopySnapshotFunctionsToFileFunctions;
	begin
		//Copy functions of snapshot to local array jumpToFunctionName.. //
		jumpToFunctionNameStart[High(jumpToFunctionNameStart)] := JtnfSnapshot.editFunctionNameStart;
		jumpToFunctionNameEnd[High(jumpToFunctionNameEnd)] := JtnfSnapshot.editFunctionNameEnd;
		jumpToFunctionName[High(jumpToFunctionNameEnd)] := JtnfSnapshot.editFunctionName;	
		jumpToFunctionFile[High(jumpToFunctionFile)] := JtnfSnapshot.FunctionFileName;
		closedFilesGlobalFunctions[High(closedFilesGlobalFunctions)]:=JtnfSnapshot.editGlobalFunctions;
	end;
	
	procedure CopySnapShotCCDataToFileData(editIndex:LongInt);
//	var
//		i,j: LongInt;
	begin
		with gClosedFilesVariablesData[editIndex][High(gClosedFilesVariablesData[editIndex])] do
		begin
			//writeln('accessFileName: ', accessFileName);
			if accessFileName then
			begin
				fileName:=JtnfSnapshot.SSfileName;
				//writeln('SSfileName: ', JtnfSnapshot.SSfileName);
				accessFileName:=false;
			end;
		
			{writeln('$Length of JtnfSnapshot.lRecordVariables: ', Length(JtnfSnapshot.lRecordVariables));
			for i:=low(JtnfSnapshot.lRecordVariables) to High(JtnfSnapshot.lRecordVariables) do
			begin
				writeln('JtnfSnapshot.lRecordVariables: ', JtnfSnapshot.lRecordVariables[i].recordName);
			end;}
			fileRecordVariables := JtnfSnapshot.lRecordVariables;
			fileVariableMembers := JtnfSnapshot.lVariableMembers;
			fileRecordPointers := JtnfSnapshot.lRecordPointers;
			//writeln('$Length of fileRecordVariables: ', Length(fileRecordVariables));
			//writeln('$Length of JtnfSnapshot.lVariableMembers: ', Length(JtnfSnapshot.lVariableMembers.variableName));
			//writeln('$Length of fileVariableMembers: ', Length(fileVariableMembers.variableName));
		end;
			
		{for i:=0 to High(gClosedFilesVariablesData[editIndex]) do
		begin
			writeln('$$$FileName:$$$ ', gClosedFilesVariablesData[editIndex][i].fileName);
		end;}
		
		{for i:=0 to High(gClosedFilesVariablesData[editIndex]) do
		begin
			with gClosedFilesVariablesData[editIndex][i] do
			begin
			for j:=0 to high(fileRecordVariables) do
				writeln('$fileRecordVariables: ',j, ' ', gClosedFilesVariablesData[editIndex][i].fileRecordVariables[j].recordName);
			end;
		end;}
	end;
	
	
	procedure CopyFileFunctionsToGlobalFunctions;
	var
		i:LongInt;
	begin
		//Copy functions to global array from local array followed by set length //CopyFileFunctionsToGlobalFunctions
		for i:= 0 to High(jumpToFunctionNameStart[High(jumpToFunctionNameStart)]) do 
		begin
			SetLength(gjumpToFunctionNameStart[editIndex], Length(gjumpToFunctionNameStart[editIndex])+1);
			gjumpToFunctionNameStart[editIndex][High(gjumpToFunctionNameStart[editIndex])] := jumpToFunctionNameStart[High(jumpToFunctionNameStart)][i];
			SetLength(gjumpToFunctionNameEnd[editIndex], Length(gjumpToFunctionNameEnd[editIndex])+1);
			gjumpToFunctionNameEnd[editIndex][High(gjumpToFunctionNameEnd[editIndex])] := jumpToFunctionNameEnd[High(jumpToFunctionNameEnd)][i];
			SetLength(gjumpToFunctionName[editIndex], Length(gjumpToFunctionName[editIndex])+1);
			gjumpToFunctionName[editIndex][High(gjumpToFunctionName[editIndex])] := jumpToFunctionName[High(jumpToFunctionName)][i];
			SetLength(gjumpToFunctionFile[editIndex], Length(gjumpToFunctionFile[editIndex])+1);	
			gjumpToFunctionFile[editIndex][High(gjumpToFunctionFile[editIndex])] := jumpToFunctionFile[High(jumpToFunctionFile)][i];

		end;
					
		for i:= low(closedFilesGlobalFunctions[High(closedFilesGlobalFunctions)]) to High(closedFilesGlobalFunctions[High(closedFilesGlobalFunctions)]) do 
		begin
			SetLength(gclosedFilesGlobalFunctions[editIndex], Length(gclosedFilesGlobalFunctions[editIndex])+1);	
			gclosedFilesGlobalFunctions[editIndex][High(gclosedFilesGlobalFunctions[editIndex])] := closedFilesGlobalFunctions[High(closedFilesGlobalFunctions)][i];
		end;
		
	end;
	
	(*procedure CopyCCFileDataToGlobalData;
	var
	i,j,k:LongInt;
	begin
		for i:=0 to High(closedFileRecordVariables[High(closedFileRecordVariables)]) do
		begin
			SetLength(gRecordVariables[editIndex], Length(gRecordVariables[editIndex])+1);
			gRecordVariables[editIndex][High(gRecordVariables[editIndex])]:= closedFileRecordVariables[High(closedFileRecordVariables)][i];
		end;
		{for i:=0 to High(closedFileRecordVariables[High(closedFileRecordVariables)]) do
		begin
			writeln('gRecordVariables.recordName: ', gRecordVariables[editIndex][i].recordName);
		end;}
		//for j:=0 to High(closedFileVariableMembers) do
		//begin
			//SetLength(gVariableMembers[editIndex], Length(gVariableMembers[editIndex])+1);
			gVariableMembers[editIndex]:= closedFileVariableMembers[High(closedFileVariableMembers)];
		//end;
		{for j:=0 to High(gVariableMembers[editIndex].variableName) do
		begin
			writeln('gVariableMembers.variableName: ', gVariableMembers[editIndex].variableName[j]);
		end;}
		
		for k:=0 to High(closedFileRecordPointers[High(closedFileRecordPointers)]) do
		begin
			SetLength(gRecordPointers[editIndex], Length(gRecordPointers[editIndex])+1);
			gRecordPointers[editIndex][High(gRecordPointers[editIndex])]:= closedFileRecordPointers[High(closedFileRecordPointers)][k];
		end;
		
		{for k:=0 to High(closedFileRecordPointers[High(closedFileRecordPointers)]) do
		begin
			writeln('gRecordPointers: ', gRecordPointers[editIndex][k].pointerVariableName);
		end;}
	end;*)
	
	procedure SetFileFunctionsLength;
	begin
		//For first file 
		SetLength(jumpToFunctionNameStart, Length(jumpToFunctionNameStart)+1);
		SetLength(jumpToFunctionNameEnd, Length(jumpToFunctionNameEnd)+1);
		SetLength(jumpToFunctionName, Length(jumpToFunctionName)+1);
		SetLength(jumpToFunctionFile, Length(jumpToFunctionFile)+1);
		SetLength(closedFilesGlobalFunctions, Length(closedFilesGlobalFunctions)+1);
	end;
	
	//Set length of code completion arrays, for each file
	procedure SetFileCCDataArrayLength(editIndex:LongInt);
	begin
		SetLength(gClosedFilesVariablesData[editIndex], Length(gClosedFilesVariablesData[editIndex])+1);
		with gClosedFilesVariablesData[editIndex][High(gClosedFilesVariablesData[editIndex])] do
			HashInitTable(fileClassNamesHashTable);
		
		{SetLength(closedFileRecordVariables, Length(closedFileRecordVariables)+1);
		SetLength(closedFileVariableMembers, Length(closedFileVariableMembers)+1);
		SetLength(closedFileRecordPointers, Length(closedFileRecordPointers)+1);}
	end;
var
theSpec:AnsiString;
extType:LongInt;

begin
	//Writeln('Entered JumpToNextFunction');
	w := GetFrontMainWindow;
	editIndex:= getWRefCon(w);
	if w <> nil then
	begin
		err := GetEditFSSpec(w, theSpec);
		extType := GetExtensionType(theSpec);
		//writeln('$extType$ ', extType);

	end;
	
	
    with j do
    begin	
	  	//Writeln('totalNoOfBufferFiles and currentBufFile: ', totalNoOfBufferFiles,', ', currentBufFile);
    	if totalNoOfBufferFiles <> 0 then
     	begin
  
			if (totalNoOfBufferFiles > (currentBufFile)) then
			begin
				
//				Writeln('goToNextFile: ', goToNextFile);
				
				if goToNextFile then 
				begin
					InitSnapShot(JtnfSnapshot, 0 );
					goToNextFile:= false;
					if firstFile then
					begin
						firstFile:=false;
						//For first file 
						SetFileFunctionsLength;	
						
						//*Set fileVariableMembers, fileRecordVariables, fileRecordPointers
						SetFileCCDataArrayLength(editIndex);
						accessFileName:=true;
					end
					else
					begin
//						writeln('editIndex from color coding: ', editIndex);
						//Copy functions to global array from local array followed by set length //CopyFileFunctionsToGlobalFunctions
						CopyFileFunctionsToGlobalFunctions;
						
						//*Copy list of fileVariableMembers, fileRecordVariables, fileRecordPointers should copy to
						//gVariableMembers, gRecordVariables, gRecordPointers.
						//CopyCCFileDataToGlobalData;
						
						//Writeln('High(gjumpToFunctionNameStart[editIndex]): ', High(gjumpToFunctionNameStart[editIndex]));
						//for i:= (Low(gjumpToFunctionNameStart[editIndex])) to (High(gjumpToFunctionNameStart[editIndex])) do
						//begin
						//Writeln('Function Start: ',  gjumpToFunctionNameStart[editIndex][i]);
						//Writeln('Function file name: ',  gjumpToFunctionFile[editIndex][i]);
						//Writeln('Function name: ',  gjumpToFunctionName[editIndex][i]);
						//end;
						
						currentBufFile +=1;
						//Writeln('currentBufFile: ', currentBufFile);
						//writeln('***Current BufferFile Name***', bufferFilename[currentBufFile]);
						//set Length of 1st array for every file 
						SetFileFunctionsLength;	
						accessFileName:=true;
						//*Set fileVariableMembers, fileRecordVariables, fileRecordPointers
						SetFileCCDataArrayLength(editIndex);
					end;
				end
				else
				begin
					//Here it should support other languages like C, python, etc
					if (extType=kExtTypePascal) then
						GetFunctionsList(JtnfSnapshot, j, editIndex)
					else if (extType=kExtTypeC) then  
						GetCFunctionsList(JtnfSnapshot, j, editIndex);
					//Copy functions of snapshot to local array jumpToFunctionName.. //
					CopySnapshotFunctionsToFileFunctions;
					
					//*copy snapshot info. to File info.
					//writeln('$$Length: ', Length(JtnfSnapshot.lVariableMembers.variableName));
					CopySnapShotCCDataToFileData(editIndex);
					
				end;
			end //if t>c
			else
			//Here It should update the files list If It is added. Get new list of files, If the number of new files are not same as previous,
			// then append new files and increase the totalnumber of files.  
			begin
				//for k:= 0 to High(gProjectFiles[editIndex]) do
	  			//Writeln('Before check; gProjectFiles: ', gProjectFiles[editIndex][k]);
	  			//End of all the jump to functions proccessed
	  			for i:= 0 to High(gusesList[editIndex]) do
	  			begin
	  				//Writeln('gusesList: ', gusesList[editIndex][i]);
	  				found:= false;
		  		 	for k:= 0 to High(gProjectFiles[editIndex]) do
		  		 	begin
		  		 		//writeln('gProjectFiles[editIndex]: ', gProjectFiles[editIndex][k]);
		  		 		if (LowerCase(gusesList[editIndex][i]) = LowerCase(gProjectFiles[editIndex][k])) then
			  		 	begin
			  		 		found:= true;
			  		 	end;
			  		 
		  		 	end;	
			  		 if not found then
			  		 begin
			  		 SetLength(gProjectFiles[editIndex], Length(gProjectFiles[editIndex])+1);
			  		 gProjectFiles[editIndex][High(gProjectFiles[editIndex])]:= gusesList[editIndex][i];
			  		 
			  		 // For the main file (given by editIndex), if a file is open, we store... what?
			  		 eINewFile := FileIsOpen(gusesList[editIndex][i]);
			  		 isOpenFlag:= eINewFile <> 0; 
			  		 SetLength(gisOpenFile[editIndex], Length(gisOpenFile[editIndex])+1);
			  		 gisOpenFile[editIndex][High(gisOpenFile[editIndex])]:= isOpenFlag; // Why do we want to know?
			  		 
			  		 if isOpenFlag then 
			    	 begin
					    editFunctionFile[eINewFile] := gusesList[editIndex][i]; //Copy the file name
					 end
					 else
					 begin
						SetLength(bufferFiles, Length(bufferFiles)+1);
						SetLength(bufferFilename, Length(bufferFilename)+1);
						bufferFiles[High(bufferFiles)] := ReadFileToString(gusesList[editIndex][i], err); // Load the data
						bufferFilename[High(bufferFilename)] := gusesList[editIndex][i];
						//Writeln('bufferFileName: ', bufferFilename[High(bufferFilename)]);
						//ToDo: Copy the isOpenFile to gisOpenFIle
						
						//writeln('Index of Files closed are: ', index, 'of ', gMultiFiles[i]);
						//Writeln('Length of BufferFiles: ', Length(bufferFiles));
						//index+=1;
					end;	

			  		 totalNoOfBufferFiles := Length(bufferFiles);
			  		 currentBufFile -=1;
			  		 goToNextFile:= true; 
			  		 
//			  		 writeln('totalNoOfBufferFiles: ', totalNoOfBufferFiles);
			  		 end;
		  		 end;
	  		
	  		//for k:= 0 to High(gProjectFiles[editIndex]) do
	  			//Writeln('After check; gProjectFiles: ', gProjectFiles[editIndex][k]);
			end;
		end;// if total<>0
	end //with JtfnVariables
end;
//When to Initialize JtnfSnapshot?
//When to call?
//When to call next file?

procedure GetFilesList(var j: JtfnVariables); 
var
	front: WindowPtr;
	err: OSErr;
	theSpec: FSSpecString;
	offset, extType, editIndex, mainEditIndex,i: Longint;
	tempMultiFiles, MultiFiles: FileArr; //multiFiles should be small.
//	readOnlyFlag:Boolean;
	isOpenFlags: array of Boolean;
	
begin
	//writeln('***Get Files List***');
	with j do
	begin	
		SetLength(bufferFiles, 0);
		SetLength(MultiFiles, 0);
		SetLength(tempMultiFiles, 0);
		SetLength(isOpenFlags, 0);
		SetLength(bufferFilename, 0);
  		// All files in project
				front := GetFrontMainWindow;
				if front <> nil then
				begin
					err := GetEditFSSpec(front, theSpec);
					//Writeln('theSpec: ',theSpec);
					if err = noErr then
					begin
						tempMultiFiles := GetIncludeFileList(theSpec, true);
						SetLength(MultiFiles, Length(MultiFiles)+1);
						MultiFiles[High(MultiFiles)]:= theSpec;
						//writeln('Length of files: ', Length(tempMultiFiles));	
						if Length(tempMultiFiles) > 0 then
						begin
							// Tag med huvudfilen också!
							SetLength(tempMultiFiles, Length(tempMultiFiles)+1);
							//tempMultiFiles[Length(tempMultiFiles)-1] := theSpec;
							//writeln('The Spec', theSpec);
							// Fast egentligen ville jag ha den först...

							//SetLength(MultiFiles, Length(tempMultiFiles));
							for i:= 0 to High(tempMultiFiles) do
							begin
								{if i<1 then
									MultiFiles[i]:= theSpec
								else
								if i>1 then
								begin}
									SetLength(MultiFiles, Length(MultiFiles)+1);
									MultiFiles[High(MultiFiles)]:= tempMultiFiles[i];
								//end;
								
							//	writeln('MultiFiles: ',MultiFiles[i]);
							end;
						end;
						SetLength(isOpenFlags, Length(MultiFiles));
					end;
				end;


	{WriteLn('*** Startar multi-file search! Lista för obj-rens; ***');
	for i := 0 to High(MultiFiles) do
		WriteLn(GetLastToken(MultiFiles[i]));
		
	WriteLn('*** Lista slut: ***');}
		
		// Loop through list, remove binary files (fix 110219) (fixed again 140923)
		offset := 0;
		for i := 0 to High(MultiFiles) do
		begin
			extType := GetExtensionType(MultiFiles[i]);
			//Here Select only supported ext types
			//ToDO: To support C, It should access header files (.h) 
			if (extType = kExtTypeObj) or (extType = kExtTypeUnknown) then
			begin
				//WriteLn('Skipped: ', MultiFiles[i], ' type ', extType);
				offset := offset + 1;
			end
			else
			begin
				//WriteLn('Not skipped: ', MultiFiles[i], ' type ', extType);
				MultiFiles[i - offset] := MultiFiles[i];
			end;
	//		if i+offset <= High(gMultiFiles) then
	//			gMultiFiles[i - offset] := gMultiFiles[i];
		end;
		SetLength(MultiFiles, Length(MultiFiles) - offset);
		//writeln('Length of MultiFiles: ', Length(MultiFiles));			
		
		{if Length(gMultiFiles) = 0 then
		begin
			MessageAlert('No files found to search', 'Check the multi-find seach options');
			Exit(DoMultiFileFind);
		end;}
		
		//SetLength(bufferFiles, Length(gMultiFiles));
		//index:=0;
		mainEditIndex:= GetWRefCon(front);

		for i := 0 to High(MultiFiles) do
		begin
			//writeln('File: ', i, ' ', MultiFiles[i]);
			editIndex := FileIsOpen(MultiFiles[i]);
			isOpenFlags[i] := editIndex <> 0; //isOpenFlags is true If file is open; 

			//Copy the files and isOpenFlgs to Global
			SetLength(gProjectFiles[mainEditIndex], Length(gProjectFiles[mainEditIndex])+1);
			SetLength(gisOpenFile[mainEditIndex], Length(gisOpenFile[mainEditIndex])+1);
						
			gProjectFiles[mainEditIndex][High(gProjectFiles[mainEditIndex])] := MultiFiles[i];  
			gisOpenFile[mainEditIndex][High(gisOpenFile[mainEditIndex])] := isOpenFlags[i];
			
//			writeln(gProjectFiles[mainEditIndex][High(gProjectFiles[mainEditIndex])], ' ', gisOpenFile[mainEditIndex][High(gisOpenFile[mainEditIndex])]);
			
			//If File is Open, save the file names to new glbal array geditFunctionFile 
			    if isOpenFlags[i] then 
			    begin
			    	editFunctionFile[editIndex] := MultiFiles[i];
			    //Writeln('editIndex: ', editIndex,' and ', 'gMultiFiles[i]: ', MultiFiles[i]);
			    //writeln('editFunctionFile[editIndex]: ', editFunctionFile[editIndex]);
			    end
				else
				begin
					SetLength(bufferFiles, Length(bufferFiles)+1);
					SetLength(bufferFilename, Length(bufferFilename)+1);
					bufferFiles[High(bufferFiles)]:= ReadFileToString(MultiFiles[i], err); //reads the text of whole file.
					//Writeln('Data of BufferFiles: ', bufferFiles[High(bufferFiles)]);
					bufferFilename[High(bufferFilename)]:= MultiFiles[i];
					//Writeln('bufferFileName: ', bufferFilename[High(bufferFilename)]);
					//writeln('Index of Files closed are: ', index, 'of ', gMultiFiles[i]);
					//Writeln('Length of BufferFiles: ', Length(bufferFiles));
					//index+=1;
				end;	
		end;
		totalNoOfBufferFiles:= Length(bufferFiles);
	end; // end with JtnfnVariables
end;

function BuildJTFNUsesList(teWind: WindowPtr; includePathInFileName: Boolean): FileArr;
//procedure BuildJTFNUsesList(target: Longint; includePathInFileName: Boolean);
var
	theTitle: Str255;
	extension, fileName: AnsiString;
	done: Boolean;
	editIndex, extType,i :LongInt;
	list: AnsiStringArray;
	err: OSErr;
	homeSpec, foundSpec, mainSpec: FSSpecString;
	front: WindowPtr;
	builtUsesList: FileArr;
begin

	GetWTitle(teWind, theTitle);

	editIndex := GetWRefCon(teWind);
	if editIndex > 0 then
		if editIndex <= kMaxEditWindows then
			list := usesList[editIndex];

	extension := GetExtension(theTitle); 
	extType := GetExtensionType(theTitle);
	
	
	//WriteLn('*** BuildMenuFromUses ***', Length(list));
	//for i := Low(list) to High(list) do
	//	WriteLn('BuildJTFNUsesList: ',list[i]);
	
	//Writeln('the Title: ', theTitle);
	//Writeln('extension: ', extension);
	//Writeln('extType: ', extType);
	
	
	// Pascal
if (extType = kExtTypePascal) or (extType = kExtTypeC) then
	for i := 0 to High(list) do
	begin
		// Old, fast method:
		// AppendMenu(usesMenu, list[i] + extension); // + file extension
		
		// Smarter:
		
		err := GetEditFSSpec(teWind, homeSpec);
		front := GetFrontMainWindow;
		if front <> nil then
			err := GetEditFSSpec(front, mainSpec)
		else
			mainSpec := homeSpec;
		SetFindFileMainSpec(mainSpec);
	
//			writeln('homeSpec: ', homeSpec);
		if (extType = kExtTypeC) then
			fileName := list[i]
		else
			fileName := list[i] + extension;
			//writeln('fileName: ', fileName);
		done := FindFileInPaths(fileName, homeSpec, foundSpec);
//			writeln('done: ', done);
//			writeln('foundSpec: ', foundSpec);
		
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
			fileName := list[i] + '.cpp';
			done := FindFileInPaths(fileName, homeSpec, foundSpec);
		end;
		if not done then
		begin
			fileName := list[i] + '.h';
			done := FindFileInPaths(fileName, homeSpec, foundSpec);
		end;
		if not done then
		begin
			fileName := list[i] + '.c';
			done := FindFileInPaths(fileName, homeSpec, foundSpec);
		end;
		if done then
		begin
			SetLength(builtUsesList, Length(builtUsesList)+1);
			if includePathInFileName then
				builtUsesList[High(builtUsesList)]:= foundSpec
			else
				builtUsesList[High(builtUsesList)]:= fileName;
			
		
		end;
	end;
	
	//for i := 0 to High(builtUsesList) do
		//Writeln('BuildJTFNUsesList ', i, ' :', builtUsesList[i]);
	BuildJTFNUsesList := builtUsesList;
end;

//initialized: array [1..kMaxEditWindows] of Boolean;

procedure UpdateJTFN;
var
	k: Longint;
	ww: WindowRef;
begin
	// No color coding running. Work on "jump to function" for current main window
	k := -1;
	ww := GetFrontMainWindow;
	k:= GetWRefCon(ww);
		//BuildMenuFromUses(teWind: WindowPtr)
		//usesMenu := BuildMenuFromUses(ww{editWind[k]});
	
	//Writeln(usesMenu);
	if (k > 0) and (k <= kMaxEditWindows) then
	with gJtfnVariables[k] do
	begin
		//writeln('Initialized: ', initialized);
		if not initialized then
		begin
			// init everything
			InitJtfnVariables(gJtfnVariables[k]);
			//writeln('$**Update JTFN**$');
			GetFilesList(gJtfnVariables[k]);
			initialized := true;
		end
		else
		begin
			// process!
			//Writeln('Go to Jump to next file');
			// Check here
			JumpToFunctions(gJtfnVariables[k]); // rename
		end;
	end;
	
	//Get files of editWindow, If its not mainWindow. i.e Get the includes of unit, If it's not program.
	
	
	
	
end;

procedure SearchAndAddFunctionslist(var editIndex: Longint; var closedFile: AnsiString);
var 
	i,j,k,l,m: LongInt;
begin
	for i:= 1 to kMaxEditWindows do
		for j:=0 to High(gProjectFiles[i]) do 
		begin
			//if closedFile = gProjectFiles[i][j] then  //To search for the closed file in the gProjectFile
			if (closedFile = gProjectFiles[i][j]) and (gisOpenFile[i][j]) then  //To search for the closed file in the gProjectFile, whose list is not there in gProjectFiles, 
																				//as the closed file was opened before the main file opens. 
// Why test gisOpenFile? Don't we believe that it is open?
			begin
				//Writeln('EditIndex from LWPEdit: ', i);
				//editIndex:= i;
				for k:= 0 to High(editFunctionName[editIndex]) do  // To copy functions to gjumpToFunctionName
				begin
					gisOpenFile[i][j]:= FALSE; //Now it should be reset.
					SetLength(gjumpToFunctionNameStart[i], Length(gjumpToFunctionNameStart[i])+1);
					gjumpToFunctionNameStart[i][High(gjumpToFunctionNameStart[i])] := editFunctionNameStart[editIndex][k];
					SetLength(gjumpToFunctionNameEnd[i], Length(gjumpToFunctionNameEnd[i])+1);
					gjumpToFunctionNameEnd[i][High(gjumpToFunctionNameEnd[i])] := editFunctionNameEnd[editIndex][k];
					SetLength(gjumpToFunctionName[i], Length(gjumpToFunctionName[i])+1);
					gjumpToFunctionName[i][High(gjumpToFunctionName[i])] := editFunctionName[editIndex][k];
					SetLength(gjumpToFunctionFile[i], Length(gjumpToFunctionFile[i])+1);	
					gjumpToFunctionFile[i][High(gjumpToFunctionFile[i])] := editFunctionFile[editIndex];
					
					//writeln('gjumpToFunctionFile[i][High(gjumpToFunctionFile[i])]: ', gjumpToFunctionFile[i][High(gjumpToFunctionFile[i])]);
					//writeln('gjumpToFunctionName[i][High(gjumpToFunctionName[i])]: ', gjumpToFunctionName[i][High(gjumpToFunctionName[i])]);
				end;
				//Copy the functions of closed file to gjtfnlist
				
				//Copy CCData of closed file to global CCData, gRecordVariables, gVariableMembers, gRecordPointers.
				for l:=0 to High(editRecordVariables[editIndex]) do
				begin
					SetLength(gRecordVariables[i], Length(gRecordVariables[i])+1);
					gRecordVariables[i][High(gRecordVariables[i])]:= editRecordVariables[editIndex][l];
				end;
				{for i:=0 to High(closedFileRecordVariables[High(closedFileRecordVariables)]) do
				begin
					writeln('gRecordVariables.recordName: ', gRecordVariables[editIndex][i].recordName);
				end;}
				//for j:=0 to High(closedFileVariableMembers) do
				//begin
					//SetLength(gVariableMembers[editIndex], Length(gVariableMembers[editIndex])+1);
					gVariableMembers[i]:= editVariableMembers[editIndex];
				//end;
				{for j:=0 to High(gVariableMembers[editIndex].variableName) do
				begin
					writeln('gVariableMembers.variableName: ', gVariableMembers[editIndex].variableName[j]);
				end;}
				
				for m:=0 to High(editRecordPointers[editIndex]) do
				begin
					SetLength(gRecordPointers[i], Length(gRecordPointers[i])+1);
					gRecordPointers[i][High(gRecordPointers[i])]:= editRecordPointers[editIndex][m];
				end;
				
				{for k:=0 to High(closedFileRecordPointers[High(closedFileRecordPointers)]) do
				begin
					writeln('gRecordPointers: ', gRecordPointers[editIndex][k].pointerVariableName);
				end;}
		
			end;
		end;
end;

procedure CloseJTFNForWindow(editIndex: Longint);
var
	closedFileName: AnsiString;
begin
	closedFileName:= GetPathFromIndex(editIndex); // fileSpec[editIndex];
//		closedFileName:= fileSpec[editIndex];
		SearchAndAddFunctionslist(editIndex, closedFileName);
		
		//Reset insitilized of jtfn variables
		if (editIndex > 0) and (editIndex <= kMaxEditWindows) then
	  	with gJtfnVariables[editIndex] do
	  	begin
//	  		writeln('Initialized: ', initialized);
  			if initialized then
  			initialized := false;
  		end;
end;

//var
//	bTIndex: array[1..kMaxEditWindows] of LongInt;

Procedure PerformBackJTFN(editIndex: Longint);
var
//	backTrace:LongInt;
	fnStartPos, fnEndPos, fOpen: LongInt;
	fileName: AnsiString;
	s: String;
begin
//	bTIndex[editIndex]-=1;
//	backTrace:= bTIndex[editIndex]; // have to think bcoz It should be top when new jTFN    Yes, this looks OK. We save the JTFN per MAIN file. /Ingemar
						// But the bTIndex helps little, the length of the arrays are more important!
//	if backTrace<0 then
//		backTrace:=0;
	//if backTrace >=0 then
	//begin
	HelpAppendLn('PerformBackJTFN');
	if editIndex < 1 then Exit(PerformBackJTFN);
	if Length(backTraceStart[editIndex]) > 1 then
	begin
	// Go to the SECOND latest!
		Str(editIndex, s);
		HelpAppendLn('PerformBackJTFN being done on window '+s);

		fnStartPos:= backTraceStart[editIndex][High(backTraceStart[editIndex])-1];
		fnEndPos:= backTraceEnd[editIndex][High(backTraceStart[editIndex])-1];
		fileName:= backTraceFile[editIndex][High(backTraceStart[editIndex])-1];

		fOpen := FileIsOpen(fileName);
		if fOpen>0 then
			SelectWindow(editWind[fOpen])
		else
			fOpen := OpenFileToWindow(fileName);

		SetLength(backTraceStart[editIndex], Length(backTraceStart[editIndex])-1);
		SetLength(backTraceEnd[editIndex], Length(backTraceEnd[editIndex])-1);
		SetLength(backTraceFile[editIndex], Length(backTraceFile[editIndex])-1);
	
		HSetSelection(teEdit[fOpen], fnStartPos, fnEndPos);
		HShowSelection(teEdit[fOpen]^.views[teEdit[fOpen]^.focusedView], true); // For the FOCUSED one!
		HInvalViews(teEdit[fOpen]);
	end
	else
		HelpAppendLn('Can not PerformBackJTFN');

end;

// editIndex refers to MAIN file!
// Possible improvement: The result is now applied in two separate places. It would be better to apply them on one place. /ingemar
procedure PerformJumpToFunction(editIndex: Longint; itemString, theRootSpec: AnsiString);
var
	fileName: AnsiString;
	i, k, fn: Longint;
	front: WindowPtr;
	fnEditIndex, fnStartPos, fnEndPos, oldStartPos, oldEndPos:LongInt;
	eIOpenFile: LongInt;
begin
		//Search for the editString in Open files using global files list (gProjectFiles).
		for i := 0 to High(gProjectFiles[editIndex]) do
		begin
			eIOpenFile := FileIsOpen(gProjectFiles[editIndex][i]); // If the file is open then it returns it's editIndex.
			//isOpenFlags[i] := eIOpenFile <> 0; //isOpenFlags is true If file is open; 
			
		    if eIOpenFile > 0 then
		    begin
		    	for k:= 0 to High(editFunctionName[eIOpenFile]) do 
				begin
					//Writeln('Item String from Open files: ', LowerCase(itemString), ' Function Name: ', editFunctionName[eIOpenFile][k] );
					if LowerCase(itemString) = LowerCase(editFunctionName[eIOpenFile][k]) then
					begin
						SelectWindow(editWind[eIOpenFile]);
						front:= GetFrontEditWindow;
						fnEditIndex:= getWRefCon(front);

						fnStartPos:= editFunctionNameStart[eIOpenFile][k];
						fnEndPos:= editFunctionNameEnd[eIOpenFile][k];
						
						// First time, save where we come from!
						if Length(backTraceStart[editIndex]) = 0 then
						begin
							SetLength(backTraceStart[editIndex], 2);
							SetLength(backTraceEnd[editIndex], 2);
							SetLength(backTraceFile[editIndex], 2);

							HGetSelection(teEdit[fnEditIndex], oldStartPos, oldEndPos);

							backTraceStart[editIndex][0]:= oldStartPos;
							backTraceEnd[editIndex][0]:= oldEndPos;
							backTraceFile[editIndex][0]:= GetPathFromIndex(editIndex);	
						end
						else
						begin
							SetLength(backTraceStart[editIndex], Length(backTraceStart[editIndex])+1);
							SetLength(backTraceEnd[editIndex], Length(backTraceEnd[editIndex])+1);
							SetLength(backTraceFile[editIndex], Length(backTraceFile[editIndex])+1);
						end;

						HSetSelection(teEdit[fnEditIndex], fnStartPos, fnEndPos+1);
						HShowSelection(teEdit[fnEditIndex]^.views[teEdit[fnEditIndex]^.focusedView], true); // For the FOCUSED one!
						// UpdateLineNumber?
						HInvalViews(teEdit[fnEditIndex]);

						backTraceStart[editIndex][High(backTraceStart[editIndex])]:= fnStartPos;
						backTraceEnd[editIndex][High(backTraceEnd[editIndex])]:= fnEndPos;
						backTraceFile[editIndex][High(backTraceFile[editIndex])]:= GetPathFromIndex(editIndex);	
//						bTIndex[editIndex]+=1;
						// Note: bTIndex will get out of synch as written! Why do we need it?
						
						Exit(PerformJumpToFunction);
					end;
				end;
		    end;  
		end;
		
		//Search for the editString in the closed files. And Open the file to show that function
		for k:= 0 to High(gjumpToFunctionName[editIndex]) do 
		begin
			//Writeln('Item String from closed files: ', LowerCase(itemString), ' Function Name: ', gjumpToFunctionName[editIndex][k] );
			
			if LowerCase(itemString) = LowerCase(gjumpToFunctionName[editIndex][k]) then
			begin
				// find or open that function file with index k
				fileName:= gjumpToFunctionFile[editIndex][k];
				//Writeln('Filename: ', fileName);
				fn:= FindOrOpen(fileName, theRootSpec); //theRootSpec is already updated above (files in the project)
				if fn > 0 then
				begin
					SelectWindow(editWind[fn]);

					front:= GetFrontEditWindow;
					fnEditIndex:= getWRefCon(front);
					
					fnStartPos:= gjumpToFunctionNameStart[editIndex][k];
					fnEndPos:= gjumpToFunctionNameEnd[editIndex][k];

					// First time, save where we come from!
					if Length(backTraceStart[editIndex]) = 0 then
					begin
						SetLength(backTraceStart[editIndex], 2);
						SetLength(backTraceEnd[editIndex], 2);
						SetLength(backTraceFile[editIndex], 2);

						HGetSelection(teEdit[fnEditIndex], oldStartPos, oldEndPos);

						backTraceStart[editIndex][0]:= oldStartPos;
						backTraceEnd[editIndex][0]:= oldEndPos;
						backTraceFile[editIndex][0]:= GetPathFromIndex(editIndex);	
					end
					else
					begin
						SetLength(backTraceStart[editIndex], Length(backTraceStart[editIndex])+1);
						SetLength(backTraceEnd[editIndex], Length(backTraceEnd[editIndex])+1);
						SetLength(backTraceFile[editIndex], Length(backTraceFile[editIndex])+1);
					end;
									
					backTraceStart[EditIndex][High(backTraceStart[EditIndex])]:= fnStartPos;
					backTraceEnd[EditIndex][High(backTraceEnd[EditIndex])]:= fnEndPos;
					backTraceFile[EditIndex][High(backTraceFile[EditIndex])]:= fileName;	

					HSetSelection(teEdit[fnEditIndex], fnStartPos, fnEndPos+1);
					HShowSelection(teEdit[fnEditIndex]^.views[teEdit[fnEditIndex]^.focusedView], true); // For the FOCUSED one!
					HInvalViews(teEdit[fnEditIndex]);

//					bTIndex[editIndex]+=1;
					//then look for extension as above
					// then Highlight the function as 	
				end;
			end;
		end;
		//SysBeep(1); // This is where we should look for functions!
end;

end.

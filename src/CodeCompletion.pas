// By Ingemar Ragnemalm 130417. Implements local code completion. Drawback: Can't work as built-in
// documentation. Strength: Works with basically any text, any language.

// But since then Sreehari has added a more serious code completion for
// FPC and C/C++!

unit CodeCompletion;
interface
uses
	MacOSAll, UtilsTypes, strutils, Halda, HaldaTypes, LWPGlobals, ClosedFilesDataManager;

function FindCompletions(source: AnsiString; origPos: Longint; var startPos: Longint; editIndex,frontMainEditIndex: Longint): AnsiStringArray;
function FindCompletionsInTE(teEdit: HaldaPtr; var fromPos, toPos: Longint; editIndex,frontMainEditIndex: Longint): AnsiStringArray;
//function TEToString(teEdit: TXNObject): AnsiString;

implementation
uses
	 LWPEdit;
	 
function FindCompletionsInTE(teEdit: HaldaPtr; var fromPos, toPos: Longint; editIndex,frontMainEditIndex: Longint): AnsiStringArray;
var
//	buffer: AnsiString;
	oStartOffset, oEndOffset: Longint;
begin
	HGetSelection(teEdit, oStartOffset, oEndOffset);
	//WriteLn('Got selection at ', oStartOffset, ',', oEndOffset);
	toPos := oEndOffset;
//	buffer := teEdit^.text;
	//WriteLn('Got buffer');
	FindCompletionsInTE := FindCompletions(teEdit^.text, oEndOffset, fromPos, editIndex,frontMainEditIndex);
end;
var
	arrayVariable:boolean;
	arrayOffsetStart, arrayOffsetEnd:LongInt;
	arrayOffset:AnsiString;

function FindRecordVariable(s:AnsiString):AnsiString;
var
	posEndVariable,posStartVariable, pos1: LongInt;
	gettingRecord, quitParsing: boolean;
	//recordVariable: AnsiString;
begin
//parse findStr. If '.' found, then pick the string before '.'
	pos1:= length(s);
	gettingRecord:= false;
	quitParsing:=false;
	arrayVariable:=false;
	arrayOffsetStart:=0;
	arrayOffsetEnd:=0;
	arrayOffset:='';
	if pos1>0 then
		repeat 
			if (s[pos1]='.') then
			begin
			 	posEndVariable:= pos1-1;
			 	gettingRecord:= true;
			end;
			//if '[' or ']' there in string, pick string before '['. So that the variable name can be picked from string. 
			if gettingRecord then
			repeat 
				pos1 -=1;
				if (s[pos1]=' ') then
				begin
					quitParsing:= true;	
				end;
				if (s[pos1]='[') then
				begin
					arrayOffsetEnd:=posEndVariable;
					posEndVariable:= pos1-1;
					arrayVariable:= true;
					arrayOffsetStart:=pos1;
					arrayOffset:= copy(s, arrayOffsetStart, (arrayOffsetEnd-arrayOffsetStart+1));
					//writeln('arrayOffset: ', arrayOffset);
				end;
					
			until quitParsing or (pos1=0);
			
			if quitParsing or (pos1=0) then
			begin
				if quitParsing then
					posStartVariable:= pos1+1
				else
					posStartVariable:= pos1;
				
				FindRecordVariable := copy(s, posStartVariable, posEndVariable);
				//writeln('FindRecordVariable: ', FindRecordVariable);
			end;
			pos1-=1;
		until (pos1<0) or quitParsing;
end;

function GetBufferScope(bufPos, editIndex:LongInt):AnsiString;
var
	i:LongInt;
	posDiff, lowestDiff, index:LongInt;
	bufferScope:AnsiString;
begin
	posdiff:=0;
	lowestDiff:=0;
	index:=0;
	//writeln('bufPos: ', bufPos);

		//Get the scope here. 
		//1. predict the scope (functionScope) based on pos.
		//1a. get the position at 'Esc' then should find out closest pos by comparing all positions of functions in the file. 
		//for i:=0 to High(editFunctionNameStart[editIndex]) do
		//begin
		if (Length(editFunctionNameStart[editIndex]) >0 ) then
		begin
			i:=0;
			repeat
				//writeln('editFunctionNameStart: ', editFunctionNameStart[editIndex][i]);
				//writeln('editFunctionName: ', editFunctionName[editIndex][i]);
				posDiff:= bufPos-editFunctionNameStart[editIndex][i];
				if (i=0) then
					lowestDiff:= posDiff;
				//writeln( 'posDiff: ', posDiff, ' lowestDiff: ', lowestDiff);
				if posDiff>0 then
				begin
					if posDiff <= lowestDiff then
					begin
						lowestDiff:= posdiff;
						index:=i;
						//writeln('index: ', index, ' i: ', i);
					end;
				end;
				i+=1;
			until (posDiff < 0) or (i>High(editFunctionNameStart[editIndex]));
		end;
		
		
		//writeln('posDiff: ', posDiff, ' index: ', index);
		//Get scope here
		if lowestDiff>0 then
		begin
			GetBufferScope:= editFunctionName[editIndex][index];
			writeln('GetBufferScope: ', editFunctionName[editIndex][index]);
		end;
end;

function FindRecordName(frontMainEditIndex, editIndex:LongInt; s:AnsiString; bufPos:LongInt):AnsiString;
var
	i,j,k:Longint;
	posDiff, lowestDiff, index:LongInt;
	bufferScope:AnsiString;
	foundVariable:Boolean;
	eIOpenFile:LongInt;
	list: AnsiStringArray;
begin
	//1 a. Search for recordVariable in variable list, i.e. gVariableMembers[editIndex].VariableName 
	//from i:= o to high. Take vaue of 'i' for which recordVariable matched.
	//1 b. then get the gVariableMembers[editIndex].VariableName[i], which supposed to be the record name

	//writeln('bufPos: ', bufPos);
	with editVariableMembers[editIndex] do
	begin

		//Get bufferScope
		bufferScope:= GetBufferScope(bufPos, editIndex);
		writeln('*bufferScope:* ', bufferScope);
		foundVariable:= false;		
		//1. look for varName in scope, i.e. functional scope
		if bufferScope <> '' then
		for i:=0 to High(VariableName) do // Here 'VariableName' is record member of editVariableMembers. //can be replaced by repeat.
		begin
			//writeln('variableScope1: ', variableScope[i]);
			if lowercase(variableScope[i]) = lowercase(bufferScope) then
			begin
				//writeln('*varName:* ', VariableName[i] );
				if (VariableName[i]) = Lowercase(s) then
				begin
					foundVariable:= true;
					//writeln('**varName:** ', VariableName[i] );
					if i<= High(VariableType) then
					begin
						FindRecordName:= VariableType[i];
						//writeln('varName: ', VariableName[i], ' varType: ',VariableType[i] , ' recName: ', FindRecordName);
						Exit(FindRecordName);
					end;
				end;
			end;
		end;
		//2. If not found, then look for varName in local scope
		if not foundVariable then
			for i:=0 to High(VariableName) do // Here 'VariableName' is record member of editVariableMembers. //can be replaced by repeat.
			begin
				//writeln('variableScope2: ', variableScope[i]);
				//writeln('*varName....:* ', VariableName[i] );
				if lowercase(variableScope[i]) = 'local' then
				begin
					//writeln('*varName:* ', VariableName[i] );
					if (VariableName[i]) = Lowercase(s) then
					begin
						foundVariable:= true;
						//writeln('**varName:** ', VariableName[i] );
						if i<= High(VariableType) then
						begin
							FindRecordName:= VariableType[i];
							//writeln('varName: ', VariableName[i], ' varType: ',VariableType[i] , ' recName: ', FindRecordName);
							Exit(FindRecordName);
						end;
					end;
				end;
			end;
	end;
	
		//writeln('foundVariable: ', foundVariable);
		//3. If not found, then look for varName in global scope
	if not foundVariable then 
		with editVariableMembers[editIndex] do
		begin
			//writeln('Look for VariableNames in editFile');
	    	for j:= 0 to High(variableName) do 
			begin
				//writeln('*varNameglobal:* ', VariableName[i] );
				if lowercase(variableScope[j]) = 'global' then
				begin
					//writeln('variableScope3: ', variableScope[j]);
					//writeln('*varName:* ', VariableName[j] );
					if (VariableName[j]) = Lowercase(s) then
					begin
						foundVariable:= true;
						//writeln('**Found varName:** ', VariableName[j] );
						if j<= High(VariableType) then
						begin
							FindRecordName:= VariableType[j];
							//writeln('editFile: varName: ', VariableName[j], ' varType: ',VariableType[j] , ' recName: ', FindRecordName);
							Exit(FindRecordName);
						end;
					end;
				end;	
			end;
		end;
		
		
		if not foundVariable then 
		//Variable name should be searched in all project files. It should be done in two steps. 1. Open files 2. closed files
			//1. in openfiles
			//writeln('editIndex: ', editIndex);
			//writeln('High(gProjectFiles[editIndex]: ', High(gProjectFiles[editIndex]));
			
			
			//1. Get usesList[editIndex]. Might be usefull.
			{if editIndex > 0 then
			if editIndex <= kMaxEditWindows then
				list := usesList[editIndex];

		extension := GetExtension(theTitle); 
		extType := GetExtensionType(theTitle);}
		if editIndex > 0 then
			if editIndex <= kMaxEditWindows then
				list := BuildJTFNUsesList(editWind[editIndex], true {withPath}); //usesList[editIndex];
				
				//writeln('Length of files list: ', Length(list));
				//for i := Low(list) to High(list) do
				 //writeln('list: ', list[i]);
		
			//2. If the file is Open, then Search from editVariableMembers[eIOpenFile]
			//3. If not open, Make sure If the filename is there in the closedFilelist of gClosedFilesVariablesData[editIndex][i].fileName.
			//4. Then gClosedFilesVariablesData[editIndex][i].fileVariableMembers.variableScope[0...high].
		
			//for i := 0 to High(gProjectFiles[frontMainEditIndex]) do //frontMainEditIndex-->editIndex
			//writeln('Looking for VariableNames in list of files...');
			for i := Low(list) to High(list) do 
			begin
				//eIOpenFile := FileIsOpen(gProjectFiles[frontMainEditIndex][i]); //If the file is open then it returns it's editIndex.
				eIOpenFile := FileIsOpen(list[i]); //If the file is open then it returns it's editIndex.
				//writeln('eIOpenFile: ', eIOpenFile);
				//writeln('list ', i, ': ', list[i]);
				if eIOpenFile > 0 then
				begin
			    	with editVariableMembers[eIOpenFile] do
					begin
						//writeln('Look for VariableNames in Opened files');
				    	for j:= 0 to High(variableName) do 
						begin
							//writeln('*varNameglobal:* ', VariableName[i] );
							if lowercase(variableScope[j]) = 'global' then
							begin
								//writeln('variableScope3: ', variableScope[j]);
								//writeln('*varName:* ', VariableName[j] );
								if (VariableName[j]) = Lowercase(s) then
								begin
									foundVariable:= true;
									//writeln('**Found: varName:** ', VariableName[j] );
									if j<= High(VariableType) then
									begin
										FindRecordName:= VariableType[j];
										//writeln('List Opened file: varName: ', VariableName[j], ' varType: ',VariableType[j] , ' recName: ', FindRecordName);
										Exit(FindRecordName);
									end;
								end;
							end;	
						end;
					end;
				end
				else//2.in closedfiles
				begin
					for k:=low(gClosedFilesVariablesData[frontMainEditIndex])  to High(gClosedFilesVariablesData[frontMainEditIndex]) do
					begin
						if (list[i] = gClosedFilesVariablesData[frontMainEditIndex][k].fileName) then
						begin        
							//writeln('closed file, list: ', list[i]);
							with gClosedFilesVariablesData[frontMainEditIndex][k].fileVariableMembers do
							begin
								//writeln('Length of variableNames: ', Length(variableName));
								for j:= low(variableName) to High(variableName) do 
								begin
									if lowercase(variableScope[j]) = 'global' then
									begin
										//writeln('variableScope4: ',  variableScope[j]);
										//writeln('*varName:* ', VariableName[j] );
										if (VariableName[j]) = Lowercase(s) then
										begin
											foundVariable:= true;
											//writeln('**varName:** ', VariableName[j] );
											if j<= High(VariableType) then
											begin
												FindRecordName:= VariableType[j];
												//writeln('closed file: varName: ', VariableName[j], ' varType: ',VariableType[j] , ' recName: ', FindRecordName);
												Exit(FindRecordName);
											end;
										end;
									end;	
								end;
							end;
						end;
					end;
				end;	
			end;

		
		{for i:=0 to High(VariableName) do // Here 'VariableName' is record member of editVariableMembers. //can be replaced by repeat.
		begin

			//writeln('varName: ', VariableName[i] );
			if (VariableName[i]) = Lowercase(s) then
			begin
				if i<= High(VariableType) then
					FindRecordName:= VariableType[i];
				//writeln('varName: ', VariableName[i], ' varType: ',VariableType[i] , ' recName: ', FindRecordName);
			end;
		end;}
	
end;

function GetrecordMembersList(frontMainEditIndex,editIndex:LongInt; s, recVariable:AnsiString;bufPos:LongInt): AnsiStringArray;
var
i,j,k,l,m:LongInt;
foundMatch:Boolean;
bufferScope:AnsiString;
eIOpenFile:LongInt;
list:AnsiStringArray;
membersList: AnsiStringArray;
begin
	//get scope here
	bufferScope:= GetBufferScope(bufPos, editIndex);
	
	//writeln('bufferScope:*', bufferScope);
	
// search 's' for the recordName in recordName list, i.e., gRecordVariables[editIndex][].recordName from i:= 0 to high.
	// and then get the recordName of 'i' for which it is matched.
	//for i:=0 to High(gRecordVariables[editIndex]) do //is it possible to replace with repeat? yes, possible. Modified.

	foundMatch:= false;
	i:=0;
	//Search in functional scope
	repeat 
	begin
		if (Length(editRecordVariables[editIndex])>0) then
		with editRecordVariables[editIndex][i] do
		begin
			if lowercase(recordScope) = lowercase(bufferScope) then
				if LowerCase(s) = recordName then
				begin
					foundMatch:= true;
					//if Length(membersList)<>0 then
						//setLength(membersList, 0);
					for k:=0 to High(recordMembers) do 
					begin
						if recordMembers[k]<>'' then
						begin
							SetLength(membersList, Length(membersList)+1);
					
							if arrayVariable then
							begin
								membersList[High(membersList)]:= recVariable + arrayOffset + '.' + recordMembers[k];
								//writeln('arrayOffset: ', arrayOffset, ' arrayVariable: ', arrayVariable, ' arrayOffsetStart: ', arrayOffsetStart, ' arrayOffsetEnd: ', arrayOffsetEnd);
							end
							else
							begin
								membersList[High(membersList)]:= recVariable + '.' + recordMembers[k];
								//writeln('arrayVariable2: ', arrayVariable);
							end;
						end;
					end;
					//writeln('Length of recMembersList: ', Length(membersList));
					{for j:=0 to High(membersList) do
					begin
						//writeln('j: ', j);
						writeln('membersList FUNCTIONSCOPE: ', membersList[j]);
					end;}
				end;
		end;
		i+=1;
	end;
	until (i> High(editRecordVariables[editIndex])) or foundMatch;

	////Search in local scope
	if not foundMatch then
	begin
		i:=0;
		repeat 
		begin
			if (Length(editRecordVariables[editIndex])>0) then
			with editRecordVariables[editIndex][i] do
			begin
				if lowercase(recordScope) = 'local' then
					if LowerCase(s) = recordName then
					begin
						foundMatch:= true;
						//if Length(membersList)<>0 then
							//setLength(membersList, 0);
						for k:=0 to High(recordMembers) do 
						begin
							if recordMembers[k]<>'' then
							begin
								SetLength(membersList, Length(membersList)+1);
								if arrayVariable then
								begin
									membersList[High(membersList)]:= recVariable + arrayOffset + '.' + recordMembers[k];
									//writeln('arrayOffset: ', arrayOffset, ' arrayVariable: ', arrayVariable, ' arrayOffsetStart: ', arrayOffsetStart, ' arrayOffsetEnd: ', arrayOffsetEnd);
								end
								else
								begin
									membersList[High(membersList)]:= recVariable + '.' + recordMembers[k];
									//writeln('arrayVariable2: ', arrayVariable);
								end;
							end;
						end;
						//writeln('Length of recMembersList: ', Length(membersList));
						{for j:=0 to High(membersList) do
						begin
							//writeln('j: ', j);
							writeln('membersList LOCALSCOPE: ', membersList[j]);
						end;}
					end;
			end;
			i+=1;
		end;
		until (i> High(editRecordVariables[editIndex])) or foundMatch;
	end;
	
	if editIndex > 0 then
		if editIndex <= kMaxEditWindows then
			list := BuildJTFNUsesList(editWind[editIndex], true {withPath}); //usesList[editIndex];
			
			{for i := Low(list) to High(list) do
			 writeln('list: ', list[i]);}
		

	//Search in Global scope
	//1a. in front file.
	if not foundMatch then
	begin
	//writeln('editIndex from LCC: ', editIndex);
		//writeln('Looking for recMemList in editFile');
	//	writeln('Length of editRecordVariables: ', High(editRecordVariables[editIndex]));
		j:=0;
		repeat 
		begin
			if (Length(editRecordVariables[editIndex])>0) then 
			with editRecordVariables[editIndex][j] do
			begin	
				if lowercase(recordScope) = 'global' then
				begin
					//writeln('editFile, recordName: ', recordName);
					if s = LowerCase(recordName) then // Fix by Sreehari entered by Ingemar 160414
					begin
						foundMatch:= true;
						//if Length(membersList)<>0 then
							//setLength(membersList, 0);
						for k:=0 to High(recordMembers) do 
						begin
							if recordMembers[k]<>'' then
							begin
								SetLength(membersList, Length(membersList)+1);
								if arrayVariable then
								begin
									membersList[High(membersList)]:= recVariable + arrayOffset + '.' + recordMembers[k];
									//writeln('arrayOffset: ', arrayOffset, ' arrayVariable: ', arrayVariable, ' arrayOffsetStart: ', arrayOffsetStart, ' arrayOffsetEnd: ', arrayOffsetEnd);
								end
								else
								begin
									membersList[High(membersList)]:= recVariable + '.' + recordMembers[k];
									//writeln('arrayVariable2: ', arrayVariable);
								end;
								
								
							end;
						end;
					end;
						{writeln('Length of recMembersList: ', Length(membersList));
						for l:=0 to High(membersList) do
						begin
							//writeln('j: ', j);
							writeln('membersList GLOBALSCOPE: ', membersList[l]);
						end;}
					end;
			end;
			j+=1;
		end;
		until (j> High(editRecordVariables[editIndex])) or foundMatch;
	end;
	
	//1b. in opened uses list of files.
	if not foundMatch then
	begin
		//for i := 0 to High(gProjectFiles[frontMainEditIndex]) do
		for i := 0 to High(list) do
		begin
			//eIOpenFile := FileIsOpen(gProjectFiles[frontMainEditIndex][i]); // If the file is open then it returns it's editIndex.
			eIOpenFile := FileIsOpen(list[i]); // If the file is open then it returns it's editIndex.
			//isOpenFlags[i] := eIOpenFile <> 0; //isOpenFlags is true If file is open; 
			//writeln('list ', i, ' ', list[i]);
		    if eIOpenFile > 0 then
		    begin	
				j:=0;
				repeat 
				begin
					if (Length(editRecordVariables[eIOpenFile])>0) then 
					with editRecordVariables[eIOpenFile][j] do
					begin
						if lowercase(recordScope) = 'global' then
						begin
							//writeln('OpenedFile, recordName: ', recordName);
							if s = LowerCase(recordName) then // Fix by Sreehari entered by Ingemar 160414
							begin
								foundMatch:= true;
								//if Length(membersList)<>0 then
									//setLength(membersList, 0);
								for k:=0 to High(recordMembers) do 
								begin
									if recordMembers[k]<>'' then
									begin
										SetLength(membersList, Length(membersList)+1);
										if arrayVariable then
										begin
											membersList[High(membersList)]:= recVariable + arrayOffset + '.' + recordMembers[k];
											//writeln('arrayOffset: ', arrayOffset, ' arrayVariable: ', arrayVariable, ' arrayOffsetStart: ', arrayOffsetStart, ' arrayOffsetEnd: ', arrayOffsetEnd);
										end
										else
										begin
											membersList[High(membersList)]:= recVariable + '.' + recordMembers[k];
											//writeln('arrayVariable2: ', arrayVariable);
										end;
										
										
									end;
								end;
							end;
								{writeln('Length of recMembersList: ', Length(membersList));
								for l:=0 to High(membersList) do
								begin
									//writeln('j: ', j);
									writeln('membersList GLOBALSCOPE: ', membersList[l]);
								end;}
							end;
					end;
					j+=1;
				end;
				until (j> High(editRecordVariables[eIOpenFile])) or foundMatch;
			end //2.closed project
			else
			begin
				for m:=0 to High(gClosedFilesVariablesData[frontMainEditIndex]) do
				begin
					if (list[i] = gClosedFilesVariablesData[frontMainEditIndex][m].fileName) then
					begin
						l:=0;
						repeat 
						begin
							//if (High(gRecordVariables[frontMainEditIndex])>0) then
							if (Length(gClosedFilesVariablesData[frontMainEditIndex][m].fileRecordVariables)>0) then
							with gClosedFilesVariablesData[frontMainEditIndex][m].fileRecordVariables[l] do
							//with gRecordVariables[frontMainEditIndex][l] do
							begin
								if lowercase(recordScope) = 'global' then
								begin	
									//writeln('closedFile, recordName: ', recordName);
									if s = LowerCase(recordName) then // Fix by Sreehari entered by Ingemar 160414
									begin
										
										foundMatch:= true;
										//if Length(membersList)<>0 then
											//setLength(membersList, 0);
										for k:=0 to High(recordMembers) do 
										begin
											if recordMembers[k]<>'' then
											begin
												SetLength(membersList, Length(membersList)+1);
												//membersList[High(membersList)]:= recVariable + '.' + recordMembers[k];
												if arrayVariable then
												begin
													membersList[High(membersList)]:= recVariable + arrayOffset + '.' + recordMembers[k];
													//writeln('arrayOffset: ', arrayOffset, ' arrayVariable: ', arrayVariable, ' arrayOffsetStart: ', arrayOffsetStart, ' arrayOffsetEnd: ', arrayOffsetEnd);
												end
												else
												begin
													membersList[High(membersList)]:= recVariable + '.' + recordMembers[k];
													//writeln('arrayVariable2: ', arrayVariable);
												end;
											end;
										end;
									end;
								 
										//writeln('Length of recMembersList: ', Length(membersList));
										for j:=0 to High(membersList) do
										begin
											//writeln('j: ', j);
											//writeln('membersList LOCALSCOPE: ', membersList[j]);
										end;
									end;
							end;
							l+=1;
						end;
						until (l> High(gClosedFilesVariablesData[frontMainEditIndex][m].fileRecordVariables)) or foundMatch;	
					end;
				end;
			end;
		end;
	end;

	//2.Closed Project files
	{if not foundMatch then
	begin
		l:=0;
		repeat 
		begin
			if (High(gRecordVariables[frontMainEditIndex])>0) then
			with gRecordVariables[frontMainEditIndex][l] do
			begin
		
				if lowercase(recordScope) = 'global' then
					
					if LowerCase(s) = recordName then
					begin
						
						foundMatch:= true;
						//if Length(membersList)<>0 then
							//setLength(membersList, 0);
						for k:=0 to High(recordMembers) do 
						begin
							if recordMembers[k]<>'' then
							begin
								SetLength(membersList, Length(membersList)+1);
								//membersList[High(membersList)]:= recVariable + '.' + recordMembers[k];
								if arrayVariable then
								begin
									membersList[High(membersList)]:= recVariable + arrayOffset + '.' + recordMembers[k];
									//writeln('arrayOffset: ', arrayOffset, ' arrayVariable: ', arrayVariable, ' arrayOffsetStart: ', arrayOffsetStart, ' arrayOffsetEnd: ', arrayOffsetEnd);
								end
								else
								begin
									membersList[High(membersList)]:= recVariable + '.' + recordMembers[k];
									//writeln('arrayVariable2: ', arrayVariable);
								end;
							end;
						end;
				
						//writeln('Length of recMembersList: ', Length(membersList));
						for j:=0 to High(membersList) do
						begin
							//writeln('j: ', j);
							//writeln('membersList LOCALSCOPE: ', membersList[j]);
						end;
					end;
			end;
			l+=1;
		end;
		until (l> High(gRecordVariables[frontMainEditIndex])) or foundMatch;
	end;}

	GetrecordMembersList := membersList;
end;

function GetGlobalFunctionsList(findStr:AnsiString; frontMainEditIndex,editIndex, pos:LongInt):AnsiStringArray;
var
	functionsList, list:AnsiStringArray;
	i, j, k, eIOpenFile:LongInt;
begin
	writeln('Entered GetGlobalFunctionsList');
	SetLength(functionsList, 0);
	if frontMainEditIndex > 0 then
	if frontMainEditIndex <= kMaxEditWindows then
		list := BuildJTFNUsesList(editWind[frontMainEditIndex], true {withPath}); //usesList[editIndex];
		
		//writeln('Length of files list: ', Length(list));
		//for i := Low(list) to High(list) do
			//writeln('list: ', list[i]);

	//2. If the file is Open, then Search from editVariableMembers[eIOpenFile]
	//3. If not open, Make sure If the filename is there in the closedFilelist of gClosedFilesVariablesData[editIndex][i].fileName.
	//4. Then gClosedFilesVariablesData[editIndex][i].fileVariableMembers.variableScope[0...high].


// NOTE: The following code seems to lock up! Why? Or is the problem somewhere else?

	//for i := 0 to High(gProjectFiles[frontMainEditIndex]) do //frontMainEditIndex-->editIndex
	//writeln('Looking for VariableNames in list of files...');
	for i := Low(list) to High(list) do 
	begin
		//eIOpenFile := FileIsOpen(gProjectFiles[frontMainEditIndex][i]); //If the file is open then it returns it's editIndex.
		eIOpenFile := FileIsOpen(list[i]); //If the file is open then it returns it's editIndex.
		writeln('eIOpenFile: ', eIOpenFile);
		writeln('Length(list): ', Length(list), 'low(list: )', low(list), ' High(list): ', High(list));
		writeln('list ', i, ': ', list[i]);
		if eIOpenFile > 0 then
		begin
			//writeln('list: ', list[i], 'Length(editGlobalFunctions): ',Length(editGlobalFunctions[eIOpenFile]));
			for j:=low(editGlobalFunctions[eIOpenFile]) to High(editGlobalFunctions[eIOpenFile]) do //By Sreehari 090416
			begin
//				if RPosex(findStr, editGlobalFunctions[eIOpenFile][j], pos)>0 then //By Sreehari 090416
				if findStr = Copy(editGlobalFunctions[eIOpenFile][j], 1, Length(findStr)) then //By Sreehari 090416
				begin
					SetLength(functionsList, Length(functionsList)+1);
					functionsList[High(functionsList)] := editGlobalFunctions[eIOpenFile][j]; //By Sreehari 090416
					//writeln('High(resultArr): ', functionsList[High(functionsList)]);
				end;
			end;
		end;
		{else
		begin
		
			
		end;}
	end;
	if Length(gclosedFilesGlobalFunctions[frontMainEditIndex]) > 0 then
	for k:=low(gclosedFilesGlobalFunctions[frontMainEditIndex]) to High(gclosedFilesGlobalFunctions[frontMainEditIndex]) do //By Sreehari 090416
	begin
		if findStr = Copy(gclosedFilesGlobalFunctions[frontMainEditIndex][k], 1, Length(findStr)) then
//		if RPosex(findStr,gclosedFilesGlobalFunctions[frontMainEditIndex][k], pos)>0 then //By Sreehari 090416
		begin
			SetLength(functionsList, Length(functionsList)+1);
			functionsList[High(functionsList)] := gclosedFilesGlobalFunctions[frontMainEditIndex][k]; //By Sreehari 090416
			//writeln('closed High(resultArr): ', functionsList[High(functionsList)]);
		end;
	end;
	
	GetGlobalFunctionsList:= functionsList;
	writeln('Done GetGlobalFunctionsList');
end;

//This is no more localcodecompletion now. It does parse all the files in project now.
function FindCompletions(source: AnsiString; origPos: Longint; var startPos: Longint; editIndex,frontMainEditIndex: Longint): AnsiStringArray;
var
	findStr, outStr: AnsiString;
	pos, pos1, endPos, hitPos, hitEnd: Longint;
	resultArr: array of AnsiString;
	
	recVariable, recName :AnsiString;
	i,j, count:LongInt;
	recMembersList: array of AnsiString;
	dotFound: Boolean;
	//foundMatch: boolean;
begin
	SetLength(resultArr, 0);
// Find findStr from position
	pos := origPos;
	endPos := pos;
	while pos > 0 do
	begin
		if source[pos-1] in ['0'..'9', 'a'..'z', 'A'..'Z', '.', '[', ']', '^' ] then
			pos -= 1
		else
			Break;
	end;
	findStr := Copy(source, pos, endPos - pos);
	writeln('findStr: ', findStr);
	
	//parse findStr. If it has '.', then call few functions to get record members. 
		pos1:= Length(findStr);
		dotFound:=false;
		if pos1 > 0 then
			repeat
				writeln('A');
				if (findStr[pos1]='.') then
				begin
					writeln('B');
				 	dotFound:= true;
				end;
				
				pos1-=1;
			until (dotFound) or (pos1<0); 
			writeln('C');
	//Get Global functions.
	resultArr := GetGlobalFunctionsList(findStr, frontMainEditIndex, editIndex, pos);
	writeln('F');
			
	//Get Record Variable.if string has '.'
	if dotFound then
	begin
		writeln('D');
		recVariable := FindRecordVariable(findStr); //rename trimToRecordVariable
		writeln('recordVariable: ', recVariable);
		
		//Find recordName from VariableMembers list, which matches recVariable.
		recName:= FindRecordName(frontMainEditIndex, editIndex, recVariable, origPos);
		writeln('recName: ', recName);

		//get recordMembersList
		if Length(recMembersList)<>0 then
			SetLength(recMembersList, 0);
		if not (recName= '') then
			recMembersList := GetrecordMembersList(frontMainEditIndex, editIndex, recName, recVariable, origPos);
		
		writeln('Length of recMembersList: ', Length(recMembersList));
		if Length(recMembersList)<>0 then
			resultArr:=recMembersList;
		{for j:=0 to High(recMembersList) do
		begin
			writeln('recMembersList: ', recMembersList[j]);
		end;}
	end;
	writeln('E');

	
	startPos := pos; // Return start of search string = area to replace!
	if findStr = '' then // Nothing to look for
	begin
		FindCompletions := resultArr;
		Exit(FindCompletions);
	end;
	
	//WriteLn(pos, ',', endPos, ',', findStr);

	pos := Length(source);
	while pos > 0 do
	begin
		hitPos := RPosex(findStr, source, pos); // Searches backwards for string
		if hitPos = 0 then Break;
		if hitPos = pos then Break;
		pos := hitPos; //  Length(findStr);
		
		//WriteLn('Found a "', findStr, '" at ', hitPos, '(', origPos, ')');
// Disregard the one at the starting position
		if hitPos+Length(findStr) <> origPos then
// Only accept strings that don't have alphanumerics right before!
			if hitPos > 0 then
				if not (source[hitPos-1] in ['0'..'9', 'a'..'z', 'A'..'Z']) then
				begin
// Grab string forward from the find!
					hitEnd := hitPos + Length(findStr);
					while hitEnd < Length(source) do
					begin
						if source[hitEnd] in ['0'..'9', 'a'..'z', 'A'..'Z', '.', '[', ']'] then
							hitEnd += 1
						else 
							Break;
					end;
//					WriteLn('Out: ', Copy(source, hitPos, hitEnd-hitPos));
// And disregard duplicates!
					outStr := Copy(source, hitPos, hitEnd-hitPos);
//					dupl := false;
//					for i := 0 to High(resultArr) do
//						if resultArr[i] = outStr then
//					WriteLn('Out="', outStr, ' Find="', findStr, '"');
					if outStr <> findStr then // Not just the same
					if not AnsiMatchStr(outStr, resultArr) then
					begin
						SetLength(resultArr, Length(resultArr)+1);
						resultArr[High(resultArr)] := outStr;
						//WriteLn('Appended: ', Copy(source, hitPos, hitEnd-hitPos));
					end;
				end;
	end;
	FindCompletions := resultArr;
end;

end.

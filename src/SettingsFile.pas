// This unit manages storing preferences
// Local prefs are stored near the main program file,
// globals in the prefs folder

unit SettingsFile;
interface
uses
	MacOSAll, UtilsTypes, FileUtils;

type
	SettingsRec = record
		postName: AnsiString;
		data: AnsiString;
	end;
	SettingsArray = array of SettingsRec;

function GetSetting(editIndex: Integer; postName: AnsiString): AnsiString;overload;
function GetSetting(editIndex: Integer; postName: AnsiString): Longint;overload;
function GetGlobalSetting(postName: AnsiString): AnsiString;overload;
function GetGlobalSetting(postName: AnsiString): Longint;overload;
function GetGlobalSetting(postName: AnsiString; defaultValue: AnsiString): AnsiString;overload;
procedure SetSetting(editIndex: Integer; postName: AnsiString; theData: AnsiString);overload;
procedure SetSetting(editIndex: Integer; postName: AnsiString; theNum: Longint);overload;
procedure SetGlobalSetting(postName: AnsiString; theData: AnsiString);overload;
procedure SetGlobalSetting(postName: AnsiString; theNum: Longint);overload;

// Load settings for given file/path
procedure LoadSettings(fileName: FSSpecString; editIndex: Longint); overload;
// Load based on array - can not skip data identical to global
procedure LoadSettings(fileName: AnsiString; var settings: SettingsArray); overload;
procedure LoadSettings(editIndex: Longint); overload;
procedure SaveSettings(fileName: FSSpecString; settings: SettingsArray); overload;
procedure SaveSettings(editIndex: Longint); overload;
procedure LoadGlobalSettings(prefsName: AnsiString);
procedure SaveGlobalSettings(prefsName: AnsiString);

implementation

uses
	LWPEdit;
var
	projectSettings: array of SettingsArray;
	globalSettings: SettingsArray;
	
//	fileName: array [1..10] of AnsiString = (
//		'/Users/ingemar/test/a.test', '/Users/ingemar/test/b.test', '/Users/ingemar/test/c.test',
//		'/Users/ingemar/test/d.test', '/Users/ingemar/test/e.test',
//		'/Users/ingemar/test/f.test', '/Users/ingemar/test/g.test',
//		'/Users/ingemar/test/h.test', '/Users/ingemar/test/i.test', '/Users/ingemar/test/j.test');
	
// Why am I not using a hash table for this?
// Maybe because the number of items should be pretty low - unlike the color coding
// dictionaries


function GetSetting(editIndex: Integer; postName: AnsiString): AnsiString;overload;
var
	i: Longint;
begin
	GetSetting := '';
	if editIndex < 1 then
		Exit(GetSetting);
	if High(projectSettings) < editIndex then
		SetLength(projectSettings, editIndex + 1);

	for i := 0 to High(projectSettings[editIndex]) do
	begin
		if postName = projectSettings[editIndex][i].postName then
		begin
			GetSetting := projectSettings[editIndex][i].data;
//			WriteLn('Found ', postName, ' = "', projectSettings[editIndex][i].data, '"');
			Exit(GetSetting);
		end;
//		else
//			WriteLn(postName, ' does not match ', projectSettings[editIndex][i].postName);
	end;
	// Also search in globalSettings!
	for i := 0 to High(globalSettings) do
	begin
		if postName = globalSettings[i].postName then
		begin
			GetSetting := globalSettings[i].data;
//			WriteLn('Found global ', postName, ' = "', globalSettings[i].data, '"');
			Exit(GetSetting);
		end;
	end;
end;

function GetSetting(editIndex: Integer; postName: AnsiString): Longint;overload;
var
	theNum: Longint;
	theData: AnsiString;
begin
	theData := GetSetting(editIndex, postName);
	Val(theData, theNum);
	GetSetting := theNum;
end;




function GetGlobalSetting(postName: AnsiString): AnsiString;overload;
var
	i: Longint;
begin
	GetGlobalSetting := '';
	for i := 0 to High(globalSettings) do
	begin
		if postName = globalSettings[i].postName then
		begin
			GetGlobalSetting := globalSettings[i].data;
			Exit; // (GetGlobalSetting);
		end;
	end;
end;

function GetGlobalSetting(postName: AnsiString; defaultValue: AnsiString): AnsiString;overload;
var
	i: Longint;
begin
	GetGlobalSetting := '';
	for i := 0 to High(globalSettings) do
	begin
		if postName = globalSettings[i].postName then
		begin
			GetGlobalSetting := globalSettings[i].data;
			Exit; // (GetGlobalSetting);
		end;
	end;
	SetGlobalSetting(postName, defaultValue);
	GetGlobalSetting := defaultValue;
end;

function GetGlobalSetting(postName: AnsiString): Longint;overload;
var
	theNum: Longint;
	theData: AnsiString;
begin
	theData := GetGlobalSetting(postName);
	Val(theData, theNum);
	GetGlobalSetting := theNum;
end;



procedure SetSetting(editIndex: Integer; postName: AnsiString; theData: AnsiString);overload;
var
	i, j, globalIndex: Longint;
begin
	if editIndex < 1 then
		Exit; // (SetSetting);
	if High(projectSettings) < editIndex then
		SetLength(projectSettings, editIndex + 1);

	globalIndex := -1; // No global
//	WriteLn('Is it in global settings?');
	for i := 0 to High(globalSettings) do
	begin
		if postName = globalSettings[i].postName then
		begin
			globalIndex := i; // Found a match in global setting
		end;
	end;

// If no global setting exists, it should be set! And then we don't need a local one. (Yet.)
// Added 160321
	if globalIndex = -1 then
	begin
		SetGlobalSetting(postName, theData);
		Exit; // (SetSetting);
	end;
	
//	WriteLn('Is it in the settings?');
	for i := 0 to High(projectSettings[editIndex]) do
	begin
		if postName = projectSettings[editIndex][i].postName then
		begin
			if globalIndex <> -1 then
				if globalSettings[i].data = theData then
				begin // Same value - remove from projectSettings!
				// For now, just set it anyway.
				// No, fix this! Now! Fixed 160321
					for j := i+1 to High(projectSettings[editIndex]) do
					begin
						projectSettings[editIndex][j-1] := projectSettings[editIndex][j];
					end;
					SetLength(projectSettings[editIndex], Length(projectSettings[editIndex])-1);
//					projectSettings[editIndex][i].data := theData;
				end
				else // Not same value
					projectSettings[editIndex][i].data := theData
			else // Did not exist in global
				projectSettings[editIndex][i].data := theData;
			Exit; // (SetSetting);
		end;
	end;
	// If we get here, the setting was not available
//	WriteLn('If we get here, the setting was not available!');
	SetLength(projectSettings[editIndex], Length(projectSettings[editIndex])+1);
//	WriteLn('Length of projectSettings = ', Length(projectSettings[editIndex]));
	projectSettings[editIndex][High(projectSettings[editIndex])].postName := postName;
	projectSettings[editIndex][High(projectSettings[editIndex])].data := '';
	projectSettings[editIndex][High(projectSettings[editIndex])].data := theData;
//	WriteLn('Setting data to "', theData, '"')
end;

procedure SetSetting(editIndex: Integer; postName: AnsiString; theNum: Longint);overload;
var
	theData: AnsiString;
begin
	Str(theNum, theData);
	SetSetting(editIndex, postName, theData);
end;


procedure SetGlobalSetting(postName: AnsiString; theData: AnsiString);overload;
var
	i: Longint;
begin
	for i := 0 to High(globalSettings) do
	begin
		if postName = globalSettings[i].postName then
		begin
			globalSettings[i].data := theData;
			Exit; {SetGlobalSetting}
		end;
	end;
	
	// If we get here, the setting was not available
	SetLength(globalSettings, Length(globalSettings)+1);
	globalSettings[High(globalSettings)].postName := postName;
	globalSettings[High(globalSettings)].data := theData;
end;

procedure SetGlobalSetting(postName: AnsiString; theNum: Longint);overload;
var
	theData: AnsiString;
begin
	Str(theNum, theData);
	SetGlobalSetting(postName, theData);
end;



// Load settings for given file/path
procedure LoadSettings(fileName: FSSpecString; editIndex: Longint); overload;
var
	err: OSErr;
	readOnlyFlag: Boolean;
	fileData, fieldName, fieldLengthStr, fieldData: AnsiString;
	fieldLength, i, start: Longint;
begin
	if High(projectSettings) < editIndex then
		SetLength(projectSettings, editIndex + 1);
	
	fileName := TrimExtension(fileName) + '.settings';
	fileData := ReadFileToString(fileName, err);
	// If no file, use global setting
	if err <> noErr then
		Exit;//(LoadSettings);
	
	// Read post by post
	// Any missing field should be filled in from global data
	// or that data is simply read from global data if found missing
//	WriteLn('STARTS LOADING');
	i := 1;
	while i < Length(fileData) do
	begin
		// Parse field name
		start := i;
		while fileData[i] <> #13 do
			i += 1;
		fieldName := Copy(fileData, start, i - start);
//		WriteLn('Read field name "', fieldName, '"');
		// Parse field length
		i += 1; // skip cr
		start := i;
		while fileData[i] <> #13 do
			i += 1;
		// Copy field data
		fieldLengthStr := Copy(fileData, start, i - start);
		Val(fieldLengthStr, fieldLength);
//		WriteLn('Read field length "', fieldLengthStr, '" decoded to ', fieldLength);
		i+=1;
		start := i;
		i += fieldLength;
		fieldData := Copy(fileData, start, i - start);
//		WriteLn('Read field data "', fieldData, '"');
		i += 1; // Skip CR
		// Set the setting
		SetSetting(editIndex, fieldName, fieldData);
	end;
//	WriteLn('FINISHED LOADING');
end;

// Load based on array - can not skip data identical to global
procedure LoadSettings(fileName: AnsiString; var settings: SettingsArray); overload;
var
	err: OSErr;
	readOnlyFlag: Boolean;
	fileData, fieldName, fieldLengthStr, fieldData: AnsiString;
	fieldLength, i, start: Longint;
begin
	fileName := TrimExtension(fileName) + '.settings';
//	WriteLn('Loads prefs from "', fileName, '"');
	fileData := ReadFileToString(fileName, err);
	if fileData = '' then
		Exit; // (LoadSettings); // But what if there really are none?
	SetLength(settings, 0);
	i := 1;
	while i < Length(fileData) do
	begin
		// Parse field name
		start := i;
		while fileData[i] <> #13 do
			i += 1;
		fieldName := Copy(fileData, start, i - start);
//		WriteLn('Read field name "', fieldName, '"');
		// Parse field length
		i += 1; // skip cr
		start := i;
		while fileData[i] <> #13 do
			i += 1;
		// Copy field data
		fieldLengthStr := Copy(fileData, start, i - start);
		Val(fieldLengthStr, fieldLength);
//		WriteLn('Read field length "', fieldLengthStr, '" decoded to ', fieldLength);
		i+=1;
		start := i;
		i += fieldLength;
		fieldData := Copy(fileData, start, i - start);
//		WriteLn('Read field data "', fieldData, '"');
		i += 1; // Skip CR
		// Set the setting
//		SetSetting(editIndex, fieldName, fieldData);
		SetLength(settings, Length(settings)+1);
		settings[High(settings)].postName := fieldName;
		settings[High(settings)].data := '';
		settings[High(settings)].data := fieldData;
	end;
	
end;

procedure LoadSettings(editIndex: Longint); overload;
var
	fileName: AnsiString; // or FSSpecString
begin
	if High(projectSettings) < editIndex then
		SetLength(projectSettings, editIndex + 1);
	fileName := GetPathFromIndex(editIndex);
	LoadSettings(fileName, projectSettings[editIndex]);
end;

// Save settings for given file/path
procedure SaveSettings(fileName: FSSpecString; settings: SettingsArray); overload;
var
	err: OSErr;
	readOnlyFlag: Boolean;
	fileData, dataLengthStr: AnsiString;
	i: Longint;
begin
	fileName := TrimExtension(fileName) + '.settings';
//	WriteLn('Saves prefs from "', fileName, '"');
	// Save post by post
	// Any field equal to global data is skipped
	// First: If all fields equal to global, don't save! Any existing file should be removed or at least cleared.
	
	fileData := '';
	for i := 0 to High(settings) do
	begin
		Str(Length(settings[i].data), dataLengthStr);
		fileData += settings[i].postName + #13 + 
			dataLengthStr + #13 + settings[i].data + #13;
	end;
//	WriteLn('WRITTEN DATA: #', fileData, '#');
	
	// Save string to file
	if Length(fileData) = 0 then
	begin // Delete file if no data! TEST BEFORE USING!!! 160321
		WriteLn('SUGGESTS DELETING '+fileName+'!!!!!');
		WriteLn('SUGGESTS DELETING '+fileName+'!!!!!');
		WriteLn('SUGGESTS DELETING '+fileName+'!!!!!');
		WriteLn('SUGGESTS DELETING '+fileName+'!!!!!');
		WriteLn('SUGGESTS DELETING '+fileName+'!!!!!');
//		unlink(fileName)
	end
	else
		err := WriteStringToFile(fileData, fileName);
end;

procedure SaveSettings(editIndex: Longint); overload;
var
	fileName: AnsiString; // or FSSpecString
begin
	if High(projectSettings) < editIndex then
		SetLength(projectSettings, editIndex + 1);
	fileName := GetPathFromIndex(editIndex);
	SaveSettings(fileName, projectSettings[editIndex]); // fileSpec in LWPEdit
end;

(*
function FSRefToPathNameString (var ref: FSRef): AnsiString; // Str255;
var
	path: AnsiString;
	err: OSErr;
	p: PChar;
begin
	p := PChar(NewPtrClear(1024)); // calloc?
	err := FSRefMakePath(ref, p, 1024);
	path := p;
	DisposePtr(Ptr(p));
	
	if err = noErr then
		FSRefToPathNameString := path
	else
		FSRefToPathNameString := '';
end;

// Change current dir to prefs
procedure GoToPrefs;
var
	err: OSErr;
	path: AnsiString;
	prefsFolderRef: FSRef;
begin
	err := FSFindFolder(0, kPreferencesFolderType, kCreateFolder, prefsFolderRef);
	if err = noErr then
	begin
		path := FSRefToPathNameString(prefsFolderRef);
		chdir(path);
		WriteLn('Current Path: ', path);
	end;
end;

function GetPrefsPath: AnsiString;
var
	err: OSErr;
	path: AnsiString;
	prefsFolderRef: FSRef;
begin
	err := FSFindFolder(0, kPreferencesFolderType, kCreateFolder, prefsFolderRef);
	if err = noErr then
	begin
		path := FSRefToPathNameString (prefsFolderRef);
		GetPrefsPath := path;
	end
	else
		GetPrefsPath := '';
end;*)

procedure LoadGlobalSettings(prefsName: AnsiString);
begin
	LoadSettings(GetPreferencesFolder + '/' + prefsName, globalSettings);
end;

procedure SaveGlobalSettings(prefsName: AnsiString);
begin
	SaveSettings(GetPreferencesFolder + '/' + prefsName, globalSettings);
//	WriteLn('Saved global settings to "', GetPreferencesFolder + '/' + prefsName, '"!');
end;

end.
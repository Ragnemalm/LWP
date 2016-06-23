// Skiss till system fšr att gšra auto-save av alla filer.

unit AutoSave;
interface


implementation

	var
		autoSaveTimer: EventLoopTimerRef = nil;
		gNextIndex = Low(editWind);

function TXNToString(te: TXNObject): AnsiString;
var
	chars: Handle;
begin
			err := TXNGetDataEncoded(te, kTXNStartOffset, kTXNEndOffset, chars, kTXNTextData);
			if err <> noErr then
			begin
				SaveTXNToFile := err;
				Exit(SaveTXNToFile);
			end;
			HLock(chars);
			
			if gFlags^^.textSaveFormat = kSaveUnix then
				ConvertToUnixLineBreaks(Handle(chars));
			
			SetLength(fileData, GetHandleSize(chars));
			MoveBytes(chars^, @fileData[1], GetHandleSize(chars));
			DisposeHandle(chars);
			TXNToString := err;
end;

// Save, open, close -> remove autosave
procedure CleanAutoForFileOnSaveOpenClose(index: Longint);
var
	path: AnsiString;
begin
	Str(gNextIndex, s);
	path := GetPreferencesFolder +'/LightweightIDEAutosaves/'+s+'.autosave');
	if FileExists(path) then
		DeleteFile(path);
	path := GetPreferencesFolder +'/LightweightIDEAutosaves/'+s+'.filename');
end;

	procedure DoAutoSaveTimer(t: EventLoopTimerRef; p: Word{Pointer}; action: Pointer);MWPascal;
	var
		editIndex: Longint;
		fileData, pref: AnsiString;
		s: String;
	begin
// One index each time it is called?
// Less risk for tiresome delays!

// dirty = file not saved
// autodirty = file not autosaved
// not dirty -> not autodirty!

		// Check the file at gNextIndex
		if editWind[gNextIndex] <> nil then // Window is open
		if not dirty[gNextIndex] then // Update this in the undo system!
		begin
			// Delete autosave file if it exists
		end
		else
		if autodirty[gNextIndex] then
		begin
			// So where should I save?
			// Prefs folder? Yes!
			pref := GetPreferencesFolder;
			if not FileExists(prefs + 'LightweightIDEAutosaves') then
				CreateFolder(prefs + 'LightweightIDEAutosaves');
			// prefs folder/LightweightIDEAutosaves/?
			
			// Save fileSpec[gNextIndex] in prefs folder/LightweightIDEAutosaves/nr.filename
			// Save file to prefs folder/LightweightIDEAutosaves/nr.autosave
			// spec := GetFileSpecFromIndex(gNextIndex)
			// Save window positions too? nr.position?
			
			Str(gNextIndex, s);
			fileData := TXNToString(teEdit[gNextIndex]);
			err := WriteStringToFile(fileData, pref+'/LightweightIDEAutosaves/'+s+'.autosave');
			err := WriteStringToFile(fileSpec[gNextIndex], pref+'/LightweightIDEAutosaves/'+s+'.filename');
			
// 1.autosave = file data
// 1.filename = file name and path
		end;
		gNextIndex += 1;
		if gNextIndex > High(editWind) then
			gNextIndex := Low(editWind);
	end;

	// Irrelevant?
	procedure CheckAutosavedWhenOpen(path: AnsiString);
	begin
		// Check if any autosaved file matches the one being opened.
		// If so, check the date.
		// If autosave newer, alert the user.
		// Or the other way around? If autosave older, alert?
		
		// Always remove autosave on close? No, put in some subfolder.
		// Auto-save all on quit. (Don't bother asking for save.)
		// Always re-open from autosaves when the program opens?
	end;
	
	procedure AutoReOpen; // On launch
	begin
	end;

	procedure AutoSaveAll; // On quit
	begin
	end;

	procedure AutoSaveInit;
	begin
		InstallEventLoopIdleTimer( GetMainEventLoop(), kEventDurationSecond*2, kEventDurationSecond*2,
							DoAutoSaveTimer, nil, autoSaveTimer);
	end;

end.

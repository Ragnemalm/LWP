// Lightweight Pascal IDE, shell script and makefile execution unit
// © Ingemar Ragnemalm 2006-2009

{$mode macpas}
unit BuildWithScript;
interface
uses
	MacOSAll, LWPGlobals, FileUtils, Classes, SysUtils, ProcessUtils,
	ErrorMessageWindow, FindReplaceDialog, Settings, ColorCoding, Console,
	BuildWithPascal, UtilsTypes;

function ExecuteScript(theSpec: FSSpecString): OSErr;
function BuildMakefile(theSpec: FSSpecString; buildApp, buildFinal: Boolean): OSErr;
//function RunMakefile(theSpec: FSSpecString): OSErr;


implementation

procedure ShellscriptCompilationDone(aborted: Boolean; result: Integer; privateData: Pointer);
var
	srcSpec: FSSpecString;
	cleanName: AnsiString;
	err: OSErr;
	fndrInfo: FInfo;
begin
	HelpAppendLn('---------------');
	if aborted then
		HelpAppendLn('Script aborted')
	else	
	if result <> noErr then
		HelpAppendLn('Script run returned error!')
	else
		HelpAppendLn('Script done!');
	
	HelpMainMessage('');
end; {MakefileCompilationDone}

procedure BuildShellscriptData(oneLineOfData: AnsiString; privateData: Pointer);
begin
	HelpAppendLn(oneLineOfData);
//	ParseGCCErrorMessage(oneLineOfData);
	HelpForceUpdate;
end;

function ExecuteScript(theSpec: FSSpecString): OSErr;
var
//	AProcess: TProcess;
	theProcess: ProcessPtr;
	path, commandLine, pathOnly: AnsiString;
	myString: AnsiString;
//	procBuffer: AnsiString;
//	aStringList: TStringList;
	cliName, ext: AnsiString;
//	n: Longint;
begin
	// Create a full pathname to the file as AnsiString
	path := theSpec;
//	ConvertPathToCommandLineFormat(path);
	
	// We need the path without filename.
	pathOnly := path;
	pathOnly := TrimLastToken(pathOnly);

	chdir(MacRomanToUTF8(pathOnly)); // Set that as current dir
	
	cliName := GetLastToken(theSpec);
//	ConvertPathToCommandLineFormat(cliName); // Allow spaces in name
	ext := GetExtension(cliName);
	if ext = '.sh' then
		commandLine := {gSettings???} '/bin/sh ' + cliName;
	if ext = '.pl' then
		commandLine := {gSettings???} '/usr/bin/perl ' + cliName;
	if ext = '.rb' then
		commandLine := {gSettings???} '/usr/bin/ruby ' + cliName;
	if ext = '.py' then
		commandLine := {gSettings???} '/usr/bin/python ' + cliName;
	if ext = '.js' then
		commandLine := {gSettings???} '/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc ' + cliName;
	if ext = '.lua' then
		commandLine := {gSettings???} '/usr/local/bin/lua ' + cliName;

	HelpAppendLn('Running script "' + GetLastToken(theSpec) + '"');
	HelpAppendLn('Command: ' + commandLine);

	LaunchCompilation(commandLine, theSpec, true{NA}, true{NA}, nil{compileCallback}, @BuildShellscriptData, @ShellScriptCompilationDone);

(*
Old way - locks up until done. This was only for short scripts.

	theProcess := ProcessLaunch(commandline, true);
	repeat
		if ProcessReadLn(theProcess, myString) then
		begin
			HelpAppendLn(myString);
			HelpForceUpdate;
		end;
	until not ProcessRunning(theProcess) and (theProcess^.inBuffer = '');
	ProcessDispose(theProcess);
*)
	
	return noErr;
end;

procedure BuildMakefileData(oneLineOfData: AnsiString; privateData: Pointer);
begin
	HelpAppendLn(oneLineOfData);
	ParseGCCErrorMessage(oneLineOfData);
	HelpForceUpdate;
end;

procedure MakefileCompilationDone(aborted: Boolean; result: Integer; privateData: Pointer);
var
	srcSpec: FSSpecString;
	cleanName: AnsiString;
	err: OSErr;
	fndrInfo: FInfo;
begin
	if aborted then
		HelpAppendLn('Compilation aborted')
	else	
	if result <> noErr then
		HelpAppendLn('Compilation NOT successful!')
	else
		HelpAppendLn('Done!');
	
	HelpMainMessage('');
end; {MakefileCompilationDone}

function BuildMakefile(theSpec: FSSpecString; buildApp, buildFinal: Boolean): OSErr;
var
	theProcess: ProcessPtr;
	path, commandLine, pathOnly: AnsiString;
	myString: AnsiString;
	cliName: AnsiString;
begin
// buildApp and buildFinal can be used for triggering other make targets than
// the standard one. Not yet implemented though.

	gLastMainFile := theSpec;
	// Create a full pathname to the file as AnsiString
	path := theSpec;
	
	// We need the path without filename.
	pathOnly := path;
	pathOnly := TrimLastToken(pathOnly);

	chdir(MacROmanToUTF8(pathOnly)); // Set that as current dir
	
	commandLine := {gSettings???} '/usr/bin/make';
	
	HelpAppendLn('Command: ' + commandLine);
	HelpMainMessage('make');
	
	LaunchCompilation(commandLine, theSpec, true{NA}, true{NA}, nil{compileCallback}, @BuildMakefileData, @MakefileCompilationDone);
	
	return noErr;
end;

// NO LONGER USED - handled with "Run" in main program
(*function RunMakefile(theSpec: FSSpecString): OSErr;
var
	theProcess: ProcessPtr;
	path, commandLine, pathOnly: AnsiString;
	myString: AnsiString;
	cliName: AnsiString;
begin
	// Create a full pathname to the file as AnsiString
	path := FSSpecToPathNameString(theSpec);
	
	// We need the path without filename.
	pathOnly := path;
	TrimLastToken(pathOnly);

	chdir(pathOnly); // Set that as current dir
	
	commandLine := {gSettings???} '/usr/bin/make run';

	HelpAppendLn('Command: ' + commandLine);

	theProcess := ProcessLaunch(commandline, true);
	repeat
		if ProcessReadLn(theProcess, myString) then
		begin
			HelpAppendLn(myString);
			HelpForceUpdate;
		end;
	until not ProcessRunning(theProcess) and (theProcess^.inBuffer = '');
	ProcessDispose(theProcess);
	
	return noErr;
end;*)


end.

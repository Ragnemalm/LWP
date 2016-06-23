// Lightweight Pascal IDE, GPC compilation interface unit
// © Ingemar Ragnemalm 2009

{$mode macpas}
unit BuildWithAda;
interface
uses
	MacOSAll, LWPGlobals, UtilsTypes, FileUtils, Classes, SysUtils, ProcessUtils,
	ErrorMessageWindow, FindReplaceDialog, AlertsUtils, BaseUnix,
	Settings, ColorCoding, Console, BuildWithPascal, IncludeAnalyzer;

function CompileAda(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
procedure SaveCommandToFile(Line, fileName: AnsiString);

implementation

// Same as C
procedure CompileCData(oneLineOfData: AnsiString; privateData: Pointer);
begin
	HelpAppendLn(oneLineOfData);
	ParseGCCErrorMessage(oneLineOfData);
	HelpForceUpdate;
end;

(*
function BuildAdaCommandline(theSpec: FSSpecString; buildApp, buildFinal: Boolean; target: Longint; targetExtension: Boolean): AnsiString;
	var
		folderSpec: FSSpecString;
		commandLine, pathOnly: AnsiString;
		err: OSErr;
		i: Longint;
		appDirId: Longint;
		cliName: AnsiString;
		pathList: AnsiStringArray;
const
	CR = Char(13);
begin
		// Remember what the main file was. (Used for searching errors from the error window!)
		gLastMainFile := theSpec;
		
		// Create a full pathname to the file as AnsiString
		//path := FSSpecToPathNameString(theSpec);
		
		cliName := theSpec.name;
		
		// We need the path without filename.
		pathOnly := TrimLastToken(pathOnly);
WriteLn('Try chdir to: "', pathOnly, '"');
		chdir(MacRomanToUTF8(pathOnly)); // Set that as current dir
WriteLn('chdir OK');
		
//		if target = kPPCTarget then
//			commandLine := gSettingsPPCCompiler + ' ' + cliName{theSpec.name} + ' ' + gSettingsOptions
//		else {386}
//			commandLine := gSettings386Compiler + ' ' + cliName{theSpec.name} + ' ' + gSettingsOptions;
		commandLine := gSettingsAdaCompiler + ' ' + cliName{theSpec.name} + ' ' + gSettingsOptions;
		WriteLn('CompileAda ', gSettingsAdaCompiler);
		
// VAD HETER DETTA I GPC?
		if not buildApp then
			commandLine := commandLine + ' -c ';

(*
FINAL/DEBUG NOT YET IMPLEMENTED
		// Final build or debug build?
		if buildFinal then
			commandLine := commandLine + ' ' + gSettingsReleaseOptions
		else
			commandLine := commandLine + ' ' + gSettingsDebugOptions;
*)
(*
		if buildFinal then
			commandLine := commandLine + ' ' + '-O'
		else
			commandLine := commandLine + ' ' + '-g';
*)
	
		// Add subfolder
//		if target = kPPCTarget then
//			commandLine := commandLine + ' -FU' + './' + kObjectFolderName + '/PPC'
//		else
//			commandLine := commandLine + ' -FU' + './' + kObjectFolderName + '/386';
// Irrelevant fšr GPC? Eller?
		
(*
MULTIPLE TARGETS NOT IMPLEMENTED
		// If we are building universal, build with target extension
		if targetExtension then
		begin
			// Get name without extension? Or just standard name for temporary binary
			// Append _ppc or _386
			if target = kPPCTarget then
				commandLine := commandLine + ' -o' + kObjectFolderName + '/LWP_U_TEMP_BINARY_PPC '
//				commandLine := commandLine + ' -o' + kObjectFolderName+'/PPC' + '/LWP_U_TEMP_BINARY_PPC '
			else
				commandLine := commandLine + ' -o' + kObjectFolderName + '/LWP_U_TEMP_BINARY_386 ';
//				commandLine := commandLine + ' -o' + kObjectFolderName+'/386' + '/LWP_U_TEMP_BINARY_386 ';
		end;
*)
	
(*
		// Add frameworks
		// -k"-framework Carbon -prebind"
		// When is prebind unnecessary?
		BuildListFromText(gSettingsFrameworks, pathList);
		AppendCustomPaths('.frameworks', pathList, theSpec);
		if Length(pathList) = 0 then
		begin
		// Add Carbon anyway?
			commandLine := commandLine + ' -Wl,' + gSettingsLinkOptions; {gSettingsLinkOptions new i 0.3.2}
		end
		else
		begin
			commandLine := commandLine + ' -Wl,' + gSettingsLinkOptions; {gSettingsLinkOptions new i 0.3.2}
			for i := 0 to Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + ',-framework,' + pathList[i];
			end;
		end;
* )
		
		// Other linking (untested)
		SetLength(pathList, 0);
		AppendCustomPaths('.link', pathList, theSpec);
		if Length(pathList) > 0 then
		begin
			for i := 0 to High(pathList) do // Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + pathList[i] + ' ';
			end;
		end;
// prebind only 10.3 or older which is not supported anyway!
//			commandLine := commandLine + ' -prebind"';
		commandLine := commandLine + ' "';
		
		// Additional paths from gSettingsPaths
		BuildListFromText(gSettingsPaths, pathList);
		AppendCustomPaths('.paths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -aI' + pathList[i];
		end;
		
		// Additional library paths from gSettingsLibPaths
		BuildListFromText(gSettingsLibPaths, pathList);
		AppendCustomPaths('.libPaths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -aO' + pathList[i];
		end;
		
HelpAppendLn('*** Command: '+commandLine);

		// Create object code subfolder
		//cleanName := TrimExtension(theSpec.name);
		folderSpec := theSpec;
		folderSpec.name := ':' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
		err := FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
		if target = kPPCTarget then
			folderSpec.name := ':' + kObjectFolderName + ':PPC'
		else
			folderSpec.name := ':' + kObjectFolderName + ':386';
		err := FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
		
		return commandLine;
end; {BuildGnuPascalCommandline}
*)

// Since gnatmake doesn't work correctly when called directly from
// ProcessUtils, I create a shellscript and call that instead.
// This is performed by SaveCommandToFile.
procedure SaveCommandToFile(Line, fileName: AnsiString);
var
	FD : Cint;
begin
WriteLn('Writing ', fileName);
	FD:=fpOpen (fileName, O_WrOnly or O_Creat or O_Trunc);
	if FD>0 then
	begin
		if length(Line)<>fpwrite (FD,Line[1],Length(Line)) then
			Writeln ('Error when writing to file !');
		fpClose(FD);
	end
	else
	WriteLn('Could not create ', fileName);
end;

function CompileAda(theSpec: FSSpecString; buildExecutable, buildFinal: Boolean;
				compileCallback: CompilationDoneProc): OSErr;
	var
		cleanName: Str255;
		path, commandLine, pathOnly, shCommandLine: AnsiString;
//		err: OSErr;
		i: Longint;
		pathList: AnsiStringArray;
		//fileList: AnsiString;
//		extType: Longint;
		folderSpec: FSSpecString;
		appDirId: Longint;
const
	CR = Char(13);
begin
		if gVersionAda = kProcessUtilsFailedString then
			if not QuestionAlert('Ada does not seem to be installed', 'Try compiling anyway?') then
				return -1;
		
		CompileAda := noErr;
		
		// Remember what the main file was. (Used for searching errors from the error window!)
		gLastMainFile := theSpec;
		
		HelpMainMessage('Compiling ' + GetLastToken(theSpec)); // Show status
		
		// Create a full pathname to the file as AnsiString
		path := theSpec;
		pathOnly := path; // Needed for chdir
		pathOnly := TrimLastToken(pathOnly); // We need the path without filename.
		chdir(MacRomanToUTF8(pathOnly)); // Set that as current dir
		
HelpAppendLn('Want to compile ' + GetLastToken(theSpec));
		
//		fileList := BuildCFileList(theSpec);
//		WriteLn('************* BuildCFileList', fileList);
		
		commandLine := gSettings.AdaCompiler + ' ' + GetLastToken(theSpec);
		
		// Options. Override if .options file exists
		SetLength(pathList, 0);
		AppendCustomPaths('.options', pathList, theSpec);
		if Length(pathList) = 0 then
		begin
		// Default options (according to settings).
			commandLine := commandLine + ' ' + gSettings.AdaOptions; // LOCAL
		end
		else
		begin
			for i := 0 to High(pathList) do // Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + ' ' + pathList[i] + ' ';
			end;
		end;
		
		// ADD FINAL OR DEBUG OPTIONS HERE! DEPENDS ON buildFinal FLAG
(*
		if buildFinal then
			commandLine := commandLine + ' ' + gSettings.ReleaseOptions
		else
			commandLine := commandLine + ' ' + gSettings.DebugOptions;
*)

// Better?
// function BuildGnuPascalCommandline(theSpec: FSSpecString; buildApp, buildFinal: Boolean; target: Longint; targetExtension: Boolean): AnsiString;
		
		// Add subfolder
		// €r -o rŠtt vŠg att visa var den skall lŠgga det, eller finns nŒt i stil med -FU?
		cleanName := TrimExtension(GetLastToken(theSpec));
//		commandLine := commandLine + ' -o ' + './' + kObjectFolderName + '/' + cleanName + '.o -c'; // -c gšr att man fŒr objektkod

// subfolder for object code (and libraries?)
//		commandLine := commandLine + ' -aO=' + './' + kObjectFolderName;
// no, like this: subfolder for object code
		commandLine := commandLine + ' -D ' + './' + kObjectFolderName;
		
		if not buildExecutable then
			commandLine := commandLine + ' -c'; // Only compile to .o file (in same folder). Do not link.
		if buildExecutable then
			commandLine := commandLine + ' -o ' + cleanName; // Normal compile and link
		//commandLine := commandLine + ' -FU' + './' + kObjectFolderName;
		
{		
		// Architectures is easy with GCC!
		if gFlags^^.target = kUniversalTarget then
			commandLine := commandLine + ' -arch ppc -arch i386 ';
		if gFlags^^.target = kPPCTarget then
			commandLine := commandLine + ' -arch ppc ';
		if gFlags^^.target = k386Target then
			commandLine := commandLine + ' -arch i386 ';
}		

// Is this correct?
// Change AFTER 0.6
(*
		if gFlags^^.target = kUniversalTarget then
			commandLine := commandLine + ' --GCC=-arch --GCC=i386 --GCC=-arch --GCC=ppc ';
		if gFlags^^.target = kPPCTarget then
			commandLine := commandLine + ' --GCC=-arch --GCC=ppc ';
		if gFlags^^.target = k386Target then
			commandLine := commandLine + ' --GCC=-arch --GCC=i386 ';
*)	

// This gives a chance to add some extra
// although that can already be done with .options
// How does link options work in Ada?
		// Link options, for libraries etc
		SetLength(pathList, 0);
		AppendCustomPaths('.link', pathList, theSpec);
		if Length(pathList) > 0 then
		begin
			for i := 0 to High(pathList) do // Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + pathList[i] + ' ';
			end;
		end;


//This is just plain wrong. gSettings.LinkOptions is for FPC!
//But frameworks should be added!
		// Add frameworks
		BuildListFromText(gSettings.Frameworks, pathList);
		AppendCustomPaths('.frameworks', pathList, theSpec);
(*
		if Length(pathList) = 0 then
		begin
		// Add Carbon anyway?
//			commandLine := commandLine + ' -Wl';
			if Length(gSettings.LinkOptions) > 0 then
				commandLine := commandLine + ' ' + gSettings.LinkOptions; {gSettings.LinkOptions new i 0.3.2}
		end
		else
		begin
//			commandLine := commandLine + ' -Wl';
			if Length(gSettings.LinkOptions) > 0 then
				commandLine := commandLine + ' ' + gSettings.LinkOptions; {gSettings.LinkOptions new i 0.3.2}
*)
// DOES NOT WORK YET - WHY???
// Maybe now?
		if Length(pathList) > 0 then
		begin
			commandLine := commandLine + ' -largs';
			for i := 0 to Length(pathList)-1 do
			begin
				if Length(pathList[i]) > 0 then
					commandLine := commandLine + ' "-framework ' + pathList[i] + '"';
				if i <> Length(pathList)-1 then // if not last
					commandLine := commandLine + ',';
			end;
		end;
// Answer: gnatmake takes frameworks after -largs, link arguments! Or that's wat I think.
// -largs "-framework Carbon","-framework Cocoa" ... ungefŠr?


		// Additional paths from gSettings.Paths
		BuildListFromText(gSettings.Paths, pathList);
		AppendCustomPaths('.paths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -aI=' + pathList[i];
		end;
		
		// Additional library paths from gSettings.LibPaths
		BuildListFromText(gSettings.LibPaths, pathList);
		AppendCustomPaths('.libPaths', pathList, theSpec);
		for i := 0 to Length(pathList)-1 do
		begin
			if Length(pathList[i]) > 0 then
				commandLine := commandLine + ' -aP' + pathList[i];
		end;
		
		HelpAppendLn('*** Command: '+commandLine);
		
		commandLine := MacRomanToUTF8(commandLine); // Entire command line to UTF-8! Or is it better in separate parts?
		
		// Create object code subfolder
		//cleanName := TrimExtension(theSpec.name);
		folderSpec := TrimLastToken(theSpec) + '/' + kObjectFolderName;
//		folderSpec.name := ':' + kObjectFolderName; // ':intermediate-files';		// This folder name should be configurable
//		{err :=} FSpDirCreate(folderSpec, 0, appDirId);	// Ignore error.
		CreateFolder(folderSpec); // FpMkdir(folderSpec, $777); // Mode???
		
//		SaveCommandToFile(commandLine, folderSpec + '/lwp-ada-build.sh');
//		shCommandLine := '/bin/sh "' + folderSpec + '/lwp-ada-build.sh"';
		SaveCommandToFile(commandLine, kObjectFolderName + '/lwp-ada-build.sh');
		shCommandLine := '/bin/sh "' + kObjectFolderName + '/lwp-ada-build.sh"';
		
		LaunchCompilation(shCommandLine, theSpec, buildExecutable, buildFinal, compileCallback, @CompileCData, @CompilationDone);
		SelectWindow(helpWind);
end;

end.

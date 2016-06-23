{$mode macpas}
unit LWPGlobals;
interface
	uses
		MacOSAll, UtilsTypes, ProcessUtils, Halda, HaldaTypes, HashTableUnit, ClassParser;
	
	const
		gVersionLWP = '0.9.9p1';
		
		// Default path to iOS SDKs
		kiOSSDKPath = '/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS';

//type
//	FileArr = array of FSSpecString; // Should be FSRef in the future.
//	AnsiStringArray = array of AnsiString;

{ Resource Numbers	}
{Most of these are no longer used and will be removed}
	const
		helpWindRes = 1000;
//		editWindRes = 1001;
//		zoomWindRes = 1002;
//		rgnWindRes = 1003;
		helpTextRes = 1000;
		
		kPathMenuID = 2000;
		
		kMaxEditWindows = 40;
		kMaxFunctionMenuItems = 255;
		
	var
		editMenu: MenuHandle;
		runMenu: MenuHandle;
		searchMenu: MenuHandle;
		windowMenu: MenuHandle;
		debugMenu: MenuHandle;
		
		helpWind: WindowPtr;
		teHelp: HaldaViewPtr; //TXNObject;		{ handle to help window TextEdit record }
		theErrorMsgWind: WindowPtr;
		aboutWind: WindowPtr;
		
		gLastMainFile: FSSpecString; // Last main file in a build
		gMainFrontIndex: Integer; // Holds the index of the main file after GetFrontMainWindow has been called.
								// Is NOT kept in synch over time.

		// Many of the following are one per window. It woud be better to group them
		// by window instead of as separate arrays.
		
//		editDialog: array [1..kMaxEditWindows] of DialogPtr;
		editWind: array [1..kMaxEditWindows] of WindowPtr;
		editWindIsMain: array [1..kMaxEditWindows] of Boolean;
		editMainWindIsProgram: array [1..kMaxEditWindows] of Boolean; // To tell a library from a program
		editWindIsCocoaProgram: array [1..kMaxEditWindows] of Boolean; // 0.8.11 - Cocoa in Pascal program, program with CocoaAll
		editWindIsiOSProgram: array [1..kMaxEditWindows] of Boolean; // 0.8.11 - FPC iPhone, with iPhoneAll
		editWindIsJVMProgram: array [1..kMaxEditWindows] of Boolean; // 0.9.9 JVM
		editWindIsAndroidProgram: array [1..kMaxEditWindows] of Boolean; // 0.9.9 JVM
		teEdit: array [1..kMaxEditWindows] of HaldaPtr;		{ handle to text window TextEdit record }
		dummy: Boolean;
		
		// Function menu, created by ColorCoder
//		editFunctionMenu: array[1..kMaxEditWindows] of MenuHandle;
//		editFunctionNameStart: array[1..kMaxEditWindows, 1..kMaxFunctionMenuItems] of Longint;
//		editFunctionNameEnd: array[1..kMaxEditWindows, 1..kMaxFunctionMenuItems] of Longint;
		editFunctionNameStart: array[1..kMaxEditWindows] of array of Longint;
		editFunctionNameEnd: array[1..kMaxEditWindows] of array of Longint;
		editFunctionName: array[1..kMaxEditWindows] of array of AnsiString;
		editGlobalFunctions: array[1..kMaxEditWindows] of array of AnsiString;
		
		// More data from ColorCoder, for uses menu and auto-framework
		usesList: array[1..kMaxEditWindows] of AnsiStringArray;
		frameworkList: array[1..kMaxEditWindows] of AnsiStringArray;
		extraOptions: array[1..kMaxEditWindows] of AnsiString;
		extraLinkOptions: array[1..kMaxEditWindows] of AnsiString;
		
		//For code completion
		type
		RecordVariables = record
			recordName: AnsiString;
			recordMembers: Array of AnsiString;
			recordScope: AnsiString; // ScopeType, or function name? It can be a function name!
		end;
		
		VariableMembers = record
			variableName: Array of AnsiString;
			variableScope: Array of AnsiString;
			variableType: Array of AnsiString;	
		end;
		//RecordPointers (record) has three variables,
		//1. pointerVariableName stores the pointer name 
		//2. pointerToPointer (boolean): stores whether it is a pointerToPointer or not.
		//3. pointerVariableType stores the pointer type.
		RecordPointers = record
			pointerVariableName: AnsiString;
			pointerVariableType: AnsiString;
			pointerToPointer: Boolean;
		end;
		
		//This is used for the closed files, which collects the filename, recordVariables (class, object, objclass), variables, recordpointers.
		
		FileVariableList = record
			fileName:AnsiString;
			fileRecordVariables: Array of RecordVariables; 
			fileVariableMembers: VariableMembers;
			fileRecordPointers: array of RecordPointers;
			fileClassNamesHashTable: HashRec;
		end;

		var
			//edit is for open windows (can be unit or program window)
   			editRecordVariables: array[1..kMaxEditWindows] of Array of RecordVariables; //2D array
   			editVariableMembers: array[1..kMaxEditWindows] of VariableMembers;//1 array of [1..kMax]
   			editRecordPointers: array[1..kMaxEditWindows] of array of RecordPointers;
   			
   			//g is for closed files of main program wiondow, no more used!
   			gRecordVariables: array[1..kMaxEditWindows] of Array of RecordVariables; //2D array
   			gVariableMembers: array[1..kMaxEditWindows] of VariableMembers;//1 array of [1..kMax]
   			gRecordPointers: array[1..kMaxEditWindows] of array of RecordPointers;   
   			
   			//It is used for updated include file names.
   			gProjectFiles: array[1..kMaxEditWindows] of array of AnsiString;
   			
   			//For closed files. Every main program has the variables data of all its includes with the file names
   			gClosedFilesVariablesData: array[1..kMaxEditWindows] of Array of FileVariableList; 
     			
			//These are in globals because of GetToken (which is in ColorCoding) is used from JumpToFunctions and these variables 
			//used in GetToken and Jumptofunctions.		
			secondPrevTokenType, prevTokenType, prevTokenStart, prevTokenEnd, secondPrevTokenStart, secondPrevTokenEnd:LongInt;
			secondPrevTokenValue, prevTokenValue: AnsiString;
			
			//For C code completion
			classNamesHashTable: array[1..kMaxEditWindows] of HashRec;
			
			//For inheritance
			inheritedObjectName:AnsiString;
			inheritedList: AnsiStringArray;
			
		var
			editClassList: array[1..kMaxEditWindows] of ClassDataList;
			closedFilesClassList: array[1..kMaxEditWindows] of ClassDataList;
			editMethods: array[1..kMaxEditWindows] of array of MethodRec;
			closedFilesMethods: array[1..kMaxEditWindows] of array of MethodRec;


{Global pointer for current process}
		gCurrentProcess: ProcessPtr;

// Version strings, for showing in the about box.
// Need to be updated when the settings change!
		gVersion386, gVersionPPC, gVersionGCC, gVersionJava: AnsiString;
		gVersionGPC, gVersionAda, gVersionCuda: AnsiString;
		// 0.8.11:
		gVersionObjP386, gVersionObjPPPC, gVersionSimulator, gVersioniOS, giOSSDK: AnsiString;
		// 0.9.8p?
		gVersionGDB: AnsiString;


		// It is nice to have the system version globally
//		gSystemVersion: record
//			major, minor, bugfix: Integer;
//		end;
		
	const
		kObjectFolderName = 'intermediate-files';		// This folder name could be configurable
		
implementation
end.
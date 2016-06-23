// This unit was created to isolate QDCG-code from LWPEdit in order
// to avoid conflicts in type names and function calls.

unit EditQDCGParts;
interface
uses
	MacOSAll, Halda, HaldaTypes, HaldaSysDept, QDCG;

procedure UpdateHaldaView(theView: HaldaViewPtr);

implementation

// Note: Sets port! Preserve old port?
procedure UpdateHaldaView(theView: HaldaViewPtr);
begin
//	WriteLn('UpdateHaldaView');
	CreatePortQDPort(nil);
	HDraw(theView);
	FinishPort;
//	WriteLn('UpdateHaldaView finished');
end;


end.

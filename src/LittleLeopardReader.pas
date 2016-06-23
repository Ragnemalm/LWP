// Lightweight Pascal IDE, process management helping unit
// © Ingemar Ragnemalm 2008
// Padding pipe for Leopard, sits and polls the input in order to get all data.
// Weeks of work and this is all that came out... but I guess I know a bit more
// about processes now.
// Verison 1.0 (0.3.3): First verison
// Verison 1.1 (0.3.4): Better termination check

{$mode macpas}
unit LittleLeopardReader;
interface
uses
	MacOSAll, cprocintf;

function StartReader(inChannel: Longint): Longint;

implementation

type DuplexRec = record
		src, dest: Longint;
	end;
	DuplexPtr = ^DuplexRec;

function Reader(arg: Pointer): Pointer;
const
	kReaderBufSize = 1024;
var
	buf: array[0..kReaderBufSize] of Char;
	done: Boolean = false;
	r: DuplexPtr;
	availableData: Longint;
begin
	r := DuplexPtr(arg);
	while not done do
	begin
		availableData := kReaderBufSize;
		availableData := read(r^.src, @buf[0], availableData);
		if availableData > 0 then
		begin
			write(r^.dest, @buf[0], availableData);
			// fflush? Onšdigt? Omšjligt?
		end;
		if availableData = 0 then done := true;
		if errNo()^ = EIO then done := true; // Added for 1.1
		if not done then usleep(100); // Reduce CPU load!
	end;
	// close pipe?
	close(r^.src);
	close(r^.dest);
	return nil;
end;

function StartReader(inChannel: Longint): Longint;
var
	r: DuplexPtr;
	id, err: Longint;
	p: TFilDes;
begin
	pipe (p);

	r := DuplexPtr(NewPtrClear(SizeOf(DuplexRec)));
	r^.src := inChannel;
	r^.dest := p[1];
	err := pthread_create(id, nil, Reader, Pointer(r));
	if err <> 0 then
		WriteLn('Problems in StartReader');
	return p[0];
end;

end.

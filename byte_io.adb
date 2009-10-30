with System;		use System;

with Win32;
with Win32.Winbase;
with Win32.Winnt;

with Conversions;	use Conversions;

package body Byte_io is

   procedure Write( f : file_type; b : byte ) is
      res_bool : Win32.BOOL;
      written : Integer;
   begin
      res_bool := Win32.Winbase.WriteFile (
                       hFile                  => f,
                       lpBuffer               => b'address,
                       nNumberOfBytesToWrite  => 1,
                       lpNumberOfBytesWritten => TO_LPDWORD( written'address ),
                       lpOverlapped           => null );
      if res_bool = 0 then
         raise file_error;
      end if;
   end Write;

   procedure Read( f : file_type; b : out byte ) is
      res_bool : Win32.BOOL;
      read : Integer;
   begin
      res_bool := Win32.Winbase.ReadFile(
                      hFile                => f,
                      lpBuffer             => b'address,
                      nNumberOfBytesToRead => 1,
                      lpNumberOfBytesRead  => TO_LPDWORD( read'address ),
                      lpOverlapped         => null );
      if res_bool = 0 then
         raise file_error;
      end if;
   end Read;

   procedure Open( f : in out file_type; name : string ) is
      buffer : string := name & ascii.nul;
   begin
      f := Win32.Winbase.CreateFile(
                        lpFileName            => TO_LPCSTR(buffer'address),
                        dwDesiredAccess       => Win32.Winnt.FILE_GENERIC_READ,
                        dwShareMode           => Win32.Winnt.FILE_SHARE_READ,
                        lpSecurityAttributes  => null,
                        dwCreationDisposition => Win32.Winbase.OPEN_EXISTING,
                        dwFlagsAndAttributes  => Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
                        hTemplateFile         => System.Null_address );
      -- test résultat
      if f = Win32.Winbase.INVALID_HANDLE_VALUE then
         raise file_not_found;
      end if;
   end Open;

   procedure Create( f : in out file_type; name : string ) is
      buffer : string := name & ascii.nul;
   begin
      f := Win32.Winbase.CreateFile(
                        lpFileName            => TO_LPCSTR(buffer'address),
                        dwDesiredAccess       => Win32.Winnt.FILE_GENERIC_WRITE,
                        dwShareMode           => 0,
                        lpSecurityAttributes  => null,
                        dwCreationDisposition => Win32.Winbase.CREATE_ALWAYS,
                        dwFlagsAndAttributes  => Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
                        hTemplateFile         => System.Null_address );
      -- test résultat
      if f = Win32.Winbase.INVALID_HANDLE_VALUE then
         raise file_not_found;
      end if;
   end Create;


   procedure Close( f : file_type ) is
      res_bool : Win32.BOOL;
   begin
      res_bool := Win32.Winbase.CloseHandle( f );
   end Close;



end Byte_io;

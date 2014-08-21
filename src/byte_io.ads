with Win32;		use Win32;
with Win32.Winnt;

package Byte_io is

   subtype file_type is Win32.Winnt.HANDLE;

   file_not_found : exception;
   file_error     : exception;

   procedure Write( f : file_type; b : byte );

   procedure Read( f : file_type; b : out byte );

   procedure Open( f : in out file_type; name : string );

   procedure Create( f : in out file_type; name : string );

   procedure Close( f : file_type );


end Byte_io;

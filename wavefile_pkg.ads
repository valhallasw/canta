with Win32.Winnt;
with Common_types;

package Wavefile_pkg is

   procedure Write_header( f : Win32.Winnt.HANDLE; data_length : integer; Fs : integer );

   procedure Save_wave( name   : string;
                        data   : Common_types.byte_ptr;
                        length : integer;
                        Fs     : integer );

end Wavefile_pkg;

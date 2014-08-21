with Unchecked_conversion;

with Interfaces.C;	use Interfaces.C;

with Win32;		use Win32;

with Common_types;
with Byte_io;		use Byte_io;
with Config_pkg;

package body Wavefile_pkg is

   -- codes utilisés pour les fichiers Wave
   File_4cc    : constant string := "RIFF";
   File_format : constant string := "WAVEfmt ";
   File_data   : constant string := "data";

   -- ***************************************************************************
   function To_Byte is new unchecked_conversion( source => character, target => byte );

   procedure write_word( f : file_type; w : word ) is
      b1, b2 : byte;
   begin
      b1 := byte( w mod 256 );
      b2 := byte( w / 256 );
      byte_io.write( f,  b1 );	-- LSB first
      byte_io.write( f, b2 );	-- MSB last
   end write_word;

   procedure write_int( f : file_type; I : integer ) is
      b1, b2, b3, b4 : byte;
      tmp : integer := I;
   begin
      b1 := byte(tmp mod 256);	-- LSB
      tmp := tmp / 256;
      b2 := byte(tmp mod 256);
      tmp := tmp / 256;
      b3 := byte(tmp mod 256);
      tmp := tmp / 256;
      b4 := byte(tmp mod 256);	-- MSB
      byte_io.write( f, b1 );	-- LSB first
      byte_io.write( f, b2 );
      byte_io.write( f, b3 );
      byte_io.write( f, b4 );	-- MSB last
   end write_int;


   -- ***************************************************************************

   procedure Write_header( f : Win32.Winnt.HANDLE; data_length : integer; Fs : integer ) is
   begin
      -- 4CC
      for i in File_4cc'range loop
         byte_io.Write( f, To_byte(file_4cc(i)) );
      end loop;
      -- longueur fichier
      write_int( f, data_length + 36 );
      -- format
      for i in File_format'range loop
         byte_io.Write( f, To_byte(file_format(i)) );
      end loop;
      -- taille du WaveFormatEx
      write_int( f, 16 );
      -- WaveFormatEx (win32-mmsystem.ads:1542)
      write_word( f, 1 );	-- format brut, wFormatTag : Win32.WORD;
      write_word( f, 1 );	-- mono, nChannels : Win32.WORD;
      write_int( f, Fs );	-- fréquence d'échantillonage, nSamplesPerSec : Win32.DWORD;
      write_int( f, 2*Fs );	-- 16 bits = 2 bytes, nAvgBytesPerSec : Win32.DWORD;
      write_word( f, 2 );	-- wBitsPerSample * nChannels / 8, nBlockAlign : Win32.WORD;
      write_word( f, 16 );	-- 16 bits, wBitsPerSample : Win32.WORD;
      -- chunk data
      for i in File_data'range loop
         byte_io.Write( f, To_byte(File_data(i)) );
      end loop;
      -- longueur des data
      write_int( f, data_length );
      -- terminé
   end Write_header;


   procedure Save_wave( name   : string;
                        data   : Common_types.byte_ptr;
                        length : integer;
                        Fs     : integer ) is
      f : byte_io.file_type;
   begin
       -- creation du fichier
       byte_io.Create( f, name );
       -- creation du header
       write_header( f, length, Fs );
       -- écriture des données
       for i in data'first..data'first+length-1 loop
          byte_io.write( f, data(i) );
       end loop;
       -- fermeture du fichier
       byte_io.Close( f );
   exception
      when others =>
          byte_io.close( f );
          raise;
   end Save_wave;


end Wavefile_pkg;

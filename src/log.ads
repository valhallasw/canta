with System;
with Win32;
with Win32.Mmsystem;

package LOG is

   procedure START_LOG;
   procedure Store( S : String );
   procedure Store_C( CS : Win32.CHAR_Array );
   procedure Store( F : Float );
   procedure Store( L : Long_Float );
   procedure STORE( I : Integer );
   procedure STORE_S( SI : Short_Integer );
   procedure STORE_D( F : Float );
   procedure STORE_TIME;
   procedure STORE( t : Duration );
   procedure Space;

   procedure Separ_line;

   procedure Error( text : string );
   procedure Midiout_error( Result  : Win32.Mmsystem.MMRESULT );

   procedure Store_Message( msg : Win32.UINT );
   procedure End_line;
   procedure END_LOG;


end LOG;

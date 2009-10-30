with System;
with Interfaces.C;	use INterfaces.C;
with Win32.Winuser;
with Win32.Mmsystem;	use Win32.Mmsystem;
with Win32.Winbase;
with Conversions;	use Conversions;
with Intl;
with Common_types;
with Log;

package body utils_pkg is


   procedure Message_box( titre : string; texte : string ) is
      msg : constant string := texte & ascii.nul;
      title : constant string := titre & ascii.nul;
      res_int : Win32.INT;
   begin
      res_int := Win32.Winuser.MessageBox( Common_types.Win_hwnd,
                       TO_LPCSTR( msg'address ),
                       TO_LPCSTR( title'address),
                       Win32.Winuser.MB_OK 			-- bouton OK
                       or Win32.Winuser.MB_ICONINFORMATION 	-- icone = information
                       or Win32.Winuser.MB_APPLMODAL );		-- modal, cliquer sur OK pour continuer
   end Message_box;


   procedure Error_box( titre : string; texte : string ) is
      err_msg : constant string := texte & ascii.nul;
      err_title : constant string := titre & ascii.nul;
      res_int : Win32.INT;
   begin
      res_int := Win32.Winuser.MessageBox( Common_types.Win_hwnd,
                       TO_LPCSTR( err_msg'address ),
                       TO_LPCSTR( err_title'address),
                       Win32.Winuser.MB_OK or Win32.Winuser.MB_ICONEXCLAMATION );
      Log.Error( titre & Intl.new_line & texte );
   end Error_box;


   procedure Check_erreur( Result   : Win32.Mmsystem.MMRESULT; caller : string ) is
      res_mm : Win32.Mmsystem.MMRESULT;
      Err_text : String(1..Win32.Mmsystem.MAXERRORLENGTH) := (others => ASCII.NUL);
   begin
      if Result /= Win32.Mmsystem.MMSYSERR_NOERROR then
         -- lecture du message d'erreur correspondant
         Res_mm := Win32.Mmsystem.waveInGetErrorText( Result,
					 TO_LPSTR(Err_text(1)'address),
                                         Win32.UINT(Err_text'length) );
         -- affiche pour l'utilisateur
         Error_box( Intl.def_err_title,  caller & Intl.new_line & Err_text );
         -- leve une exception pour sortir du programme
         raise Common_types.fatal_error;
      end if;
   end Check_erreur;

   -- ==============================================================================

   procedure Log_Windows_error( text : string := "" ) is
      err_code : Win32.DWORD;
      len : Win32.DWORD;
      buffer : string(1..256);
   begin
      -- lecture du code de la dernière erreur Windows
      err_code := Win32.Winbase.GetLastError;
      -- récupération du texte de l'erreur
      Len := Win32.Winbase.FormatMessage(
                           dwFlags      => Win32.Winbase.FORMAT_MESSAGE_FROM_SYSTEM
                                           + Win32.Winbase.FORMAT_MESSAGE_IGNORE_INSERTS,
                           lpSource     => Common_types.N_A,
                           dwMessageId  => err_code,
                           dwLanguageId => 0,
                           lpBuffer     => TO_LPSTR(buffer'address),
                           nSize        => Win32.DWORD(buffer'length) );
      declare
         -- création du message complet
         msg : constant string := "Windows error: code=" & Win32.DWORD'image(err_code) & " in " & text
                 & Intl.new_line & To_Ada(buffer);
      begin
         -- stockage dans le log
         Log.Error(  msg );
      end;
   end Log_Windows_error;



   -- lève l'exception Fatal_error après un message pour l'utilisateur
   procedure Raise_fatal_error( Id : obj_id_type; text : string := "" ) is
      err_code : Win32.DWORD;
      len : Win32.DWORD;
      buffer : string(1..256);
   begin
      -- lecture du code de la dernière erreur Windows
      err_code := Win32.Winbase.GetLastError;
      -- récupération du texte de l'erreur
      Len := Win32.Winbase.FormatMessage(
                           dwFlags      => Win32.Winbase.FORMAT_MESSAGE_FROM_SYSTEM
                                           + Win32.Winbase.FORMAT_MESSAGE_IGNORE_INSERTS,
                           lpSource     => Common_types.N_A,
                           dwMessageId  => err_code,
                           dwLanguageId => 0,
                           lpBuffer     => TO_LPSTR(buffer'address),
                           nSize        => Win32.DWORD(buffer'length) );
      declare
         -- création du message complet
         msg : constant string := Intl.err_obj_txt & obj_Id_type'image(Id)
                 & Intl.new_line & "code" & Win32.DWORD'image(err_code) & " in " & text
                 & Intl.new_line & To_Ada(buffer);
      begin
         -- affichage à l'écran et stockage dans le log
         Error_box( Intl.err_init_txt, msg );
      end;
      -- sortie du programme
      raise Common_types.fatal_error;
   end Raise_fatal_error;



end utils_pkg;

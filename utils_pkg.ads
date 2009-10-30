with Win32.Mmsystem;
with Resources_pkg;	use Resources_pkg;

package utils_pkg is

   -- message d'erreur � l'�cran + dans le Log
   procedure Error_box( titre : string; texte : string );

   -- message utilisateur � l'�cran
   procedure Message_box( titre : string; texte : string );


   procedure Check_erreur( Result : Win32.Mmsystem.MMRESULT; caller : string );

   -- lecture de l'erreur Windows par GetLastError, r�cup�ration du message
   -- affichage � l'�cran et dans le log et leve fatal_error -> sortie du programme
   procedure Raise_fatal_error( Id : obj_id_type; text : string := "" );

   -- lecture de l'erreur Windows par GetLastError, r�cup�ration du message
   -- l'�crit dans le log et continue
   procedure Log_Windows_error( text : string := "" );

end utils_pkg;

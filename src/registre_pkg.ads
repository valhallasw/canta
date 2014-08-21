package Registre_pkg is

   -- exception li� � la base de registre
   Reg_error : exception;

   -- configuration microphone et device MIDI
   function Read_config return boolean;
   procedure Store_config;

   -- Dernier r�pertoire fichiers MIDI
   function Get_last_midi_dir return string;
   procedure Set_last_midi_dir( dir : string );

   -- dernier r�pertoire de sauvegarde wav
   function Get_last_save_dir return string;
   procedure Set_last_save_dir( dir : string );

   -- date derniere d�marrage de demo
   function Get_last_start return string;
   procedure Set_last_start( time : string );
   -- date derni�re fin de d�mo
   function Get_last_duree return string;
   procedure Set_last_duree( duree : string );

   -- skin de l'interface
   function Get_current_skin return string;
   procedure Set_current_skin( skin : string );

   -- r�pertoire d'installation
   function Get_exe_dir return string;

end Registre_pkg;

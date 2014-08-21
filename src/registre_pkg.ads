package Registre_pkg is

   -- exception lié à la base de registre
   Reg_error : exception;

   -- configuration microphone et device MIDI
   function Read_config return boolean;
   procedure Store_config;

   -- Dernier répertoire fichiers MIDI
   function Get_last_midi_dir return string;
   procedure Set_last_midi_dir( dir : string );

   -- dernier répertoire de sauvegarde wav
   function Get_last_save_dir return string;
   procedure Set_last_save_dir( dir : string );

   -- date derniere démarrage de demo
   function Get_last_start return string;
   procedure Set_last_start( time : string );
   -- date dernière fin de démo
   function Get_last_duree return string;
   procedure Set_last_duree( duree : string );

   -- skin de l'interface
   function Get_current_skin return string;
   procedure Set_current_skin( skin : string );

   -- répertoire d'installation
   function Get_exe_dir return string;

end Registre_pkg;

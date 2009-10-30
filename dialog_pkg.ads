with Win32;
with Win32.Windef;

with Common_types;
with midifile_pkg;

package Dialog_pkg is

   -- nom du skin � charger
   new_skin : Common_types.string_pt;

   -- s�lection du micro, de la fr�quence et du device MIDI
   procedure Select_All;

   -- ouverture d'un fichier MIDI ("" si annul�)
   function Select_midi_file return string;

   -- s�lection fichier sauvegarde
   function Select_Save_file return string;

   -- s�lection de la piste m�lodie (-1 si annul�)
   function Select_track( root : midifile_pkg.midi_data_ptr ) return integer;

   -- saisie de la cl� d'autorisation correspondant � la signature hard
   procedure Affiche_cle_autorisation( sign : string );

   -- affichage et saisie des options
   function Options return boolean;

   -- propose de lancer Sndvol32.exe
   procedure Launch_sndvol;

end Dialog_pkg;

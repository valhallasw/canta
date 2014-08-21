with Win32;
with Win32.Windef;

with Common_types;
with midifile_pkg;

package Dialog_pkg is

   -- nom du skin à charger
   new_skin : Common_types.string_pt;

   -- sélection du micro, de la fréquence et du device MIDI
   procedure Select_All;

   -- ouverture d'un fichier MIDI ("" si annulé)
   function Select_midi_file return string;

   -- sélection fichier sauvegarde
   function Select_Save_file return string;

   -- sélection de la piste mélodie (-1 si annulé)
   function Select_track( root : midifile_pkg.midi_data_ptr ) return integer;

   -- saisie de la clé d'autorisation correspondant à la signature hard
   procedure Affiche_cle_autorisation( sign : string );

   -- affichage et saisie des options
   function Options return boolean;

   -- propose de lancer Sndvol32.exe
   procedure Launch_sndvol;

end Dialog_pkg;

with Common_types;
with Data_pkg;

package Midi_pkg is


   type player_status_type is ( stopped, playing, paused );

   -- pour centrage auto
   max_melody : integer := Common_types.max_midi;
   min_melody : integer := Common_types.min_midi;

   -- *******************************************************************

   -- Interface 'utilisateur' du Player MIDI

   -- charge un fichier MIDI et le pr�pare pour la lecture
   -- return true si tout est OK, false autrement
   -- initialise les pointeurs de lecture au d�but du fichier
   function Load_file return boolean;

   -- nom du fichier charg� (juste la fin, pas la path)
   function File_name return string;

   -- joue le fichier midi pr�alablement charg� � partir de la position courante
   -- si track num /= 0 , ne joue que la piste demand�e
   procedure Play( track_num : integer );

   -- arr�te la lecture du fichier midi et remet les pointeurs au d�but
   procedure Stop;

   -- entre ou sort deu mode pause
   procedure Toggle_Pause;

   -- Dit si le player est en train de jouer quelque chose
   function Is_Playing return Boolean;

   function Get_player_status return player_status_type;

   -- transpose le fichier en cours de lecture
   procedure Set_Transpo( valeur : integer );

   -- change l'instrument de la m�lodie
   procedure Change_instrument;

   -- arr�te toutes les notes de tous les channels
   procedure All_off;

   -- -------------------------------------------------------------------------------
   -- Lecture en boucle entre deux mesures

   -- entre ou sort du mode lecture en boucle
   procedure Set_mode_loop( do_loop : boolean );

   -- mesure de d�part
   procedure Set_Start_mesure( num_mesure : natural );

   -- mesure de fin
   procedure Set_End_mesure( num_mesure : natural );

   -- numero de la derniere mesure du fichier en cours
   function Get_last_mesure return natural;

   -- -------------------------------------------------------------------------------

   -- reglage des volumes: valeur est dans le range 0..100
   -- 50 = pas de changement, 100 = tout au max, 0 = pas de son
   procedure Set_Melody_volume( value : natural );
   procedure Set_Accomp_volume( value : natural );

   -- -------------------------------------------------------------------------------
   -- Mode interactif

   -- joue une note et l'arr�te
   procedure Play_user_note( note : integer );
   procedure Stop_user_note;

   -- *******************************************************************************

   -- Interface de bas niveau

   -- ouverture et fermeture du device midi
   procedure Close_midi_device;
   procedure Open_Midi_device;

   -- appel� par le timer pour chaque tick (intervalle de timer_delay ms)
   procedure Do_Next_Tick;

   -- positionne le player pour jouer � partir d'un temps donn�
   procedure Seek_player( time : integer );

   -- lecture des notes de le melodie comprises dans l'intervalle temporel [debut,fin]
   procedure Get_notes( start_time, end_time  : Integer;
                        first_note, last_note : out Data_pkg.midi_event_ptr;
                        time_offset           : out integer ); -- pour la conversion en temps absolu

   -- temps interne du player
   function Get_Player_time return integer;

   -- lecture du texte suivant
   procedure Get_parole( temps : out integer;
                         texte : out Common_types.string_pt;
                         next  : boolean );

end Midi_pkg;

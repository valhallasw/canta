with Data_pkg;

package Affichage_pkg is

   procedure Display_textes( Pitch : Long_Float; Note, Octave : Natural );
   -- nom du fichier MIDI
   procedure Display_file_name;

   procedure Display_volume( volume : Short_Integer );

   procedure Display_spectre( index : integer );

   -- mise à jour périodique du score
   procedure Update_score( now : integer );
   procedure Update_score;			-- réaffichage sans changement du temps

   -- décalage d'affichage en octave
   procedure Set_Decalage( valeur : Long_float );

   -- transposition des notes, par 1/2 ton (ie note midi)
   procedure Set_Transpo( valeur : integer );

   -- décalage de l'affichage des notes par octave
   procedure Set_Octave( valeur : integer );

   -- changement taille du score
   procedure Resize_score;

   -- idem pour les textes
   procedure Update_textes;

   -- augmente, si possible, le niveau de zoom du score
   procedure Zoom;

   -- diminue, le niveau de zoom du score
   procedure Un_zoom;

   -- scroll le score si possible
   procedure Scroll_haut;
   procedure Scroll_bas;
   procedure Scroll_vertical( note : integer );

   -- position du milieu de l'écran en note MIDI
   function Get_Milieu return integer;


   -- entre ou sort du mode pause
   procedure Toggle_pause;

   function in_pause return boolean;

   -- vitesse du défilement horizontal
   procedure Accelerer;

   procedure Ralentir;

   -- remet à zéro le tableau des paroles
   -- doit être applelé après le chargement d'un fichier Midi
   procedure Init_Paroles;

   -- retourne la mesure correspondante à la position horizontale en pixels dans le display MESURES_ID
   function Get_mesure( position : integer ) return Data_pkg.mesure_event_ptr;

   -- retourne la note correspondant à a position verticale en pixel dans le display SCORE_ID
   function Get_note( position : integer ) return integer;

   -- réécrit les paroles après un seek
   procedure Reset_paroles;

   -- Centrage automatique sur la mélodie
   procedure Auto_centrage( min, max : Integer );

end Affichage_pkg;

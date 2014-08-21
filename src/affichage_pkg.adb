with Ada.Strings.Unbounded;
use Ada.Strings.unbounded;
with Calendar;

with Interfaces.C;	use Interfaces.C;

with Win32;		use Win32;
with Win32.Windef;
with Win32.Winuser;
with Win32.Wingdi;

with Intl;
with Objects_pkg;
with Resources_pkg;	use Resources_pkg;
with Conversions;	use Conversions;
with Common_types;	use Common_types;
with Log;
with Timer_pkg;
with Midi_pkg;
with Process_pkg;
with Data_pkg;		use Data_pkg;
with Config_pkg;


package body Affichage_pkg is


   -- note du milieu servant de référence pour l'affichage
   milieu : integer  := (Common_types.Min_midi + Common_types.Max_Midi) / 2;
   -- nombre de notes affichées
   nb_note : integer := Common_types.max_midi - Common_types.min_midi;

   -- trou max rebouche
   max_trou : integer := 13;
   -- vitesse du scrolling horizontal en pixel par secondes
   min_scroll_horiz : constant := 30.0;
   scroll_horiz : Long_Float := 65.0;
   x_speed : Long_Float := scroll_horiz / 1000.0;

   -- hauteur d'un 1/2 ton dans le score
   min_hauteur_note : constant := 5;
   hauteur_note  : Integer := min_hauteur_note;		-- toujours impair
   offset_note   : Integer := (hauteur_note+1) / 2 - 1;	-- pixel du centre

   -- décalage entre l'affichage et la lecture en secondes
   decalage_affichage : Long_Float := 2.0;

   -- nombre d'octave de décalage pur l'affichage des notes MIDI
   decalage_octave : integer := 0;
   -- transposition par 1/2 tons
   transpo : integer := 0;

   -- seuil maximum de changement de couleur de l'indicateur de volume
   seuil_satur : constant Short_Integer := (16#7FFF# * 95) / 100;

   -- temps de l'affichage
   last_time : integer := 0;
   -- position X de l'origine des temps
   origine  : integer;


   -- flag de pause: fige l'affichage
   freezed : boolean := false;

   -- type et data pour l'affichage des paroles
   type parole_item_type is record
      temps : integer := -1;		-- flag de non-initialisation
      texte : string_pt;
   end record;
   type parole_array_type is array(natural range <>) of parole_item_type;
   type parole_array_pt is access parole_array_type;
   -- data pour les paroles
   paroles : parole_array_pt;
   -- nombre de lignes de textes
   NB_lignes : natural := 0;
   hauteur_ligne : Natural := 0;

   -- mesure: largeur d'une barre de séparation en pixels
   largeur_barre : constant := 2;

   -- ***********************************************************************



   -- image d'un float avec 1 chiffre après la virgule
   function String_image( f : Long_float ) return string is
      tmp : constant Integer := Integer( f * 10.0 );
      s : constant String := Integer'image( tmp );
      buffer : string(s'first..s'last+1);		-- un char de plus pour le '.'
   begin
      buffer(s'first..s'last-1) := s(s'first..s'last-1);
      buffer(s'last) := '.';
      buffer(s'last+1) := s(s'last);
      return buffer;
   end String_image;


   -- image d'un entier naturel sans l'espace devant
   function String_Image( I : Natural ) return String is
      s : constant string := Integer'Image(I);
   begin
      return s(s'first+1..s'last);
   end String_Image;

   -- image d'un entier avec le signe + ou - ou rien si 0
   function Signed_image( I : Integer ) return String is
   begin
      if I > 0 then
         return "+" & String_Image(i);  -- ajoute le signe +
      elsif I < 0 then
         return Integer'image(I);	-- le signe - est déjà là
      else
         return "0";
      end if;
   end Signed_image;


   -- ================================================================================


   procedure Display_textes( Pitch : Long_Float; Note, Octave : Natural ) is
      -- génère les strings
      pitch_string  : constant String := String_image( pitch );
      Nom_string    : constant string := Intl.Nom_notes(Note);
      Octave_string : constant string := String_image(Octave);
   begin
      -- affiche chaque string dans sa fenêtre
      Objects_pkg.Set_Text( PITCH_ID, pitch_string );
      Objects_pkg.Set_Text( NOM_ID, Nom_string );
      Objects_pkg.Set_Text( OCT_ID, octave_string );
   end Display_textes;

   -- ================================================================================

   procedure Display_file_name is
   begin
      Objects_pkg.Set_Text( NOM_FIC_ID, Midi_pkg.File_name );
   end Display_file_name;


   -- ================================================================================


   procedure Display_volume( volume : Short_Integer ) is
      res_int : Win32.INT;
      val_max : Win32.Windef.RECT;
      display : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( VOLUME_ID );
      fond_rect : Win32.Windef.RECT;
   begin
      --
      -- dessin dans la bitmap
      --
      -- rectangle pour effacement et écriture
      fond_rect := ( 0, 0, Win32.LONG(display.inner_w), Win32.LONG(display.inner_h) );
      -- calcul du rectangle à afficher
      val_max := fond_rect; -- on part du fond complet
      -- on ajuste la hauteur en fonction du volume
      val_max.top := val_max.bottom - (Win32.LONG(display.inner_h) * Win32.LONG(volume)) / 16#7FFF# ;
      --
      -- effacement
      res_int := Win32.Winuser.FillRect( display.memdc, TO_ACRECT(fond_rect'address), display.brush );
      -- affichage du niveau
      if volume > seuil_satur then
         -- risque de saturation -> rouge
         res_int := Win32.Winuser.FillRect( display.memdc, TO_ACRECT(val_max'address), brush_rouge );
      elsif volume < Common_types.seuil_pitch then
         -- niveau trop faible -> gris
         res_int := Win32.Winuser.FillRect( display.memdc, TO_ACRECT(val_max'address), brush_grise );
      else
         -- niveau normal -> vert
         res_int := Win32.Winuser.FillRect( display.memdc, TO_ACRECT(val_max'address), brush_verte );
      end if;
      --
      -- Affichage à l'écran
      Objects_pkg.Display_display( display );
      --
   end Display_Volume;


   -- ==============================================================================


   procedure Display_spectre( index : integer ) is
      res_int : Win32.INT;
      display : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( SPECTRE_ID );
      fond_rect, rec_filtre  : Win32.Windef.RECT;
      points : array(0..Integer(display.inner_w)) of Short_Integer := (others => 0);
      old_obj : Win32.Windef.HGDIOBJ;
      min_i, max_i : Integer;
      res_bool : Win32.BOOL;

      procedure Interpole is
         DY, Inc : Long_Float;
         X, X0 : Integer;
      begin
         -- essai 1 : interpolation linéaire
         X0 := 0;
         for I in 1..Process_pkg.num_bins-1 loop
            DY := Long_Float( points(X0+largeur_filtre)-points(X0)) / Long_Float(largeur_filtre);
            X := X0 + 1;
            Inc := DY;
            for J in 1..largeur_filtre-1 loop
               points(X) := points(X0) + Short_Integer( Inc );
               X := X + 1;
               INC := INC + DY;
            end loop;
            X0 := X0 + largeur_filtre;
         end loop;
      end Interpole;

   begin
      -- on efface le fond
      fond_rect := ( 0, 0, Win32.LONG(display.inner_w), Win32.LONG(display.inner_h) );
      res_int := Win32.Winuser.FillRect( display.memdc, TO_ACRECT(fond_rect'address), display.brush );
      -- bas de l'affichage
      rec_filtre.bottom := Win32.LONG(display.inner_h);
      -- le max de la sortie de chaque filtre
      for i in 0..Process_pkg.num_bins-1 loop
         -- les valeurs des filtres
         points(i * largeur_filtre) := Short_Integer( Process_pkg.spectre(i+1) * Long_Float(display.inner_h) );
      end loop;
      -- Interporlation des points pour avoir une courbe agréable à l'oeil
      Interpole;
      --
      -- affichage de la courbe
      --
      -- zone devant être colorée en rouge
      if index > 0 then
         if index = 1 then
            min_i := 0;
            max_i := largeur_filtre/2+1;
         elsif index = Process_pkg.Num_bins then
            max_i := points'last+1;
            min_i := max_i - largeur_filtre/2;
         else
            min_i := (Index-1) * largeur_filtre - largeur_filtre/2;
            max_i := min_i + largeur_filtre;
         end if;
         -- sélection du vert
         old_obj :=  Win32.Wingdi.SelectObject( display.memdc, Pen_vert );
         for i in points'range loop
            -- déplacement en (X,0)
            res_bool := Win32.Wingdi.MoveToEx( display.memdc, Win32.INT(i), Win32.INT(display.inner_h), null );
            -- ligne vers (X,Y)
            if i = min_i then
               -- début de la zone rouge
               old_obj :=  Win32.Wingdi.SelectObject( display.memdc, Pen_Rouge );
            elsif i = max_i + 1 then
               -- fin de la zone rouge
               old_obj :=  Win32.Wingdi.SelectObject( display.memdc, Pen_vert );
            end if;
            -- trace ligne verticale
            res_bool := Win32.Wingdi.LineTo( display.memdc, Win32.INT(i), Win32.INT(display.inner_h) - Win32.INT(points(i)+1) );
         end loop;
      else
         -- sélection du gris pour tout le spectre
         old_obj :=  Win32.Wingdi.SelectObject( display.memdc, Pen_gris );
         for i in points'range loop
            -- déplacement en (X,0)
            res_bool := Win32.Wingdi.MoveToEx( display.memdc, Win32.INT(i), Win32.INT(display.inner_h), null );
            -- trace ligne verticale
            res_bool := Win32.Wingdi.LineTo( display.memdc, Win32.INT(i), Win32.INT(display.inner_h) - Win32.INT(points(i)+1) );
         end loop;

      end if;
      --
      -- Affichage à l'écran
      Objects_pkg.DIsplay_display( display );
      --
   end Display_Spectre;


   -- ==========================================================================================

   -- Affichage de la partie centrale: le Score
   --
   procedure Update_score( now : integer ) is
      rect     : Win32.Windef.RECT;
      res_int  : Win32.INT;
      old_obj  : Win32.Windef.HGDIOBJ;
      res_bool : Win32.BOOL;
      grille_y : integer;
      base_do  : integer;
      prem_mes_pt, last_mes_pt   : Data_pkg.mesure_event_ptr;
      prem_midi_pt, last_midi_pt : Data_pkg.midi_event_ptr;
      prem_note_pt, last_note_pt : Data_pkg.note_event_ptr;
      offset   : integer;
      restart : boolean;
      showstr : Unbounded_String;
      --
      display : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( SCORE_ID );
      mesure  : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( MESURES_ID );


      package SU   renames Ada.Strings.Unbounded;
      -- Mapping Note (en cents) -> ordonnées (en pixels)
      function To_Y( N : integer ) return Integer is
      begin
         return display.inner_h/2 + ((milieu*100 - N) * hauteur_note)/100;
      end To_Y;

      -- Mapping temps <-> abscisse

      -- ( T - now ) * X_speed = Origine - X
      function To_X ( t : integer ) return Integer is
      begin
         return origine - Integer( Long_Float(now-t) * x_speed );
      end To_X;

      function To_Time( X : integer ) return integer is
      begin
         return Integer( Long_Float(now) -  Long_Float(origine - X) / x_speed );
      end To_Time;

      -- Dessin
      -- Y est l'ordonnée de la ligne centrale

      -- Midi : un rectangle pour chaque note
      procedure Draw_midi( debut_x, fin_x, y : integer;
                           avenir : boolean := false ) is
         res_int : Win32.INT;
         rect    : Win32.Windef.RECT;
      begin
         if Y in 0..Integer(display.inner_h-1) then
            rect := ( top => Win32.LONG(y - offset_note),
                      left => Win32.LONG(debut_x),
                      right => Win32.LONG(fin_x),
                      bottom => Win32.LONG(y + offset_note) );
            if avenir then
               res_int := Win32.Winuser.FillRect( display.memDC, TO_ACRECT(rect'address),
                                                  Resources_pkg.brush_grise_leger );
            else
               res_int := Win32.Winuser.FillRect( display.memDC, TO_ACRECT(rect'address),
                                                  Resources_pkg.brush_midi );
            end if;
         end if;
      end Draw_midi;

      -- Mesures: un rectangle numéroté
      procedure Draw_Mesure( debut_x, fin_x, numero : integer ) is
         res_int : Win32.INT;
         rect    : Win32.Windef.RECT;
         str     : constant string := string_image(numero);
      begin
         -- définition rectangle de la mesure
         rect := ( top => 0,
                   left => Win32.LONG(debut_x),
                   right => Win32.LONG(fin_x),
                   bottom => Win32.LONG(mesure.inner_h) );
         -- remplissage du fond du rectangle
         res_int := Win32.Winuser.FillRect( mesure.memDC, TO_ACRECT(rect'address),
                                            Resources_pkg.brush_mesure );
         -- barre de début
         rect := ( top => 0,
                   left => Win32.LONG(debut_x),
                   right => Win32.LONG(debut_x + largeur_barre ),
                   bottom => Win32.LONG(mesure.inner_h) );
         -- remplissage du rectangle
         res_int := Win32.Winuser.FillRect( mesure.memDC, TO_ACRECT(rect'address),
                                            Resources_pkg.brush_noire );
         -- barre de fin
         rect := ( top => 0,
                   left => Win32.LONG(fin_x - largeur_barre),
                   right => Win32.LONG(fin_x ),
                   bottom => Win32.LONG(mesure.inner_h) );
         -- remplissage du rectangle
         res_int := Win32.Winuser.FillRect( mesure.memDC, TO_ACRECT(rect'address),
                                            Resources_pkg.brush_noire );
         -- écriture numéro de mesure:
         -- définition du rectangle
         rect := ( top => 0,
                   left => Win32.LONG(debut_x + largeur_barre + 5),
                   right => Win32.LONG(fin_x - largeur_barre - 5),
                   bottom => Win32.LONG(mesure.inner_h) );
         -- écrit le texte
         res_int := Win32.Winuser.DrawText( mesure.memDC,
                         TO_PCCH(str'address),
                         str'length,
                         TO_PRECT(rect'address),
                         Win32.Winuser.DT_VCENTER + Win32.Winuser.DT_LEFT );
      end Draw_Mesure;


      procedure Commence_Ligne( x, y : integer ) is
         res_int : Win32.INT;
         rect    : Win32.Windef.RECT;
      begin
         -- point isolé
         rect := ( top => Win32.LONG(y-1),
                      left => Win32.LONG(x-1),
                      right => Win32.LONG(x+1),
                      bottom => Win32.LONG(y+1) );
         res_int := Win32.Winuser.FillRect( display.memDC, TO_ACRECT(rect'address), Resources_pkg.brush_score );
         -- mémorise début de ligne
         res_bool := Win32.Wingdi.MoveToEx( display.memDC, Win32.INT(x), Win32.INT(y), null );
      end Commence_Ligne;


      procedure Continue_ligne( x, y : integer ) is
         res_bool : Win32.BOOL;
      begin
         -- trace ligne vers (x,y)
         res_bool := Win32.Wingdi.LineTo( display.memDC, Win32.INT(x), Win32.INT(y) );
      end Continue_ligne;

      -- dessine une ligne horizontal de la grille
      procedure Draw_grille( y : integer ) is
      begin
         -- si à l'intérieur de la fenetre, dessiner une ligne avec le Pen courant
         -- déplacement en (0,grille_y)
         res_bool := Win32.Wingdi.MoveToEx( display.memDC, 0, Win32.INT(y), null );
         -- trace ligne horizontale
         res_bool := Win32.Wingdi.LineTo( display.memDC, Win32.INT(display.inner_w), Win32.INT(y) );
      end Draw_grille;

   begin
      -- memorise le temps d'affichage
      last_time := now;
      --
      -- position de l'origine en X
      origine := Integer(display.inner_w) - Integer(decalage_affichage * scroll_horiz);
      --
      -- on efface tout
      -- score
      rect := ( top => 0,
                left => 0,
                right => Win32.LONG(display.inner_w),
                bottom => Win32.LONG(display.inner_h) );
      res_int := Win32.Winuser.FillRect( display.memDC, TO_ACRECT(rect'address), display.brush );
      -- mesure
      rect := ( top => 0,
                left => 0,
                right => Win32.LONG(mesure.inner_w),
                bottom => Win32.LONG(mesure.inner_h) );
      res_int := Win32.Winuser.FillRect( mesure.memDC, TO_ACRECT(rect'address), mesure.brush );
      --
      -- On dessine la grille dans le fond
      --
      -- parcours les octaves
      base_do := 3600;			-- Do 2, en cents
      -- position verticale de ce Do
      grille_Y := TO_Y( base_do );
      for oct in 2..6 loop
         -- sélection du pen de couleur gris, 1 pixel, plein
         old_obj := Win32.Wingdi.SelectObject( display.memDC, Resources_pkg.Pen_gris_plein );
         draw_grille( grille_y );
         grille_Y := grille_Y - hauteur_note;
         -- dessine les autres demi-tons
         old_obj := Win32.Wingdi.SelectObject( display.memDC, Resources_pkg.Pen_gris_leger );
         for ton in 1..11 loop
            draw_grille( grille_y );
            grille_Y := grille_Y - hauteur_note;
         end loop;
         -- octave suivant
      end loop;
      -- ligne verticale à l'origine ( temps = now )
      old_obj := Win32.Wingdi.SelectObject( display.memDC, Resources_pkg.Pen_gris_plein );
      res_bool := Win32.Wingdi.MoveToEx( display.memDC, Win32.INT(origine), Win32.INT(display.inner_h), null );
      res_bool := Win32.Wingdi.LineTo( display.memDC, Win32.INT(origine), 0 );
      --
      -- MIDI
      --
      if Midi_pkg.Is_Playing then
         -- Affiche les notes à venir en gris
         Midi_pkg.Get_notes( start_time  => now,
                             end_time    => To_Time(Integer(display.inner_w)),
                             first_note  => prem_midi_pt,
                             last_note   => last_midi_pt,
                             time_offset => offset );
         -- parcourt la liste et dessine
         while prem_midi_pt /= null loop
            -- affiche la note
            Draw_midi( To_X(prem_midi_pt.start_time + offset),	-- converti en temps absolu
                       To_X(prem_midi_pt.End_time + offset),	-- idem
                       To_Y(Integer(prem_midi_pt.note * 100) + transpo * 100 + decalage_octave * 1200),	-- converti en cents
                       true );
            -- test si c'était le dernier event
            exit when prem_midi_pt = last_midi_pt;
            -- non, passe au suivant
            prem_midi_pt := prem_midi_pt.next;
         end loop;
      end if;
      --
      -- affiche les notes effectivement jouées en bleu
      Data_pkg.Get_Midi( start_time  => To_time(0),
                         end_time    => now,
                         first_event => prem_midi_pt,
                         last_event  => last_midi_pt );
      -- parcourt la liste et dessine
      while prem_midi_pt /= null loop
         -- affiche la note
         Draw_midi( To_X(prem_midi_pt.start_time),
                    To_X(prem_midi_pt.End_time),
                    To_Y(Integer(prem_midi_pt.note)+ transpo * 100 + decalage_octave * 1200) );
         -- test si c'était le dernier event
         exit when prem_midi_pt = last_midi_pt;
         -- non, passe au suivant
         prem_midi_pt := prem_midi_pt.next;
      end loop;
      --
      -- affiche les mesures
      Data_pkg.Get_mesure( start_time    => To_time(0),
                           end_time      => now,
                           first_mesure  => prem_mes_pt,
                           last_mesure   => last_mes_pt );
      -- parcourt la liste et dessine
      while prem_mes_pt /= null loop
         -- affiche la note
         Draw_Mesure( To_X(prem_mes_pt.start_time),	-- converti en temps absolu
                      To_X(prem_mes_pt.End_time),		-- idem
                      Integer(prem_mes_pt.note) );				-- numéro de mesure
         -- test si c'était le dernier event
         exit when prem_mes_pt = last_mes_pt;
         -- non, passe au suivant
         prem_mes_pt := prem_mes_pt.next;
      end loop;
      --
      -- Notes chantées
      --
      -- lecture des notes à afficher
      Data_pkg.Get_note( start_time  => To_Time(0),
                         end_time    => now,
                         first_event => prem_note_pt,
                         last_event  => last_note_pt );
      -- test s quelque chose à faire
      if prem_note_pt /= null and last_note_pt /= null then
        -- sélection du pen de couleur rouge, 1 pixel, plein
         old_obj := Win32.Wingdi.SelectObject( display.memDC, Resources_pkg.Pen_score );
         --
         -- premier point, pour initialiser le tracé de lignes
         while prem_note_pt /= last_note_pt and then prem_note_pt.note = 0 loop
            -- ignore les fins de notes
            prem_note_pt := prem_note_pt.next;
         end loop;
         -- test si au moins une note
         if prem_note_pt = last_note_pt then
            -- un seul point
            if  prem_note_pt.note /= 0 then
               -- ... à afficher
               commence_ligne( To_X(prem_note_pt.time), TO_Y(Integer(prem_note_pt.note)) );
            end if;
            -- sinon ne rien faire
         else
            -- plusieurs points, on affiche le premier
            commence_ligne( To_X(prem_note_pt.time), TO_Y(Integer(prem_note_pt.note)) );
            -- on continue avec les suivant
            prem_note_pt := prem_note_pt.next;
            restart := false;
            loop
               if prem_note_pt.note = 0 and prem_note_pt.ampli = 0 then
                  -- hors note, ne rien afficher
                  restart := true;
               elsif prem_note_pt.note > 0 then
                  if restart then
                     -- debut d'une nouvelle note
                     commence_ligne( To_X(prem_note_pt.time), TO_Y(Integer(prem_note_pt.note)) );
                     restart := false;
                  else
                     -- suite d'une note
                     continue_ligne( To_X(prem_note_pt.time), TO_Y(Integer(prem_note_pt.note)) );
                  end if;
               end if;
               -- test si on vient de tracer le dernier point
               exit when prem_note_pt = last_note_pt;
               -- non, on passe au suivant
               prem_note_pt := prem_note_pt.next;
            end loop;
         end if;
      end if;
      --
      -- affiche les bitmaps
      --
      Objects_pkg.Display_display( display );
      Objects_pkg.Display_display( mesure );
      --

      Append(showstr, To_Unbounded_String("t="));
      Append(showstr, To_Unbounded_String(Integer'Image(now)));
      Append(showstr, To_Unbounded_String(" midi="));
      if last_midi_pt /= null then
         showstr := showstr & Short_Integer'Image(last_midi_pt.note);
      else
         showstr := showstr & "null" ;
      end if;

      showstr := showstr & " input=";
      if last_mes_pt /= null then
         showstr := showstr & Short_Integer'Image(last_mes_pt.note);
      else
         showstr := showstr & "null";
      end if;

      Objects_pkg.Set_Text(NOM_FIC_ID, To_String(showstr));
   end Update_score;

   procedure Update_Score is
   begin
      Update_Score( Last_time );
   end Update_Score;


   -- ====================================================================================


   -- donne la mesure correspondant à la position sur le display
   function Get_mesure( position : integer ) return Data_pkg.mesure_event_ptr is
      time : integer;
   begin
      -- calcule le temps absolu correspondant à la position
      time := Integer( Long_Float(last_time) -  Long_Float(origine - position) / x_speed );
      -- recherche la mesure correspondant
      return Data_pkg.Get_Mesure( time );
   end Get_mesure;

   -- donne la note MIDI correspondant à la position sur le score
   -- (inverse de To_Y mais en MIDI pas en cents)
   function Get_note( position : integer ) return integer is
      display : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( SCORE_ID );
   begin
      if position >= Integer(display.inner_h)/2 then
         return milieu - ( position - Integer(display.inner_h)/2 + hauteur_note/2 ) / hauteur_note;
      else
         return milieu - ( position - Integer(display.inner_h)/2 - hauteur_note/2 ) / hauteur_note;
      end if;
   end Get_note;


   -- ============================================================================================


   procedure Set_Decalage( valeur : Long_float ) is
   begin
      decalage_affichage := valeur;
   end Set_Decalage;


   procedure Set_Octave( valeur : integer ) is
   begin
      decalage_octave := valeur;
   end Set_Octave;


   procedure Set_Transpo( valeur : integer ) is
   begin
      transpo := valeur;
   end Set_Transpo;

   -- ----------------------------------------------------------------------------------


   procedure Ecrit_paroles( display : Objects_pkg.UI_object_ptr ) is
      rect    : Win32.Windef.RECT;
      res_int : Win32.INT;
      Y       : Win32.LONG;
      old     : Win32.WIndef.HGDIOBJ;
   begin
      -- on efface tout
      rect := ( top => 0,
                left => 0,
                right => Win32.LONG(display.inner_w),
                bottom => Win32.LONG(display.inner_h) );
      res_int := Win32.Winuser.FillRect( display.memDC, TO_ACRECT(rect'address), display.brush );
      -- on écrit les textes
      Y := 0;
      for i in 1..NB_lignes loop
         if paroles(i).texte /= null then
            if i = 1 then
               -- sélection de la police: bold
               old := Win32.Wingdi.SelectObject( display.memDC, Resources_pkg.Text_font );
            elsif i = 2 then
               -- sélection de la police: normal
               old := Win32.Wingdi.SelectObject( display.memDC, Resources_pkg.Text_light_font );
            end if;
            -- cadre le rectangle à gauche
            rect := ( top    => Y,
                      bottom => Y + Win32.LONG(hauteur_ligne),
                      left   => 3,
                      right  => Win32.LONG(display.inner_w) );
            -- écrit le texte
            res_int := Win32.Winuser.DrawText( display.memDC,
                         TO_PCCH(paroles(i).texte.all'address),
                         paroles(i).texte.all'length,
                         TO_PRECT(rect'address),
                         Win32.Winuser.DT_VCENTER + Win32.Winuser.DT_LEFT );

         end if;
         Y := Y + Win32.LONG(hauteur_ligne);
      end loop;
      --
      -- on affiche la bitmap
      Objects_pkg.Display_display( display );
      --
   end Ecrit_paroles;


   procedure Init_paroles is
      display   : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( TEXTE_ID );
      lptm    : aliased Win32.Wingdi.TEXTMETRICA;
      res_bool : Win32.BOOL;
      res_int : Win32.INT;
   begin
      if paroles = null then
         -- fond transparent
         res_int := Win32.Wingdi.SetBkMode( display.memDC, Win32.Wingdi.TRANSPARENT );
         -- lecture hauteur d'une ligne de la font sélectionnée
         res_bool := Win32.Wingdi.GetTextMetrics ( display.memDC, lptm'access );
         hauteur_ligne := Integer(lptm.tmHeight);
         -- nombre de ligne de l'affichage
         NB_lignes := Integer(display.inner_h) / hauteur_ligne;
         -- allocation tableau
         paroles := new parole_array_type(1..NB_lignes);
      end if;
      -- lecture des valeurs initiales
      for i in 1..NB_lignes loop
         Midi_pkg.Get_parole( paroles(i).temps, paroles(i).texte, next => i > 1 );
      end loop;
      -- affichage
      Ecrit_paroles( display );
      --
   end Init_paroles;

   -- réécrit les paroles apreès un changement de temps (seek)
   procedure Reset_paroles is
      display   : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( TEXTE_ID );
   begin
      for i in 1..NB_lignes loop
         Midi_pkg.Get_parole( paroles(i).temps, paroles(i).texte, next => i > 1 );
      end loop;
      -- affichage
      Ecrit_paroles( display );
   end Reset_paroles;


   procedure Update_textes is
      display   : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( TEXTE_ID );
   begin
      -- met en premier le texte de la deuxième ligne quand il devient actuel
      if paroles(2).temps <= Midi_pkg.Get_Player_time then
         -- scroll le tableau des paroles
         for i in 1..nb_lignes-1 loop
            paroles(i) := paroles(i+1);
         end loop;
         -- lecture nouvelle parole
         Midi_pkg.Get_parole( paroles(NB_lignes).temps, paroles(NB_lignes).texte, next => true );
      end if;
      --
     Ecrit_paroles( display );
   end Update_textes;


   -- **********************************************************************************

   procedure resize_score is
      display : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( SCORE_ID );
   begin
      -- nouveaux parametres
      hauteur_note := Integer(display.inner_h) / nb_note;
      offset_note  := (hauteur_note+1) / 2 - 1;
   end Resize_score;

   -- SCROLL ------------------------------------------------------------------------------

   procedure Limite_scroll is
   begin
      if milieu > max_midi - nb_note / 2 then
         milieu := max_midi - nb_note / 2;
      elsif milieu < min_midi + nb_note / 2 then
         milieu := min_midi + nb_note / 2;
      end if;
   end Limite_scroll;

   procedure Scroll_haut is
   begin
      milieu := milieu - 1;
      limite_scroll;
      if freezed then
         Update_score;
      end if;
   end Scroll_haut;


   procedure Scroll_bas is
   begin
      milieu := milieu + 1;
      limite_scroll;
      if freezed then
         Update_score;
      end if;
   end Scroll_bas;


   procedure Scroll_vertical( note : integer ) is
   begin
      milieu := note;
      limite_scroll;
      if freezed then
         Update_score;
      end if;
   end Scroll_vertical;


   -- ZOOM ------------------------------------------------------------------


   -- augmente le zoom d'un niveau: les rectangles des notes augmentent en hauteur
   procedure Zoom is
      display : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( SCORE_ID );
      max_hauteur : Integer;
   begin
      --
      -- s'assure qu'un octave est toujours visible
      max_hauteur := Integer(display.inner_h) / 12;
      if hauteur_note < max_hauteur - 2 then
         -- augmente la hauteur des notes affichées
         hauteur_note := hauteur_note + 2;
         offset_note   := (hauteur_note+1) / 2 - 1;
         -- memorise nombre de note pour le resizing
         nb_note := Integer(display.inner_h) / hauteur_note;
         --
         Limite_scroll;
      end if;
      if freezed then
         Update_score;
      end if;
   end Zoom;

   -- diminue le zoom dun niveau: les rectangles des notes diminuent en hauteur
   procedure Un_zoom is
      display : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( SCORE_ID );
   begin
      if hauteur_note >= min_hauteur_note + 2 -- taille minimale absolue d'affichage
         and hauteur_note > Integer(display.inner_h)/(max_midi-min_midi) -- taille minimale à cette dimension
      then
         -- diminue la hauteur des notes
         hauteur_note := hauteur_note - 2;
         offset_note  := (hauteur_note+1) / 2 - 1;
         -- memorise nombre de note pour le resizing
         nb_note := Integer(display.inner_h) / hauteur_note;
         -- scroll pour que ça reste bien centré
         limite_scroll;
      end if;
      --
      if freezed then
         Update_score;
      end if;
   end Un_zoom;


   -- retourne la note du milieu de l'écran pour mettre à jour l'ascenseur vertical
   function Get_milieu return integer is
   begin
      return milieu;
   end Get_milieu;

   -- PAUSE -----------------------------------------------------------------------

   procedure Toggle_pause is
   begin
      freezed := not freezed;
   end Toggle_pause;

   function in_pause return boolean is
   begin
      return freezed;
   end in_pause;

   -- VITESSE -------------------------------------------------------------------------

   procedure Accelerer is
   begin
      -- augmente la vitesse du scroll horizontal
      scroll_horiz := scroll_horiz + 5.0;
      -- recalcule le trou correspondant en pixels
      max_trou := Integer(scroll_horiz) / 5;
      -- calcul le décalage en pixel par tick
      x_speed := scroll_horiz / 1000.0;
      if freezed then
         Update_score;
      end if;
   end Accelerer;

   procedure Ralentir is
   begin
      if scroll_horiz > min_scroll_horiz then
         scroll_horiz := scroll_horiz - 5.0;
         max_trou := Integer(scroll_horiz) / 5;
         x_speed := scroll_horiz / 1000.0;
      end if;
      if freezed then
         Update_score;
      end if;
   end Ralentir;

   -- AUTO-CENTRAGE ---------------------------------------------------------------------

   procedure Auto_centrage( min, max : Integer ) is
      display : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( SCORE_ID );
      max_note : integer := max + decalage_octave*12;
      min_note : integer := min + decalage_octave*12;
      max_hauteur : integer;
   begin
      -- reste dans les limites autorisées
      if min_note < Common_types.min_midi then
         min_note := Common_types.min_midi;
      end if;
      if max_note > Common_types.max_midi then
         max_note := Common_types.max_midi;
      end if;
      -- calcul les paramètres
      milieu := (max_note + min_note) / 2;	-- note du milieu de l'écran
      hauteur_note := Integer(display.inner_h) / (max_note - min_note + 2);	-- hauteur des notes
      -- s'assure que hauteur note est dans les limites autorisées
      if hauteur_note < min_hauteur_note then
         hauteur_note := min_hauteur_note;
      end if;
      max_hauteur := Integer(display.inner_h) / 12;
      if hauteur_note > max_hauteur then
         hauteur_note := max_hauteur;
      end if;
      -- s'assure que hauteur_note est impair
      if hauteur_note mod 2 = 0 then
         hauteur_note := hauteur_note - 1;
      end if;
      -- position du centre
      offset_note  := (hauteur_note+1) / 2 - 1;
      -- nombre de notes, pour le resizing
      nb_note := Integer(display.inner_h) / hauteur_note;
      --
      if freezed then
         Update_score;
      end if;
   end Auto_centrage;

end Affichage_pkg;

with System;
with Interfaces.C;	use Interfaces.C;

with Win32;
with Win32.Windef;
with Win32.Winnt;
with Win32.Winuser;
with Win32.Wingdi;

with Conversions;	use Conversions;
with Common_types;	use Common_types;
with Resources_pkg;	use Resources_pkg;
with Bitmap_pkg;
with Affichage_pkg;
with Process_pkg;
with Objects_pkg;	use Objects_pkg;
with Intl;
with Log;
with Skins_pkg;

package body Layout_pkg is

   min_window_width  : constant := 703;
   min_window_height : constant := 535;

   Largeur_main : integer := min_window_width;
   Hauteur_main : integer := min_window_height;

   -- marges à l'intérieur de la main window
   marge_haut : constant := 15;
   marge_gauche : constant := 15;
   marge_droite : constant := 12;
   marge_bas : constant := 15;

   -- taille volume
   hauteur_volume : constant := 100;
   largeur_volume : constant := 18;

   -- largeur spectre
   hauteur_spectre : constant := 100;
   largeur_spectre : constant := Common_types.largeur_filtre * (Process_pkg.num_bins-1);

   -- labels Hertz
   largeur_Hz : constant := 35;
   hauteur_Hz : constant := 22;

   -- affichage pitch
   hauteur_pitch : constant := 22;
   largeur_pitch : constant := 78;

   -- affichage nom de note
   largeur_nom : constant := 85;
   hauteur_nom : constant := 45;

   -- affichage octave
   largeur_oct : constant := 30;

   -- Score
   min_largeur_score : constant := 622;
   min_hauteur_score : constant := 275;

   -- Paroles
   largeur_texte : constant := 125;
   hauteur_texte : constant := 102;

   -- label highlighted: transposition
   largeur_hi_label : constant := 28;
   hauteur_hi_label : constant := 24;

   -- compteur et status
   largeur_compteur : constant := 72;
   hauteur_compteur : constant := 45;

   -- nom du fichier MIDI
   hauteur_nom_fic : constant := 16;

   -- hauteur display des mesures
   hauteur_mesures : constant := 18;

   -- cadre par défaut
   default_frame : Objects_pkg.frame_set := ( DISP_CHG_ID, DISP_BH_ID, DISP_CHD_ID,
                                            DISP_BD_ID,  DISP_CBD_ID, DISP_BB_ID,
                                            DISP_CBG_ID, DISP_BG_ID);
   -- cadre fin
   thin_frame : Objects_pkg.frame_set := ( THIN_CHG_ID, THIN_BH_ID, THIN_CHD_ID,
                                            THIN_BD_ID,  THIN_CBD_ID, THIN_BB_ID,
                                            THIN_CBG_ID, THIN_BG_ID);

   -- *************************************************************************************


   function Window_width return integer is
   begin
      return Largeur_main;
   end Window_width;

   function Window_height return integer is
   begin
      return Hauteur_main;
   end Window_height;

   -- --------------------------------------------------------------------------------------

   procedure A_droite( objet, reference  : UI_object_ptr;
                       espace_horiz : integer;
                       en_haut : boolean := true ) is
   begin
      if en_haut then
         -- meme haut
         objet.Y := reference.Y;
      else
         -- meme bas
         objet.Y := reference.Y + reference.hauteur - objet.hauteur;
      end if;
      -- l'objet est à droite de la refence + espace
      objet.X := reference.X + reference.largeur + espace_horiz;
   end A_Droite;

   procedure A_gauche( objet, reference  : UI_object_ptr;
                       espace_horiz : integer;
                       en_haut : boolean := true ) is
   begin
      if en_haut then
         -- meme haut
         objet.Y := reference.Y;
      else
         -- meme bas
         objet.Y := reference.Y + reference.hauteur - objet.hauteur;
      end if;
      -- l'objet est à gauche de la référence + espace
      objet.X := reference.X - objet.largeur - espace_horiz;
   end A_gauche;

   procedure Au_dessous( objet, reference  : UI_object_ptr;
                          espace_vert : integer ) is
   begin
      -- cadré à gauche
      objet.X := reference.X;
      -- l'objet est en dessou de la référence + espace
      objet.Y := reference.Y + reference.hauteur + espace_vert;
   end Au_dessous;


   procedure Au_dessus( objet, reference  : UI_object_ptr;
                        espace_vert : integer ) is
   begin
      -- cadré à gauche
      objet.X := reference.X;
      -- l'objet est au dessus de la référence + espace
      objet.Y := reference.Y - objet.Hauteur - espace_vert;
   end Au_dessus;


   procedure Centrer_V( objet, reference  : UI_object_ptr ) is
   begin
      -- l'objet est centré verticalement par rapport à la référence
      objet.Y := reference.Y + (reference.hauteur - objet.hauteur) / 2;
   end Centrer_V;

   procedure Centrer_H( objet, reference  : UI_object_ptr ) is
   begin
      -- l'objet est centré horizontalement par rapport à la référence
      objet.X := reference.X + (reference.largeur - objet.largeur) / 2;
   end Centrer_H;


   function Max( val1, val2 : integer ) return integer is
   begin
      if val1 > val2 then
         return val1;
      else
         return val2;
      end if;
   end Max;

   function Min( val1, val2 : integer) return integer is
   begin
      if val1 < val2 then
         return val1;
      else
         return val2;
      end if;
   end Min;

   -- redispose les objest de l'interface en fonction de la taille de la main window
   procedure Compute_layout is
      obj, ref : Objects_pkg.UI_object_ptr;
      bas, haut : integer;
      client : Win32.Windef.RECT;
      res_bool : Win32.BOOL;
      largeur_window : integer;
      hauteur_window : integer;
      large, droite, decale : integer;
   begin
      --
      -- taille de la "client area"
      res_bool := Win32.Winuser.GetClientRect( Common_types.win_hwnd, TO_PRECT(client'address) );
      largeur_window := Integer(client.right) - Integer(client.left);
      hauteur_window := Integer(client.bottom) - Integer(client.top);
      -- si minimized ne rien faire
      if largeur_window = 0 or hauteur_window = 0 then
         return;
      end if;
      --
      -- Depuis le coin en haut à gauche, en descendant et vers la droite
      --
      -- fader: en haut à gauche
      obj := Object_of( FADER_ID );
      -- test si init fait
      if obj = null then
         return;
      end if;
      obj.X := marge_gauche;
      obj.Y := marge_haut;
      obj.hauteur := hauteur_volume;
      bas := obj.Y + obj.hauteur;
      Objects_pkg.Resize_Fader( obj );
      -- volume : à droite du fader
      ref := obj;
      obj := Object_of( VOLUME_ID );
      A_droite( obj, ref, 3 );
      bas := Max( bas, obj.Y + obj.hauteur );
      -- spectre : à droite du volume
      ref := obj;
      obj := Object_of( SPECTRE_ID );
      A_droite( obj, ref, 5 );
      bas := Max( bas, obj.Y + obj.hauteur );
      -- pitch: à droite du spectre, en haut
      ref := obj;
      obj := Object_of( PITCH_ID );
      A_droite( obj, ref, 5 );
      bas := Max( bas, obj.Y + obj.hauteur );
      -- nom note : sous la frequence
      ref := obj;
      obj := Object_of( NOM_ID );
      Au_dessous( obj, ref, 5 );
      bas := Max( bas, obj.Y + obj.hauteur );
      -- octave : à droite du nom
      ref := obj;
      obj := Object_of( OCT_ID );
      A_droite( obj, ref, 3 );
      bas := Max( bas, obj.Y + obj.hauteur );
      -- label Hz à droite de frequence
      ref := Object_of( PITCH_ID );
      obj := Object_of( FREQ_ID );
      A_droite( obj, ref, 3 );
      bas := Max( bas, obj.Y + obj.hauteur );
      -- version
      ref := Object_of( NOM_ID );
      version_rect.top := Win32.LONG( ref.Y + ref.hauteur + 5);
      version_rect.left := Win32.LONG( ref.X );
      version_rect.right := version_rect.left + 200;
      version_rect.bottom := version_rect.top + 24;
      --
      -- Depuis le haut à droite, en descendant et vers la gauche
      --
      -- bouton aide : en haut à droite
      obj := Object_of( AIDE_ID );
      obj.X := largeur_window - marge_droite - obj.largeur;
      obj.Y := marge_haut;
      bas := Max( bas, obj.Y + obj.hauteur );
      -- bouton param: à gauche du bouton aide
      ref := obj;
      obj := Object_of( PARAM_ID );
      A_gauche( obj, ref, 3 );
      bas := Max( bas, obj.Y + obj.hauteur );
      -- bouton web: sous bouton aide
      ref := Object_of( AIDE_ID );
      obj := Object_of( GOTO_SITE_ID );
      Au_dessous( obj, ref, 3 );
      bas := Max( bas, obj.Y + obj.hauteur );
      -- bouton option: à gauche de web
      ref := obj;
      obj := Object_of( OPTIONS_ID );
      A_gauche( obj, ref, 3 );
      --
      -- ======================================================================
      -- En dessous de tous les précédents, sur la gauche de la fenêtre
      --
      -- bouton (+) zoom V  : en dessous de tous les précédents, à gauche de la fenetre
      obj := Object_of( ZOOM_PLUS_ID );
      obj.Y := bas + 5;
      obj.X := max(largeur_window - marge_droite - obj.largeur,0);
      -- bouton (-) zoom V: sous + zoom V
      ref := obj;
      obj := Object_of( ZOOM_MOINS_ID );
      Au_dessous( obj, ref, 3 );
      -- bouton scroll haut: au dessous de (-) zoom
      ref := obj;
      obj := Object_of( SCROLL_PLUS_ID );
      Au_dessous( obj, ref, 5 );
      --
      -- ======================================================================
      -- Depuis le bas à droite, en remontant et vers la gauche
      --
      -- bouton sortie : en bas à droite
      obj := Object_of( SORTIE_ID );
      obj.X := max(largeur_window - obj.largeur - marge_droite,0);
      obj.Y := max(hauteur_window - obj.hauteur - marge_bas,0);
      -- paroles : à gauche de sortie, même bas
      ref := obj;
      obj := Object_of( TEXTE_ID );
      A_Gauche( obj, ref, 5, false );
      haut := obj.Y;
      --
      -- ---------------------------------------------------
      -- calcule largeur max de label et de fader
      obj := Object_of( LAB_VOL_MEL_ID );
      ref := Object_of( VOL_MELODIE_ID );
      large := Max( obj.largeur, ref.largeur );
      --
      -- label volume melodie : à gauche même haut
      ref := Object_of( TEXTE_ID );
      obj := Object_of( LAB_VOL_MEL_ID );
      A_Gauche( obj, ref, 5 + (large - obj.largeur)/2, en_haut => true );
      --
      -- fader volume melodie: en dessous du label, centré, même bas que paroles
      ref := obj;
      obj := Object_of( VOL_MELODIE_ID );
      Au_dessous( obj, ref, 2 );
      Centrer_H( obj, ref );
      ref := Object_of( TEXTE_ID );
      obj.hauteur := ref.hauteur - (obj.Y - ref.Y);	-- même bas
      Objects_pkg.Resize_Fader( obj );
      --
      -- label volume accompagnement: à gauche de volume melodie
      ref := Object_of( LAB_VOL_MEL_ID );
      obj := Object_of( LAB_VOL_AC_ID );
      A_Gauche( obj, ref, 5 + (large - obj.largeur)/2, en_haut => true );
      --
      -- fader volume accompagnement: sous son label, centré, même hauteur que volume melodie
      ref := obj;
      obj := Object_of( VOL_ACCOMP_ID );
      Au_dessous( obj, ref, 2 );
      Centrer_H( obj, ref );
      ref := Object_of( VOL_MELODIE_ID );
      obj.hauteur := ref.hauteur;
      Objects_pkg.Resize_Fader( obj );
      --
      -- position max à droite de ASCENSEUR_H_ID
      droite := obj.X - ( large - obj.largeur)/2;
      --
      -- num_edit transposition : au dessous de ASCENSEUR_H_ID et de HORIZ_PLUS_ID, à droite
      -- mais comme ils n'ont pas été calculés, on ne prend que leurs hauteurs
      ref := Object_of( ASCENSEUR_H_ID );
      decale := ref.hauteur;
      ref := Object_of( HORIZ_PLUS_ID );
      decale := Max( decale, ref.hauteur );
      --
      ref := Object_of( LAB_VOL_AC_ID );
      obj := Object_of( NUM_TRANS_ID );
      obj.X := droite - obj.largeur - 5;
      obj.Y := ref.Y + decale + 2;
      --
      -- num-edit décalage : en dessous de num-edit transpo
      ref := obj;
      obj := Object_of( NUM_DECAL_ID );
      Au_dessous( obj, ref, 5 );
      --
      -- Label decalage: à gauche de decalage
      ref := Object_of( NUM_DECAL_ID );
      obj := Object_of( LAB_DECAL_ID );
      A_Gauche( obj, ref, 1, false  );
      Centrer_V( obj, ref );
      --
      -- Label transposition: à gauche de transposition
      ref := Object_of( NUM_TRANS_ID );
      obj := Object_of( LAB_TRANS_ID );
      A_Gauche( obj, ref, 1, false  );
      Centrer_V( obj, ref );
      --
      -- compteur : à gauche de label transpo, même haut
      ref := Object_of( LAB_TRANS_ID );
      obj := Object_of( COMPTEUR_ID );
      A_gauche( obj, ref, 5, en_haut => true );
      --
      -- mesure de fin : à gauche de compteur même haut
      ref := obj;
      obj := Object_of( MESURE_FIN_ID );
      A_gauche( obj, ref, 5, en_haut => true );
      -- label fin: à gauche
      ref := obj;
      obj := Object_of( FIN_PLAYER_ID );
      A_gauche( obj, ref, 1 );
      Centrer_V( obj, ref );
      -- mesure de debut : à gauche de label fin
      ref := obj;
      obj := Object_of( MESURE_DEBUT_ID );
      A_gauche( obj, ref, 3 );
      Centrer_V( obj, ref );
      -- label debut
      ref := obj;
      obj := Object_of( DEBUT_PLAYER_ID );
      A_Gauche( obj, ref, 1 );
      Centrer_V( obj, ref );
      -- bistable de boucle: à gauche
      ref := obj;
      obj := Object_of( LOOP_ID );
      A_gauche( obj, ref, 3 );
      Centrer_V( obj, ref );

      -- bouton stop: à gauche de compteur, même bas
      ref := Object_of( COMPTEUR_ID );
      obj := Object_of( STOP_ID );
      A_gauche( obj, ref, 3, false );
      -- bouton play : à gauche de stop
      ref := obj;
      obj := Object_of( PLAY_ID );
      A_gauche( obj, ref, 3);
      -- bouton pause: à gauche de stop
      ref := obj;
      obj := Object_of( PAUSE_ID );
      A_gauche( obj, ref, 3);
      -- bouton load : à gauche de pause
      ref := obj;
      obj := Object_of( LOAD_ID );
      A_gauche( obj, ref, 3);

      -- nom fichier: au dessous de LOAD jusqu'à la gauche de vol_a
      ref := obj;
      obj := Object_of( NOM_FIC_ID );
      Au_dessous( obj, ref, 3 );
      ref := Object_of( VOL_ACCOMP_ID );
      obj.largeur := ref.X - obj.X - 5;	-- calcul largeur de la zone
      Objects_pkg.Compute_inner( obj );	-- calcule la taille de l'intérieur
      --
      -- =======================================================
      -- A partir du bord gauche, à la même hauteur que le plus haut du bas
      -- en allant vers la droite
      --
      -- bouton (+) speed : au dessus de load à gauche
      obj := Object_of( HORIZ_PLUS_ID );
      obj.X := marge_gauche;
      obj.Y := haut;
      -- bouton (-) speed : à droite de + speed
      ref := obj;
      obj := Object_of( HORIZ_MOINS_ID );
      A_droite( obj, ref, 3 );
      -- ascenseur entre label accompagnement et le (-) speed
      ref := obj;
      obj := Object_of( ASCENSEUR_H_ID );
      A_droite( obj, ref, 3 );
      Centrer_V( obj, ref );		-- centrage vertical
      obj.largeur := droite - ref.X - ref.largeur  - 5;
      haut := min( haut, obj.y );
      --
      -- bouton freeze: en dessous de (+) speed
      obj := Object_of( FREEZE_ID );
      ref := Object_of( HORIZ_PLUS_ID );
      Au_dessous( obj, ref, 2 );
      -- bouton clear: à droite de freeze
      ref := obj;
      obj := Object_of( CLEAR_ID );
      A_droite( obj, ref, 3 );
      -- bouton record: en dessous de freeze
      obj := Object_of( REC_ID );
      Au_dessous( obj, ref, 2);
      --
      -- ======================================================================================
      -- dans l'espace restant
      --
      -- barre des mesures: au dessus de tout le bas, toute la largeur disponible
      obj := Object_of( MESURES_ID );
      obj.X := marge_gauche;
      obj.Y := max(haut - obj.hauteur - 2,0);
      ref := Object_of( ZOOM_PLUS_ID );
      obj.largeur := max(ref.X - obj.X - 3,0);		-- étiré en longeur, hauteur inchangé
      -- score: Y = (+) zoom, à gauche, largeur max, hauteur max
      obj := Object_of( SCORE_ID );
      obj.X := marge_gauche;
      obj.Y := ref.Y;
      obj.largeur := max(ref.X - obj.X - 3,0);
      ref := Object_of( MESURES_ID );
      obj.hauteur := max(ref.Y - obj.Y - 3,0);
      -- bouton scroll bas : sous + zoom, même bas que spectre
      ref := obj;
      obj := Object_of( SCROLL_MOINS_ID );
      A_Droite( obj, ref, 3, false );
      -- ascenseur vertical
      obj := Object_of( ASCENSEUR_V_ID );
      ref := Object_of( SCROLL_PLUS_ID );
      Au_dessous( obj, ref, 2 );
      Centrer_H( obj, ref );
      ref := Object_of( SCROLL_MOINS_ID );
      obj.hauteur := max(ref.Y - obj.Y - 2,0);
      -- bouton centrage: meme X que ZOOM_PLUS, centrer en Y sur MESURES_ID
      obj := Object_of( CENTRER_ID );
      ref := Object_of( ZOOM_PLUS_ID );
      obj.X := ref.X;
      ref := Object_of( MESURES_ID );
      Centrer_V( obj, ref );
      --
   end Compute_layout;


   procedure Resize_window is
      main_rect : Win32.Windef.RECT;
      res_bool : Win32.BOOL;
      new_width, new_height : integer;
   begin
      -- lecture nouvelle taille de fenêtre
      res_bool := Win32.Winuser.GetWindowRect( Common_types.Win_hwnd, TO_PRECT(main_rect'address) );
      new_width := Integer(main_rect.right) - Integer(main_rect.left);
      new_height := Integer(main_rect.bottom) - Integer(main_rect.top);
      -- assure que la window n'est pas plus petite que le minimum
      if new_width < min_window_width then
         Largeur_main := min_window_width;
      else
         Largeur_main := new_width;
      end if;
      if new_height < min_window_height then
         Hauteur_main := min_window_height;
      else
         Hauteur_main := new_height;
      end if;
      -- Calcule la nouvelle disposition
      Compute_Layout;
      -- redimensionne la fenêtre principale
      res_bool := Win32.Winuser.MoveWindow( Common_types.Win_hwnd, Win32.INT(main_rect.left),
                                            Win32.INT(main_rect.top),
                                            Win32.INT(Largeur_main), Win32.INT(Hauteur_main), 1 );
      -- recréée le background
      Bitmap_pkg.Create_background;
      -- Déplace et redimensionne tous les objets
      Objects_pkg.Resize_all;
   end Resize_window;

   -- ***************************************************************************************


   procedure Init_layout is
      obj     : UI_object_ptr;
      res_int : Win32.INT;
      old     : Win32.Windef.HGDIOBJ;
      old_color : Win32.Windef.COLORREF;
   begin
      --
      -- création des objets d'interface
      --
      -- Réglage du volume du microphone
      Objects_pkg.Create_Fader( Id => FADER_ID,
                                horiz  => false,
                                resizeable => false,
                                fond_1 => FADER_FOND_1_ID,
                                fond_2 => FADER_FOND_2_ID,
                                fond_3 => FADER_FOND_3_ID,
                                curs_1 => FADER_CURSEUR_OFF_ID,
                                curs_2 => FADER_CURSEUR_ON_ID,
                                curs_3 => FADER_CURSEUR_OFF_ID,	-- pas utilisé
                                min    => 0,
                                max    => 65535,
                                offset_1 => Skins_pkg.fader_diff_haut,
                                offset_2 => Skins_pkg.fader_diff_bas );
      -- Volume de la mélodie
      Objects_pkg.Create_Fader( Id => VOL_MELODIE_ID,
                                horiz  => false,
                                resizeable => false,
                                fond_1 => FADER_FOND_1_ID,
                                fond_2 => FADER_FOND_2_ID,
                                fond_3 => FADER_FOND_3_ID,
                                curs_1 => FADER_CURSEUR_OFF_ID,
                                curs_2 => FADER_CURSEUR_ON_ID,
                                curs_3 => FADER_CURSEUR_OFF_ID,	-- pas utilisé
                                min    => 0,
                                max    => 100,
                                offset_1 => Skins_pkg.fader_diff_haut,
                                offset_2 => Skins_pkg.fader_diff_bas );
      -- Volume de l'accompagnement
      Objects_pkg.Create_Fader( Id => VOL_ACCOMP_ID,
                                horiz  => false,
                                resizeable => false,
                                fond_1 => FADER_FOND_1_ID,
                                fond_2 => FADER_FOND_2_ID,
                                fond_3 => FADER_FOND_3_ID,
                                curs_1 => FADER_CURSEUR_OFF_ID,
                                curs_2 => FADER_CURSEUR_ON_ID,
                                curs_3 => FADER_CURSEUR_OFF_ID,	-- pas utilisé
                                min    => 0,
                                max    => 100,
                                offset_1 => Skins_pkg.fader_diff_haut,
                                offset_2 => Skins_pkg.fader_diff_bas );
      -- Ascenseur Horizontal
      Objects_pkg.Create_Fader( Id => ASCENSEUR_H_ID,
                                horiz  => true,
                                resizeable => true,
                                fond_1 => ASC_H_GAUCHE_ID,
                                fond_2 => ASC_H_CENTRE_ID,
                                fond_3 => ASC_H_DROITE_ID,
                                curs_1 => ASC_H_CURS_OFF_ID,
                                curs_2 => ASC_H_CURS_ON_ID,
                                curs_3 => ASC_H_CURS_INVAL_ID,
                                min    => 0,
                                max    => 1000,
                                offset_1 => Skins_pkg.asc_H_diff_gauche,
                                offset_2 => Skins_pkg.asc_H_diff_droite );
      -- Ascenseur Vertical
      Objects_pkg.Create_Fader( Id => ASCENSEUR_V_ID,
                                horiz  => false,
                                resizeable => true,
                                fond_1 => ASC_V_HAUT_ID,
                                fond_2 => ASC_V_CENTRE_ID,
                                fond_3 => ASC_V_BAS_ID,
                                curs_1 => ASC_V_CURS_OFF_ID,
                                curs_2 => ASC_V_CURS_ON_ID,
                                curs_3 => ASC_V_CURS_INVAL_ID,
                                min    => 0,
                                max    => 1000,
                                offset_1 => Skins_pkg.asc_V_diff_haut,
                                offset_2 => Skins_pkg.asc_V_diff_bas );
      --
      -- displays
      --
      -- volume
      Objects_pkg.Create_Display( Id => VOLUME_ID,
                            framed       => true,
                            frames       => default_frame,
                            inner_width  => largeur_volume,
                            inner_height => hauteur_volume,
                            brush        => Resources_pkg.brush_noire,
                            resizeable   => false );
      -- spectre
      Objects_pkg.Create_Display( Id => SPECTRE_ID,
                            framed       => true,
                            frames       => default_frame,
                            inner_width  => largeur_spectre,
                            inner_height => hauteur_spectre,
                            brush        => Resources_pkg.brush_noire,
                            resizeable   => false );
      -- score
      Objects_pkg.Create_Display( Id => SCORE_ID,
                            framed       => true,
                            frames       => default_frame,
                            inner_width  => min_largeur_score,
                            inner_height => min_hauteur_score,
                            brush        => Resources_pkg.brush_blanche,
                            resizeable   => true );
      -- mesures
      Objects_pkg.Create_Display( Id => MESURES_ID,
                            framed       => true,
                            frames       => default_frame,
                            inner_width  => min_largeur_score,
                            inner_height => hauteur_mesures,
                            brush        => Resources_pkg.brush_blanche,
                            resizeable   => true );
      obj := Objects_pkg.Object_of( MESURES_ID );
      res_int := Win32.Wingdi.SetBkMode( obj.memDC, Win32.Wingdi.TRANSPARENT );
      old := Win32.Wingdi.SelectObject( obj.memDC, Resources_pkg.Text_font );
      -- paroles
      Objects_pkg.Create_Display( Id => TEXTE_ID,
                            framed       => true,
                            frames       => thin_frame,
                            inner_width  => largeur_texte,
                            inner_height => hauteur_texte,
                            brush        => Resources_pkg.brush_beige,
                            resizeable   => false );
      -- compteur et status du player
      Objects_pkg.Create_Display( Id => COMPTEUR_ID,
                            framed       => true,
                            frames       => default_frame,
                            inner_width  => largeur_compteur,
                            inner_height => hauteur_compteur,
                            brush        => Resources_pkg.brush_noire,
                            resizeable   => false );
      obj := Objects_pkg.Object_of( COMPTEUR_ID );
      res_int := Win32.Wingdi.SetBkMode( obj.memDC, Win32.Wingdi.TRANSPARENT );
      old := Win32.Wingdi.SelectObject( obj.memDC, Resources_pkg.Text_font );
      old_color := Win32.Wingdi.SetTextColor( obj.memDC, 16#00FFFFFF# );      -- couleur blanc
      --
      -- Affichages textes
      --
      Objects_pkg.Create_Text( Id => PITCH_ID,
                            framed       => true,
                            frames       => default_frame,
                            inner_width  => largeur_pitch,
                            inner_height => hauteur_pitch,
                            max_char     => 7,
                            font         => Resources_pkg.Small_font,
                            color        => 0,
                            brush        => Resources_pkg.brush_beige,
                            cadrage      => droite );
      -- nom de note (do, ré, mi, ... )
      Objects_pkg.Create_Text( Id => NOM_ID,
                            framed       => true,
                            frames       => default_frame,
                            inner_width  => largeur_nom,
                            inner_height => hauteur_nom,
                            max_char     => 4,
                            font         => Resources_pkg.Big_font,
                            color        => 0,
                            brush        => Resources_pkg.brush_beige,
                            cadrage      => droite );
      -- octave, 1 chiffre
      Objects_pkg.Create_Text( Id => OCT_ID,
                            framed       => true,
                            frames       => default_frame,
                            inner_width  => largeur_oct,
                            inner_height => hauteur_nom,	-- même hauteur que pour le nom de note
                            max_char     => 1,
                            font         => Resources_pkg.Big_font,
                            color        => 0,
                            brush        => Resources_pkg.brush_beige,
                            cadrage      => gauche );
      -- nom du fichier MIDI
      Objects_pkg.Create_Text( Id => NOM_FIC_ID,
                            framed       => true,
                            frames       => default_frame,
                            inner_width  => 300,			-- valeur bidon, recalculé par Layout
                            inner_height => hauteur_nom_fic,
                            max_char     => 128,
                            font         => Resources_pkg.Small_Small_font,
                            color        => 0,
                            brush        => Resources_pkg.brush_beige,
                            cadrage      => gauche );
      -- Label "Hz"
      Objects_pkg.Create_Text( Id => FREQ_ID,
                            framed       => false,
                            frames       => default_frame,
                            inner_width  => largeur_Hz,
                            inner_height => hauteur_Hz,
                            max_char     => 2,
                            font         => Skins_pkg.Label_font,
                            color        => Skins_pkg.label_color,
                            brush        => N_A,
                            cadrage      => gauche );
      Objects_pkg.Set_Text( FREQ_ID, Intl.Freq_string );
      -- Label transposition
      Objects_pkg.Create_Text( Id => LAB_TRANS_ID,
                            framed       => false,
                            frames       => default_frame,
                            inner_width  => largeur_Hz,
                            inner_height => hauteur_Hz,
                            max_char     => Intl.Transpo'length,
                            font         => Resources_pkg.Small_small_font,
                            color        => Skins_pkg.label_color,
                            brush        => N_A,
                            cadrage      => gauche );
      Objects_pkg.Set_Text( LAB_TRANS_ID, Intl.Transpo );
      Objects_pkg.Adjuste_text_to_string( LAB_TRANS_ID );
      --
      -- Label décalage d'octave
      Objects_pkg.Create_Text( Id => LAB_DECAL_ID,
                            framed       => false,
                            frames       => default_frame,
                            inner_width  => largeur_Hz,
                            inner_height => hauteur_Hz,
                            max_char     => Intl.Decalage'length,
                            font         => Resources_pkg.Small_small_font,
                            color        => Skins_pkg.label_color,
                            brush        => N_A,
                            cadrage      => gauche );
      Objects_pkg.Set_Text( LAB_DECAL_ID, Intl.Decalage );
      Objects_pkg.Adjuste_text_to_string( LAB_DECAL_ID );
      --
      -- label volume melodie
      Objects_pkg.Create_Text( Id => LAB_VOL_MEL_ID,
                            framed       => false,
                            frames       => default_frame,
                            inner_width  => largeur_Hz,
                            inner_height => hauteur_Hz,
                            max_char     => Intl.Vol_melodie'length,
                            font         => Resources_pkg.Small_small_font,
                            color        => Skins_pkg.label_color,
                            brush        => N_A,
                            cadrage      => gauche );
      Objects_pkg.Set_Text( LAB_VOL_MEL_ID, Intl.Vol_melodie );
      Objects_pkg.Adjuste_text_to_string( LAB_VOL_MEL_ID );
      --
      -- label volume accompagnement
      Objects_pkg.Create_Text( Id => LAB_VOL_AC_ID,
                            framed       => false,
                            frames       => default_frame,
                            inner_width  => largeur_Hz,
                            inner_height => hauteur_Hz,
                            max_char     => Intl.Vol_accomp'length,
                            font         => Resources_pkg.Small_small_font,
                            color        => Skins_pkg.label_color,
                            brush        => N_A,
                            cadrage      => gauche );
      Objects_pkg.Set_Text( LAB_VOL_AC_ID, Intl.Vol_accomp );
      Objects_pkg.Adjuste_text_to_string( LAB_VOL_AC_ID );
      --
      --
      -- Boutons text
      --
      Objects_pkg.Create_Button_Text( Id => SORTIE_ID,
                            fond_off   => BUTTON_OFF_ID,
                            fond_on    => BUTTON_ON_ID,
                            fond_clic  => BUTTON_CLIC_ID,
                            fond_inval => BUTTON_INVAL_ID,
                            text       => Intl.bouton_sortie_txt,
                            font       => Skins_pkg.Button_font,
                            color      => Skins_pkg.button_color,
                            offset_x   => Skins_pkg.large_but_x,
                            offset_y   => Skins_pkg.large_but_y );
      --
      Objects_pkg.Create_Button_Text( Id => AIDE_ID,
                            fond_off   => BUTTON_OFF_ID,
                            fond_on    => BUTTON_ON_ID,
                            fond_clic  => BUTTON_CLIC_ID,
                            fond_inval => BUTTON_INVAL_ID,
                            text       => Intl.bouton_aide_txt,
                            font       => Skins_pkg.Button_font,
                            color      => Skins_pkg.button_color,
                            offset_x   => Skins_pkg.large_but_x,
                            offset_y   => Skins_pkg.large_but_y );
      --
      Objects_pkg.Create_Button_Text( Id => GOTO_SITE_ID,
                            fond_off   => BUTTON_OFF_ID,
                            fond_on    => BUTTON_ON_ID,
                            fond_clic  => BUTTON_CLIC_ID,
                            fond_inval => BUTTON_INVAL_ID,
                            text       => Intl.bouton_site_txt,
                            font       => Skins_pkg.Button_font,
                            color      => Skins_pkg.button_color,
                            offset_x   => Skins_pkg.large_but_x,
                            offset_y   => Skins_pkg.large_but_y );
      --
      Objects_pkg.Create_Button_Text( Id => PARAM_ID,
                            fond_off   => BUTTON_OFF_ID,
                            fond_on    => BUTTON_ON_ID,
                            fond_clic  => BUTTON_CLIC_ID,
                            fond_inval => BUTTON_INVAL_ID,
                            text       => Intl.bouton_param_txt,
                            font       => Skins_pkg.Button_font,
                            color      => Skins_pkg.button_color,
                            offset_x   => Skins_pkg.large_but_x,
                            offset_y   => Skins_pkg.large_but_y );
      --
      Objects_pkg.Create_Button_Text( Id => REC_ID,
                            fond_off   => BUT_REC_OFF_ID,
                            fond_on    => BUT_REC_ON_ID,
                            fond_clic  => BUT_REC_CLIC_ID,
                            fond_inval => BUT_REC_INVAL_ID,
                            text       => Intl.bouton_record_txt,
                            font       => Skins_pkg.Button_font,
                            color      => Skins_pkg.button_color,
                            offset_x   => Skins_pkg.large_but_x,
                            offset_y   => Skins_pkg.large_but_y );
      -- Freeze
      Objects_pkg.Create_Button_Text( Id => FREEZE_ID,
                            fond_off   => BUTTON_OFF_ID,
                            fond_on    => BUTTON_ON_ID,
                            fond_clic  => BUTTON_CLIC_ID,
                            fond_inval => BUTTON_INVAL_ID,
                            text       => Intl.bouton_freeze_txt,
                            font       => Skins_pkg.Button_font,
                            color      => Skins_pkg.button_color,
                            offset_x   => Skins_pkg.large_but_x,
                            offset_y   => Skins_pkg.large_but_y );
      -- Clear
      Objects_pkg.Create_Button_Text( Id => CLEAR_ID,
                            fond_off   => BUTTON_OFF_ID,
                            fond_on    => BUTTON_ON_ID,
                            fond_clic  => BUTTON_CLIC_ID,
                            fond_inval => BUTTON_INVAL_ID,
                            text       => Intl.bouton_clear_txt,
                            font       => Skins_pkg.Button_font,
                            color      => Skins_pkg.button_color,
                            offset_x   => Skins_pkg.large_but_x,
                            offset_y   => Skins_pkg.large_but_y );
      -- Options
      Objects_pkg.Create_Button_Text( Id => OPTIONS_ID,
                            fond_off   => BUTTON_OFF_ID,
                            fond_on    => BUTTON_ON_ID,
                            fond_clic  => BUTTON_CLIC_ID,
                            fond_inval => BUTTON_INVAL_ID,
                            text       => Intl.bouton_option_txt,
                            font       => Skins_pkg.Button_font,
                            color      => Skins_pkg.button_color,
                            offset_x   => Skins_pkg.large_but_x,
                            offset_y   => Skins_pkg.large_but_y );
      --
      -- Petits boutons
      --
      Objects_pkg.Create_Button_Bitmap( Id => ZOOM_PLUS_ID,
                            fond_off   => PET_BUT_OFF_ID,
                            fond_on    => PET_BUT_ON_ID,
                            fond_clic  => PET_BUT_CLIC_ID,
                            fond_inval => PET_BUT_INVAL_ID,
                            icone      => BMAP_PLUS_ID,
                            offset_x   => Skins_pkg.pet_but_x,
                            offset_y   => Skins_pkg.pet_but_y );
      --
      Objects_pkg.Create_Button_Bitmap( Id => ZOOM_MOINS_ID,
                            fond_off   => PET_BUT_OFF_ID,
                            fond_on    => PET_BUT_ON_ID,
                            fond_clic  => PET_BUT_CLIC_ID,
                            fond_inval => PET_BUT_INVAL_ID,
                            icone      => BMAP_MOINS_ID,
                            offset_x   => Skins_pkg.pet_but_x,
                            offset_y   => Skins_pkg.pet_but_y );
      -- Zoom horizontal
      Objects_pkg.Create_Button_Bitmap( Id => HORIZ_PLUS_ID,
                            fond_off   => PET_BUT_OFF_ID,
                            fond_on    => PET_BUT_ON_ID,
                            fond_clic  => PET_BUT_CLIC_ID,
                            fond_inval => PET_BUT_INVAL_ID,
                            icone      => BMAP_PLUS_ID,
                            offset_x   => Skins_pkg.pet_but_x,
                            offset_y   => Skins_pkg.pet_but_y );
      --
      Objects_pkg.Create_Button_Bitmap( Id => HORIZ_MOINS_ID,
                            fond_off   => PET_BUT_OFF_ID,
                            fond_on    => PET_BUT_ON_ID,
                            fond_clic  => PET_BUT_CLIC_ID,
                            fond_inval => PET_BUT_INVAL_ID,
                            icone      => BMAP_MOINS_ID,
                            offset_x   => Skins_pkg.pet_but_x,
                            offset_y   => Skins_pkg.pet_but_y );
      -- Flêches haut et bas
      Objects_pkg.Create_Button_Bitmap( Id => SCROLL_PLUS_ID,
                            fond_off   => PET_BUT_OFF_ID,
                            fond_on    => PET_BUT_ON_ID,
                            fond_clic  => PET_BUT_CLIC_ID,
                            fond_inval => PET_BUT_INVAL_ID,
                            icone      => BMAP_TRI_HAUT_ID,
                            offset_x   => Skins_pkg.pet_but_x,
                            offset_y   => Skins_pkg.pet_but_y );
      --
      Objects_pkg.Create_Button_Bitmap( Id => SCROLL_MOINS_ID,
                            fond_off   => PET_BUT_OFF_ID,
                            fond_on    => PET_BUT_ON_ID,
                            fond_clic  => PET_BUT_CLIC_ID,
                            fond_inval => PET_BUT_INVAL_ID,
                            icone      => BMAP_TRI_BAS_ID,
                            offset_x   => Skins_pkg.pet_but_x,
                            offset_y   => Skins_pkg.pet_but_y );
      --
      Objects_pkg.Create_Button_Bitmap( Id => CENTRER_ID,
                            fond_off   => PET_BUT_OFF_ID,
                            fond_on    => PET_BUT_ON_ID,
                            fond_clic  => PET_BUT_CLIC_ID,
                            fond_inval => PET_BUT_INVAL_ID,
                            icone      => BMAP_CIBLE_ID,
                            offset_x   => Skins_pkg.pet_but_x,
                            offset_y   => Skins_pkg.pet_but_y );
      --
      -- boutons moyen
      --
      Objects_pkg.Create_Button_Bitmap( Id => LOAD_ID,
                            fond_off   => MOY_BUT_OFF_ID,
                            fond_on    => MOY_BUT_ON_ID,
                            fond_clic  => MOY_BUT_CLIC_ID,
                            fond_inval => MOY_BUT_INVAL_ID,
                            icone      => BMAP_LOAD_ID,
                            offset_x   => Skins_pkg.med_but_x,
                            offset_y   => Skins_pkg.med_but_y );
      --
      Objects_pkg.Create_Button_Bitmap( Id => PLAY_ID,
                            fond_off   => MOY_BUT_OFF_ID,
                            fond_on    => MOY_BUT_ON_ID,
                            fond_clic  => MOY_BUT_CLIC_ID,
                            fond_inval => MOY_BUT_INVAL_ID,
                            icone      => BMAP_PLAY_ID,
                            offset_x   => Skins_pkg.med_but_x,
                            offset_y   => Skins_pkg.med_but_y );
      --
      Objects_pkg.Create_Button_Bitmap( Id => STOP_ID,
                            fond_off   => MOY_BUT_OFF_ID,
                            fond_on    => MOY_BUT_ON_ID,
                            fond_clic  => MOY_BUT_CLIC_ID,
                            fond_inval => MOY_BUT_INVAL_ID,
                            icone      => BMAP_STOP_ID,
                            offset_x   => Skins_pkg.med_but_x,
                            offset_y   => Skins_pkg.med_but_y );
      --
      Objects_pkg.Create_Button_Bitmap( Id => PAUSE_ID,
                            fond_off   => MOY_BUT_OFF_ID,
                            fond_on    => MOY_BUT_ON_ID,
                            fond_clic  => MOY_BUT_CLIC_ID,
                            fond_inval => MOY_BUT_INVAL_ID,
                            icone      => BMAP_PAUSE_ID,
                            offset_x   => Skins_pkg.med_but_x,
                            offset_y   => Skins_pkg.med_but_y );
      -- bistable lecture en boucle
      Objects_pkg.Create_bistable( Id => LOOP_ID,
                            fond_off  => LOOP_OFF_ID,
                            fond_on   => LOOP_ON_ID );
      -- num-edit premiere mesure à joouer
      Objects_pkg.Create_Num_edit( Id => MESURE_DEBUT_ID,
                              max_digit => 3,
                              font      => Resources_pkg.Small_small_font,
                              color     => 0,
                              brush     => Resources_pkg.brush_beige,
                              framed    => true,
                              frames    => thin_frame,
                              min       => 1,
                              max       => 1,
                              value     => 1 );
      -- num-edit derniere mesure à jouer
      Objects_pkg.Create_Num_edit( Id => MESURE_FIN_ID,
                              max_digit => 3,
                              font      => Resources_pkg.Small_small_font,
                              color     => 0,
                              brush     => Resources_pkg.brush_beige,
                              framed    => true,
                              frames    => thin_frame,
                              min       => 1,
                              max       => 1,
                              value     => 1 );
      --
      Objects_pkg.Create_Text( Id => DEBUT_PLAYER_ID,
                            framed       => false,
                            frames       => default_frame,
                            inner_width  => largeur_Hz,
                            inner_height => hauteur_Hz,
                            max_char     => Intl.debut_player'length,
                            font         => Resources_pkg.Small_small_font,
                            color        => Skins_pkg.label_color,
                            brush        => N_A,
                            cadrage      => gauche );
      Objects_pkg.Set_Text( DEBUT_PLAYER_ID, Intl.debut_player );
      Objects_pkg.Adjuste_text_to_string( DEBUT_PLAYER_ID );
      --
      Objects_pkg.Create_Text( Id => FIN_PLAYER_ID,
                            framed       => false,
                            frames       => default_frame,
                            inner_width  => largeur_Hz,
                            inner_height => hauteur_Hz,
                            max_char     => Intl.fin_player'length,
                            font         => Resources_pkg.Small_small_font,
                            color        => Skins_pkg.label_color,
                            brush        => N_A,
                            cadrage      => gauche );
      Objects_pkg.Set_Text( FIN_PLAYER_ID, Intl.fin_player );
      Objects_pkg.Adjuste_text_to_string( FIN_PLAYER_ID );
      --
      -- num-edit decalage
      Objects_pkg.Create_Num_edit( Id => NUM_DECAL_ID,
                              max_digit => 3,	-- 2 utiles, le 3eme pour le centrage avec celui de dessous
                              font      => Resources_pkg.Small_small_font,
                              color     => 0,
                              brush     => Resources_pkg.brush_beige,
                              framed    => true,
                              frames    => thin_frame,
                              min       => -4,
                              max       => 4,
                              value     => 0 );
      -- num-edit transposition
      Objects_pkg.Create_Num_edit( Id => NUM_TRANS_ID,
                              max_digit => 3,	-- 2 digit plus le signe = 3
                              font      => Resources_pkg.Small_small_font,
                              color     => 0,
                              brush     => Resources_pkg.brush_beige,
                              framed    => true,
                              frames    => thin_frame,
                              min       => -11,
                              max       => 11,
                              value     => 0 );

      -- calcule leur disposition sur la main window
      Resize_window;
   end Init_Layout;


   -- Appelé après un chargement de skin
--     procedure Compute_new_geometry is
--     begin
--        -- recalcule tous les paramètres dépendant des bitmaps
--        Objects_pkg.Change_all_bitmaps;
--        --
--        -- Paramètres spécifiques à chaque objet
--        --
--        -- fader
--        Objects_pkg.Set_Fader_Geometry( FADER_ID, Skins_pkg.fader_diff_haut, Skins_pkg.fader_diff_bas );
--        Objects_pkg.Set_Fader_Geometry( VOL_MELODIE_ID, Skins_pkg.fader_diff_haut, Skins_pkg.fader_diff_bas );
--        Objects_pkg.Set_Fader_Geometry( VOL_ACCOMP_ID, Skins_pkg.fader_diff_haut, Skins_pkg.fader_diff_bas );
--        -- ascenseur H
--        Objects_pkg.Set_Fader_Geometry( ASCENSEUR_H_ID, Skins_pkg.asc_H_diff_gauche, Skins_pkg.asc_H_diff_droite );
--        -- ascenseur V
--        Objects_pkg.Set_Fader_Geometry( ASCENSEUR_V_ID, Skins_pkg.asc_V_diff_haut, Skins_pkg.asc_V_diff_bas );
--        -- grands boutons
--        for Id in obj_id_type range SORTIE_ID..OPTIONS_ID loop
--           Objects_pkg.Set_button_rect( Id, Skins_pkg.large_but_x, Skins_pkg.large_but_y );
--           Objects_pkg.Set_font( Id, Skins_pkg.Button_font );
--           Objects_pkg.Set_Color( Id, Skins_pkg.button_color );
--        end loop;
--        -- moyens boutons
--        for Id in obj_id_type range LOAD_ID..STOP_ID loop
--           Objects_pkg.Set_button_rect( Id, Skins_pkg.med_but_x, Skins_pkg.med_but_y );
--        end loop;
--        -- petits boutons
--        for Id in obj_id_type range ZOOM_PLUS_ID..CENTRER_ID loop
--           Objects_pkg.Set_button_rect( Id, Skins_pkg.pet_but_x, Skins_pkg.pet_but_y );
--        end loop;
--        -- les labels
--        Objects_pkg.Set_font( FREQ_ID, Skins_pkg.Label_font );	-- Label "Hz"
--        for Id in obj_id_type range FREQ_ID..LAB_VOL_AC_ID loop
--           Objects_pkg.Set_Color( Id, Skins_pkg.Label_color );
--        end loop;
--        --
--     end Compute_new_geometry;

end Layout_pkg;

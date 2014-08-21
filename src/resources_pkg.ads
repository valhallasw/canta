with System;
with Conversions;
with Win32.Windef;
with Win32.Wingdi;

package Resources_pkg is

   -- Identifier child windows ****************************

   type obj_id_type is (

   NULL_ID,		-- pour le main program

   -- Reglages par curseurs
   FADER_ID,		-- réglage volume microphone
   VOL_MELODIE_ID,	-- volume de la mélodie
   VOL_ACCOMP_ID,	-- volume de l'accompagnement

   -- Affichages
   VOLUME_ID,		-- affichage du volume
   SPECTRE_ID,		-- affichage du spectre
   PITCH_ID,		-- fréquence
   NOM_ID,		-- nom de la note : Do, Ré, ...
   OCT_ID,		-- octave
   SCORE_ID,		-- affichage des notes
   TEXTE_ID,		-- paroles du fichier MIDI
   COMPTEUR_ID,		-- compteur et Statut MIDI
   NOM_FIC_ID,		-- nom du fichier MIDI
   MESURES_ID,		-- numéro des mesures du fichier MIDI

   -- labels fixes
   FREQ_ID,		-- label "Hz"
   DEBUT_PLAYER_ID,	-- "début:"
   FIN_PLAYER_ID,	-- "fin:"
   LAB_DECAL_ID,	-- "O:"
   LAB_TRANS_ID,	-- 'T:"
   LAB_VOL_MEL_ID,	-- label volume de la mélodie: "M"
   LAB_VOL_AC_ID,	-- label volume de l'accompagnement : "A"

   -- grands boutons avec textes
   SORTIE_ID,
   AIDE_ID,
   GOTO_SITE_ID,
   PARAM_ID,
   REC_ID,
   FREEZE_ID,
   CLEAR_ID,
   OPTIONS_ID,

   -- boutons moyens avec icones
   LOAD_ID,
   PLAY_ID,
   PAUSE_ID,
   STOP_ID,

   -- petits boutons avec icones
   ZOOM_PLUS_ID,
   ZOOM_MOINS_ID,
   SCROLL_PLUS_ID,
   SCROLL_MOINS_ID,
   HORIZ_PLUS_ID,
   HORIZ_MOINS_ID,
   CENTRER_ID,

   -- acenceurs
   ASCENSEUR_H_ID,
   ASCENSEUR_V_ID,

   -- transposition et decalage
   NUM_DECAL_ID,	-- décalage
   NUM_TRANS_ID,	-- transposition

   -- Debut, fin et loop du player
   LOOP_ID,		-- bistable lecture en boucle
   MESURE_DEBUT_ID,	-- numero de la mesure de début
   MESURE_FIN_ID	-- numero de la mesure de fin
   );

   -- ************************************************************
   -- identifiant des bitmaps
   -- IMPORTANT:
   -- l'ordre des identifiants ne doit pas être changé car Load_Skin utilise la fonction 'succ
   -- pour associer les bitmap avec leur identifiant

   type bmap_id_type is (
      -- null quand pas utilisé
      NULL_ID,
      -- le fond de la main window
      MAIN_BACKGROUND_ID,
      -- cadre de la main windows
      MAIN_CHG_ID, MAIN_BH_ID, MAIN_CHD_ID, MAIN_BD_ID, MAIN_CBD_ID, MAIN_BB_ID, MAIN_CBG_ID, MAIN_BG_ID,
      -- cadre des display
      DISP_CHG_ID, DISP_BH_ID, DISP_CHD_ID, DISP_BD_ID, DISP_CBD_ID, DISP_BB_ID, DISP_CBG_ID, DISP_BG_ID,
      -- cadre fin
      THIN_CHG_ID, THIN_BH_ID, THIN_CHD_ID, THIN_BD_ID,  THIN_CBD_ID, THIN_BB_ID, THIN_CBG_ID, THIN_BG_ID,
      -- le Fader: haut, milieu, bas, curseur On et Off
      FADER_FOND_1_ID, FADER_FOND_2_ID, FADER_FOND_3_ID, FADER_CURSEUR_OFF_ID, FADER_CURSEUR_ON_ID,
      -- fond des boutons
      BUTTON_OFF_ID, BUTTON_ON_ID, BUTTON_CLIC_ID, BUTTON_INVAL_ID,
      -- bouton Record
      BUT_REC_OFF_ID, BUT_REC_ON_ID, BUT_REC_CLIC_ID, BUT_REC_INVAL_ID,
      -- boutons moyenne taille
      MOY_BUT_OFF_ID, MOY_BUT_ON_ID, MOY_BUT_CLIC_ID, MOY_BUT_INVAL_ID,
      -- petits boutons
      PET_BUT_OFF_ID, PET_BUT_ON_ID, PET_BUT_CLIC_ID, PET_BUT_INVAL_ID,
      -- player
      BMAP_PLAY_ID, BMAP_PAUSE_ID, BMAP_STOP_ID, BMAP_LOAD_ID, VOYANT_REC_ID, LOOP_OFF_ID, LOOP_ON_ID,
      -- couples d'icones
      BMAP_PLUS_ID, BMAP_MOINS_ID,
      BMAP_TRI_HAUT_ID, BMAP_TRI_BAS_ID,
      BMAP_TRI_GAUCHE_ID, BMAP_TRI_DROITE_ID,
      BMAP_CIBLE_ID,
      -- ascenceur horizontal
      ASC_H_GAUCHE_ID, ASC_H_CENTRE_ID, ASC_H_DROITE_ID, ASC_H_CURS_OFF_ID, ASC_H_CURS_ON_ID, ASC_H_CURS_INVAL_ID,
      -- ascenceur vertical
      ASC_V_HAUT_ID, ASC_V_CENTRE_ID, ASC_V_BAS_ID, ASC_V_CURS_OFF_ID, ASC_V_CURS_ON_ID, ASC_V_CURS_INVAL_ID

    );

   -- =================================================================

   -- utilisés dans Dialog_pkg
   TRACK_ID             : constant := 1000;
   EDIT_ID              : constant := 2000;

   -- rectangle ou est affiché la version, mis à jour par Layout_pkg
   version_rect : Win32.Windef.Rect;

   -- couleurs, pen et brush pour le fond de l'affichage
   -- Format : AABBGGRR, A = alpha, B=blue, G=green, R=red
   couleur_vert  	: constant := 16#0011AA00#;
   couleur_vert_clair  	: constant := 16#00CCFFCC#;
   couleur_rouge 	: constant := 16#004622DA#;
   Couleur_fond  	: constant := 16#00BEE5F4#;
   Couleur_beige 	: constant := 16#00EAF6FA#;
   couleur_gris  	: constant := 16#00888888#;
   couleur_gris_leger	: constant := 16#00DDDDDD#;
   couleur_bleue 	: constant := 16#00A8C015#;

   brush_verte   	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(couleur_vert);
   brush_rouge   	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(couleur_rouge);
   brush_blanche 	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(16#00FFFFFF#);
   brush_noire   	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(16#00000000#);
   brush_grise   	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(couleur_gris);
   brush_grise_leger	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(couleur_gris_leger);
   brush_fond    	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(Couleur_fond);
   brush_beige   	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(Couleur_beige);
   brush_midi    	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(Couleur_bleue);
   brush_score   	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(couleur_rouge);
   brush_mesure 	: constant Win32.Windef.HBRUSH := Win32.Wingdi.CreateSolidBrush(couleur_vert_clair);

   Pen_vert  : constant Win32.Windef.HPEN := Win32.Wingdi.CreatePen(Win32.Wingdi.PS_SOLID, 1, couleur_vert);
   Pen_noir  : constant Win32.Windef.HPEN := Win32.Wingdi.CreatePen(Win32.Wingdi.PS_SOLID, 1, 16#00000000#);
   Pen_blanc  : constant Win32.Windef.HPEN := Win32.Wingdi.CreatePen(Win32.Wingdi.PS_SOLID, 1, 16#00FFFFFF#);
   Pen_rouge : constant Win32.Windef.HPEN := Win32.Wingdi.CreatePen(Win32.Wingdi.PS_SOLID, 1, couleur_rouge);
   Pen_gris  : constant Win32.Windef.HPEN := Win32.Wingdi.CreatePen(Win32.Wingdi.PS_SOLID, 1, couleur_gris);
   Pen_score : constant Win32.Windef.HPEN := Win32.Wingdi.CreatePen(Win32.Wingdi.PS_SOLID, 3, couleur_rouge);
   Pen_midi  : constant Win32.Windef.HPEN := Win32.Wingdi.CreatePen(Win32.Wingdi.PS_SOLID, 3, couleur_bleue);

   Pen_gris_plein  : constant Win32.Windef.HPEN := Win32.Wingdi.CreatePen(Win32.Wingdi.PS_SOLID, 1, couleur_gris);
   Pen_gris_leger  : constant Win32.Windef.HPEN := Win32.Wingdi.CreatePen(Win32.Wingdi.PS_SOLID, 1, couleur_gris_leger);


   -- nom police
   font_name : constant string := "Times" & ascii.nul;

   -- police pour affichage pitch, note et octave: Times
   Big_font : Win32.Windef.HFONT := Win32.Wingdi.CreateFont(
                         nHeight => 48,
                         nWidth => 0,
                         nEscapement => 0,
                         nOrientation => 0,
                         fnWeight => Win32.Wingdi.FW_NORMAL,		-- light, normal, bold, ...
                         fdwItalic => 0,
                         fdwUnderline => 0,
                         fdwStrikeOut => 0,
                         fdwCharSet => Win32.Wingdi.ANSI_CHARSET,
                         fdwOutputPrecision => Win32.Wingdi.OUT_DEFAULT_PRECIS,
                         fdwClipPrecision => Win32.Wingdi.CLIP_DEFAULT_PRECIS,
                         fdwQuality => Win32.Wingdi.DEFAULT_QUALITY,
                         fdwPitchAndFamily => 0,
                         lpszFace => Conversions.TO_LPCSTR(font_name'address)
                         );

   -- police pour affichage label
   Small_font : Win32.Windef.HFONT := Win32.Wingdi.CreateFont(
                         nHeight => 24,
                         nWidth => 0,
                         nEscapement => 0,
                         nOrientation => 0,
                         fnWeight => Win32.Wingdi.FW_BOLD,		-- light, normal, bold, ...
                         fdwItalic => 0,
                         fdwUnderline => 0,
                         fdwStrikeOut => 0,
                         fdwCharSet => Win32.Wingdi.ANSI_CHARSET,
                         fdwOutputPrecision => Win32.Wingdi.OUT_DEFAULT_PRECIS,
                         fdwClipPrecision => Win32.Wingdi.CLIP_DEFAULT_PRECIS,
                         fdwQuality => Win32.Wingdi.DEFAULT_QUALITY,
                         fdwPitchAndFamily => 0,
                         lpszFace => Conversions.TO_LPCSTR(font_name'address) 	-- nom police
                         );

   -- police pour affichage des paroles
   Text_font : Win32.Windef.HFONT := Win32.Wingdi.CreateFont(
                         nHeight => 18,
                         nWidth => 0,
                         nEscapement => 0,
                         nOrientation => 0,
                         fnWeight => Win32.Wingdi.FW_BOLD,		-- light, normal, bold, ...
                         fdwItalic => 0,
                         fdwUnderline => 0,
                         fdwStrikeOut => 0,
                         fdwCharSet => Win32.Wingdi.ANSI_CHARSET,
                         fdwOutputPrecision => Win32.Wingdi.OUT_DEFAULT_PRECIS,
                         fdwClipPrecision => Win32.Wingdi.CLIP_DEFAULT_PRECIS,
                         fdwQuality => Win32.Wingdi.DEFAULT_QUALITY,
                         fdwPitchAndFamily => 0,
                         lpszFace => Conversions.TO_LPCSTR(font_name'address) 	-- nom police
                         );
   Text_light_font : Win32.Windef.HFONT := Win32.Wingdi.CreateFont(
                         nHeight => 18,
                         nWidth => 0,
                         nEscapement => 0,
                         nOrientation => 0,
                         fnWeight => Win32.Wingdi.FW_NORMAL,		-- light, normal, bold, ...
                         fdwItalic => 0,
                         fdwUnderline => 0,
                         fdwStrikeOut => 0,
                         fdwCharSet => Win32.Wingdi.ANSI_CHARSET,
                         fdwOutputPrecision => Win32.Wingdi.OUT_DEFAULT_PRECIS,
                         fdwClipPrecision => Win32.Wingdi.CLIP_DEFAULT_PRECIS,
                         fdwQuality => Win32.Wingdi.DEFAULT_QUALITY,
                         fdwPitchAndFamily => 0,
                         lpszFace => Conversions.TO_LPCSTR(font_name'address) 	-- nom police
                         );


   -- police des grands boutons
   Button_font : Win32.Windef.HFONT := Win32.Wingdi.CreateFont(
                         nHeight => 18,
                         nWidth => 0,
                         nEscapement => 0,
                         nOrientation => 0,
                         fnWeight => Win32.Wingdi.FW_BOLD,		-- light, normal, bold, ...
                         fdwItalic => 0,
                         fdwUnderline => 0,
                         fdwStrikeOut => 0,
                         fdwCharSet => Win32.Wingdi.ANSI_CHARSET,
                         fdwOutputPrecision => Win32.Wingdi.OUT_DEFAULT_PRECIS,
                         fdwClipPrecision => Win32.Wingdi.CLIP_DEFAULT_PRECIS,
                         fdwQuality => Win32.Wingdi.DEFAULT_QUALITY,
                         fdwPitchAndFamily => 0,
                         lpszFace => Conversions.TO_LPCSTR(font_name'address) 	-- nom police
                         );

   -- police des petits boutons
   Small_Small_font : Win32.Windef.HFONT := Win32.Wingdi.CreateFont(
                         nHeight => 14,
                         nWidth => 0,
                         nEscapement => 0,
                         nOrientation => 0,
                         fnWeight => Win32.Wingdi.FW_NORMAL,		-- light, normal, bold, ...
                         fdwItalic => 0,
                         fdwUnderline => 0,
                         fdwStrikeOut => 0,
                         fdwCharSet => Win32.Wingdi.ANSI_CHARSET,
                         fdwOutputPrecision => Win32.Wingdi.OUT_DEFAULT_PRECIS,
                         fdwClipPrecision => Win32.Wingdi.CLIP_DEFAULT_PRECIS,
                         fdwQuality => Win32.Wingdi.DEFAULT_QUALITY,
                         fdwPitchAndFamily => 0,
                         lpszFace => Conversions.TO_LPCSTR(font_name'address) 	-- nom police
                         );

   --
   -- Les curseurs -------------------------------------------------------------------
   default_cursor : Win32.Windef.HCURSOR := System.Null_address;
   drag_cursor    : Win32.Windef.HCURSOR := System.Null_address;

   -- ---------------------------------------------------------------------------------

   procedure get_bitmap( nom : string;
                         header : out Win32.Wingdi.BITMAPINFOHEADER;
                         bits   : out Win32.LPVOID );

   function Id_of( value : Integer ) return obj_id_type;

end Resources_pkg;

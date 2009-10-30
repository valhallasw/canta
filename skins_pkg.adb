with System;		use System;
with Ada.Text_io;	use Ada.Text_io;
with GNAT.Case_Util;
with Interfaces.C;	use Interfaces.C;
with Unchecked_deallocation;

with Win32;		use Win32;
with Win32.Wingdi;
with Win32.Windef;
with Win32.Winbase;
with Win32.Winnt;

with Common_types;	use Common_types;
with Conversions;	use Conversions;
with Registre_pkg;
with Utils_pkg;
with Intl;
with Log;
with Resources_pkg;	use Resources_pkg;
with Bitmap_pkg;	use Bitmap_pkg;
with Conversions;


package body Skins_pkg is

   -- type pour les polices
   -- thin, extra-thin, light, normal, medium, semi-bold, bold, extra-bold, heavy
   type Weight_type is ( T, ET, L, N, M, SB, B, EB, H );

   -- mots clés
   type Token_type is (
            BACKGROUND,		-- fond de la main window
            MAIN_FRAME, 	-- bords de la main window
            BUTTON_LARGE, 	-- bitmap des gros boutons
            BUTTON_REC, 	-- bitmap du bouton d'enregistrement
            BUTTON_MEDIUM, 	-- bitmap de fond des moyens boutons (Player)
            BUTTON_SMALL,	-- bitmap de fond des petits boutons
            FONT_BUTTON, 	-- police des boutons avec taille et couleur
            FONT_LABEL,		-- idem pour les labels
            FADER,		-- bitmap du fader
            LIFT_H, 		-- bitmap de l'ascenseur horizontal
            LIFT_V,		-- bitmap de l'ascenseur vertical
            DISPLAY_FRAME, 	-- bords des display
            THIN_FRAME,		-- bords minces
            PLAYER,		-- bitmap intérieur du player
            ZOOM,		-- bitmap intérieurs des boutons zoom
            OFFSET_H,		-- idem offset horizontal
            OFFSET_V,		-- idem vertical
            TARGET		-- bitmap du bouton de centrage
   );

   type cell_type is record
      num_bitmap: natural;	-- nombre de bitmap à charger: noms de fichier
      base_id   : bmap_id_type;	-- Id de la 1ere bitmap
      offset    : boolean;	-- indique s'il y a un offset à lire: Top + Left (entiers)
      font      : boolean;	-- idem pour une police: nom (string)+hauteur (entier)+poids (entier)+couleur (hexa)
   end record;

   type Syntax_array is array(Token_type) of cell_type;

   Syntax : Syntax_array := ( --  bitmap Id 1ere bitmap        offset police
            BACKGROUND          => (1, MAIN_BACKGROUND_ID, 	false, false),
            MAIN_FRAME 		=> (8, MAIN_CHG_ID, 		false, false),
            BUTTON_LARGE 	=> (4, BUTTON_OFF_ID, 		true,  false),
            BUTTON_REC          => (4, BUT_REC_OFF_ID, 		true,  false),
            BUTTON_MEDIUM 	=> (4, MOY_BUT_OFF_ID, 		true,  false),
            BUTTON_SMALL 	=> (4, PET_BUT_OFF_ID, 		true,  false),
            FONT_BUTTON 	=> (0, MAIN_BACKGROUND_ID, 	false, true), -- ID pas utilisé en fait
            FONT_LABEL   	=> (0, MAIN_BACKGROUND_ID, 	false, true), -- idem
            FADER 		=> (5, FADER_FOND_1_ID, 	true,  false),
            LIFT_H 		=> (6, ASC_H_GAUCHE_ID, 	true,  false),
            LIFT_V 		=> (6, ASC_V_HAUT_ID, 		true,  false),
            DISPLAY_FRAME 	=> (8, DISP_CHG_ID, 		false, false),
            THIN_FRAME 	        => (8, THIN_CHG_ID, 		false, false),
            PLAYER 		=> (7, BMAP_PLAY_ID, 		false, false),
            ZOOM 		=> (2, BMAP_PLUS_ID, 		false, false),
            OFFSET_H 		=> (2, BMAP_TRI_GAUCHE_ID, 	false, false),
            OFFSET_V 		=> (2, BMAP_TRI_HAUT_ID, 	false, false),
            TARGET		=> (1, BMAP_CIBLE_ID,		false, false)
            );

   subtype buffer_type is string(1..256);

   sub_dir : constant string := "\skins\" ;
   extension : constant string := ".skn";

   default_name : constant string := "default";

   -- list for skins
   type skin_cell;
   type skin_cell_pt is access skin_cell;
   type skin_cell is record
      name : string_pt;
      next : skin_cell_pt;
   end record;
   skin_list : skin_cell_pt;
   nbr_skins : natural := 0;

   procedure Free is new Unchecked_deallocation( object => skin_cell, name => skin_cell_pt );

   procedure Free_skin_list is
      tmp : skin_cell_pt;
   begin
      while skin_list /= null loop
         tmp := skin_list.next;
         Free( skin_list );
         skin_list := tmp;
      end loop;
   end Free_skin_list;

   -- *************************************************************************

   procedure Default_font is
      -- nom police
      font_name : constant string := "Times" & ascii.nul;
      res_bool : Win32.BOOL;
   begin
      if button_font /= System.Null_Address then
         res_bool := Win32.Wingdi.DeleteObject( Button_font );
      end if;
      -- police des grands boutons
      Button_font := Win32.Wingdi.CreateFont(
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
      -- police pour affichage label
      if Label_font /= System.Null_Address then
         res_bool := Win32.Wingdi.DeleteObject( Label_font );
      end if;
      Label_font := Win32.Wingdi.CreateFont(
                         nHeight => 24,
                         nWidth => 0,
                         nEscapement => 0,
                         nOrientation => 0,
                         fnWeight => Win32.Wingdi.FW_BOLD,
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
      -- couleurs par défaut = noir
      button_color := 0;
      Label_color :=0 ;
   end Default_font;


   procedure Default_rectangles is
   begin
      large_but_x  := 0;
      large_but_y  := 0;
      med_but_x    := 0;
      med_but_y    := 0;
      pet_but_x    := 0;
      pet_but_y    := 0;
   end Default_rectangles;

   procedure Default_fader is
   begin
      -- position max du curseur par rapport au fader
      fader_diff_haut    := 4;
      fader_diff_bas     := 4;
      --
      asc_V_diff_haut    := 4;
      asc_V_diff_bas     := 4;
      --
      asc_H_diff_gauche  := 4;
      asc_H_diff_droite  := 4;
   end Default_fader;

   procedure Default_skin is
   begin
      -- desalloue les bitmaps ayant pu être alloués précédement
      Bitmap_pkg.Free_all_bitmaps;
      --
      -- création des fonts
      Default_font;
      -- rectangles de centrages des contenu de boutons
      Default_rectangles;
      -- géométrie dy fader par défaut
      Default_fader;
      --
      -- Création des bitmaps à partir des valeurs hard-codées
      --
      -- Fond
      Bitmap_pkg.Create_bitmap( "background",  		Resources_pkg.MAIN_BACKGROUND_ID );
      -- Fader
      Bitmap_pkg.Create_bitmap( "fond_fader_1",		Resources_pkg.FADER_FOND_1_ID );
      Bitmap_pkg.Create_bitmap( "fond_fader_2",		Resources_pkg.FADER_FOND_2_ID );
      Bitmap_pkg.Create_bitmap( "fond_fader_3",		Resources_pkg.FADER_FOND_3_ID );
      Bitmap_pkg.Create_bitmap( "fader_curseur_on",  	Resources_pkg.FADER_CURSEUR_ON_ID );
      Bitmap_pkg.Create_bitmap( "fader_curseur_off", 	Resources_pkg.FADER_CURSEUR_OFF_ID );
      -- Button_map: dessus
      Bitmap_pkg.Create_bitmap( "bitmap_play",   	Resources_pkg.BMAP_PLAY_ID );
      Bitmap_pkg.Create_bitmap( "bitmap_pause",  	Resources_pkg.BMAP_PAUSE_ID );
      Bitmap_pkg.Create_bitmap( "bitmap_stop",   	Resources_pkg.BMAP_STOP_ID );
      Bitmap_pkg.Create_bitmap( "bitmap_open", 		Resources_pkg.BMAP_LOAD_ID );
      Bitmap_pkg.Create_bitmap( "plus", 		Resources_pkg.BMAP_PLUS_ID );
      Bitmap_pkg.Create_bitmap( "moins", 		Resources_pkg.BMAP_MOINS_ID );
      Bitmap_pkg.Create_bitmap( "tri_haut", 		Resources_pkg.BMAP_TRI_HAUT_ID );
      Bitmap_pkg.Create_bitmap( "tri_bas", 		Resources_pkg.BMAP_TRI_BAS_ID );
      Bitmap_pkg.Create_bitmap( "tri_gauche", 		Resources_pkg.BMAP_TRI_GAUCHE_ID );
      Bitmap_pkg.Create_bitmap( "tri_droite", 		Resources_pkg.BMAP_TRI_DROITE_ID );
      -- Grands boutons
      Bitmap_pkg.Create_bitmap( "but_1_off",  		Resources_pkg.BUTTON_OFF_ID );
      Bitmap_pkg.Create_bitmap( "but_1_on",   		Resources_pkg.BUTTON_ON_ID );
      Bitmap_pkg.Create_bitmap( "but_1_clic", 		Resources_pkg.BUTTON_CLIC_ID );
      Bitmap_pkg.Create_bitmap( "but_1_inval", 		Resources_pkg.BUTTON_INVAL_ID );
      -- moyen boutons
      Bitmap_pkg.Create_bitmap( "but_2_off",   		Resources_pkg.MOY_BUT_OFF_ID );
      Bitmap_pkg.Create_bitmap( "but_2_on",    		Resources_pkg.MOY_BUT_ON_ID );
      Bitmap_pkg.Create_bitmap( "but_2_clic",  		Resources_pkg.MOY_BUT_CLIC_ID );
      Bitmap_pkg.Create_bitmap( "but_2_inval", 		Resources_pkg.MOY_BUT_INVAL_ID );
      -- Petits boutons
      Bitmap_pkg.Create_bitmap( "but_3_off",  		Resources_pkg.PET_BUT_OFF_ID );
      Bitmap_pkg.Create_bitmap( "but_3_on",   		Resources_pkg.PET_BUT_ON_ID );
      Bitmap_pkg.Create_bitmap( "but_3_clic", 		Resources_pkg.PET_BUT_CLIC_ID );
      Bitmap_pkg.Create_bitmap( "but_3_inval", 		Resources_pkg.PET_BUT_INVAL_ID );
      -- Bouton record
      Bitmap_pkg.Create_bitmap( "but_rec_off", 		Resources_pkg.BUT_REC_OFF_ID );
      Bitmap_pkg.Create_bitmap( "but_1_on",   		Resources_pkg.BUT_REC_ON_ID );
      Bitmap_pkg.Create_bitmap( "but_1_clic", 		Resources_pkg.BUT_REC_CLIC_ID );
      Bitmap_pkg.Create_bitmap( "but_1_inval", 		Resources_pkg.BUT_REC_INVAL_ID );
      -- bords des display épais
      Bitmap_pkg.Create_bitmap( "disp_chg",		Resources_pkg.DISP_CHG_ID );
      Bitmap_pkg.Create_bitmap( "disp_chd",		Resources_pkg.DISP_CHD_ID );
      Bitmap_pkg.Create_bitmap( "disp_cbg", 		Resources_pkg.DISP_CBG_ID );
      Bitmap_pkg.Create_bitmap( "disp_cbd", 		Resources_pkg.DISP_CBD_ID );
      Bitmap_pkg.Create_bitmap( "disp_bh",       	Resources_pkg.DISP_BH_ID );
      Bitmap_pkg.Create_bitmap( "disp_bb",        	Resources_pkg.DISP_BB_ID );
      Bitmap_pkg.Create_bitmap( "disp_bg",     		Resources_pkg.DISP_BG_ID );
      Bitmap_pkg.Create_bitmap( "disp_bd",     		Resources_pkg.DISP_BD_ID );
      -- bords des display fins
      Bitmap_pkg.Create_bitmap( "fins_chg",		Resources_pkg.THIN_CHG_ID );
      Bitmap_pkg.Create_bitmap( "fins_chd",		Resources_pkg.THIN_CHD_ID );
      Bitmap_pkg.Create_bitmap( "fins_cbg", 		Resources_pkg.THIN_CBG_ID );
      Bitmap_pkg.Create_bitmap( "fins_cbd", 		Resources_pkg.THIN_CBD_ID );
      Bitmap_pkg.Create_bitmap( "fins_bh",       	Resources_pkg.THIN_BH_ID );
      Bitmap_pkg.Create_bitmap( "fins_bb",        	Resources_pkg.THIN_BB_ID );
      Bitmap_pkg.Create_bitmap( "fins_bg",     		Resources_pkg.THIN_BG_ID );
      Bitmap_pkg.Create_bitmap( "fins_bd",     		Resources_pkg.THIN_BD_ID );
      -- bords de la main window
      Bitmap_pkg.Create_bitmap( "main_chg",		Resources_pkg.MAIN_CHG_ID );
      Bitmap_pkg.Create_bitmap( "main_chd",		Resources_pkg.MAIN_CHD_ID );
      Bitmap_pkg.Create_bitmap( "main_cbg", 		Resources_pkg.MAIN_CBG_ID );
      Bitmap_pkg.Create_bitmap( "main_cbd", 		Resources_pkg.MAIN_CBD_ID );
      Bitmap_pkg.Create_bitmap( "main_bh",       	Resources_pkg.MAIN_BH_ID );
      Bitmap_pkg.Create_bitmap( "main_bb",        	Resources_pkg.MAIN_BB_ID );
      Bitmap_pkg.Create_bitmap( "main_bg",    		Resources_pkg.MAIN_BG_ID );
      Bitmap_pkg.Create_bitmap( "main_bd",    		Resources_pkg.MAIN_BD_ID );
      -- création du bitmap du voyant d'enregistrement
      Bitmap_pkg.Create_bitmap( "voyant_rec",   	Resources_pkg.VOYANT_REC_ID );
      -- création du bitmap du bouton de centrage
      Bitmap_pkg.Create_bitmap( "cible",   		Resources_pkg.BMAP_CIBLE_ID );
      -- ascenseurs
      Bitmap_pkg.Create_bitmap( "asc_h_c",   		Resources_pkg.ASC_H_CENTRE_ID );
      Bitmap_pkg.Create_bitmap( "asc_h_d",   		Resources_pkg.ASC_H_DROITE_ID );
      Bitmap_pkg.Create_bitmap( "asc_h_g",   		Resources_pkg.ASC_H_GAUCHE_ID );
      Bitmap_pkg.Create_bitmap( "asc_h_curs_off",   	Resources_pkg.ASC_H_CURS_OFF_ID );
      Bitmap_pkg.Create_bitmap( "asc_h_curs_on",	Resources_pkg.ASC_H_CURS_ON_ID );
      Bitmap_pkg.Create_bitmap( "asc_h_curs_inval", 	Resources_pkg.ASC_H_CURS_INVAL_ID );
      --
      Bitmap_pkg.Create_bitmap( "asc_v_c",   		Resources_pkg.ASC_V_CENTRE_ID );
      Bitmap_pkg.Create_bitmap( "asc_v_b",   	 	Resources_pkg.ASC_V_BAS_ID );
      Bitmap_pkg.Create_bitmap( "asc_v_h",   		Resources_pkg.ASC_V_HAUT_ID );
      Bitmap_pkg.Create_bitmap( "asc_v_curs_off",   	Resources_pkg.ASC_V_CURS_OFF_ID );
      Bitmap_pkg.Create_bitmap( "asc_v_curs_on",	Resources_pkg.ASC_V_CURS_ON_ID );
      Bitmap_pkg.Create_bitmap( "asc_v_curs_inval", 	Resources_pkg.ASC_V_CURS_INVAL_ID );
      --
      Bitmap_pkg.Create_bitmap( "loop_on",		Resources_pkg.LOOP_ON_ID );
      Bitmap_pkg.Create_bitmap( "loop_off", 		Resources_pkg.LOOP_OFF_ID );
   end Default_skin;


   -- **************************************************************************************


   function Load_skin( name : string ) return boolean is
      f : Ada.Text_io.file_type;
      skin_name : string := name;	-- copie pour mise en minuscule
      dir_name : string := Registre_pkg.Get_exe_dir;
      file_name : string := dir_name & sub_dir & skin_name & extension;
      root : string := dir_name & sub_dir & skin_name & "\";
      line_number : natural := 0;
      source_line : buffer_type;
      line_length : natural;
      empty : boolean := true;
      cur_char : natural := 0;
      syntax_error : exception;
      token : token_type;

      procedure skip_empty is
      begin
         loop
            -- si nécessaire lecture d'une ligne
            if empty or cur_char > line_length then
               Ada.Text_io.get_line( f, source_line, line_length );
               line_number := line_number + 1;
               empty := false;
               cur_char := 1;
            end if;
            -- recherche premier caractere
            while cur_char <= line_length loop
               case source_line(cur_char) is
                  when ' ' | ascii.ht => 		-- blanc
                     cur_char := cur_char + 1;
                  when ';' | '#' | '-' | '/' => 	-- commentaire
                     empty := true;
                     exit;
                  when 'a'..'z' | 'A'..'Z' | '0'..'9' =>
                     return; -- trouvé !
                  when others =>
                     raise syntax_error;
               end case;
            end loop;
         end loop;
      end skip_empty;


      function Get_Token return Token_type is
         start : natural;
      begin
         -- se place sur le 1er char qui est alphanumérique
         skip_empty;
         ---
         start := cur_char;
         while cur_char <= line_length and then
               ( source_line(cur_char) /= ' ' and source_line(cur_char) /= ascii.ht ) loop
            cur_char := cur_char + 1;
         end loop;
         return Token_type'value( source_line(start..cur_char-1 ) );
      end Get_token;

      procedure read_param( str : in out string;
                            len : out natural ) is
         length : natural := 0;
      begin
         skip_empty;
         loop
            length := length + 1;
            str(length) := source_line(cur_char);
            cur_char := cur_char + 1;
            exit when cur_char > line_length or else
               ( source_line(cur_char) = ' ' or source_line(cur_char) = ascii.ht );
         end loop;
         len := length;
      end Read_param;


      function param return string is
         temp : buffer_type;
         len : natural := 0;
      begin
         read_param( temp, len );
         return temp(1..len);
      end Param;

      procedure Get_Offset is
         L, T : integer;
      begin
         -- lecture des paramètres
         L := integer'value( param );	-- left
         T := integer'value( param );	-- top
         --
         case token is
            when BUTTON_LARGE =>
               -- Grands boutons
               large_but_x := L;
               large_but_y := T;
            when BUTTON_MEDIUM =>
               -- Moyens boutons
               med_but_x := L;
               med_but_y := T;
            when BUTTON_SMALL  =>
               -- Petits boutons
               pet_but_x := L;
               pet_but_y := T;
            when FADER =>
               fader_diff_haut:= L;
               fader_diff_bas := T;
            when LIFT_H =>
               asc_H_diff_gauche := L;
               asc_H_diff_droite := T;
            when LIFT_V =>
               asc_V_diff_haut := L;
               asc_V_diff_bas  := T;
            when others => null;
         end case;
      end Get_Offset;


      procedure Get_font is
         temp_font : Win32.Windef.HFONT;
         height : integer;
         weight : Win32.INT;
         font_name : buffer_type;
         name_length : natural := 0;
         color : integer := 0;
         res_bool : Win32.BOOL;
      begin
         -- lecture des paramètres
         --
         -- 1.nom de la police
         read_param( font_name, name_length );
         font_name(name_length+1) := ascii.nul;	-- ajout d'un Nul à la fin pour CreateFont
         -- 2.hauteur
         begin
            height := integer'value( param );
         exception
            when others =>
               Log.Store( "Invalide font height");Log.End_line;
               raise syntax_error;
         end;
         -- 3.poids
         begin
            case Weight_type'value( param ) is
               when T   => weight := Win32.Wingdi.FW_THIN;
               when ET  => weight := Win32.Wingdi.FW_EXTRALIGHT;
               when L   => weight := Win32.Wingdi.FW_LIGHT;
               when N   => weight := Win32.Wingdi.FW_NORMAL;
               when M   => weight := Win32.Wingdi.FW_MEDIUM;
               when SB  => weight := Win32.Wingdi.FW_SEMIBOLD;
               when B   => weight := Win32.Wingdi.FW_BOLD;
               when EB  => weight := Win32.Wingdi.FW_EXTRABOLD;
               when H   => weight := Win32.Wingdi.FW_HEAVY;
            end case;
         exception
            when others =>
               Log.Store( "Invalide font weight");Log.End_line;
               raise syntax_error;
         end;
         --
         -- 4.couleur
         begin
            color := integer'value( "16#00" & param & "#" );
         exception
            when others =>
               Log.Store( "Invalid color" ); Log.End_line;
               raise syntax_error;
         end;
         --
         -- création de la police
         --
         temp_font := Win32.Wingdi.CreateFont(
                            nHeight => Win32.INT(height),
                            nWidth => 0,
                            nEscapement => 0,
                            nOrientation => 0,
                            fnWeight => weight,	-- light, normal, bold, ...
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
         -- enregistre avec le bon identifier
         --
         if Token = FONT_BUTTON then
            if button_font /= System.Null_Address then
               res_bool := Win32.Wingdi.DeleteObject( Button_font );
            end if;
            Button_font := temp_font;
            Button_color := Win32.Windef.COLORREF(color);
         else
            if Label_font /= System.Null_Address then
               res_bool := Win32.Wingdi.DeleteObject( Label_font );
            end if;
            Label_font := temp_font;
            Label_color := Win32.Windef.COLORREF(color);
         end if;
      end Get_Font;


      -- lit en succession 'number' parametre et crée autant de bitmap associés
      -- aux valeurs successives de l'identifiant 'base'
      -- l'ordre de déclaration des identifiants dans Resources_pkg est fondamental
      procedure Get_Bitmap( dir    : string;
                            base   : Resources_pkg.bmap_Id_type;
                            number : natural ) is
         Id : Resources_pkg.bmap_Id_type := base;
         nom_fichier : buffer_type;
         len : natural := 0;
      begin
         for i in 1..number loop
            read_param( nom_fichier, len );
            -- lecture dans le sous-répertoire défini par skin_name
            Bitmap_pkg.Create_from_file( dir & nom_fichier(1..len), Id );
            --
            if i < number then	-- pour eviter CE dans succ
               Id := Resources_pkg.bmap_Id_type'succ(Id);
            end if;
         end loop;
      end Get_Bitmap;


   begin	-- LOAD_SKIN
      --
      GNAT.Case_Util.To_Lower( skin_name );
      if skin_name = default_name or skin_name ="" then
         -- on utilise les valeurs hard-codées
         Default_skin;
         return true;
      end if;
      --
      -- ouverture du fichier skin à charger à partir du nom défini dans la base de registre
      begin
         Open( f, in_file, file_name );
      exception
         when others =>
            -- erreur d'ouverture du fichier
            Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_skin_not_found & file_name );
            -- on utilise les valeurs hard-codées
            return false;
      end;
      --
      -- lectures des info police et bitmap
      line_number := 0;
      while not end_of_file( f ) loop
         begin
            -- lecture du prochain token (saute lignes vides et commentaires)
            Token := Get_token;
         exception
            when Ada.Text_io.END_ERROR => exit;	-- fin de fichier, sortir de la boucle
         end;
         begin
            -- lecture du nombre de bitmap spécifié
            if Syntax(Token).num_bitmap > 0 then
               Get_bitmap( root, Syntax(Token).base_id , Syntax(Token).num_bitmap );
            end if;
            -- lecture de l'offfset si spécifié
            if Syntax(Token).offset then
               Get_offset;
            end if;
            -- lecture de la police si spécifié
            if Syntax(Token).font then
               Get_Font;
            end if;
         exception
            -- erreur de syntaxe, message dans le handler suivant
            when Ada.Text_io.END_ERROR => raise Syntax_error;
         end;
      end loop;
      --
      -- fermeture
      Close( f );
      -- test completude
      for Id in Resources_pkg.bmap_id_type'range loop
         if ID /= NULL_ID and then Bitmap_pkg.Bitmap_of( Id ) = null then
            Utils_pkg.Error_box( Intl.err_init_txt,
                                 Intl.err_skin_missing & Resources_pkg.bmap_id_type'image(Id) );
            return false;
         end if;
      end loop;
      -- terminé
      return true;
   exception
      when others =>
         Utils_pkg.Error_box( Intl.err_init_txt,
                              Intl.err_skin_syntax & Natural'image(line_number) & Intl.new_line
                              & source_line(1..line_length) & Intl.new_line
                              & Intl.new_line & file_name );
         Close( f );
         return false;
   end Load_skin;


   -- retourne le nombre de skins disponible et crée la liste des skin names utilisé par Skin_Name
   function Count_skins return integer is
      dir_name : string := Registre_pkg.Get_exe_dir;
      specif : string := dir_name & sub_dir & "*" & extension & ascii.nul;
      data : Win32.Winbase.WIN32_FIND_DATAA;
      search : Win32.Winnt.HANDLE;
      last_skin : skin_cell_pt;
      res_bool : Win32.BOOL;

      function Filename_To_Ada return string is
         s : string(1..260);
         for s use at data.cFileName'address;
         len : natural := 0;
      begin
         while len < 260 and then s(len+1) /= ascii.nul loop
            len := len + 1;
         end loop;
         return s(1..len-4);	-- saute l'extension ".txt"
      end Filename_To_Ada;

   begin
      -- création liste des skins
      Free_skin_list;
      skin_list := new skin_cell'( new string'( default_name ), null );
      last_skin := skin_list;
      nbr_skins := 1;
      -- recherche les fichiers texte dans le répertoire skins
      search := Win32.Winbase.FindFirstFile( Conversions.To_LPCSTR(specif'address),
                                             TO_LPWFD(data'address) );
      --
      if search = Win32.Winbase.INVALID_HANDLE_VALUE then
         return 1;	-- toujours 1 pour "default"
      end if;
      loop
         last_skin.next := new skin_cell'( new string'( Filename_to_Ada ), null );
         last_skin := last_skin.next;
         GNAT.Case_Util.To_Lower( last_skin.name.all );	-- en minuscule
         nbr_skins := nbr_skins + 1;
         --
         res_bool := Win32.Winbase.FindNextFile( search, TO_LPWFD(data'address) );
         exit when res_bool = 0;
      end loop;
      res_bool := Win32.Winbase.FindClose( search );
      --
      return nbr_skins;
   end Count_skins;


   -- retourne le nom du skin de rang 'number' (1 = default )
   -- Count_Skins doit avoir été appelé avant !
   function Skin_name( number : integer ) return string is
      last_skin : skin_cell_pt := skin_list;
   begin
      for i in 1..number-1 loop
         last_skin := last_skin.next;
      end loop;
      return last_skin.name.all;
   exception
      when others => return default_name;
   end Skin_name;


end Skins_pkg;

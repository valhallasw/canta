with System;		use System;
with Calendar;		use Calendar;

with Interfaces.C;	use Interfaces.C;
with GNAT.Current_Exception;

with Win32;		use Win32;
with Win32.Windef;	use Win32.Windef;
with Win32.Wingdi;	use Win32.Wingdi;
with Win32.Winuser;	use Win32.Winuser;
with Win32.Winbase;
with Win32.Winmain;
with Win32.Winnt;
with Win32.Mmsystem;
with Win32.Shellapi;

with Conversions;	use Conversions;
with Common_types;	use Common_types;
with Win_Audio;
with Process_pkg;
with Resources_pkg;	use Resources_pkg;
with Objects_pkg;
with Bitmap_pkg;	use Bitmap_pkg;
with Registre_pkg;
with Affichage_pkg;
with Log;
with Intl;
with Config_pkg;
with Midi_pkg;		use Midi_pkg;
with Utils_pkg;
with Timer_pkg;
with Dialog_pkg;
with Layout_pkg;
with Data_pkg;		use Data_pkg;
with Skins_pkg;
with Debug_pkg;


package body User_Interface is

   mixer_debug_flag : constant string := "mixer";

   -- ===========================================================================

   -- Interface

   status_rect    : constant Win32.Windef.RECT := ( left => 4, top => 2,  right => 80, bottom => 20 );
   compteur_rect  : constant Win32.Windef.RECT := ( left => 4, top => 21, right => 83, bottom => 67 );
   voyant_X : constant := 55;
   voyant_Y : constant := 5;

   -- affichage status
   stop_str  : aliased string := "Stop";
   play_str  : aliased string := "Play";
   pause_str : aliased string := "Pause";

   subtype string_2 is string(1..2);
   chiffres : array(0..9) of character := ('0','1','2','3','4','5','6','7','8','9');

   decalage_affichage : constant Long_Float := 2.0;

   -- racine du nom de fichier de sauvegarde
   root : string_pt;
   -- flag
   sauvegarde_encours : boolean := false;

   -- flags pour lemode freeze
   freezed     : boolean := false;
   file_loaded : boolean := false;	-- fichier MIDI chargé
   was_playing : boolean := false;	-- player MIDI lancé

   -- decalage et transposition
   decalage_octave : integer := 0;
   transposition : integer := 0;
   max_transposition : constant := 11;
   min_transposition : constant := -11;

   -- drag and drop des mesures pour début et fin
   num_mesure : natural := 1;
   drag_mesure : boolean := false;

   -- ==============================================================================

   -- fonction Windows manquant dans Win32Ada et utilisée pour chargé l'icone
   -- lors de la création de la classe de la main window
   -- L'icone est affichée dans la barre de titre et quand l'appli est minimisée
   function LoadImage( hInst  : Win32.Windef.HINSTANCE;
                       name   : Win32.LPCSTR;
                       kind   : Win32.UINT;
                       cx     : Win32.INT;
                       cy     : Win32.INT;
                       fuLoad : Win32.UINT ) return Win32.Winnt.HANDLE;
   pragma Import (Stdcall, LoadImage, "LoadImageA");

   LR_DEFAULTCOLOR     : constant Win32.UINT := 16#0000#;
   LR_MONOCHROME       : constant Win32.UINT := 16#0001#;
   LR_COLOR            : constant Win32.UINT := 16#0002#;
   LR_COPYRETURNORG    : constant Win32.UINT := 16#0004#;
   LR_COPYDELETEORG    : constant Win32.UINT := 16#0008#;
   LR_LOADFROMFILE     : constant Win32.UINT := 16#0010#;	-- utilisé
   LR_LOADTRANSPARENT  : constant Win32.UINT := 16#0020#;
   LR_DEFAULTSIZE      : constant Win32.UINT := 16#0040#;
   LR_VGACOLOR         : constant Win32.UINT := 16#0080#;
   LR_LOADMAP3DCOLORS  : constant Win32.UINT := 16#1000#;
   LR_CREATEDIBSECTION : constant Win32.UINT := 16#2000#;
   LR_COPYFROMRESOURCE : constant Win32.UINT := 16#4000#;
   LR_SHARED           : constant Win32.UINT := 16#8000#;

   IMAGE_BITMAP    : constant Win32.UINT := 0;
   IMAGE_ICON      : constant Win32.UINT := 1;	-- utilisé
   IMAGE_CURSOR    : constant Win32.UINT := 2;

   -- ==============================================================================

   procedure Display_status is
      status : Midi_pkg.player_status_type;
      str : access string;
      display : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( COMPTEUR_ID );
      res_int : Win32.INT;
   begin
      -- lecture status du player
      status := Midi_pkg.Get_player_status;
      -- sélection de la string à afficher
      case status is
         when Midi_pkg.stopped => str := stop_str'access;
         when Midi_pkg.playing => str := play_str'access;
         when Midi_pkg.paused  => str := pause_str'access;
      end case;
      -- affichage de la string
      -- effacement du fond avec la brush de l'objet
      res_int := Win32.Winuser.FillRect( display.memDC, TO_ACRECT(status_rect'address), display.brush );
      -- écrit la string (en mémoire)
      res_int := Win32.Winuser.DrawText( display.memDC,
                     TO_PCCH(str.all'address),
                     Win32.INT(str'length),
                     TO_PRECT(status_rect'address),
                     Win32.Winuser.DT_TOP + Win32.Winuser.DT_LEFT );
      -- voyant de sauvegarde
      if sauvegarde_encours then
         Bitmap_pkg.Affiche_bitmap( display.memDC, Resources_pkg.Voyant_rec_id,
                                    X=> voyant_X, Y => voyant_Y );
      end if;
      -- affiche à l'écran
      Objects_pkg.Display_display( display );
   end Display_status;


   function zero_image( n : integer ) return string_2 is
      hi, lo : integer;
   begin
      lo := n mod 10;
      hi := n / 10;
      return chiffres(hi) & chiffres(lo);
   end zero_image;


   procedure Update_compteur is
      obj : Objects_pkg.UI_object_ptr := Objects_pkg.Object_of( COMPTEUR_ID );
      res_int : Win32.INT;
      time : integer;
      min, sec, dix : integer;
   begin
      -- temps interne du player en ms / 100 => dixième de secondes
      time := Midi_pkg.Get_player_time / 100;
      -- conversion en minutes, secondes et dixièmes
      dix := time mod 10;
      time := time / 10;	-- secondes
      sec := time mod 60;
      min := time / 60;
      -- effacement du fond avec la brush de l'objet
      res_int := Win32.Winuser.FillRect( obj.memDC, TO_ACRECT(compteur_rect'address), obj.brush );
      declare
         compt_str : string := zero_image(min) & ":" & zero_image(sec) & "." & chiffres(dix);
      begin
         -- écrit la string (en mémoire)
         res_int := Win32.Winuser.DrawText( obj.memDC,
                        TO_PCCH(compt_str'address),
                        Win32.INT(compt_str'length),
                        TO_PRECT(compteur_rect'address),
                        Win32.Winuser.DT_VCENTER + Win32.Winuser.DT_LEFT );
      end;
      -- affiche à l'écran
      Objects_pkg.Display_display( obj );
   end Update_compteur;

   -- ==============================================================================

   procedure Start_sauvegarde is
      path_name : string := Dialog_pkg.Select_Save_file;      -- dialog : sélection fichier
      index : integer;
   begin
      if path_name = "" then
         return;
      end if;
      -- recherche de l'extension
      index := path_name 'last;
      while index >= path_name'first and then (path_name(index) /= '.' and path_name(index) /= '\') loop
         index := index-1;
      end loop;
      --
      if index < path_name'first or else path_name(index) = '\' then
         -- pas d'extension
         index := path_name'last+1;
      end if;
      -- sauvegarde racine du nom
      Free( root );
      root := new string'( path_name(path_name'first..index-1) );
      -- démarre sauvegarde wav
      if Process_pkg.Demarre_sauvegarde( root.all ) then
         sauvegarde_encours := true;
         -- affichage du voyant
         Display_status;
      end if;
   end Start_sauvegarde;


   procedure Fin_sauvegarde is
   begin
      Process_pkg.Termine_sauvegarde;
      sauvegarde_encours := false;
      -- efface voyant
      Display_status;
   end Fin_sauvegarde;

   -- ==========================================================================================

   procedure Display_all( volume   : Short_Integer;
                          pitch    : Long_Float;
                          Note, Octave : Natural;
                          Index    : Integer ) is
      tmp : Long_Float;
   begin
      -- affichage des valeur du pitch, seulement si défini, sinon on reste avec la dernière valeur affichée
      if Pitch > 0.0 then
         Affichage_pkg.Display_textes( Pitch, Note, Octave );
      end if;
      -- indicateur de volume
      Affichage_pkg.Display_volume( volume );
      if volume < Common_types.seuil_pitch then
         -- spectre harmonique tout en gris
         Affichage_pkg.Display_Spectre( - 1 );
      else
         -- spectre harmonique en vert et rouge
         Affichage_pkg.Display_Spectre( index );
      end if;
      -- si saturation et auto-volume set, diminuer le volume du micro
      if volume >= 32600 and Common_types.auto_volume then
         tmp := Long_Float(Win_Audio.Get_Micro_Volume);		-- lecture de la nouvelle valeur
         tmp := tmp * 0.9;	-- diminue de 10%
         Objects_Pkg.Set_Fader_Value( FADER_ID, integer(tmp) );	-- positionnement du fader
         Win_audio.Set_Micro_Volume( Win32.WORD(tmp) );
      end if;
   end Display_all;


   -- ===========================================================================

   -- positionne le curseur du fader en fonction de la valeur donné par le
   -- control du volume du microphone
   procedure Fader_Follow_Volume is
      tmp : Long_Float;
   begin
       tmp := Long_Float(Win_Audio.Get_Micro_Volume);		-- lecture de la nouvelle valeur
       Objects_Pkg.Set_Fader_Value( FADER_ID, integer(tmp) );	-- positionnement du fader
       Objects_pkg.Display( FADER_ID );				-- mise à jour de l'affichage
   end Fader_Follow_Volume;


   -- **************************************************************************

   procedure Exit_freeze_mode is
   begin
      -- re-valide les boutons
      Objects_pkg.Validate( REC_ID );
      Objects_pkg.Validate( PARAM_ID );
      if not no_midi_device then
         -- seulement si un device MIDi existe
         Objects_pkg.Validate( LOAD_ID );
      end if;
      -- pour le player ce depend de l'état précédent
      if file_loaded then
         Objects_pkg.Validate( PLAY_ID );
         Objects_pkg.Validate( PAUSE_ID );
         Objects_pkg.Validate( STOP_ID );
      end if;
      -- Ascenseur horizontal: curseur à gauche et invalide
      Objects_pkg.Set_Fader_Value( Ascenseur_H_ID, 0 );
      Objects_pkg.Invalidate( Ascenseur_H_ID );
      --
      Affichage_pkg.Set_Decalage( decalage_affichage );
      -- relance l'affichage
      Affichage_pkg.Toggle_pause;
      -- relance le micro
      Process_pkg.End_freeze;
      -- relance le player midi
      if was_playing then
         Midi_pkg.Toggle_Pause;
      end if;
      --
      freezed := false;
   end Exit_freeze_mode;



   procedure Enter_freeze_mode is
      min : integer;
      max : integer;
   begin
      -- mémorise l'état des bouton du player
      file_loaded := Objects_pkg.Is_valid( PLAY_ID );
      -- invalide les boutons
      Objects_pkg.Invalidate( REC_ID );
      Objects_pkg.Invalidate( PARAM_ID );
      Objects_pkg.Invalidate( LOAD_ID );
      Objects_pkg.Invalidate( PLAY_ID );
      Objects_pkg.Invalidate( PAUSE_ID );
      Objects_pkg.Invalidate( STOP_ID );
      -- Pause le player si nécessaire
      was_playing := Midi_pkg.Get_Player_Status = Midi_pkg.playing;
      if was_playing then
         Midi_Pkg.Toggle_Pause;
      end if;
      -- Pause le micro
      Process_pkg.Start_freeze;
      -- supprime le décalage pour le temps réel et le MIDI
      Affichage_pkg.Set_Decalage( 0.0 );
      -- pause l'affichage
      Affichage_pkg.Toggle_Pause;
      -- paramètre l'Ascenseur horizontal
      -- valeur du min
      min := Data_pkg.Get_Min_time;
      -- valeur du max
      max := Data_pkg.Get_Max_Time;
      Objects_pkg.Fader_Set_Min_Max( Ascenseur_H_ID, min, max );
      -- position initial du curseur = valeur max
      Objects_pkg.Set_Fader_Value( Ascenseur_H_ID, min );
      -- valide Ascenseur horizontal
      Objects_pkg.Validate( Ascenseur_H_ID );
      --
      -- flag interne
      freezed := true;
   end Enter_freeze_mode;


   procedure Stop_Midi_Player is
   begin
      if Midi_pkg.Get_player_status /= stopped then
         -- arrête le player midi sans fermer le device
         Midi_pkg.Stop;
         Affichage_pkg.Init_paroles;
         Display_status;
         Update_compteur;
      end if;
   end Stop_Midi_Player;



   procedure Start_Aide is
      dir_len : Integer;
      dir_buf : string(1..512);
      op_str : constant string := "open" & ascii.nul;
      file_str : constant string := "help\index.htm" & ascii.nul;
      htmp : Win32.Windef.HINSTANCE;
   begin
      -- lecture répertoire courant
      dir_len := Integer( Win32.Winbase.GetCurrentDirectory( Win32.DWORD(dir_buf'length),
                                                             TO_LPSTR(dir_buf'address) ));
      htmp := Win32.Shellapi.ShellExecute(
                         hwnd => System.Null_address,
                         lpOperation => TO_LPCSTR( op_str'address ),
                         lpFile => TO_LPCSTR( file_str'address ),
                         lpParameters => null,
                         lpDirectory => TO_PCCH(dir_buf'address),
                         nShowCmd => Win32.Winuser.SW_SHOWNORMAL);
      -- test resultat de la commande
      if To_Integer( htmp ) <= 32 then
         Log.Store_time;
         Log.Store("Start_aide, ShellExecute=" & integer'image(To_Integer( htmp )) );
         Log.End_line;
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_lance_aide );
      end if;
   end Start_Aide;


   procedure Start_web is
      op_str : constant string := "open" & ascii.nul;
      file_str : constant string := Intl.URL_Site & ascii.nul;
      htmp : Win32.Windef.HINSTANCE;
   begin
      htmp := Win32.Shellapi.ShellExecute(
                        hwnd => System.Null_address,
                        lpOperation => TO_LPCSTR( op_str'address ),
                        lpFile => TO_LPCSTR( file_str'address ),
                        lpParameters => null,
                        lpDirectory => null,
                        nShowCmd => Win32.Winuser.SW_SHOWNORMAL);
      -- test resultat de la commande
      if To_Integer( htmp ) <= 32 then
         Log.Store_time;
         Log.Store("Start_web, ShellExecute=" & integer'image(To_Integer( htmp )) );
         Log.End_line;
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_lance_web );
      end if;
   end Start_web;


   -- ==========================================================================
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


   procedure Set_Range_Decal_Transpo is
      value : integer;
   begin
      -- decalage max
      value := Min( (max_midi - Midi_pkg.Max_melody - transposition) / 12, 4 );
      Objects_pkg.Num_Set_Max( NUM_DECAL_ID, value );
      -- decalage min
      value := Max( (min_midi - Midi_pkg.Min_melody - transposition) / 12, -4 );
      Objects_pkg.Num_Set_Min( NUM_DECAL_ID, value );
      -- transposition max
      value := Min( max_midi - Midi_pkg.Max_melody - decalage_octave *12, 11);
      Objects_pkg.Num_Set_Max( NUM_TRANS_ID, value );
      -- transposition min
      value := Max( min_midi - Midi_pkg.Min_melody - decalage_octave *12, -11);
      Objects_pkg.Num_Set_Min( NUM_TRANS_ID, value );
   end Set_Range_Decal_Transpo;


   procedure Set_Decalage is
   begin
      if Midi_pkg.max_melody + transposition + decalage_octave*12 > max_midi then
         decalage_octave := (max_midi - transposition - Midi_pkg.max_melody) / 12;
      elsif Midi_pkg.min_melody + transposition + decalage_octave*12 < min_midi then
         decalage_octave := (min_midi - transposition - Midi_pkg.min_melody) / 12;
      end if;
      -- mis à jour affichage des notes
      Affichage_pkg.Set_Octave( decalage_octave );
      -- mis à jour manuel du score si gelé
      if freezed then
         Affichage_pkg.Update_Score;
      end if;
      -- s'aasure que les num-edit sont toujours dans le bon range
      Set_Range_Decal_Transpo;
   end Set_Decalage;


   procedure Set_Transposition is
   begin
      if Midi_pkg.max_melody + transposition + decalage_octave*12 > max_midi then
         transposition := max_midi - Midi_pkg.max_melody - decalage_octave*12;
      elsif Midi_pkg.min_melody + transposition + decalage_octave*12 < min_midi then
         transposition := min_midi - Midi_pkg.min_melody - decalage_octave*12;
      end if;
      -- on informe le player MIDI
      Midi_pkg.Set_Transpo( transposition );
      -- mis à jour de l'affichage
      Affichage_pkg.Set_Transpo( transposition );
   end Set_Transposition;


   -- ==========================================================================

   procedure Display_version( hdc :  Win32.Windef.HDC ) is
      ombre  : Win32.Windef.Rect;
      res_int : Win32.INT;
      old_obj : Win32.WIndef.HGDIOBJ;
      old_color : Win32.Windef.COLORREF;
      texte : constant string := Config_pkg.version & "-" & Intl.langue;
   begin
      -- écriture version et type (demo ou pas) dans le coin inférieur gauche
      -- police: Times New Roman, 18pt, bold
      old_obj := Win32.Wingdi.SelectObject( hdc, Resources_pkg.Text_font );
      -- fond transparent
      res_int := Win32.Wingdi.SetBkMode(hdc, Win32.Wingdi.TRANSPARENT );
      -- ombre, pen : gris 1 pixel
      -- couleur gris moyen
      old_color := Win32.Wingdi.SetTextColor( hdc, 16#007F7F7F# );
      ombre := Resources_pkg.version_rect;
      -- décalage: 2 pixel à gauche, 1 pixel en haut
      ombre.top := ombre.top + 1;
      ombre.left := ombre.left + 2;
      -- écriture
      res_int := Win32.Winuser.DrawText( hdc,
                         TO_PCCH(texte'address),
                         texte'length,
                         TO_PRECT(ombre'address),
                         Win32.Winuser.DT_BOTTOM + Win32.Winuser.DT_LEFT );
      -- couleur blanc
      old_color := Win32.Wingdi.SetTextColor( hdc, 16#00FFFFFF# );
      -- écriture
      res_int := Win32.Winuser.DrawText( hdc,
                         TO_PCCH(texte'address),
                         texte'length,
                         TO_PRECT(Resources_pkg.version_rect'address),
                         Win32.Winuser.DT_BOTTOM + Win32.Winuser.DT_LEFT );
   end Display_version;


   procedure Paint_main( hwnd : Win32.Windef.HWND; Hdc : Win32.Windef.HDC ) is
      bord_width, bord_height : Integer;
      main_rect : Win32.Windef.Rect;
      largeur, hauteur : Integer;
      res_bool : Win32.BOOL;
   begin
      -- taille de la main window (intérieur)
      res_bool := Win32.Winuser.GetClientRect( hwnd, TO_LPRECT(main_rect'address) );
      largeur := Integer( main_rect.right );
      hauteur := Integer( main_rect.bottom );
      -- le fond
      -- dessin du bitmap de fond de la main
      res_bool := Win32.Wingdi.BitBlt(
                    hdcDest => Main_DC,
                    nXDest  => 0,
                    nYDest  => 0,
                    nWidth  => Win32.INT(main_rect.right),
                    nHeight => Win32.INT(main_rect.bottom),
                    hdcSrc  => Back_dc,
                    nXSrc   => 0,
                    nYSrc   => 0,
                    dwRop   => Win32.Wingdi.SRCCOPY );
      --
      -- taille des bords
      bord_width := Bitmap_pkg.Largeur( MAIN_CHG_ID );
      bord_height := Bitmap_pkg.Hauteur( MAIN_CHG_ID );
      -- les 4 coins
      Bitmap_pkg.Affiche_bitmap( hdc, MAIN_CHG_ID, 0, 0 );	-- coin haut gauche
      Bitmap_pkg.Affiche_bitmap( hdc, MAIN_CHD_ID, Win32.INT(largeur-bord_width), 0 );	-- coin haut droite
      Bitmap_pkg.Affiche_bitmap( hdc, MAIN_CBG_ID, 0, Win32.INT(hauteur - bord_height) );	-- coin bas gauche
      Bitmap_pkg.Affiche_bitmap( hdc, MAIN_CBD_ID, Win32.INT(largeur-bord_width), Win32.INT(hauteur - bord_height) );	-- coin bas droite
      -- les 4 bords
      Bitmap_pkg.Etire_bitmap_H( hdc, MAIN_BH_ID, Win32.INT(bord_width), 0, Win32.INT(largeur-2*bord_width) );	-- bord haut
      Bitmap_pkg.Etire_bitmap_H( hdc, MAIN_BB_ID, Win32.INT(bord_width), Win32.INT(hauteur-bord_height), Win32.INT(largeur-2*bord_width) );	-- bord bas
      Bitmap_pkg.Etire_bitmap_V( hdc, MAIN_BG_ID, 0, Win32.INT(bord_height), Win32.INT(hauteur-2*bord_height) );	-- bord gauche
      Bitmap_pkg.Etire_bitmap_V( hdc, MAIN_BD_ID, Win32.INT(largeur-bord_width), Win32.INT(bord_height), Win32.INT(hauteur-2*bord_height) );	-- bord droite
      --
      Display_Version( hdc );
   end Paint_Main;


   procedure Prepare_midi_file is
      tmp : natural;
   begin
      -- autorise les boutons
      Objects_pkg.Validate( PLAY_ID );
      Objects_pkg.Validate( PAUSE_ID );
      Objects_pkg.Validate( STOP_ID );
      -- initialise les paroles
      Affichage_pkg.Init_paroles;
      Display_status;
      Update_compteur;
      -- affiche le nom du fichier
      Affichage_pkg.Display_file_name;
      -- remise à zéro de la transposition
      transposition := 0;
      -- paramètres les range des num-edit decalage et transposition
      Set_Range_Decal_Transpo;
      -- on informe le player MIDI
      Midi_pkg.Set_Transpo( transposition );
      -- mis à jour de l'affichage
      Affichage_pkg.Set_Transpo( transposition );
      -- repositionne l'ascenseur apres un auto-centrage du à Load_file
      Objects_pkg.Set_Fader_Value( Ascenseur_V_ID, Affichage_pkg.Get_Milieu );
      -- maj intervalles pour les mesures
      tmp := Midi_pkg.Get_last_mesure;
      Objects_pkg.Num_set_min( MESURE_DEBUT_ID, 1);
      Objects_pkg.Num_set_max( MESURE_DEBUT_ID, tmp);
      Objects_pkg.Num_Set_value( MESURE_DEBUT_ID, 1);
      Midi_pkg.Set_Start_mesure( 1 );
      --
      Objects_pkg.Num_set_min( MESURE_FIN_ID, 1);
      Objects_pkg.Num_set_max( MESURE_FIN_ID, tmp);
      Objects_pkg.Num_Set_value( MESURE_FIN_ID, tmp);
      Midi_pkg.Set_End_mesure( tmp );
      -- affichage des valeurs min et max des mesures
      Objects_pkg.Display_num_edit( MESURE_DEBUT_ID );
      Objects_pkg.Display_num_edit( MESURE_FIN_ID );
   end Prepare_midi_file;


   function Window_Proc (hwnd    : Win32.Windef.HWND;
                         message : Win32.UINT;
                         wParam  : Win32.WPARAM;
			 lParam  : Win32.LPARAM)
			 return Win32.LRESULT;
   pragma Convention (Stdcall, Window_Proc);


   function Window_Proc (hwnd    : Win32.Windef.HWND;
                         message : Win32.UINT;
                         wParam  : Win32.WPARAM;
			 lParam  : Win32.LPARAM)
			 return Win32.LRESULT is
      Hdc : Win32.Windef.HDC;
      ps : aliased Win32.Winuser.PAINTSTRUCT;
      res_bool : Win32.BOOL;
      tmp : Integer;
      mouse_x, mouse_y : Win32.SHORT;
      new_mesure : Data_pkg.mesure_event_ptr;
      note : integer;
--      old_curs : Win32.Windef.HCURSOR;
   begin

      case message is

         when Win32.Winuser.WM_LBUTTONUP =>
            case Resources_pkg.Id_of( Integer(wParam) ) is

               when MESURES_ID =>
                   -- lecture position de la souris
                   Conversions.Split_short( Win32.LONG(LParam), hi => mouse_y, low => mouse_x );
                   -- recherche de la mesure correspondant à la position
                   new_mesure := Affichage_pkg.Get_mesure( integer(mouse_x) );
                   -- si on est dans une mesure jouée:
                   if new_mesure /= null then
                      -- sort du mode gel si nécessaire
                      if freezed then
                         Exit_freeze_mode;
                      end if;
                      -- reprogramme le player
                      Midi_pkg.Seek_player( new_mesure.player_time );
                   end if;

               when SCORE_ID =>
                   Midi_pkg.Stop_user_note;

               when others =>
                  drag_mesure := false;
--                  old_curs := Win32.Winuser.SetCursor( Resources_pkg.default_cursor );
                  -- sinon ne rien faire, clic sans intérêt
                  return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);

            end case;
     	    -- reset du flag pour le drag and drop
            drag_mesure := false;
--            old_curs := Win32.Winuser.SetCursor( Resources_pkg.default_cursor );

         when Win32.Winuser.WM_LBUTTONDOWN =>
            if Resources_pkg.Id_of( Integer(wParam) ) = Resources_pkg.SCORE_ID then
               -- lecture position de la souris
               Conversions.Split_short( Win32.LONG(LParam), hi => mouse_y, low => mouse_x );
               -- recherche de la note correspondant à la position
               note := Affichage_pkg.Get_note( integer(mouse_y) );
               -- joue la note jusqu'à buttonup
               Midi_pkg.Play_user_note( note );

            elsif Resources_pkg.Id_of( Integer(wParam) ) = Resources_pkg.MESURES_ID then
                -- lecture position de la souris
                Conversions.Split_short( win32.LONG(LParam), hi => mouse_y, low => mouse_x );
                -- recherche de la mesure correspondant à la position
                new_mesure := Affichage_pkg.Get_mesure( integer(mouse_x) );
                if new_mesure /= null then
                   -- se souvenir du numero de mesure
                   num_mesure := natural(new_mesure.note);
                   drag_mesure := true;
                end if;
--                old_curs := Win32.Winuser.SetCursor( Resources_pkg.drag_cursor );

            else	-- sinon ne rien faire, clic sans intérêt
               return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);
            end if;

         when Win32.Winuser.WM_SIZE =>
            if Win32.Winuser.IsIconic( hwnd ) = 0 then
               -- resized mais pas iconifié
               Layout_pkg.Resize_window;
               -- vérifier que le zoom est ok pour cette taille de fenêtre
               Affichage_pkg.Resize_score;
               if freezed then
                  Affichage_pkg.Update_Score;
               end if;
            else	-- minimized : ne rien faire
               return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);
            end if;


         when Win32.Winuser.WM_TIMER =>
            if not freezed then
               Affichage_pkg.Update_Score( Process_pkg.Card_Clock );
               if Midi_pkg.Is_playing then
                  Affichage_pkg.Update_textes;
                  Update_compteur;
               end if;
            end if;

         when Win32.Winuser.WM_PAINT =>
            Hdc := Win32.Winuser.BeginPaint( hwnd, ps'access );
            Paint_Main( hwnd, hdc );
            res_bool := Win32.Winuser.EndPaint( hwnd, ps'access );
            if freezed then
               Affichage_pkg.Update_Score;
            end if;


	 -- clic sur fermeture de fenetre
	 when Win32.Winuser.WM_DESTROY =>
            -- arrête proprement les timers et ferme les devices ouverts
	    Stop_appli;
	    -- fin du programme par Windows
	    Win32.Winuser.PostQuitMessage (0);

         -- mesage envoyé par le mixeur quand la valeur du control est modifiée
         -- à l'extérieur de l'appli (par SndVol32 par exemple)
         when Win32.Mmsystem.MM_MIXM_CONTROL_CHANGE =>
            if Debug_pkg.Is_set( mixer_debug_flag ) then
               Log.Store_Time;Log.Store( "MM_MIXM_CONTROL_CHANGE Id=" & Win32.LPARAM'image(lparam) );Log.End_Line;
            end if;
            if Win32.DWORD(lParam) = Common_types.Volume_control_Id then
               Fader_Follow_volume;
               return 0;
            else
               return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);
            end if;

         -- Message envoyé par le Fader quand le curseur est déplacé
         when Common_types.FADER_VALUE_CHANGED =>
           case Resources_pkg.Id_of( Integer(wParam) ) is

              when FADER_ID =>
                 tmp := Objects_pkg.Fader_value(FADER_ID);
                 Win_audio.Set_Micro_Volume( Win32.WORD(tmp) );

              when Ascenseur_H_ID =>
                 -- Ascenseur horizontal: temps, seulement en mode freezed
                 if freezed then
                    tmp := Objects_pkg.Fader_value(Ascenseur_H_ID);
                    Affichage_pkg.Update_score( tmp );
                 end if;

              when Ascenseur_V_ID =>
                 -- Ascenseur vertical: notes
                 tmp := Objects_pkg.Fader_value(Ascenseur_V_ID);
                 Affichage_pkg.Scroll_vertical( tmp );

              when VOL_MELODIE_ID =>
                 tmp := Objects_pkg.Fader_value(VOL_MELODIE_ID);
                 Midi_pkg.Set_Melody_volume( tmp );

              when VOL_ACCOMP_ID =>
                 tmp := Objects_pkg.Fader_value(VOL_ACCOMP_ID);
                 Midi_pkg.Set_Accomp_volume( tmp );

              when others => null;
           end case;

         when Common_types.PAINT_STATUS =>
            Display_status;
            Update_compteur;

         -- l'usage a cliqué sur un bouton
         when Common_types.BUTTON_CLICKED =>
            case Resources_pkg.Id_of( Integer(wParam) ) is

               when LOAD_ID =>
                  if Midi_pkg.Get_player_status /= stopped then
                     Stop_Midi_Player;
                  end if;
                  if Midi_pkg.Load_file then
                     Prepare_midi_file;
                  end if;


               when SORTIE_ID =>
                   if sauvegarde_encours then
                      Fin_sauvegarde;
                   end if;
                   Win_Audio.Stop_Lecture;
	           -- fin du programme par Windows
	           Win32.Winuser.PostQuitMessage (0);

               when AIDE_ID => Start_Aide;

               when GOTO_SITE_ID => Start_web;

               when PARAM_ID =>
                   -- arrêt timer et carte son
                   Stop_appli;
                   -- dialog de sélection des paramètres
                   Dialog_pkg.Select_all;
                   -- on stocke la config dans la base de registre
                   Registre_pkg.Store_config;
                   -- on repart
                   Start_Appli;

               when ZOOM_PLUS_ID =>
                   Affichage_pkg.Zoom;
                   Objects_pkg.Set_Fader_Value( Ascenseur_V_ID, Affichage_pkg.Get_Milieu );

               when ZOOM_MOINS_ID =>
                   Affichage_pkg.Un_zoom;
                   Objects_pkg.Set_Fader_Value( Ascenseur_V_ID, Affichage_pkg.Get_Milieu );

               when SCROLL_PLUS_ID =>
                   Affichage_pkg.Scroll_haut;
                   Objects_pkg.Set_Fader_Value( Ascenseur_V_ID, Affichage_pkg.Get_Milieu );

               when SCROLL_MOINS_ID =>
                   Affichage_pkg.Scroll_bas;
                   Objects_pkg.Set_Fader_Value( Ascenseur_V_ID, Affichage_pkg.Get_Milieu );

               when HORIZ_PLUS_ID => Affichage_pkg.Accelerer;

               when HORIZ_MOINS_ID => Affichage_pkg.Ralentir;

               when PLAY_ID =>
                   Midi_pkg.Play(-1);
                   Display_status;

               when STOP_ID =>
                  Stop_Midi_Player;

               when PAUSE_ID =>
                   Midi_pkg.Toggle_Pause;
                   Display_status;

               when REC_ID =>
                   if sauvegarde_encours then
                      Fin_sauvegarde;
                   else
                      Start_sauvegarde;
                   end if;

               when FREEZE_ID =>
                   if freezed then
                      Exit_freeze_mode;
                   else
                      Enter_freeze_mode;
                      Affichage_pkg.Update_Score;
                   end if;

               when CLEAR_ID =>
                   -- message de confirmation ?
                   Data_pkg.Clear_all;
                   Affichage_pkg.Update_score( Process_pkg.Card_Clock );

               when OPTIONS_ID =>
                   if Dialog_pkg.Options then
                      -- change le nom du skin que si le chargement est OK
                      Registre_pkg.Set_current_skin( Dialog_pkg.new_skin.all );
                      -- demande à l'utilisateur de relancer le programme
                      Utils_pkg.Message_box( Intl.Info_titre, Intl.changement_skin );
                   end if;
                   -- change l'instrument de la mélodie
                   if Common_types.Change_instrument then
                      Midi_pkg.Change_instrument;
                   end if;

               when CENTRER_ID =>
                   -- calcul zoom et scroll pour centrer les notes sur l'affichage
                   Affichage_pkg.Auto_centrage( Midi_pkg.min_melody+transposition,
                                                Midi_pkg.max_melody+transposition );
                   -- repositionne l'ascenseur vertical
                   Objects_pkg.Set_Fader_Value( Ascenseur_V_ID, Affichage_pkg.Get_Milieu );

               when others =>
                   return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);

            end case;

         when Common_types.BISTABLE_CHANGED =>
            case Resources_pkg.Id_of( Integer(wParam) ) is
               when LOOP_ID =>
                   -- le bistable lecture en boucle à changer d'état => parameter le player
                   Midi_pkg.Set_mode_loop( Objects_pkg.Bistable_on(LOOP_ID) );

               when others =>
                  return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);
            end case;

         when Common_types.NUM_CHANGED =>
            if drag_mesure then
               -- mode drag'n drop
               drag_mesure := false;
--               old_curs := Win32.Winuser.SetCursor( Resources_pkg.default_cursor );
               --
               case Resources_pkg.Id_of( Integer(wParam) ) is

                  when MESURE_DEBUT_ID =>
                     Objects_pkg.Num_Set_value( MESURE_DEBUT_ID, num_mesure );
                     Objects_pkg.Display_Num_edit( MESURE_DEBUT_ID );
                     -- la fin ne doit pas être avant le début
                     Objects_pkg.Num_Set_Min( MESURE_FIN_ID, num_mesure );
                     -- paramétrage du player
                     Midi_pkg.Set_Start_mesure( num_mesure );

                  when MESURE_FIN_ID =>
                     Objects_pkg.Num_Set_value( MESURE_FIN_ID, num_mesure );
                     Objects_pkg.Display_Num_edit( MESURE_FIN_ID );
                     -- le début ne doit pas être après la fin
                     Objects_pkg.Num_Set_Max( MESURE_DEBUT_ID, num_mesure );
                     -- paramétrage du player
                     Midi_pkg.Set_End_mesure( num_mesure );

                  when others =>
                     return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);
               end case;
            else
               -- mode normal
               case Resources_pkg.Id_of( Integer(wParam) ) is

                  when MESURE_DEBUT_ID =>
                     -- lecture de la valeur
                     tmp := Objects_pkg.Num_Get_value(MESURE_DEBUT_ID);
                     -- paramétrage du player
                     Midi_pkg.Set_Start_mesure( tmp );
                     -- la fin ne doit pas être avant le début
                     Objects_pkg.Num_Set_Min( MESURE_FIN_ID, tmp );

                  when MESURE_FIN_ID =>
                     -- lecture de la valeur
                     tmp := Objects_pkg.Num_Get_value(MESURE_FIN_ID);
                     -- paramétrage du player
                     Midi_pkg.Set_End_mesure( tmp );
                     -- le début ne doit pas être après la fin
                     Objects_pkg.Num_Set_Max( MESURE_DEBUT_ID, tmp );

                  when NUM_DECAL_ID =>
                     -- decalage d'octave
                     decalage_octave := Objects_pkg.Num_Get_value( NUM_DECAL_ID );
                     Set_Decalage;
                     Objects_pkg.Num_Set_value( NUM_DECAL_ID, decalage_octave );
                     Objects_pkg.Display_Num_edit( NUM_DECAL_ID );


                  when NUM_TRANS_ID =>
                     -- transposition
                     transposition := Objects_pkg.Num_Get_value( NUM_TRANS_ID );
                     Set_Transposition;
                     Objects_pkg.Num_Set_value( NUM_TRANS_ID, transposition );
                     Objects_pkg.Display_Num_edit( NUM_TRANS_ID );

                  when others =>
                     return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);
               end case;
            end if;

            when Common_types.PLAYER_RESET =>
                -- réaffiche les paroles après le seek
                Affichage_pkg.Reset_paroles;
                -- met à jour le compteur
                Display_status;


	 -- autres
         when others =>
            return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);

      end case;

      return 0;

   exception
      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_exception );
         -- fin du programme par Windows
	 Win32.Winuser.PostQuitMessage (0);
         return 1;
   end Window_Proc;


   -- ====================================================================================

   procedure Create_Main_Window  is
      err_code : Win32.DWORD;
      titre : constant string := Config_pkg.Appli_name & " - " & Config_pkg.version  & ASCII.Nul;
   begin
      -- Création de la main Window
      Common_types.Win_Hwnd := Win32.Winuser.CreateWindow (
        lpClassName  => Conversions.TO_PCCH(Common_types.APPCLASS'address),
        lpWindowName => Conversions.TO_PCCH(titre'address),
        dwStyle      => Win32.Winuser.WS_OVERLAPPEDWINDOW,
        X            => Win32.Winuser.CW_USEDEFAULT,
        Y            => Win32.Winuser.CW_USEDEFAULT,
        nWidth       => Win32.INT(Layout_pkg.Window_width),
        nHeight      => Win32.INT(Layout_pkg.Window_height),
        hWndParent   => System.Null_Address,
        hMenu        => System.Null_Address,
        hInstance    => Common_types.hInst,
        lpParam      => System.Null_Address);

      -- test résultat de l'appel
      if Common_types.Win_Hwnd = System.Null_Address then
         err_code := Win32.Winbase.GetLastError;
         Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_win_txt & "Main, code="  & Win32.DWORD'image(err_code) );
         raise fatal_error;
      end if;

      if Bitmap_pkg.Bitmap_of( MAIN_BACKGROUND_ID ) /= null then
         Main_DC := Win32.Winuser.GetDC(Common_types.Win_Hwnd);
         -- crée un DC pour le background
         Common_types.Back_DC := Win32.Wingdi.CreateCompatibleDC( Main_DC );
         -- crée la bitmap du background
         Bitmap_pkg.Create_background;
      end if;

   end Create_Main_Window;


   procedure Init_UI is
      result : Win32.BOOL;
   begin
      -- création de la fenêtre principale en premier
      Create_Main_Window;
      -- création window Data pour le traitement des données audio
      Process_pkg.Create_Data_Win;
      -- positionnement des objets d'interface
      Layout_pkg.Init_Layout;
      --
      -- Invalide les boutons du player
      Objects_pkg.Invalidate( PLAY_ID );
      Objects_pkg.Invalidate( PAUSE_ID );
      Objects_pkg.Invalidate( STOP_ID );
      -- Ascenseur horizontal: curseur à gauche et invalide
      Objects_pkg.Set_Fader_Value( Ascenseur_H_ID, 1000 );
      Objects_pkg.Invalidate( Ascenseur_H_ID );
      -- Ascenseur vertical: min_midi et max_midi, curseur au centre
      Objects_pkg.Fader_Set_Min_Max( Ascenseur_V_ID, Common_types.Min_midi, Common_types.Max_Midi );
      Objects_pkg.Set_Fader_Value( Ascenseur_V_ID, Affichage_pkg.Get_Milieu );
      -- volume accompagnement et melodie: 50%
      Objects_pkg.Set_Fader_Value( VOL_MELODIE_ID, 50 );
      Objects_pkg.Set_Fader_Value( VOL_ACCOMP_ID, 50 );
      Midi_pkg.Set_Melody_volume( 50 );
      Midi_pkg.Set_Accomp_volume( 50 );
      -- Volume MIDI au max
      Win_audio.Set_Volume_Max;
      --
      Affichage_pkg.Set_Decalage( decalage_affichage );
      --
      Objects_pkg.Validate( REC_ID );
      Objects_pkg.Validate( FREEZE_ID );
      Objects_pkg.Validate( CLEAR_ID );
      -- affichage de la version
      Display_Version( Win32.Winuser.GetDC(Common_types.Win_Hwnd) );
      -- rend la fenetre visible
      Result := Win32.Winuser.ShowWindow (Common_types.Win_Hwnd, Win32.Winmain.Get_nCmdShow);
      Result := Win32.Winuser.UpdateWindow (Common_types.Win_Hwnd);

   end Init_UI;

   -- =================================================================================


   procedure Create_Main_Class is
      Wnd_Class : Win32.Winuser.WNDCLASS;
      res_atom  : Win32.Windef.ATOM;
      err_code : Win32.DWORD;
      name : constant string := "ICON" & ascii.nul;
   begin
      --
      -- classe de la main window
      --
      Wnd_Class.style := Win32.Winuser.CS_HREDRAW or Win32.Winuser.CS_VREDRAW or Win32.Winuser.CS_OWNDC;
      Wnd_Class.lpfnWndProc := Window_Proc'Access;
      Wnd_Class.cbClsExtra := 0;
      Wnd_Class.cbWndExtra := 0;
      Wnd_Class.hInstance  := hInst;
      -- chargement de l'icone à partir des resources ('win_resources.o' créé avec 'windres.exe')
      Wnd_Class.hIcon := LoadImage( Common_types.hInst, TO_LPCSTR(name'address), IMAGE_ICON, 0, 0, LR_DEFAULTCOLOR );
      Wnd_Class.hCursor := Resources_pkg.default_cursor;
      Wnd_Class.hbrBackground := System.Null_address;
      Wnd_Class.lpszClassName := Conversions.TO_PCCH(Common_types.APPCLASS'address);
      -- enregistrement de la classe
      res_atom := Win32.Winuser.RegisterClass( TO_LPWNDCLASS(Wnd_Class'address) );
      --
      if res_atom = 0 then
         err_code := Win32.Winbase.GetLastError;
         Utils_pkg.Error_box( Intl.err_class_title, Intl.err_class_txt & Win32.DWORD'image(err_code) );
         raise fatal_error;
      end if;
      --
   end Create_Main_Class;


   -- Création des classe pour les objets windows
   procedure Init_Instance is
      def_cur_num : constant Win32.Long := 1;
   begin
      -- récupére l'instance de l'application
      Common_types.hInst := Win32.Winmain.Get_hInstance;
      --
      -- Chargement des ressources
--      Resources_pkg.drag_cursor := LoadImage( Common_types.hInst, TO_LPCSTR(def_cur_num'address), IMAGE_CURSOR, 0, 0, LR_DEFAULTCOLOR );
--      Resources_pkg.drag_cursor := Win32.Winuser.LoadCursor( Common_types.hInst, TO_LPCSTR(def_cur_num'address));
      Resources_pkg.default_cursor := Win32.Winuser.LoadCursor ( System.Null_Address, Win32.LPCSTR(IDC_ARROW) );
      --
      -- création des classes
      --
      Create_Main_Class;
      Process_Pkg.Create_Data_Class;
      Objects_pkg.Create_Objects_Class;
      --
   end Init_Instance;


   procedure Start_appli is
   begin
      -- flag pour redémarrage: ne pas tenir compte du 1er buffer envoyé par la carte
      Common_types.restarting := true;
      -- taille de buffer correspondant à environ 10 fps
      if Common_types.Fs = 44100 then
         Common_types.buff_size := 4096;	-- 44100 / 4096 = 10.76
      elsif Common_types.Fs = 22050 then
         Common_types.buff_size := 2048;	-- 22050 / 2048 = 10.76
      else -- 11025
         Common_types.buff_size := 1024;	-- 11025 / 1024 = 10.76
      end if;
      -- initialisation buffer filtrage
      Process_pkg.Init_data;
      -- ouverture du mixer
      if not Win_Audio.Open_mixer then
         -- si pas OK, proposer Sndvol32.exe
         Dialog_pkg.Launch_sndvol;
      else
         -- positionnement du fader en fonction du volume microphone
         User_Interface.Fader_Follow_Volume;
      end if;
      -- démarre la lecture du micro
      Win_Audio.Start_Lecture;
      -- démarreg du timer interne
      Timer_pkg.Start;
      -- ouverture du device MIDI
      Midi_pkg.Open_midi_device;
   end Start_appli;


   procedure Stop_appli is
   begin
      -- arrêt du player midi
      Stop_Midi_Player;
      -- fermeture du device midi
      Midi_pkg.Close_midi_device;
      -- arrêt du timer
      Timer_pkg.Stop;
      -- arrêt de la lecture du micro
      Win_Audio.Stop_lecture;
      -- fermeture du mixer
      Win_Audio.Close_Mixer;
      -- raz des données d'affichage
      Data_pkg.Clear_all;
   end Stop_appli;


   procedure Invalidate_player is
   begin
      Objects_pkg.Invalidate( LOAD_ID );
   end Invalidate_player;

end User_Interface;

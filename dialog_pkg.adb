with Unchecked_deallocation;
with System;		use System;
with Interfaces.C;	use Interfaces.C;
with GNAT.Current_Exception;
with GNAT.Case_Util;

with Win32;		use Win32;
with Win32.Windef;
with Win32.Mmsystem;	use Win32.Mmsystem;
with Win32.Winmain;
with Win32.Winuser;	use Win32.Winuser;
with Win32.Winnls;
with Win32.Commdlg;
with Win32.Winnt;
with Win32.Winbase;
with Win32.Windowsx;
with Win32.Shellapi;

with Common_types;	use Common_types;
with Conversions;	use Conversions;
with Template_Pkg;
with Intl;
with Registre_pkg;
with Utils_pkg;		use Utils_pkg;
with midifile_pkg;	use midifile_pkg;
with Intl;
with Resources_pkg;
with General_midi;
with Log;
with Debug_pkg;
with Config_pkg;
with Skins_pkg;


package body Dialog_pkg is

   -- Template
   ID_TEXT_1 : constant := 200;
   ID_TEXT_2 : constant := 201;
   ID_TEXT_3 : constant := 202;
   ID_TEXT_4 : constant := 203;
   ID_TEXT_5 : constant := 204;
   ID_SIGNATURE : constant := 205;
   ID_COMBO_MICRO : constant := 300;
   ID_COMBO_MIXER : constant := 301;
   ID_COMBO_FS    : constant := 302;
   ID_COMBO_MIDI  : constant := 303;
   ID_EDIT_1      : constant := 400;
   ID_BOUT_COPIER : constant := 500;
   ID_BOUT_LANCER : constant := 600;

   -- Options: Id des combos
   ID_AUTOVOLUME : constant := 300;	-- volume automatique O/N
   ID_CHANGEINST : constant := 301;	-- change instrument O/N
   ID_INSTRNAME  : constant := 302;	-- combo nom de l'instrument
   ID_INSTRFAMI  : constant := 303;	-- combo famille d'instrument
   ID_SKINNAME   : constant := 304;	-- nom de skin

   Str_44K : constant String := "44100" & ASCII.NUl;
   Str_22K : constant String := "22050" & ASCII.NUl;
   Str_11K : constant String := "11025" & ASCII.NUl;

   max_track_name_length : constant := 25;

   -- les objets aliased doivent être globaux !!!
   --
   opfn : aliased Win32.Commdlg.OPENFILENAME;
   -- capacités pour les Device MIDI
   midi_caps : aliased Win32.Mmsystem.MIDIOUTCAPSA;

   inconnu_txt : string_pt := new string'("???");
   vide_txt    : string_pt := new string'("");

   nbr_skins : natural := 0;
   default_skin : natural := 1;	-- stocke la valeur du skin lors de l'appel de Options

   -- flag de debug
   mixer_flag : constant string := "mixer";

   -- nombre de devices WaveIn et MidiOut utilisables
   nbr_micro, nbr_devices : Win32.UINT := 0;

   type uint_rec;
   type uint_rec_ptr is access uint_rec;
   type uint_rec is record
      Id : Win32.UINT;
      next : uint_rec_ptr;
   end record;
   procedure Free is new Unchecked_deallocation( uint_rec, uint_rec_ptr );

   list_wavein  : uint_rec_ptr;
   list_midiout : uint_rec_ptr;

   -- ************************************************************************

   -- procedures de dialog
   -- sélection paramètre
   function Dialog_Proc_All( hwnd : Win32.Windef.HWND;
                         msg : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM)
                                  return Win32.BOOL;
   pragma Convention (Stdcall, Dialog_Proc_All);

   -- sélection piste MIDi de la mélodie
   function Dialog_Proc_Track( hwnd : Win32.Windef.HWND;
                         msg : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM)
                                  return Win32.BOOL;
   pragma Convention (Stdcall, Dialog_Proc_Track);

   -- saisie code autorisation
   function Dialog_proc_Auto( hwnd : Win32.Windef.HWND;
                         msg : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM)
                                  return Win32.BOOL;
   pragma Convention (Stdcall, Dialog_proc_Auto);


   -- dialog procedure pour les options
   function Option_proc( hwnd : Win32.Windef.HWND;
                         msg : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM)
                                  return Win32.BOOL;
   pragma Convention (Stdcall, Option_proc);

   function Dialog_proc_Sndvol( hwnd : Win32.Windef.HWND;
                         msg : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM) return Win32.BOOL;
   pragma Convention (Stdcall, Dialog_proc_Sndvol);


   -- =========================================================================

   procedure Create_device_list is
      next_ptr : uint_rec_ptr;
      nbr_wi, nbr_mo : Win32.UINT;
      Result  : Win32.Mmsystem.MMRESULT;
      in_caps   : Win32.Mmsystem.WAVEINCAPS ;
   begin
      -- suppression des anciennes listes
      while list_wavein /= null loop
         next_ptr := list_wavein.next;
         Free( list_wavein );
         list_wavein := next_ptr;
      end loop;
      nbr_micro := 0;
      --
      while list_midiout /= null loop
         next_ptr := list_midiout.next;
         Free( list_midiout );
         list_midiout := next_ptr;
      end loop;
      nbr_devices := 0;
      --
      -- Liste des WaveIn
      --
      -- nombre de wave in
      nbr_wi := Win32.Mmsystem.waveInGetNumDevs;
      for wi in 0..nbr_wi-1 loop
         -- lecture des capaciés du device
         result := Win32.Mmsystem.waveInGetDevCaps( wi,
			TO_LPWAVEINCAPS(in_caps'address), WAVEINCAPS'size/8);
         -- si pas d'erreur...
         if result = Win32.Mmsystem.MMSYSERR_NOERROR then
            -- si le Format = 0, on ne peux rien faire avec ce device !
            if in_caps.dwFormats /= 0 then
               -- ajoute dans la liste
               next_ptr := new uint_rec'( wi, list_wavein );
               list_wavein := next_ptr;
               -- compte le nombr de wavein (i.e. microphone)
               nbr_micro := nbr_micro + 1;
            end if;
         end if;
      end loop;
      if Debug_pkg.Is_set( mixer_flag ) then
         Log.Store( "Nombre de micros:" & Win32.UINT'image(nbr_micro) );
         Log.End_Line;
      end if;
      -- si pas de microphone, pas utilisable
      if nbr_micro = 0 then
         Utils_pkg.Error_box( Intl.err_micro_title, Intl.err_micro_txt );
         raise fatal_error;
      end if;
      --
      -- Liste des MidiOut
      --
      nbr_mo := Win32.Mmsystem.midiOutGetNumDevs;
      for mo in 0..nbr_mo-1 loop
         -- lecture des capacités du device
         result := Win32.Mmsystem.midiOutGetDevCaps( Win32.UINT(mo),
                        midi_caps'access, MIDIOUTCAPSA'size / 8 );
         -- si pas d'erreur...
         if result = Win32.Mmsystem.MMSYSERR_NOERROR then
            -- on ne prend pas les Port ni ceux qui ont des voix à 0
            if midi_caps.wTechnology /= MOD_MIDIPORT and midi_caps.wVoices > 0 then
               -- ajoute dans la liste
               next_ptr := new uint_rec'( mo, list_midiout );
               list_midiout := next_ptr;
               -- compte le nombre de MidiOut utilisables
               nbr_devices := nbr_devices + 1;
            end if;
         end if;
      end loop;
      --
      if Debug_pkg.Is_set( mixer_flag ) then
         Log.Store( "Nombre de MidiOut:" & Win32.UINT'image(nbr_devices) );
         Log.End_Line;
      end if;
      --
      if nbr_devices = 0 then
         Utils_pkg.Error_box(  Intl.err_midi_title, Intl.err_midi_dev_txt);
         no_midi_device := true;
      else
         no_midi_device := false;
      end if;
      --
   end Create_device_list;


   function Create_Template_All return Template_pkg.template_type is
      my_template : Template_pkg.template_type;
   begin
      -- creation des listes de devices utilisables
      Create_device_list;
      --
      -- Création de la template
      my_template := Template_pkg.New_template( 230, 107, Intl.dlg_param_title );
      -- Item 1 : Bouton OK
      Template_Pkg.Add_Item( my_template, 73, 80, 71, 16, Win32.Winuser.IDOK, Intl.dlg_ok_txt, Template_pkg.Button );
      -- Item 2 : Label Microphone
      Template_Pkg.Add_Item( my_template, 12, 15, 43, 12, ID_TEXT_1, Intl.dlg_micro_txt, Template_pkg.Static );
      -- Item 3 : ComboBox Microphone
      Template_Pkg.Add_Item( my_template, 67, 15, 135, Win32.SHORT(18 * nbr_micro), ID_COMBO_MICRO, "", Template_pkg.Combo_box );
      -- Item 4 : Label Fréquence
      Template_Pkg.Add_Item( my_template, 12, 35, 43, 12, ID_TEXT_3, Intl.dlg_freq_txt, Template_pkg.Static );
      -- Item 5 : ComboBox Fréquence
      Template_Pkg.Add_Item( my_template, 67, 35, 135, Win32.SHORT(18 * 3), ID_COMBO_FS, "", Template_pkg.Combo_box );
      -- Item 6 : Label Device Midi
      Template_Pkg.Add_Item( my_template, 12, 55, 43, 12, ID_TEXT_4, Intl.dlg_device_txt, Template_pkg.Static );
      if nbr_devices > 0 then
         -- Item 7 : ComboBox DeviceMidi
         Template_Pkg.Add_Item( my_template, 67, 55, 135, Win32.SHORT(18 * nbr_devices), ID_COMBO_MIDI, "", Template_pkg.Combo_box );
      else
         -- Item 7 : Label : pas de device
         Template_Pkg.Add_Item( my_template, 67, 55, 43, 12, ID_TEXT_5, Intl.dlg_no_midi_txt, Template_pkg.Static );
      end if;
      --
      return  my_template;
   end Create_Template_All;


   -- ----------------------------------------------------------------------------------------


   -- centrage de la Dialog box par rapport à la fenêtre principale
   procedure Centre_Dialog( Dialog : Win32.Windef.HWND ) is
      res_bool : Win32.BOOL;
      parent_rect, dialog_rect : Win32.Windef.RECT;
      X, Y : Win32.INT;
   begin
      -- lecture des rectangles des deux fenetres
      res_bool := Win32.Winuser.GetWindowRect( Common_types.Win_Hwnd, TO_LPRECT(parent_rect'address) );
      res_bool := Win32.Winuser.GetWindowRect( Dialog, TO_LPRECT(dialog_rect'address) );
      -- calcul de la nouvelle position
      X := Win32.INT((parent_rect.left + parent_rect.right - dialog_rect.right + dialog_rect.left) / 2);
      Y := Win32.INT((parent_rect.bottom + parent_rect.top - dialog_rect.bottom + dialog_rect.top)/2);
      -- déplacement
      res_bool := Win32.Winuser.SetWindowPos( Dialog,
                                Win32.Winuser.HWND_TOP,
                                X, Y,
                                0, 0,
                                Win32.Winuser.SWP_NOSIZE );
   end Centre_Dialog;


   -- ========================================================================================


   -- procedure de traitement des messages Windows de la dialog box
   function Dialog_Proc_All( hwnd   : Win32.Windef.HWND;
                         msg    : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM)  return Win32.BOOL is
      in_caps         : Win32.Mmsystem.WAVEINCAPS ;
      res_bool        : Win32.BOOL;
      wm_Id           : Win32.WORD;
      notif_msg       : Win32.WORD;
      hwnd_combo_micro,
      hwnd_combo_freq,
      hwnd_combo_midi : Win32.Windef.HWND;
      result          : Win32.Mmsystem.MMRESULT;
      resu_long       : Win32.LRESULT;
      freq_num        : Integer;
      cur_sel         : Integer;
      cur_sel_long    : Win32.LRESULT;
      capacite        : WAVEINCAPS;	-- Win32.Mmsystem:1502
      fin             : integer;
      tmp             : integer;
      dev_ptr         : uint_rec_ptr;
      cur_dev,
      dev_count       : Win32.UINT;
   begin
      case msg is

         when WM_INITDIALOG =>
            -- centrage de la fentre du dialog
            Centre_Dialog( hwnd );
            --
            -- Initialisation du combo micro
            --
            hwnd_combo_micro := GetDlgItem( hwnd, ID_COMBO_MICRO );
            dev_ptr := list_wavein;
            cur_dev := 0;
            dev_count := 0;
            while dev_ptr /= null loop
	       Result := Win32.Mmsystem.waveInGetDevCaps( dev_ptr.Id,
					  TO_LPWAVEINCAPS(in_caps'address), WAVEINCAPS'size/8);
               Utils_pkg.Check_erreur( Result, "Dialog_Proc_All.INITDIALOG: waveInGetDevCaps-1");
               resu_long := SendMessage( hwnd_combo_micro, CB_ADDSTRING, 0,
                                          TO_LPARAM(in_caps.szPname'address) );
               if Debug_pkg.Is_set( mixer_flag ) then
                  Log.Store( integer(dev_ptr.Id) ); Log.Store_C( in_caps.szPname );
                  Log.End_Line;
               end if;
               --
               if dev_ptr.Id = Common_types.micro_num then
                  cur_dev := dev_count;	-- position du device courant dans la liste
               end if;
               dev_count := dev_count + 1;
               --
               dev_ptr := dev_ptr.next;
            end loop;
            -- sélection valeur initiale: le courant
            resu_long := SendMessage( hwnd_combo_micro, CB_SETCURSEL, win32.WPARAM(cur_dev), 0 );
            --
            -- Initialisation du combo frequence
            --
            hwnd_combo_freq := GetDlgItem( hwnd, ID_COMBO_FS );
            -- lecture des capacités du microphone
            result :=  Win32.Mmsystem.waveInGetDevCaps( Common_types.micro_num,
                              TO_LPWAVEINCAPS( capacite'address ),
                              (Win32.Mmsystem.WAVEINCAPS'size) / 8 );
            Check_Erreur( result, "Dialog_proc_all.INITDIALOG: waveInGetDevCaps-2" );
            --
            cur_sel := 0;
            freq_num := 0;
            if (capacite.dwFormats and Win32.Mmsystem.WAVE_FORMAT_4M16) /= 0 then
                resu_long := SendMessage( hwnd_combo_freq, CB_ADDSTRING, 0,
                                          TO_LPARAM(Str_44K'address) );
                if Common_types.fs = 44100 then
                   freq_num := cur_sel;
                end if;
                cur_sel := cur_sel + 1;
            end if;
            if (capacite.dwFormats and Win32.Mmsystem.WAVE_FORMAT_2M16) /= 0 then
                resu_long := SendMessage( hwnd_combo_freq, CB_ADDSTRING, 0,
                                          TO_LPARAM(Str_22K'address) );
                if Common_types.fs = 22050 then
                   freq_num := cur_sel;
                end if;
                cur_sel := cur_sel + 1;
            end if;
            if (capacite.dwFormats and Win32.Mmsystem.WAVE_FORMAT_1M16) /= 0 then
                resu_long := SendMessage( hwnd_combo_freq, CB_ADDSTRING, 0,
                                          TO_LPARAM(Str_11K'address) );
                if Common_types.fs = 11025 then
                   freq_num := cur_sel;
                end if;
            end if;
            -- sélection valeur initiale: la valeur courante de Fs
            resu_long := SendMessage( hwnd_combo_freq, CB_SETCURSEL, Win32.WPARAM(freq_num), 0 );
            --
            -- Initialisation ComboBox Midi device
            --
            hwnd_combo_midi := GetDlgItem( hwnd, ID_COMBO_MIDI );
            dev_ptr := list_midiout;
            cur_dev := 0;
            dev_count := 0;
            while dev_ptr /= null loop
               -- lecture 'capacites' du device, dont son nom
               result := Win32.Mmsystem.midiOutGetDevCaps( dev_ptr.Id, midi_caps'access, MIDIOUTCAPSA'size / 8 );
               if Result /= Win32.Mmsystem.MMSYSERR_NOERROR then
                  Log.Midiout_error( result );
               end if;
               -- ajout dans la combobox
               resu_long := SendMessage( hwnd_combo_midi, CB_ADDSTRING, 0,
                                          TO_LPARAM(midi_caps.szPname'address) );
               if dev_ptr.Id = Common_types.midi_num then
                  cur_dev := dev_count;	-- position du device courant dans la liste
               end if;
               dev_count := dev_count + 1;
               --
               dev_ptr := dev_ptr.next;
            end loop;
            -- sélection valeur initiale: le courant
            resu_long := SendMessage( hwnd_combo_midi, CB_SETCURSEL, win32.WPARAM(cur_dev), 0 );
            --
            -- doit toujours retourner 1 si INITDIALOG
            return 1;


         when WM_COMMAND =>
	    -- lecture de l'Id
            wm_ID     := Win32.Windef.LOWORD (Win32.DWORD (wParam));

            case wm_ID is

               when IDOK =>
                  -- lecture des handle des combobox
                  hwnd_combo_micro := GetDlgItem( hwnd, ID_COMBO_MICRO );
                  hwnd_combo_freq  := GetDlgItem( hwnd, ID_COMBO_FS );
                  hwnd_combo_midi := GetDlgItem( hwnd, ID_COMBO_MIDI );
                  --
                  -- lecture des valeurs sélectionnées
                  --
                  -- Microphone numéro et string
                  --
                  tmp := Integer(SendMessage( hwnd_combo_micro, CB_GETCURSEL, 0, 0 ));
                  dev_ptr := list_wavein;
                  for i in 0..tmp-1 loop
                     dev_ptr := dev_ptr.next;
                  end loop;
                  Common_types.micro_num := dev_ptr.Id;
                  --
                  Result := Win32.Mmsystem.waveInGetDevCaps( Common_types.micro_num, TO_LPWAVEINCAPS(in_caps'address), WAVEINCAPS'size/8);
                  Check_Erreur( result, "Dialog_proc_all.WMCOMMAND: waveInGetDevCaps" );
                  -- recherche fin de string
                  fin := 0;
                  while fin <= 31 and then in_caps.szPname(fin) /= Win32.Nul loop
                     fin := fin + 1;
                  end loop;
                  Free( Common_types.micro_str );
                  Common_types.micro_str := new string(1..fin);
                  for j in 1..fin loop
                     Common_types.micro_str(j) := Conversions.TO_CHARACTER(in_caps.szPname(j-1));
                  end loop;
                  --
                  -- Device MIDI : numéro et string
                  --
                  if not no_midi_device then
                     tmp := Integer(SendMessage( hwnd_combo_midi, CB_GETCURSEL, 0, 0 ));
                     dev_ptr := list_midiout;
                     for i in 0..tmp-1 loop
                        dev_ptr := dev_ptr.next;
                     end loop;
                     Common_types.midi_num := dev_ptr.Id;
                     --
                     result := Win32.Mmsystem.midiOutGetDevCaps( Common_types.midi_num, midi_caps'access, MIDIOUTCAPSA'size / 8 );
                     Check_Erreur( result, "Dialog_proc_all.WMCOMMAND: midiOutGetDevCaps" );
                     -- recherche fin de string
                     fin := 0;
                     while fin <= 31 and then midi_caps.szPname(fin) /= Win32.Nul loop
                        fin := fin + 1;
                     end loop;
                     Free( Common_types.midi_str );
                     Common_types.midi_str := new string(1..fin);
                     for j in 1..fin loop
                        Common_types.midi_str(j) := Conversions.TO_CHARACTER(midi_caps.szPname(j-1));
                     end loop;
                  end if;
                  --
                  -- Fréquence
                  --
                  cur_sel_long := SendMessage( hwnd_combo_freq, CB_GETCURSEL, 0, 0 );
                  resu_long := SendMessage( hwnd_combo_freq, CB_GETLBTEXTLEN, Win32.WPARAM(cur_sel_long), 0 );
                  declare
                     buffer : string(1..Integer(resu_long)+1);
                  begin
                     resu_long := SendMessage( hwnd_combo_freq, CB_GETLBTEXT,  Win32.WPARAM(cur_sel_long),
                                            TO_LPARAM(buffer'address));
                     if buffer = Str_44K then
                        -- fréquence sélectionnée
                        Common_types.Fs := 44100;
                     elsif Buffer = Str_22K then
                        Common_types.Fs := 22050;
                     elsif buffer = Str_11K then
                        Common_types.Fs := 11025;
                     else
                        raise fatal_error;
                     end if;
                  end;
                  -- Fin du dialog
                  res_bool := Win32.Winuser.EndDialog( hwnd, IDOK );
                  return 1;

               when ID_COMBO_MICRO =>
                  --
                  notif_msg := Win32.Windef.HIWORD (Win32.DWORD (wParam));
                  if notif_msg = CBN_SELCHANGE then
                     -- sélection d'un nouveau microphone: il faut lire ses capacités et
                     -- recréer la liste des fréquences possibles
                     --
                     -- lecture du numéro du micro sélectionné
                     tmp := Integer(SendMessage( hwnd_combo_micro, CB_GETCURSEL, 0, 0 ));
                     dev_ptr := list_wavein;
                     for i in 0..tmp-1 loop
                        dev_ptr := dev_ptr.next;
                     end loop;
                     Common_types.micro_num := dev_ptr.Id;
                     -- lecture des capacités du microphone
                     result :=  Win32.Mmsystem.waveInGetDevCaps( Common_types.micro_num,
                              TO_LPWAVEINCAPS( capacite'address ),
                              (Win32.Mmsystem.WAVEINCAPS'size) / 8 );
                     Check_Erreur( result, "Dialog_proc_all.COMBO_MICRO: waveInGetDevCaps" );
                     -- reset du combo
                     resu_long := SendMessage( hwnd_combo_freq, CB_RESETCONTENT, 0, 0);
                     -- création de la nouvelle liste
                     cur_sel := 0;
                     freq_num := 0;
                     -- 44 100 Hz ?
                     if (capacite.dwFormats and Win32.Mmsystem.WAVE_FORMAT_4M16) /= 0 then
                        -- oui, on ajoute
                        resu_long := SendMessage( hwnd_combo_freq, CB_ADDSTRING, 0,
                                          TO_LPARAM(Str_44K'address) );
                        -- est-ce la valeur en cours ?
                        if Common_types.fs = 44100 then
                           -- oui on la garde pour l'affichage
                           freq_num := cur_sel;
                        end if;
                        cur_sel := cur_sel + 1;
                     end if;
                     -- 22 050 Hz ?
                     if (capacite.dwFormats and Win32.Mmsystem.WAVE_FORMAT_2M16) /= 0 then
                        -- oui, on ajoute
                         resu_long := SendMessage( hwnd_combo_freq, CB_ADDSTRING, 0,
                                          TO_LPARAM(Str_22K'address) );
                         if Common_types.fs = 22050 then
                            freq_num := cur_sel;
                         end if;
                         cur_sel := cur_sel + 1;
                     end if;
                     -- 11025 Hz ?
                     if (capacite.dwFormats and Win32.Mmsystem.WAVE_FORMAT_1M16) /= 0 then
                        -- oui, on ajoute
                         resu_long := SendMessage( hwnd_combo_freq, CB_ADDSTRING, 0,
                                          TO_LPARAM(Str_11K'address) );
                         if Common_types.fs = 11025 then
                            freq_num := cur_sel;
                         end if;
                     end if;
                     -- sélection valeur initiale
                     resu_long := SendMessage( hwnd_combo_freq, CB_SETCURSEL, Win32.WPARAM(freq_num), 0 );

                  end if;
                  return 0;

               when others =>
                  return 0;

            end case;

	 -- autres
         when others =>
           return 0;

       end case;

   exception
      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
         Log.End_Log;
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_exception );
         -- fin du programme par Windows
         Win32.Winuser.PostQuitMessage (0);
         return 1;
   end Dialog_Proc_All;


   -- ******************************************************************************


   -- création de la dialog box All
   procedure Select_All is
      my_template : Template_pkg.template_type;
      res_int : Win32.INT;
   begin
      -- création du template
      my_template := Create_Template_All;
      -- création et affichage de la dialog box
      Res_int := Win32.Winuser.DialogBoxIndirect(
                             Common_types.hInst,
                             Template_Pkg.Get_Template(my_Template),
                             Common_types.Win_hwnd,
                             Dialog_proc_All'access );
      if res_int = -1 then
         Utils_pkg.Log_Windows_error( "Dialog_pkg.Select_all: DialogBoxIndirect");
      end if;
      -- libération mémoire utilisée par la template
      Template_Pkg.Free(my_template);
   end Select_All;


   -- ******************************************************************************


   function Dialog_proc_Sndvol( hwnd : Win32.Windef.HWND;
                         msg : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM) return Win32.BOOL is
      wm_ID : Win32.WORD;
      res_bool : Win32.BOOL;
      h_ins : Win32.Windef.HINSTANCE;
      ope : constant string := "open" & ascii.nul;
      file : constant string := "sndvol32.exe" & ascii.nul;
   begin
      case msg is

         when WM_INITDIALOG =>
            -- centrage de la fentre du dialog
            Centre_Dialog( hwnd );
            -- doit toujours retourner 1 si INITDIALOG
            return 1;


         when WM_COMMAND =>
	    -- lecture de l'Id
            wm_ID := Win32.Windef.LOWORD (Win32.DWORD (wParam));

            case wm_ID is

                when IDOK =>
                   -- Fin du dialog
                   res_bool := Win32.Winuser.EndDialog( hwnd, IDOK );
                   return 1;

               when ID_BOUT_LANCER =>
                  H_ins := Win32.Shellapi.ShellExecute( N_A,
                           TO_LPCSTR(ope'address),
                           TO_LPCSTR(file'address),
                           null, null, Win32.Winuser.SW_SHOW);
                   res_bool := Win32.Winuser.EndDialog( hwnd, IDOK );
                   return 1;

               when others =>
                  return 0;
            end case;

	 -- autres
         when others =>
           return 0;

       end case;
   exception
      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
         Log.End_Log;
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_exception );
         return 0;
   end Dialog_proc_Sndvol;


   function Create_Template_Sndvol return Template_pkg.template_type is
      my_template : Template_pkg.template_type;
   begin
      my_template := Template_pkg.New_template( 230, 140, Intl.dlg_nocontrol_title );
      -- Item 1 : Bouton "OK"              X   Y   W   H
      Template_Pkg.Add_Item( my_template, 90, 120, 45, 16, Win32.Winuser.IDOK, Intl.dlg_OK_txt, Template_pkg.Button );
      -- Item 2 : texte d'instruction     X   Y   W   H
      Template_Pkg.Add_Item( my_template, 12, 5, 200, 75, ID_TEXT_1, Intl.dlg_novolum_txt, Template_pkg.Static );
      -- Item 5: bouton "Lancer"
      Template_Pkg.Add_Item( my_template, 40, 80, 150, 16, ID_BOUT_LANCER, Intl.dlg_lancer_txt, Template_pkg.Button );
      --
      return my_template;
   end Create_Template_Sndvol;

   procedure Launch_sndvol is
      my_template : Template_pkg.template_type;
      res_int : Win32.INT;
   begin
      -- création du template
      my_template := Create_Template_Sndvol;
      -- création et affichage de la dialog box
      Res_int := Win32.Winuser.DialogBoxIndirect(
                             Common_types.hInst,
                             Template_Pkg.Get_Template(my_Template),
                             Common_types.Win_hwnd,
                             Dialog_proc_Sndvol'access );
      if res_int = -1 then
         Utils_pkg.Log_Windows_error( "Dialog_pkg.Launch_sndvol: DialogBoxIndirect");
      end if;
      -- libération mémoire utilisée par la template
      Template_Pkg.Free(my_template);
   end Launch_sndvol;

   -- ******************************************************************************



   function Select_midi_file return string is
      filter_ext : constant string := Intl.fichier_midi & ascii.nul & "*.mid;*.kar" & ascii.nul & ascii.nul;
      buffer   : string(1..1024) := (others => ascii.nul );
      last_dir : constant string := Registre_pkg.Get_last_midi_dir & ascii.nul;
      midi_ext : constant string := "mid" & ascii.nul;
      res_bool : Win32.BOOL;
      len, dir_len : Integer;
      title : constant string := Intl.Titre_dialog_open_midi & ascii.nul;
   begin

      -- paramètrage de la dialog box
      opfn.lStructSize       := Win32.DWORD( Win32.Commdlg.OPENFILENAME'size / 8 );
      opfn.hwndOwner         := Common_types.Win_hwnd;
      opfn.lpstrFilter       := TO_LPCSTR( filter_ext'address );
      opfn.lpstrCustomFilter := null;
      opfn.nMaxCustFilter    := 0;
      opfn.nFilterIndex      := 1;
      opfn.lpstrFile         := TO_LPSTR( buffer'address );
      opfn.nMaxFile          := Win32.DWORD( buffer'length );
      opfn.lpstrFileTitle    := null;
      opfn.nMaxFileTitle     := 0;
      opfn.lpstrInitialDir   := TO_LPCSTR( last_dir'address );
      opfn.lpstrTitle        := TO_LPCSTR( title'address );
      opfn.Flags             := Win32.Commdlg.OFN_FILEMUSTEXIST + Win32.Commdlg.OFN_PATHMUSTEXIST;
      opfn.nFileOffset       := 0;
      opfn.nFileExtension    := 0;
      opfn.lpstrDefExt       := TO_LPCSTR( midi_ext'address );
      opfn.lCustData         := 0;
      opfn.lpfnHook          := null;
      opfn.lpTemplateName    := null;
      --
      --  appel de la dialog box
      res_bool := Win32.Commdlg.GetOpenFileName( opfn'access );
      -- test résultat
      if res_bool = 0 then
         return "";
      end if;
      -- recherche le nul final
      len := 1;
      while len < buffer'last and then buffer(len+1) /= ascii.nul loop
         len := len + 1;
      end loop;
      -- recherche la fin du répertoire
      dir_len := len;
      while dir_len > 0 and then buffer(dir_len) /= '\' loop
         dir_len := dir_len - 1;
      end loop;
      -- enregistre le répertoire
      Registre_pkg.Set_last_midi_dir( buffer(1..dir_len) );
      --
      -- retourne le nom complet
      return buffer(1..len);
   end Select_Midi_file;


   -- *************************************************************************************


   function Select_Save_file return string is
--      filter_ext : constant string := Intl.fichier_wave & ascii.nul & "*.wav;*.mid" & ascii.nul & ascii.nul;
      filter_ext : constant string := Intl.fichier_wave & ascii.nul & "*.wav" & ascii.nul & ascii.nul;
      buffer   : string(1..1024) := (others => ascii.nul );
      last_dir : constant string := Registre_pkg.Get_last_save_dir & ascii.nul;
      res_bool : Win32.BOOL;
      len, dir_len : Integer;
      title : constant string := Intl.Titre_dialog_save & ascii.nul;
   begin

      -- paramètrage de la dialog box
      opfn.lStructSize       := Win32.DWORD( Win32.Commdlg.OPENFILENAME'size / 8 );
      opfn.hwndOwner         := Common_types.Win_hwnd;
      opfn.lpstrFilter       := TO_LPCSTR( filter_ext'address );
      opfn.lpstrCustomFilter := null;
      opfn.nMaxCustFilter    := 0;
      opfn.nFilterIndex      := 1;
      opfn.lpstrFile         := TO_LPSTR( buffer'address );
      opfn.nMaxFile          := Win32.DWORD( buffer'length );
      opfn.lpstrFileTitle    := null;
      opfn.nMaxFileTitle     := 0;
      opfn.lpstrInitialDir   := TO_LPCSTR( last_dir'address );
      opfn.lpstrTitle        := TO_LPCSTR( title'address );
      opfn.Flags             := Win32.Commdlg.OFN_OVERWRITEPROMPT + Win32.Commdlg.OFN_PATHMUSTEXIST;
      opfn.nFileOffset       := 0;
      opfn.nFileExtension    := 0;
      opfn.lpstrDefExt       := null;
      opfn.lCustData         := 0;
      opfn.lpfnHook          := null;
      opfn.lpTemplateName    := null;
      --
      --  appel de la dialog box
      res_bool := Win32.Commdlg.GetSaveFileName( opfn'access );
      -- test résultat
      if res_bool = 0 then
         return "";
      end if;
      -- recherche le nul final
      len := 1;
      while len < buffer'last and then buffer(len+1) /= ascii.nul loop
         len := len + 1;
      end loop;
      -- recherche la fin du répertoire
      dir_len := len;
      while dir_len > 0 and then buffer(dir_len) /= '\' loop
         dir_len := dir_len - 1;
      end loop;
      -- enregistre le répertoire
      Registre_pkg.Set_last_save_dir( buffer(1..dir_len) );
      --
      -- retourne le nom complet
      return buffer(1..len);
   end Select_Save_file;

   -- ******************************************************************************


   function Dialog_Proc_Track( hwnd : Win32.Windef.HWND;
                         msg : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM)
                                  return Win32.BOOL is
      wm_Id : Win32.WORD;
      res_bool : Win32.BOOL;
   begin
      case msg is

         when WM_INITDIALOG =>
            -- centrage de la fentre du dialog
            Centre_Dialog( hwnd );

         when WM_COMMAND =>
	    -- lecture de l'Id
            wm_ID := Win32.Windef.LOWORD (Win32.DWORD (wParam));

            case wm_ID is

               when IDCANCEL =>
                  -- Fin du dialog, annulation => retourne -1
                  res_bool := Win32.Winuser.EndDialog( hwnd, -1 );
                  return 1;

               when Resources_pkg.TRACK_ID..Resources_pkg.EDIT_ID =>
                  -- Fin du dialog, retourne la valeur sélectionnée transmise dans l'Id
                  res_bool := Win32.Winuser.EndDialog( hwnd, Win32.INT(wm_ID) - Resources_pkg.TRACK_ID );
                  return 1;

               when others =>
                  return 0;

            end case;

	 -- autres
         when others =>
            return 0;

       end case;
       return 0;
   end Dialog_Proc_Track;

   -- ================================================================================

   function Truncate( s : string; max : integer ) return string is
   begin
      if s'length < max then
         return s;
      end if;
      return s(s'first..s'first+max-1);
   end Truncate;


   function Create_Template_Track( root : midifile_pkg.midi_data_ptr ) return Template_pkg.template_type is
      my_template : Template_pkg.template_type;
      trk : midifile_pkg.track_data_ptr;
      count : Win32.SHORT := 0;
      col_1, col_2 : natural := 0;
      hauteur, largeur, pos : Win32.SHORT;
      hauteur_ligne : constant := 20;
      largeur_char : constant := 4;

      function Text_1( index : natural; track : midifile_pkg.track_data_ptr ) return string is
         text : string_pt;
      begin
         if trk.track_name /= null then
            text := trk.track_name;	-- nom de la piste
         elsif trk.instr_name /= null then
            text := trk.instr_name;	-- nom de l'instrument
         else
            text := inconnu_txt;
         end if;
         return Integer'image(Integer(index)) & ": " & Truncate( text.all, max_track_name_length );
      end Text_1;

      function Text_2( track : midifile_pkg.track_data_ptr ) return string is
      begin
         if track.prog_num in general_midi.instrument_range_type then
            return General_midi.Instrument_name( track.prog_num ).all;
         else
            return vide_txt.all;
         end if;
      end Text_2;

   begin
      -- comptage du nombre d'item à insérer
      trk := root.tracks;
      col_1 := Intl.dlg_piste_txt'length;	-- longueur titre colonne 1
      col_2 := Intl.dlg_instr_txt'length;	-- longueur titre colonne 2
      while trk /= null loop
         if trk.polyphonie = 1 then
            -- compte le nombre de piste
            count := count + 1;
            -- calcule la taille max de chaque colonne
            declare
               str_1 : constant string := Text_1( Integer(count), trk );
               str_2 : constant string := Text_2( trk );
            begin
               if col_1 < str_1'length then
                  col_1 := str_1'length;
               end if;
               if col_2 < str_2'length then
                  col_2 := str_2'length;
               end if;
            end;
         end if;
         -- piste suivante
         trk := trk.next;
      end loop;
      -- calcul hauteur
      hauteur := 46 + (count+1) * hauteur_ligne;
      -- calcul largeur
      largeur := 130 + Win32.Short(col_1+col_2) * largeur_char;
      -- Création de la template
      my_template := Template_pkg.New_template( largeur, hauteur, Intl.dlg_track_title );
      -- Item 1 : Bouton "Annuler"
      Template_Pkg.Add_Item( my_template, (largeur-71)/2, hauteur - hauteur_ligne, 71, 16, Win32.Winuser.IDCANCEL,
                             Intl.dlg_cancel_txt, Template_pkg.Button );
      --
      -- ligne de titre
      Template_Pkg.Add_Item( my_template,
                             12, 17,
                             Win32.short(col_1)*largeur_char, 12,
                             0,
                             Intl.dlg_piste_txt,
                             Template_pkg.Static );
      -- edit avec le nom de l'instrument GM
      pos := 12 + Win32.short(col_1)*largeur_char + 5;
      Template_Pkg.Add_Item( my_template,
                             pos, 17,
                             Win32.short(col_2)*largeur_char, 12,
                             0,
                             Intl.dlg_instr_txt,
                             Template_pkg.Static );
      -- ajoute une ligne bouton+edit pour chaque piste possible
      trk := root.tracks;
      count := 1;		-- saute ligne de titre
      while trk /= null loop
         if trk.polyphonie = 1 then
            count := count + 1;
            -- edit avec le nom de la piste
            Template_Pkg.Add_Item( my_template,
                                   12, 17 + (count-1)*hauteur_ligne,
                                   Win32.short(col_1)*largeur_char, 12,
                                   Resources_pkg.EDIT_ID + Win32.WORD(count),
                                   Text_1( Integer(count-1), trk ),
                                   Template_pkg.Static );
            -- edit avec le nom de l'instrument GM
            pos := 12 + Win32.short(col_1)*largeur_char + 5;
            Template_Pkg.Add_Item( my_template,
                                   pos, 17 + Win32.SHORT(count-1)*hauteur_ligne,
                                   Win32.short(col_2)*largeur_char, 12,
                                   Resources_pkg.EDIT_ID + Win32.WORD(count),
                                   Text_2(trk),
                                   Template_pkg.Static );
            -- label avec le % de semblance
            pos := pos + Win32.short(col_2)*largeur_char + 5;
            Template_Pkg.Add_Item( my_template,
                                   pos, 17 + Win32.SHORT(count-1)*hauteur_ligne,
                                   30, 12,
                                   Resources_pkg.EDIT_ID + Win32.WORD(count),
                                   Integer'image(trk.semblance) & " %",
                                   Template_pkg.Static );

            -- bouton "Select"
            pos := pos + 35;
            Template_Pkg.Add_Item( my_template,
                                   pos, 15 + (count-1)*hauteur_ligne,
                                   70, 16,
                                   Resources_pkg.TRACK_ID + Win32.WORD(trk.track_num),
                                   Intl.dlg_select_txt, Template_pkg.Button );
         end if;
         -- piste suivante
         trk := trk.next;
      end loop;
      --
      -- terminé
      return  my_template;
   end Create_Template_Track;



   function Select_track( root : midifile_pkg.midi_data_ptr ) return integer is
      my_template : Template_pkg.template_type;
      res_int : Win32.INT;
   begin
      -- création du template
      my_template := Create_Template_Track( root );
      -- création et affichage de la dialog box
      Res_int := Win32.Winuser.DialogBoxIndirect(
                             Common_types.hInst,
                             Template_Pkg.Get_Template(my_Template),
                             Common_types.Win_hwnd,
                             Dialog_proc_Track'access );
      if res_int = -1 then
         Utils_pkg.Log_Windows_error( "Dialog_pkg.Select_track: DialogBoxIndirect");
      end if;
      -- libération mémoire utilisée par la template
      Template_Pkg.Free(my_template);
      --
      return Integer(res_int);
   exception
      when others =>
         -- libération mémoire utilisée par la template
         Template_Pkg.Free(my_template);
         raise;
   end Select_track;


   -- =====================================================================================

   function Dialog_proc_Auto( hwnd : Win32.Windef.HWND;
                         msg : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM) return Win32.BOOL is
      wm_ID : Win32.WORD;
      buffer : string(1..128);
      res_int : Win32.UINT;
      res_bool : Win32.BOOL;
      hcopy : Win32.Windef.HGLOBAL;
      lp : Win32.LPVOID;	-- = System.Address
      hdata : Win32.Winnt.HANDLE;
   begin
      case msg is

         when WM_INITDIALOG =>
            -- centrage de la fentre du dialog
            Centre_Dialog( hwnd );
            -- doit toujours retourner 1 si INITDIALOG
            return 1;


         when WM_COMMAND =>
	    -- lecture de l'Id
            wm_ID := Win32.Windef.LOWORD (Win32.DWORD (wParam));

            case wm_ID is

                when IDOK =>
                   -- Fin du dialog
                   res_bool := Win32.Winuser.EndDialog( hwnd, IDOK );
                   return 1;


               when ID_BOUT_COPIER =>
                   -- lecture de la signature
                   res_int := Win32.Winuser.GetDlgItemText( hwnd, ID_SIGNATURE,
                            TO_LPSTR( buffer'address ), Win32.INT( buffer'length ) );
                  -- ouverture du presse-papier
                  res_bool := Win32.Winuser.OpenClipBoard( hwnd );
                  if res_bool /= 0 then
                     -- vidage
                     res_bool := Win32.Winuser.EmptyClipboard;
                     -- alloc mémoire windows
                     hcopy := Win32.Winbase.GlobalAlloc( Win32.Winbase.GMEM_MOVEABLE,
                          Win32.DWORD(res_int + 1) );
                     --- vérouillage
                     lp := Win32.Winbase.GlobalLock(hcopy);
                     -- copie
                     declare
                        s : string(1..integer(res_int)+1);
                        for s use at lp;
                     begin
                        s(1..integer(res_int)) := buffer(1..integer(res_int));
                        s(s'last) := ascii.nul;
                     end;
                     -- dévérouillage
                     res_bool := Win32.Winbase.GlobalUnlock(hcopy);
                     -- copie dans le presse-papier
                     hdata := SetClipboardData ( Win32.Winuser.CF_TEXT, hcopy );
                     -- ferme
                     res_bool := Win32.Winuser.CloseClipboard;
                  else
                     -- impossible d'ouvrir le presse-papier: il est déjà ouvert
                     Utils_pkg.Error_box( intl.def_err_title, Intl.clipboard_error );
                  end if;
                  return 0;

               when others =>
                  return 0;
            end case;

	 -- autres
         when others =>
           return 0;

       end case;
   exception
      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
         Log.End_Log;
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_exception );
         return 0;
   end Dialog_proc_Auto;


   function Create_Auto_dlg( signature : string ) return Template_pkg.template_type is
      my_template : Template_pkg.template_type;
      but_ok_y, label_y, sign_y, but_copier_y : Win32.SHORT;
   begin
      -- Création de la template                   W    H
      if Config_pkg.Is_full then
         my_template := Template_pkg.New_template( 230, 140, Intl.dlg_auto_title );
         but_ok_y := 120;
         label_y := 85;
         sign_y := 95;
         but_copier_y := 90;
      else
         my_template := Template_pkg.New_template( 230, 160, Intl.dlg_demo_title );
         but_ok_y := 140;
         label_y := 105;
         sign_y := 115;
         but_copier_y := 110;
      end if;
      -- Item 1 : Bouton "OK"              X   Y   W   H
      Template_Pkg.Add_Item( my_template, 90, but_ok_y, 45, 16, Win32.Winuser.IDOK, Intl.dlg_OK_txt, Template_pkg.Button );
      -- Item 2 : texte d'instruction        X   Y   W   H
      if Config_pkg.Is_full then
         Template_Pkg.Add_Item( my_template, 12, 5, 200, 75, ID_TEXT_1, Intl.dlg_instruct_txt, Template_pkg.Static );
      else
         Template_Pkg.Add_Item( my_template, 12, 5, 200, 95, ID_TEXT_1, Intl.dlg_demo_intro, Template_pkg.Static );
      end if;
      -- Item 3 : Label signature
      Template_Pkg.Add_Item( my_template, 12, label_y, 120, 10, ID_TEXT_2, Intl.dlg_sign_txt, Template_pkg.Static );
      -- Item 4 : Signature
      Template_Pkg.Add_Item( my_template, 30, sign_y, 130, 15, ID_SIGNATURE, signature, Template_pkg.Static );
      -- Item 5: bouton "Copier"
      Template_Pkg.Add_Item( my_template, 150, but_copier_y, 50, 16, ID_BOUT_COPIER, Intl.dlg_copier_txt, Template_pkg.Button );
      --
      return my_template;
   end Create_Auto_dlg;


   procedure Affiche_cle_autorisation( sign : string ) is
      my_template : Template_pkg.template_type;
      res_int : Win32.INT;
   begin
      -- création du template
      my_template := Create_Auto_dlg( sign );
      -- création et affichage de la dialog box
      Res_int := Win32.Winuser.DialogBoxIndirect(
                             Common_types.hInst,
                             Template_Pkg.Get_Template(my_Template),
                             Common_types.Win_hwnd,
                             Dialog_proc_Auto'access );
      if res_int = -1 then
         Utils_pkg.Log_Windows_error( "Dialog_pkg.Affiche_cle_autorisation: DialogBoxIndirect");
      end if;
      -- libération mémoire utilisée par la template
      Template_Pkg.Free(my_template);
   exception
      when others =>
         -- libération mémoire utilisée par la template
         Template_Pkg.Free(my_template);
         raise;
   end Affiche_cle_autorisation;


   -- **********************************************************************************

   function Create_Option_dlg return Template_pkg.template_type is
      my_template : Template_pkg.template_type;
   begin
      -- Création de la template
      my_template := Template_pkg.New_template( width => 230, height => 150,
                                                titre => Intl.Option_dlg_title );
      -- Item 1 : Bouton OK
      Template_Pkg.Add_Item( my_template, X => 30, Y => 125, width => 40, height => 16,
                             Id    => Win32.Winuser.IDOK,
                             titre => Intl.dlg_ok_txt,
                             class => Template_pkg.Button );
      -- Item 2 : Bouton "Annuler"
      Template_Pkg.Add_Item( my_template, X => 120, Y => 125, width => 40, height => 16,
                             Id    => Win32.Winuser.IDCANCEL,
                             titre => Intl.dlg_cancel_txt,
                             class => Template_pkg.Button );
      -- Item 3 : Label Auto-volume
      Template_Pkg.Add_Item( my_template, X => 12, Y => 15, width => 80, height => 12,
                             Id    => ID_TEXT_1,
                             titre => Intl.dlg_auto_volume,
                             class => Template_pkg.Static );
      -- Item 4 : ComboBox Oui/Non
      Template_Pkg.Add_Item( my_template, X => 100, Y => 15, width => 40, height => 36, -- 2 lignes
                             Id    => ID_AUTOVOLUME,
                             titre => "",
                             class => Template_pkg.Combo_box );
      -- Item 5 : Label Change instrument
      Template_Pkg.Add_Item( my_template, X => 12, Y => 35, width => 80, height => 12,
                             Id    => ID_TEXT_2,
                             titre => Intl.Dlg_change_instr,
                             class => Template_pkg.Static );
      -- Item 6 : ComboBox Oui/Non
      Template_Pkg.Add_Item( my_template, X => 100, Y => 35, width => 40, height => 36, -- 2 lignes
                             Id    => ID_CHANGEINST,
                             titre => "",
                             class => Template_pkg.Combo_box );
      -- Item 7 : Label Famille
      Template_Pkg.Add_Item( my_template, X => 52, Y => 55, width => 80, height => 12,
                             Id    => ID_TEXT_3,
                             titre => Intl.dlg_famille,
                             class => Template_pkg.Static );
      -- Item 8 : ComboBox Famille: General Midi families list
      Template_Pkg.Add_Item( my_template, X => 140, Y => 55, width => 80, height => 288, -- 16 lignes
                             Id    => ID_INSTRFAMI,
                             titre => "",
                             class => Template_pkg.Combo_box );
      -- Item 9 : Label Instrument
      Template_Pkg.Add_Item( my_template, X => 52, Y => 75, width => 80, height => 12,
                             Id    => ID_TEXT_4,
                             titre => Intl.dlg_instrument,
                             class => Template_pkg.Static );
      -- Item 10 : ComboBox Instrument: General Midi instrument list
      Template_Pkg.Add_Item( my_template, X => 140, Y => 75, width => 80, height => 144, -- 8 lignes
                             Id    => ID_INSTRNAME,
                             titre => "",
                             class => Template_pkg.Combo_box );
      -- Item 11 : Label Skin
      Template_Pkg.Add_Item( my_template, X => 12, Y => 95, width => 80, height => 12,
                             Id    => ID_TEXT_4,
                             titre => Intl.dlg_skin,
                             class => Template_pkg.Static );
      -- comptage du nombre de skin
      nbr_skins := Skins_pkg.Count_skins;
      -- Item 12 : ComboBox Skins
      Template_Pkg.Add_Item( my_template, X => 100, Y => 95, width => 80, height => Win32.SHORT(18 * nbr_skins),
                             Id    => ID_SKINNAME,
                             titre => "",
                             class => Template_pkg.Combo_box );
      --
      return  my_template;
   end Create_Option_dlg;


   function Option_proc( hwnd : Win32.Windef.HWND;
                         msg : Win32.UINT;
                         wParam : Win32.WPARAM;
                         lParam : Win32.LPARAM) return Win32.BOOL is
      hwnd_autovolume, hwnd_changeinst, hwnd_family, hwnd_instrument, hwnd_skins : Win32.Windef.HWND;
      resu_long : Win32.LPARAM;
      res_bool : Win32.BOOL;
      wm_Id     : Win32.WORD;
      num_skin, family, instrum : natural;
      notif_msg : Win32.WORD;
      change : Win32.BOOL;
   begin
      case msg is

         when WM_INITDIALOG =>
            -- centrage de la fenetre du dialog
            Centre_Dialog( hwnd );
            --
            -- création combo autovolume
            hwnd_autovolume := GetDlgItem( hwnd, ID_AUTOVOLUME );
            resu_long := SendMessage( hwnd_autovolume, CB_ADDSTRING, 0, TO_LPARAM(Intl.dlg_Non_sz'address) );
            resu_long := SendMessage( hwnd_autovolume, CB_ADDSTRING, 0, TO_LPARAM(Intl.dlg_Oui_sz'address) );
            if Common_types.Auto_volume then
               resu_long := SendMessage( hwnd_autovolume, CB_SETCURSEL, 1, 0 );	-- True = 1
            else
               resu_long := SendMessage( hwnd_autovolume, CB_SETCURSEL, 0, 0 );	-- False = 0
            end if;
            --
            -- création combo change instrument
            hwnd_changeinst := GetDlgItem( hwnd, ID_CHANGEINST );
            resu_long := SendMessage( hwnd_changeinst, CB_ADDSTRING, 0, TO_LPARAM(Intl.dlg_Non_sz'address) );
            resu_long := SendMessage( hwnd_changeinst, CB_ADDSTRING, 0, TO_LPARAM(Intl.dlg_Oui_sz'address) );
            if Common_types.Change_instrument then
               resu_long := SendMessage( hwnd_changeinst, CB_SETCURSEL, 1, 0 );	-- True = 1
            else
               resu_long := SendMessage( hwnd_changeinst, CB_SETCURSEL, 0, 0 );	-- False = 0
            end if;
            --
            -- création combo famille
            hwnd_family := GetDlgItem( hwnd, ID_INSTRFAMI );
            for i in General_midi.families'range loop
               declare
                  name : string := General_midi.families(i).all & ascii.nul;
               begin
                  resu_long := SendMessage( hwnd_family, CB_ADDSTRING, 0, TO_LPARAM(name'address) );
               end;
            end loop;
            family := Integer(Common_types.Instrument_number) / 8;
            instrum := Integer(Common_types.Instrument_number) mod 8;
            resu_long := SendMessage( hwnd_family, CB_SETCURSEL, Win32.WPARAM(family), 0 );
            --
            -- création combo instrument
            hwnd_instrument := GetDlgItem( hwnd, ID_INSTRNAME );
            for i in family*8..family*8+7 loop
               declare
                  name : string := General_midi.Instrument_name(i).all & ascii.nul;
               begin
                  resu_long := SendMessage( hwnd_instrument, CB_ADDSTRING, 0, TO_LPARAM(name'address) );
               end;
            end loop;
            resu_long := SendMessage( hwnd_instrument, CB_SETCURSEL, Win32.WPARAM(instrum), 0 );
            --
            if not Common_types.Change_instrument then
               res_bool := Win32.Windowsx.ComboBox_Enable( hwnd_family, 0 );
               res_bool := Win32.Windowsx.ComboBox_Enable( hwnd_instrument, 0 );
            end if;
            --
            -- création combo skin
            hwnd_skins := GetDlgItem( hwnd, ID_SKINNAME );
            default_skin := 1;
            declare
               current_skin : string := Registre_pkg.Get_current_skin;
            begin
               GNAT.Case_Util.To_Lower( current_skin );
               for i in 1..nbr_skins loop
                  declare
                     name : string := Skins_pkg.Skin_name(i) & ascii.nul;
                  begin
                     resu_long := SendMessage( hwnd_skins, CB_ADDSTRING, 0, TO_LPARAM(name'address) );
                     -- test si c'est la skin en cours
                     if name(name'first..name'last-1) = current_skin then
                        default_skin := i;
                     end if;
                  end;
               end loop;
            end;
            resu_long := SendMessage( hwnd_skins, CB_SETCURSEL, Win32.WPARAM(default_skin-1), 0 );
            --
            -- doit toujours retourner 1 si INITDIALOG
            return 1;

         when WM_COMMAND =>
	    -- lecture de l'Id
            wm_ID := Win32.Windef.LOWORD (Win32.DWORD (wParam));

            case wm_ID is

               when ID_CHANGEINST =>
                  notif_msg := Win32.Windef.HIWORD (Win32.DWORD (wParam));
                  if notif_msg = CBN_SELCHANGE then
                     -- lecture des handle
                     hwnd_changeinst := GetDlgItem( hwnd, ID_CHANGEINST );
                     hwnd_instrument := GetDlgItem( hwnd, ID_INSTRNAME );
                     hwnd_family  := GetDlgItem( hwnd, ID_INSTRFAMI );
                     -- lecture de la valeur sélectionnée 0 = Non, 1 = Oui
                     change := BOOL(SendMessage( hwnd_changeinst, CB_GETCURSEL, 0, 0 ));
                     -- modification des combos
                     res_bool := Win32.Windowsx.ComboBox_Enable( hwnd_family, change );
                     res_bool := Win32.Windowsx.ComboBox_Enable( hwnd_instrument, change );
                  end if;
                  return 0;

               when ID_INSTRFAMI =>
                  --
                  notif_msg := Win32.Windef.HIWORD (Win32.DWORD (wParam));
                  if notif_msg = CBN_SELCHANGE then
                     -- lecture des handle des combobox
                     hwnd_instrument := GetDlgItem( hwnd, ID_INSTRNAME );
                     hwnd_family  := GetDlgItem( hwnd, ID_INSTRFAMI );
                     -- lecture valeur famille sélectionnée
                     family := Integer(SendMessage( hwnd_family, CB_GETCURSEL, 0, 0 ));
                     -- reset du combo instrument
                     resu_long := SendMessage( hwnd_instrument, CB_RESETCONTENT, 0, 0);
                     -- création nouvelle liste
                     for i in family*8..family*8+7 loop
                        declare
                           name : string := General_midi.Instrument_name(i).all & ascii.nul;
                        begin
                           resu_long := SendMessage( hwnd_instrument, CB_ADDSTRING, 0, TO_LPARAM(name'address) );
                        end;
                     end loop;
                     -- sélectionne le premier de la liste
                     resu_long := SendMessage( hwnd_instrument, CB_SETCURSEL, 0, 0 );
                  end if;
                  return 0;

               when IDOK =>
                  -- lecture des valeurs des combo
                  -- autovolume
                  hwnd_autovolume := GetDlgItem( hwnd, ID_AUTOVOLUME );
                  Common_types.Auto_volume :=  Integer(SendMessage( hwnd_autovolume, CB_GETCURSEL, 0, 0 )) = 1;
                  --
                  -- change instrument
                  hwnd_changeinst := GetDlgItem( hwnd, ID_CHANGEINST );
                  Common_types.Change_instrument :=  Integer(SendMessage( hwnd_changeinst, CB_GETCURSEL, 0, 0 )) = 1;
                  --
                  -- nom d'instrument
                  hwnd_family := GetDlgItem( hwnd, ID_INSTRFAMI );	-- famille
                  hwnd_instrument := GetDlgItem( hwnd, ID_INSTRNAME );	-- nom
                  family := Integer(SendMessage( hwnd_family, CB_GETCURSEL, 0, 0 ));
                  instrum := Integer(SendMessage( hwnd_instrument, CB_GETCURSEL, 0, 0 ));
                  -- numéro d'instrument
                  Common_types.Instrument_number := byte(family*8+instrum);
                  --
                  -- nom skin
                  hwnd_skins := GetDlgItem( hwnd, ID_SKINNAME );
                  num_skin := Integer(SendMessage( hwnd_skins, CB_GETCURSEL, 0, 0 )) + 1;
                  if num_skin /= default_skin then
                     Free( new_skin );
                     new_skin := new string'( Skins_pkg.Skin_name(num_skin) );
                     res_bool := Win32.Winuser.EndDialog( hwnd, 1 );	-- skin changé
                  else
                     res_bool := Win32.Winuser.EndDialog( hwnd, 0 );     -- skin pas changé
                  end if;
                  --
                  return 1;

               when IDCANCEL =>
                   res_bool := Win32.Winuser.EndDialog( hwnd, 0 );
                   return 1;

               when others => return 0;

            end case;

	 -- autres
         when others => return 0;

      end case;
      --
   exception
      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
         Log.End_Log;
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_exception );
         -- fin du programme par Windows
         Win32.Winuser.PostQuitMessage (0);
         return 1;
   end Option_proc;


      -- affichage et saisie des options
   function Options return boolean is
      my_template : Template_pkg.template_type;
      res_int : Win32.INT;
   begin
      -- création du template
      my_template := Create_Option_dlg;
      -- création et affichage de la dialog box
      Res_int := Win32.Winuser.DialogBoxIndirect(
                             Common_types.hInst,
                             Template_Pkg.Get_Template(my_Template),
                             Common_types.Win_hwnd,
                             Option_proc'access );
      if res_int = -1 then
         Utils_pkg.Log_Windows_error( "Dialog_pkg.Options: DialogBoxIndirect");
      end if;
      -- libération mémoire utilisée par la template
      Template_Pkg.Free(my_template);
      --
      return res_int = 1;
   exception
      when others =>
         -- libération mémoire utilisée par la template
         Template_Pkg.Free(my_template);
         raise;
   end Options;


end Dialog_pkg;

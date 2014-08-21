with Unchecked_Conversion;
with System.Address_Image;
with Interfaces.C;	use Interfaces.C;
with System;		use System;
with GNAT.Current_Exception;

with Win32;
with Win32.Mmsystem;	use Win32.Mmsystem;
with Win32.Winuser;

with Conversions;	use Conversions;
with Common_types;	use Common_types;
with Dialog_pkg;
with Utils_pkg;		use Utils_pkg;
with Intl;
with Log;
with Debug_pkg;


package body Win_audio is

   -- code pour le debugging
   debug_flag : constant string := "mixer";
   function TO_ADDRESS is new Unchecked_Conversion( Win32.DWORD, System.Address );

   -- handle pour le mixer
   mixer : Win32.Mmsystem.HMIXER := N_A;

   -- constant pour les controls
   Not_found   : constant Win32.DWORD := Win32.DWORD'LAST;

   -- si pas de control
   no_control  : boolean := True;

   -- capacités pour les Device MIDI
   midi_caps : aliased Win32.Mmsystem.MIDIOUTCAPSA;

--     type MIDIOUTCAPSA is                                    --  mmsystem.h:772
--        record
--           wMid : Win32.WORD;                     --  mmsystem.h:773
--           wPid : Win32.WORD;                     --  mmsystem.h:774
--           vDriverVersion : MMVERSION;                      --  mmsystem.h:775
--           szPname : Win32.CHAR_Array (0 .. 31);        --  mmsystem.h:776
--           wTechnology : Win32.WORD;                     --  mmsystem.h:777
--           wVoices : Win32.WORD;                     --  mmsystem.h:778
--           wNotes : Win32.WORD;                     --  mmsystem.h:779
--           wChannelMask : Win32.WORD;                     --  mmsystem.h:780
--           dwSupport : Win32.DWORD;                    --  mmsystem.h:781
--        end record;

   -- ******************************************************************************


   -- Interface avec le control de volume du Microphone

   function Get_Micro_Volume return Win32.WORD is
      Result  : MMRESULT;
      details : MIXERCONTROLDETAILS;
      level   : MIXERCONTROLDETAILS_UNSIGNED;
   begin
      -- si pas de lecture possible, retour immédiat
      if no_control then
         return 0;
      end if;
      --
      -- on peut accéder directement au controle de volume
      details.cbStruct    := (MIXERCONTROLDETAILS'size)/8;
      details.dwControlID := Common_types.Volume_control_Id;	  	-- ID du control que l'on lit
      details.cChannels   := 1;				  -- mono = 1 seul channel
      details.HW_DW.hwndOwner := System.Null_address;	  -- pas utilisé dans ce cas
      details.paDetails := Win32.LPVOID( level'address );      -- où stocker le résultat
      details.cbDetails := (MIXERCONTROLDETAILS_UNSIGNED'size)/8;
      --
      Result := mixerGetControlDetails( mixer,
                            TO_LPMIXERCONTROLDETAILS( details'address ),
                            MIXER_GETCONTROLDETAILSF_VALUE );
      if Result /= 0 then
         return 0;		-- erreur
      else
         return Win32.WORD(level.dwValue);
      end if;
         --
   end Get_Micro_Volume;


   procedure Set_Micro_Volume( valeur : Win32.WORD ) is
      Result  : Win32.Mmsystem.MMRESULT;
      details : MIXERCONTROLDETAILS;
      level   : MIXERCONTROLDETAILS_UNSIGNED;
   begin
      if No_control then
         return; 	-- rien à faire !
      end if;
      --
      -- on peut accéder directement au controle de volume
      level.dwValue := Win32.DWORD(valeur);
      --
      details.cbStruct    := (MIXERCONTROLDETAILS'size)/8;
      details.dwControlID := Common_types.Volume_control_Id;	  	-- ID du control que l'on lit
      details.cChannels   := 1;				  -- mono = 1 seul channel
      details.HW_DW.hwndOwner := System.Null_address;	  -- pas utilisé dans ce cas
      details.paDetails := Win32.LPVOID( level'address );      -- où stocker le résultat
      details.cbDetails := (MIXERCONTROLDETAILS_UNSIGNED'size)/8;
      -- envoi au mixer
      Result := mixerSetControlDetails( mixer,
                     TO_LPMIXERCONTROLDETAILS( details'address ),
                     MIXER_SETCONTROLDETAILSF_VALUE );
      Utils_pkg.Check_erreur( Result, "Win_Audio.Set_Micro_Volume" );
      --
   end Set_Micro_Volume;


   -- ==============================================================================


   procedure Set_Volume_Max is
      Result      : Win32.Mmsystem.MMRESULT;
      nb_dev      : Win32.UINT;
      line        : Win32.Mmsystem.MIXERLINE;			-- win32-mmsystem.ads:1673
      line_control: Win32.Mmsystem.MIXERLINECONTROLS;
      mixer_cap   : Win32.Mmsystem.MIXERCAPSA;
      nbr_connect : Win32.DWORD;

      procedure Traite_control( Line_Id : Win32.DWORD; Nb_control : Win32.DWORD ) is
         -- un mixer control pour chaque control déclaré
         mixer_control : array(1..Nb_control) of Win32.Mmsystem.MIXERCONTROL;
         details : MIXERCONTROLDETAILS;
         level   : MIXERCONTROLDETAILS_UNSIGNED; -- détail pour les fader
         on_off  : MIXERCONTROLDETAILS_BOOLEAN; -- détail pour les switch
      begin
         -- traitement cas special
         if Nb_control = 0 then
            return;
         end if;
         -- init des mixer control (est-ce nécessaire?)
         for cont in 1..Nb_control loop
            mixer_control(cont).cbStruct := (Win32.Mmsystem.MIXERCONTROL'size) / 8;
         end loop;
         -- init de la structure line_control
         line_control.cbStruct  := (line_control'size) / 8;
         line_control.dwLineID  := Line_Id; -- Id de la ligne
         line_control.cbmxctrl  := (Win32.Mmsystem.MIXERCONTROL'size) / 8;
         line_control.pamxctrl  := TO_LPMIXERCONTROLA( mixer_control(1)'address ); -- adresse résultat
         line_control.cControls := Nb_control;	-- nombre de control de la ligne
         -- lecture liste des controls
         Result := Win32.Mmsystem.mixerGetLineControls( mixer,
                                     TO_LPMIXERLINECONTROLSA( line_control'address ),
                                     MIXER_GETLINECONTROLSF_ALL );
         -- recherche dans le tableau
         for cont in 1..Nb_control loop
            case mixer_control(cont).dwControlType is

               when MIXERCONTROL_CONTROLTYPE_VOLUME | MIXERCONTROL_CONTROLTYPE_FADER =>
                  -- valeur au maximum
                  level.dwValue := mixer_control(cont).Bounds.DD.dwMaximum;
                  --
                  details.cbStruct    := (MIXERCONTROLDETAILS'size)/8;
                  details.dwControlID := mixer_control(cont).dwControlID;	  	-- ID du control que l'on lit
                  details.cChannels   := 1;				  -- tous
                  details.HW_DW.hwndOwner := System.Null_address;	  -- pas utilisé dans ce cas
                  details.paDetails := Win32.LPVOID( level'address );      -- valeurs
                  details.cbDetails := (MIXERCONTROLDETAILS_UNSIGNED'size)/8;
                  -- envoi au mixer
                  Result := mixerSetControlDetails( mixer,
                                 TO_LPMIXERCONTROLDETAILS( details'address ),
                                 MIXER_SETCONTROLDETAILSF_VALUE );

               when MIXERCONTROL_CONTROLTYPE_MUTE =>
                  On_off.fValue := 0; -- NO mute
                  --
                  details.cbStruct    := (MIXERCONTROLDETAILS'size)/8;
                  details.dwControlID := mixer_control(cont).dwControlID;	  	-- ID du control que l'on lit
                  details.cChannels   := 1;				  -- tous
                  details.HW_DW.hwndOwner := System.Null_address;	  -- pas utilisé dans ce cas
                  details.paDetails := Win32.LPVOID( On_off'address );      -- valeurs
                  details.cbDetails := (MIXERCONTROLDETAILS_BOOLEAN'size)/8;
                  -- envoi au mixer
                  Result := mixerSetControlDetails( mixer,
                                 TO_LPMIXERCONTROLDETAILS( details'address ),
                                 MIXER_SETCONTROLDETAILSF_VALUE );

               when others => null;
            end case;
         end loop;
      end Traite_control;


   begin
      -- lecture nombre de mixer
      nb_dev := Win32.Mmsystem.mixerGetNumDevs;
      -- pour chaque mixer..
      for mix in 0..nb_dev-1 loop
         -- lecture info du mixer pour avoir le nombre de lignes de destination
         result := Win32.Mmsystem.MixerGetDevCaps(
                            mix, To_PMixerCapsA(mixer_cap'address), mixer_cap'size/8 );
         -- ouverture du mixer
         result := Win32.Mmsystem.mixerOpen( TO_LPHMIXER(mixer'address),
                             mix,	       -- numéro du device
                             0, 0, 0 );
         -- pour chaque ligne de destination
         for dest in 0..mixer_cap.cDestinations-1 loop
            -- init de la structure line
            line.cbStruct := Win32.DWORD(Win32.Mmsystem.MIXERLINE'size/8);
            line.dwDestination := dest;
            -- lecture info ligne
            Result := Win32.Mmsystem.mixerGetLineInfo( mixer,
                               TO_LPMIXERLINE( line'address ),
                               Win32.Mmsystem.MIXER_GETLINEINFOF_DESTINATION );
            --
            -- Traitement des control MASTER
            --
            -- test son type
            if (line.dwComponentType = Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_SPEAKERS
               or line.dwComponentType = Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_HEADPHONES)
            then
               Traite_control( line.dwLineID, line.cControls );
            end if;
            --
            -- Traitement des sources connectées de type Synthesizer
            --
            nbr_connect := line.cConnections;
            for src in 0..nbr_connect-1 loop
               -- init de la structure line
               line.cbStruct := Win32.DWORD(Win32.Mmsystem.MIXERLINE'size/8);
               line.dwDestination := dest;
               line.dwSource := src;
               --
               Result := Win32.Mmsystem.mixerGetLineInfo( mixer,
                                  TO_LPMIXERLINE( line'address ),
                                  Win32.Mmsystem.MIXER_GETLINEINFOF_SOURCE );
               --
               -- test son type
               if line.dwComponentType = Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER
               then
                  Traite_control( line.dwLineID, line.cControls );
               end if;
               --
            end loop; -- sources connectées
            --
         end loop;-- lignes destination
         --
      end loop;-- mixers
      --
   end Set_Volume_Max;

   -- --------------------------------------------------------------------------

      -- parmi tous les control de la ligne, recherche le premier de type Volume
      -- si trouvé: assigne sa valeur à Common_types.Volume_control_Id e retourne true,
      -- si pas trouvé: ne fait rien et retourne false
      function Get_Fader_control( line : Win32.Mmsystem.MIXERLINE ) return Win32.DWORD is
         Result      : Win32.Mmsystem.MMRESULT;
         line_control  : Win32.Mmsystem.MIXERLINECONTROLS;
         mixer_control : array(1..line.cControls) of Win32.Mmsystem.MIXERCONTROL;			-- win32-mmsystem.ads:1780
      begin
         if line.cControls = 0 then
            return NOT_FOUND;
         end if;
         --
         for cont in 1..line.cControls loop
            mixer_control(cont).cbStruct := (Win32.Mmsystem.MIXERCONTROL'size) / 8;
         end loop;
         -- init de la structure line_control
         line_control.cbStruct  := (line_control'size) / 8;
         line_control.dwLineID  := line.dwLineID;
         line_control.cbmxctrl  := (Win32.Mmsystem.MIXERCONTROL'size) / 8;
         line_control.pamxctrl  := TO_LPMIXERCONTROLA( mixer_control(1)'address );
         line_control.cControls := line.cControls;
         -- lecture liste des controls
         Result := Win32.Mmsystem.mixerGetLineControls( mixer,
                            TO_LPMIXERLINECONTROLSA( line_control'address ),
                            MIXER_GETLINECONTROLSF_ALL );
         -- recherche d'abord un control de volume
         for cont in 1..line.cControls loop
            if mixer_control(cont).dwControlType = MIXERCONTROL_CONTROLTYPE_VOLUME then
               -- Trouvé !!
               return mixer_control(cont).dwControlID;
            end if;
         end loop;
         -- pas trouvé, cherche un control 'fader'
         for cont in 1..line.cControls loop
            if mixer_control(cont).dwControlType = MIXERCONTROL_CONTROLTYPE_FADER then
               -- Trouvé !!
               return mixer_control(cont).dwControlID;
            end if;
         end loop;
         --
         return NOT_FOUND;
      end Get_Fader_control;

   -- --------------------------------------------------------------------------

   function Open_Mixer return boolean is
      Result      : Win32.Mmsystem.MMRESULT;
      nb_dev      : Win32.UINT;
      line        : Win32.Mmsystem.MIXERLINE;			-- win32-mmsystem.ads:1673
      mixer_cap   : Win32.Mmsystem.MIXERCAPSA;
      nbr_connect : Win32.DWORD;
      Id          : Win32.DWORD;

      procedure Set_result_ok( value : Win32.DWORD ) is
      begin
         Common_types.Volume_control_Id := value;
         No_control := false;
         if Debug_pkg.Is_set( debug_flag ) then
           Log.Store( "Control prêt, ID=" & Win32.DWORD'image(Common_types.Volume_control_Id) );
           Log.End_Line;
         end if;
      end Set_result_ok;

   begin
      -- vérification du nombre de mixer du systeme
      nb_dev := Win32.Mmsystem.mixerGetNumDevs;
      if Debug_pkg.Is_set( debug_flag ) then
         Log.Store( "Nombre de mixer:" & Win32.UINT'image(nb_dev) );Log.End_Line;
      end if;
      if nb_dev = 0 then
         -- pas de mixer !
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_no_mixer );
         No_control := True;
         return false;
      end if;
      --
      -- Ouverture du mixer correspondand au micro sélectionné
      Result := Win32.Mmsystem.mixerOpen( TO_LPHMIXER(mixer'address),
                          Common_types.micro_num,	     -- Device Id du micro
                          TO_ULONG(Common_types.Win_hwnd),   -- window qui traite les messages du mixer
                          0,
                          MIXER_OBJECTF_WAVEIN or CALLBACK_WINDOW );
      Check_erreur( Result, "Win_Audio.Open_Mixer: mixerOpen" );
      if mixer = N_A then
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_mixer_open );
         No_control := True;
         return false;
      else
         if Debug_pkg.Is_set( debug_flag ) then
            Log.Store( "Mixer ouvert pour le device:" & Win32.UINT'image(Common_types.micro_num) );
            Log.End_Line;
         end if;
      end if;
      -- lecture capacité du mixer
      result := Win32.Mmsystem.MixerGetDevCaps( Common_types.micro_num,
                          To_PMixerCapsA(mixer_cap'address), mixer_cap'size/8 );
      Check_erreur( Result, "Win_Audio.Open_Mixer: MixerGetDevCaps" );
      --
      -- Recherche d'une ligne Destination WaveIn
      --
      -- pour chaque ligne de destination
      for dest in 0..mixer_cap.cDestinations-1 loop
         line.cbStruct := (line'size) / 8;
         line.dwDestination := dest;
         --
         Result := Win32.Mmsystem.mixerGetLineInfo( mixer,
                               TO_LPMIXERLINE( line'address ),
                               Win32.Mmsystem.MIXER_GETLINEINFOF_DESTINATION );
         Check_erreur( Result, "Win_Audio.Open_Mixer: mixerGetLineInfo(destination)" );
         --
         -- si le Target est WaveIn...
         if line.Target.dwType = Win32.Mmsystem.MIXERLINE_TARGETTYPE_WAVEIN then
            -- pour chaque source pouvant être connectée à cette destination
            nbr_connect := line.cConnections;
            for src in 0..nbr_connect-1 loop
               -- init de la structure line
               line.cbStruct := (line'size) / 8;
               line.dwDestination := dest; 	-- destination
               line.dwSource := src;		-- source
               --
               Result := Win32.Mmsystem.mixerGetLineInfo( mixer,
                                  TO_LPMIXERLINE( line'address ),
                                  Win32.Mmsystem.MIXER_GETLINEINFOF_SOURCE );
               Check_erreur( Result, "Win_Audio.Open_Mixer: mixerGetLineInfo(source)" );
               --
               -- si source de type Microphone...
               if line.dwComponentType = Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE then
                  -- recherche d'un controle Fader
                  Id := Get_Fader_control(line);
                  if Id /= NOT_FOUND then
                     Set_result_ok( Id );
                     return true;
                  end if;
               end if;-- microphone
            end loop;-- boucle sur les sources
            --
            -- aucune source avec un control de microphone !
            --
            -- recher d'un control de type mux sur la ligne destination wavein
            Result := Win32.Mmsystem.mixerGetLineInfo( mixer,
                               TO_LPMIXERLINE( line'address ),
                               Win32.Mmsystem.MIXER_GETLINEINFOF_DESTINATION );
            -- recherche d'un controle Fader sur cette ligne
            Id := Get_Fader_control(line);
            if Id /= NOT_FOUND then
               Set_result_ok( Id );
               return true;
            end if;
            --
         end if;-- wavein
      end loop;-- boucle sur les destinations
      --
      -- Pas trouvé !
      if Debug_pkg.Is_set( debug_flag ) then
         Log.Store( "Pas de control" );
         Log.End_Line;
      end if;
      No_control := True;
      return false;
   end Open_Mixer;


   procedure Close_Mixer is
      Result : MMRESULT;
   begin
      result := Win32.Mmsystem.MixerClose( mixer );
      --
      if Debug_pkg.Is_set( debug_flag ) then
         Log.Store( "Mixer fermé" );Log.End_Line;
      end if;
   end Close_Mixer;


   -- ==================================================================================


   procedure Start_lecture is
      Result  : Win32.Mmsystem.MMRESULT;
      Format  : Win32.Mmsystem.WAVEFORMATEX; -- win32-mmsystem.ads:1452
   begin
     --
     -- MICROPHONE
     --
     -- format des données à recevoir
      Format.wFormatTag 	:= Win32.Mmsystem.WAVE_FORMAT_PCM;	-- format brut
      Format.nChannels 		:= 1;			-- mono
      Format.nSamplesPerSec 	:= Win32.DWORD(Fs);	-- fréquence d'échantillonage
      Format.nAvgBytesPerSec 	:= 2*Win32.DWORD(Fs);	-- 16 bits = 2 bytes
      Format.nBlockAlign 	:= 2;			-- wBitsPerSample * nChannels / 8
      Format.wBitsPerSample 	:= 16;			-- 16 bits
      Format.cbSize		:= 0;			-- pas d'info supplémentaire pour le PCM
      -- ouverture du device
      Result := waveInOpen( TO_LPHWAVEIN(micro'address),
			 Common_types.micro_num,		                -- numero du device sélectionné
			 TO_LPCWAVEFORMATEX(Format'address),	-- format demandé
			 TO_DWORD( Data_Hwnd ),			-- handle de la window
			 0,					-- pas utilisé
                         Win32.Mmsystem.CALLBACK_WINDOW); 	-- code pour callback par messages window
      Utils_pkg.Check_erreur( Result, "Win_Audio.Start_lecture: waveInOpen"  );

      -- préparation des buffers
      for i in 1..nb_buff loop
	 -- allocation du header
         if Headers(i) = null then	-- si deja alloué, rien à faire
	    Headers(i) := new Win32.Mmsystem.WAVEHDR;
         end if;
         -- si deja alloué, desallouer pour être sur d'avoir la bonne taille
         if Buffers(i) /= null then
            Common_types.Free( Buffers(i) );
         end if;
	 -- allocation du buffer
         Buffers(i) := new buff_type'(1..Common_types.buff_size => 0);
	 -- initialisation des champs
	 Headers(i).dwFlags := 0;
	 Headers(i).dwBufferLength := Win32.ULONG(Common_types.buff_size);
         Headers(i).lpData := Win32.TO_PBYTE(Buffers(i)(1)'address);
         Headers(i).dwUser := Win32.DWORD(i);
         Headers(i).dwBytesRecorded := 0;
         Headers(i).dwLoops := 0;
         Headers(i).lpNext := null;
         Headers(i).reserved := 0;
	 -- preparation des buffers
	 Result := waveInPrepareHeader( micro, TO_LPWAVEHDR(Headers(i)), Win32.UINT(WAVEHDR'size/8) );
         Utils_pkg.Check_erreur( Result, "Win_Audio.Start_lecture: waveInPrepareHeader" );

	 -- ajout des buffers
	 Result := waveInAddBuffer( micro, TO_LPWAVEHDR(Headers(i)), Win32.UINT(WAVEHDR'size/8) );
         Utils_pkg.Check_erreur( Result, "Win_Audio.Start_lecture: waveInAddBuffer" );
      end loop;
      --
      -- démarre la lecture
      --
      Result := waveInStart( micro );
      Utils_pkg.Check_erreur( Result, "Win_Audio.Start_lecture: waveInStart" );
      --
      if Debug_pkg.Is_set( debug_flag ) then
         Log.Store_Time;
         Log.Store( "Lecture audio démarrée" );Log.End_Line;
      end if;

   exception
      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
         Log.End_Log;
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_exception );
         -- fin du programme par Windows
	 Win32.Winuser.PostQuitMessage (0);
   end Start_lecture;


   -- ===============================================================================

   procedure Stop_Lecture is
      Result   : Win32.Mmsystem.MMRESULT;
   begin
      -- arrêt carte son
      Result := waveInStop( micro );
      --
      if Debug_pkg.Is_set( debug_flag ) then
         Log.Store_Time;
         Log.Store( "Lecture audio arrêtée" );Log.End_Line;
      end if;
   end Stop_Lecture;


   -- ***************************************************************************************


   function Check_config return boolean is
      result : Win32.Mmsystem.MMRESULT;
      in_caps : WAVEINCAPS;	-- Win32.Mmsystem:1502
      fin : integer;
   begin
      -- on vérifie que le numéro et la string correspondent
      --
      -- micro
      Result := Win32.Mmsystem.waveInGetDevCaps( Common_types.micro_num, TO_LPWAVEINCAPS(in_caps'address), WAVEINCAPS'size/8);
      if result /= Win32.Mmsystem.MMSYSERR_NOERROR then
         Log.Midiout_error( result );
         Utils_pkg.Error_box( Intl.err_midi_title, Intl.err_midi_dev_txt );
         return false;
      end if;
      -- recherche fin de string
      fin := 0;
      while fin <= 31 and then in_caps.szPname(fin) /= Win32.Nul loop
         fin := fin + 1;
      end loop;
      declare
         nom : string(1..fin);
      begin
         for j in 1..fin loop
            nom(j) := Conversions.TO_CHARACTER(in_caps.szPname(j-1));
         end loop;
         if nom /= Common_types.micro_str.all then
            return false;
         end if;
      end;
      --
      -- device midi
      result := Win32.Mmsystem.midiOutGetDevCaps( Common_types.midi_num, midi_caps'access, MIDIOUTCAPSA'size / 8 );
      if result /= Win32.Mmsystem.MMSYSERR_NOERROR then
         Log.Midiout_error( result );
         Utils_pkg.Error_box( Intl.err_midi_title, Intl.err_midi_dev_txt );
         return false;
      end if;
      --
      no_midi_device := false;
      -- recherche fin de string
      fin := 0;
      while fin <= 31 and then midi_caps.szPname(fin) /= Win32.Nul loop
         fin := fin + 1;
      end loop;
      declare
         nom : string(1..fin);
      begin
         for j in 1..fin loop
            nom(j) := Conversions.TO_CHARACTER(midi_caps.szPname(j-1));
         end loop;
         if nom /= Common_types.midi_str.all then
            return false;
         end if;
      end;
      --
      -- ça correspond
      return true;
   end Check_config;


end Win_Audio;

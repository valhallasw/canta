with Unchecked_conversion;
with System;
with System.Address_Image;
with Interfaces.C;	use Interfaces.C;
with Win32;
with Win32.Mmsystem;	use Win32.Mmsystem;

with Text_io;	use Text_io;

procedure Dump_mixer is

   MIDICAPS_STREAM : constant := 8;
   MOD_WAVETABLE   : constant := 6;
   MOD_SWSYNTH     : constant := 7;
   WAVECAPS_SAMPLEACCURATE : constant := 32;

   -- handle pour le mixer
   mixer : Win32.Mmsystem.HMIXER;
   -- capacités
   mixer_cap    : Win32.Mmsystem.MIXERCAPSA;
   midiout_caps : Win32.Mmsystem.MIDIOUTCAPSA;
   midiin_caps  : Win32.Mmsystem.MIDIINCAPSA;
   wavein_caps  : Win32.Mmsystem.WAVEINCAPS;
   waveout_caps : Win32.Mmsystem.WAVEOUTCAPS;

   -- pour les lignes
   line        : Win32.Mmsystem.MIXERLINE;			-- win32-mmsystem.ads:1673

   -- capacités pour les Device MIDI
--   midi_caps : Win32.Mmsystem.MIDIOUTCAPSA;

   -- nombre de device, ie mixer
   nb_dev, nb_wave_in, nb_midi_out, nb_wave_out, nb_midi_in : Win32.UINT;

   Result : Win32.Mmsystem.MMRESULT;

   f_out : Text_io.file_type;

   nbr_connect : Win32.DWORD;
   -- --------------------------------------------------------------------------

   -- Address -> types Windows
   function To_PMixerCapsA is new Unchecked_conversion( System.Address, Win32.Mmsystem.LPMIXERCAPSA);
   function TO_LPHMIXER    is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPHMIXER );
   function TO_LPMIXERLINE     is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPMIXERLINE );
   function TO_LPMIXERCONTROLA is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPMIXERCONTROLA );
   function TO_LPMIXERLINECONTROLSA
                               is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPMIXERLINECONTROLSA );
   function TO_LPMIXERCONTROLDETAILS
                               is new Unchecked_Conversion( System.Address, Win32.Mmsystem.LPMIXERCONTROLDETAILS );
   function TO_WAVEINCAPS is new  Unchecked_Conversion( System.Address, Win32.Mmsystem.PWAVEINCAPSA);
   function TO_WAVEOUTCAPS is new  Unchecked_Conversion( System.Address, Win32.Mmsystem.LPWAVEOUTCAPSA );
   function TO_MIDIINCAPS is new  Unchecked_Conversion( System.Address, Win32.Mmsystem.PMIDIINCAPSA);
   function TO_MIDIOUTCAPS is new  Unchecked_Conversion( System.Address, Win32.Mmsystem.PMIDIOUTCAPSA);
   --
   -- types Windows -> Address
   function TO_ADDRESS is new Unchecked_Conversion( Win32.DWORD, System.Address );

   -- ==========================================================================


   function To_ada( sz : Win32.CHAR_Array ) return string is
      s : string(1..sz'length) := (others => ' ');
      j : natural := 0;
   begin
      for i in s'range loop
         exit when sz(j) = Interfaces.C.nul;
         s(i) := character( sz(j) );
         j := j+ 1;
      end loop;
      return s(1..j);
   end To_Ada;

   function Image( n : integer ) return string is
      s : string := integer'image(n);
   begin
      return s(s'first+1..s'last);
   end Image;

   procedure Separ( level : integer ) is
   begin
      case level is
         when 1 => Text_io.put_line( f_out, "==================================");
         when 2 => Text_io.put_line( f_out, "----------------------------------");
         when 3 => Text_io.put_line( f_out, ascii.ht & ascii.ht & "------------");
         when 4 => Text_io.put_line( f_out, ascii.ht & ascii.ht & ascii.ht & "...........");
         when others => null;
      end case;
   end Separ;


   procedure Ecrit( level : natural; text : string ) is
   begin
      for i in 1..level loop
         Text_io.Put( f_out, ascii.ht );
      end loop;
      Text_io.Put( f_out, text );
   end Ecrit;

   procedure Ecrit_line( level : natural; text : string ) is
   begin
      for i in 1..level loop
         Text_io.Put( f_out, ascii.ht );
      end loop;
      Text_io.Put_line( f_out, text );
   end Ecrit_line;

      procedure Ecrit_type( level : integer; ComponentType : Win32.DWORD) is
      begin
         Ecrit(level, "Type     : ");
         case ComponentType is
            -- Destinations
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_DIGITAL =>
               Ecrit_line(0, "Destination Digital");
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_LINE =>
               Ecrit_line(0, "Destination Line level" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_MONITOR =>
               Ecrit_line(0, "Destination Monitor" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_SPEAKERS =>
               Ecrit_line(0, "Destination Speakers" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_HEADPHONES =>
               Ecrit_line(0, "Destination Headphone" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_TELEPHONE =>
               Ecrit_line(0, "Destination Telephone" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_WAVEIN =>
               Ecrit_line(0, "Destination Wavein" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_VOICEIN =>
               Ecrit_line(0, "Destination Voice Recognition" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_DST_UNDEFINED =>
               Ecrit_line(0, "Destination Undefined");
            -- Sources
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_DIGITAL =>
               Ecrit_line(0, "Source Digital");
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_LINE =>
               Ecrit_line(0, "Source Line level" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_MICROPHONE =>
               Ecrit_line(0, "Source Microphone" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_SYNTHESIZER =>
               Ecrit_line(0, "Source Synthesizer" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_COMPACTDISC =>
               Ecrit_line(0, "Source CD Audio" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_TELEPHONE =>
               Ecrit_line(0, "Source Telephone" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_PCSPEAKER =>
               Ecrit_line(0, "Source PC Speaker" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_WAVEOUT =>
               Ecrit_line(0, "Source Wave Out" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_AUXILIARY =>
               Ecrit_line(0, "Source Auxiliary" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_ANALOG =>
               Ecrit_line(0, "Source Analog" );
            when Win32.Mmsystem.MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED =>
               Ecrit_line(0, "Source Undefined" );
            when others => Ecrit_line(0, "Indéfini" & Win32.DWORD'image(ComponentType) );
         end case;
      end Ecrit_type;

   procedure Ecrit_flags( level : integer; flags :Win32.DWORD ) is
   begin
      Ecrit(level, "Flags    : " );
      if (flags and Win32.Mmsystem.MIXERLINE_LINEF_SOURCE) = 0 then
         Ecrit(0, "Destination, " );
      else
         Ecrit(0, "Source, " );
      end if;
      if (flags and Win32.Mmsystem.MIXERLINE_LINEF_ACTIVE) = 0 then
         Ecrit(0, "Inactif, " );
      else
         Ecrit(0, "Actif, " );
      end if;
      if (flags and Win32.Mmsystem.MIXERLINE_LINEF_DISCONNECTED) = 0 then
         Ecrit_line(0, "Connecté" );
      else
         Ecrit_line(0, "Déconnecté" );
      end if;
   end Ecrit_flags;

   procedure Ecrit_control_type( level : integer; CtrlType : Win32.DWORD ) is
   begin
      Ecrit(level, "Ctrl Type : ");
      case CtrlType is
         -- class 0 : Custom
         when MIXERCONTROL_CONTROLTYPE_CUSTOM => Ecrit_line(0, "CUSTOM");
         -- calss 1 : Meter
         when MIXERCONTROL_CONTROLTYPE_BOOLEANMETER  => Ecrit_line(0, "METER BOOLEAN");
         when MIXERCONTROL_CONTROLTYPE_SIGNEDMETER   => Ecrit_line(0, "METER SIGNED");
         when MIXERCONTROL_CONTROLTYPE_PEAKMETER     => Ecrit_line(0, "METER PEAK");
         when MIXERCONTROL_CONTROLTYPE_UNSIGNEDMETER => Ecrit_line(0, "METER UNSIGNED");
         -- class 2 : switch
         when MIXERCONTROL_CONTROLTYPE_BOOLEAN   => Ecrit_line(0, "SWITCH BOOLEAN");
         when MIXERCONTROL_CONTROLTYPE_BUTTON    => Ecrit_line(0, "SWITCH BUTTON");
         when MIXERCONTROL_CONTROLTYPE_MONO      => Ecrit_line(0, "SWITCH MONO");
         when MIXERCONTROL_CONTROLTYPE_MUTE      => Ecrit_line(0, "SWITCH MUTE");
         when MIXERCONTROL_CONTROLTYPE_ONOFF     => Ecrit_line(0, "SWITCH ONOFF");
         when MIXERCONTROL_CONTROLTYPE_STEREOENH => Ecrit_line(0, "SWITCH STEREOENH");
         when MIXERCONTROL_CONTROLTYPE_LOUDNESS  => Ecrit_line(0, "SWITCH LOUDNESS");
         -- class 3 : Number
         when MIXERCONTROL_CONTROLTYPE_DECIBELS => Ecrit_line(0, "NUMBER DECIBELS");
         when MIXERCONTROL_CONTROLTYPE_PERCENT  => Ecrit_line(0, "NUMBER PERCENT");
         when MIXERCONTROL_CONTROLTYPE_SIGNED   => Ecrit_line(0, "NUMBER SIGNED");
         when MIXERCONTROL_CONTROLTYPE_UNSIGNED => Ecrit_line(0, "NUMBER UNSIGNED");
         -- class 4 : Slider
         when MIXERCONTROL_CONTROLTYPE_PAN       => Ecrit_line(0, "SLIDER PAN");
         when MIXERCONTROL_CONTROLTYPE_QSOUNDPAN => Ecrit_line(0, "SLIDER QSOUNDPAN");
         when MIXERCONTROL_CONTROLTYPE_SLIDER    => Ecrit_line(0, "SLIDER");
         -- class 5 : Fader
         when MIXERCONTROL_CONTROLTYPE_BASS      => Ecrit_line(0, "FADER BASS");
         when MIXERCONTROL_CONTROLTYPE_EQUALIZER => Ecrit_line(0, "FADER EQUALIZER");
         when MIXERCONTROL_CONTROLTYPE_FADER     => Ecrit_line(0, "FADER");
         when MIXERCONTROL_CONTROLTYPE_TREBLE    => Ecrit_line(0, "FADER TREBLE");
         when MIXERCONTROL_CONTROLTYPE_VOLUME    => Ecrit_line(0, "FADER VOLUME");
         -- class 6 : Time
         when MIXERCONTROL_CONTROLTYPE_MICROTIME => Ecrit_line(0, "TIME MICROSEC");
         when MIXERCONTROL_CONTROLTYPE_MILLITIME => Ecrit_line(0, "TIME MILLISEC");
         -- class 7 : List
         when MIXERCONTROL_CONTROLTYPE_MIXER          => Ecrit_line(0, "LIST MIXER");
         when MIXERCONTROL_CONTROLTYPE_MULTIPLESELECT => Ecrit_line(0, "LIST MULTIPLESELECT");
         when MIXERCONTROL_CONTROLTYPE_MUX            => Ecrit_line(0, "LIST MUX");
         when MIXERCONTROL_CONTROLTYPE_SINGLESELECT   => Ecrit_line(0, "LIST SINGLESELECT");
         -- indéfini
         when others => Ecrit_line(0, "Indéfini" & Win32.DWORD'image(CtrlType) );
      end case;
   end Ecrit_control_type;

   procedure Dump_line( level : integer) is
      line_control  : Win32.Mmsystem.MIXERLINECONTROLS;		-- win32-mmsystem.ads:1889
      details : MIXERCONTROLDETAILS;
      fader   : MIXERCONTROLDETAILS_UNSIGNED; -- détail pour les fader
      on_off  : MIXERCONTROLDETAILS_BOOLEAN; -- détail pour les switch
      do_fader, do_switch : boolean;
   begin
      -- nom
      Ecrit_line(level, "Nom      : """ & To_Ada(line.szName) & """" );
      -- type
      Ecrit_type(level, line.dwComponentType );
      -- Id
      Ecrit_line(level, "Line Id  : " & System.Address_Image(To_ADDRESS(line.dwLineID)) );
      -- flags
      Ecrit_flags(level, line.fdwLine );
      -- Channels
      Ecrit_line(level, "Channels :" & Win32.DWORD'image(line.cChannels) );
      -- target type
      Ecrit(level,      "Target   : " );
      case line.Target.dwType is
         when Win32.Mmsystem.MIXERLINE_TARGETTYPE_AUX => Ecrit_line(0,"AUX");
         when Win32.Mmsystem.MIXERLINE_TARGETTYPE_WAVEOUT => Ecrit_line(0,"WAVEOUT");
         when Win32.Mmsystem.MIXERLINE_TARGETTYPE_WAVEIN => Ecrit_line(0,"WAVEIN");
         when Win32.Mmsystem.MIXERLINE_TARGETTYPE_MIDIOUT => Ecrit_line(0,"MIDIOUT");
         when Win32.Mmsystem.MIXERLINE_TARGETTYPE_MIDIIN => Ecrit_line(0,"MIDIIN");
         when Win32.Mmsystem.MIXERLINE_TARGETTYPE_UNDEFINED => Ecrit_line(0,"UNDEFINED");
         when others => Ecrit_line(0, "Indéfini" & Win32.DWORD'image(line.Target.dwType) );
      end case;
      -- Controls
      Ecrit_line(level, "Controls :" & Win32.DWORD'image(line.cControls) );
      --
      -- les controls
      if line.cControls > 0 then
         declare
            mixer_control : array(1..Integer(line.cControls)) of Win32.Mmsystem.MIXERCONTROL;			-- win32-mmsystem.ads:1780
         begin
            for cont in 1..Integer(line.cControls) loop
               mixer_control(cont).cbStruct := (Win32.Mmsystem.MIXERCONTROL'size) / 8;
            end loop;
            -- init de la structure line_control
            line_control.cbStruct  := (line_control'size) / 8;
            line_control.dwLineID  := line.dwLineID;
            line_control.cbmxctrl  := (Win32.Mmsystem.MIXERCONTROL'size) / 8;
            line_control.pamxctrl  := TO_LPMIXERCONTROLA( mixer_control(1)'address );
            line_control.cControls := line.cControls;
            --
            Result := Win32.Mmsystem.mixerGetLineControls( mixer,
                               TO_LPMIXERLINECONTROLSA( line_control'address ),
                               MIXER_GETLINECONTROLSF_ALL );
            -- pour chaque control
            for cont in 1..Integer(line.cControls) loop
                Separ(level+2);
                -- Id
                Ecrit_line(level+1, "Control Id: " & Win32.DWORD'image(mixer_control(cont).dwControlID) );
                -- son type
                Ecrit_control_type(level+1, mixer_control(cont).dwControlType );
                -- nom du control
                Ecrit_line(level+1, "Nom       : """ & To_Ada(mixer_control(cont).szName) & """" );
                -- flags
                Ecrit(level+1, "Flags     : ");
                if (mixer_control(cont).fdwControl and MIXERCONTROL_CONTROLF_DISABLED) = 0 then
                   Ecrit(0, "Activé, ");
                else
                   Ecrit(0, "Desactivé, ");
                end if;
                if (mixer_control(cont).fdwControl and MIXERCONTROL_CONTROLF_MULTIPLE) = 0 then
                   Ecrit(0, "Simple, ");
                else
                   Ecrit(0, "Multiple, ");
                end if;
                if (mixer_control(cont).fdwControl and MIXERCONTROL_CONTROLF_UNIFORM) = 0 then
                   Ecrit_line(0, "Non-uniforme");
                else
                   Ecrit_line(0, "Uniforme");
                end if;
                -- multiple
                if (mixer_control(cont).fdwControl and MIXERCONTROL_CONTROLF_MULTIPLE) /= 0 then
                   Ecrit_line(level+1, "Multiple  : " & Win32.DWORD'image(mixer_control(cont).cMultipleItems) );
                end if;
                --
                Ecrit_line(level+1, "Max       :" & Win32.DWORD'image(mixer_control(cont).Bounds.DD.dwMaximum) );
                Ecrit_line(level+1, "Min       :" & Win32.DWORD'image(mixer_control(cont).Bounds.DD.dwMinimum) );
                --
                do_fader := false;
                do_switch := false;
                case (mixer_control(cont).dwControlType and MIXERCONTROL_CT_CLASS_MASK ) is

                   when MIXERCONTROL_CT_CLASS_FADER =>
                     details.paDetails := Win32.LPVOID( fader'address );      -- valeurs
                     details.cbDetails := (MIXERCONTROLDETAILS_UNSIGNED'size)/8;
                     do_fader := true;

                   when MIXERCONTROL_CT_CLASS_SWITCH =>
                      details.paDetails := Win32.LPVOID( On_off'address );      -- valeurs
                      details.cbDetails := (MIXERCONTROLDETAILS_BOOLEAN'size)/8;
                      do_switch := true;

                   when others => null;

                end case;
                if do_fader or do_switch then
                   details.cbStruct    := (MIXERCONTROLDETAILS'size)/8;
                   details.dwControlID := mixer_control(cont).dwControlID;	  	-- ID du control que l'on lit
                   details.cChannels   := 1;				  -- tous
                   details.HW_DW.hwndOwner := System.Null_address;	  -- pas utilisé dans ce cas
                   --
                   Result := mixerGetControlDetails( mixer,
                            TO_LPMIXERCONTROLDETAILS( details'address ),
                            MIXER_GETCONTROLDETAILSF_VALUE );
                   if do_fader then
                      Ecrit_line(level+1, "Valeur    :" & Win32.DWORD'image(fader.dwValue) );
                   else
                      Ecrit_line(level+1, "Valeur    :" & Win32.LONG'image(On_off.fValue) );
                   end if;
                end if;
            end loop;
         end;
      end if;
   end Dump_line;

begin
   -- création fichier de sortie
   Create( f_out, out_file, "dump_mixer.txt" );
   --
   Separ(1);
   Ecrit_line(0, "Dump_mixer par Chaumet Software");
   Separ(1);
   -- lecture nombre de mixer
   nb_dev := Win32.Mmsystem.mixerGetNumDevs;
   Ecrit_line(0, "Nombre de mixer =" & Win32.UINT'image(nb_dev) );
   -- nombre de wavein
   nb_wave_in := Win32.Mmsystem.waveInGetNumDevs;
   Ecrit_line(0, "        Wave In =" & Win32.UINT'image(nb_wave_in) );
   -- nombre de waveout
   nb_wave_out := Win32.Mmsystem.waveOutGetNumDevs;
   Ecrit_line(0, "       Wave Out =" & Win32.UINT'image(nb_wave_out) );
   -- nombre de MIDIin
   nb_midi_in := Win32.Mmsystem.midiInGetNumDevs;
   Ecrit_line(0, "        MIDI In =" & Win32.UINT'image(nb_midi_in) );
   -- nombre de MIDIout
   nb_midi_out := Win32.Mmsystem.midiOutGetNumDevs;
   Ecrit_line(0, "       MIDI Out =" & Win32.UINT'image(nb_midi_out) );
   -- --------------------------------------------------------------------------
   Separ(1);
   Ecrit_line(0, "           MIXERS");
   --
   -- pour chaque mixer...
   for dev_num in 0..nb_dev-1 loop
      Separ(1);
      -- lecture info du mixer
      result := Win32.Mmsystem.MixerGetDevCaps(
                         Win32.UINT(dev_num), To_PMixerCapsA(mixer_cap'address), mixer_cap'size/8 );
      -- ecriture info du mixer
      Ecrit_line(0, "              Nom: """ & To_ada( mixer_cap.szPname ) & """" );
      Ecrit_line(0, "        Device Id:" & Win32.UINT'image(dev_num) );
      Ecrit_line(0, "  Manufacturer Id:" & Win32.WORD'image(mixer_cap.wMid) );
      Ecrit_line(0, "       Product Id:" & Win32.WORD'image(mixer_cap.wPid) );
      Ecrit_line(0, "   Driver Version:" & Integer'image(Integer(mixer_cap.vDriverVersion)/256)
                         & "." & Image(Integer(mixer_cap.vDriverVersion) mod 256)  );
      Ecrit_line(0, "    Support Flags:" & Win32.DWORD'image(mixer_cap.fdwSupport) );
      Ecrit_line(0, "Destination lines:" & Win32.DWORD'image(mixer_cap.cDestinations) );
      -- ouverture du mixer
      result := Win32.Mmsystem.mixerOpen( TO_LPHMIXER(mixer'address),
                          Win32.UINT(dev_num),	       -- numéro du device
                          0, 0, 0 );
      -- dump du contenu
      -- pour chaque ligne de destination
      for dest in 0..mixer_cap.cDestinations-1 loop
         Separ(2);
         --
         Ecrit_line( 0, "Ligne n°" & Image(Integer(dest)) );
         -- init de la structure line
         line.cbStruct := Win32.DWORD(Win32.Mmsystem.MIXERLINE'size/8);
         line.dwDestination := dest;
         --
         Result := Win32.Mmsystem.mixerGetLineInfo( mixer,
                               TO_LPMIXERLINE( line'address ),
                               Win32.Mmsystem.MIXER_GETLINEINFOF_DESTINATION );
         Dump_line(1);
         --
         Separ(2);
         Ecrit_line(2, "Sources connectables:");
         -- pour chaque source pouvant être connectée à cette destination
         nbr_connect := line.cConnections;
         for src in 0..nbr_connect-1 loop
            Separ(3);
            -- init de la structure line
            line.cbStruct := Win32.DWORD(Win32.Mmsystem.MIXERLINE'size/8);
            line.dwDestination := dest;
            line.dwSource := src;
            --
            Result := Win32.Mmsystem.mixerGetLineInfo( mixer,
                               TO_LPMIXERLINE( line'address ),
                               Win32.Mmsystem.MIXER_GETLINEINFOF_SOURCE );
            --
            Dump_line(2);
         end loop;
         --
      end loop;
      -- fermeture du mixer
      result := Win32.Mmsystem.MixerClose( mixer );
   end loop;
   --
   Separ(1);
   if nb_wave_in > 0 then
      Ecrit_line(0, "           WAVE IN");
      for wi in 0..nb_wave_in-1 loop
         Separ(1);
         --
         Result := Win32.Mmsystem.waveInGetDevCaps( wi, TO_WAVEINCAPS(wavein_caps'address),
                            Win32.Mmsystem.WAVEINCAPSA'size / 8 );
         --
         Ecrit_line(0, "       Device Id :" & Win32.UINT'image(wi) );
         Ecrit_line(0, "             Nom : """ & To_ada(wavein_caps.szPname) & """" );
         Ecrit_line(0, " Manufacturer Id :" & Win32.WORD'image(wavein_caps.wMid) );
         Ecrit_line(0, "      Product Id :" & Win32.WORD'image(wavein_caps.wPid) );
         Ecrit_line(0, "  Driver Version :" & Integer'image(Integer(wavein_caps.vDriverVersion)/256)
                         & "." & Image(Integer(wavein_caps.vDriverVersion) mod 256)  );
         Ecrit_line(0, "        Channels :" & Win32.WORD'image(wavein_caps.wChannels) );
         Ecrit_line(0, "         Formats : " & System.Address_Image(To_ADDRESS(wavein_caps.dwFormats)) );
      end loop;
      Separ(1);
   end if;
   -- --------------------------------------------------------------------------
   if nb_wave_out > 0 then
      Ecrit_line(0, "           WAVE OUT");
      for wo in 0..nb_wave_out-1 loop
         Separ(1);
         --
         Result := Win32.Mmsystem.waveOutGetDevCaps( wo, TO_WAVEOUTCAPS(waveout_caps'address),
                            Win32.Mmsystem.WAVEOUTCAPS 'size / 8 );
         --
         Ecrit_line(0, "       Device Id :" & Win32.UINT'image(wo) );
         Ecrit_line(0, "             Nom : """ & To_ada(waveout_caps.szPname) & """" );
         Ecrit_line(0, " Manufacturer Id :" & Win32.WORD'image(waveout_caps.wMid) );
         Ecrit_line(0, "      Product Id :" & Win32.WORD'image(waveout_caps.wPid) );
         Ecrit_line(0, "  Driver Version :" & Integer'image(Integer(waveout_caps.vDriverVersion)/256)
                         & "." & Image(Integer(waveout_caps.vDriverVersion) mod 256)  );
         Ecrit_line(0, "        Channels :" & Win32.WORD'image(waveout_caps.wChannels) );
         Ecrit_line(0, "         Formats : " & System.Address_Image(To_ADDRESS(waveout_caps.dwFormats)) );
         Ecrit(0,      "         Support : ");
         if (waveout_caps.dwSupport and WAVECAPS_LRVOLUME) /= 0 then
            Ecrit(0, "Droite-Gauche, ");
         end if;
         if (waveout_caps.dwSupport and WAVECAPS_PITCH) /= 0 then
            Ecrit(0, "Pitch, ");
         end if;
         if (waveout_caps.dwSupport and WAVECAPS_PLAYBACKRATE) /= 0 then
            Ecrit(0, "Playback rate, ");
         end if;
         if (waveout_caps.dwSupport and WAVECAPS_SYNC) /= 0 then
            Ecrit(0, "Synchrone, ");
         end if;
         if (waveout_caps.dwSupport and WAVECAPS_VOLUME) /= 0 then
            Ecrit(0, "Volume, ");
         end if;
         if (waveout_caps.dwSupport and WAVECAPS_SAMPLEACCURATE) /= 0 then
            Ecrit(0, "Sample acurate");
         end if;
         Ecrit_line(0, "");
      end loop;
      Separ(1);
   end if;
   -- --------------------------------------------------------------------------
   if nb_midi_in > 0 then
      Ecrit_line(0, "           MIDI IN");
      for mi in 0..nb_midi_in-1 loop
         Separ(1);
         --
         Result := Win32.Mmsystem.midiInGetDevCaps( mi, TO_MIDIINCAPS(midiin_caps'address),
                              MIDIINCAPSA'size / 8 );
         --
         Ecrit_line(0, "       Device Id :" & Win32.UINT'image(mi) );
         Ecrit_line(0, "             Nom : """ & To_ada(midiin_caps.szPname) & """" );
         Ecrit_line(0, " Manufacturer Id :" & Win32.WORD'image(midiin_caps.wMid) );
         Ecrit_line(0, "      Product Id :" & Win32.WORD'image(midiin_caps.wPid) );
         Ecrit_line(0, "  Driver Version :" & Integer'image(Integer(midiin_caps.vDriverVersion)/256)
                         & "." & Image(Integer(midiin_caps.vDriverVersion) mod 256)  );
      end loop;
      Separ(1);
   end if;
   -- --------------------------------------------------------------------------
   if nb_midi_out > 0 then
      Ecrit_line(0, "           MIDI OUT");
      for mo in 0..nb_midi_out-1 loop
         Separ(1);
         --
         Result := Win32.Mmsystem.midiOutGetDevCaps( mo, TO_MIDIOUTCAPS(midiout_caps'address),
                              MIDIOUTCAPSA'size / 8 );
         --
         Ecrit_line(0, "       Device Id :" & Win32.UINT'image(mo) );
         Ecrit_line(0, "             Nom : """ & To_ada(midiout_caps.szPname) & """" );
         Ecrit_line(0, " Manufacturer Id :" & Win32.WORD'image(midiout_caps.wMid) );
         Ecrit_line(0, "      Product Id :" & Win32.WORD'image(midiout_caps.wPid) );
         Ecrit_line(0, "  Driver Version :" & Integer'image(Integer(midiout_caps.vDriverVersion)/256)
                         & "." & Image(Integer(midiout_caps.vDriverVersion) mod 256)  );
         Ecrit(0,      "      Technology : ");
         case midiout_caps.wTechnology is
            when MOD_MIDIPORT  => Ecrit_line(0, "MIDI hardware port");
            when MOD_SYNTH     => Ecrit_line(0, "Synthesizer" );
            when MOD_SQSYNTH   => Ecrit_line(0, "Square wave synthesizer" );
            when MOD_FMSYNTH   => Ecrit_line(0, "FM synthesizer" );
            when MOD_MAPPER    => Ecrit_line(0, "Microsoft MIDI mapper" );
            when MOD_WAVETABLE => Ecrit_line(0, "Hardware wavetable synthesizer" );
            when MOD_SWSYNTH   => Ecrit_line(0, "Software synthesizer" );
            when others        => Ecrit_line(0, "Indéfinie !");
         end case;
         Ecrit_line(0, "            Voix :" & Win32.WORD'image(midiout_caps.wVoices) );
         Ecrit_line(0, "       Max Notes :" & Win32.WORD'image(midiout_caps.wNotes) );
         Ecrit_line(0, "        Channels :" & Win32.WORD'image(midiout_caps.wChannelMask) );
              Ecrit(0, "         Support : ");
         if (midiout_caps.dwSupport and MIDICAPS_VOLUME) /= 0 then
            Ecrit(0, "Volume, ");
         end if;
         if (midiout_caps.dwSupport and MIDICAPS_LRVOLUME) /= 0 then
            Ecrit(0, "Droite-Gauche, ");
         end if;
         if (midiout_caps.dwSupport and MIDICAPS_CACHE) /= 0 then
            Ecrit(0, "Caching, ");
         end if;
         if (midiout_caps.dwSupport and MIDICAPS_STREAM) /= 0 then
            Ecrit(0, "Streaming ");
         end if;
         Ecrit_line(0,"");
      end loop;
      Separ(1);
   end if;
   --
   --
   -- fermeture fichier de sortie
   Close( f_out );
end Dump_mixer;

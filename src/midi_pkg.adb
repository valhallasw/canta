with Calendar;
with Unchecked_deallocation;
with Interfaces.C;	use Interfaces.C;
with Ada.Characters.Handling;

with Win32;		use Win32;
with Win32.Winuser;
with Win32.Mmsystem;	use Win32.Mmsystem;

with Conversions;	use Conversions;
with Common_types;	use Common_types;
with Midifile_pkg;	use Midifile_pkg;
with Dialog_pkg;
with utils_pkg;
with Intl;
with General_Midi;
with Log;
with Affichage_pkg;
with Process_pkg;
with Timer_pkg;
with Resources_pkg;
with Data_pkg;		use Data_pkg;
with Config_pkg;

package body Midi_pkg is

   instrument_voix : constant := 73;	-- flute

   -- nombre minimum de textes dans une piste pour que cela soit les paroles
   min_paroles : constant := 20;

   -- flags:
   -- fichier MIDI chargé
   file_loaded : boolean := False;
   -- nom du fichier
   current_file_name : string_pt;

   -- status du player
   status : player_status_type := stopped;
   -- temps de début de la pause
   start_pause : integer := 0;

   track_to_play : integer := -1;

   -- poiteur sur les données MIDI
   current_midi : Midifile_pkg.midi_data_ptr;
   -- piste sélectionnée pour la mélodie
   melodie_num : integer;
   -- les paroles à afficher
   Paroles_track : midifile_pkg.track_data_ptr;
   cur_parole : midifile_pkg.events_ptr;		-- pointeur sur parole courante

   -- décalage de temps entre palyer et timer
   start_time_offset : integer;

   -- temps minimum pour une vraie polyphonie (en ms)
   min_time_poly : constant := 200;
   -- tempo par defaut en micro-secondes
   default_tempo : constant := 500_000;	-- 0.5s => 120 bpm
   -- temps interne du player
   player_time : natural := 0;
   -- temps de fin du morceau: max du temps  des evenst 'end_of_track'
   end_time : natural := 0;


   -- handler du device midi
   midi_handler : Win32.Mmsystem.HMIDIOUT;

   -- debut de la liste
   melodie_track : Data_pkg.midi_event_ptr;
   -- event courant à afficher dans cette piste
   melodie_event : Data_pkg.midi_event_ptr;

   -- liste des mesures
   mesure_track : Data_pkg.midi_event_ptr;
   -- mesure courante à afficher
   mesure_event : Data_pkg.midi_event_ptr;
   -- delai maximu d'alignement des mesures
   max_align_mesure : constant := 100;

   -- niveau de transposition en 1/2 tons
   transposition : integer;

   --
   procedure Free is new Unchecked_deallocation( object => Data_pkg.midi_event_rec, name => Data_pkg.midi_event_ptr );

   -- event pour que le user joue une note avec la souris
   user_evt_on : midifile_pkg.events_ptr := new events_rec_type'( channel_event, note_on, 0, 0, null, null, 0, 0, 100);
   user_evt_off : midifile_pkg.events_ptr := new events_rec_type'( channel_event, note_off, 0, 0, null, null, 0, 0, 0);
   -- flag user joue note MIDI
   user_playing : boolean := false;

   -- event de changement d'instrument
   change_inst_evt : midifile_pkg.events_ptr := new events_rec_type'(
           event_type => channel_event,
           kind => program_change,
           tick => 0,
           time => 0,
           next => null,
           prev => null,
           channel => 0,
           program_number => 0 );	-- sera modifié quand utilisé

   -- +- tolerance pour coincidence mélodie/paroles en ms
   parole_tolerance : constant := 150;

   -- brne pour la lecture
   mesure_debut : natural := 1;
   mesure_fin : natural := natural'last;
   mode_loop : boolean := false;

   -- volume pour la mélodie et l'accompagnement
   melody_volume : natural;
   accomp_volume : natural;
   -- valeur des volume min et max pour la melodie (dans le fichier midi)
   max_melody_vol : natural;
   min_melody_vol : natural;
   -- idem pour l'accompagnement
   max_accomp_vol : natural;
   min_accomp_vol : natural;

   type volume_array is array(0..127) of byte;
   tab_vol_melody : volume_array;
   tab_vol_accomp : volume_array;

   -- ****************************************************************

   -- recherche du tempo à ce moment là
   function Get_tempo( root : midi_data_ptr;
                       at_this_tick : natural ) return natural is
      ptr : events_ptr;
   begin
      -- cas trivial: pas de tempo défini, on prend la valeur par défaut
      if root.tempos = null then
         -- valeur par défaut : 120 bpm
         return default_tempo;
      end if;
      ---
      -- recherche du tempo en cours
      ptr := root.tempos;
      while ptr /= null loop
         if ptr.tick > at_this_tick then
            -- on est trop loin
            return default_tempo;
         end if;
         if ptr.next = null then
            return ptr.tempo;
         end if;
         if ptr.next.tick > at_this_tick then
            -- le suivant est trop loin, celui là est le bon
            return ptr.tempo;
         end if;
         -- suivant
         ptr := ptr.next;
      end loop;
      --
      -- test résultat de la recherche
      if ptr = null then
         return default_tempo;
      else
         return ptr.tempo;
      end if;
   end Get_Tempo;



   function Difference_absolue( root : midi_data_ptr;
                                tick_1, tick_2 : natural ) return natural is
      cur_tempo : natural;
      resu : natural;
   begin
      cur_tempo := Get_tempo( root, tick_1 );
      -- calcul temps absolu en milli-secondes
      resu := (abs(tick_2 - tick_1) * (cur_tempo / 1000)) / root.ticks_per_beats ;
      return resu;
   exception
      when others => return 0;
   end Difference_absolue;



   procedure Analyse_midi( root : midi_data_ptr ) is
      cur_track : track_data_ptr;
      cur_event : events_ptr;
      cur_time : Natural;
      same_time : natural;
      start_poly : array(0..128) of natural;
      last_text : string_pt;
      multi_text : boolean;
   begin
      -- test nombre de pistes
      if root.nbr_tracks = 0 then
         raise invalid_midi;
      end if;
      -- analyse chaque piste
      cur_track := root.tracks;
      while cur_track /= null loop
         cur_track.polyphonie := 0;
         if cur_track.events /= null then
            -- parcours des events
            cur_event := cur_track.events;
            -- initialisation
            same_time := 0;
            cur_time := 0;
            multi_text := false;
            last_text := null;
            --
            while cur_event /= null loop
               if cur_event.event_type = channel_event then
                  -- channel event: traiter les note-on et note-off pour calculer la polyphonie
                  -- le drum channel doit être ignorer
                  if cur_event.channel /= General_midi.drum_channel then
                     -- calcul polyphonie
                     if cur_event.kind = note_on then
                        -- une note de plus
                        same_time := same_time + 1;
                        -- se souvenir du moment du début de ce niveau de polyphonie
                        start_poly(same_time) := cur_event.tick;
                     elsif cur_event.kind = note_off then
                        -- une note de moins
                        -- le max du nombre de notes jouées en même temps donne le degré de polyphonie
                        -- mais cela doit avoir duré suffisamment longtemps
                        if Difference_absolue(root, cur_event.tick, start_poly(same_time)) > min_time_poly then
                           -- on retient le max
                           if cur_track.polyphonie < same_time then
                              cur_track.polyphonie := same_time;
                           end if;
                        end if;
                        -- le niveau de polyphonie a diminué
                        if same_time > 0 then
                           same_time := same_time - 1;
                        end if;
                     end if;
                  end if;
                  --
               elsif cur_event.event_type = meta_event then
                  -- chercher les textes pouvant servir de nom de piste
                  if cur_event.kind = meta_text then
                     if last_text = null then
                        -- premier text rencontré, s'en souvenir
                        last_text := cur_event.text;
                     else
                        -- plusieurs texts dans cettepiste, les oublier
                        multi_text := true;
                     end if;
                  end if;
               end if;
               -- event suivant
               cur_event := cur_event.next;
            end loop;
         end if;
         --
         if last_text /= null and not multi_text then
            -- un seul text dans cette piste
            -- si pas d'autre noms définis, prendre celui là
            if cur_track.track_name = null and cur_track.instr_name = null then
               cur_track.track_name := last_text;
            end if;
         end if;
         --
         -- piste suivante
         cur_track := cur_track.next;
      end loop;
   end Analyse_midi;


   -- ================================================================


   -- calcule du temps absolu de chaque evenement
   procedure Ajuste_temps( root : midifile_pkg.midi_data_ptr ) is
      evt : midifile_pkg.events_ptr;
      cur_tempo : natural;
      trk : midifile_pkg.track_data_ptr;
      last_tick : natural;
      last_time : natural;

      procedure Ajuste_track( event_list : midifile_pkg.events_ptr ) is
         evt, tmp : midifile_pkg.events_ptr;
         last_tick : natural;
         last_time : natural;
         cur_tempo : natural;
      begin
         last_tick := 0;
         last_time := 0;
         -- tempo initial
         tmp := root.tempos;
         if tmp /= null and then tmp.tick = 0 then
            cur_tempo := tmp.tempo;
         else
            cur_tempo := default_tempo;
         end if;
         -- liste des events
         evt := event_list;
         while evt /= null loop
            -- ajuste le tempo
            while tmp /= null and then tmp.next /= null and then tmp.next.tick <= evt.tick loop
               tmp := tmp.next;
               last_time := tmp.time;
               last_tick := tmp.tick;
               cur_tempo := tmp.tempo;
            end loop;
            -- calcul temps en milli-secondes pour cet event
            evt.time := (((evt.tick - last_tick) * (cur_tempo / 1000)) / root.ticks_per_beats) + last_time;
            -- met à jour le max des temps
            if evt.time > end_time then
               end_time := evt.time;
            end if;
            -- evenement suivant
            evt := evt.next;
         end loop;
      end Ajuste_track;

   begin
      -- temps de fin du morceau, raz
      end_time := 0;
      --
      -- on commence par les tempos
      -- tempo par défaut au démarrage
      cur_tempo := default_tempo;
      last_tick := 0;
      last_time := 0;
      evt := root.tempos;
      while evt /= null loop
         -- calcul temps de début en ms
         evt.time := ((evt.tick - last_tick)* (cur_tempo / 1000)) / root.ticks_per_beats + last_time;
         last_tick := evt.tick;
         last_time := evt.time;
         -- changement de tempo
         cur_tempo := evt.tempo;
         -- suivant
         evt := evt.next;
      end loop;
      --
      -- pour chaque piste
      trk := root.tracks;
      while trk /= null loop
         Ajuste_track( trk.events );
         -- piste suivante
         trk := trk.next;
      end loop;
      -- idem pour les signatures
      Ajuste_track( root.signatures );
   end Ajuste_temps;



   procedure raz_current_midi is
      trk : midifile_pkg.track_data_ptr;
   begin
      -- RAZ temps interne du player
      player_time := 0;
      -- init des cur_events
      trk := current_midi.tracks;
      while trk /= null loop
         trk.cur_event := trk.events;
         trk := trk.next;
      end loop;
      -- init de melodie_event en début de liste
      melodie_event := Melodie_track;
      -- init du pointeur des paroles
      if Paroles_track /= null then
         cur_parole := paroles_track.events;
      else
         cur_parole := null;
      end if;
      -- initialise pointeur courant sur la première mesure
      mesure_event := mesure_track;
      --
   end raz_current_midi;


   -- ====================================================================================

   procedure Get_notes( start_time, end_time  : Integer;
                        first_note, last_note : out Data_pkg.midi_event_ptr;
                        time_offset           : out integer ) is
      tmp : Data_pkg.midi_event_ptr;
      deb_play, fin_play : integer;
   begin
      -- converti en temps relatifs du player
      deb_play := start_time - start_time_offset;
      fin_play := end_time - start_time_offset;
      time_offset := start_time_offset;	-- pour la conversion inverse
      --
      -- recherche du début
      tmp := Melodie_track;
      while tmp /= null and then tmp.end_time < deb_play loop
         tmp := tmp.next;
      end loop;
      if tmp = null then
         first_note := null;
         last_note := null;
         return;
      end if;
      -- la premier note est trouvée
      first_note := tmp;
      --
      -- recherche la derniere
      while tmp /= null and then tmp.next /= null and then tmp.next.start_time < fin_play loop
         -- evenement suivant
         tmp := tmp.next;
      end loop;
      -- derniere note, peut être null
      last_note := tmp;
   end Get_notes;


   procedure next_lyrics( ref : in out events_ptr ) is
   begin
      -- si pas un texte, passe au suivant
      while ref /= null
      and then (ref.kind /= meta_lyrics
      and ref.kind /= meta_text)
      loop
         ref := ref.next;
      end loop;
   end next_lyrics;

   procedure Get_parole( temps : out integer;
                         texte : out Common_types.string_pt;
                         next : boolean ) is
   begin
      if Paroles_track = null then
         -- pas de parole pour ce fichier
         temps := integer'last;
         texte := null;
         return;
      end if;
      if cur_parole = null then
         -- paroles terminées
         temps := integer'last;
         texte := null;
         return;
      end if;
      -- cherche texte suivant
      if next then
         cur_parole := cur_parole.next;
      end if;
      next_lyrics( cur_parole );
      if cur_parole = null then
         -- paroles terminées
         temps := integer'last;
         texte := null;
         return;
      end if;
      -- il y a des paroles et ce n'est pas encore fini
      --
      -- temps en ticks
      temps := cur_parole.time;
      -- le texte
      texte := cur_parole.text;
   end Get_parole;



   function Get_Player_time return integer is
   begin
      return player_time;
   end Get_Player_time;


   -- ============================================================================================

   -- desallocation de la liste des events de la melodie
   procedure Remove_melodie is
      tmp : midi_event_ptr;
   begin
      while Melodie_track /= null loop
         tmp := Melodie_track.next;
         Free( Melodie_Track );
         Melodie_Track := tmp;
      end loop;
      melodie_event := null;
   end Remove_melodie;


   procedure Prepare_melodie is
     event : events_ptr;
     trk : midifile_pkg.track_data_ptr;
     note_pt, last_pt, tmp : midi_event_ptr;
   begin
      -- cleaning au cas ou ...
      if melodie_track /= null then
         Remove_melodie;
      end if;
      -- reset centrage auto
      max_melody := Common_types.min_midi;
      min_melody := Common_types.max_midi;
      -- reset volume min et max
      max_melody_vol := 0;
      min_melody_vol := 128;
      --
      -- recheche de la piste melodie
      trk := current_midi.tracks;
      while trk /= null and then trk.track_num /= melodie_num loop
         trk := trk.next;
      end loop;
      event := trk.events;
      --
      -- création de la liste
      while event /= null loop
         if event.event_type = channel_event then

            if event.kind = note_on then
               -- nouvelle note qui démarre
               note_pt := new midi_event_rec;
               note_pt.start_time := event.time;
               note_pt.note := short_Integer(event.note);
               -- insertion en queue de liste
               if last_pt = null then
                  -- le premier de la liste
                  Melodie_track := note_pt;
               else
                  -- chainage double
                  last_pt.next := note_pt;
                  note_pt.prev := last_pt;
                  -- verifie fin du précédent
                  if last_pt.end_time > note_pt.start_time then
                     -- toute note qui commence termine la précédente
                     last_pt.end_time := note_pt.start_time;
                  end if;
               end if;
               -- met à jour la queue de liste
               last_pt := note_pt;
               -- centrage auto: stocke min et max des notes de la mélodie
               if Integer(event.note) > max_melody then
                  max_melody := Integer(event.note);
               end if;
               if Integer(event.note) < min_melody then
                  min_melody := Integer(event.note);
               end if;
               -- mesure min et max du volume
               if Integer(event.param) > max_melody_vol then
                  max_melody_vol := Integer(event.param);
               end if;
               if Integer(event.param) < min_melody_vol then
                  min_melody_vol := Integer(event.param);
               end if;

            elsif event.kind = note_off then

               -- fin d'une note précédemment démarrée
               -- recherche en arrière de la note
               tmp := last_pt;
               while tmp /= null and then tmp.note /= short_Integer(event.note) loop
                  tmp := tmp.prev;
               end loop;
               if tmp /= null then
                  tmp.end_time := event.time;
                  -- test avec debut note suivante
                  if tmp.next /= null and then tmp.end_time > tmp.next.start_time then
                     tmp.end_time := tmp.next.start_time;
                  end if;
               end if;

            end if;

         elsif event.kind = meta_end_of_track then
            -- fin de piste, ajuste fin de la dernière note
            if last_pt /= null and then last_pt.end_time > event.time then
               last_pt.end_time := event.time;
            end if;
         end if;
         -- event MIDI suivant
         event := event.next;
      end loop;
      --
      -- Centrage du score sur les notes de la mélodies
      Affichage_pkg.Auto_centrage( min_melody, max_melody );
      --
      -- vérifie que toutes les notes ont une fin
      note_pt := Melodie_track;
      while note_pt /= null loop
         if note_pt.end_time = integer'last then
            if note_pt.next /= null then
               -- termine la note au démarrage de la suivante
               note_pt.end_time := note_pt.next.start_time;
            end if;
         end if;
         -- note suivante
         note_pt := note_pt.next;
      end loop;
      --
      -- initialise pointeur courant
      melodie_event := Melodie_track;
      --
      -- terminé
   end Prepare_melodie;

   -- --------------------------------------------------------------------------

   function Strip_blank( s : string ) return string is
      resu : string := s;
      i : natural;
   begin
      i := resu'first;
      while i <= resu'last and then Ada.Characters.Handling.Is_Control(resu(i)) loop
         i := i + 1;
      end loop;
      return resu(i..resu'last);
   end Strip_blank;

   function Uppercase( s: string ) return string is
      resu : string := s;
   begin
      for i in resu'range loop
         if Ada.Characters.Handling.Is_lower( resu(i) ) then
            resu(i) := Ada.Characters.Handling.To_Upper( resu(i) );
         end if;
      end loop;
      return resu;
   end Uppercase;

   procedure Prepare_paroles( midi_data : Midifile_pkg.midi_data_ptr) is
     event, next_event : events_ptr;
     trk : midifile_pkg.track_data_ptr;
     count : integer;
     b : byte;

     function Start_match( s : string; target : string ) return boolean is
        nom : string := Uppercase( Strip_blank(s) );
     begin
        if nom'length >= target'length then
           return nom(nom'first..nom'first+target'length-1) = target;
        else
           return false;
        end if;
     end Start_match;

   begin
      -- init
      Paroles_track := null;
      cur_parole := null;
      --
      -- Recherche la piste des paroles
      --
      -- recherche piste dont le nom est "lyrics" , "Words" ou "paroles"
      trk := midi_data.tracks;
      while trk /= null loop
         if trk.track_name /= null then
            if      Start_match( trk.track_name.all, "WORDS" )
            or else Start_match( trk.track_name.all, "PAROLES" )
            or else Start_match( trk.track_name.all, "LYRICS" )
            then
               Paroles_track := trk;
               exit;
            end if;
         end if;
         trk := trk.next;
      end loop;
      --
      -- si pas trouvé cherche piste contenant suffisament de texte
      if Paroles_track = null then
         trk := midi_data.tracks;
         while trk /= null loop
            -- recherche piste sans note
            while trk /= null and then trk.polyphonie /= 0 loop
               trk := trk.next;
            end loop;
            exit when trk = null;
            -- test si des texte ou lyrics en nombre suffisant
            event := trk.events;
            count := 0;
            while event /= null loop
               if event.kind = meta_text or event.kind = meta_lyrics then
                  count := count + 1;
               end if;
               event := event.next;
            end loop;
            if count > min_paroles then
               Paroles_track := trk;
               exit;
            end if;
            trk := trk.next;
         end loop;
      end if;
      --
      -- si pas trouvé, même chose avec une piste monophonique musique + paroles
      if Paroles_track = null then
         trk := midi_data.tracks;
         while trk /= null loop
            -- recherche piste monophonique
            while trk /= null and then trk.polyphonie /= 1 loop
               trk := trk.next;
            end loop;
            exit when trk = null;
            -- test si des texte ou lyrics en nombre suffisant
            event := trk.events;
            count := 0;
            while event /= null loop
               if event.kind = meta_text or event.kind = meta_lyrics then
                  count := count + 1;
               end if;
               event := event.next;
            end loop;
            if count > min_paroles then
               Paroles_track := trk;
               exit;
            end if;
            trk := trk.next;
         end loop;
      end if;
      --
      -- Cleaning des paroles
      --
      if Paroles_track /= null then
         -- parcours tous les events
         event := Paroles_track.events;
         while event /= null loop
            -- se souvenir du suivant au cas ou on doit supprimer l'event
            next_event := event.next;
            --
            if event.kind = meta_text or event.kind = meta_lyrics then
               -- event de texte
               if event.text'length >= 1 and then event.text(event.text'first) = '@' then
                  -- commande Karaoke
                  if event.text'length >=2 and then event.text(event.text'first+1) = 'T' then
                     -- info de titre, on garde mais on efface les codes de commandes
                     event.text(event.text'first) := ' ';
                     event.text(event.text'first+1) := ' ';
                  else
                     -- autres commandes : on supprime l'event
                     if event.prev /= null then
                       event.prev.next := next_event;
                     else
                        Paroles_track.events := next_event;
                     end if;
                     if next_event /= null then
                        next_event.prev := event.prev;
                     end if;
                  end if;
               else
                  -- pas une commande
                  for i in event.text'range loop
                     case event.text(i) is
                        when '\' | '/' | '0'..'9' =>
                           -- cleaning: supprime le char
                           event.text(i) := ' ';
                        when others =>
                           b := Conversions.To_byte(event.text(i));
                           if b > 128 then
                              -- conversion des caractères accentués de l'ASCII IBM en ASCII ANSI
                              case b is
                                 when 16#81# => event.text(i) := 'ü';
                                 when 16#82# => event.text(i) := 'é';
                                 when 16#83# => event.text(i) := 'â';
                                 when 16#84# => event.text(i) := 'ä';
                                 when 16#85# => event.text(i) := 'à';
                                 when 16#87# => event.text(i) := 'ç';
                                 when 16#88# => event.text(i) := 'ê';
                                 when 16#89# => event.text(i) := 'ë';
                                 when 16#8A# => event.text(i) := 'è';
                                 when 16#8B# => event.text(i) := 'ï';
                                 when 16#93# => event.text(i) := 'ô';
                                 when 16#94# => event.text(i) := 'ö';
                                 when 16#96# => event.text(i) := 'û';
                                 when 16#97# => event.text(i) := 'ù';
                                 when others => null;
                              end case;
                           end if;
                     end case;
                  end loop;
               end if;
            end if;
            event := next_event;
         end loop;
         -- initialise pointeur pour les paroles
         cur_parole := Paroles_track.events;
         next_lyrics(cur_parole);
      end if;
   end Prepare_paroles;

   -- ================================================================================

   procedure Remove_mesure is
      tmp : midi_event_ptr;
   begin
      while mesure_track /= null loop
         tmp := mesure_track.next;
         Free( mesure_Track );
         mesure_Track := tmp;
      end loop;
      mesure_event := null;
   end Remove_mesure;


   procedure Prepare_mesure is
      temps_mesure, ticks_mesure : integer;
      numero_mesure : short_integer;
      temps_courant: integer;
      tmp, note : midi_event_ptr;
      tempo_courant, signature_courant : events_ptr;
      ecart : integer;
      start_melodie : integer;
      --
      tempo : integer := 500_000;	-- defaut = 120 BPM
      num   : integer := 4;		-- defaut = 4/4
   begin
      -- cleaning au cas ou
      if mesure_track /= null then
         Remove_mesure;
      end if;
      -- initialise le tempo
      tempo_courant := current_midi.tempos;
      if tempo_courant /= null and then tempo_courant.time = 0 then
         tempo := tempo_courant.tempo;
      end if;
      -- initialise la signature
      signature_courant := current_midi.signatures;
      if signature_courant /= null and then signature_courant.time = 0 then
         num := integer(signature_courant.numerator);
      end if;
      -- durée mesure initiale en ms
      ticks_mesure := current_midi.ticks_per_beats * num;
      temps_mesure := tempo / 1_000 * ticks_mesure / current_midi.ticks_per_beats ;
      -- creation event de la première mesure
      mesure_track := new midi_event_rec'( 0, temps_mesure, 1, null, null );
      tmp := mesure_track;
      temps_courant := temps_mesure;
      numero_mesure := 2;
      -- boucle création des autres mesures
      while temps_courant < end_time loop
         -- on reste à jour dans les signatures
         while signature_courant /= null and then signature_courant.next /= null
         and then signature_courant.next.time < temps_courant loop
            signature_courant := signature_courant.next;
            -- si il y a une signature, on prend sa valeur, sinon on garde l'ancien
            if signature_courant /= null then
               num := integer(signature_courant.numerator);
               -- mise à jour durée de la mesure
               ticks_mesure := current_midi.ticks_per_beats * num;
               temps_mesure := tempo / 1_000 * ticks_mesure / current_midi.ticks_per_beats ;
            end if;
         end loop;
         -- on reste à jour dans les tempos
         while tempo_courant /= null and then tempo_courant.next /= null
         and then tempo_courant.next.time < temps_courant loop
            tempo_courant := tempo_courant.next;
            -- si il y a un tempo, on prend sa valeur, sinon on garde l'ancien
            if tempo_courant /= null then
               tempo := tempo_courant.tempo;
               -- mise à jour durée de la mesure
               temps_mesure := tempo / 1_000 * ticks_mesure / current_midi.ticks_per_beats ;
            end if;
         end loop;
         -- création d'un nouvelle mesure
         tmp.next := new midi_event_rec'( temps_courant, temps_courant+temps_mesure, numero_mesure, null, tmp );
         tmp.next.prev := tmp;
         tmp := tmp.next;
         -- incremente le temps: on passe à la fin de la mesure
         temps_courant := temps_courant + temps_mesure;
         -- incrémente numéro de mesure
         numero_mesure := numero_mesure + 1;
      end loop;
      --
      -- recalage des mesures pour commencer avec la mélodie
      --
      start_melodie := melodie_track.start_time;
      -- recherche mesure corespondante
      tmp := mesure_track;
      while tmp /= null and then tmp.end_time <= start_melodie loop
         tmp := tmp.next;
      end loop;
      if tmp /= null then
         -- calcule différence de temps entre le debut de la mesure et la premiere note de la melodie
         ecart := start_melodie - tmp.start_time;
         if ecart > 0 then
            -- propage le décalage à toutes les mesures
            tmp := mesure_track;
            while tmp /= null loop
               tmp.start_time := tmp.start_time + ecart;
               tmp.end_time   := tmp.end_time   + ecart;
               -- la derniere mesure ne doit pas dépasser la fin du morceau
               if tmp.next = null then
                  if tmp.end_time > end_time then
                     tmp.end_time := end_time;
                  end if;
               end if;
               tmp := tmp.next;
            end loop;
         end if;
      end if;
      --
      -- alignement avec début de notes
      tmp := mesure_track;
      note := melodie_track;
      while tmp /= null loop
         -- recherche premiere note dont la fin est dans la mesure
         while note /= null and then note.end_time < tmp.start_time loop
            note := note.next;
         end loop;
         if note /= null then
            -- si l'écart est plus petit qu'un delai maximum s'aligner sur le début ou sur la fin
            -- sinon s'aligner sur la fin de la précédente si pas trop loin
            if abs(note.start_time - tmp.start_time) < max_align_mesure then
               tmp.start_time := note.start_time;
            elsif abs(note.end_time - tmp.start_time) < max_align_mesure then
               tmp.start_time := note.end_time;
            elsif note.prev /= null and then abs(note.prev.end_time - tmp.start_time) < max_align_mesure then
               tmp.start_time := note.prev.end_time;
            end if;
         end if;
         tmp := tmp.next;
      end loop;
      --
      -- initialise pointeur courant sur la première mesure
      mesure_event := mesure_track;
   end Prepare_mesure;

   -- ===============================================================================

   procedure Calcule_semblance( midi_data : Midifile_pkg.midi_data_ptr ) is
      trk : track_data_ptr;
      ref_paroles : events_ptr;
      event : events_ptr;
      count, total : natural;

   begin
      trk := midi_data.tracks;
      while trk /= null loop
         if trk.polyphonie = 1 then
            --
            count := 0;
            total := 0;
            -- pour chaque parole compte si une note lui correspond
            event := trk.events;
            ref_paroles := cur_parole;
            while ref_paroles /= null loop
               -- ne prend en compte que les texte non-vide après la 1ere seconde: saute les titres
               if ref_paroles.text'length > 0 and then ref_paroles.time > 1000 then
                  total := total + 1;		-- nombre total de paroles
                  -- pour chaque nouvelle parole, regarde si cela correspond à une note de cette piste
                  while event /= null and then event.kind /= note_on
                  and then event.time < ref_paroles.time - parole_tolerance loop
                     event := event.next;
                  end loop;
                  if event/= null and then event.kind = note_on
                  and then event.time < ref_paroles.time + parole_tolerance then
                     count := count + 1;	-- match
                  end if;
               end if;
               -- parole suivante
               ref_paroles := ref_paroles.next;
               next_lyrics( ref_paroles );
            end loop;
            -- pour chaque note, compte si une parole lui correspond
            event := trk.events;
            ref_paroles := cur_parole;
            while event /= null loop
               if event.kind = note_on then
                  total := total + 1;
                  -- pour chaque nouvelle note, regarde si cela correspond à une parole
                  next_lyrics( ref_paroles );
                  while ref_paroles /= null
                  and then ref_paroles.time < event.time - parole_tolerance loop
                     ref_paroles := ref_paroles.next;
                     next_lyrics( ref_paroles );
                  end loop;
                  if ref_paroles /= null and then ref_paroles.time < event.time + parole_tolerance then
                     count := count + 1;
                  end if;
               end if;
               -- evenement suivant
               event := event.next;
            end loop;
            -- calcule
            if total > 0 then
               trk.semblance := (count * 100) / total;
            end if;
         end if;
         -- next track
         trk := trk.next;
      end loop;
   end Calcule_semblance;

   -- ===============================================================================

   procedure Calcule_accomp_vol is
      trk : track_data_ptr;
      event : events_ptr;
   begin
      -- init
      max_accomp_vol := 0;
      min_accomp_vol := 128;
      --
      trk := current_midi.tracks;
      -- pour toutes les pistes /= melodie
      while trk /= null loop
         if trk.track_num /= melodie_num then
            -- parcours des events
            event := trk.events;
            while event /= null loop
               if event.kind = note_on and then event.param > 0 then
                  -- mesure min et max du volume
                  if Integer(event.param) > max_accomp_vol then
                     max_accomp_vol := Integer(event.param);
                  end if;
                  if Integer(event.param) < min_accomp_vol then
                     min_accomp_vol := Integer(event.param);
                  end if;
               end if;
               --
               event := event.next;
            end loop;
         end if;
         --
         trk := trk.next;
      end loop;
   end Calcule_accomp_vol;

   -- ===============================================================================

   procedure Set_File_Name( name : string ) is
      i : integer;
   begin
     -- desalloue ancien nom
     Free( current_file_name );
     -- recherche 1er séparateur
     i := name'last;
     while i >= name'first and then (name(i) /= '\' and name(i) /= '/') loop
        i := i - 1;
     end loop;
     current_file_name := new string'( name(i+1..name'last) );
   end Set_File_Name;


   function File_name return string is
   begin
      if current_file_name = null then
         return "";
      else
         return current_file_name.all;
      end if;
   end File_name;


   -- chargement d'un fichier MIDi et préparation des structures pour jouer le fichier
   -- et afficher la mélodie
   function Load_file return boolean is
      new_midi : Midifile_pkg.midi_data_ptr;
      trk : track_data_ptr;
      count : natural;
   begin
      --
      if Common_types.no_midi_device then
         Utils_pkg.Error_box( Intl.Err_midi_title, Intl.err_midi_dev_txt );
         return false;
      end if;
      --
      if status /= stopped then
         Stop;
      end if;
      --
      declare
         file_name : string := Dialog_pkg.Select_Midi_file;      -- dialog : sélection fichier
      begin
         if file_name = "" then
            return false;
         end if;
         -- ouverture fichier et décodage
         new_midi := Midifile_pkg.read_file( file_name );
         --
         -- arrivé ici, le fichier a été correctement chargé
         --
         -- analyse du fichier: polyphonie de chaque piste
         Analyse_midi( new_midi );
         -- calcul temps absolu de chaque evenement
         Ajuste_temps( new_midi );
         -- Recherche de la piste paroles s'il y en a une
         Prepare_paroles( new_midi );
         --
         -- sélection de la piste mélodie
         trk := new_midi.tracks;
         count := 0;
         while trk /= null loop
            if trk.polyphonie = 1 then
               count := count + 1;
               if count = 1 then
                  -- une piste trouvée, si c'est la seule c'est celle qui sera utilisée
                  melodie_num := trk.track_num;
               end if;
            end if;
            -- piste suivante
            trk := trk.next;
         end loop;
         -- si aucune piste monophonique: inutilisable
         if count = 0 then
            -- fichier inutilisable
            Utils_pkg.Error_box( Intl.def_err_title, Intl.err_chant_txt );
            -- abandon, cleaning
            Midifile_pkg.Remove( new_midi );
            return false;
         end if;
         --
         if count > 1 then
            -- calcule la correspondance entre les paroles et les note_on
            if Paroles_track /= null then
               Calcule_semblance( new_midi );
            end if;
            -- plusieurs choix possibles, demander à l'utilisateur
            melodie_num := Dialog_pkg.Select_track( new_midi );
            -- test si choisi ou non
            if melodie_num = -1 then
               -- abandon, cleaning
               Midifile_pkg.Remove( new_midi );
               return false;
            end if;
         end if;
         --
         -- si ancien fichier chargé, nettoyage
         if current_midi /= null then
            Midifile_pkg.Remove( current_midi );
            Remove_melodie;
            Remove_mesure;
            Data_pkg.Clear_mesure;
            -- on ferme le device midi
            Close_midi_device;
            -- on ré-ouvre le device MIDI
            Open_Midi_device;
         end if;
         --
         -- le nouveau devient le courant
         current_midi := new_midi;
         -- on initialise les pointeurs sur les evenements
         raz_current_midi;
         -- Prépare la piste mélodie et mesures
         Prepare_melodie;
         Prepare_mesure;
         -- Calcule min et max volume de l'accompagnement
         Calcule_accomp_vol;
         -- Recalcule les tables des volumes
         Set_melody_volume( melody_volume );
         Set_accomp_volume( accomp_volume );
         -- flag prêt à jouer
         file_loaded := true;
         -- mémorise nom du fichier
         Set_File_name( file_name );
         --
         return true;
         --
      exception
         when Midifile_pkg.not_midi_file | Midifile_pkg.invalid_midi | Constraint_error =>
            -- affichage message utilisateur
            Utils_pkg.Error_box( Intl.def_err_title, Intl.err_midi_txt & file_name );
            -- flag
            file_loaded := false;
            return false;
         when others =>
            Utils_pkg.Error_box( Intl.def_err_title, Intl.err_fnf_txt & file_name );
            -- flag
            file_loaded := false;
            return false;
      end;
   end Load_File;


   -- *****************************************************************************************


   procedure Play_event( evt : midifile_pkg.events_ptr; is_melody : boolean := true ) is
      result  : Win32.Mmsystem.MMRESULT;
      message : Win32.DWORD := 0;
      buf : array(0..3) of Win32.BYTE;
      for buf use at message'address;
   begin
      -- status = code * 16 + channel
      buf(0) := midifile_pkg.get_status( evt.kind ) * 16 + evt.channel;
      -- paramètres
      case evt.kind is

         when note_on =>
            if evt.channel = general_midi.drum_channel then
               buf(1) := evt.note;
            else
               buf(1) := byte( Integer(evt.note) + transposition );
            end if;
            --
            if is_melody then
               buf(2) := tab_vol_melody(Integer(evt.param));
            else
               buf(2) := tab_vol_accomp(Integer(evt.param));
            end if;

         when note_off | note_aftertouch =>
            if evt.channel = general_midi.drum_channel then
               buf(1) := evt.note;
            else
               buf(1) := byte( Integer(evt.note) + transposition );
            end if;
            --
            buf(2) := evt.param;

         when controller =>
            buf(1) := evt.controller_nbr;
            buf(2) := evt.controller_value;

         when program_change =>
            buf(1) := evt.program_number;

         when channel_aftertouch =>
            buf(1) := evt.aftertouch_value;

         when pitch_bend =>
            buf(1) := BYTE(evt.pitch_value mod 256);
            buf(2) := BYTE(evt.pitch_value / 256);

         when others =>
            -- rien à faire
            return;
      end case;
      -- envoie du message au device MIDI
      result := Win32.Mmsystem.midiOutShortMsg( midi_handler, message );
   exception
      -- si pb, ne rien faire et continuer
      when others => null;
   end Play_event;


   procedure All_off is
      result  : Win32.Mmsystem.MMRESULT;
      message : Win32.DWORD := 0;
      buf : array(0..3) of Win32.BYTE;
      for buf use at message'address;
   begin
      -- pour chaque channel
      for chan in 0..15 loop
         buf(0) := 16#B0# + Byte(chan);	-- controller + channel
         buf(1) := 120;			-- All sound off
         result := Win32.Mmsystem.midiOutShortMsg( midi_handler, message );
      end loop;
   end All_off;

   -- ****************************************************************************************

   -- positionnne tous les pointeur pour correspondre au temps player passé en paramètre
   procedure Seek_player( time : integer ) is
      old_status : player_status_type;
      trk : midifile_pkg.track_data_ptr;
      resu_long: Win32.LRESULT;
   begin
      old_status := status;
      -- pause pour éviter de jouer d'autres notes quand le seek est appelé pendant que la player joue
      status := paused;
      --
      if old_status = playing then
         -- arrêt des notes en cours
         All_off;
      end if;
      --
      -- tronque toutes données après ce temps
      Data_pkg.Stop_all( Process_pkg.Card_clock );
      --
      -- reset des pointeurs de toutes les pistes
      trk := current_midi.tracks;
      while trk /= null loop
         trk.cur_event := trk.events;
         while trk.cur_event /= null and then trk.cur_event.time < time loop
            trk.cur_event := trk.cur_event.next;
         end loop;
         -- piste suivantes
         trk := trk.next;
      end loop;
      --
      -- idem pour mélodie
      melodie_event := melodie_track;
      while melodie_event /= null and then melodie_event.start_time < time loop
         melodie_event := melodie_event.next;
      end loop;
      --
      -- idem pour les paroles
      if paroles_track /= null then
         cur_parole := paroles_track.events;
         while cur_parole /= null
         and then cur_parole.next /= null
         and then cur_parole.next.time < time loop
            cur_parole := cur_parole.next;
         end loop;
      end if;
      --
      -- idem pour les mesures
      if mesure_event /= null and then mesure_event.start_time /= time then
         mesure_event := mesure_track;
         while mesure_event /= null and then mesure_event.start_time < time loop
            mesure_event := mesure_event.next;
         end loop;
      end if;
      --
      -- décalage du temps du player
      start_time_offset := Process_pkg.Card_clock - time - 1;
      --
      -- maj status
      status := playing;
      --
      -- notifie le UI pour mise à jour de l'affichage
      resu_long := Win32.Winuser.SendMessage( Common_types.Win_hwnd,
                           Common_types.PLAYER_RESET,		-- code notification
                           0, 0 );				-- non utilisé
      --
   end Seek_player;


   procedure Seek_to_mesure( num_mesure : natural ) is
   begin
      mesure_event := mesure_track;
      while mesure_event /= null and then natural(mesure_event.note) < num_mesure loop
         mesure_event := mesure_event.next;
      end loop;
      Seek_player( mesure_event.start_time );
   end Seek_to_mesure;


   -- ************************************************************************************

   -- LE PLAYER

   procedure Do_Next_tick is
      trk : midifile_pkg.track_data_ptr;
      count : natural := 0;
      resu_long : Win32.LRESULT;
      now : integer;
   begin
      if status /= playing then
         return;
      end if;
      -- temps absolue données par la carte son
      now := Process_pkg.Card_clock;
      --
      -- temps interne du player: millisecondes depuis le Start
      player_time := now - start_time_offset;
      --
      -- parcours des mesures
      if mesure_event /= null and then player_time >= mesure_event.start_time then
         -- teste si mesure de fin dépassée (car elle doit être jouée!)
         if Natural(mesure_event.note) > mesure_fin then
            -- en mode boucle recommence à la mesure de début
            if mode_loop then
               Seek_to_mesure( mesure_debut );
            else
               -- arrêt du player
               Stop;
               -- notification de l'UI pour mise à jour de l'affichage
               resu_long := Win32.Winuser.SendMessage( Common_types.Win_hwnd,
                           Common_types.BUTTON_CLICKED,		-- code notification
                           Win32.WPARAM(Resources_pkg.obj_Id_type'pos(Resources_pkg.STOP_ID)), 	-- Id du bouton
                           0 );				-- non utilisé
            end if;
            -- on arrête là pour ce cycle
            return;
         end if;
         -- debut d'une nouvelle mesure
         Data_pkg.Store_mesure( start_time  => now,
                                end_time    => now-mesure_event.start_time+mesure_event.end_time,
                                player_time => mesure_event.start_time,
                                numero 	    => mesure_event.note );
         -- passe à la suivante
         mesure_event := mesure_event.next;
      end if;
      --
      -- parcours des pistes
      trk := current_midi.tracks;
      while trk /= null loop

         if trk.polyphonie > 0 then	-- si pas de note sauter la piste

         -- si mode une seule piste ne jouer que celle-là
         if track_to_play < 0 or else track_to_play = trk.track_num then

            while trk.cur_event /= null loop
               -- compte les pistes non-terminée
               count := count + 1;
               -- on 'joue' l'evenement lorsque c'est le moment
               if trk.cur_event.event_type = channel_event then
                  if trk.cur_event.time <= player_time then
                     -- on le joue sauf program_change pour la mélodie en mode instrument changé
                     if not Common_types.Change_instrument
                        or else trk.track_num /= melodie_num
                        or else trk.cur_event.kind /= program_change
                     then
                        Play_event( trk.cur_event, trk.track_num = melodie_num );
                     end if;
                     -- affichage de la mélodie
                     if trk.track_num = melodie_num then
                        if trk.cur_event.kind = note_on then
                           -- stockage des notes effectivement jouées
                           Data_pkg.Store_Midi( now,
                                                now + melodie_event.end_time - melodie_event.start_time,
                                                melodie_event.note * 100 );
                           -- incrémente pointeur
                           melodie_event := melodie_event.next;
                        end if;
                     end if;
                  else
                     -- ne pas jouer d'autre pour cette piste
                     exit;
                  end if;
               end if;
               -- on passe au suivant
               trk.cur_event := trk.cur_event.next;
            end loop;
         end if;
         end if;
         -- piste suivante
         trk := trk.next;
      end loop;
      -- si tout est terminé, arrêt du player par envoie d'un message STOP à la fenetre principale
      if count = 0 then
         if mode_loop then
            Seek_to_mesure( mesure_debut );
         else
            resu_long := Win32.Winuser.SendMessage( Common_types.Win_hwnd,
                              Common_types.BUTTON_CLICKED,		-- code notification
                              Win32.WPARAM(Resources_pkg.obj_Id_type'pos(Resources_pkg.STOP_ID)), 	-- Id du bouton
                              0 );				-- non utilisé
         end if;
      end if;
   end Do_Next_tick;


   -- **************************************************************************

   function Get_melody_channel return byte is
      trk : track_data_ptr;
      evt : events_ptr;
   begin
      if current_midi = null then
         return 0;
      end if;
      -- recherche de la piste mélodie
      trk := current_midi.tracks;
      while trk /= null   loop
         if trk.track_num = melodie_num then
            -- recherche du channel
            evt := trk.events;
            while evt /= null and then evt.event_type /= channel_event loop
               evt := evt.next;
            end loop;
            return evt.channel;	-- ne peut pas être nul, sinon pas de mélodie possible
         end if;
         trk := trk.next;
      end loop;
      return 0;	-- par défaut pour le compilateur
   end Get_melody_channel;


   procedure Change_instrument is
   begin
      -- channel de la mélodie
      change_inst_evt.channel := Get_melody_channel;
      -- instrument
      change_inst_evt.program_number := Common_types.Instrument_number;
      -- envoie la commande au device MIDI
      Play_event( change_inst_evt );
   end Change_instrument;


   procedure Play( track_num : integer ) is
   begin
      -- test si quelque chose à jouer
      if not file_loaded or status /= stopped then
         return;
      end if;
      -- joue la piste demandée ou toutes si -1
      track_to_play := track_num;
      --
      if Common_types.Change_instrument then
         -- envoie un program change pour la piste de la mélodie
         Change_instrument;
      end if;
      --
      if mesure_debut > 1 then
         Seek_to_mesure( mesure_debut );
      else
         start_time_offset := Process_pkg.Card_Clock;
         --
         status := playing;
      end if;
      --
   end Play;


   procedure Toggle_Pause is
   begin
      -- si nécessaire arrête les notes
      case status is
         when playing =>
            All_off;
            -- mémorise temps de début de la pause
            start_pause := Process_pkg.Card_clock;
            status := paused;
         when paused =>
            -- fin de pause, met à jour le décalage de temps pour le player
            start_time_offset := start_time_offset + Process_pkg.Card_clock - start_pause;
            status := playing;
         when stopped =>
            null;
      end case;
   end Toggle_Pause;


   procedure Stop is
   begin
      -- test quelque chose à faire
      if status = stopped then
         return;
      end if;
      -- arrête le player
      status := stopped;
      -- on arrête toutes les notes en cours
      All_off;
      -- RAZ des pointeurs
      raz_current_midi;
   end Stop;


   function Is_Playing return Boolean is
   begin
      return status = playing;
   end Is_Playing;


   procedure Set_transpo( valeur : integer ) is
   begin
      transposition := valeur;
   end Set_transpo;

   function Get_player_status return player_status_type is
   begin
      return status;
   end Get_player_status;

   -- **************************************************************************************

   -- on ferme le device midi
   procedure Close_midi_device is
      result : Win32.Mmsystem.MMRESULT;
   begin
      result := Win32.Mmsystem.midiOutClose( midi_handler );
   end Close_midi_device;

   procedure Open_Midi_device is
      result : Win32.Mmsystem.MMRESULT;
   begin
      -- ouverture device Midi, sans callback
      result := Win32.Mmsystem.midiOutOpen( TO_LPHMIDIOUT( midi_handler'address ), Common_types.midi_num, 0, 0, 0 );
      Utils_pkg.Check_erreur( result, "Open_Midi_device: midiOutOpen" );
   end Open_Midi_device;


   -- =======================================================================================

   procedure Play_user_note( note : integer ) is
      channel : byte;
   begin
      if Common_types.no_midi_device then
         return;
      end if;
      if note < 1 or note > 128 then
         return;
      end if;
      --
      if user_playing then
         Stop_user_note;
      end if;
      -- raz channel
      user_evt_on.channel := 0;
      user_evt_off.channel := 0;
      -- si un fichier MIDI est chargé, utilisé le channel de la mélodie
      if current_midi /= null then
         channel := Get_melody_channel;
         -- on l'assigne aux event ON et OFF
         user_evt_on.channel  := channel;
         user_evt_off.channel := channel;
      end if;
      -- note pour ON et OFF
      user_evt_on.note := byte(note);
      user_evt_off.note := byte(note);
      --
      Play_event( user_evt_on );
      user_playing := true;
      --
   end Play_user_note;


   procedure Stop_user_note is
   begin
      if Common_types.no_midi_device then
         return;
      end if;
      if user_playing then
         -- arrêt de la note si on était en train d'en jouer une
         Play_event( user_evt_off );	-- évènement note_off
      end if;
      --
      user_playing := false;
   end Stop_user_note;

   -- *****************************************************************************************

   -- entre ou sort du mode lecture en boucle
   procedure Set_mode_loop( do_loop : boolean ) is
   begin
      mode_loop := do_loop;
   end Set_mode_loop;

   -- mesure de départ
   procedure Set_Start_mesure( num_mesure : natural ) is
   begin
      mesure_debut := num_mesure;
   end Set_Start_mesure;


   -- mesure de fin
   procedure Set_End_mesure( num_mesure : natural ) is
   begin
      mesure_fin := num_mesure;
   end Set_End_mesure;

   -- numero de la derniere mesure du fichier midi en cours
   function Get_last_mesure return natural is
      tmp : midi_event_ptr := mesure_track;
   begin
      while tmp /= null and then tmp.next /= null loop
         tmp := tmp.next;
      end loop;
      if tmp = null then
         return 0;
      else
         return natural(tmp.note);
      end if;
   end Get_last_mesure;

   -- --------------------------------------------------------------------------

   procedure Set_volume_table( volume : natural; table : in out volume_array ) is
      N : natural;
   begin
      if volume <= 50 then
         for i in 0..127 loop
            table(i) := byte(i * volume / 50);
         end loop;
      elsif  volume >= 100 then
         for i in 1..127 loop
            table(i) := 127;
         end loop;
         table(0) := 0;
      else
         N := (127 * (100-volume))/100;
         for i in 0..N loop
            table(i) := byte( i * 127/N);
         end loop;
         for i in N+1..127 loop
            table(i) := 127;
         end loop;
      end if;
   end Set_volume_table;

   procedure Set_Melody_volume( value : natural ) is
   begin
      melody_volume := value;
      Set_volume_table( melody_volume, tab_vol_melody );
   end Set_Melody_volume;

   procedure Set_Accomp_volume( value : natural ) is
   begin
      accomp_volume := value;
      Set_volume_table( accomp_volume, tab_vol_accomp );
   end Set_Accomp_volume;

end Midi_pkg;

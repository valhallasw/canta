with Win32;		use Win32;
with Stat_pkg;
with Log;
with Config_pkg;
with Debug_pkg;

package body data_pkg is

   debug_flag : constant string := "data";

   -- notes chantées
   -- anneau; prem_list est le premier et dern_list le dernier dans l'ordre chronologique
   -- le parcours chronologique se fait par les pointeurs 'next'
   -- prem_list -> .. -> next -> .. -> dern_list
   -- dern_list ne fait pas parti de la liste, c'est le prochain à être utilisé
   -- prem_list fait parti de la liste
   prem_list, dern_list  : note_event_ptr;
   max_free_note : constant := 480_000;	-- 480_000 x 7.5 ms = 1 H


   -- idem pour les notes MIDI
   -- notes MIDi jouées
   prem_midi, dern_midi : midi_event_ptr;
   max_free_midi : constant := 10_000;

   -- idem pour les mesures
   -- notes MIDi jouées
   prem_mesure, dern_mesure : mesure_event_ptr;
   max_free_mesure : constant := 1_000;

   -- ************************************************************************

   function new_note return note_event_ptr is
      tmp : note_event_ptr;
   begin
      -- le prochain est pointé par dern_list
      tmp := dern_list;
      dern_list := dern_list.next;
      -- si on a fait le tour on libere le premier de la liste
      if prem_list = dern_list then
         prem_list := prem_list.next;
      end if;
      return tmp;
   end New_note;


   procedure Store_Note( time  : integer;
                         note  : short_integer;
                         ampli : short_integer) is
      tmp : note_event_ptr;
      median : short_integer;
   begin
      -- calcule la médianne
      median := Stat_pkg.Add_median( note );
      -- nouveau record
      tmp := new_note;
      -- copie les valeurs
      tmp.time  :=  time;
      tmp.note  := median;
      tmp.ampli := ampli;
      -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      if Debug_pkg.Is_set( debug_flag ) then
         Log.Store(time);Log.Store(Integer(note));Log.Store(Integer(ampli));
         Log.End_line;
      end if;
      -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   end Store_Note;


   function Get_Max_Time return integer is
   begin
      -- dern_list ne fait pas partie de la liste, le dernier élément est pointé par der_list.prev
      return dern_list.prev.time;
   end Get_Max_Time;

   function Get_Min_Time return integer is
   begin
      -- premier event pointé par prem_list
       return prem_list.time;
   end Get_Min_Time;

   -- ==================================================================================


   function new_midi return midi_event_ptr is
      tmp : midi_event_ptr;
   begin
      -- le prochain est pointé par dern_midi
      tmp := dern_midi;
      dern_midi := dern_midi.next;
      -- si on a fait le tour on libere le premier de la liste
      if prem_midi = dern_midi then
         prem_midi := prem_midi.next;
      end if;
      return tmp;
   end New_midi;


   procedure Store_Midi( start_time,
                         end_time : integer;
                         note : short_integer ) is
      tmp : midi_event_ptr;
   begin
      tmp := New_midi;
      tmp.start_time := start_time;
      tmp.end_time := end_time;
      tmp.note := note;
   end Store_Midi;

   procedure Get_Midi( start_time,
                       end_time   : integer;
                       first_event,
                       last_event : out midi_event_ptr ) is
      first, last : midi_event_ptr;
   begin
      first_event := null;
      last_event := null;
      --
      -- cas trivial: liste vide
      if dern_midi = prem_midi then
         return;		-- -----------------------------> exit
      end if;
      --
      -- recherche le premier event
      first := dern_midi.prev;
      while first /= prem_midi and then first.end_time > start_time loop
         first := first.prev;
      end loop;
      -- test si rien à afficher
      if first = prem_midi and first.start_time > end_time then
         return;		-- -----------------------------> exit
      end if;
      --
      -- recherche le dernier
      last := dern_midi.prev;
      while last /= first and then last.start_time > end_time loop
         last := last.prev;
      end loop;
      --
      -- resultats
      first_event := first;
      last_event := last;
   end Get_Midi;

   -- ========================================================================

   function New_mesure return mesure_event_ptr is
      tmp : mesure_event_ptr;
   begin
      -- le prochain est pointé par dern_mesure
      tmp := dern_mesure;
      dern_mesure := dern_mesure.next;
      -- si on a fait le tour on libere le premier de la liste
      if prem_mesure = dern_mesure then
         prem_mesure := prem_mesure.next;
      end if;
      return tmp;
   end New_mesure;


   procedure Store_Mesure( start_time,
                           end_time    : integer;
                           player_time : integer;
                           numero      : short_integer ) is
      tmp : mesure_event_ptr;
   begin
      tmp := New_mesure;
      tmp.start_time  := start_time;
      tmp.end_time    := end_time;
      tmp.player_time := player_time;
      tmp.note        := numero;
   end Store_Mesure;


   procedure Get_Mesure( start_time,
                         end_time   : integer;
                         first_mesure,
                         last_mesure : out mesure_event_ptr ) is
      first, last : mesure_event_ptr;
   begin
      first_mesure := null;
      last_mesure := null;
      --
      -- cas trivial: liste vide
      if dern_mesure = prem_mesure then
         return;		-- -----------------------------> exit
      end if;
      --
      -- recherche le premier event
      first := dern_mesure.prev;
      while first /= prem_mesure and then first.end_time > start_time loop
         first := first.prev;
      end loop;
      -- test si rien à afficher
      if first = prem_mesure and first.start_time > end_time then
         return;		-- -----------------------------> exit
      end if;
      --
      -- recherche le dernier
      last := dern_mesure.prev;
      while last /= first and then last.start_time > end_time loop
         last := last.prev;
      end loop;
      --
      -- resultats
      first_mesure := first;
      last_mesure := last;
   end Get_Mesure;


   function Get_Mesure( time : integer ) return mesure_event_ptr is
      tmp : mesure_event_ptr;
   begin
      tmp := dern_mesure.prev;
      loop
         -- si le temps est dans l'intervalle de la mesure, c'est la bonne
         if tmp.start_time <= time and tmp.end_time >= time then
            return tmp;
         end if;
         -- test fin de données
         exit when tmp = prem_mesure;
         -- suivant
         tmp := tmp.prev;
      end loop;
      -- pas trouvé
      return null;
   end Get_Mesure;


   -- ==================================================================================


   procedure Get_Note( start_time,
                       end_time   : integer;
                       first_event,
                       last_event : out note_event_ptr ) is
      first, last : note_event_ptr;
   begin
      first_event := null;
      last_event := null;
      --
      -- cas trivial: liste vide
      if dern_list = prem_list then
         return; 	-- --------------------------------------> exit
      end if;
      --
      -- recherche le premier event
      first := dern_list.prev; -- dernier event enregistré
      while first /= prem_list and then first.time > start_time loop
         first := first.prev;
      end loop;
      -- test si rien à afficher
      if first = prem_list and first.time > end_time then
         return;	-- --------------------------------------> exit
      end if;
      --
      -- recherche le dernier
      last := dern_list.prev;
      while last /= first and then last.time > end_time loop
         last := last.prev;
      end loop;
      -- ajoute dernier event en dehors si continuation de ligne
      if last /= dern_list.prev and then last.note /= 0 then
         last := last.next;
      end if;
      --
      -- resultats
      first_event := first;
      last_event  := last;
   end Get_Note;



  -- ======================================================================================

   -- préallocation de mémoire
   procedure Init is
      tmp_n   : note_event_ptr;
      tmp_m   : midi_event_ptr;
      tmp_mes : mesure_event_ptr;
   begin
      --
      -- rempli la note list
      dern_list := new note_event_rec'( 0, 0, 0, next => null, prev => null );
      prem_list := dern_list;
      tmp_n := dern_list;
      for i in 2..max_free_note loop
         tmp_n.next := new note_event_rec'( 0, 0, 0, next => null, prev => tmp_n );
         tmp_n := tmp_n.next;
      end loop;
      -- bouclage
      dern_list.prev := tmp_n;
      tmp_n.next := dern_list;
      --
      -- rempli la midi list
      dern_midi := new midi_event_rec'( 0, 0, 0, next => null, prev => null );
      prem_midi := dern_midi;
      tmp_m := dern_midi;
      for i in 2..max_free_midi loop
         tmp_m.next := new midi_event_rec'( 0, 0, 0, next => null, prev => tmp_m );
         tmp_m := tmp_m.next;
      end loop;
      -- bouclage
      dern_midi.prev := tmp_m;
      tmp_m.next := dern_midi;
      --
      -- rempli la mesure list
      dern_mesure := new mesure_event_rec'( 0, 0, 0, 0, next => null, prev => null );
      prem_mesure := dern_mesure;
      tmp_mes := dern_mesure;
      for i in 2..max_free_mesure loop
         tmp_mes.next := new mesure_event_rec'( 0, 0, 0, 0, next => null, prev => tmp_mes );
         tmp_mes := tmp_mes.next;
      end loop;
      -- bouclage
      dern_mesure.prev := tmp_mes;
      tmp_mes.next := dern_mesure;
      --
   end Init;

   procedure Clear_mesure is
      tmp_mes : mesure_event_ptr;
   begin
      -- reset des pointeurs
      prem_mesure := dern_mesure;
      -- parcour des data et RAZ du contenu
      tmp_mes := dern_mesure.next;
      loop
         tmp_mes.start_time  := 0;
         tmp_mes.end_time    := 0;
         tmp_mes.player_time := 0;
         tmp_mes.note        := 0;
         -- on sort quand on a fait le tour complet
         exit when tmp_mes = dern_mesure;
         tmp_mes := tmp_mes.next;
      end loop;
   end Clear_mesure;


   -- nettoyage des listes
   procedure Clear_all is
      tmp_n   : note_event_ptr;
      tmp_m   : midi_event_ptr;
   begin
      --
      -- note list
      --
      -- reset des pointeurs
      prem_list := dern_list;
      -- parcour des data et RAZ du contenu
      tmp_n := dern_list.next;
      loop
         tmp_n.time := 0;
         tmp_n.ampli := 0;
         tmp_n.note := 0;
         -- on sort quand on a fait le tour complet
         exit when tmp_n = dern_list;
         tmp_n := tmp_n.next;
      end loop;
      --
      -- MIDI list
      --
      -- reset des pointeurs
      prem_midi := dern_midi;
      -- parcour des data et RAZ du contenu
      tmp_m := dern_midi.next;
      loop
         tmp_m.start_time := 0;
         tmp_m.end_time := 0;
         tmp_m.note := 0;
         -- on sort quand on a fait le tour complet
         exit when tmp_m = dern_midi;
         tmp_m := tmp_m.next;
      end loop;
      --
      -- Mesure List
      --
      Clear_mesure;
      --
   end Clear_all;


   -- tronque toutes données après ce temps absolu
   -- ie end_time := max(end_time, time )
   procedure Stop_all( time : integer ) is
      tmp_m   : midi_event_ptr;
      tmp_mes : mesure_event_ptr;
   begin
      --
      -- MIDI list
      --
      -- parcour des data et RAZ du contenu
      tmp_m := dern_midi.next;
      loop
         if tmp_m.end_time > time then
            tmp_m.end_time := time;
         end if;
         -- on sort quand on a fait le tour complet
         exit when tmp_m = prem_midi;
         tmp_m := tmp_m.next;
      end loop;
      --
      -- Mesure List
      --
      -- parcour des data et RAZ du contenu
      tmp_mes := dern_mesure.next;
      loop
         if tmp_mes.end_time > time then
            tmp_mes.end_time := time;
         end if;
         -- on sort quand on a fait le tour complet
         exit when tmp_mes = prem_mesure;
         tmp_mes := tmp_mes.next;
      end loop;
   end Stop_all;



end Data_pkg;

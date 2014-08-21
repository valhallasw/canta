with Ada.Unchecked_deallocation;
with Ada.Sequential_io;
with Ada.Unchecked_Conversion;
with GNAT.Current_Exception;

with Win32;

with Byte_io;
with Log;
with Debug_pkg;

package body midifile_pkg is

   use type Win32.Byte;

   -- flag pour le debugging
   debug_flag : constant string := "midifile";

   -- type conversion byte to char
   function to_char is new Ada.Unchecked_Conversion (Source => byte, Target => Character);
   function to_byte is new Ada.Unchecked_Conversion (Source => Character, Target => byte);

   -- channel event code
   note_off_code           : constant byte := 8;
   note_on_code            : constant byte := 9;
   note_aftertouch_code    : constant byte := 10;
   controller_code         : constant byte := 11;
   program_change_code     : constant byte := 12;
   channel_aftertouch_code : constant byte := 13;
   pitch_bend_code         : constant byte := 14;

   -- meta events codes
   META_code            : constant byte := 16#FF#;  -- meta event header code
   sequence_code        : constant byte := 0;    -- sequence number
   text_code            : constant byte := 1;    -- texte message
   copyright_code       : constant byte := 2;    -- copyright message
   seq_track_name_code  : constant byte := 3;    -- sequence or track name
   instrument_name_code : constant byte := 4;    -- instrument name
   lyrics_code          : constant byte := 5;    -- lyrics
   marker_code          : constant byte := 6;    -- marker
   cue_point_code       : constant byte := 7;    -- cue point
   channel_prefix_code  : constant byte := 16#20#;   -- MIDI Channel Prefix
   port_prefix_code     : constant byte := 16#21#;   -- MIDI Port Prefix
   end_of_track_code    : constant byte := 16#2F#;   -- end track
   tempo_code           : constant byte := 16#51#;   -- tempo change
   smpte_offset_code    : constant byte := 16#54#;   -- smpte offset
   time_signature_code  : constant byte := 16#58#; -- time signature
   key_signature_code   : constant byte := 16#59#; -- key signature
   specific_code        : constant byte := 16#7F#;   -- sequencer specific meta event
   -- Syxex events code
   SYSEX_code           : constant byte := 16#F0#;
   SYSEX_continue_code  : constant byte := 16#F7#;



   status_of : constant array(event_kind_type) of byte := (
    note_on 		=> note_on_code,
    note_off		=> note_off_code,
    note_aftertouch	=> note_aftertouch_code,
    controller		=> controller_code,
    program_change	=> program_change_code,
    channel_aftertouch	=> channel_aftertouch_code,
    pitch_bend		=> pitch_bend_code,
    -- Meta events
    meta_sequence	=> meta_code,
    meta_text		=> text_code,
    meta_copyright	=> copyright_code,
    meta_seq_track_name	=> seq_track_name_code,
    meta_instrument_name=> instrument_name_code,
    meta_lyrics		=> lyrics_code,
    meta_marker		=> marker_code,
    meta_cue_point	=> cue_point_code,
    meta_channel_prefix	=> channel_prefix_code,
    meta_port_prefix	=> port_prefix_code,
    meta_end_of_track	=> end_of_track_code,
    meta_tempo		=> tempo_code,
    meta_smpte_offset	=> smpte_offset_code,
    meta_time_signature	=> time_signature_code,
    meta_key_signature	=> key_signature_code,
    meta_specific	=> specific_code,
    -- Sysex events
    sysex_normal	=> SYSEX_code,
    sysex_divide	=> SYSEX_code,
    sysex_authorization => SYSEX_continue_code
    );


   -- type fo file or track Id
   subtype string_4 is String (1 .. 4);
   midi_header : constant string_4 := "MThd";
   track_header : constant string_4 := "MTrk";

   -- -------------------------------------------------------------------
   hex_char: array(byte range 0..16#F#) of character :=
     ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
   -- -------------------------------------------------------------------


   -- the running status for channel events
   running_status : byte := 0;
   -- absolute time for each track
   abs_time : natural;

   -- global variables, easyer for debugging !
   f          : byte_io.File_Type;  	-- the file descriptor
    	-- pointer to current track
   cur_event  : events_ptr;  		-- pointer to current event
   size       : Natural := 0;   	-- track size to be read
   cur_byte   : byte;  			-- tempo byte for reading file
   delta_time : Integer := 0;		-- delta dentre deux events d'une meme track
   tmp        : events_ptr;   		-- tempo event

   cur_track  : track_data_ptr;
   -- the root of midi data read from file
   midi : midi_data_ptr;

   -- --------------------------------------------------------------------------

   -- buffers pour l'écriture d'un fichier midi

   type byte_array is array(0..65535) of byte;
   type buf_ptr is access all byte_array;

   type buffer_type;
   type buffer_ptr is access buffer_type;
   type buffer_type is record
      buf : buf_ptr;
      next : buffer_ptr;
   end record;

   buffers : buffer_ptr;	-- liste des buffers
   write_pos : integer;		-- position d'écriture dans buffer : pre-incremente
   cur_buffer : buffer_ptr;	-- buffer courant

   -- --------------------------------------------------------------------------

   -- maskage entre deux bytes

   type bool_byte is array(1..8) of boolean;
   pragma pack( bool_byte );
   for bool_byte'size use 8;

   function to_bool_byte is new Ada.unchecked_conversion( source => Win32.byte,
                                                         target => bool_byte );
   function to_byte is new Ada.unchecked_conversion( source => bool_byte,
                                                    target => Win32.byte );

      -- mask a byte by another one i.e return 'b and m'
   function mask( b : Win32.byte; m : Win32.byte ) return Win32.byte is
      bb, bm : bool_byte;
   begin
      bb := to_bool_byte( b );
      bm := to_bool_byte( m );
      bb := bb and bm;
      return to_byte( bb );
   end mask;

   -- -----------------------------------------------------------------------------

   -- Gestion des buffers et écriture dans fichier

   procedure reset_buffers is
      -- doit être appellé avant toute écriture dans les buffers
   begin
      -- allocation 1er buffer si nécessaire
      if buffers = null then
         buffers := new buffer_type;
         buffers.buf := new byte_array;
      end if;
      -- le courant devient le premier de la liste
      cur_buffer := buffers;
      -- position d'écriture avant 1ere position
      write_pos := byte_array'first - 1;
   end reset_buffers;

   procedure flush_buffers is
      b : buffer_ptr;
   begin
      b := buffers;
      -- écriture des buffers complets
      while b /= cur_buffer loop
         -- écriture du range entier
         for i in byte_array'range loop
            byte_io.write( f, b.buf(i) );
         end loop;
         -- buffer suivant
         b := b.next;
      end loop;
      -- écriture dernier buffer incomplet
      for i in byte_array'first..write_pos loop
         byte_io.write( f, b.buf(i) );
      end loop;
   end flush_buffers;

   procedure write_buffer_size is
      t : natural := 0;
      b : buffer_ptr;
      b1, b2, b3, b4 : byte;
   begin
      -- calcul de la taille totale
      b := buffers;
      -- buffers complets
      while b /= cur_buffer loop
         t := t + byte_array'last - byte_array'first + 1;
         b := b.next;
      end loop;
      -- reste
      t := t + write_pos - byte_array'first + 1;
      -- découpage en bytes
      b4 := byte(t mod 256);	-- LSB
      t := t / 256;
      b3 := byte(t mod 256);
      t := t / 256;
      b2 := byte(t mod 256);
      t := t / 256;
      b1 := byte(t mod 256);	-- MSB
      -- écriture dans fichier
      byte_io.write( f, b1 );	-- MSB first
      byte_io.write( f, b2 );
      byte_io.write( f, b3 );
      byte_io.write( f, b4 );	-- LSB last
   end write_buffer_size;

   -- --------------------------------------------------------------------------

   -- Ecriture dans buffer

   procedure write_byte( b : byte ) is
   begin
      write_pos := write_pos + 1;
      -- dépassement buffer ?
      if write_pos > byte_array'last then
         -- allocation d'un nouveau buffer
         cur_buffer.next := new buffer_type;
         cur_buffer := cur_buffer.next;		-- le buffer courant est le nouveau
         cur_buffer.buf := new byte_array;
         write_pos := byte_array'first;		-- ecriture dans 1ere position
      end if;
      --  écriture du byte
      cur_buffer.buf(write_pos) := b;
   end write_byte;

   procedure write_word( n : integer ) is
      b1, b2 : byte;
   begin
      b1 := byte( n / 256 );
      b2 := byte(n mod 256);
      write_byte( b1 );	-- MSB first
      write_byte( b2 );	-- LSB last
   end write_word;

   procedure write_3_byte( n : natural ) is
      b1, b2, b3 : byte;
      tmp : natural := n;
   begin
      b3 := byte(tmp mod 256);	-- LSB
      tmp := tmp / 256;
      b2 := byte(tmp mod 256);
      tmp := tmp / 256;
      b1 := byte(tmp mod 256);	-- MSB
      write_byte( b1);	-- MSB first
      write_byte( b2 );
      write_byte( b3 );	-- LSB last
   end write_3_byte;

   procedure write_long( n : natural ) is
      b1, b2, b3, b4 : byte;
      tmp : natural := n;
   begin
      b4 := byte(tmp mod 256);	-- LSB
      tmp := tmp / 256;
      b3 := byte(tmp mod 256);
      tmp := tmp / 256;
      b2 := byte(tmp mod 256);
      tmp := tmp / 256;
      b1 := byte(tmp mod 256);	-- MSB
      write_byte( b1);	-- MSB first
      write_byte( b2 );
      write_byte( b3 );
      write_byte( b4 );	-- LSB last
   end write_long;

   procedure write_var_len( n : natural ) is
      b1, b2, b3, b4 : byte;
      tmp : natural := n;
      started : boolean;
   begin
      b4 := byte(tmp mod 128);	-- LSB
      tmp := tmp / 128;
      b3 := byte(tmp mod 128);
      tmp := tmp / 128;
      b2 := byte(tmp mod 128);
      tmp := tmp / 128;
      b1 := byte(tmp mod 128);	-- MSB
      --
      started := false;
      if b1 /= 0 then
         write_byte( b1 + 16#80# );
         started := true;
      end if;
      if started or else b2 /= 0 then
         write_byte( b2 + 16#80# );
         started := true;
      end if;
      if started or else b3 /= 0 then
         write_byte( b3 + 16#80# );
         started := true;
      end if;
      write_byte( b4 );
   end write_var_len;

   -- --------------------------------------------------------------------------

   -- read from file
   procedure read( b : in out byte ) is
   begin
      byte_io.read( f, b );
   end read;

   function read_word return Integer is
      b1, b2 : byte := 0;
   begin
      Read ( b1);
      Read ( b2);
      return Integer (b1) * 256 + Integer (b2);
   end read_word;


   -- Read variable length data
   procedure read_var_len (value : in out Natural) is
      b : byte := 0;
   begin
      -- init value
      value := 0;
      loop
         Read ( b);
         size := size - 1;
         if mask( b, 16#80#) /= 0 then
            -- another byte is following
            b     := mask (b, 16#7F#);
            value := value * 128 + Natural (b);
         else
            -- last byte
            value := value * 128 + Natural (b);
            exit;
         end if;
      end loop;
   end read_var_len;

   -- read cur_byte when reading track data
   procedure read_cur_byte is
   begin
      Read ( cur_byte);
      size := size - 1;
   end read_cur_byte;

   -- insert tmp event as new cur_event in the events list
   procedure insert_tmp_event is
   begin
      if cur_event = null then
         -- first event in track
         cur_track.events := tmp;
         cur_event        := tmp;
      else
         -- not first event
         cur_event.next := tmp;
         tmp.prev       := cur_event;
         cur_event      := tmp;
      end if;
      -- set time
      cur_event.tick := abs_time;
   end insert_tmp_event;

   -- read string with length specified
   function read_text (length : Natural) return string_pt is
      str : string_pt;
   begin
      -- allocate string space
      str := new String (1 .. length);
      -- read data
      for i in  1 .. length loop
         read_cur_byte;
         str(i) := to_char (cur_byte);
      end loop;
      return str;
   end read_text;

   -- -------------------------------------------------------------------

   procedure read_meta_event is
      code   : byte;
      length : Natural := 0;
      ptr : events_ptr;
   begin
      -- get code of meta event
      read_cur_byte;
      code := cur_byte;
      -- read length
      read_var_len (length);
      --
      case code is

         when seq_track_name_code =>
            -- nom de la piste
            cur_track.track_name := read_text (length);
            if debug_pkg.is_set( debug_flag ) then
               Log.Store( "[" &Integer'image(abs_time) & " ]" );
               Log.Store("Track name: """ & cur_track.track_name.all & """" );
               Log.End_line;
            end if;


         when instrument_name_code =>
            -- nom de l'instrument
            cur_track.instr_name := read_text (length);
            if debug_pkg.is_set( debug_flag ) then
               Log.Store( "[" &Integer'image(abs_time) & " ]" );
               Log.Store("Instrument name: """ & cur_track.instr_name.all & """" );
               Log.End_line;
            end if;

         when lyrics_code =>
            -- paroles de chanson
            tmp := new events_rec_type (event_type => meta_event, kind => meta_lyrics);
            tmp.text := read_text (length);
            insert_tmp_event;

         when text_code =>
            -- paroles de chanson
            tmp := new events_rec_type (event_type => meta_event, kind => meta_text );
            tmp.text := read_text (length);
            insert_tmp_event;

         when end_of_track_code =>
            -- fin de la piste
            tmp := new events_rec_type (event_type => meta_event, kind => meta_end_of_track );
            insert_tmp_event;
            -- check if no size error
            while size > 0 loop
               read_cur_byte;
            end loop;

         when tempo_code =>
            -- tempo
            -- check for length = 3
            if length /= 3 then
               Log.Store( "[" &Integer'image(abs_time) & " ]" );
               if cur_track /= null then
                  Log.Store("Track" & integer'image(cur_track.track_num) );
               end if;
               Log.store("Invalid length in tempo event");
               Log.End_line;
               raise invalid_midi;
            end if;
            -- inserer dans liste des tempo
            if midi.tempos = null then
               -- nouvel event
               tmp := new events_rec_type (event_type => meta_event, kind => meta_tempo);
               -- le premier
               midi.tempos := tmp;
            else
               -- insére en queue de liste
               ptr := midi.tempos;
               while ptr.next /= null loop
                  ptr := ptr.next;
               end loop;
               if ptr.tick < abs_time then
                  -- nouvel event
                  tmp := new events_rec_type (event_type => meta_event, kind => meta_tempo);
                  ptr.next := tmp;
                  tmp.prev := ptr;
               else
                  -- on réutilise le même event
                  tmp := ptr;
               end if;
            end if;
            -- lecture valeur
            tmp.tempo := 0;
            for i in  1 .. 3 loop
               read_cur_byte;
               tmp.tempo := tmp.tempo * 256 + Integer (cur_byte);
            end loop;
            -- le temps
            tmp.tick := abs_time;

         when time_signature_code =>
            -- check for length = 4
            if length /= 4 then
               Log.Store( "[" &Integer'image(abs_time) & " ]" );
               if cur_track /= null then
                  Log.Store("Track" & integer'image(cur_track.track_num) );
               end if;
               Log.store("Invalid length in time signature event");
               Log.End_line;
               raise invalid_midi;
            end if;
            if midi.signatures = null then
               tmp := new events_rec_type (event_type => meta_event, kind => meta_time_signature);
               midi.signatures := tmp;
            else
               -- insére en queue de liste
               ptr := midi.signatures;
               while ptr.next /= null loop
                  ptr := ptr.next;
               end loop;
               if ptr.tick < abs_time then
                  -- nouvel event
                  tmp := new events_rec_type (event_type => meta_event, kind => meta_time_signature);
                  ptr.next := tmp;
                  tmp.prev := ptr;
               else
                  -- on réutilise le même event
                  tmp := ptr;
               end if;
            end if;
            -- lecture des valeurs
            -- byte 1
            read_cur_byte;
            tmp.numerator := cur_byte;
            -- byte 2
            read_cur_byte;
            tmp.denominator := cur_byte;
            -- byte 3
            read_cur_byte;
            tmp.metronome := cur_byte;
            -- byte 4
            read_cur_byte;
            tmp.rate_32nd_per_quarter := cur_byte;

         when others =>
            -- ignorés
            for i in  1 .. length loop
               read_cur_byte;
            end loop;
            if debug_pkg.is_set( debug_flag ) then
               Log.Store( "[" &Integer'image(abs_time) & " ]" );
               Log.Store("META, Ignored, code=" & integer'image(integer(code)) );
               Log.End_line;
            end if;
            return;

      end case;
      --
      --
      if debug_pkg.is_set( debug_flag ) and then cur_event /= null then
         Log.Store( "[" & integer'image(cur_event.tick) & " ]");
         Log.Store( event_kind_type'image(cur_event.kind) );
         if cur_event.kind in meta_text..meta_cue_point then
            Log.Store("  """ & cur_event.text.all & """");
         end if;
         Log.End_line;
      end if;

   end read_meta_event;

   -- -------------------------------------------------------------------

   procedure read_sysex_event is
      length : Natural := 0;
   begin
      -- read bytes but ignore event
      read_var_len (length);
      for i in  1 .. length loop
         read_cur_byte;
      end loop;
      if debug_pkg.is_set( debug_flag ) then
         Log.Store( "[" &Integer'image(abs_time) & " ]" );
         Log.Store("SYSEX, Ignored");
         Log.End_line;
      end if;
   end read_sysex_event;

   -- -------------------------------------------------------------------

   procedure read_channel_event is
      channel        : byte;
      code           : byte;
      param1, param2 : byte;
      status : byte;
   begin
      -- check for running status
      if mask( cur_byte, 16#80# ) = 0 then
         -- running status
         status := running_status;
         param1 := cur_byte;
      else
         -- normal status
         status := cur_byte;
         running_status := status;
         -- read parameter
         read_cur_byte;
         param1 := cur_byte;
      end if;
      -- extract channel
      channel := status mod 16;
      -- extract event code
      code := status / 16;
      -- create the event
      case code is

         when note_on_code =>
            read_cur_byte;
            param2 := cur_byte;
            if param2 = 0 then
               -- note_on avec vol=0 => note_off
               tmp := new events_rec_type (event_type => channel_event, kind => note_off);
            else
               tmp := new events_rec_type (event_type => channel_event, kind => note_on);
            end if;
            insert_tmp_event;
            cur_event.note := param1;
            cur_event.param := param2;

         when note_off_code =>
            tmp := new events_rec_type (event_type => channel_event, kind => note_off);
            insert_tmp_event;
            cur_event.note := param1;
            read_cur_byte;
            param2          := cur_byte;
            cur_event.param := param2;

         when note_aftertouch_code =>
            tmp := new events_rec_type (event_type => channel_event, kind => note_aftertouch);
            insert_tmp_event;
            cur_event.note := param1;
            read_cur_byte;
            param2          := cur_byte;
            cur_event.param := param2;

         when controller_code =>
            tmp := new events_rec_type (event_type => channel_event, kind => controller);
            insert_tmp_event;
            cur_event.controller_nbr := param1;
            read_cur_byte;
            param2                     := cur_byte;
            cur_event.controller_value := param2;

         when program_change_code =>
            tmp := new events_rec_type (event_type => channel_event, kind => program_change);
            insert_tmp_event;
            cur_event.program_number := param1;
            if cur_track.prog_num = 0 then
               cur_track.prog_num := Integer(cur_event.program_number);
            end if;

         when channel_aftertouch_code =>
            tmp := new events_rec_type (event_type => channel_event, kind => channel_aftertouch);
            insert_tmp_event;
            cur_event.aftertouch_value := param1;

         when pitch_bend_code =>
            tmp := new events_rec_type (event_type => channel_event, kind => pitch_bend);
            insert_tmp_event;
            read_cur_byte;
            param2                := cur_byte;
            cur_event.pitch_value := Integer (param2) + Integer (param1) * 128;

         when others =>
            Log.Store( "[" &Integer'image(abs_time) & " ]" );
            if cur_track /= null then
               Log.Store("Track" & integer'image(cur_track.track_num) );
            end if;
            Log.Store("Invalid channel event code:" & integer'image(integer(code)) );
            Log.End_line;
            raise Invalid_midi;

      end case;
      -- for all channel events:
      -- set channel
      cur_event.channel := channel;
      --
      if debug_pkg.is_set( debug_flag ) then
         Log.Store( "[" & integer'image(cur_event.tick) & " ]");
         Log.Store( event_kind_type'image(cur_event.kind) );
         Log.Store(" Channel=" & byte'image(channel) );
         case cur_event.kind is
            when note_on | note_off | note_aftertouch  =>
               Log.Store("Note=" & integer'image(Integer(cur_event.note)) );
               Log.Store("Value=" & integer'image(Integer(cur_event.param)) );
            when controller =>
               Log.Store("Nbr=" & integer'image(Integer(cur_event.controller_nbr)) );
               Log.Store("Value=" & integer'image(Integer(cur_event.controller_value)) );
            when program_change =>
               Log.Store("Nbr=" & integer'image(Integer(cur_event.program_number)) );
            when channel_aftertouch =>
               Log.Store("Value=" & integer'image(Integer(cur_event.aftertouch_value)) );
            when pitch_bend =>
               Log.Store("Value=" & integer'image(cur_event.pitch_value) );
            when others => null;
         end case;
         Log.End_line;
      end if;

   end read_channel_event;

   -- -------------------------------------------------------------------

   procedure read_one_event is
   begin -- read one event
      -- read delta time
      read_var_len (delta_time);
      -- compute absolute time
      abs_time := abs_time + delta_time;
      -- read event type
      read_cur_byte;
      --
      if cur_byte = META_code then
         -- meta event
         read_meta_event;
      elsif cur_byte >= SYSEX_code then
         -- sysex event : read but ignore content
         read_sysex_event;
      else
         -- channel event
         read_channel_event;
      end if;
   end read_one_event;

   -- -------------------------------------------------------------------

   procedure read_track is
      trk_id : string_4;
      b      : byte := 0;
   begin
      -- initialize absolute time
      abs_time := 0;
      -- -----------------
      -- read track header: chunk id (4 bytes) and chunk size (4 bytes)
      -- -----------------
      -- read track id
      for i in  1 .. 4 loop
         Read ( b);
         trk_id (i) := to_char (b);
      end loop;
      if trk_id /= track_header then
         if cur_track /= null then
            Log.Store("(Previous track:" & integer'image(cur_track.track_num) & ")" );
         end if;
         Log.Store("Invalid track header");
         Log.End_line;
         raise invalid_midi;
      end if;
      -- read chunk size
      size := 0;
      for i in  1 .. 4 loop
         Read ( b);
         size := size * 256 + Integer (b);
      end loop;
      -- -------------------
      -- read all track data
      -- -------------------
      if debug_pkg.is_set( debug_flag ) then
         Log.Store( "Track header, size=" & integer'image(size) );
         Log.End_line;
         Log.Store("-- Starting track data --");
         Log.End_line;
      end if;
      while size > 0 loop
         -- read one event
         read_one_event;
      end loop;
      -- end of track
   end read_track;

   -- ---------------------------------------------------------------------
   -- Main procedure: read_file
   -- ---------------------------------------------------------------------

   procedure Init_data is
   begin
      -- the running status for channel events
      running_status := 0;
      -- absolute time for each track
      abs_time := 0;
      -- global variables, easyer for debugging ! (limitation du debugger)
      cur_track  := null; -- pointer to current track
      cur_event  := null;  -- pointer to current event
      size       := 0;   -- track size to be read
      cur_byte   := 0;  -- tempo byte for reading file
      delta_time := 0;
      tmp        := null;   -- tempo event
   end Init_data;


   function read_file (name : String) return midi_data_ptr is
      id : string_4;       -- header id
      b  : byte := 0;
      n  : Integer;   -- tempo integer for file format
   begin
      -- initialisation des données globales
      Init_data;
      -- open file
      begin
         byte_io.Open (f, name);
      exception
         when others =>
            raise file_not_found;
      end;

      -- allocation structure racine
      midi := new midi_data_type;

      -- ----------------
      -- read file header
      -- ----------------

      -- read file id
      for i in  1 .. 4 loop
         Read ( b);
         id (i) := to_char (b);
      end loop;
      -- check for midi file id
      if id /= midi_header then
         Log.Store("Invalid MIDI header file");
         Log.End_line;
         raise invalid_midi;
      end if;
      -- skip length ( always 6 )
      for i in  1 .. 4 loop
         Read ( b);
      end loop;
      -- read format
      n := read_word;
      -- read number of tracks
      midi.nbr_tracks := read_word;
      -- read time division
      Read ( b);
      if b >= 16#80# then
         -- frames: ignore, reste avec le défaut de 120 bpm
         Read ( b);
      else
         -- beats
         b := mask (b, 16#7F#);
         midi.ticks_per_beats := integer(b) * 256;
         Read ( b);
         midi.ticks_per_beats := midi.ticks_per_beats + integer(b);
      end if;

      if debug_pkg.is_set( debug_flag ) then
         Log.Store( "File header , nbr_tacks ="  & integer'image(midi.nbr_tracks)  & ", ticks/beat=" & integer'image(midi.ticks_per_beats) );
         Log.End_line;
      end if;

      -- ---------------
      -- read all tracks
      -- ---------------

      for track_num in  1 .. midi.nbr_tracks loop
         -- allocate track record
         if cur_track = null then
            -- first track
            midi.tracks   := new track_data_type;
            cur_track     := midi.tracks;
            cur_track.track_num := 1;
         else
            -- following tracks
            cur_track.next   := new track_data_type;
            cur_track.next.prev := cur_track;
            cur_track        := cur_track.next;
            cur_track.track_num := track_num;
         end if;
         -- reset event pointer
         cur_event := null;
         -- read data
         read_track;
      end loop;

      -- --------------
      -- close the file
      -- --------------
      byte_io.Close (f);

      if debug_pkg.is_set( debug_flag ) then
         Log.Store( "*** End of file ***");
         Log.End_line;
      end if;

      return midi;

   exception
      when invalid_midi | file_not_found =>
         -- if still open close the file
         byte_io.Close (f);
         Remove(midi);
         -- log
         Log.Store( "File:");
         Log.Store( name );
         Log.End_line;
         --
         raise;

      when others =>
         -- if still open close the file
         byte_io.Close (f);
         Remove(midi);
         if debug_pkg.is_set( debug_flag ) then
            Log.Error( GNAT.Current_Exception.Exception_Information );
            Log.End_line;
         end if;
         raise invalid_midi;
   end read_file;

   -- **************************************************************************

   procedure write_delta_time( time : natural ) is
   begin
      write_var_len( time - abs_time );
      abs_time := time;
   end write_delta_time;


   procedure write_event( event : events_ptr ) is
   begin
      --
      case event.event_type is

         when channel_event =>
            case event.kind is
               when note_on =>
                  write_delta_time( event.tick );
                  write_byte( note_on_code * 16 + event.channel );
                  write_byte( event.note );
                  write_byte( event.param );

               when note_off =>
                  write_delta_time( event.tick );
                  write_byte( note_off_code * 16 + event.channel );
                  write_byte( event.note );
                  write_byte( event.param );

               when note_aftertouch =>
                  write_delta_time( event.tick );
                  write_byte( note_aftertouch_code * 16 + event.channel );
                  write_byte( event.note );
                  write_byte( event.param );

               when controller =>
                  write_delta_time( event.tick );
                  write_byte( controller_code * 16 + event.channel );
                  write_byte( event.controller_nbr );
                  write_byte( event.controller_value );

               when program_change =>
                  write_delta_time( event.tick );
                  write_byte( program_change_code * 16 + event.channel );
                  write_byte( event.program_number );

               when channel_aftertouch =>
                  write_delta_time( event.tick );
                  write_byte( channel_aftertouch_code * 16 + event.channel );
                  write_byte( event.aftertouch_value );

               when pitch_bend =>
                  write_delta_time( event.tick );
                  write_byte( pitch_bend_code * 16 + event.channel );
                  write_byte( byte(event.pitch_value / 128) );
                  write_byte( byte(event.pitch_value mod 128) );

                when others => null;
            end case;

         when meta_event =>
            --
            case event.kind is

               when meta_tempo =>
                  write_delta_time( event.tick );
                  write_byte( META_code );
                  write_byte( tempo_code );
                  write_byte( 3 );
                  write_3_byte( event.tempo );

               when meta_end_of_track =>
                  write_delta_time( event.tick );
                  write_byte( META_code );
                  write_byte( end_of_track_code );
                  write_byte( 0 );

               when others => null;	-- pour le compilateur

            end case;
         when sysex_event =>
            null;
      end case;
   end write_event;


   procedure write_cur_track is
   begin
      -- track header
      reset_buffers;
      for i in track_header'range loop
         write_byte( to_byte(track_header(i)) );
      end loop;
      flush_buffers;
      --
      -- écriture des events dans un buffer
      reset_buffers;
      cur_event := cur_track.events;
      -- RAZ du temps absolu pour le calcul du delta-time de chaque event
      abs_time := 0;
      while cur_event /= null loop
         -- écriture de l'event
         write_event( cur_event );
         -- suivant
         cur_event := cur_event.next;
      end loop;
      -- écriture de la taille
      write_buffer_size;
      -- flush du buffer dans le fichier
      flush_buffers;
   end write_cur_track;


   procedure write_file( data : midi_data_ptr;
                         name : String) is
   begin
      -- creation du fichier de sortie
      begin
         byte_io.Create (f, name);
      exception
         when others =>
            raise not_created;
      end;
      -- ------------------
      -- écriture du header
      --
      reset_buffers;
      --
      for i in midi_header'range loop
         write_byte( to_byte(midi_header(i)) );
      end loop;
      -- écriture longueur du header
      write_long( 6 );
      -- écriture format
      write_word( 1 );
      -- écriture nombre de tracks
      write_word( data.nbr_tracks );
      -- time division
      write_word( data.ticks_per_beats );
      --
      flush_buffers;
      --
      -- fin du header
      -- -------------

      -- écritures des tracks
      cur_track := data.tracks;
      while cur_track /= null loop
         write_cur_track;
         cur_track := cur_track.next;
      end loop;

      -- fermeture fichier
      byte_io.close( f );

   exception
      when others =>
         byte_io.close( f );
         raise;
   end write_file;

   -- **************************************************************************

   -- -------------
   -- Deallocation
   -- -------------


   procedure Free is new Ada.Unchecked_deallocation
     ( object => midi_data_type, name => midi_data_ptr );

   procedure Free is new Ada.Unchecked_deallocation
     ( object => track_data_type, name => track_data_ptr );

   procedure Free is new Ada.Unchecked_deallocation
     ( object => string, name => string_pt );

   procedure Free is new Ada.Unchecked_deallocation
     ( object => events_rec_type, name => events_ptr );

   procedure Remove( events : in out events_ptr ) is
      next : events_ptr;
   begin
      while events /= null loop
         next := events.next;
         --
         case events.kind is
            when meta_text | meta_lyrics =>
               Free( events.text );
            when others =>
               null;
         end case;
         Free( events );
         --
         events := next;
      end loop;
   end Remove;

   procedure Remove( tracks : in out track_data_ptr ) is
      next : track_data_ptr;
   begin
      while tracks /= null loop
         next := tracks.next;
         --
         Remove( tracks.events );
         Free( tracks );
         --
         tracks := next;
      end loop;
   end Remove;

   procedure Remove( midi : in out midi_data_ptr ) is
   begin
      if midi = null then
         return;
      end if;
      Remove( midi.tracks );
      Free( midi );
      midi := null;
   end Remove;

   -- *********************************************************************************

   function get_status( code : event_kind_type ) return byte is
   begin
      return status_of( code );
   end get_status;


end midifile_pkg;

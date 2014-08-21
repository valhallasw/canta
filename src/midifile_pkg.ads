with Win32;	use Win32;

with Common_types;	use Common_types;

package midifile_pkg is


   -- exceptions levée pendant la lecture
   file_not_found : exception;
   not_midi_file : exception;
   invalid_midi : exception;

   -- exceptions levées pendant l'écriture
   not_created    : exception;

   -- ------------------
   -- type for midi data
   -- ------------------

   controller_sustain: constant byte := 64;

   -- --------------
   -- the event type
   -- --------------

   -- all types of event
   type event_type_type is (channel_event, meta_event, sysex_event);

   type event_kind_type is
   (
    -- Channel events
    note_on,
    note_off,
    note_aftertouch,
    controller,
    program_change,
    channel_aftertouch,
    pitch_bend,
    -- Meta events
    meta_sequence,
    meta_text,
    meta_copyright,
    meta_seq_track_name,
    meta_instrument_name,
    meta_lyrics,
    meta_marker,
    meta_cue_point,
    meta_channel_prefix,
    meta_port_prefix,
    meta_end_of_track,
    meta_tempo,
    meta_smpte_offset,
    meta_time_signature,
    meta_key_signature,
    meta_specific,
    -- Sysex events
    sysex_normal,
    sysex_divide,
    sysex_authorization
   );



   -- event record
   type events_rec_type;
   type events_ptr is access events_rec_type;
   type events_rec_type (event_type : event_type_type; kind : event_kind_type) is record
      tick       : Natural := 0;
      time       : Natural := 0;
      next, prev : events_ptr;   -- next and previous event
      case event_type is

         when channel_event =>
            channel : byte;
            case kind is
               when note_on | note_off | note_aftertouch =>
                  note  : byte := 0;
                  param : byte := 0;
               when controller =>
                  controller_nbr   : byte := 0;
                  controller_value : byte := 0;
               when program_change =>
                  program_number : byte := 0;
               when channel_aftertouch =>
                  aftertouch_value : byte := 0;
               when pitch_bend =>
                  pitch_value : Integer := 0;
               when others =>
                  null;
            end case;

         when meta_event =>
            case kind is
               when meta_tempo =>
                  tempo : natural := 0;	-- changement de tempo !
               when meta_lyrics | meta_text | meta_seq_track_name =>
                  text : string_pt;	-- les paroles des karaokes
               when meta_time_signature => -- signature 4/4 ou 6/8 etc
                  numerator             : byte := 4;
                  denominator           : byte := 4;
                  metronome             : byte := 24;
                  rate_32nd_per_quarter : byte := 8;
               when others =>
                  null;
            end case;

         when sysex_event =>
            null;
      end case;
   end record;

   -- Track type
   type track_data_type;
   type track_data_ptr is access track_data_type;
   type track_data_type is record
      track_num  : natural := 0;	-- the track number
      track_name : string_pt;		-- nom piste
      instr_name : string_pt;		-- nom instrument
      prog_num   : integer := 0;	        -- premier instrument
      events     : events_ptr; 		-- all events par ordre chronologique
      cur_event  : events_ptr;
      polyphonie : natural := 0;	-- nombre max de notes jouées simultanément
      semblance  : natural := 0;	-- ressemblance avec les paroles en %
      next, prev : track_data_ptr;  	-- next and previous tracks
   end record;

   -- outermost structure
   type midi_data_type is record
      ticks_per_beats : Natural := 0;		-- horloge
      nbr_tracks      : Natural := 0; 	        -- number of tracks
      tracks          : track_data_ptr; 	-- pointer for tracks description
      tempos          : events_ptr; 		-- les meta event tempo pour le calcul du temps absolu
      signatures      : events_ptr;		-- les meta event time signature pour le calcul des mesures
   end record;

   -- pointeur sur structure
   type midi_data_ptr is access midi_data_type;

   -- *******************************************************************

   -- lecture d'un fichier et décodage
   function read_file (name : String) return midi_data_ptr;

   -- écriture d'un fichier
   procedure write_file( data : midi_data_ptr; name : String);

   -- libération de la mémoire utilisée
   procedure Remove( midi : in out midi_data_ptr );

   -- donne le code (ou status) pour le kind donné
   function get_status( code : event_kind_type ) return byte;

end midifile_pkg;

with Unchecked_deallocation;

package data_pkg is

   type note_event_rec;
   type note_event_ptr is access note_event_rec;
   type note_event_rec is record
      time  : integer;
      note  : short_integer;
      ampli : short_integer;
      -- chainage
      next : note_event_ptr;
      prev : note_event_ptr;
   end record;

   -- types pour les notes Midi
   type midi_event_rec;
   type midi_event_ptr is access midi_event_rec;
   type midi_event_rec is record
      start_time : integer;
      end_time   : integer;
      note       : short_integer;
      -- chainage
      next       : midi_event_ptr;
      prev       : midi_event_ptr;
   end record;

   -- types pour les mesures
   type mesure_event_rec;
   type mesure_event_ptr is access mesure_event_rec;
   type mesure_event_rec is record
      start_time : integer;	--temps absolu de d�but et de fin
      end_time   : integer;
      player_time: integer;	-- d�but en temps du player
      note       : short_integer;
      -- chainage
      next       : mesure_event_ptr;
      prev       : mesure_event_ptr;
   end record;

   -- ================================================================================================

   -- Initialisation: allocation m�moire pour le stockage des notes chant�es
   procedure Init;

   procedure Clear_all;

   -- stockage d'une note chant�e
   procedure Store_Note( time  : integer;
                         note  : short_integer;
                         ampli : short_integer);

   -- stockage d'une note midi jou�e
   procedure Store_Midi( start_time,
                         end_time : integer;
                         note     : short_integer );	-- en cents !

   -- stockage d'une note midi jou�e
   procedure Store_Mesure( start_time,
                           end_time    : integer;
                           player_time : integer;
                           numero      : short_integer );

   -- lecture des mesures
   procedure Get_Mesure( start_time,
                         end_time   : integer;
                         first_mesure,
                         last_mesure : out mesure_event_ptr );

   -- mesure correspondant au temps
   function Get_Mesure( time : integer ) return mesure_event_ptr;

   -- efface toutes les donn�es concernant les mesures
   procedure Clear_mesure;

   -- lecture des notes chant�es
   procedure Get_Note( start_time,
                       end_time   : integer;
                       first_event,
                       last_event : out note_event_ptr );

   -- lecture des notes midi jou�es
   procedure Get_Midi( start_time,
                       end_time   : integer;
                       first_event,
                       last_event : out midi_event_ptr );

   -- temps min et max des notes dans la liste
   function Get_Max_Time return integer;
   function Get_Min_Time return integer;

   -- tronque toutes donn�es apr�s ce temps
   procedure Stop_all( time : integer );

end Data_pkg;

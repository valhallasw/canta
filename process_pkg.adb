with Ada.Numerics.Elementary_Functions;
with Ada.Numerics;
with Ada.Numerics.Long_Complex_Types;	use Ada.Numerics.Long_Complex_Types;
with Ada.Numerics.Long_Elementary_Functions;	use Ada.Numerics.Long_Elementary_Functions;

with Unchecked_deallocation;
with System;		use System;
with Calendar;

with Interfaces.C;	use Interfaces.C;
with GNAT.Current_Exception;

with Win32;		use Win32;
with Win32.Mmsystem;	use Win32.Mmsystem;
with Win32.Windef;	use Win32.Windef;
with Win32.Winuser;
with Win32.Winbase;
with Win32.Winnt;

with Conversions;	use Conversions;
with Common_types;	use Common_types;
with Resources_pkg;
with User_Interface;
with Win_Audio;		use Win_Audio;
with Log;
with Config_pkg;
with Utils_pkg;
with Intl;
with Timer_pkg;
with Wavefile_pkg;
with Midi_pkg;
with Data_pkg;		use Data_pkg;
with Stat_pkg;
with Debug_pkg;

package body Process_pkg is

   debug_flag   : constant string := "process";
   debug_flag_2   : constant string := "pics";

   -- exception levée à la fin de la démo
   end_demo : exception;
   stopping : boolean := false;	-- flag pour éviter plusieurs affichage de pop-up

   -- --------------------------------------------------------------------------
   -- compteurs du nombre d'échantilons traité => temps de la carte
   sample_courant : integer := 0;	-- compte le nombre de samples reçus
   sample_data : integer := 0;		-- utilisé pour calculer le temps des mesures
   -- temps de la dernière mesure (= temps courant) en millisecondes
   current_time : integer := 0;
   -- pointeur dans 'data' de la dernière mesure valide
   dern_mesure : integer := 0;
   -- inverse de la fréquence d'échantillonnage: 1/Fs
   periode_echantillons : Long_float;
   -- tableaux des pics pour recherche du pitch
   max_nb_pics : constant := 6;
   type pic_record_type is record
      freq : Long_float;	-- fréquence en Hz
      amp : long_float;		-- amplitude
      bin : integer;		-- index dans le tableau des amplitudes
   end record;
   type pic_array is array(1..max_nb_pics) of pic_record_type;
   pics : pic_array;
   --
   pic_index : natural := 0;	-- index dans pics du pic  précédemment choisi
   h_rang    : natural := 0;	-- position harmonique du pitch: 1 = fondamentale, 2 = harmonique 2, etc
   track_bin : natural := 0;	-- bin du pic à suivre

   -- fenetre de calcul d'autocorrélation
   win_time : constant Long_float := 15.0e-3;	-- 15 ms
   win_size : integer;				-- taille en nombre d'échantillons
   -- rapport entre max et min lors de recherche d'autocorrélation
   max_min_correlation : constant Long_Float := 2.0;
   -- --------------------------------------------------------------------------

   -- nom de classe pour la window Data
   DataCLASS    : constant String := Config_pkg.Appli_name & "Data" & ASCII.nul;

   -- valeur minimale de l'amplitude nécessaire pour une mesure correcte
   Seuil_ampli : constant Long_Float := 1_000.0;

   -- --------------------------------------------------------------------------
   -- flag de sauvegarde
   sauvegarde : boolean := False;
   data_length : integer := 0;
   -- temps de démarrage de la sauvegarde, pour la calcul des temps MIDI
   time_start : integer;
   -- racine des noms de fichier ('racine.wav' et 'racine.mid')
   racine : string_pt;
   -- fichier wave
   wave_file : Win32.Winnt.HANDLE;
   wave_file_name : string_pt;
   -- --------------------------------------------------------------------------

   type short_array is array(integer range<>) of Short_integer;

   -- mode gel
   freezed : boolean := false;

   -- duree max de la démo: 10mn en millisecondes
   max_duree_demo : constant := 600_131;	-- plus un chouia pour tromper l'ennemi !

   -- **************************************************************************
   -- taille de la FFT
   order : integer := 11;	-- par défaut 2048 points
   N     : integer := 2**order;

   type comp_array_type is array(integer range <>) of complex;
   type comp_array_pt is access comp_array_type;
   procedure Free is new Unchecked_deallocation( comp_array_type, comp_array_pt );

   type float_array_type is array(integer range <>) of Long_float;
   type float_array_pt is access float_array_type;
   procedure Free is new Unchecked_deallocation( float_array_type, float_array_pt );


   data  : float_array_pt;	-- les données de départ
   resu  : comp_array_pt;	-- le résultat de la FFT en complexes
   ampli : float_array_pt;	-- l'amplitude du spectre
   --
   type int_array_type is array(integer range <>) of integer;
   type int_array_pt is access int_array_type;
   procedure Free is new Unchecked_deallocation( int_array_type, int_array_pt );
   --
   reversal : int_array_pt;	-- tableau pour l'inversion de bits (index des permuttations)
   unite : comp_array_pt;	-- tableau des constantes = exp(-i*pi/k^n)

   -- procedure d'initialisation des tableau de constantes
   procedure FFT_create_tables( Fs : integer ) is
      j: integer;
      m : integer;
      theta : long_float;
   begin
      case FS is
         when 11_025 => order := 9;
         when 22_050 => order := 10;
         when others => order := 11;
      end case;
      -- taille de la FFT, et des buffers
      N := 2**order;
      -- table des permutation
      -- allocation
      Free( reversal );
      reversal := new int_array_type(0..N-1);
      for i in reversal'range loop
         reversal(i) := i;
      end loop;
      --
      j := 0;
      for i in 0..N/2-1 loop
         if j > i then
            -- permutte data(i) et data(j)
            reversal(i) := j;
            reversal(j) := i;
            if j < N/2 then
               reversal(N-1-j) := N-1-i;
               reversal(N-1-i) := N-1-j;
            end if;
         end if;
         m := N/2;
         while j >= m loop
            j := j - m;
            m := m / 2;
         end loop;
         j := j + m;
      end loop;
      -- table trigo
      Free( unite );
      unite := new comp_array_type(0..order);
      --
      unite(0) := (re => 1.0, im => 0.0);
      for k in 1..order loop
         theta := Ada.Numerics.pi / 2.0**(k-1);
         unite(k) := ( re => cos(theta), im => sin(theta) );
      end loop;
      --
      -- allocations et initialisation tableau entrées-sorties
      --
      -- data et deriv ont une taille plus grande pour permettre plusieurs mesures
      -- à cheval sur deux buffers de suite
      Free(data);
      data := new float_array_type(0..N+Common_types.buff_size-1);
      data.all := float_array_type'(0..N+Common_types.buff_size-1 => 0.0);	-- initialise à 0
      -- résultat en complexes
      Free(resu);
      resu := new comp_array_type(0..N-1);
      -- amplitude data et derivée en réels
      Free(ampli);
      ampli := new float_array_type(0..N-1);
      --
   end FFT_create_tables;


   -- calcul de la FFT : entrées dans 'in_data(offset..offset+N-1)' resultats complexe dans 'out_resu(0..N-1)'
   -- et amplitudes dans 'out_ampli'
   -- les data sont "windowées" par 'in_wind' si non null
   procedure Do_FFT( in_data   : float_array_pt;
                     offset    : integer;
                     out_ampli : float_array_pt ) is
      pragma Suppress(All_Checks);
      temp : complex;
      m_max, i, j : integer;
      istep : integer;
      wp, w : complex;
   begin
      -- bit-reversal
      for i in 0..N-1 loop
         resu(i).Re := in_data(reversal(i)+offset);
         resu(i).Im := 0.0;
      end loop;
      -- "butterfly"
      m_max := 1;
      for k in 1..order loop
         istep := m_max * 2;
         wp := unite(k);
         w  := unite(0);
         for m in 0..m_max-1 loop
            i := m;
            while i < N loop
               j := i + m_max;
               temp        := w * resu(j);
               resu(j) := resu(i) - temp;
               resu(i) := resu(i) + temp;
               --
               i := i + istep;
            end loop;
            w := w * wp;
         end loop;
         m_max := istep;
      end loop;
      -- calcule amplitude
      for k in 0..N-1 loop
         out_ampli(k) := Modulus(resu(k));
      end loop;
   end Do_FFT;


   -- **************************************************************************
   procedure Toggle_freeze is
   begin
      freezed := not freezed;
   end Toggle_freeze;

   procedure Start_freeze is
   begin
      freezed := true;
   end Start_freeze;

   procedure End_freeze is
   begin
      freezed := false;
   end End_freeze;

   -- retourne le temps en millisecondes de la dernière mesure réaliséee
   function Card_clock return integer is
   begin
      return current_time;
   end Card_clock;

   -- ===========================================================================

   procedure Save_wave_file( buffer : Win32.LPBYTE; length : Win32.DWORD ) is
      res_bool : Win32.BOOL;
      written : Integer;
   begin
      res_bool := Win32.Winbase.WriteFile (
                       hFile                  => wave_file,
                       lpBuffer               => TO_ADDRESS(buffer),
                       nNumberOfBytesToWrite  => length,
                       lpNumberOfBytesWritten => TO_LPDWORD( written'address ),
                       lpOverlapped           => null );
      if res_bool = 0 then
         Utils_pkg.Log_Windows_error("Save_wave_file: WriteFile");
      end if;
      -- compteur octets enregistrés
      data_length := data_length + Integer(length);
   end Save_wave_file;


   function Demarre_sauvegarde( root : string ) return boolean is
   begin
      -- racine du nom de fichier
      Free( racine );
      racine := new string'( root );
      --
      -- sauvegarde nom fichier wav
      Free( wave_file_name );
      wave_file_name := new string'( root & ".wav" & ascii.nul);
      -- ouverture fichier
      wave_file := Win32.Winbase.CreateFile(
                        lpFileName            => TO_LPCSTR(wave_file_name.all'address),
                        dwDesiredAccess       => Win32.Winnt.FILE_GENERIC_WRITE,
                        dwShareMode           => Win32.Winnt.FILE_SHARE_READ,
                        lpSecurityAttributes  => null,
                        dwCreationDisposition => Win32.Winbase.CREATE_ALWAYS,
                        dwFlagsAndAttributes  => Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
                        hTemplateFile         => System.Null_address );
      -- test résultat
      if wave_file = Win32.Winbase.INVALID_HANDLE_VALUE then
         -- fichier existant locké ?
         Utils_pkg.Error_box( Intl.def_err_title, """" & root & ".wav"":" & new_line & new_line & Intl.err_cre_wav);
         sauvegarde := false;
         return false;
      end if;
      -- réserve espace pour le header Wave
      Wavefile_pkg.Write_header( wave_file, 0, Common_types.Fs );
      -- reset longueur
      data_length := 0;
      --
      -- MIDI
      --
      -- mémorise temps de démarrage
      time_start := current_time;
      --
      --
      -- flag sauvegarde en cours
      sauvegarde := true;
      return true;
   end Demarre_sauvegarde;


   procedure Termine_sauvegarde is
      res_bool : Win32.BOOL;
   begin
      -- flag: arrêt écriture
      sauvegarde := false;
      --
      -- WAV
      --
      -- ferme fichier wave
      res_bool := Win32.Winbase.CloseHandle( wave_file );
      if res_bool = 0 then
         Utils_pkg.Log_Windows_error("Termine_sauvegarde: CloseHandle-1");
      end if;
      -- réouverture en lecture/ecriture du fichier wav
      wave_file := Win32.Winbase.CreateFile(
                        lpFileName            => TO_LPCSTR(wave_file_name.all'address),
                        dwDesiredAccess       => Win32.Winnt.FILE_GENERIC_WRITE or Win32.Winnt.FILE_GENERIC_READ,
                        dwShareMode           => Win32.Winnt.FILE_SHARE_READ,
                        lpSecurityAttributes  => null,
                        dwCreationDisposition => Win32.Winbase.OPEN_EXISTING,
                        dwFlagsAndAttributes  => Win32.Winnt.FILE_ATTRIBUTE_NORMAL,
                        hTemplateFile         => System.Null_address );
      if wave_file = Win32.Winbase.INVALID_HANDLE_VALUE then
         Utils_pkg.Log_Windows_error("Termine_sauvegarde: CreateFile");
      end if;
      -- écriture header correct
      Wavefile_pkg.Write_header( wave_file, data_length, Common_types.Fs );
      -- ferme fichier temporaire
      res_bool := Win32.Winbase.CloseHandle( wave_file );
      if res_bool = 0 then
         Utils_pkg.Log_Windows_error("Termine_sauvegarde: CloseHandle-2");
      end if;
      --
      -- MIDI
      --
      -- génère le Midi
--      Generer_Midi;
   end Termine_sauvegarde;


   -- ===========================================================================

   function To_cents( freq : Long_Float ) return Long_Float is
   pragma Inline(To_cents);
   begin
      return Ada.Numerics.Long_Elementary_Functions.log( freq / 440.0, base => 10.0 ) * 3986.313714 + 6900.0;
   end To_cents;


   function egal_a_peu_pres( l, r : long_float ) return boolean is
      tolerance : constant Long_float := 0.1;
   begin
      if r *(1.0-tolerance) <= l and then l <= r * (1.0+tolerance) then
         return true;
      else
         return false;
      end if;
   end egal_a_peu_pres;


   function Compute_Pitch return Long_Float is
      pitch : Long_float := 0.0;
      somme : Long_float := 0.0;
      max_ampli : Long_float;
      nb_pic : integer;
      fonda, h_2, h_3 : integer := 0;

      function harmonique_present( ref_index, rang : integer ) return integer is
         p : integer;
      begin
         p := ref_index+1;
         while p <= nb_pic loop
            if abs( pics(ref_index).bin * rang - pics(p).bin) <= rang then
               return p;
            end if;
            p := p + 1;
         end loop;
         return 0;
      end harmonique_present;

      -- recherche de fondamentale basée sur la présence des harmoniques 2 et 3
      procedure cherche_fondamentale is
      begin
         fonda := 0;
         for i in 1..nb_pic loop
            -- si ce pic est la fondamentale alors on doit trouver des harmoniques dans les pics
            -- recherche harmonique 2 et 3
            h_2 := harmonique_present( i, 2 );
            h_3 := harmonique_present( i, 3 );
            if h_2 > 0 then
               if h_2 = i + 1 then
                  -- le pic suivant est h2
                  fonda := i;
                  return;
               elsif h_3 > 0 then
                  if h_3 = h_2 + 1 then
                     -- un pic s'est intercalé entre f0 et h2
                     fonda := i;
                     return;
                  end if;
               end if;
            elsif h_3 > 0 then
               -- pas d'harmonique 2 mais harmonique 3
               if h_3 = i+1 then
                  -- il manque juste h2
                  fonda := i;
                  return;
               end if;
            end if;
         end loop;
         -- par défaut fonda = 0
      end cherche_fondamentale;


      function autocorr( bin : integer; rang_h : integer ) return Long_float is
         min_periode, max_periode, best : integer;
         nb_mesures  : integer;
         mesure_time : integer;
         tmp : integer;
         local_max_ampli : Long_float;
         sum, ecart, min, max, optimum, discri, moyenne, freq : Long_float;
         valide : boolean;
         loop_count : natural := 0;
      begin
         -- evite CE en cas d'erreur sur le parametre 'bin'
         if bin < prem_bin then
            return 0.0;
         end if;
         -- l'autocorrelation doit se faire sur la période de la fondamentale
         -- periode min = période de la fréquence max, celle du bin au dessus
         min_periode := Integer( Long_float'Floor(Long_Float(N) / Long_Float(bin+1)) ) * rang_h;
         -- periode max = période de la fréquence min, celle du bin en dessous
         max_periode := Integer( Long_float'Ceiling(Long_Float(N) / Long_Float(bin-1)) ) * rang_h;
         -- si l'écart est trop faible, l'agrandir
         if max_periode - min_periode < 5 then
            tmp := (5 - max_periode + min_periode) / 2 + 1;
            min_periode := min_periode - tmp;
            max_periode := max_periode + tmp;
         end if;
         --
         while loop_count < 2 loop
           loop_count := loop_count + 1;	-- compteur pour éviter le bouclage infini
           --
            declare
               diff : array(min_periode..max_periode) of Long_Float;
            begin
               -- compteur du nombre de mesures réalisées dans la boucle
               nb_mesures := 0;
               moyenne := 0.0;
               --
               while dern_mesure+win_size+max_periode < data'last loop
                  -- temps de la mesure en ms
                  mesure_time := Integer( Long_Float'rounding( periode_echantillons * Long_Float(sample_data+dern_mesure) * 1000.0 ) /1000.0 );
                  -- calcul du max d'amplitude sur l'intervalle de mesure
                  local_max_ampli := 0.0;
                  for i in 0..win_size loop
                     if data(dern_mesure+i) > local_max_ampli then
                       local_max_ampli := data(dern_mesure+i);
                     end if;
                  end loop;
                  -- calcul pour chaque valeur de décalage
                  for I in min_periode..max_periode loop
                     -- calcul de la somme des carrés des différences pour le décalage i
                     sum := 0.0;
                     for T in dern_mesure..dern_mesure+win_size-1 loop
                        ecart := data(T) - data(T+i);
                        sum := sum + ecart * ecart;
                     end loop;
                     -- stockage du résultat pour le décalage i
                     diff(i) := sum;
                  end loop;
                  --
                  -- recherche du minimum de différence
                  min := diff(min_periode);
                  max := min;
                  best := min_periode;
                  for i in min_periode+1..max_periode loop
                     if diff(i) < min then
                        min := diff(i);
                        best := i;
                     end if;
                     if diff(i) > max then
                        max := diff(i);
                     end if;
                  end loop;
                  --
                  if min >= diff(min_periode) or min >= diff(max_periode) then
                     -- pas de minimum net dans cet intervalle
                     -- on essaie le double
                     min_periode := min_periode * 2;
                     max_periode := max_periode * 2;
                     exit;			-- recommence la boucle de calcul
                  else
                     valide := true;	-- flag pour sortir de la boucle de calcul
                  end if;
                  --
--                   if max / min > max_min_correlation then
                     -- mesure valide
                     nb_mesures := nb_mesures + 1;
                     -- periode de la fondamentale en nombre d'échantillons
                     optimum := Long_Float(best);
                     -- interpolation parabolique
                     if best > min_periode and best < max_periode then
                        discri := diff(best+1) + diff(best-1) - diff(best) - diff(best);
                        if discri > 0.0 then
                          optimum := optimum + ( diff(best-1) - diff(best+1) ) / ( 2.0 * discri );
                        end if;
                     end if;
                     --
                     -- calcule la fréquence à partir de la période
                     freq := Long_Float(Fs) / optimum * Long_float(h_rang);
                     --
                     -- sommation pour calcul de la moyenne
                     moyenne := moyenne + freq;
                     -- stocke le résultat pour affichage
                     Data_pkg.Store_Note( mesure_time, Short_Integer(To_cents(freq)), Short_integer(local_max_ampli) );
                     -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                     if Debug_pkg.Is_set( debug_flag_2 ) then
                        Log.End_line;
                        Log.Store( "->" & integer'image(integer(freq) ) );
                     end if;
                     -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--                    else
--                       -- stocke 0
--                       -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--                       if Debug_pkg.Is_set( debug_flag_2 ) then
--                          Log.End_line;
--                          Log.Store( "-> 0"  );
--                       end if;
--                       -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--                       Data_pkg.Store_Note( mesure_time, 0, 0 );
--                    end if;
                  --
                  -- position dans le buffer
                  dern_mesure := dern_mesure + win_size;
                  --
               end loop;
            end;
            exit when valide;
         end loop;
         --
         -- calcule la moyenne
         if nb_mesures > 0 then
            if rang_h = 1 then
               return moyenne / Long_float(nb_mesures);
            else
               return moyenne / Long_float(nb_mesures) * 2.0;
            end if;
         else
            return 0.0;
         end if;
      end autocorr;


      procedure Pick_tracking is
         tracked : boolean;
      begin
         if nb_pic = 0 then
            -- cas trivial: pas de pic !
            pitch := 0.0;
            pic_index := 0;
            h_rang := 0;
            --
         elsif nb_pic = 1 then
            -- il n'y a qu'un seul pic, c'est lui qu'on choisit
            pitch := pics(1).freq;
            pic_index := 1;
            h_rang := 1;	-- fondamentale
            --
         else
            -- plusieurs pics, il faut choisir
            if track_bin > 0 then
               -- il existe une mesure juste avant,
               -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               if Debug_pkg.Is_set( debug_flag_2 ) then
                  Log.Store( "Prev bin=" & integer'image(track_bin) );
               end if;
               -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               -- il faut assurer la continuité
               tracked := false;
               for i in 1..nb_pic loop
                  if abs(pics(i).bin - track_bin) <= 2 then	-- un pic à + ou - 2 bin de distance
                     tracked := true;
                     pic_index := i;
                     exit;
                  end if;
               end loop;
               if tracked then
                  -- le pic n'a pas beaucoup changé, on le garde
                  pitch := pics(pic_index).freq;
                  track_bin := pics(pic_index).bin;
                  -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  if Debug_pkg.Is_set( debug_flag_2 ) then
                     Log.Store( "New bin=" & integer'image(track_bin) );
                     Log.Store( "Rang=" & integer'image(h_rang) );
                  end if;
                  -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  -- terminé, le reste ne change pas
                  return;
               end if;
            end if;
            --
            -- première mesure ou pic perdu
            ---
            -- recherche de la fondamentale
            cherche_fondamentale;
            --
            -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            if Debug_pkg.Is_set( debug_flag_2 ) then
               if fonda > 0 then
                  Log.Store( "F=" & integer'image(fonda) );
                  if h_2 > 0 then
                     Log.Store( "H2=" & integer'image(h_2) );
                  end if;
                  if h_3 > 0 then
                     Log.Store( "H3=" & integer'image(h_3) );
                  end if;
               else
                  Log.Store( "Pas de fonda");
               end if;
            end if;
            -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            --
            if fonda > 0 then
              -- la fondamentale a été trouvée, est-ce le pitch ?
               if h_2 > 0 and then pics(h_2).amp > pics(fonda).amp * 5.0 then
                  -- non h2 est prédominant
                  pitch := pics(h_2).freq;
                  track_bin := pics(h_2).bin;
                  pic_index := h_2;
                  h_rang := 2;		-- harmonique 2
                  -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
                  if Debug_pkg.Is_set( debug_flag_2 ) then
                     Log.Store( "** H2 **");
                  end if;
                  -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               else
                 -- ok, pitch = fondamentale par défaut
                 h_rang := 1;
                 if track_bin > 0 then
                    -- teste pour perte de fondamentale
                    if abs( pics(fonda).bin - 2* track_bin) <= 1 then
                       h_rang := 2;
                    end if;
                 end if;
                 pitch := pics(fonda).freq;
                 track_bin := pics(fonda).bin;
                 pic_index := fonda;
               end if;
            else
               -- pas de fondamentale !
               -- on prend le premier pic
               pitch := pics(1).freq;
               track_bin := pics(1).bin;
               pic_index := 1;
               -- test si le pic suivant est l'harmonique 3 d'une fondamentale manquante
               -- dont le premier pic serait l'harmonique 2
               if nb_pic >= 2 and then egal_a_peu_pres( pitch / 2.0, pics(2).freq / 3.0 ) then
                  -- la fondamentale est manquante mais le picth est h2
                  h_rang := 2;
               else
                  -- fondamentale par défaut
                  h_rang := 1;
               end if;
               -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               if Debug_pkg.Is_set( debug_flag_2 ) then
                  Log.Store( "1er pic=" & integer'image(track_bin) );
                  Log.Store( "Rang=" & integer'image(h_rang) );
               end if;
               -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            end if;
            --
         end if;
         --
      end Pick_tracking;


   begin	-- Compute_Pitch
      --
      -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      if Debug_pkg.Is_set( debug_flag_2 ) then
         Log.End_line;
      end if;
      -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      -- max d'amplitude du spectre des données
      max_ampli := 0.0;
      for i in 1..N/2-1 loop
         if ampli(i) > max_ampli then
            max_ampli := ampli(i);
         end if;
      end loop;
      --
      -- recherche tous les pics dans l'intervalle prem_bin..dern_bin (60-1200 Hz)
      -- recherche des pics
      nb_pic := 0;
      for i in prem_bin..dern_bin loop
         if ampli(i) > max_ampli / 10.0 -- ignore les pics trop petits (-20dB)
            and then ampli(i) >= ampli(i-1) and then ampli(i) >= ampli(i+1)
         then
            -- un pic de plus
            nb_pic := nb_pic + 1;
            -- on calcule sa fréquence approximative
            pics(nb_pic).freq := Long_float(i *Common_types.Fs ) / Long_float(N);
            -- et on stocke son amplitude
            pics(nb_pic).amp := ampli(i);
            -- et son bin pour affichage
            pics(nb_pic).bin := i;
            -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            if Debug_pkg.Is_set( debug_flag_2 ) then
               Log.Store( integer'image(i) & ":" & Integer'image(Integer(pics(nb_pic).freq)) );
            end if;
            -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         end if;
         exit when nb_pic >= max_nb_pics;
      end loop;
      -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      if Debug_pkg.Is_set( debug_flag ) then
         if nb_pic = 0 then
            Log.Store( "Pas de pics" );
            Log.End_line;
         end if;
      end if;
         -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      --
      -- recherche du pic correspondant au pitch
      Pick_tracking;
      --
      if pitch > 0.0 then
         -- calcule de la fréquence précise par 'autocorrélation'
         pitch := autocorr( track_bin, h_rang );
      end if;
      --
      --
      -- teste si fin de démo
      if not Config_pkg.Is_full then
         if current_time > max_duree_demo then
            raise end_demo;
         end if;
      end if;
      --
      return pitch;
      --
   end Compute_Pitch;


   procedure Mesure_nulle is
      mesure_time : integer;
   begin
      while dern_mesure + win_size < data'last loop
         -- temps de la mesure
         mesure_time := Integer( Long_Float'rounding( periode_echantillons * Long_Float(sample_data+dern_mesure) * 1000.0 ) /1000.0 );
         -- stocke 0,0
         Data_pkg.Store_note( mesure_time, 0, 0 );
         -- DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         if Debug_pkg.Is_set( debug_flag_2 ) then
            Log.End_line;
            Log.Store( "-> 0");
         end if;
         -- END DEBUG %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
         -- incrémente pointeurs
         dern_mesure := dern_mesure + win_size;
      end loop;
   end Mesure_nulle;


   procedure Traite_data( lParam  : Win32.LPARAM ) is
      -- appellée par le device quand un buffer de donnée est prêt
      -- lparam = @du header du buffer
      header       : head_pt := TO_HEADPT( lParam );
      buff         : short_array(1..Common_types.buff_size);
      for buff use at TO_ADDRESS( header.lpData );
      nb_bytes     : constant integer := TO_INTEGER( header.dwBytesRecorded );
      nb_values    : constant integer := nb_bytes / 2;	-- short_integer = 2 bytes
      Result       : Win32.Mmsystem.MMRESULT;
      max          : Short_integer;
      index, j     : Integer := 0;
      val          : Short_integer;
      max_spectre  : Long_float := 0.0;
      cents        : Long_float;
      --
      Pitch        : Long_float;
      Midi_Note    : Long_Float := 0.0;
      Int_Note,
      Note,
      Octave       : Integer := 0;
      --
   begin
      -- calcul du temps courant
      begin
         sample_data    := sample_data + nb_values;
         sample_courant := sample_courant + nb_values;
      exception
         when Constraint_Error =>	-- peut arriver après 13h30 de fonctionnement continu à 44.1KHz
            Data_pkg.Clear_all;
            sample_data := 0;
            sample_courant := data'length;
      end;
      current_time := Integer( Long_Float'rounding( periode_echantillons * Long_Float(sample_courant) * 1000.0 ) /1000.0 );
      --
      -- en cas de redémarrage, oublie le 1er buffer recu
      if Common_types.restarting or freezed then
         -- reset du flag
         Common_types.restarting := false;
         -- redonne le buffer au device
         Result := waveInAddBuffer( Win_Audio.micro, TO_LPWAVEHDR(header), Win32.UINT(WAVEHDR'size/8) );
         Utils_pkg.Check_erreur( Result, "Traite_data: waveInAddBuffer-1" );
         return;
      end if;
      --
      -- calcul du max pour affichage dans indicateur
      max := 0;
      --
      for i in 1..nb_values loop
	 -- lecture valeur absolue
         if buff(i) /= Short_Integer'First then
	    val := abs( buff(i) );
         else
            val := Short_Integer'Last;
         end if;
	 -- calcul du maximum
	 if val > max then
	    max := val;			-- valeur du max
         end if;
         --
      end loop;
      --
      -- sauvergarde sur disque
      if sauvegarde then
         Save_wave_file( header.lpData, header.dwBytesRecorded );
      end if;
      --
      -- copie des data dans le buffer d'entrée
      data(0..data'length-nb_values-1) := data(nb_values..data'length-1);	-- translate ancienne données
      j := data'length-nb_values;
      for i in 1..nb_values loop
         data(j) := Long_Float( buff(i) );	-- copies et transforme en float les nouvelles valeurs
         j := j + 1;
      end loop;
      -- mise à jour du pointeur sur la dernièere mesure réalisée
      dern_mesure := dern_mesure - nb_values;
      --
      -- le buffer n'est plus utile, on le redonne au device
      Result := waveInAddBuffer( Win_Audio.micro, TO_LPWAVEHDR(header), Win32.UINT(WAVEHDR'size/8) );
      Utils_pkg.Check_erreur( Result, "Traite_data: waveInAddBuffer-2" );
      --
      if max > seuil_pitch then
         -- calcul de la FFT pour avoir le spectre du signal
         -- et rechercher la fondamentale et les harmoniques
         if dern_mesure < data'last then
            Do_FFT( data, dern_mesure, ampli );
         else
            Do_FFT( data, data'last-nb_values, ampli );
         end if;
         --
         -- extrait la partie utile : arguments des bins entre 60 et 1000 Hz
         -- pour affichage dans fenêtre principale
         j := spectre'first;
         for i in prem_bin..dern_bin loop
            spectre(j) := ampli(i);
            if spectre(j) > max_spectre then
               max_spectre := spectre(j);
            end if;
            j := j + 1;
         end loop;
         -- normalisation
         for i in spectre'range loop
            spectre(i) := spectre(i) / max_spectre * Long_Float(max)/32767.0;
         end loop;
         --
         -- calcule le pitch
         pitch := Compute_pitch;
         --
         if pitch > 60.0 then
            -- transforme en cents
            cents := To_cents( pitch );
            -- converti la fréquence en note MIDI et Note-Octave
            -- référence : La 3 = 440 Hz = 69 MIDI, 12 notes par octave également espacées
            Midi_Note := cents /100.0;
            Int_Note := Integer(Long_Float'Rounding(Midi_Note)); -- partie entière
            Note := Int_Note mod 12;
            Octave := (Int_Note - Note) / 12 - 2;	-- La 3 => 69 => 60/12 = 5 => faire -2
         else
            pitch := 0.0;
            Mesure_nulle;
         end if;
      else
         -- niveau insuffisant pour les mesures
         pitch := 0.0;
         track_bin := 0;
         spectre := (others => 0.0);
         Mesure_nulle;
      end if;
      --
      if pitch > 0.0 then
         -- calcule l'index dans le spectre de la position du pitch
         Index := track_bin - prem_bin + 1;
      end if;
      --
      -- affichage des résultats
      User_interface.Display_all( Max, Pitch, Note, Octave, Index );
      --
   end Traite_data;


   -- ===============================================================================

   function Data_Proc( hwnd    : Win32.Windef.HWND;
                        message : Win32.UINT;
                        wParam  : Win32.WPARAM;
			lParam  : Win32.LPARAM)
			        return Win32.LRESULT;
      pragma Convention (Stdcall, Data_Proc);

   function Data_Proc( hwnd    : Win32.Windef.HWND;
                        message : Win32.UINT;
                        wParam  : Win32.WPARAM;
			lParam  : Win32.LPARAM)
			        return Win32.LRESULT is
      res_bool : Win32.BOOL;
   begin

      case message is

	 when Win32.Mmsystem.MM_WIM_DATA =>
	    -- un buffer est prêt à être utilisé
	    Traite_data( lParam );

         -- autres
         when others =>
           return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);

      end case;

      return 0;
   exception
      when end_demo =>
         if not stopping then
            stopping := true;
            Win_audio.Stop_lecture;
            Utils_pkg.Error_box( Intl.dlg_demo_title, Intl.demo_text );
            res_bool := Win32.Winuser.PostMessage( Common_types.Win_hwnd, Win32.Winuser.WM_DESTROY, 0, 0 );
         end if;
         return 0;

      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
         return 0;
   end Data_Proc;

   -- ============================================================================


   procedure Create_Data_Win is
   begin
      -- creation de l'objet windows
      data_hwnd := Win32.Winuser.CreateWindow(
        lpClassName  => Conversions.TO_PCCH(DataCLASS'address),
        lpWindowName => Conversions.TO_PCCH(System.Null_Address),
        dwStyle      => Win32.Winuser.WS_CHILDWINDOW,
        X            => 0,
        Y            => 0,
        nWidth       => 0,
        nHeight      => 0,
        hWndParent   => Common_types.Win_hwnd,		-- handle de la fenetre principale
        hMenu        => TO_HWND(2),			-- identifier
        hInstance    => Common_types.hInst,
        lpParam      => System.Null_Address);
      --
      if data_hwnd = System.Null_address then
         Utils_pkg.Raise_fatal_error( Resources_pkg.Null_id, "Process_pkg.Create_Data_Win: CreateWindow");
      end if;
   end Create_Data_Win;


   -- ===================================================================================

   -- Création des classe pour la fenêtre data
   procedure Create_Data_Class is
      Data_Class : Win32.Winuser.WNDCLASS;
      res_atom  : Win32.Windef.ATOM;
   begin
      --
      -- Création de la Class pour les Data
      --
      Data_Class.lpfnWndProc   := Data_proc'access;
      Data_Class.hIcon         := System.Null_Address;
      Data_Class.lpszClassName := Conversions.TO_PCCH(DataCLASS'address);
      Data_Class.style         := Win32.Winuser.CS_HREDRAW or Win32.Winuser.CS_VREDRAW;
      Data_Class.cbClsExtra    := 0;
      Data_Class.cbWndExtra    := 0;
      Data_Class.hInstance     := Common_types.hInst;
      Data_Class.hCursor       := System.Null_Address;
      Data_Class.hbrBackground := System.Null_Address;
      --
      -- enregistrement de la classe
      res_atom := Win32.Winuser.RegisterClass( TO_LPWNDCLASS(Data_Class'address) ) ;
      --
      if res_atom = 0 then
         -- erreur !
         Utils_pkg.Raise_fatal_error( Resources_pkg.Null_id, "Process_pkg.Create_Data_Class: RegisterClass");
      end if;
      --
   end Create_Data_Class;


   -- ===================================================================================

   procedure Init_Data is
   begin
      -- temps d'un échantillon en millisecondes
      periode_echantillons := 1000.0 / Long_Float(Common_types.Fs);
      --  taille de la fenetre de mesure en nombre d'échantillons
      win_size := integer( Long_Float(Fs) * win_time);
      -- tables utilisées la FFT et le buffer de données
      FFT_create_tables(Common_types.Fs);
      -- nombre de samples traités utilisé pour mesurer le temps
      sample_courant := data'length;	-- on simile le remplissage du buffer pour éviter les temps négatifs
      sample_data := 0;
      -- index de la dernière mesure
      dern_mesure := data'last;
      track_bin := 0;
   end Init_Data;


end Process_pkg;

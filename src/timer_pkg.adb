with Interfaces.C;	use Interfaces.C;
with GNAT.Current_Exception;

with Win32;		use Win32;
with Win32.Mmsystem;	use Win32.Mmsystem;
with Win32.Winuser;

with Conversions;	use Conversions;
with Common_types;	use Common_types;
with Midi_pkg;
with Utils_pkg;
with Intl;
with Log;

package body Timer_pkg is

   -- nombre de ticks avant de mettre àjour l'affichage
   -- 10 x 10 = 100 ms
   appel_score : constant := 10;


   -- constant manquante dans win32-mmsystem.ads
   TIME_KILL_SYNCHRONOUS : constant := 16#0100#;
   -- identifiant du timer
   timer_id : Win32.UINT;

   -- compteur pour l'appel de l'affichage
   counter : integer := 0;


  -- ===========================================================================


   type LPCALLBACK is access procedure( uID    : Win32.UINT;
                        uMsg   : Win32.UINT;
                        dwUser : Win32.DWORD;
                        dw1    : Win32.DWORD;
                        dw2    : Win32.DWORD );
   pragma Convention (Stdcall, LPCALLBACK);

   function timeSetEvent
     (uDelay : Win32.UINT;
      uResolution : Win32.UINT;
      lpFunction : LPCALLBACK;
      dwUser : Win32.DWORD;
      uFlags : Win32.UINT)
     return Win32.UINT;
   pragma Import (Stdcall, timeSetEvent, "timeSetEvent");


   -- ***********************************************************************


   procedure Next_tick( uID    : Win32.UINT;
                        uMsg   : Win32.UINT;
                        dwUser : Win32.DWORD;
                        dw1    : Win32.DWORD;
                        dw2    : Win32.DWORD );
   pragma Convention (Stdcall, Next_tick);


   procedure Next_tick( uID    : Win32.UINT;
                        uMsg   : Win32.UINT;
                        dwUser : Win32.DWORD;
                        dw1    : Win32.DWORD;
                        dw2    : Win32.DWORD ) is
      res_bool : Win32.BOOL;
   begin
      --
      -- appel le player Midi
      Midi_pkg.Do_Next_Tick;
      --
      -- compte le nombre d'appels (supposés être régulièrement toutes les 10 ms)
      counter := counter + 1;
      if counter = appel_score then
         counter := 0;
         -- envoie un message pour mettre à jour l'affichage
         res_bool := Win32.Winuser.PostMessage( Common_types.Win_hwnd, Win32.Winuser.WM_TIMER, 0, 0 );
      end if;
   exception
      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
         Log.End_Log;
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_exception );
         -- fin du programme par Windows
	 Win32.Winuser.PostQuitMessage (0);
   end Next_tick;


   procedure Start is
      -- résolution du timer
      resolution : Win32.UINT;
      result : Win32.Mmsystem.MMRESULT;
      tcaps : Win32.Mmsystem.TIMECAPS;
   begin
      -- défaut : résolution à 1 ms
      resolution := 1;
      -- lecture capacité hardware
      result := Win32.Mmsystem.timeGetDevCaps( TO_LPTIMECAPS( tcaps'address ),
                                                Win32.UINT( tcaps'size / 8 ) );
      if result = Win32.Mmsystem.TIMERR_NOERROR then
         -- si résultat valide, on prend le max
         if resolution < tcaps.wPeriodMin then
             resolution := tcaps.wPeriodMin;
         end if;
      end if;
      -- démarrage du timer
      timer_id := timeSetEvent( Common_types.timer_delay,
                                resolution,
                                Next_tick'access,
                                0,
                                Win32.Mmsystem.TIME_PERIODIC + TIME_KILL_SYNCHRONOUS );
      if timer_id = 0 then
         -- erreur
         Utils_pkg.Error_box( Intl.err_play_title, Intl.err_timer_txt);
         raise fatal_error;
      end if;
   end Start;


   procedure Stop is
      result : Win32.Mmsystem.MMRESULT;
   begin
      -- on arrête le timer
      result := Win32.Mmsystem.timeKillEvent( timer_id );
   end Stop;

end Timer_pkg;

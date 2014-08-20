with System;	use System;
with Interfaces.C;	use Interfaces.C;
with GNAT.Current_Exception;

with Win32;		use Win32;
with Win32.Winmain;
with Win32.Winuser;	use Win32.Winuser;
with Win32.Winnt;
with Win32.Winbase;

-- package de traitement
with Common_types;
with User_interface;
with Win_audio;
with Registre_pkg;
with Dialog_pkg;
with LOG;
with Utils_pkg;
with Intl;
with Bitmap_pkg;
with Data_pkg;
with Debug_pkg;
with Config_pkg;
with Skins_pkg;
with Resources_pkg;

procedure Canta is
   Result      : Win32.BOOL;
   Long_Result : Win32.LRESULT;
   msg         : Win32.Winuser.LPMSG := new Win32.Winuser.MSG;
   process     : Win32.Winnt.HANDLE;
   res_bool    : Win32.BOOL;
   init_param : constant string := "init";
begin
   Log.Start_Log;
   Debug_pkg.Init_Debug;

   -- Lecture de l'instance d'application
   Common_types.hInst := Win32.Winmain.Get_hInstance;
   -- handle du process
   Process := Win32.Winbase.GetCurrentProcess;
   -- priorité du process = élevé
   res_bool := Win32.Winbase.SetPriorityClass( process, Win32.Winbase.HIGH_PRIORITY_CLASS );
--   res_bool := Win32.Winbase.SetPriorityClass( process, Win32.Winbase.REALTIME_PRIORITY_CLASS );
   if res_bool = 0 then
      Utils_pkg.Raise_fatal_error( Resources_pkg.Null_id, "SetPriorityClass" );
   end if;

   -- création des bitmaps: doivent être créés avant les windows
   if Skins_pkg.Load_skin( Registre_pkg.Get_current_skin ) then
      if Debug_pkg.Is_set(init_param) then
         Log.Store("Load_skin: OK");Log.End_line;
      end if;
   else
      if Debug_pkg.Is_set(init_param) then
         Log.Store("Load_skin: Error, loading default skin");Log.End_line;
      end if;
      --
      Skins_pkg.Default_skin;
   end if;
   --
   -- Pré-allocation mémoire pour le stockage des évènements
   Data_pkg.Init;
   if Debug_pkg.Is_set(init_param) then
      Log.Store("Data_pkg.Init: OK");Log.End_line;
   end if;

   -- si nouvelle instance, initialise l'instance
   if Win32.Winmain.Get_hPrevInstance = System.Null_Address then
      User_interface.Init_Instance;
   end if;

   -- initialisation de l'application

   -- interface utilisateur
   User_Interface.Init_UI;
   if Debug_pkg.Is_set(init_param) then
      Log.Store("Init_UI: OK");Log.End_line;
   end if;

   -- lecture de la config dans la base de registre
   if not Registre_pkg.Read_config or else not Win_audio.Check_config then
      -- pas de config ou config pas bonne, on utilise une dialog box
      Dialog_pkg.Select_all;
      if Debug_pkg.Is_set(init_param) then
         Log.Store("Dialog_select: OK");Log.End_line;
      end if;
      -- on stocke la config dans la base de registre
      Registre_pkg.Store_config;
      if Debug_pkg.Is_set(init_param) then
         Log.Store("Store_config: OK");Log.End_line;
      end if;
   else
      if Debug_pkg.Is_set(init_param) then
         Log.Store("Read+Check_Config: OK");Log.End_line;
      end if;
   end if;
   --
   if Common_types.no_midi_device then
      Log.Store( "No MIDI device, invalidating player");Log.End_line;
      User_Interface.Invalidate_player;
   end if;

   -- Démarre la lecture et l'affichage
   User_Interface.Start_appli;
   if Debug_pkg.Is_set(init_param) then
      Log.Store("Start_appli: OK");Log.End_line;
   end if;

   -- Boucle principale: Get-Translate-Dispatch
   while (Win32.Winuser.GetMessage (msg, System.Null_Address, 0, 0) /= Win32.FALSE) loop
      Result := Win32.Winuser.TranslateMessage (Win32.Winuser.ac_MSG_t (msg));
      Long_Result := Win32.Winuser.DispatchMessage (Win32.Winuser.ac_MSG_t (msg));
   end loop;

   -- arrêt timer et interruptions
   User_Interface.Stop_appli;
   if Debug_pkg.Is_set(init_param) then
      Log.Store("Stop_appli: OK");Log.End_line;
   end if;

   Log.End_Log;

exception
   when others =>
      Log.Error( GNAT.Current_Exception.Exception_Information );
      Utils_pkg.Error_box( Intl.def_err_title, Intl.err_exception);
      Log.End_Log;
end Canta;

with System;
with Interfaces.C;	use Interfaces.C;

with Win32;		use Win32;
with Win32.Winreg;
with Win32.Winnt;

with Common_types;
with Conversions;	use Conversions;
with Config_pkg;
with Log;
with Debug_pkg;
with Utils_pkg;

package body Registre_pkg is

   -- param de debug
   debug_param : constant string := "config";

   -- clé de base dans HKEY_LOCAL_MACHINE
   KeyName      : constant String := "SOFTWARE\\Chaumet\\" & Config_pkg.Appli_name & ASCII.Nul;

   Micro_num_substr : constant String := "MicroNum" & ASCII.Nul;
   Micro_str_substr : constant String := "MicroStr" & ASCII.Nul;
   Midi_num_substr  : constant String := "MidiNum" & ASCII.Nul;
   Midi_str_substr  : constant String := "MidiStr" & ASCII.Nul;
   Fs_substr        : constant String := "Freq" & ASCII.Nul;
   Midi_dir_str     : constant String := "MidiDir" & ASCII.Nul;
   Save_dir_str     : constant String := "SaveDir" & ASCII.Nul;
   Last_demo_start  : constant String := "Claque" & ASCII.Nul;
   Last_demo_duree  : constant String := "Raclee" & ASCII.Nul;
   Current_skin_name: constant String := "Skin" & ascii.nul;
   Exe_dir_name     : constant String := "Exedir" & ascii.nul;

   -- ==============================================================================


   function read_int(Key  : Win32.Winreg.HKEY ;
                     Name : String ) return Win32.INT is
      ValueType : unsigned_long;
      len : unsigned_long := 4;
      resu : Win32.LONG;
      --
      tmp : Win32.INT;
      buffer : string(1..4) := (others => ascii.nul );
      for buffer use at tmp'address;
   begin
      resu := Win32.Winreg.RegQueryValueEx( Key,
                               TO_PCHAR(Name(name'first)'address),
                               null,
                               Win32.To_PULong(ValueType'address),
                               Win32.To_PByte(Buffer'address),
                               Win32.To_PULong(len'address) );
      -- test résultat
      if resu /= 0 then
         Utils_pkg.Log_Windows_Error( "Registre_pkg.Read_int: RegQueryValueEx '" & name & "'" );
         raise Reg_error;
      elsif ValueType /= Win32.Winnt.REG_DWORD then
         -- la valeur n'existe pas ou n'est pas du bon type
         Log.Store( "Registre_pkg.Read_int: valeur pas du bon type pour '" & name & "'" );
         raise Reg_error;
      else
         return tmp;
      end if;
   end read_int;


   procedure Write_int(Key    : Win32.Winreg.HKEY;
                       Name   : String;
                       object : Win32.UINT ) is
      resu : Win32.LONG;
      --
      tmp : Win32.UINT := object;
      buffer : string(1..4);
      for buffer use at tmp'address;
   begin
      -- écriture de la valeur
      resu := Win32.Winreg.RegSetValueEx( Key,
                             TO_PCCH(Name(Name'first)'address),
                             0,
                             Win32.Winnt.REG_DWORD,
                             TO_PCBYTE(Buffer'address),
                             4 );
      if resu /= 0 then
         -- erreur d'écriture
         Utils_pkg.Log_Windows_Error( "Registre_pkg.Write_int: RegSetValueEx '"
                                      & name & "':'" & Win32.Uint'image(object) & "'");
         raise Reg_error;
      end if;
   end Write_int;

   -- ---------------------------------------------------------------------------------

   function Read_Str(Key  : Win32.Winreg.HKEY ;
                     Name : String ) return string is
      ValueType : unsigned_long;
      resu : Win32.LONG;
      --
      buffer : string(1..4096) := (others => ascii.nul );
      len : unsigned_long := unsigned_long( buffer'length );
   begin
      resu := Win32.Winreg.RegQueryValueEx( Key,
                               TO_PCHAR(Name(name'first)'address),
                               null,
                               Win32.To_PULong(ValueType'address),
                               Win32.To_PByte(Buffer'address),
                               Win32.To_PULong(len'address) );
      -- test résultat
      if resu /= 0 then
         Utils_pkg.Log_Windows_Error( "Registre_pkg.Read_str: RegQueryValueEx resu=" & Win32.LONG'image(resu) & " pour '" & name & "'" );
         return "";
      elsif ValueType /= Win32.Winnt.REG_SZ then
         -- la valeur n'existe pas ou n'est pas du bon type
         Log.Store( "Registre_pkg.Read_str: valeur pas du bon type pour '" & name & "'" );
         return "";
      else
         return buffer(1..Integer(len-1));	-- NB: le -1 est à cause du nul final !!
      end if;
   end Read_Str;


   procedure Write_Str(Key    : Win32.Winreg.HKEY;
                       Name   : String;
                       object : string ) is
      resu : Win32.LONG;
      --
      buffer : string(1..object'length+1);
   begin
      -- copie dans le buffer et ajoute le nul final
      buffer(1..object'length):= object;
      buffer(object'length+1) := ascii.nul;
      -- écriture de la valeur
      resu := Win32.Winreg.RegSetValueEx( Key,
                             TO_PCCH(Name(Name'first)'address),
                             0,
                             Win32.Winnt.REG_SZ,
                             TO_PCBYTE(Buffer'address),
                             buffer'length );
      if resu /= 0 then
         -- erreur d'écriture
         Utils_pkg.Log_Windows_Error( "Registre_pkg.Write_str: RegSetValueEx '"
                                     & name & "':'"  & object & "'" );
         raise Reg_error;
      end if;
   end Write_Str;


   -- ===============================================================================


   procedure Store_config is
      resu : Win32.LONG;
      Key  : Win32.Winreg.HKEY;
   begin
      -- ouverture ou création de la clé de régistre
      resu := Win32.Winreg.RegCreateKeyEx( Win32.Winreg.HKEY_LOCAL_MACHINE,
                              TO_PCCH(KeyName(KeyName'first)'address),
                              0,
                              null,
                              Win32.Winnt.REG_OPTION_NON_VOLATILE,
                              Win32.Winnt.KEY_WRITE,
                              null,
                              To_PHKEY(Key'Address),
                              Win32.To_PULong(System.Null_address) );
       if resu /= 0 then
          Utils_pkg.Log_Windows_Error( "Registre_pkg.Store_config: RegCreateKeyEx" );
       end if;
       -- enregistrement des valeurs
       Write_Int(Key, Micro_num_substr, Common_types.micro_num );
       --
       Write_Str(Key, Micro_str_substr, Common_types.micro_str.all );
       --
       Write_Int(Key, Midi_num_substr, Common_types.midi_num );
       --
       Write_Str(Key, Midi_str_substr, Common_types.midi_str.all );
       --
       Write_Int(Key, Fs_substr, Win32.UINT(Common_types.Fs) );
       --
       if Debug_pkg.Is_Set( debug_param ) then
          Log.Separ_line;
          Log.Store("Microphone:");
          Log.Store(Integer(Common_types.micro_num));
          Log.Store(Common_types.micro_str.all);
          Log.End_line;
          Log.Store("Midi:");
          Log.Store(Integer(Common_types.midi_num));
          Log.Store(Common_types.midi_str.all);
          Log.End_line;
          Log.Store("Store_config: OK");Log.End_line;
          Log.Separ_line;
       end if;
   end Store_config;


   function Read_config return boolean is
      Key  : Win32.Winreg.HKEY;
      resu : Win32.LONG;
   begin
      -- ouverture de la clé de régistre
      for i in 1..3 loop
         resu := Win32.Winreg.RegOpenKeyEx(
                            Win32.Winreg.HKEY_LOCAL_MACHINE,
                            TO_PCCH(KeyName(KeyName'first)'address),
                            0,
                            Win32.Winnt.KEY_ALL_ACCESS,
                            To_PHKEY(Key'Address)   );
         exit when resu = 0;	-- si OK, sortie immédiate
         delay 0.1;	-- sinon on attend un peu avant de recommencer
      end loop;
      if resu /= 0 then
         Utils_pkg.Log_Windows_Error( "Registre_pkg.Read_config: RegOpenKeyEx" );
         return false;
      end if;
      -- la clé existe, lire les valeurs stockées
      begin
         if Debug_pkg.Is_Set( debug_param ) then
            Log.Separ_line;
            Log.Store("Read_config:");Log.End_line;
         end if;
         Common_types.micro_num := Win32.UINT( Read_Int(Key, Micro_num_substr) );
         --
         Common_types.Free( Common_types.micro_str );
         Common_types.micro_str := new string'( Read_str(key, Micro_str_substr ) );
         if Debug_pkg.Is_Set( debug_param ) then
            Log.Store("Microphone:");
            Log.Store(Integer(Common_types.micro_num));
            Log.Store(Common_types.micro_str.all);
            Log.End_line;
         end if;
         --
         Common_types.Fs        := Integer( Read_Int(Key, Fs_substr) );
         if Debug_pkg.Is_Set( debug_param ) then
             Log.Store("Fs:");
             Log.Store(Common_types.Fs);
             Log.End_line;
         end if;
         --
         Common_types.midi_num := Win32.UINT( Read_Int(Key, Midi_num_substr) );
         --
         Common_types.Free( Common_types.midi_str );
         Common_types.midi_str := new string'( Read_Str(Key, Midi_str_substr ) );
         if Debug_pkg.Is_Set( debug_param ) then
             Log.Store("Midi:");
             Log.Store(Integer(Common_types.midi_num));
             Log.Store(Common_types.midi_str.all);
             Log.End_line;
             Log.Separ_line;
         end if;
      exception
         when Reg_error =>
            -- la config n'est pas valable
            if Debug_pkg.Is_Set( debug_param ) then
               Log.Error("Config invalide");
               Log.Separ_line;
            end if;
            return false;
      end;
      --
      -- tout s'est bien passé
      return true;
   end Read_config;


   -- ================================================================================

   function Get_String( subkey : string ) return string is
      Key  : Win32.Winreg.HKEY;
      resu : Win32.LONG;
   begin
      -- ouverture de la clé de régistre
      for i in 1..3 loop
         resu := Win32.Winreg.RegOpenKeyEx(
                            Win32.Winreg.HKEY_LOCAL_MACHINE,
                            TO_PCCH(KeyName(KeyName'first)'address),
                            0,
                            Win32.Winnt.KEY_ALL_ACCESS,
                            To_PHKEY(Key'Address)   );
         exit when resu = 0;	-- si OK, sortie immédiate
         delay 0.1;	-- sinon on attend un peu avant de recommencer
      end loop;
      if resu /= 0 then
         Utils_pkg.Log_Windows_Error( "Registre_pkg.Get_String: RegOpenKeyEx resu="
                 & Win32.LONG'image(resu) & " pour '" & subkey & "'" );
         return "";
      end if;
      -- la clé existe déjà, lire les valeurs stockées
      return Read_Str(Key, subkey );
   exception
      when Reg_error =>
         -- la config n'est pas valable
         Utils_pkg.Log_Windows_Error( "Registre_pkg.Get_String: Reg_error pour '" & subkey & "'" );
         return "";
   end Get_String;

   procedure Set_String( subkey : string; value : string ) is
      Key  : Win32.Winreg.HKEY;
      resu : Win32.LONG;
   begin
      -- ouverture ou création de la clé de régistre
      resu := Win32.Winreg.RegCreateKeyEx( Win32.Winreg.HKEY_LOCAL_MACHINE,
                              TO_PCCH(KeyName(KeyName'first)'address),
                              0,
                              null,
                              Win32.Winnt.REG_OPTION_NON_VOLATILE,
                              Win32.Winnt.KEY_WRITE,
                              null,
                              To_PHKEY(Key'Address),
                              Win32.To_PULong(System.Null_address) );
       if resu /= 0 then
          Utils_pkg.Log_Windows_Error( "Registre_pkg.Set_String: RegCreateKeyEx '"
                                        & subkey & "':'" & value & "'");
       else
          -- enregistrement des valeurs
          Write_Str(Key, subkey, value );
       end if;
   end Set_String;


   -- ================================================================================


   function Get_last_midi_dir return string is
   begin
      return Get_String( Midi_dir_str );
   end Get_last_midi_dir;


   procedure Set_last_midi_dir( dir : string ) is
   begin
       Set_String( Midi_dir_str, dir );
   end Set_last_midi_dir;

   function Get_last_save_dir return string is
   begin
      return Get_String( Save_dir_str);
   end Get_last_save_dir;

   procedure Set_last_save_dir( dir : string ) is
   begin
       Set_String( Save_dir_str, dir );
   end Set_last_save_dir;

   function Get_last_start return string is
   begin
      return Get_String( Last_demo_start );
   end Get_last_start;

   procedure Set_last_start( time : string ) is
   begin
      Set_String( Last_demo_start, time );
   end Set_last_start;

   function Get_last_duree return string is
   begin
      return Get_String( Last_demo_duree );
   end Get_last_duree;

   procedure Set_last_duree( duree : string ) is
   begin
      Set_String( Last_demo_duree, duree );
   end Set_last_duree;


   function Get_current_skin return string is
   begin
      return Get_String( Current_skin_name );
   end Get_current_skin;

   procedure Set_current_skin( skin : string ) is
   begin
      Set_String( Current_skin_name, skin );
   end Set_current_skin;

   function Get_exe_dir return string is
   begin
      return Get_String( Exe_dir_name );
   end Get_exe_dir;

end Registre_pkg;

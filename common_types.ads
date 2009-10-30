with System;

with Unchecked_deallocation;
with Unchecked_Conversion;

with Win32;
with Win32.Mmsystem;
with Win32.Windef;
with Win32.Winuser;
with Win32.Wingdi;

with Config_pkg;

package Common_types is

   type string_pt is access all string;
   -- ----------------------------------------------------------------------------------------
   copyright : constant String :=  "Copyright Chaumet Software";	-- ne jamais changer !!
   -- ----------------------------------------------------------------------------------------

   N_A : System.Address renames System.Null_address;


   -- intervalle Midi maximum affiché par le score
   min_midi : constant := 38;
   max_midi : constant := 88;

   -- intervalle du timer en millisecondes
   timer_delay : constant := 10;

   -- message pour la main window
   FADER_VALUE_CHANGED : constant := Win32.Winuser.WM_USER + 100;
   BUTTON_CLICKED      : constant := Win32.Winuser.WM_USER + 101;
   PAINT_STATUS        : constant := Win32.Winuser.WM_USER + 102;
   ASC_VALUE_CHANGED   : constant := Win32.Winuser.WM_USER + 103;
   BISTABLE_CHANGED    : constant := Win32.Winuser.WM_USER + 104;
   NUM_CHANGED         : constant := Win32.Winuser.WM_USER + 105;
   PLAYER_RESET        : constant := Win32.Winuser.WM_USER + 106;

   -- handle de la fenetre principale
   Win_hwnd  : Win32.Windef.HWND;
   Main_DC   : Win32.Windef.HDC;

   -- background
   Back_DC   : Win32.Windef.HDC;
   background : Win32.Windef.HBITMAP;
   back_bits : aliased Win32.LPVOID;
   back_header : Win32.Wingdi.BITMAPINFOHEADER;


   -- fenetre cachée pour le traitement de l'input
   Data_hwnd : Win32.Windef.HWND;
   -- handle du fader
   Fader_hwnd   : Win32.Windef.HWND;

   -- instance de l'application
   hInst    : Win32.Windef.HINSTANCE;

   -- nom de la classe de l'application
   APPCLASS     : constant String := Config_pkg.Appli_name & ASCII.Nul;

   -- taille en ECHANTILLONS ( de 2 octets)
   buff_size : Integer := 4096;

   -- fréquence d'échantillonage: valeur modifiée par l'utilisateur
   Fs : Integer := 44_100;

   -- Device Id du micro: valeur modifiée par l'utilisateur
   micro_num : Win32.UINT := 0;
   micro_str : string_pt := new string'("<>");

   -- Device Id du MidiOut sélectionné
   midi_num : Win32.UINT := 0;
   midi_str : string_pt := new string'("<>");
   no_midi_device : boolean := true;

   -- Id du control pour le volume du microphone (winaudio et UI)
   Volume_control_Id : Win32.DWORD;

   -- Options
   Auto_volume       : boolean := false;	-- empecher saturation
   Change_instrument : boolean := false;	-- chnage l'instrument de la mélodie
   Instrument_number : Win32.byte := 0;		-- instrument par défaut


   -- seuil minimum pour la détection du pitch
   seuil_pitch : constant Short_Integer := 1000;

   -- largeur en pixel d'un filtre du spectre
   largeur_filtre : constant := 5;

   -- header des buffers d'enregistrement
   type head_pt is access Win32.Mmsystem.WAVEHDR;

   -- buffers d'enregistrement
   type buff_type is array(natural range <>) of Short_integer;
   type buff_pt is access buff_type;

   -- buffers pour le filtrage
   type F_buff is array(natural range <>) of Float;
   type F_buff_pt is access F_Buff;

   -- pour les conversions entre pointeurs
   type byte_array is array(integer range <>) of Win32.BYTE;
   type byte_ptr is access byte_array;

   -- type pour le transfer des notes et des textes
   type int_array is array(natural range <>) of Integer;
   type str_array is array(natural range <>) of String_pt;

   -- desallocation des buffers audio
   procedure Free is new Unchecked_Deallocation( object => byte_array, name => byte_ptr );
   procedure Free is new Unchecked_deallocation( object => buff_type,  name => buff_pt );
   procedure Free is new Unchecked_deallocation( object => F_buff,     name => F_buff_pt );

   procedure Free is new Unchecked_deallocation( object => string,     name => string_pt );

   -- exception pour stoper le programme
   fatal_error : exception;
   -- exception pour stopper une fonction
   banal_error : exception;

   -- pointeur sur entier utilisé pour les Id d'objets
   type Integer_ptr is access Integer;

   -- redémarrage après un changement de paramètre
   restarting : boolean := false;

   new_line : constant string := ( 1 => ascii.lf, 2 => ascii.cr );

end Common_Types;

with Win32;
with Win32.Mmsystem;

with Common_types;

package Win_audio is

   -- la fréquence d'échantillonnage et la taille donnent
   -- la fréquence de rafraichissement de l'affichage
   -- ex: 8000/1024 = 7.8 fps
   -- le nombre de buffer doit être suffisant pour permettre le traitement sans perte
   -- et assurer 1 seconde de stockage
   nb_buff : constant := 11;


   -- buffers:
   Buffers: array(1..nb_buff) of Common_types.buff_pt;
   -- header des buffers
   Headers : array(1..nb_buff) of Common_types.head_pt;


   -- handle pour l'input
   micro : Win32.Mmsystem.HWAVEIN;	-- = HANDLE = PVOID = System.Address

   function Get_Micro_Volume return Win32.WORD;

   procedure Set_Micro_Volume( valeur : Win32.WORD );

   function Open_Mixer return boolean;

   procedure Close_Mixer;

   procedure Set_Volume_Max;

   procedure Start_lecture;

   procedure Stop_lecture;

   function Check_config return boolean;

end Win_Audio;

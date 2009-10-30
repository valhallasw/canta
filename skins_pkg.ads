with System;
with Win32.Windef;
with Resources_pkg;

package Skins_pkg is

   -- polices customisables
   Button_font : Win32.Windef.HFONT  := System.Null_address;
   button_color: Win32.Windef.COLORREF := 0;
   --
   Label_font  : Win32.Windef.HFONT  := System.Null_address;
   label_color : Win32.Windef.COLORREF := 0;

   -- position des texte et icones dans les boutons
   large_but_x  : integer := 0;
   large_but_y  : integer := 0;
   med_but_x    : integer := 0;
   med_but_y    : integer := 0;
   pet_but_x    : integer := 0;
   pet_but_y    : integer := 0;
   -- fader
   fader_diff_haut    : integer := 0;	-- distance min entre le curseur et le haut
   fader_diff_bas     : integer := 0;	-- distance min entre le curseur et le bas
   asc_V_diff_haut    : integer := 0;	-- distance min entre le curseur et le haut
   asc_V_diff_bas     : integer := 0;	-- distancemin entre le curseur et le bas
   asc_H_diff_gauche  : integer := 0;	-- distance min entre le curseur et la gauche
   asc_H_diff_droite  : integer := 0;	-- distancemin entre le curseur et la droite



   -- charge la skin, retourne True si tout s'est bien passé
   function Load_skin( name : string ) return boolean;

   -- charge la skin par defaut hard-codée
   procedure Default_skin;

   -- retourne le nombre de skins disponible
   -- et crée la liste des skin utilisé par Skin_name
   function Count_skins return integer;

   -- retourne le nom du skin de rang 'number'
   -- Count_Skins doit avoir été appelé avant !
   function Skin_name( number : integer ) return string;

end Skins_pkg;

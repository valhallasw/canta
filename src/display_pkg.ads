with Win32;
with Win32.Windef;

with Resources_pkg;

package Display_Pkg is

   type display_info;
   type display_info_ptr is access display_info;
   type display_info is record
      hwnd    : Win32.Windef.HWND;	-- handle
      screenDC: Win32.Windef.hdc;	-- DC propre de la window (Class CS_OWNDC)
      memDC   : Win32.Windef.hdc;	-- DC pour écrire dans la bitmap
      Id      : Resources_pkg.obj_Id_type;	-- Identifier
      Parent  : Win32.Windef.HWND;	-- son parent
      X       : Win32.INT;		-- position / parent
      Y       : Win32.INT;
      Largeur : Win32.INT;		-- taille en pixels
      Hauteur : Win32.INT;
      bitmap  : Win32.Windef.HBITMAP;	-- bitmap pour écriture en mémoire
      fond    : Win32.Windef.HBRUSH;	-- brush pour le fond
      mince   : boolean;
      --
      next   : display_info_ptr;
   end record;


   procedure Create_Display_Class;

   function New_Display( X, Y, largeur, hauteur : Integer;
                         Parent       : Win32.Windef.HWND;
                         Id           : Resources_pkg.obj_Id_type;
                         fond         : Win32.Windef.HBRUSH;
                         bords_minces : boolean := false ) return Win32.Windef.HWND;

   function Display_of( id : Resources_pkg.obj_Id_type ) return display_info_ptr;

   function Display_of( hwnd : Win32.Windef.HWND ) return display_info_ptr;

   procedure Affiche_display( display : display_info_ptr);

   procedure Resize( id : Resources_pkg.obj_Id_type; largeur, hauteur : Integer );

   -- Dessine le cadre d'un display
   procedure Paint_frame( hdc : Win32.Windef.HDC; X, Y, largeur, hauteur : Win32.INT );

   -- retourne le rectangle intérieur d'un display de largeur x hauteur
   function User_rect( largeur, hauteur : Win32.INT ) return Win32.Windef.RECT;

   -- doit ête appelé après chaque changement de skin
   procedure Get_bitmap_size;

end Display_Pkg;

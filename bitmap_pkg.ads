with System;

with Win32;
with Win32.Windef;
with Win32.Wingdi;

with Resources_pkg;

package Bitmap_pkg is

   -- exception pour le chargement des bitmap
   invalid_bitmap_file : exception;

   type bitmap_info;
   type bitmap_info_ptr is access bitmap_info;

   type bitmap_info is record
      header : Win32.Wingdi.BITMAPINFOHEADER;	-- win32.wingdi.ads:1732
      handle : Win32.Windef.HBITMAP;
      Id     : Resources_pkg.bmap_Id_type;
      bits   : Win32.PVOID;
      next   : bitmap_info_ptr;
   end record;

   function Bitmap_of( Id : Resources_pkg.bmap_Id_type ) return bitmap_info_ptr;

   -- creation d'une bitmap à partir d'une ressource hard-codée
   procedure Create_bitmap( Nom : string;
                            Id : Resources_pkg.bmap_Id_type );
   -- création d'une bitmap à partir d'un fichier BMP
   procedure Create_from_file( file_name : string;
                               Id : Resources_pkg.bmap_Id_type );

   procedure Affiche_bitmap( Win_HDC  : Win32.Windef.HDC;
                             Id       : Resources_pkg.bmap_Id_type;
                             X, Y     : Win32.INT );

   procedure Transparent_copy( source         : System.address;
                               largeur_source : integer;
                               hauteur_source : integer;
                               destination    : System.address;
                               largeur_dest   : integer;
                               hauteur_dest   : integer;
                               X, Y           : integer );

   -- pour afficher en transparent sur une autre bitmap
   procedure Affiche_transparent( Win_HDC   : Win32.Windef.HDC;
                                  bitmap_id : Resources_pkg.bmap_Id_type;
                                  fond_id   : Resources_pkg.bmap_Id_type;
                                  X, Y      : Win32.INT );	-- position / window locale

   -- pour afficher en transparent sur le fond de la main
   procedure Affiche_transparent( Win_HDC   : Win32.Windef.HDC;
                                  bitmap_id : Resources_pkg.bmap_Id_type;
                                  X_local, Y_local : Win32.INT;		-- position / window local
                                  X_main, Y_main : Win32.INT ); 	-- position de la window locale dans la main

   function Largeur( Id : Resources_pkg.bmap_Id_type ) return Integer;

   function Hauteur( Id : Resources_pkg.bmap_Id_type ) return Integer;

   -- etire une bitmap verticalement
   procedure Etire_bitmap_V( Win_HDC   : Win32.Windef.HDC;
                             bitmap_id : Resources_pkg.bmap_Id_type;
                             X, Y      : Win32.INT;
                             hauteur   : Win32.INT );

   -- etire une bitmap horizontalement
   procedure Etire_bitmap_H( Win_HDC   : Win32.Windef.HDC;
                             bitmap_id : Resources_pkg.bmap_Id_type;
                             X, Y      : Win32.INT;
                             largeur   : Win32.INT );

   -- étire une bitmap dans les 2 directions
   procedure Etire_bitmap( Win_HDC   : Win32.Windef.HDC;
                           bitmap_id : Resources_pkg.bmap_Id_type;
                           X, Y      : Win32.INT;
                           largeur   : Win32.INT;
                           hauteur   : Win32.INT );

   -- Desalloue toutes les bitmaps de l'interface (changement de skin)
   procedure Free_all_bitmaps;

   -- creation du background
   procedure Create_background;

end Bitmap_pkg;

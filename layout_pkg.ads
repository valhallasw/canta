with Resources_pkg;

package Layout_pkg is

   -- dimension de la main window (partie client)
   function Window_width return integer;
   function Window_height return integer;

   -- calcul les propriétés des objets après un changement de skin
--   procedure Compute_new_geometry;

   -- redimensionne les objets apres un resize de la main window
   procedure Resize_window;

   -- calcule la position et la taille des objets en fonction de la taille de la main window
   procedure Compute_layout;

   -- création de tous les objets d'interface
   procedure Init_Layout;

end Layout_pkg;

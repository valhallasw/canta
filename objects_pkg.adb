with System;		use System;
with Interfaces.C;	use Interfaces.C;
with GNAT.Current_Exception;

with Win32;		use Win32;
with Win32.Windef;	use Win32.Windef;
with Win32.Winbase;
with Win32.Winnt;	use Win32.Winnt;
with Win32.Winuser;
with Win32.Wingdi;

with Conversions;	use Conversions;
with Common_types;	use Common_types;
with Resources_pkg;	use Resources_pkg;
with Bitmap_pkg;
with Utils_pkg;
with Log;
with Intl;


package body Objects_pkg is

   -- nom de la class windows pour les objets d'interface
   CantaUIObjClass  : constant String := "CantaUIObjects" & ASCII.nul;

   -- liste des objets d'interface
   object_list, last_object : UI_object_ptr;

   WM_CAPTURECHANGED : constant := 533;


   -- **************************************************************************************

   -- Declaration de la procedure des callback
   function Objects_Proc(hwnd    : Win32.Windef.HWND;
                         message : Win32.UINT;
                         wParam  : Win32.WPARAM;
			 lParam  : Win32.LPARAM)
			 		return Win32.LRESULT;
   pragma Convention (Stdcall, Objects_Proc);

   -- ***************************************************************************************

   function String_width( str : string;
                          DC  : Win32.Windef.HDC ) return natural is
      -- pour TrueType
      type table_ABC_width_type is array(32..255) of aliased Win32.Wingdi.ABC;
      table_ABC_width : aliased table_ABC_width_type;
      -- pour autres
      type table_width_type is array(32..255) of aliased Win32.INT;
      table_width : aliased table_width_type;
      -- ou cas où ...
      tm : aliased Win32.Wingdi.Textmetric;
      --
      len : natural := 0;
      index : natural;
      res_bool : Win32.BOOL;
   begin
      -- essaye les TrueType d'abord
      res_bool := Win32.Wingdi.GetCharABCWidths( DC, 32, 255, table_ABC_width(32)'access );
      if res_bool /= 0 then
         -- calcule la longueur en ajoutant la taille de chaque caractère
         for i in str'range loop
            index := character'pos( str(i) );
            len := len + integer(table_abc_width(index).abcA) + integer(table_abc_width(index).abcB)
                        + integer(table_abc_width(index).abcC);
         end loop;
         -- terminé
         return len;
      end if;
      --
      -- pas True type ? essayer avec ça:
      res_bool := Win32.Wingdi.GetCharWidth32( DC, 32, 255, table_width(32)'access );
      if res_bool /= 0 then
         for i in str'range loop
            index := character'pos( str(i) );
            len := len + natural(table_width(index));
         end loop;
         -- terminé
         return len;
      end if;
      --
      -- à défaut retourne la taille moyenne
      res_bool := Win32.Wingdi.GetTextmetrics( dc, tm'access );
      return str'length * natural( tm.tmAveCharWidth );
      --
   end String_width;


   -- ***************************************************************************************

   function Object_of( Id : Resources_pkg.obj_id_type ) return UI_object_ptr is
      tmp : UI_object_ptr := object_list;
   begin
      while tmp /= null and then tmp.Id /= Id loop
         tmp := tmp.next;
      end loop;
      return tmp;
   end Object_of;


   function Object_of( hwnd  : Win32.Winnt.HANDLE ) return UI_object_ptr is
      tmp : UI_object_ptr := object_list;
   begin
      while tmp /= null and then tmp.hwnd /= hwnd loop
         tmp := tmp.next;
      end loop;
      return tmp;
   end Object_of;


   -- allocation d'un nouvel object et mise dans la queue de la liste
   function New_object( kind : object_kind; Id : Resources_pkg.obj_id_type ) return UI_object_ptr is
   begin
      if object_list = null then
         object_list := new UI_object_type(kind);
         last_object := object_list;
      else
         last_object.next := new UI_object_type(kind);
         last_object := last_object.next;
      end if;
      last_object.Id := Id;
      return last_object;
   end New_object;


   -- Maximum de deux entiers
   function Max( v1, v2 : integer ) return integer is
   begin
      if v1 > v2 then
         return v1;
      else
         return v2;
      end if;
   end Max;

   -- Minimum de deux entiers
   function Min( v1, v2 : integer ) return integer is
   begin
      if v1 < v2 then
         return v1;
      else
         return v2;
      end if;
   end Min;


   -- **************************************************************************************

   procedure Set_Font( obj : UI_object_ptr; font : Win32.Windef.HFONT ) is
      old_obj : Win32.Windef.HGDIOBJ;
   begin
      obj.font := font;
      -- associe la font
      old_obj := Win32.Wingdi.SelectObject( obj.DC, font );
   end Set_Font;

   procedure Set_Font( Id : Resources_pkg.obj_id_type; font : Win32.Windef.HFONT ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      Set_FOnt( obj, font );
   end Set_Font;


   procedure Set_Brush( obj : UI_object_ptr; brush : Win32.Windef.HBRUSH ) is
      old_obj : Win32.Windef.HGDIOBJ;
   begin
      obj.brush := brush;
      -- associe la font
      old_obj := Win32.Wingdi.SelectObject( obj.DC, brush );
   end Set_Brush;

   procedure Set_Brush( Id : Resources_pkg.obj_id_type; brush : Win32.Windef.HBRUSH ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      Set_Brush( obj, brush );
   end Set_Brush;


   procedure Set_Color( obj : UI_object_ptr; color : Win32.Windef.COLORREF ) is
      old_color : Win32.Windef.COLORREF;
   begin
      obj.color := color;
      -- couleur du texte
      old_color := Win32.Wingdi.SetTextColor( obj.dc, color );
   end Set_Color;

   procedure Set_Color( Id : Resources_pkg.obj_id_type; color : Win32.Windef.COLORREF ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      Set_Color( obj, color );
   end Set_Color;

   -- ===============================================================================

   -- création de la window associé à l'objet
   procedure Create_Window( obj : UI_object_ptr ) is
   begin
      obj.hwnd := Win32.Winuser.CreateWindow(
           lpClassName  => Conversions.TO_PCCH(CantaUIObjClass'address),
           lpWindowName => Conversions.TO_PCCH(System.Null_Address),
           dwStyle      => Win32.Winuser.WS_CHILDWINDOW + Win32.Winuser.WS_VISIBLE
                               + Win32.Winuser.WS_CLIPSIBLINGS,
           X            => Win32.INT(obj.X),
           Y            => Win32.INT(obj.Y),
           nWidth       => Win32.INT(obj.Largeur),
           nHeight      => Win32.INT(obj.Hauteur),
           hWndParent   => Common_types.Win_hwnd,		-- handle de la main window
           hMenu        => System.Null_Address,
           hInstance    => Common_types.hInst,
           lpParam      => System.Null_Address);
      --
      if obj.hwnd = System.Null_address then
         -- erreur, window pas créée
         Utils_pkg.Raise_fatal_error(obj.id, "Objects_pkg.Create_Window");
      end if;
      -- récupére son propre DC
      obj.DC := Win32.Winuser.GetDC( obj.hwnd );
   end Create_Window;


   -- =====================================================================================


   procedure Compute_Outer( obj : UI_object_ptr ) is
   begin
      if obj.framed then
         obj.bord_x := Bitmap_pkg.Largeur( obj.frames(border_l) );	-- bord gauche
         obj.bord_y := Bitmap_pkg.Hauteur( obj.frames(border_t) );	-- bord haut
         obj.largeur := obj.inner_w  + 2 * obj.bord_x ;	-- ... plus 2 bords verticaux
         obj.hauteur := obj.inner_h + 2 * obj.bord_y;	-- ... plus 2 bords horizontaux
      else
         obj.bord_x := 0;
         obj.bord_y := 0;
         obj.largeur := obj.inner_w;
         obj.hauteur := obj.inner_h;
      end if;
   end Compute_Outer;


   procedure Compute_Inner( obj : UI_object_ptr ) is
   begin
      -- calcul des taille intérieures
      if obj.framed then
         obj.bord_x := Bitmap_pkg.Largeur( obj.frames(border_l) );
         obj.bord_y := Bitmap_pkg.Hauteur( obj.frames(border_t) );
         obj.inner_w := obj.largeur -  2* obj.bord_x;
         obj.inner_h := obj.hauteur -  2 * obj.bord_y;
      else
         obj.bord_x := 0;
         obj.bord_y := 0;
         obj.inner_w := obj.largeur;
         obj.inner_h := obj.hauteur;
      end if;
   end Compute_inner;


   -- =====================================================================================

   -- NUM_EDIT

   function Clean_image( num : integer ) return string is
      resu : string := integer'image( num );
   begin
      if num < 0 then
         return resu;
      else
         return resu(resu'first+1..resu'last);
      end if;
   end Clean_image;

   procedure Display_Num_edit( obj : UI_object_ptr ) is
      fond_rect : Win32.Windef.RECT;
      res_int : Win32.INT;
      res_bool : Win32.BOOL;
      texte : string := clean_image( obj.number );
   begin
      -- effacement du fond
      if obj.brush /= N_A then
         -- effacement du fond avec la brush du DC
         res_bool := Win32.Wingdi.PatBlt( obj.DC,
                                          nLeftRect => Win32.INT(obj.bord_x),
                                          nTopRect  => Win32.INT(obj.bord_y),
                                          nwidth    => Win32.INT(obj.inner_w),
                                          nheight   => Win32.INT(obj.inner_h),
                                          fdwRop    => Win32.Wingdi.PATCOPY );
      end if;
      -- rectangle pour le fond
      fond_rect := ( Win32.LONG(obj.bord_x), Win32.LONG(obj.bord_y),
                     Win32.LONG(obj.inner_w+obj.bord_x), Win32.LONG(obj.inner_h+obj.bord_y) );
      -- écriture dans l'objet Windows
      res_int := Win32.Winuser.DrawText( obj.DC,
                        Conversions.TO_PCCH( texte'address),
                        Win32.INT(texte'length),
                        Conversions.TO_PRECT(fond_rect'address),
                        Win32.Winuser.DT_VCENTER + Win32.Winuser.DT_CENTER );	-- centré
   end Display_Num_edit;


   procedure Display_Num_edit( Id : Resources_pkg.obj_id_type ) is
      obj : UI_object_ptr := Object_of( Id );
   begin
      Display_Num_edit( obj );
   end Display_Num_edit;


   procedure Create_Num_edit( Id        : Resources_pkg.obj_id_type;
                              max_digit : natural;
                              font      : Win32.Windef.HFONT;
                              color     : Win32.Windef.COLORREF;
                              brush     : Win32.Windef.HBRUSH;
                              framed    : boolean;
                              frames    : frame_set;
                              min,
                              max,
                              value     : integer := 0 ) is
      obj : UI_object_ptr;
      res_bool : Win32.BOOL;
      res_int : Win32.INT;
      tm : aliased Win32.Wingdi.Textmetric;
   begin
      -- création du record
      obj := New_object( Num_Edit, Id );
      -- copie des paramètres
      obj.max_digit := max_digit;
      obj.max_num   := max;
      obj.min_num   := min;
      obj.number    := value;
      obj.framed    := framed;
      obj.frames    := frames;
      -- creation de l'objet windows et de son DC
      Create_Window( obj );
      -- écriture en transparent
      res_int := Win32.Wingdi.SetBkMode( obj.DC, Win32.Wingdi.TRANSPARENT );
      -- sélectionne la font
      Set_Font( obj, font );
      -- enregistre la brush pour le fond
      Set_brush( obj, brush );
      -- couleur du texte
      Set_color( obj, color );
      -- calcule de la taille interne
      res_bool := Win32.Wingdi.GetTextmetrics( obj.dc, tm'access );
      obj.inner_h := natural( tm.tmHeight );	-- hauteur de la police
      -- calcule la largeur à partir de la taile moyenne des chiffres
      obj.inner_w := (obj.max_digit * String_width("0123456789", obj.DC)) / 10;
      -- calcule la taille externe
      Compute_outer( obj );
   end Create_Num_edit;


   procedure Num_Compute_value( obj : UI_object_ptr; mouse_y : integer ) is
      decal, new_value : integer;
      demi_h : natural := obj.hauteur / 2;
   begin
      -- calcule l'incrément
      decal := - (mouse_y / demi_h);
      if decal = 0 and then not obj.moved then
         decal := 1;
      elsif mouse_y < 0 then
         decal := decal + 1;
      end if;
      -- nouvelle valeur
      new_value := obj.base + decal;
      -- s'assurer de rester dans l'intervalle autorisé
      if new_value > obj.max_num then
         new_value := obj.max_num;
      end if;
      if new_value < obj.min_num then
         new_value := obj.min_num;
      end if;
      -- assigne la nouvelle valeur
      obj.number := new_value;
   end Num_Compute_value;


   function Num_Get_value( Id : Resources_pkg.obj_id_type ) return integer is
      obj : UI_object_ptr := Object_of( Id );
   begin
      return obj.number;
   end Num_Get_value;


   procedure Num_Set_Min( Id : Resources_pkg.obj_id_type;
                              min_value : integer ) is
      obj : UI_object_ptr := Object_of( Id );
   begin
      obj.min_num := min_value;
      -- maj de la valeur
      if obj.number < obj.min_num then
         obj.number := obj.min_num;
      end if;
   end Num_Set_Min;


   procedure Num_Set_Max( Id : Resources_pkg.obj_id_type;
                          max_value : integer ) is
      obj : UI_object_ptr := Object_of( Id );
   begin
      obj.max_num := max_value;
      -- maj de la valeur
      if obj.number > obj.max_num then
         obj.number := obj.max_num;
      end if;
   end Num_Set_Max;


   procedure Num_Set_value( Id : Resources_pkg.obj_id_type;
                            value : integer ) is
      obj : UI_object_ptr := Object_of( Id );
   begin
      obj.number := value;
   end Num_Set_value;


   -- =====================================================================================

   -- BISTABLES

   procedure Display_bistable( obj : UI_object_ptr ) is
      Fond: Resources_pkg.bmap_Id_type;
   begin
      -- affichage du fond
      if obj.state_on then
         Fond := obj.on_bitmap;
      else
         Fond := obj.off_bitmap;
      end if;
      -- affiche en transparent sur le background
      Bitmap_pkg.Affiche_Transparent( obj.DC, Fond,
                                      X_local => 0, Y_local => 0,
                                      X_main => Win32.INT(obj.X), Y_main => Win32.INT(obj.Y) );
   end Display_bistable;


   procedure Change_state( obj : UI_object_ptr ) is
   begin
      obj.state_on := not obj.state_on;
   end Change_state;


   procedure Create_Bistable( Id : Resources_pkg.obj_id_type;
                              fond_off, fond_on : Resources_pkg.bmap_id_type ) is
      obj : UI_object_ptr;
   begin
      -- création du record
      obj := New_object( Bistable, Id );
      -- stockage des données spécifiques: les bitmaps
      obj.on_bitmap  := fond_on;
      obj.off_bitmap := fond_off;
      -- taille du bistable = taille de la bitmap (les 2 doivent avoir la même taille !)
      obj.inner_w := Bitmap_pkg.Largeur( obj.off_bitmap );
      obj.inner_h := Bitmap_pkg.Hauteur( obj.off_bitmap );
      Compute_outer( obj );
      -- creation de l'objet windows et de son DC
      Create_Window( obj);
      --
   end Create_Bistable;


   function Bistable_on( Id : Resources_pkg.obj_id_type ) return boolean is
      obj : UI_object_ptr := Object_of( Id );
   begin
      return obj.State_on;
   end Bistable_on;


   -- =====================================================================================

   -- FADERS

   -- dessine une fader ou un ascenseur
   procedure Display_Fader( obj : UI_object_ptr ) is
      curseur : Resources_pkg.bmap_id_type;
      x1, x2, y1, y2, xc, yc, longueur : integer;
      res_bool : Win32.BOOL;
      source_bitmap : Bitmap_pkg.bitmap_info_ptr;
   begin
      -- copie du background dans la bitmap de travail du fader
      res_bool := Win32.Wingdi.BitBlt(
                    hdcDest => obj.memDC, 			-- DC mémoire
                    nXDest  => 0,
                    nYDest  => 0,
                    nWidth  => Win32.INT(obj.inner_w),
                    nHeight => Win32.INT(obj.inner_h),
                    hdcSrc  => Back_DC, 			-- DC associé au background
                    nXSrc   => Win32.INT(obj.x+obj.bord_x),	-- position X du fader + décalage bordure
                    nYSrc   => Win32.INT(obj.y+obj.bord_y),	-- idem en Y
                    dwRop   => Win32.Wingdi.SRCCOPY );
      -- calcule coordonnées des bitmaps des extrémités
      if obj.horiz then
         -- etremite 1: gauche, coordonnées inversées en Y
         x1 := 0;
         y1 := 0;
         -- ectremite 2 : droite, coordonnées inversées en Y
         x2 := obj.largeur - obj.d2;
         y2 := 0;
         -- centre, coordonnées normales
         xc := obj.d1;
         yc := 0;
         longueur := obj.largeur - obj.d1 - obj.d2;
      else
         -- extremite 1: haut, coordonnées inversées en Y
         x1 := 0;
         y1 := obj.hauteur - obj.d1;
         -- extremite 2 : en bas, coordonnées inversées en Y
         x2 := 0;
         y2 := 0;
         -- centre, coordonnées normales
         xc := 0;
         yc := obj.d1;
         longueur := obj.hauteur - obj.d1 - obj.d2;
      end if;
      -- extrémité haut ou gauche en (0,0)
      source_bitmap := Bitmap_pkg.Bitmap_of( obj.extrem_1 );
      Bitmap_pkg.Transparent_copy( source         => source_bitmap.bits,
                                   largeur_source => Integer(source_bitmap.header.biWidth),
                                   hauteur_source => Integer(source_bitmap.header.biHeight),
                                   destination    => obj.bits,
                                   largeur_dest   => obj.inner_w,
                                   hauteur_dest   => obj.inner_h,
                                   X              => x1,
                                   Y              => y1 );
      -- extrémité bas ou droite en (x2,y2)
      source_bitmap := Bitmap_pkg.Bitmap_of( obj.extrem_2 );
      Bitmap_pkg.Transparent_copy( source         => source_bitmap.bits,
                                   largeur_source => Integer(source_bitmap.header.biWidth),
                                   hauteur_source => Integer(source_bitmap.header.biHeight),
                                   destination    => obj.bits,
                                   largeur_dest   => obj.inner_w,
                                   hauteur_dest   => obj.inner_h,
                                   X              => x2,
                                   Y              => y2 );
      -- le centre en (x1,y1)
      if obj.horiz then
         -- étire horizontalement le centre pour remplir tout l'espace
         Bitmap_pkg.Etire_bitmap_H( obj.memDC, obj.middle, X => Win32.INT(xc), Y => Win32.INT(yc),
                              largeur => Win32.INT(longueur) );
      else
         -- idem verticalement
         Bitmap_pkg.Etire_bitmap_V( obj.memDC, obj.middle, X => Win32.INT(xc), Y => Win32.INT(yc),
                              hauteur => Win32.INT(longueur) );
      end if;
      --
      -- le curseur
      -- sélection du bitmap correspondant à son état
      case obj.fader_state is
         when fader_Off   => curseur := obj.curs_off;
         when fader_On    => curseur := obj.curs_on;
         when fader_Inval => curseur := obj.curs_inval;
      end case;
      -- affichage du curseur
      source_bitmap := Bitmap_pkg.Bitmap_of( curseur );
      Bitmap_pkg.Transparent_copy( source         => source_bitmap.bits,
                                   largeur_source => Integer(source_bitmap.header.biWidth),
                                   hauteur_source => Integer(source_bitmap.header.biHeight),
                                   destination    => obj.bits,
                                   largeur_dest   => obj.inner_w,
                                   hauteur_dest   => obj.inner_h,
                                   X              => obj.curs_x,
                                   -- ATTENTION: le DIB étant bottom-up, les coordonnées Y sont inversée !!
                                   Y              => obj.hauteur - obj.curs_haut - obj.curs_y );
      -- transfert du bitmap dans la window
      res_bool := Win32.Wingdi.BitBlt(
                    obj.DC, 			-- destination = DC écran
                    Win32.INT(obj.bord_x),
                    Win32.INT(obj.bord_y),	-- décalage du à la bordure (peut etre 0,0)
                    Win32.INT(obj.inner_w), Win32.INT(obj.inner_h),
                    obj.memDC, 			-- source = DC mémoire
                    0,0,
                    Win32.Wingdi.SRCCOPY );
   end Display_Fader;


   -- positionne le curseur en fonction de la valeur
   procedure Set_Cursor_position( obj : UI_object_ptr ) is
      new_pos, taille, offset : integer;
   begin
      if obj.horiz then
         taille := obj.largeur;
         offset := obj.curs_large;
      else
         taille := obj.hauteur;
         offset := obj.curs_haut;
      end if;
      if obj.fader_state = fader_inval then
         -- si invalidé, tout à droite ou tout en bas
         new_pos := taille - obj.min_2 - offset;
      else
         -- calcul position
         new_pos := obj.min_1 + (obj.max_val - obj.value) * ( taille - obj.min_1 - obj.min_2 - offset)
                                           / (obj.max_val - obj.min_val);
         -- s'assure que le curseur reste dans l'intervalle autorisé
         new_pos := Max( new_pos, obj.min_1 );
         new_pos := Min( new_pos, taille - obj.min_2 - offset );
      end if;
      -- assignation à la coordonnées ad hoc
      if obj.horiz then
         obj.curs_x := new_pos;
      else
         obj.curs_y := new_pos;
      end if;
   end Set_Cursor_position;


   -- stocke le décalage dela souris / curseur pour la suivre sans à coups
   procedure Set_mouse_offset( obj : UI_object_ptr; mouse_x, mouse_y : integer ) is
   begin
      if obj.horiz then
         obj.mouse := mouse_x - obj.curs_x;
      else
         obj.mouse := mouse_y - obj.curs_y;
      end if;
   end Set_mouse_offset;


   -- déplace le curseur pour suivre la souris et recalcue la valeur associée
   procedure Move_cursor( obj : UI_object_ptr; mouse_x, mouse_y : integer ) is
   begin
      if obj.horiz then
         obj.curs_x := max( min(mouse_x - obj.mouse, obj.largeur - obj.min_2 - obj.curs_large), obj.min_1 );
         obj.value := obj.min_val + ((obj.max_val - obj.min_val)
                    * (obj.curs_x - obj.min_1)) / ( obj.largeur - obj.min_2 - obj.min_1 - obj.curs_large) ;
      else
         obj.curs_y := max( min(mouse_y - obj.mouse, obj.hauteur - obj.min_2 - obj.curs_haut), obj.min_1 );
         obj.value := obj.max_val - ((obj.max_val - obj.min_val)
                   * (obj.curs_y - obj.min_1)) / ( obj.hauteur - obj.min_2 - obj.min_1 - obj.curs_haut ) ;
      end if;
   end Move_cursor;


   -- calcule les paramètres du fader liés au bitmap
   procedure Compute_Fader( obj : UI_object_ptr ) is
   begin
      obj.curs_large := Bitmap_pkg.Largeur( obj.curs_off );	-- taille du curseur en pixels
      obj.curs_haut  := Bitmap_pkg.Hauteur( obj.curs_off );
      if obj.horiz then
         -- horizontal
         obj.d1 := Bitmap_pkg.Largeur( obj.extrem_1 );
         obj.d2 := Bitmap_pkg.Largeur( obj.extrem_2 );
         --
         obj.hauteur := Bitmap_pkg.Hauteur( obj.extrem_1 );
         obj.largeur := obj.d1 + obj.d2 + max( obj.curs_large, Bitmap_pkg.Largeur(obj.middle));	-- largeur minimale
         --
         obj.curs_y := (obj.hauteur - obj.curs_haut) / 2;	-- centrage vertical
      else
         -- vertical
         obj.d1 := Bitmap_pkg.Hauteur( obj.extrem_1 );
         obj.d2 := Bitmap_pkg.Hauteur( obj.extrem_2 );
         --
         obj.largeur := Bitmap_pkg.Largeur( obj.extrem_1 );
         obj.hauteur := obj.d1 + obj.d2 + max( obj.curs_haut, Bitmap_pkg.Hauteur(obj.middle));	-- hauteur minimale
         --
         obj.curs_x := (obj.largeur - obj.curs_large) / 2;	-- centrage horizontal
      end if;
      Compute_Inner( obj );
   end Compute_Fader;


   -- création d'un Fader ou d'un ascenseur
   procedure Create_Fader( Id : Resources_pkg.obj_id_type;
                           horiz : boolean;
                           resizeable : boolean;
                           fond_1, fond_2, fond_3 : Resources_pkg.bmap_id_type;
                           curs_1, curs_2, curs_3 : Resources_pkg.bmap_id_type;
                           min, max : integer;
                           offset_1, offset_2 : integer ) is
      obj : UI_object_ptr;
      bitmap_header : Win32.Wingdi.BITMAPINFOHEADER := ( 40, 0, 0, 1, 32, 0, 0, 2834, 2834, 0, 0 );
      old_obj : Win32.Windef.HGDIOBJ;
      bits : aliased Win32.LPVOID;
      pix_size : integer;
   begin
      -- création du record
      obj := New_object( Fader, Id );
      -- stockage des données spécifiques
      obj.horiz      := horiz;	-- type horizontal ou vertical
      obj.resize     := resizeable;
      -- les 3 bitmaps du fond
      obj.extrem_1   := fond_1;
      obj.middle     := fond_2;
      obj.extrem_2   := fond_3;
      -- les 3 bitmaps du curseur
      obj.curs_off   := curs_1;
      obj.curs_on    := curs_2;
      obj.curs_inval := curs_3;
      -- limite de déplacement du curseur
      obj.min_1      := offset_1;	-- minimum de distance avec l'extrémité 1
      obj.min_2      := offset_2;	-- idem pour l'extrémité 2
      -- limites des valeurs
      obj.min_val    := min;
      obj.max_val    := max;
      obj.value      := min;	-- valeur courante = minimum
      -- calcule les paramètres liès aux bitmpas
      Compute_Fader( obj );
      -- calcule la position du curseur
      Set_Cursor_position( obj );
      -- creation de l'objet windows
      Create_Window( obj );
      -- création d'un DC pour écriture en mémoire
      obj.memDC := Win32.Wingdi.CreateCompatibleDC( System.Null_Address );
      if obj.memdc = System.Null_address then
         Utils_pkg.Raise_fatal_error(obj.id, "Objects_pkg.Create_fader: CreateCompatibleDC" );
      end if;
      -- Création du bitmap pour écriture en mémoire
      bitmap_header.biWidth     := Win32.LONG(obj.inner_w);
      bitmap_header.biHeight    := Win32.LONG(obj.inner_h); -- (NEGATIF pour avoir en Top-Down)
      bitmap_header.biSizeImage := Win32.DWORD(obj.inner_w * obj.inner_h * 4 + 2);	-- nombre d'octets
      obj.bitmap := Win32.Wingdi.CreateDIBSection( System.Null_Address,
                            TO_ACBITMAPINFO(bitmap_header'address),
                            Win32.Wingdi.DIB_RGB_COLORS,
                            bits'access,			-- adresse du pointer sur les bits
                            System.Null_Address, 0 );		-- pas utilisé
      if obj.bitmap = System.Null_address then
         Utils_pkg.Raise_fatal_error(obj.id, "Objects_pkg.Create_fader: CreateDIBSection" );
      end if;
      obj.bits := bits;
      -- calcul taille
      pix_size := Integer(bitmap_header.biWidth * bitmap_header.biHeight);
      declare
         dest : Win32.UINT_array(1..pix_size);	-- 1 UINT = 1 pixel = 4 octets
         for dest use at bits;
      begin
         dest := Win32.UINT_array'(1..pix_size => 0);
      end;
      -- sélection de la bitmap dans le DC en mémoire
      old_obj := Win32.Wingdi.SelectObject( obj.memDC, obj.bitmap );
      --
   end Create_Fader;


   procedure Resize_fader( obj : UI_object_ptr ) is
      bitmap_header : Win32.Wingdi.BITMAPINFOHEADER := ( 40, 0, 0, 1, 32, 0, 0, 2834, 2834, 0, 0 );
      old_obj : Win32.Windef.HGDIOBJ;
      bits : aliased Win32.LPVOID;
      res_bool : Win32.BOOL;
   begin
      -- calcul la taille intérieure
      Compute_inner( obj );
      -- Création du bitmap avec la nouvelle taille
      bitmap_header.biWidth     := Win32.LONG(obj.inner_w);
      bitmap_header.biHeight    := Win32.LONG(obj.inner_h); -- (NEGATIF pour avoir en Top-Down)
      bitmap_header.biSizeImage := Win32.DWORD(obj.inner_w * obj.inner_h * 4 + 2);	-- nombre d'octets
      obj.bitmap := Win32.Wingdi.CreateDIBSection( System.Null_Address,
                            TO_ACBITMAPINFO(bitmap_header'address),
                            Win32.Wingdi.DIB_RGB_COLORS,
                            bits'access,			-- adresse du pointer sur les bits
                            System.Null_Address, 0 );		-- pas utilisé
      if obj.bitmap = System.Null_address then
         Utils_pkg.Raise_fatal_error(obj.id, "Objects_pkg.Resize_fader: CreateDIBSection" );
      end if;
      obj.bits := bits;
      -- sélection de la bitmap dans le DC
      old_obj := Win32.Wingdi.SelectObject( obj.memDC, obj.bitmap );
      -- destruction ancien bitmap
      res_bool := Win32.Wingdi.DeleteObject( old_obj );
      --
   end Resize_fader;


   -- =====================================================================================

   -- BUTTONS

   procedure Display_button( button : UI_object_ptr ) is
      res_int : Win32.INT;
      Fond: Resources_pkg.bmap_Id_type;
   begin
      -- affichage du fond
      case button.but_state is
         when but_On    => Fond := button.fond_on;
         when but_Off   => Fond := button.fond_off;
         when but_Clic  => Fond := button.fond_clic;
         when but_Inval => Fond := button.Fond_Inval;
      end case;
      -- affiche en transparent sur le background
      Bitmap_pkg.Affiche_Transparent( button.DC, Fond,
                                      X_local => 0, Y_local => 0,
                                      X_main => Win32.INT(button.X), Y_main => Win32.INT(button.Y) );
      if button.but_text /= null then
         -- affichage du texte avec font et couleur du DC
         res_int := Win32.Winuser.DrawText( button.dc,
                      Conversions.TO_PCCH(button.but_text.all'address),
                      button.but_text.all'length,
                      Conversions.TO_PRECT(button.but_rect'address),
                      Win32.Winuser.DT_VCENTER + Win32.Winuser.DT_CENTER + Win32.Winuser.DT_SINGLELINE );
      else
         -- affichage du bitmap en transparence sur le fond, centré
         Bitmap_pkg.Affiche_transparent( button.DC,
                                      bitmap_id => button.but_map,
                                      fond_id   => fond,
                                      X => Win32.INT(button.but_rect.left),
                                      Y => Win32.INT(button.but_rect.top) );
      end if;
   end Display_button;


   -- calcule le rectangle à parti des offset, les bitmaps doivent être définies
   procedure Set_button_rect( obj : UI_object_ptr; offset_x, offset_y : integer ) is
   begin
      if obj.but_text /= null then
         -- bouton texte: le texte est centré par Windows dans le rectangle
         obj.but_rect.left   := Win32.LONG(offset_x);
         obj.but_rect.top    := Win32.LONG(offset_y);
      else
         -- bouton bitmap: l'icone est centrée par calcul de Top et Left
         obj.but_rect.left   := Win32.LONG(offset_x + (obj.largeur - Bitmap_pkg.Largeur(obj.but_map))/2 );
         obj.but_rect.top    := Win32.LONG(offset_y + (obj.hauteur - Bitmap_pkg.Hauteur(obj.but_map))/2 );
      end if;
      obj.but_rect.right  := Win32.LONG(obj.largeur) + obj.but_rect.left;
      obj.but_rect.bottom := Win32.LONG(obj.hauteur) + obj.but_rect.top;
   end Set_button_rect;

   -- modifie la taille en fonction des bitmap
   procedure Compute_Button( obj : UI_object_ptr ) is
   begin
      -- taille du bouton = taille de la bitmap
      obj.largeur := Bitmap_pkg.Largeur( obj.fond_off );
      obj.hauteur := Bitmap_pkg.Hauteur( obj.fond_off );
   end Compute_Button;


   -- création d'un bouton texte ou bitmap
   procedure Create_Button_Bitmap( Id : Resources_pkg.obj_id_type;
                            fond_off, fond_on, fond_clic, fond_inval : Resources_pkg.bmap_id_type;
                            icone : bmap_id_type;
                            offset_x, offset_y : integer ) is
      obj : UI_object_ptr;
   begin
      -- création du record
      obj := New_object( Button, Id );
      -- stockage des données spécifiques
      obj.fond_off   := fond_off;
      obj.fond_on    := fond_on;
      obj.fond_clic  := fond_clic;
      obj.fond_inval := fond_inval;
      obj.but_map    := icone;
      -- calcul taille
      Compute_Button( obj );
      -- défini le rectangle d'affichage en centrant l'icone décalée
      Set_button_rect( obj, offset_x, offset_y );
      -- creation de l'objet windows
      Create_Window( obj );
   end Create_Button_Bitmap;


   procedure Create_Button_Text( Id : Resources_pkg.obj_id_type;
                            fond_off, fond_on, fond_clic, fond_inval : Resources_pkg.bmap_id_type;
                            text  : string;
                            font  : Win32.Windef.HFONT;
                            color : Win32.Windef.COLORREF;
                            offset_x, offset_y : integer ) is
      obj : UI_object_ptr;
      res_int : Win32.INT;
   begin
      -- création du record
      obj := New_object( Button, Id );
      -- stockage des données spécifiques
      obj.fond_off   := fond_off;
      obj.fond_on    := fond_on;
      obj.fond_clic  := fond_clic;
      obj.fond_inval := fond_inval;
      obj.but_text   := new string'(text);		-- allocation et copie
      obj.but_rect.left := Win32.LONG(offset_x);	-- décentrage du rectangle d'affichage
      obj.but_rect.top  := Win32.LONG(offset_y);
      -- calcul taille
      Compute_Button( obj );
      -- défini le rectangle d'affichage
      Set_button_rect( obj, offset_x, offset_y );
      -- creation de l'objet windows et de son DC
      Create_Window( obj);
      -- fond transparent
      res_int := Win32.Wingdi.SetBkMode( obj.DC, Win32.Wingdi.TRANSPARENT );
      -- associe la font
      Set_Font( obj, font );
      -- couleur du texte
      Set_Color( obj, color );
   end Create_Button_Text;

   -- =====================================================================================

   procedure Display_text( text : UI_object_ptr ) is
      fond_rect : Win32.Windef.RECT;
      res_int : Win32.INT;
      res_bool : Win32.BOOL;
      format : Win32.UINT;
   begin
      -- effacement du fond
      if text.brush /= N_A then
         -- effacement du fond avec la brush du DC
         res_bool := Win32.Wingdi.PatBlt( text.DC,
                                          nLeftRect => Win32.INT(text.bord_x),
                                          nTopRect  => Win32.INT(text.bord_y),
                                          nwidth    => Win32.INT(text.inner_w),
                                          nheight   => Win32.INT(text.inner_h),
                                          fdwRop    => Win32.Wingdi.PATCOPY );
      end if;
      -- cadrage
      case text.cadrage is
         when gauche => format := Win32.Winuser.DT_VCENTER + Win32.Winuser.DT_LEFT;
         when droite => format := Win32.Winuser.DT_VCENTER + Win32.Winuser.DT_RIGHT;
         when centre => format := Win32.Winuser.DT_VCENTER + Win32.Winuser.DT_CENTER;
      end case;
      -- rectangle pour le fond
      fond_rect := ( Win32.LONG(text.bord_x), Win32.LONG(text.bord_y),
                     Win32.LONG(text.inner_w+text.bord_x), Win32.LONG(text.inner_h+text.bord_y) );
      -- écriture dans l'objet Windows
      res_int := Win32.Winuser.DrawText( text.DC,
                        Conversions.TO_PCCH(text.buffer.all'address),
                        Win32.INT(text.length),
                        Conversions.TO_PRECT(fond_rect'address),
                        format );
   end Display_text;


   -- création d'une zone de texte pour affichage ou label
   procedure Create_Text( Id : Resources_pkg.obj_id_type;
                          framed : boolean;
                          frames : frame_set;
                          inner_width, inner_height : integer;
                          max_char : integer;
                          font : Win32.Windef.HFONT;
                          color : Win32.Windef.COLORREF;
                          brush : Win32.Windef.HBRUSH;
                          cadrage : text_cadrage ) is
      obj : UI_object_ptr;
      res_int : Win32.INT;
   begin
      -- création du record
      obj := New_object( Text, Id );
      -- stockage des données spécifiques
      obj.framed  := framed;
      obj.frames  := frames;
      obj.inner_w := inner_width;
      obj.inner_h := inner_height;
      obj.buffer  := new string'(1..max_char => ' ');
      obj.cadrage := cadrage;
      -- calcul taille totale
      Compute_Outer( obj );
      -- creation de l'objet windows et de son DC
      Create_Window( obj );
      -- écriture en transparent
      res_int := Win32.Wingdi.SetBkMode( obj.DC, Win32.Wingdi.TRANSPARENT );
      -- sélectionne la font
      Set_Font( obj, font );
      -- enregistre la brush pour le fond
      Set_brush( obj, brush );
      -- couleur du texte
      Set_color( obj, color );
   end Create_Text;


   procedure Adjuste_text_to_string( Id : Resources_pkg.obj_id_type ) is
      obj : UI_object_ptr := Object_of( Id );
      tm : aliased Win32.Wingdi.Textmetric;
      res_bool : Win32.BOOL;
   begin
      -- calcule la largeur de la string
      obj.inner_w := String_width( obj.buffer.all, obj.DC );
      -- lecture caractéristique de la police
      res_bool := Win32.Wingdi.GetTextmetrics( obj.dc, tm'access );
      obj.inner_h := natural( tm.tmHeight );	-- hauteur de la police
      --
      Compute_outer( obj );
   end Adjuste_text_to_string;


   -- =====================================================================================


   -- création d'un display
   procedure Create_Display( Id : Resources_pkg.obj_id_type;
                            framed : boolean;
                            frames : frame_set;
                            inner_width, inner_height : integer;
                            brush : Win32.Windef.HBRUSH;
                            resizeable : boolean ) is
      obj : UI_object_ptr;
      old_obj : Win32.Windef.HGDIOBJ;
      rect: Win32.Windef.RECT;
      res_int : Win32.INT;
   begin
      -- création du record
      obj := New_object( Display, Id );
      -- stockage des données spécifiques
      obj.framed  := framed;
      obj.frames  := frames;
      obj.resize  := resizeable;
      obj.inner_w := inner_width;
      obj.inner_h := inner_height;
      obj.brush   := brush;
      -- calcul taille totale
      Compute_Outer( obj );
      -- creation de l'objet windows
      Create_Window( obj );
      -- création d'un DC pour écriture en mémoire
      obj.memDC := Win32.Wingdi.CreateCompatibleDC( System.Null_Address );
      if obj.memdc = System.Null_address then
         Utils_pkg.Raise_fatal_error(obj.id, "Objects_pkg.Create_display: CreateCompatibleDC" );
      end if;
      -- Création du bitmap pour écriture en mémoire
      obj.bitmap := Win32.Wingdi.CreateCompatibleBitmap( Common_types.Main_dc,
                              nWidth  => Win32.INT(obj.inner_w),	-- taille intérieure
                              nHeight => Win32.INT(obj.inner_h) );
      if obj.bitmap = System.Null_address then
         Utils_pkg.Raise_fatal_error(obj.id, "Objects_pkg.Create_display: CreateCompatibleBitmap" );
      end if;
      -- sélection de la bitmap dans le DC
      old_obj := Win32.Wingdi.SelectObject( obj.memDC, obj.bitmap );
      -- police par défaut
      old_obj := Win32.Wingdi.SelectObject( obj.memDC, Resources_pkg.Small_font );
      -- effacement du fond avec la brush
      rect := ( top => 0, left => 0, right => Win32.LONG(obj.inner_w+1),
                bottom => Win32.LONG(obj.inner_h+1) );
      res_int := Win32.Winuser.FillRect( obj.memdc, Conversions.TO_ACRECT(rect'address), brush );

   end Create_Display;


   procedure Resize_display( obj : UI_object_ptr ) is
      res_bool : Win32.BOOL;
      res_int  : Win32.INT;
      old_obj  :  Win32.Windef.HGDIOBJ;
      rect     : Win32.Windef.RECT;
   begin
      -- calcul des taille intérieures
      Compute_inner( obj );
      -- création nouveau bitmap
      obj.bitmap := Win32.Wingdi.CreateCompatibleBitmap( Main_DC,
                              nWidth  => Win32.INT(obj.inner_w),
                              nHeight => Win32.INT(obj.inner_h) );
      -- sélection de la bitmap dans le DC
      old_obj := Win32.Wingdi.SelectObject( obj.memDC, obj.bitmap );
      -- destruction ancien bitmap
      res_bool := Win32.Wingdi.DeleteObject( old_obj );
      -- effacement du fond si une brush est spécifiée
      if obj.brush /= N_A then
         rect := ( top => 0, left => 0, right => Win32.LONG(obj.inner_w+1),
                    bottom => Win32.LONG(obj.inner_h+1) );
         res_int := Win32.Winuser.FillRect( obj.memdc, Conversions.TO_ACRECT(rect'address), obj.brush );
      end if;
   end Resize_display;


   -- affichage du contenu du disply (son bitmap) à l'écran
   procedure Display_display( obj : UI_object_ptr ) is
      res_bool  : Win32.BOOL;
   begin
      res_bool := Win32.Wingdi.BitBlt(
                    obj.DC, 			-- DC écran
                    Win32.INT(obj.bord_x),
                    Win32.INT(obj.bord_y),			-- décalage du à la bordure (peut etre 0,0)
                    Win32.INT(obj.inner_w), Win32.INT(obj.inner_h),
                    obj.memDC, 			-- DC mémoire
                    0,0,
                    Win32.Wingdi.SRCCOPY );
   end Display_display;

   -- *************************************************************************************

   -- appelé pour redimensionné les windows quand le calcul des dimensions et des positions
   -- est achevé dans Layout_pkg
   procedure Resize_all is
      tmp      : UI_object_ptr := object_list;
      res_bool : Win32.BOOL;
   begin
      while tmp /= null loop
         -- deplace l'objet windows associé
         res_bool := Win32.Winuser.MoveWindow( tmp.hwnd,
                         Win32.INT(tmp.X),
                         Win32.INT(tmp.Y),
                         Win32.INT(tmp.Largeur),
                         Win32.INT(tmp.Hauteur),
                         1 );	--repaint = true
         -- redimensionne les displays quand nécessaire
         if tmp.resize then
            case tmp.kind is
               when Display => Resize_display( tmp );
               when Fader   => Resize_fader( tmp );
               when others  => null;
            end case;
         end if;
         -- suivant
         tmp := tmp.next;
      end loop;
   end Resize_all;


   -- =====================================================================================


   -- appelé pour mettre à jour les objets après un changement de bitmap
   procedure Change_all_bitmaps is
      obj : UI_object_ptr := object_list;
   begin
      while obj /= null loop
         case obj.kind is
            when Fader   => Compute_Fader( obj );
            when Button  => Compute_button( obj );
            when Text | Display | Bistable | Num_edit => Compute_Outer( obj );
         end case;
         obj := obj.next;
      end loop;
   end Change_all_bitmaps;

   -- ****************************************************************************************

   -- forward declaration
   procedure Display_object( obj : UI_object_ptr );

   procedure Set_Frame( Id : Resources_pkg.obj_id_type; frames : frame_set ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      obj.frames := frames;
      Display_object( obj );
   end Set_Frame;


   procedure Set_button_rect( Id : Resources_pkg.obj_id_type; offset_x, offset_y : integer ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      Set_button_rect( obj, offset_x, offset_y );
   end Set_button_rect;


   procedure Set_Fader_geometry( Id : Resources_pkg.obj_id_type; min_1, min_2 : integer ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      -- copie les valeurs
      obj.min_1 := min_1;
      obj.min_2 := min_2;
      -- s'assure que le curseur est dans le rectangle
      Set_Cursor_position( obj );
   end Set_Fader_geometry;


   -- ****************************************************************************************

   procedure Invalidate( Id : Resources_pkg.obj_id_type ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      case obj.kind is
         when fader =>
            obj.fader_state := fader_Inval;
            Display_fader( obj );
         when button =>
            obj.but_state := but_Inval;
            Display_button( obj );
         when others => null;
      end case;
   end Invalidate;


   procedure Validate( Id : Resources_pkg.obj_id_type ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      case obj.kind is
         when fader =>
            obj.fader_state := fader_off;
            Display_fader( obj );
         when button =>
            obj.but_state := but_off;
            Display_button( obj );
         when others => null;
      end case;
   end Validate;


   procedure Set_Text( Id : Resources_pkg.obj_id_type; text : string ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      -- calcul de la longueur, tronquée si plus grand que le buffer
      obj.length := min( obj.buffer'length, text'length );
      -- copie la slice dans le buffer
      obj.buffer(1..obj.length) := text(text'first..text'first-1+obj.length);
      Display_text( obj );
   end Set_Text;


   procedure Set_Fader_value( Id : Resources_pkg.obj_id_type; new_value : integer ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      -- copie la valeur en restant dans l'intervalle autorisé
      obj.value := max( min( new_value, obj.max_val ), obj.min_val );
      -- déplacement du curseur pour correspondre à la nouvelle valeur
      Set_Cursor_position( obj );
      -- mise à jour de l'affichage
      Display_fader( obj );
   end Set_Fader_value;


   function Fader_value( Id : Resources_pkg.obj_id_type ) return integer is
      obj : UI_object_ptr := Object_of(Id);
   begin
      return obj.value;
   end Fader_value;

   procedure Fader_Set_Min_Max( Id : Resources_pkg.obj_id_type; min_val, max_val : integer ) is
      obj : UI_object_ptr := Object_of(Id);
   begin
      obj.min_val := min_val;
      obj.max_val := max_val;
      -- s'assure que la valeur est dans l'intervalle
      obj.value := max( min(obj.value, max_val), min_val );
      -- déplacement du curseur pour correspondre à la nouvelle valeur
      Set_Cursor_position( obj );
   end Fader_Set_Min_Max;

   -- Dit si un bouton est valide
   -- Lève CE si l'objet n'est pas un bouton
   function Is_valid( Id : Resources_pkg.obj_id_type ) return boolean is
      obj : UI_object_ptr := Object_of(Id);
   begin
      return obj.but_state /= but_inval;
   end Is_valid;

   -- ****************************************************************************************


   procedure Create_Objects_Class is
      Wnd_Class : Win32.Winuser.WNDCLASS;
      res_atom  : Win32.Windef.ATOM;
      err_code : Win32.DWORD;
   begin
      --
      -- classe pour les objets
      --
      Wnd_Class.style         := Win32.Winuser.CS_HREDRAW or Win32.Winuser.CS_VREDRAW
                                    or Win32.Winuser.CS_OWNDC;
      Wnd_Class.lpfnWndProc   := Objects_Proc'Access;	-- callback des évènements
      Wnd_Class.cbClsExtra    := 0;
      Wnd_Class.cbWndExtra    := 0;
      Wnd_Class.hInstance     := Common_types.hInst;
      Wnd_Class.hCursor       := Win32.Winuser.LoadCursor ( System.Null_Address, Win32.LPCSTR(Win32.Winuser.IDC_ARROW) );
      Wnd_Class.hIcon         := System.Null_Address;
      Wnd_Class.hbrBackground := System.Null_address;
      Wnd_Class.lpszClassName := Conversions.TO_PCCH( CantaUIObjClass'address );
      -- enregistrement de la classe
      res_atom := Win32.Winuser.RegisterClass( Conversions.TO_LPWNDCLASS(Wnd_Class'address) );
      --
      if res_atom = 0 then
         err_code := Win32.Winbase.GetLastError;
         Utils_pkg.Error_box( Intl.err_class_title,
                              Intl.err_class_txt & "Objects, code=" & Win32.DWORD'image(err_code) );
         raise fatal_error;
      end if;
   end Create_Objects_Class;

   -- ****************************************************************************************

   procedure Display_Frame( obj : UI_object_ptr ) is
   begin
      -- les 4 coins
      -- coin haut gauche
      Bitmap_pkg.Affiche_transparent( obj.dc, obj.frames(corner_lt),
                                      0,
                                      0,
                                      Win32.INT(obj.X),
                                      Win32.INT(obj.Y) );
       -- coin haut droite
      Bitmap_pkg.Affiche_transparent( obj.dc, obj.frames(corner_rt),
                                      Win32.INT(obj.largeur - obj.bord_x),
                                      0,
                                      Win32.INT(obj.X),
                                      Win32.INT(obj.Y)  );
      -- coin bas gauche
      Bitmap_pkg.Affiche_transparent( obj.dc, obj.frames(corner_lb),
                                      0,
                                      Win32.INT(obj.hauteur - obj.bord_y),
                                      Win32.INT(obj.X),
                                      Win32.INT(obj.Y) );
      -- coin bas droite
      Bitmap_pkg.Affiche_transparent( obj.dc, obj.frames(corner_rb),
                                      Win32.INT(obj.largeur - obj.bord_x),
                                      Win32.INT(obj.hauteur - obj.bord_y),
                                      Win32.INT(obj.X),
                                      Win32.INT(obj.Y) );
      -- les 4 bords
      -- bord haut
      Bitmap_pkg.Etire_bitmap_H( obj.dc, obj.frames(border_t),
                                 X       => Win32.INT(obj.bord_x),
                                 Y       => 0,
                                 largeur => Win32.INT(obj.largeur - 2*obj.bord_x) );
      -- bord bas
      Bitmap_pkg.Etire_bitmap_H( obj.dc, obj.frames(border_b),
                                 X       => Win32.INT(obj.bord_x),
                                 Y       => Win32.INT(obj.hauteur - obj.bord_y),
                                 largeur => Win32.INT(obj.largeur - 2*obj.bord_x) );
      -- bord gauche
      Bitmap_pkg.Etire_bitmap_V( obj.dc, obj.frames(border_l),
                                 X       => 0,
                                 Y       => Win32.INT(obj.bord_y),
                                 hauteur => Win32.INT(obj.hauteur - 2*obj.bord_y) );
      -- bord droite
      Bitmap_pkg.Etire_bitmap_V( obj.dc, obj.frames(border_r),
                                 X       => Win32.INT(obj.largeur - obj.bord_x),
                                 Y       => Win32.INT(obj.bord_y),
                                 hauteur => Win32.INT(obj.hauteur - 2*obj.bord_y) );
   end Display_Frame;


   -- dispatching de l'affichage des objet + affichage du cadre
   procedure Display_object( obj : UI_object_ptr ) is
   begin
      case obj.kind is
         when Fader    => Display_fader( obj );
         when Button   => Display_button( obj );
         when Text     => Display_text( obj );
         when Display  => Display_display( obj );
         when Bistable => Display_bistable( obj );
         when Num_edit => Display_num_edit( obj );
      end case;
      if obj.framed then
         Display_frame( obj );
      end if;
   end Display_object;


   procedure Display( Id : Resources_pkg.obj_id_type ) is
   begin
      Display_object( Object_of(Id) );
   end Display;


   -- notification applicative
   procedure Notify_Parent( obj   : UI_object_ptr;
                            code  : Win32.WPARAM;
                            param : Win32.LPARAM := 0 ) is
      resu_long : Win32.LONG;
   begin
      resu_long := Win32.Winuser.SendMessage(
                           Common_types.Win_hwnd,		-- la main window
                           code,				-- code notification
                           Win32.UINT(obj_Id_type'pos(obj.Id)),	-- Id de l'objet
                           param );					-- inutilisé
   end Notify_Parent;


   function Objects_Proc(hwnd    : Win32.Windef.HWND;
                         message : Win32.UINT;
                         wParam  : Win32.WPARAM;
			 lParam  : Win32.LPARAM) return Win32.LRESULT is
      obj : UI_object_ptr := Object_of( hwnd );	-- l'object concerné
      ps : aliased Win32.Winuser.PAINTSTRUCT;
      hdc : Win32.Windef.HDC;
      mouse_x, mouse_y : Win32.SHORT;
      old : Win32.Windef.HWND;
      res_bool : Win32.BOOL;
   begin
      if obj = null then
         return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);
      end if;
      --
      case message is

         when Win32.Winuser.WM_PAINT  | Win32.Winuser.WM_ERASEBKGND =>
            hdc := Win32.Winuser.BeginPaint( hwnd, ps'access );
            -- redessine l'objet
            Display_object( obj );
            res_bool := Win32.Winuser.EndPaint( hwnd, ps'access);
            -- cas special du compteur
            if obj.ID = Resources_pkg.COMPTEUR_ID then
               -- envoie message à la main window pour réaffichage différé du contenu
               Notify_Parent( obj, Common_types.PAINT_STATUS );
            end if;


         when Win32.Winuser.WM_LBUTTONDOWN =>
            case obj.kind is

               when fader =>
                  -- position de la souris
                  Conversions.Split_short( LParam, hi => mouse_y, low => mouse_x );
                  -- si souris est sur curseur, la capturer et la suivre
                  if integer(mouse_x) in obj.curs_X..obj.curs_X+obj.curs_large and then
                     integer(mouse_y) in obj.curs_Y..obj.curs_Y+obj.curs_haut
                  then
                     obj.fader_state := fader_on;		-- changer d'état
                     old := Win32.Winuser.SetCapture( hwnd );	-- capture la souris
                     -- stocke le décalage de la souris / curseur
                     Set_mouse_offset( obj, integer(mouse_x), integer(mouse_y) );
                     -- affichage du fader
                     Display_Fader( obj );
                  end if;
               	  --

               when text => null;

               when display =>
                  -- pour Mesure: seek player, et Score : debut de note
                  Notify_parent( obj, Win32.Winuser.WM_LBUTTONDOWN, lParam );

               when button =>
                  if obj.but_state /= but_Inval then
                     obj.but_state := but_Clic;
                  end if;
                  Display_button( obj );

               when bistable =>
                  Change_state( obj );
                  Display_bistable( obj );
                  Notify_parent( obj, Common_types.BISTABLE_CHANGED );

               when num_edit =>
                  -- capture la souris
                  old := Win32.Winuser.SetCapture( hwnd );
                  obj.capture_on := true;
                  obj.base := obj.number;	-- base de changement
                  obj.moved := false;

            end case;

         when Win32.Winuser.WM_LBUTTONUP =>
            case obj.kind is

               when fader =>
                  if obj.fader_state = fader_on then
                     -- changer d'état
                     obj.fader_state := fader_off;
                     -- relacher la souris
                     res_bool := Win32.Winuser.ReleaseCapture;
                     -- affichage du fader
                     Display_Fader( obj );
                  end if;

               when text => null;

               when display =>
                  -- pour Score : arrêt de la note
                  Notify_parent( obj, Win32.Winuser.WM_LBUTTONUP, lParam );

               when button =>
                  if obj.but_state /= but_Inval then
                     obj.but_state := but_off;
                     Display_button( obj );
                     Notify_parent( obj, Common_types.BUTTON_CLICKED );
                  end if;

               when bistable => null;

               when num_edit =>
                  -- relacher la souris
                  res_bool := Win32.Winuser.ReleaseCapture;
                  obj.capture_on := false;
                  -- position de la souris
                  Conversions.Split_short( LParam, hi => mouse_y, low => mouse_x );
                  -- suivre la souris et recalcule la valeur associée
                  Num_Compute_value( obj, integer(mouse_y) );
                  -- affichage du fader
                  Display_num_edit( obj );
                  -- signaler le changement de valeur
                  Notify_parent( obj, Common_types.NUM_CHANGED );

            end case;

         when Win32.Winuser.WM_MOUSEMOVE =>
            case obj.kind is

               when fader =>
                  if obj.fader_state = fader_on then
                     -- position de la souris
                     Conversions.Split_short( LParam, hi => mouse_y, low => mouse_x );
                      -- positionne le curseur pour suivre la souris et recalcule la valeur associée
                      Move_cursor( obj, integer(mouse_x), integer(mouse_y) );
                      -- affichage du fader
                      Display_Fader( obj );
                      -- notifie la main pour application de la nouvelle valeur
                      Notify_parent( obj, Common_types.FADER_VALUE_CHANGED );
                  end if;

               when text => null;

               when display =>
                  if obj.Id = SCORE_ID then
                     -- on teste la position de la souris
                     Conversions.Split_short( LParam, hi => mouse_y, low => mouse_x );
                     if mouse_x < 0 or else mouse_x > Win32.SHORT(obj.largeur)
                        or else mouse_y < 0 or else mouse_Y > Win32.SHORT(obj.hauteur) then
                        -- on relache la souris
                        res_bool := Win32.Winuser.ReleaseCapture;
                        -- Notification de la main pour arrêt note si nécessaire
                        -- simule un "mouse button up"
                         Notify_parent( obj, Win32.Winuser.WM_LBUTTONUP );
                     else
                        -- capture la souris pour savoir quand elle quitte le rectangle
                        old := Win32.Winuser.SetCapture( hwnd );
                     end if;
                  end if;

               when button =>
                  case obj.but_state is
                     when but_Off =>
                        -- capture la souris pour savoir quand elle quitte le rectangle
                        old := Win32.Winuser.SetCapture( hwnd );	-- capture la souris
                        -- change l'état
                        obj.but_state := but_On;
                        Display_Button( obj );
                     when but_On | but_Clic =>
                        -- on teste la position de la souris
                        Conversions.Split_short( LParam, hi => mouse_y, low => mouse_x );
                        if mouse_x < 0 or else mouse_x > Win32.SHORT(obj.largeur)
                           or else mouse_y < 0 or else mouse_Y > Win32.SHORT(obj.hauteur) then
                           -- on relache la souris
                           res_bool := Win32.Winuser.ReleaseCapture;
                           -- on repasse à l'état éteint
                           obj.but_state := but_Off;
                           Display_button( obj );
                        end if;
                     when but_Inval =>
                        null;
                  end case;

               when bistable => null;

               when num_edit =>
                  if obj.capture_on then
                     -- se souvenir qu'il y a eu déplacement
                     obj.moved := true;
                     -- position de la souris
                     Conversions.Split_short( LParam, hi => mouse_y, low => mouse_x );
                     -- suivre la souris et recalcule la valeur associée
                     Num_Compute_value( obj, integer(mouse_y) );
                     -- affichage du fader
                     Display_num_edit( obj );
                  end if;

            end case;

         when WM_CAPTURECHANGED =>
            case obj.kind is
               when fader =>
                  if obj.fader_state = fader_On then
                     -- on relache la souris
                     res_bool := Win32.Winuser.ReleaseCapture;
                  end if;
                  -- on repasse à l'état éteint
                  obj.fader_state := fader_Off;
                  Display_Fader( obj );
               when text => null;
               when display => null;
               when button =>
                  if obj.but_state = but_On then
                     -- on relache la souris
                     res_bool := Win32.Winuser.ReleaseCapture;
                  end if;
                  -- on repasse à l'état éteint
                  obj.but_state := but_Off;
                  Display_button( obj );
               when bistable => null;
               when num_edit => null;
	    end case;

         -- autres messages
         when others =>
            return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);

      end case;

      return 0;
   exception
      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
         Utils_pkg.Error_box( Intl.def_err_title, Intl.err_exception);
         return 0;
   end Objects_Proc;


end Objects_pkg;

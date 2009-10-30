with Interfaces.C;	use Interfaces.C;
with System;		use System;

with Win32;
with Win32.Winuser;
with Win32.Windef;	use Win32.Windef;
with Win32.Winbase;
with Win32.Wingdi;

with Common_types;	use Common_types;
with Conversions;	use Conversions;
with Bitmap_pkg;
with Resources_pkg;	use Resources_pkg;
with Config_pkg;
with Utils_pkg;
with Intl;
with Skins_pkg;

package body Display_Pkg is


   -- string pour la class des display
   DisplayCLASS : constant String := Config_pkg.Appli_name & "Display" & ASCII.nul;


   -- liste des display
   display_list : display_info_ptr;

   -- taille des bords
   bord_width, bord_height : Win32.INT := 0;

   -- *******************************************************************************


   function Display_of( id : Resources_pkg.obj_Id_type ) return display_info_ptr is
      tmp : display_info_ptr := display_list;
   begin
      while tmp /= null and then tmp.Id /= Id loop
         tmp := tmp.next;
      end loop;
      return tmp;
   end Display_of;


   function Display_of( hwnd : Win32.Windef.HWND ) return display_info_ptr is
      tmp : display_info_ptr := display_list;
   begin
      while tmp /= null and then tmp.hwnd /= hwnd loop
         tmp := tmp.next;
      end loop;
      return tmp;
   end Display_of;


   -- ===================================================================================

   procedure Affiche_display( display : display_info_ptr) is
      res_bool  : Win32.BOOL;
   begin
      if display.mince then
         res_bool := Win32.Wingdi.BitBlt(
                    display.screenDC, 			-- DC écran
                    1,
                    1,					-- décalage du à la bordure
                    display.Largeur, display.hauteur,
                    display.memDC, 			-- DC mémoire
                    0,0,
                    Win32.Wingdi.SRCCOPY );
      else
         res_bool := Win32.Wingdi.BitBlt(
                    display.screenDC, 			-- DC écran
                    bord_width,
                    bord_height,		-- décalage du à la bordure
                    display.Largeur, display.hauteur,
                    display.memDC, 			-- DC mémoire
                    0,0,
                    Win32.Wingdi.SRCCOPY );
      end if;
   end Affiche_display;

   procedure Paint_frame( hdc : Win32.Windef.HDC; X, Y, largeur, hauteur : Win32.INT ) is
   begin
      -- les 4 coins
      Bitmap_pkg.Affiche_transparent( hdc, Resources_pkg.DISP_CHG_ID, -- coin haut gauche
                                      0, 0,
                                      X, Y );
      Bitmap_pkg.Affiche_transparent( hdc, Resources_pkg.DISP_CHD_ID, -- coin haut droite
                                      largeur - bord_width, 0,
                                      X, Y  );
      Bitmap_pkg.Affiche_transparent( hdc, Resources_pkg.DISP_CBG_ID, -- coin bas gauche
                                      0, hauteur - bord_height,
                                      X, Y );
      Bitmap_pkg.Affiche_transparent( hdc, Resources_pkg.DISP_CBD_ID, 	-- coin bas droite
                                      largeur - bord_width, hauteur - bord_height,
                                      X, Y );
      -- les 4 bords
      Bitmap_pkg.Etire_bitmap_H( hdc, Resources_pkg.DISP_BH_ID, 	-- bord haut
                                 bord_width, 0,
                                 largeur - 2*bord_width );
      Bitmap_pkg.Etire_bitmap_H( hdc, Resources_pkg.DISP_BB_ID, 	-- bord bas
                                 bord_width, hauteur - bord_height,
                                 largeur - 2*bord_width );
      Bitmap_pkg.Etire_bitmap_V( hdc, Resources_pkg.DISP_BG_ID, 	-- bord gauche
                                 0, bord_height,
                                 hauteur - 2*bord_height );
      Bitmap_pkg.Etire_bitmap_V( hdc, Resources_pkg.DISP_BD_ID, 	-- bord droite
                                 largeur - bord_width, bord_height,
                                 hauteur - 2*bord_height );
   end Paint_Frame;


   procedure Re_paint( hwnd : Win32.Windef.HWND; Hdc : Win32.Windef.HDC ) is
      display : display_info_ptr := Display_of( hwnd );
      res_bool : Win32.BOOL;
      old_obj : Win32.WIndef.HGDIOBJ;
   begin
      if display.mince then
         -- bord noir à droite et en haut
         old_obj :=  Win32.Wingdi.SelectObject( Hdc, Resources_pkg.Pen_noir );
         res_bool := Win32.Wingdi.MoveToEx( Hdc, 0, display.hauteur, null );
         res_bool := Win32.Wingdi.LineTo( Hdc, 0, 0 );
         res_bool := Win32.Wingdi.LineTo( Hdc, display.largeur, 0 );
      else
         -- bord large de display
         Paint_frame( hdc, display.X, display.Y, display.largeur+2*bord_width, display.hauteur+2*bord_height );
      end if;
      --
      Affiche_display( display );
   end Re_Paint;

   -- ===================================================================================

   procedure Get_bitmap_size is
   begin
      -- taille des bitmaps des bords
      bord_width  := Win32.INT(Bitmap_pkg.Largeur( Resources_pkg.DISP_CHG_ID ));
      bord_height := Win32.INT(Bitmap_pkg.Hauteur( Resources_pkg.DISP_CHG_ID ));
   end Get_bitmap_size;


   function User_rect( largeur, hauteur : Win32.INT ) return Win32.Windef.RECT is
      fond_rect : Win32.Windef.RECT;
   begin
      fond_rect.top := Win32.LONG( bord_height );
      fond_rect.left:= Win32.LONG( bord_width );
      fond_rect.bottom := Win32.LONG( hauteur  - 2*bord_height );
      fond_rect.right  := Win32.LONG( largeur - 2*bord_width );
      return fond_rect;
   end User_rect;


   -- ===================================================================================

   function Display_Proc(hwnd    : Win32.Windef.HWND;
                         message : Win32.UINT;
                         wParam  : Win32.WPARAM;
			 lParam  : Win32.LPARAM)
			 		return Win32.LRESULT;
   pragma Convention (Stdcall, Display_Proc);


   function Display_Proc(hwnd    : Win32.Windef.HWND;
                         message : Win32.UINT;
                         wParam  : Win32.WPARAM;
			 lParam  : Win32.LPARAM)
			 return Win32.LRESULT is
      res_bool : Win32.BOOL;
      Hdc : Win32.Windef.HDC;
      ps : aliased Win32.Winuser.PAINTSTRUCT;
      display : display_info_ptr;
      result : Win32.LRESULT;
   begin

      case message is

         when Win32.Winuser.WM_PAINT =>
            -- dessine les cadres
            Hdc := Win32.Winuser.BeginPaint( hwnd, ps'access );
            Re_Paint( hwnd, hdc );
            res_bool := Win32.Winuser.EndPaint( hwnd, ps'access );
            -- cas spécial du compteur
            display := Display_of( hwnd );
            if display.ID = Resources_pkg.COMPTEUR_ID then
               -- envoie message à la main window pour réaffichage différé du contenu
               res_bool := Win32.Winuser.PostMessage( Common_types.Win_hwnd, Common_types.PAINT_STATUS, 0, 0 );
            end if;

         -- bouton gauche up
         when Win32.Winuser.WM_LBUTTONUP =>
            -- cas spécial du display des mesures
            display := Display_of( hwnd );
            -- envoie message à la main window pour execution immédiate
            result := Win32.Winuser.SendMessage( Common_types.Win_hwnd, Win32.Winuser.WM_LBUTTONUP,
                              Win32.WPARAM(Resources_pkg.obj_id_type'pos(display.Id)), 	-- display concerné
                              lParam );	-- position de la souris

         -- bouton gauche up
         when Win32.Winuser.WM_LBUTTONDOWN =>
            -- cas spécial du display des mesures
            display := Display_of( hwnd );
            -- envoie message à la main window pour execution immédiate
            result := Win32.Winuser.SendMessage( Common_types.Win_hwnd, Win32.Winuser.WM_LBUTTONDOWN,
                               Win32.WPARAM(Resources_pkg.obj_id_type'pos(display.Id)), -- display concerné
                               lParam );-- position de la souris


	 -- autres
         when others =>
            return Win32.Winuser.DefWindowProc (hwnd, message, wParam, lParam);

      end case;

      return 0;

   end Display_Proc;



   -- ===================================================================================



   procedure Create_Display_Class is
      Wnd_Class : Win32.Winuser.WNDCLASS;
      res_atom  : Win32.Windef.ATOM;
      err_code : Win32.DWORD;
   begin
      --
      -- classe pour les Display
      --
      Wnd_Class.style      := Win32.Winuser.CS_HREDRAW or Win32.Winuser.CS_VREDRAW
                                    or Win32.Winuser.CS_OWNDC;
      Wnd_Class.lpfnWndProc:= Display_Proc'Access;
      Wnd_Class.cbClsExtra := 0;
      Wnd_Class.cbWndExtra := 0;
      Wnd_Class.hInstance  := Common_types.hInst;
      Wnd_Class.hIcon      := System.Null_Address;
      Wnd_Class.hCursor    := Win32.Winuser.LoadCursor ( System.Null_Address, Win32.LPCSTR(Win32.Winuser.IDC_ARROW) );
      Wnd_Class.hbrBackground := System.Null_address;
      Wnd_Class.lpszClassName := Conversions.TO_PCCH( DisplayCLASS'address );
      -- enregistrement de la classe
      res_atom := Win32.Winuser.RegisterClass( TO_LPWNDCLASS(Wnd_Class'address) );
      --
      if res_atom = 0 then
         err_code := Win32.Winbase.GetLastError;
         Utils_pkg.Error_box( Intl.err_class_title,
                              Intl.err_class_txt & "Display, code=" & Win32.DWORD'image(err_code) );
         raise fatal_error;
      end if;
      -- chargement des bitmaps
      Get_bitmap_size;
   end Create_Display_Class;



   function New_Display( X, Y, largeur, hauteur : Integer;
                         Parent       : Win32.Windef.HWND;
                         Id           : Resources_pkg.obj_Id_type;
                         fond         : Win32.Windef.HBRUSH;
                         bords_minces : boolean := false ) return Win32.Windef.HWND is
      tmp : display_info_ptr;
      res_int : Win32.INT;
      res_bool : Win32.BOOL;
      Parent_hdc : Win32.Windef.HDC;
      err_code : Win32.DWORD;
      old_obj : Win32.Windef.HGDIOBJ;
      rect: Win32.Windef.RECT;
   begin
      -- nouvelle entrée dans la liste: insertion en début
      tmp := new display_info;
      tmp.next := display_list;
      display_list := tmp;
      -- initialisation position et taille
      tmp.X := Win32.INT(X)-bord_width;
      tmp.Y := Win32.INT(Y)-bord_height;
      tmp.Largeur := Win32.INT(largeur)+2*bord_width;
      tmp.Hauteur := Win32.INT(hauteur)+2*bord_height;
      tmp.Id := Id;
      tmp.Parent := Parent;
      tmp.fond := fond;
      tmp.mince := bords_minces;
      -- creation de l'objet windows
      if bords_minces then
         -- bords minces: 1 pixel en haut et 1 pixel à droite
         tmp.hwnd := Win32.Winuser.CreateWindow(
              lpClassName  => Conversions.TO_PCCH(DisplayCLASS'address),
              lpWindowName => Conversions.TO_PCCH(System.Null_Address),
              dwStyle      => Win32.Winuser.WS_CHILDWINDOW or Win32.Winuser.WS_VISIBLE
                                  or Win32.Winuser.WS_CLIPSIBLINGS,
              X            => tmp.X-1,
              Y            => tmp.Y-1,
              nWidth       => tmp.Largeur,
              nHeight      => tmp.Hauteur,
              hWndParent   => Parent,			-- handle du parent
              hMenu        => TO_HWND(Win32.UINT(obj_Id_type'pos(Id))),				-- identifier
              hInstance    => Common_types.hInst,
              lpParam      => System.Null_Address);
      else
         -- bords épais sur les 4 côtés
         tmp.hwnd := Win32.Winuser.CreateWindow(
              lpClassName  => Conversions.TO_PCCH(DisplayCLASS'address),
              lpWindowName => Conversions.TO_PCCH(System.Null_Address),
              dwStyle      => Win32.Winuser.WS_CHILDWINDOW or Win32.Winuser.WS_VISIBLE
                                  or Win32.Winuser.WS_CLIPSIBLINGS,
              X            => tmp.X - Win32.INT(bord_width),
              Y            => tmp.Y - Win32.INT(bord_height),
              nWidth       => tmp.Largeur + 2 * Win32.INT(bord_width),
              nHeight      => tmp.Hauteur + 2 * Win32.INT(bord_height),
              hWndParent   => Parent,			-- handle du parent
              hMenu        => TO_HWND(Win32.UINT(obj_Id_type'pos(Id))),				-- identifier
              hInstance    => Common_types.hInst,
              lpParam      => System.Null_Address);
      end if;
      --
      if tmp.hwnd = System.Null_address then
         -- erreur, window pas créée
         err_code := Win32.Winbase.GetLastError;
         Utils_pkg.Error_box( Intl.err_init_txt,
                              Intl.err_display_txt & obj_Id_type'image(Id) & ", code="
                                  & Win32.DWORD'image(err_code) );
         raise fatal_error;
      end if;
      --
      -- récupération du DC
      tmp.screenDC := Win32.Winuser.GetDC( tmp.hwnd );
      -- création d'un DC pour écriture en mémoire
      tmp.memDC := Win32.Wingdi.CreateCompatibleDC( System.Null_Address );
      if tmp.memdc = System.Null_address then
         err_code := Win32.Winbase.GetLastError;
         raise fatal_error;
      end if;
      --
      -- Création du bitmap pour écriture en mémoire
      Parent_hdc := Win32.Winuser.GetDC( Parent );			-- DC du parent
      tmp.bitmap := Win32.Wingdi.CreateCompatibleBitmap( Parent_hdc,
                              nWidth  => tmp.Largeur,
                              nHeight => tmp.Hauteur );
      --
      -- sélection de la bitmap dans le DC
      old_obj := Win32.Wingdi.SelectObject( tmp.memDC, tmp.bitmap );
      -- effacement du fond
      rect := ( top => 0, left => 0, right => Win32.LONG(tmp.largeur+1),
                bottom => Win32.LONG(tmp.hauteur+1) );
      res_int := Win32.Winuser.FillRect( tmp.memdc, TO_ACRECT(rect'address), tmp.fond );
      --
      res_bool := Win32.Winuser.ShowWindow( tmp.hwnd, Win32.Winuser.SW_SHOW);
      res_bool := Win32.Winuser.UpdateWindow( tmp.hwnd );
      --
      return tmp.hwnd;
   end New_Display;


   procedure Resize( id : Resources_pkg.obj_Id_type; largeur, hauteur : Integer ) is
      tmp : display_info_ptr := Display_of( Id );
      res_bool : Win32.BOOL;
      res_int : Win32.INT;
      parent_hdc : Win32.Windef.HDC;
      old_obj : Win32.Windef.HGDIOBJ;
      rect: Win32.Windef.RECT;
   begin
      -- destruction ancien bitmap
      res_bool := Win32.Wingdi.DeleteObject( tmp.bitmap );
      -- stockage nouvelle taille
      tmp.Largeur := Win32.INT(largeur);
      tmp.Hauteur := Win32.INT(hauteur);
      -- création nouveau bitmap à la nouvelle taille
      Parent_hdc := Win32.Winuser.GetDC( tmp.Parent );			-- DC du parent
      tmp.bitmap := Win32.Wingdi.CreateCompatibleBitmap( Parent_hdc,
                              nWidth  => tmp.Largeur,
                              nHeight => tmp.Hauteur );
      -- sélection de la bitmap dans le DC
      old_obj := Win32.Wingdi.SelectObject( tmp.memDC, tmp.bitmap );
      -- effacement du fond
      rect := ( top => 0, left => 0, right => Win32.LONG(tmp.largeur+1),
                bottom => Win32.LONG(tmp.hauteur+1) );
      res_int := Win32.Winuser.FillRect( tmp.memdc, TO_ACRECT(rect'address), tmp.fond );
   end Resize;


end Display_Pkg;

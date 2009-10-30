with Unchecked_deallocation;
with System;		use System;
with Interfaces.C;	use Interfaces.C;

with Win32;		use Win32;
with Win32.Wingdi;
with Win32.Windef;
with Win32.Winuser;
with Win32.Winbase;

with Common_types;	use Common_types;
with Conversions;	use Conversions;
with Resources_pkg;	use Resources_pkg;
with Log;
with Byte_io;
with Utils_pkg;
with Intl;
with Layout_pkg;
with Skins_pkg;


package body Bitmap_pkg is


   -- liste des bitmaps créés
   bitmap_list : bitmap_info_ptr;

   file_header : array(1..14) of Win32.BYTE;
   info_header : array(1..40 ) of Win32.BYTE;
   bitmap_header : Win32.Wingdi.BITMAPINFOHEADER;
   for bitmap_header use at info_header'address;

   -- deallocation
   procedure Free is new Unchecked_deallocation( bitmap_info, bitmap_info_ptr );

   -- *********************************************************************************


   function Bitmap_of( Id : Resources_pkg.bmap_Id_type ) return bitmap_info_ptr is
      tmp : bitmap_info_ptr := bitmap_list;
   begin
      while tmp /= null and then tmp.Id /= Id loop
         tmp := tmp.next;
      end loop;
      return tmp;
   end Bitmap_of;


   -- ================================================================================

   procedure Create( Id : Resources_pkg.bmap_Id_type;
                     bitmap_header : Win32.Wingdi.BITMAPINFOHEADER;
                     data   : Win32.LPVOID ) is
      bits   : aliased Win32.LPVOID;  -- adresse ou sera stocké l'adresse de le DIB
      tmp : bitmap_info_ptr;
      bitmap: Win32.Windef.HBITMAP;
      pix_size : integer;
   begin
      -- calcul taille
      pix_size := Integer(bitmap_header.biWidth * bitmap_header.biHeight);
      -- creation de la bitmap
      bitmap := Win32.Wingdi.CreateDIBSection( System.Null_Address,
                            TO_ACBITMAPINFO(bitmap_header'address),
                            Win32.Wingdi.DIB_RGB_COLORS,
                            bits'access,			-- adresse du pointer sur les bits
                            System.Null_Address, 0 );		-- pas utilisé
      -- copie des bits dans la section
      declare
         source : Win32.UINT_array(1..pix_size);
         for source use at data;
         dest : Win32.UINT_array(1..pix_size);
         for dest use at bits;
      begin
         dest := source;
      end;
      --
      tmp := new bitmap_info;
      tmp.next := bitmap_list;
      bitmap_list := tmp;
      -- copie des info
      tmp.header := bitmap_header;
      tmp.bits := System.Address(bits);
      tmp.Id := Id;
      tmp.handle := bitmap;
   end Create;


   procedure Create_bitmap( nom : string;
                            Id : Resources_pkg.bmap_Id_type ) is
      data : Win32.LPVOID;	      -- adresse des bits de données
      bitmap_header : Win32.Wingdi.BITMAPINFOHEADER;
   begin
      -- récupération des données
      Resources_pkg.get_bitmap( nom, bitmap_header, data );
      -- création
      Create( Id, bitmap_header, data );
   end Create_bitmap;


   procedure Create_background is
      pix_size : integer;
      data : bitmap_info_ptr := Bitmap_pkg.Bitmap_of( MAIN_BACKGROUND_ID );
      old_obj : Win32.Windef.HGDIOBJ;
      res_int : Win32.INT;
      res_bool : Win32.BOOL;
      old_back : Win32.Windef.HBITMAP := Common_types.background;
   begin
      if data = null then
         return;
      end if;
      -- calcul taille
      pix_size := Layout_pkg.Window_width * Layout_pkg.Window_height;
      -- redéfini le header du background
      Common_types.Back_header := ( 40, 		-- structure size = constant
                    Win32.LONG(Layout_pkg.Window_width),
                    Win32.LONG(Layout_pkg.Window_height),
                    1, 32, 0, 			-- plane, count, compression
                    Win32.DWORD(pix_size), 	-- image size
                    2834, 2834, 0, 0 );		-- X et Y pix per meter, color used, color important
      -- creation de la bitmap
      Common_types.background := Win32.Wingdi.CreateDIBSection( System.Null_Address,
                            TO_ACBITMAPINFO(Common_types.Back_header'address),
                            Win32.Wingdi.DIB_RGB_COLORS,
                            Common_types.back_bits'access,	-- adresse du pointer sur les bits
                            System.Null_Address, 0 );		-- pas utilisé
      -- association avec le DC
      old_obj := Win32.Wingdi.SelectObject( Common_types.Back_DC, Common_types.background );
      -- copie des bits dans la section
      res_int := Win32.Wingdi.StretchDIBits(
                       Common_types.Back_DC,
                       0, 0,					-- X,Y destination
                       Win32.INT(Layout_pkg.Window_width),	-- W, H destination
                       Win32.INT(Layout_pkg.Window_height),
                       0, 0,					-- X, Y source
                       Win32.INT(data.header.biWidth),		-- W, H source
                       Win32.INT(data.header.biHeight),
                       data.bits,				-- bits sources
                       TO_ACBITMAPINFO(data.header'address),	-- adresse header source
                       Win32.Wingdi.DIB_RGB_COLORS,
                       Win32.Wingdi.SRCCOPY );
      -- desalloue le bitmap précédent
      res_bool := Win32.Wingdi.DeleteObject( old_back );
   end Create_background;


  -- *******************************************************************************


   procedure Affiche_bitmap( Win_HDC  : Win32.Windef.HDC;
                             Id       : Resources_pkg.bmap_Id_type;
                             X, Y     : Win32.INT ) is
      old_obj : Win32.Windef.HGDIOBJ;
      res_bool : Win32.BOOL;
      Source : Win32.Windef.HDC;
      err_code : Win32.ULONG;
      bitmap : bitmap_info_ptr;
   begin
      bitmap := Bitmap_of( Id );
      if bitmap = null then
         return;
      end if;
      -- Création du DC en mémoire
      Source := Win32.Wingdi.CreateCompatibleDC( Win_HDC );
      -- association avec la DC
      old_obj := Win32.Wingdi.SelectObject( Source, bitmap.handle );
      -- affichage
      res_bool := Win32.Wingdi.BitBlt(
                    hdcDest => Win_HDC,
                    nXDest  => X,
                    nYDest  => Y,
                    nWidth  => Win32.INT(bitmap.header.biWidth),
                    nHeight => Win32.INT(bitmap.header.biHeight),
                    hdcSrc  => Source,
                    nXSrc   => 0,
                    nYSrc   => 0,
                    dwRop   => Win32.Wingdi.SRCCOPY );
      if res_bool = 0 then
         err_code := Win32.Winbase.GetLastError;
         Log.Store("Erreur GDI");Log.Store("Affiche_bitmap");
         Log.Store("code=" & Integer'image(Integer(err_code)));
         Log.End_line;
         raise fatal_error;
      end if;
      -- suppression du DC temporaire
      res_bool := Win32.Wingdi.DeleteDC( source );
   end Affiche_bitmap;


   procedure Etire_bitmap_H( Win_HDC   : Win32.Windef.HDC;
                             bitmap_id : Resources_pkg.bmap_Id_type;
                             X, Y      : Win32.INT;
                             largeur   : Win32.INT ) is
      res_bool : Win32.BOOL;
      bitmap : bitmap_info_ptr := Bitmap_of( bitmap_id );
      Source : Win32.Windef.HDC;
      old_obj : Win32.Windef.HGDIOBJ;
   begin
      if bitmap = null then
         return;
      end if;
      -- Création du DC en mémoire
      Source := Win32.Wingdi.CreateCompatibleDC( Win_HDC );
      -- association avec la DC
      old_obj := Win32.Wingdi.SelectObject( Source, bitmap.handle );
      --
      res_bool := Win32.Wingdi.StretchBlt ( Win_HDC,
                        nXOriginDes => X,
                        nYOriginDes => Y,
                        nWidthDest  => largeur,
                        nHeightDest => Win32.INT(bitmap.header.biHeight),
                        hdcSrc      => Source,
                        nXOriginSrc => 0,
                        nYOriginSrc => 0,
                        nWidthSrc   => Win32.INT(bitmap.header.biWidth),
                        nHeightSrc  => Win32.INT(bitmap.header.biHeight),
                        dwRop       => Win32.Wingdi.SRCCOPY );
      -- suppression du DC temporaire
      res_bool := Win32.Wingdi.DeleteDC( source );
   end Etire_bitmap_H;


   procedure Etire_bitmap_V( Win_HDC   : Win32.Windef.HDC;
                             bitmap_id : Resources_pkg.bmap_Id_type;
                             X, Y      : Win32.INT;
                             hauteur   : Win32.INT ) is
      res_bool : Win32.BOOL;
      bitmap : bitmap_info_ptr := Bitmap_of( bitmap_id );
      Source : Win32.Windef.HDC;
      old_obj : Win32.Windef.HGDIOBJ;
   begin
      if bitmap = null then
         return;
      end if;
      -- Création du DC en mémoire
      Source := Win32.Wingdi.CreateCompatibleDC( Win_HDC );
      -- association avec la DC
      old_obj := Win32.Wingdi.SelectObject( Source, bitmap.handle );
      --
      res_bool := Win32.Wingdi.StretchBlt ( Win_HDC,
                        nXOriginDes => X,
                        nYOriginDes => Y,
                        nWidthDest  => Win32.INT(bitmap.header.biWidth),
                        nHeightDest => hauteur,
                        hdcSrc      => Source,
                        nXOriginSrc => 0,
                        nYOriginSrc => 0,
                        nWidthSrc   => Win32.INT(bitmap.header.biWidth),
                        nHeightSrc  => Win32.INT(bitmap.header.biHeight),
                        dwRop       => Win32.Wingdi.SRCCOPY );
      -- suppression du DC temporaire
      res_bool := Win32.Wingdi.DeleteDC( source );
   end Etire_bitmap_V;

   procedure Etire_bitmap( Win_HDC   : Win32.Windef.HDC;
                           bitmap_id : Resources_pkg.bmap_Id_type;
                           X, Y      : Win32.INT;
                           largeur   : Win32.INT;
                           hauteur   : Win32.INT ) is
      res_bool : Win32.BOOL;
      bitmap : bitmap_info_ptr := Bitmap_of( bitmap_id );
      Source : Win32.Windef.HDC;
      old_obj : Win32.Windef.HGDIOBJ;
   begin
      if bitmap = null then
         return;
      end if;
      -- Création du DC en mémoire
      Source := Win32.Wingdi.CreateCompatibleDC( Win_HDC );
      -- association avec la DC
      old_obj := Win32.Wingdi.SelectObject( Source, bitmap.handle );
      --
      res_bool := Win32.Wingdi.StretchBlt ( Win_HDC,
                        nXOriginDes => X,
                        nYOriginDes => Y,
                        nWidthDest  => largeur,
                        nHeightDest => hauteur,
                        hdcSrc      => Source,
                        nXOriginSrc => 0,
                        nYOriginSrc => 0,
                        nWidthSrc   => Win32.INT(bitmap.header.biWidth),
                        nHeightSrc  => Win32.INT(bitmap.header.biHeight),
                        dwRop       => Win32.Wingdi.SRCCOPY );
      -- suppression du DC temporaire
      res_bool := Win32.Wingdi.DeleteDC( source );
   end Etire_bitmap;


   -- *********************************************************************************************

   -- Copie les pixels de 'source' dans 'destination' en utilisant le coefficient Alpha de 'source'
   -- aux coordonnées (X,Y) de 'destination'
   -- les pixels sont sur 4 octets au format 'ABGR', ie s(0)=Rouge,s(1)=Vert,s(2)=Bleu,s(3)=Alpha
   -- On doit avoir 0 <= X <= largeur_dest et 0 <= Y <= hauteur_dest
   -- Si X ou Y sont en dehors de destination, on ne fait rien
   -- Si la source est partiellement en dehors de destination, elle est tronquée
   procedure Transparent_copy( source         : System.address;
                               largeur_source : integer;
                               hauteur_source : integer;
                               destination    : System.address;
                               largeur_dest   : integer;
                               hauteur_dest   : integer;
                               X, Y           : integer ) is
      -- tableaux de pixels (4 bytes par pixel)
      src : common_types.byte_array(1..largeur_source * hauteur_source * 4);
      for src use at source;
      dest : common_types.byte_array(1..largeur_dest * hauteur_dest * 4);
      for dest use at destination;
      --
      base_dest, base_src : integer;
      nb_ligne, nb_col : integer;
      si, di : integer;
      alpha : integer;
      r_sour, r_dest, g_sour, g_dest, b_sour, b_dest : integer;
   begin
      if source = N_A or destination = N_A then
         return;
      end if;
      -- test X et Y
      if X < 0 or X > largeur_dest-1 then
         return;
      end if;
      if Y < 0 or Y > hauteur_dest-1 then
         return;
      end if;
      -- coupe si trop large ou trop haut
      if X + largeur_source > largeur_dest then
         nb_col := largeur_dest - X;
      else
         nb_col := largeur_source;
      end if;
      if Y + hauteur_source > hauteur_dest then
         nb_ligne := hauteur_dest - Y;
      else
         nb_ligne := hauteur_source;
      end if;
      --
      base_dest := (X + Y * largeur_dest) * 4 + 1;
      base_src := 1;
      for ligne in 1..nb_ligne loop
         -- index en début de ligne
         di := base_dest;
         si := base_src;
         -- copie la ligne
         for col in 1..nb_col loop
            alpha  := Integer(src(si+3));
            --
            if alpha = 255 then
               -- copie de la source
               dest(di)   := src(si);
               dest(di+1) := src(si+1);
               dest(di+2) := src(si+2);
            elsif alpha /= 0 then	-- si alpha = 0 on ne fait rien: le fond n'est pas modifié
               -- Red
               r_sour := Integer(src(si));
               r_dest := Integer(dest(di));
               dest(di)   := BYTE( (alpha * r_sour + (255-alpha) * r_dest) / 255 );
               -- Green
               g_sour := Integer(src(si+1));
               g_dest := Integer(dest(di+1));
               dest(di+1) := BYTE( (alpha * g_sour + (255-alpha) * g_dest) / 255 );
               -- Blue
               b_sour := Integer(src(si+2));
               b_dest := Integer(dest(di+2));
               dest(di+2) := BYTE( (alpha * b_sour + (255-alpha) * b_dest) / 255 );
               --
            end if;
            -- pixel suivant
            di := di + 4;
            si := si + 4;
         end loop;
         -- index pour la ligne suivantes
         base_dest := base_dest + largeur_dest * 4;
         base_src  := base_src + largeur_source * 4;
      end loop;
   end Transparent_copy;



   procedure Affiche_transparent( Win_HDC   : Win32.Windef.HDC;
                                  bitmap_id : Resources_pkg.bmap_Id_type;
                                  fond_id   : Resources_pkg.bmap_Id_type;
                                  X, Y      : Win32.INT ) is
      old_obj : Win32.Windef.HGDIOBJ;
      res_bool : Win32.BOOL;
      MemDC : Win32.Windef.HDC;
      err_code : Win32.ULONG;
      fond, bitmap : bitmap_info_ptr;
      tempo : Win32.Windef.HBITMAP;
      bitmap_header : Win32.Wingdi.BITMAPINFOHEADER;
      bits   : aliased Win32.LPVOID;
      pix_size : Integer;
   begin
      -- récupère info sur les bitmap (si pas trouvé CE sera levé plus loin)
      fond := Bitmap_of( fond_Id );
      bitmap := Bitmap_of( bitmap_id );
      if bitmap = null or fond = null then
         return;
      end if;
      --
      -- création zone de travail de même taille que le bitmap à afficher
      pix_size := Integer(bitmap.header.biWidth) * Integer(bitmap.header.biHeight);
      bitmap_header := bitmap.header;
      tempo := Win32.Wingdi.CreateDIBSection( System.Null_Address,
                            TO_ACBITMAPINFO(bitmap_header'address),
                            Win32.Wingdi.DIB_RGB_COLORS,
                            bits'access,			-- adresse du pointer sur les bits
                            System.Null_Address, 0 );		-- pas utilisé
      --
      declare
         source : common_types.byte_array(1..Integer(fond.header.biWidth * fond.header.biHeight) * 4);
         for source use at System.Address(fond.bits);
         --
         zone : common_types.byte_array(1..pix_size*4);
         for zone use at System.Address(bits);
         --
         origine : common_types.byte_array(1..pix_size*4);
         for origine use at System.Address(bitmap.bits);
         --
         sour, dest : integer;
         large : Integer;
         r_dest,g_dest,b_dest,alpha : Integer;
         r_sour, g_sour, b_sour : Integer;
         i : Integer;
      begin
         --
         -- copie du fond dans la zone de travail
         sour := ((Integer(fond.header.biheight) - Integer(Y) - Integer(bitmap.header.biHeight)) * Integer(fond.header.biWidth)
                          + Integer(X) ) * 4 + 1;	-- byte source
         dest := 1;         				-- byte destination
         large := Integer(bitmap.header.biWidth) * 4 -1;
         for h in 1..Integer(bitmap.header.biHeight) loop
            -- copie une ligne entière
            zone(dest..dest+large) := source(sour..sour+large);
            --
            dest := dest + large + 1;
            sour := sour + Integer(fond.header.biWidth) * 4;
         end loop;
         ---
         -- alpha blending de la bitmap sur le fond
         i := 1;
         for pix in 1..pix_size loop
            alpha  := Integer(origine(i+3));
            --
            if alpha = 255 then
               -- copie de la source
               zone(i) := origine(i);
               zone(i+1) := origine(i+1);
               zone(i+2) := origine(i+2);
            elsif alpha /= 0 then	-- si alpha = 0 on ne fait rien: le fond n'est pas modifié
               r_sour := Integer(origine(i));
               r_dest := Integer(zone(i));
               zone(i)   := BYTE( (alpha * r_sour + (255-alpha) * r_dest) / 255 );
               --
               g_sour := Integer(origine(i+1));
               g_dest := Integer(zone(i+1));
               zone(i+1) := BYTE( (alpha * g_sour + (255-alpha) * g_dest) / 255 );
               --
               b_sour := Integer(origine(i+2));
               b_dest := Integer(zone(i+2));
               zone(i+2) := BYTE( (alpha * b_sour + (255-alpha) * b_dest) / 255 );
               --
            end if;
            --
            i := i + 4;
         end loop;
      end;
      --
      -- Création du DC en mémoire
      MemDC := Win32.Wingdi.CreateCompatibleDC( Win_HDC );
      -- sélection de la zone de travail dans le DC
      old_obj := Win32.Wingdi.SelectObject( MemDC, tempo );
      -- affichage
      res_bool := Win32.Wingdi.BitBlt(
                    hdcDest => Win_HDC,
                    nXDest  => X,
                    nYDest  => Y,
                    nWidth  => Win32.INT(bitmap.header.biWidth),
                    nHeight => Win32.INT(bitmap.header.biHeight),
                    hdcSrc  => MemDC,
                    nXSrc   => 0,
                    nYSrc   => 0,
                    dwRop   => Win32.Wingdi.SRCCOPY );
     if res_bool = 0 then
        err_code := Win32.Winbase.GetLastError;
     end if;
     --
     -- suppression bitmap temporaire
     old_obj := Win32.Wingdi.SelectObject( MemDC, old_obj );
     res_bool := Win32.Wingdi.DeleteObject( tempo );
     -- suppression du DC temporaire
     res_bool := Win32.Wingdi.DeleteDC( MemDC );
   end Affiche_transparent;


   -- Affichage en transparent d'un bitmap sur le fond de la main window
   --
   procedure Affiche_transparent( Win_HDC   : Win32.Windef.HDC;
                                  bitmap_id : Resources_pkg.bmap_Id_type;
                                  X_local, Y_local : Win32.INT;		-- position dans la window local
                                  X_main, Y_main : Win32.INT ) is	-- position de la window locale dans la main
      old_obj : Win32.Windef.HGDIOBJ;
      res_bool : Win32.BOOL;
      err_code : Win32.ULONG;
      bitmap : bitmap_info_ptr;
      tempo_bmp : Win32.Windef.HBITMAP;
      tempo_DC : Win32.Windef.HDC;
      bitmap_header : Win32.Wingdi.BITMAPINFOHEADER;
      bits   : aliased Win32.LPVOID;
      pix_size : Integer;
   begin
      -- récupère info sur les bitmap (si pas trouvé CE sera levé plus loin)
      bitmap := Bitmap_of( bitmap_id );
      if bitmap = null then
         return;
      end if;
      --
      -- création zone de travail de même taille que le bitmap à afficher
      pix_size := Integer(bitmap.header.biWidth) * Integer(bitmap.header.biHeight);
      bitmap_header := bitmap.header;
      tempo_bmp := Win32.Wingdi.CreateDIBSection( System.Null_Address,
                            TO_ACBITMAPINFO(bitmap_header'address),
                            Win32.Wingdi.DIB_RGB_COLORS,
                            bits'access,			-- adresse du pointer sur les bits
                            System.Null_Address, 0 );		-- pas utilisé
      tempo_dc := Win32.Wingdi.CreateCompatibleDC( Common_types.back_DC );
      old_obj := Win32.Wingdi.SelectObject( tempo_dc, tempo_bmp );
      -- copy de la main sur la zone de travail
      -- remplissage avec la bitmap de fond étirée
      res_bool := Win32.Wingdi.BitBlt(
                            tempo_dc,
                            0, 0,				-- position dans zone de travail
                            Win32.INT(bitmap.header.biWidth),
                            Win32.INT(bitmap.header.biHeight),
                            Common_types.Back_DC,
                            X_main+X_local, Y_main+Y_local,	-- position du bitmap / main
                            Win32.Wingdi.SRCCOPY );
      -- copy du bitmap avec transparence
      declare
         zone : common_types.byte_array(1..pix_size*4);	-- zone d'écriture dans tempo_bmp
         for zone use at System.Address(bits);
         --
         origine : common_types.byte_array(1..pix_size*4);	-- bits de la bitmap à copier
         for origine use at System.Address(bitmap.bits);
         --
         r_dest,g_dest,b_dest,alpha : Integer;
         r_sour, g_sour, b_sour : Integer;
         i : Integer;
      begin
         --
         -- alpha blending de la bitmap sur le fond
         i := 1;
         for pix in 1..pix_size loop
            alpha  := Integer(origine(i+3));
            --
            if alpha = 255 then
               -- copie de la source
               zone(i) := origine(i);
               zone(i+1) := origine(i+1);
               zone(i+2) := origine(i+2);
            elsif alpha /= 0 then	-- si alpha = 0 on ne fait rien: le fond n'est pas modifié
               r_sour := Integer(origine(i));
               r_dest := Integer(zone(i));
               zone(i)   := BYTE( (alpha * r_sour + (255-alpha) * r_dest) / 255 );
               --
               g_sour := Integer(origine(i+1));
               g_dest := Integer(zone(i+1));
               zone(i+1) := BYTE( (alpha * g_sour + (255-alpha) * g_dest) / 255 );
               --
               b_sour := Integer(origine(i+2));
               b_dest := Integer(zone(i+2));
               zone(i+2) := BYTE( (alpha * b_sour + (255-alpha) * b_dest) / 255 );
               --
            end if;
            --
            i := i + 4;
         end loop;
      end;
      --
      -- affichage: tansfert dans la fenetre cible
      res_bool := Win32.Wingdi.BitBlt(
                    hdcDest => Win_HDC,
                    nXDest  => X_local,		-- position dans la fenetre cible
                    nYDest  => Y_local,
                    nWidth  => Win32.INT(bitmap.header.biWidth),
                    nHeight => Win32.INT(bitmap.header.biHeight),
                    hdcSrc  => tempo_DC,
                    nXSrc   => 0,		-- position dans la zone de travail
                    nYSrc   => 0,
                    dwRop   => Win32.Wingdi.SRCCOPY );
     if res_bool = 0 then
        err_code := Win32.Winbase.GetLastError;
     end if;
     --
     -- suppression bitmap temporaire
     old_obj := Win32.Wingdi.SelectObject( tempo_DC, old_obj );
     res_bool := Win32.Wingdi.DeleteObject( tempo_bmp );
     -- suppression du DC temporaire
     res_bool := Win32.Wingdi.DeleteDC( tempo_DC );
  end Affiche_transparent;


   function Largeur( Id : Resources_pkg.bmap_Id_type ) return Integer is
      bitmap : bitmap_info_ptr := Bitmap_of( Id );
   begin
     if bitmap = null then
         return 0;
      else
         return Integer( bitmap.header.biWidth );
      end if;
   end Largeur;

   function Hauteur( Id : Resources_pkg.bmap_Id_type ) return Integer is
      bitmap : bitmap_info_ptr := Bitmap_of( Id );
   begin
     if bitmap = null then
         return 0;
      else
         return Integer( bitmap.header.biHeight );
      end if;
   end Hauteur;

   -- ***********************************************************************************************

   procedure Free_all_bitmaps is
      tmp : bitmap_info_ptr;
      res_bool : Win32.BOOL;
   begin
      -- deallocation si précédement alloué
      while bitmap_list /= null loop
         -- se souvenir du suivant
         tmp := bitmap_list.next;
         --
         res_bool := Win32.Wingdi.DeleteObject( bitmap_list.handle );
         Free( bitmap_list );
         -- suivant
         bitmap_list := tmp;
      end loop;
   end Free_all_bitmaps;

   -- ***********************************************************************************************


   procedure Create_from_file( file_name : string;
                               Id : Resources_pkg.bmap_Id_type ) is
      byte_size : integer;
      f : byte_io.file_type;
   begin
      -- récupération des données
      -- ouverture du fichier
      begin
         byte_io.open(  f, file_name );
      exception
         when others =>
            utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_notfound & file_name );
            raise byte_io.file_error;
      end;
            -- lecture du bitmap FILE header
      for i in file_header'range loop
         Byte_IO.Read( f, file_header(i) );
      end loop;
      -- test du type
      if file_header(1) /= 16#42# or else file_header(2) /= 16#4D# then
          Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_inval & file_name );
          byte_io.close( f );
          raise byte_io.file_error;
      end if;
      --
      -- lecture du bitmap INFO header
      for i in info_header'range loop
         Byte_io.Read( f, info_header(i) );
      end loop;
      -- vérification du format: 32 bit, non comprimé, 72 dpi, bottom-up
      -- Les bitmap doivent avoir une couche Alpha, lignes non-inversées
      if bitmap_header.biSize /= 40 then
          Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_taille & file_name );
          byte_io.close( f );
          raise byte_io.file_error;
      end if;
      if bitmap_header.biHeight <= 0 then	-- image bottom-up
          Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_sens  & file_name);
          byte_io.close( f );
          raise byte_io.file_error;
      end if;
      if bitmap_header.biPlanes /= 1 then
          Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_plan  & file_name);
          byte_io.close( f );
          raise byte_io.file_error;
      end if;
      if bitmap_header.biBitCount /= 32 then
          Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_32bits  & file_name);
          byte_io.close( f );
          raise byte_io.file_error;
      end if;
      if bitmap_header.biCompression /= 0 then
          Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_compres  & file_name);
          byte_io.close( f );
          raise byte_io.file_error;
      end if;
      if bitmap_header.biXPelsPerMeter /= 2834 then -- = 72 dpi
          Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_dens_h  & file_name);
          byte_io.close( f );
          raise byte_io.file_error;
      end if;
      if bitmap_header.biYPelsPerMeter /= 2834 then -- = 72 dpi
          Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_dens_v  & file_name);
          byte_io.close( f );
          raise byte_io.file_error;
      end if;
      if bitmap_header.biClrUsed /= 0 then
          Utils_pkg.Error_box( Intl.err_init_txt, Intl.err_bitmap_true  & file_name);
          byte_io.close( f );
          raise byte_io.file_error;
      end if;
      --
      -- taille en pixels
      byte_size := Integer(bitmap_header.biHeight) * Integer(bitmap_header.biWidth) * 4;
      declare
         -- allocation mémoire
         buffer : Common_types.byte_array(1..byte_size):= (1..byte_size => 0);
      begin
         -- lecture des données
         begin
            for i in 1..byte_size loop
               byte_io.read( f, buffer(i) );
            end loop;
         exception
            when others =>
               utils_pkg.Error_box( Intl.err_init_txt,  Intl.err_bitmap_read & file_name );
               byte_io.close( f );
               raise byte_io.file_error;
         end;
         -- fermeture fichier bitmap
         byte_io.close(f);
         -- création du DIB et de la structure applicative
         Create( Id, bitmap_header, buffer'address );
      end;
   end Create_from_file;



end Bitmap_pkg;

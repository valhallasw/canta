with System;		use System;
with Sequential_IO;
with Interfaces.C;	use Interfaces.C;
with Ada.Unchecked_conversion;
with Text_io;
with Win32;		use Win32;
with Win32.Wingdi;
with Win32.Winnt;
with Win32.Winbase;	use Win32.Winbase;

procedure add_bitmap is

   function to_LPFFD is  new Ada.Unchecked_conversion(System.Address, Win32.Winbase.LPWIN32_FIND_DATAA);
   function To_LPCSTR is new Ada.Unchecked_Conversion(System.Address, LPCSTR);

   -- package d'I/O pour lire les fichiers de bitmap
   package byte_io is new Sequential_IO( Win32.BYTE );

   -- fichier de sortie
   f_out : text_io.file_type;
   -- fichier pour header et footer
   f_in  : text_io.file_type;
   -- nombre de fichiers traités
   nb_fic : integer;

   type string_pt is access string;

   -- liste des noms des fichiers
   type liste_rec;
   type liste_ptr is access liste_rec;
   type liste_rec is record
      nom  : string_pt;
      next : liste_ptr;
   end record;
   ma_liste : liste_ptr;

   -- header des bitmaps
   file_header : array(1..14) of Win32.BYTE;
   info_header : array(1..40 ) of Win32.BYTE;
   bitmap_header : Win32.Wingdi.BITMAPINFOHEADER;
   for bitmap_header use at info_header'address;

   -- pour FindFile
   FindDir : constant string := "*.bmp" & ascii.nul;
   FindFileData : Win32.Winbase.WIN32_FIND_DATAA;
   Handle : Win32.Winnt.HANDLE;
   resu : Win32.BOOL;

   subtype string_2 is string(1..2);
   hex_char : constant array(0..15) of character := ( '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F' );

   function hex_image( b : Win32.byte ) return string_2 is
      s : string_2;
   begin
      s(1) := hex_char( Integer(b / 16) );
      s(2) := hex_char( Integer(b mod 16) );
      return s;
   end Hex_image;



   procedure process_file( fichier : string ) is
      f : byte_io.file_type;
      pix_size : integer;
      line_len : Integer;
      b0, b1, b2, b3 : Win32.BYTE;
      nom : constant string := fichier(fichier'first..fichier'last-4);
      tmp : liste_ptr;
   begin
      begin
         -- ouverture du fichier
         Byte_io.open( f, byte_io.in_file, fichier );
         Text_IO.Put_line( fichier & " ouvert");
      exception
         when others =>
            text_io.put_line("Fichier non trouvé: " & fichier );
            return;
      end;
      --
      -- lecture du bitmap FILE header
      for i in file_header'range loop
         Byte_IO.Read( f, file_header(i) );
      end loop;
      -- test du type
      if file_header(1) /= 16#42# or else file_header(2) /= 16#4D# then
          text_io.Put_line("Ce fichier n'est pas un bitmap valide");
          return;
      end if;
      --
      -- lecture du bitmap INFO header
      for i in info_header'range loop
         Byte_io.Read( f, info_header(i) );
      end loop;
      -- vérification du format: 32 bit, non comprimé, 72 dpi, bottom-up
      -- Les bitmap doivent avoir une couche Alpha, lignes non-inversées
      if bitmap_header.biSize /= 40 then
         text_IO.Put_line("Taille de header invalide");
         return;
      end if;
      if bitmap_header.biHeight <= 0 then	-- image bottom-up
         text_io.put_line("Image top-down, invalide");
         return;
      end if;
      if bitmap_header.biPlanes /= 1 then
         text_io.put_line("Nombre de plans incorrect");
         return;
      end if;
      if bitmap_header.biBitCount /= 32 then
         text_io.put_line("Ce bitmap n'est pas en 32 bits");
         return;
      end if;
      if bitmap_header.biCompression /= 0 then
         text_io.put_line("Bitmap compressées, invalide");
         return;
      end if;
      if bitmap_header.biXPelsPerMeter /= 2834 then -- = 72 dpi
         text_io.put_line("Densité horizontale /= 72 dpi");
         return;
      end if;
      if bitmap_header.biYPelsPerMeter /= 2834 then -- = 72 dpi
         text_io.put_line("Densité verticale /= 72 dpi");
         return;
      end if;
      if bitmap_header.biClrUsed /= 0 then
         text_io.put_line("N'est pas en true color");
         return;
      end if;
      --
      -- Ecriture du header
      --
      Text_io.put( f_out, "   " & nom & "_header : Win32.Wingdi.BITMAPINFOHEADER := ( 40,");
      Text_io.put( f_out,Win32.LONG'image(bitmap_header.biWidth) );
      Text_IO.put( f_out,"," );
      Text_io.put( f_out,Win32.LONG'image(bitmap_header.biHeight) );
      text_IO.Put( f_out,", 1, 32, 0," );
      Text_IO.Put( f_out,Win32.DWORD'image(bitmap_header.biSizeImage) );
      Text_IO.Put( f_out,", 2834, 2834, 0, 0 );" );
      Text_IO.New_line( f_out, 2 );
      --
      -- Excriture des bits
      --
      -- taille en pixels
      pix_size := Integer(bitmap_header.biHeight) * Integer(bitmap_header.biWidth);
      Text_IO.Put_line( f_out, "   " & nom & "_bits : Win32.uint_array(1.." & integer'image(pix_size) & ") := (" );
      line_len := 0;
      -- lectures des bits
      begin
         for i in 1..pix_size loop
            if line_len = 0 then
               text_io.put( f_out, "      ");
               line_len := 6;
            end if;
            Byte_io.Read( f, b0 );
            Byte_io.Read( f, b1 );
            Byte_io.Read( f, b2 );
            Byte_io.Read( f, b3 );
            text_io.put( f_out, "16#" & hex_image(b3) & hex_image(b2) & hex_image(b1) & hex_image(b0) & "#" );
            line_len := line_len + 12;
            if i /= pix_size then
               text_io.put( f_out, ", ");
               line_len := line_len + 3;
            end if;
            if line_len > 120 then
               text_io.new_line( f_out );
               line_len := 0;
            end if;
         end loop;
      exception
         when others =>
            Text_IO.Put_line("Erreur dans la lecture des bits");
            return;
      end;
      -- termine l'array
      text_io.put_line( f_out, " );" );
      text_io.new_line( f_out );
      --
      -- terminé pour ce fichier
      Byte_io.Close( f );
      Text_IO.Put_line("Termine");
      -- on l'ajoute dans la liste
      tmp := new liste_rec;
      tmp.nom := new string'(nom);
      tmp.next := ma_liste;
      ma_liste := tmp;
      --
   end Process_file;

   function To_String( szCharArray : Win32.CHAR_Array ) return String is
      buffer : string(1..260);
      i, j : natural;
   begin
      j := 0;
      i := szCharArray'first;
      while i <= szCharArray'last and then szCharArray(i) /= Interfaces.C.nul loop
         j := j + 1;
         buffer(j) := To_Ada(szCharArray(i));
         i := i + 1;
      end loop;
      return buffer(1..j);
   end To_String;

   procedure copie_file( name : string ) is
      s : string(1..1024);
      len : natural;
   begin
      Text_io.Open( f_in, text_io.in_file, name );
      while not text_io.end_of_file( f_in ) loop
         text_io.get_line( f_in, s, len );
         text_io.put_line( f_out, s(1..len) );
      end loop;
      Text_io.close( f_in );
   exception
      when others =>
         text_io.Put_line("### Erreur dans le fichier " & name );
         raise;
   end copie_file;

begin
   ---
   -- Création du fichier de sortie
   Text_io.Create( f_out, text_io.out_file, "resources_pkg.adb" );

   -- copie du header
   copie_file( "header.txt" );

   -- initialisation et premier fichier
   Handle := Win32.Winbase.FindFirstFile (To_LPCSTR(FindDir'Address),
                                           To_LPFFD(FindFileData'Address) );
   -- test resultat
   if Handle = Win32.Winbase.INVALID_HANDLE_VALUE then
      Text_IO.Put_line( "Impossible de lire la liste des fichiers !");
      return;
   end if;
   -- on traite le premier fichier
   process_file( To_String(FindFileData.cFileName) );
   nb_fic := 1;
   -- reste de la liste
   loop
      resu := Win32.Winbase.FindNextFileA (Handle, To_LPFFD(FindFileData'Address) );
      exit when resu = 0;
      -- on traite le fichier suivant
      process_file( To_String(FindFileData.cFileName) );
      nb_fic := nb_fic + 1;
   end loop;

   -- fermeture handle
   resu := Win32.Winbase.FindClose(Handle);

   -- tableau des ressources
   text_io.put_line( f_out, "   type data_rec_type is record");
   text_io.put_line( f_out, "      nom  : string_pt;");
   text_io.put_line( f_out, "      bih  : Win32.Wingdi.BITMAPINFOHEADER;");
   text_io.put_line( f_out, "      bits : Win32.LPVOID;");
   text_io.put_line( f_out, "   end record;");
   text_io.new_line( f_out );
   text_io.put_line( f_out, "   data : array(1.." & Integer'image(nb_fic) & ") of data_rec_type := (" );
   for i in 1..nb_fic loop
      text_io.put( f_out, "      (new string'(""" & ma_liste.nom.all & """), " & ma_liste.nom.all & "_header, " & ma_liste.nom.all & "_bits'address)" );
      if i /= nb_fic then
         text_io.put_line( f_out, "," );
      else
         text_io.put_line( f_out, " );" );
      end if;
      ma_liste := ma_liste.next;
   end loop;

   -- copie du footer
   copie_file( "footer.txt" );

   -- fermeture fichier sortie
   Text_IO.Close( f_out );

end Add_bitmap;

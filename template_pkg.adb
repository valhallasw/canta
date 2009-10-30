with Unchecked_deallocation;

with Interfaces.C;	use Interfaces.C;

with Win32;
with Win32.Winuser;
with Win32.Winnls;

with Common_types;	use Common_types;
with Conversions;	use Conversions;
with Utils_pkg;

package body Template_pkg is

   Class_value : constant array(Class_type) of Win32.BYTE :=(
                            Button     => 16#80#,
                            Edit       => 16#81#,
                            Static     => 16#82#,
                            List_box   => 16#83#,
                            Scroll_bar => 16#84#,
                            Combo_box  => 16#85# );

   --
   font_size : constant := 8;
   Default_font_name : constant String := "MS Shell Dlg";

   -- *****************************************************************************


   procedure DWord_Aligne( t : template_type ) is
   -- alignement de write_pos sur des DWORD
   begin
      while t.write_pos mod 4 /= 0 loop
         t.write_pos := t.write_pos + 1;
      end loop;
   end DWord_Aligne;

   procedure Word_Aligne( t : template_type ) is
   -- alignement de write_pos sur des DWORD
   begin
      if t.write_pos mod 2 /= 0 then
         t.write_pos := t.write_pos + 1;
      end if;
   end Word_Aligne;

   procedure Write_byte( t : template_type; B : Win32.BYTE ) is
   begin
      t.buffer( t.write_pos ) := B;
      t.write_pos := t.write_pos + 1;
   end Write_Byte;

   -- ==============================================================================

   function  New_template( Width, height : Win32.SHORT;
                           Titre : String) return template_type is
      t : template_type;
      size : Integer;
      titre_str : constant string := Titre & ascii.nul;
      font_str : constant string := Default_font_name & ascii.nul;
   begin
      -- allocation
      t := new template_rec;
      -- écriture du header
      declare
         header : Win32.Winuser.DLGTEMPLATE;
         for header use at t.buffer(0)'address;
      begin
         header.style := Win32.Winuser.DS_SETFONT
                      or Win32.Winuser.WS_VISIBLE
                      or Win32.Winuser.WS_THICKFRAME
                      or Win32.Winuser.WS_CAPTION;
         header.dwExtendedStyle := 0;
         header.cdit := 0;		-- nombre d'objets
         header.x := 0;		        -- position en dialog box unit, par défaut : 0 0
         header.y := 0;
         header.cx := Width;		-- taille en dialog box unit
         header.cy := Height;
      end;
      -- pas de menu => +2
      -- dialog class = defaut => +2
      t.write_pos := Win32.Winuser.DLGTEMPLATE'Size/8 + 4;
      -- Titre
      size := Integer(Win32.Winnls.MultiByteToWideChar(
                           Win32.Winnls.CP_ACP,
                           0,
                           TO_LPCSTR( Titre_str'address ),
                           -1,
                           TO_PWCH(t.buffer(t.write_pos)'address),
                           50));
      -- mise à jour du pointeur d'écriture, 1 Wide_Char = 2 octets
      t.write_pos := t.write_pos + size * 2;
      -- Font
      Write_Byte(t, font_size);
      Write_Byte(t, 0);
      size := Integer(Win32.Winnls.MultiByteToWideChar(
                           Win32.Winnls.CP_ACP,
                           0,
                           TO_LPCSTR( font_str'address ),
                           -1,
                           TO_PWCH(t.buffer(t.write_pos)'address),
                           50));
      t.write_pos := t.write_pos + size * 2;
      --
      return t;
   end New_template;


   procedure Add_Item( t: template_type;
                       X, Y , Width, Height: Win32.SHORT;
                       Id : Win32.WORD;
                       Titre : String;
                       Class : Class_type ) is
      all_flags : Win32.DWORD := Win32.Winuser.WS_CHILD or Win32.Winuser.WS_VISIBLE;
      size : Integer;
      titre_str : constant string := titre & ascii.nul;
      header : Win32.Winuser.DLGTEMPLATE;
      for header use at t.buffer(0)'address;
   begin
      -- Incrémente le nombre d'item du template
      header.cdit := header.cdit + 1;
      --  ajustement des flags en fonction de la classe
      case Class is
         when Button     =>
                All_Flags := All_Flags or Win32.Winuser.WS_TABSTOP;
                if header.cdit = 1 then
                   -- le premier bouton est le bouton par défaut
                   All_Flags := All_Flags or Win32.Winuser.BS_DEFPUSHBUTTON;
                end if;
         when Edit       => All_Flags := All_Flags or Win32.Winuser.WS_TABSTOP;
         when Static     => All_Flags := All_Flags or Win32.Winuser.SS_LEFT;
         when List_box   => All_Flags := All_Flags or Win32.Winuser.WS_TABSTOP;
         when Scroll_bar => null;
         when Combo_box  => All_flags := All_Flags or Win32.Winuser.WS_TABSTOP or Win32.Winuser.DS_SYSMODAL;
      end case;
      -- Alignement sur DWORD
      DWord_Aligne(t);
      --
      declare
         item : Win32.Winuser.DLGITEMTEMPLATE;
         for item use at t.buffer(t.write_pos)'address;
      begin
         item.style :=  All_flags;
         item.dwExtendedStyle := 0;
         item.x := X;
         item.y := Y;
         item.cx := Width;
         item.cy := Height;
         item.id := ID;
      end;
      t.write_pos := t.write_pos + Win32.Winuser.DLGITEMTEMPLATE'size / 8;
      -- class = predefined
      Write_Byte(t, 16#FF#);
      Write_Byte(t, 16#FF#);
      -- Button class
      Write_Byte(t, Class_value(Class) );
      Write_Byte(t, 0);
      -- Titre
      if Class /= Combo_box and then Titre'length > 0 then
         size := Integer(Win32.Winnls.MultiByteToWideChar(
                              Win32.Winnls.CP_ACP,
                              0,
                              TO_LPCSTR( Titre_str'address ),
                              -1,
                              TO_PWCH(t.buffer(t.write_pos)'address),
                              titre_str'length * 2+2));
         if size = 0 then
            raise banal_error;
         end if;
         -- mise à jour du pointeur d'écriture
         t.write_pos := t.write_pos + size * 2;
      else
         Write_Byte(t, 0);
         Write_Byte(t, 0);
      end if;
      -- création data
      Word_Aligne(t);
      t.write_pos := t.write_pos + 2;	-- pas de data => 0 0
   end Add_Item;



   -- retourne un pointeur utilisable par DialogBox
   function  Get_template( t : template_type ) return Win32.Winuser.LPCDLGTEMPLATEA is
   begin
       return Conversions.TO_LPCDLGTEMPLATE(t.buffer(0)'address);
   end Get_template;


   procedure Free_template is new Unchecked_deallocation( object => template_rec, name => template_type );

   procedure Free( t : in out template_type ) is
   begin
      Free_template(t);
   end Free;

end Template_pkg;

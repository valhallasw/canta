with Text_IO;		use TEXT_IO;
with Calendar;		use Calendar;
with Interfaces.C;	use Interfaces.C;

with Win32;
with Win32.Winuser;	use Win32.Winuser;
with Win32.Winbase;

package body LOG is

   Fichier : Text_IO.File_type;

   log_enable : boolean := false;

   decimal : array(0..9) of character := ('0','1','2','3','4','5','6','7','8','9');

   separ : constant string := string'( 1 => ascii.ht);

   procedure Space is
   begin
      if not log_enable then
         return;
      end if;
      Text_IO.Put( Fichier, separ );
   end Space;

   procedure Separ_line is
   begin
      text_io.Put_line( Fichier, "-------------------------------------------");
   end Separ_line;


   procedure Store( S : String ) is
   begin
      if not log_enable then
         return;
      end if;
      Text_io.Put( Fichier, S );
      Text_IO.Put( Fichier, separ );
   end Store;

   procedure Store_C( CS : Win32.CHAR_Array ) is
   begin
      if not log_enable then
         return;
      end if;
      for i in CS'range loop
         exit when CS(i) = Interfaces.C.nul;
         Text_io.Put( Fichier, Interfaces.C.To_Ada( CS(i) ) );
      end loop;
      Text_IO.Put( Fichier, separ );
   end Store_C;

   procedure Store( F : Float ) is
   begin
      if not log_enable then
         return;
      end if;
      Text_io.Put( Fichier, Float'Image(F) );
      Text_IO.Put( Fichier, separ);
   end Store;

   procedure Store( L : Long_Float ) is
   begin
      if not log_enable then
         return;
      end if;
      Text_io.Put( Fichier, Long_Float'Image(L) );
      Text_IO.Put( Fichier, separ);
   end Store;


   procedure STORE( I : Integer ) is
   begin
      if not log_enable then
         return;
      end if;
      Text_io.Put( Fichier, Integer'image(I) );
      Text_io.Put( Fichier, separ);
   end STORE;

   procedure STORE_S( SI : Short_Integer ) is
   begin
      if not log_enable then
         return;
      end if;
      Text_io.Put( Fichier, Short_Integer'image(SI) );
      Text_io.Put( Fichier, separ);
   end Store_S;

   procedure STORE_D( F : Float ) is
      Int : integer;
   begin
      if not log_enable then
         return;
      end if;
      Int := Integer(F * 1000.0 );
      if Int = 0 then
         Store(0);
         return;
      end if;
      declare
         s : string := integer'image(int);	-- image
      begin
         -- signe
         if s(s'first) /= ' ' then
            Text_io.Put( Fichier, "-" );
         end if;
         case s'length is
            when 2 =>
               Text_io.put( Fichier, "0.00" );
               Text_io.put( Fichier, s(s'last) );
            when 3 =>
               Text_io.put( Fichier, "0.0" );
               Text_io.put( Fichier, s(s'last-1..s'last) );
            when 4 =>
               Text_io.put( Fichier, "0." );
               Text_io.put( Fichier, s(s'last-2..s'last) );
            when others =>
               -- partie avant le point décimal
               for i in s'first+1..s'last-3 loop
                  Text_io.Put( Fichier, s(i) );
               end loop;
               -- point decimal
               Text_io.put( Fichier, "." );
               Text_io.put( Fichier, s(s'last-3..s'last) );
         end case;
      end;
      Text_io.Put( Fichier, separ);
   end Store_D;

   function Format( Temps : Calendar.Time ) return string is
      An    : Calendar.Year_Number;
      Mois  : Calendar.Month_Number;
      Jour  : Calendar.Day_Number;
      Reste : Calendar.Day_Duration;
      Heures, minutes, secondes, milli : integer;
      IReste : integer;
                              -- 12345678901234567890123
      buffer : string(1..23) := "0000-00-00 00:00:00.000";
      j : integer;

      procedure write_int( value : integer; droite : integer ) is
         s : string := Integer'image(value);
         j : integer;
      begin
         j := droite;
         for i in reverse 2..s'length loop
            buffer(j) := s(i);
            j := j - 1;
         end loop;
      end write_int;

   begin
      -- extraction des champs
      Calendar.Split( Temps, An, Mois, Jour, Reste );
      IReste := Integer( Float'Floor( Float(Reste) * 1000.0 ) );	-- en ms
      milli := Ireste mod 1000;					-- les millisecondes
      IReste := (IReste - milli)/1000;			-- en secondes
      secondes := IReste mod 60;				-- s
      IReste := (IReste - secondes)/60;
      minutes := IReste mod 60;					-- mn
      IReste := (Ireste - minutes)/60;
      Heures := Ireste mod 24;					-- h
      -- création string
      write_int( an, 4);
      write_int( mois, 7 );
      write_int( jour, 10 );
      write_int( heures, 13 );
      write_int( minutes, 16 );
      write_int( secondes, 19 );
      declare
         s : string := Integer'image(milli);
      begin
         j := 21;
         for i in 2..s'length loop
            buffer(j) := s(i);
            j := j + 1;
         end loop;
      end;
      --
      return buffer;
   end Format;

   procedure STORE_TIME is
   begin
      if not log_enable then
         return;
      end if;
      Text_IO.Put( Fichier, Format(Calendar.Clock)  );
      Text_io.Put( Fichier, separ);
   end STORE_TIME;

   procedure STORE( t : Duration ) is
   begin
      if not log_enable then
         return;
      end if;
      Text_IO.Put( Fichier, Duration'Image( t ) );
      Text_io.Put( Fichier, separ);
   end STORE;

   procedure Error( text : string ) is
   begin
      if not log_enable then
         Start_Log;
      end if;
      Store_Time;
      Text_io.put_line( Fichier, text );
   end Error;


   procedure End_line is
   begin
      if not log_enable then
         return;
      end if;
      Text_io.New_line( Fichier );
   end End_line;

   procedure END_LOG is
   begin
      if not log_enable then
         return;
      end if;
      Text_io.put( Fichier, "Log closed at " );
      Text_io.put_line( Fichier, Format(calendar.clock) );
      Text_IO.Close( Fichier );
   end END_LOG;

   procedure START_LOG is
   begin
      Text_IO.Create( Fichier, Text_IO.Out_File, "log.txt" );
      Text_io.put( Fichier, "Log started at " );
      Text_io.put_line( Fichier, Format(calendar.clock) );
      log_enable := true;
   end START_LOG;

   -- *******************************************************************************************

   procedure Midiout_error( Result : Win32.Mmsystem.MMRESULT ) is
   begin
      case Result is
         when Win32.Mmsystem.MMSYSERR_NOERROR      => return;
         when Win32.Mmsystem.MMSYSERR_ERROR        => Error("MMSYSERR_ERROR");
         when Win32.Mmsystem.MMSYSERR_BADDEVICEID  => Error("MMSYSERR_BADDEVICEID");
         when Win32.Mmsystem.MMSYSERR_NOTENABLED   => Error("MMSYSERR_NOTENABLED");
         when Win32.Mmsystem.MMSYSERR_ALLOCATED    => Error("MMSYSERR_ALLOCATED");
         when Win32.Mmsystem.MMSYSERR_INVALHANDLE  => Error("MMSYSERR_INVALHANDLE");
         when Win32.Mmsystem.MMSYSERR_NODRIVER     => Error("MMSYSERR_NODRIVER");
         when Win32.Mmsystem.MMSYSERR_NOMEM        => Error("MMSYSERR_NOMEM");
         when Win32.Mmsystem.MMSYSERR_NOTSUPPORTED => Error("MMSYSERR_NOTSUPPORTED");
         when Win32.Mmsystem.MMSYSERR_BADERRNUM    => Error("MMSYSERR_BADERRNUM");
         when Win32.Mmsystem.MMSYSERR_INVALFLAG    => Error("MMSYSERR_INVALFLAG");
         when Win32.Mmsystem.MMSYSERR_INVALPARAM   => Error("MMSYSERR_INVALPARAM");
         when Win32.Mmsystem.MMSYSERR_HANDLEBUSY   => Error("MMSYSERR_HANDLEBUSY");
         when Win32.Mmsystem.MMSYSERR_INVALIDALIAS => Error("MMSYSERR_INVALIDALIAS");
         when others => Error("MMSYSERR_???" & integer'image(integer(result)));
      end case;
   end Midiout_error;


   procedure Store_Message( msg : Win32.UINT ) is
   begin
      if not log_enable then
         return;
      end if;
      case msg is
         when WM_NULL => Store("WM_NULL");
         when WM_CREATE => Store("WM_CREATE");
         when WM_DESTROY => Store("WM_DESTROY");
         when WM_MOVE => Store("WM_MOVE");
         when WM_SIZE => Store("WM_SIZE");
         when WM_ACTIVATE => Store("WM_ACTIVATE");
         when WM_SETFOCUS => Store("WM_SETFOCUS");
         when WM_KILLFOCUS => Store("WM_KILLFOCUS");
         when WM_ENABLE => Store("WM_ENABLE");
         when WM_SETREDRAW => Store("WM_SETREDRAW");
         when WM_SETTEXT => Store("WM_SETTEXT");
         when WM_GETTEXT => Store("WM_GETTEXT");
         when WM_GETTEXTLENGTH => Store("WM_GETTEXTLENGTH");
         when WM_PAINT => Store("WM_PAINT");
         when WM_CLOSE => Store("WM_CLOSE");
         when WM_QUERYENDSESSION => Store("WM_QUERYENDSESSION");
         when WM_QUIT => Store("WM_QUIT");
         when WM_QUERYOPEN => Store("WM_QUERYOPEN");
         when WM_ERASEBKGND => Store("WM_ERASEBKGND");
         when WM_SYSCOLORCHANGE => Store("WM_SYSCOLORCHANGE");
         when WM_ENDSESSION => Store("WM_ENDSESSION");
         when WM_SHOWWINDOW => Store("WM_SHOWWINDOW");
         when WM_WININICHANGE => Store("WM_WININICHANGE");
         when WM_DEVMODECHANGE => Store("WM_DEVMODECHANGE");
         when WM_ACTIVATEAPP => Store("WM_ACTIVATEAPP");
         when WM_FONTCHANGE => Store("WM_FONTCHANGE");
         when WM_TIMECHANGE => Store("WM_TIMECHANGE");
         when WM_CANCELMODE => Store("WM_CANCELMODE");
         when WM_SETCURSOR => Store("WM_SETCURSOR");
         when WM_MOUSEACTIVATE => Store("WM_MOUSEACTIVATE");
         when WM_CHILDACTIVATE => Store("WM_CHILDACTIVATE");
         when WM_QUEUESYNC => Store("WM_QUEUESYNC");
         when WM_GETMINMAXINFO => Store("WM_GETMINMAXINFO");
         when WM_PAINTICON => Store("WM_PAINTICON");
         when WM_ICONERASEBKGND => Store("WM_ICONERASEBKGND");
         when WM_NEXTDLGCTL => Store("WM_NEXTDLGCTL");
         when WM_SPOOLERSTATUS => Store("WM_SPOOLERSTATUS");
         when WM_DRAWITEM => Store("WM_DRAWITEM");
         when WM_MEASUREITEM => Store("WM_MEASUREITEM");
         when WM_DELETEITEM => Store("WM_DELETEITEM");
         when WM_VKEYTOITEM => Store("WM_VKEYTOITEM");
         when WM_CHARTOITEM => Store("WM_CHARTOITEM");
         when WM_SETFONT => Store("WM_SETFONT");
         when WM_GETFONT => Store("WM_GETFONT");
         when WM_SETHOTKEY => Store("WM_SETHOTKEY");
         when WM_GETHOTKEY => Store("WM_GETHOTKEY");
         when WM_QUERYDRAGICON => Store("WM_QUERYDRAGICON");
         when WM_COMPAREITEM => Store("WM_COMPAREITEM");
         when WM_COMPACTING => Store("WM_COMPACTING");
         when WM_COMMNOTIFY => Store("WM_COMMNOTIFY");
         when WM_WINDOWPOSCHANGING => Store("WM_WINDOWPOSCHANGING");
         when WM_WINDOWPOSCHANGED => Store("WM_WINDOWPOSCHANGED");
         when WM_POWER => Store("WM_POWER");
         when WM_COPYDATA => Store("WM_COPYDATA");
         when WM_CANCELJOURNAL => Store("WM_CANCELJOURNAL");
         when WM_NCCREATE => Store("WM_NCCREATE");
         when WM_NCDESTROY  => Store("WM_NCDESTROY");
         when WM_NCCALCSIZE => Store("WM_NCCALCSIZE");
         when WM_NCHITTEST => Store("WM_NCHITTEST");
         when WM_NCPAINT  => Store("WM_NCPAINT");
         when WM_NCACTIVATE => Store("WM_NCACTIVATE");
         when WM_GETDLGCODE => Store("WM_GETDLGCODE");
         when WM_NCMOUSEMOVE => Store("WM_NCMOUSEMOVE");
         when WM_NCLBUTTONDOWN => Store("WM_NCLBUTTONDOWN");
         when WM_NCLBUTTONUP => Store("WM_NCLBUTTONUP");
         when WM_NCLBUTTONDBLCLK => Store("WM_NCLBUTTONDBLCLK");
         when WM_NCRBUTTONDOWN => Store("WM_NCRBUTTONDOWN");
         when WM_NCRBUTTONUP => Store("WM_NCRBUTTONUP");
         when WM_NCRBUTTONDBLCLK => Store("WM_NCRBUTTONDBLCLK");
         when WM_NCMBUTTONDOWN => Store("WM_NCMBUTTONDOWN");
         when WM_NCMBUTTONUP => Store("WM_NCMBUTTONUP");
         when WM_NCMBUTTONDBLCLK => Store("WM_NCMBUTTONDBLCLK");
         when WM_KEYDOWN => Store("WM_KEYDOWN");
         when WM_KEYUP => Store("WM_KEYUP");
         when WM_CHAR => Store("WM_CHAR");
         when WM_DEADCHAR => Store("WM_DEADCHAR");
         when WM_SYSKEYDOWN => Store("WM_SYSKEYDOWN");
         when WM_SYSKEYUP => Store("WM_SYSKEYUP");
         when WM_SYSCHAR => Store("WM_SYSCHAR");
         when WM_SYSDEADCHAR => Store("WM_SYSDEADCHAR");
         when WM_KEYLAST => Store("WM_KEYLAST");
         when WM_INITDIALOG => Store("WM_INITDIALOG");
         when WM_COMMAND => Store("WM_COMMAND");
         when WM_SYSCOMMAND => Store("WM_SYSCOMMAND");
         when WM_TIMER => Store("WM_TIMER");
         when WM_HSCROLL => Store("WM_HSCROLL");
         when WM_VSCROLL => Store("WM_VSCROLL");
         when WM_INITMENU => Store("WM_INITMENU");
         when WM_INITMENUPOPUP => Store("WM_INITMENUPOPUP");
         when WM_MENUSELECT => Store("WM_MENUSELECT");
         when WM_MENUCHAR => Store("WM_MENUCHAR");
         when WM_ENTERIDLE  => Store("WM_ENTERIDLE");
         when WM_CTLCOLORMSGBOX  => Store("WM_CTLCOLORMSGBOX");
         when WM_CTLCOLOREDIT  => Store("WM_CTLCOLOREDIT");
         when WM_CTLCOLORLISTBOX  => Store("WM_CTLCOLORLISTBOX");
         when WM_CTLCOLORBTN  => Store("WM_CTLCOLORBTN");
         when WM_CTLCOLORDLG  => Store("WM_CTLCOLORDLG");
         when WM_CTLCOLORSCROLLBAR => Store("WM_CTLCOLORSCROLLBAR");
         when WM_CTLCOLORSTATIC => Store("WM_CTLCOLORSTATIC");
         when WM_MOUSEMOVE  => Store("WM_MOUSEMOVE");
         when WM_LBUTTONDOWN  => Store("WM_LBUTTONDOWN");
         when WM_LBUTTONUP  => Store("WM_LBUTTONUP");
         when WM_LBUTTONDBLCLK  => Store("WM_LBUTTONDBLCLK");
         when WM_RBUTTONDOWN  => Store("WM_RBUTTONDOWN");
         when WM_RBUTTONUP  => Store("WM_RBUTTONUP");
         when WM_RBUTTONDBLCLK  => Store("WM_RBUTTONDBLCLK");
         when WM_MBUTTONDOWN  => Store("WM_MBUTTONDOWN");
         when WM_MBUTTONUP => Store("WM_MBUTTONUP");
         when WM_MBUTTONDBLCLK  => Store("WM_MBUTTONDBLCLK");
         when WM_PARENTNOTIFY  => Store("WM_PARENTNOTIFY");
         when WM_ENTERMENULOOP  => Store("WM_ENTERMENULOOP");
         when WM_EXITMENULOOP  => Store("WM_EXITMENULOOP");
         when WM_MDICREATE  => Store("WM_MDICREATE");
         when WM_MDIDESTROY  => Store("WM_MDIDESTROY");
         when WM_MDIACTIVATE  => Store("WM_MDIACTIVATE");
         when WM_MDIRESTORE  => Store("WM_MDIRESTORE");
         when WM_MDINEXT  => Store("WM_MDINEXT");
         when WM_MDIMAXIMIZE  => Store("WM_MDIMAXIMIZE");
         when WM_MDITILE  => Store("WM_MDITILE");
         when WM_MDICASCADE  => Store("WM_MDICASCADE");
         when WM_MDIICONARRANGE  => Store("WM_MDIICONARRANGE");
         when WM_MDIGETACTIVE  => Store("WM_MDIGETACTIVE");
         when WM_MDISETMENU  => Store("WM_MDISETMENU");
         when WM_DROPFILES  => Store("WM_DROPFILES");
         when WM_CUT  => Store("WM_CUT");
         when WM_COPY  => Store("WM_COPY");
         when WM_PASTE  => Store("WM_PASTE");
         when WM_CLEAR => Store("WM_CLEAR");
         when WM_UNDO => Store("WM_UNDO");
         when WM_RENDERFORMAT => Store("WM_RENDERFORMAT");
         when WM_RENDERALLFORMATS => Store("WM_RENDERALLFORMATS");
         when WM_DESTROYCLIPBOARD => Store("WM_DESTROYCLIPBOARD");
         when WM_DRAWCLIPBOARD => Store("WM_DRAWCLIPBOARD");
         when WM_PAINTCLIPBOARD  => Store("WM_PAINTCLIPBOARD");
         when WM_VSCROLLCLIPBOARD  => Store("WM_VSCROLLCLIPBOARD");
         when WM_SIZECLIPBOARD => Store("WM_SIZECLIPBOARD");
         when WM_ASKCBFORMATNAME  => Store("WM_ASKCBFORMATNAME");
         when WM_CHANGECBCHAIN  => Store("WM_CHANGECBCHAIN");
         when WM_HSCROLLCLIPBOARD  => Store("WM_HSCROLLCLIPBOARD");
         when WM_QUERYNEWPALETTE  => Store("WM_QUERYNEWPALETTE");
         when WM_PALETTEISCHANGING  => Store("WM_PALETTEISCHANGING");
         when WM_PALETTECHANGED  => Store("WM_PALETTECHANGED");
         when WM_HOTKEY  => Store("WM_HOTKEY");
         when WM_PENWINFIRST  => Store("WM_PENWINFIRST");
         when WM_PENWINLAST  => Store("WM_PENWINLAST");
         when WM_USER => Store("WM_USER");
         when 533 => Store("WM_CAPTURECHANGED");
         when others =>
            if msg > WM_USER then
               Store("USER +" & Integer'image(Integer(msg-WM_USER)));
            else
               Store("Other" & Integer'image(Integer(msg)));
            end if;
      end case;
      Text_IO.Put( Fichier, separ );
   end Store_Message;

end LOG;

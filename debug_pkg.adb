with Text_io;	use Text_io;
with GNAT.Current_Exception;
with Log;
with Common_types;	use Common_types;

package body Debug_pkg is

   type dbg_param_rec;
   type dbg_param_pt is access dbg_param_rec;
   type dbg_param_rec is record
      param : string_pt;
      next : dbg_param_pt;
   end record;

   Param_list, last_pt : dbg_param_pt;

   procedure Init_debug is
      f : file_type;
      buffer : string(1..1024);
      len, start, fin : natural;
   begin
      begin
         -- si le fichier existe, l'ouvrir
         Open( f, in_file, "debug.txt" );
      exception
         -- sinon retour sans rien faire
         when others => return;
      end;
      -- lecture du fichier
      while not end_of_file( f ) loop
         -- lecture d'une ligne
         Get_line( f, buffer, len );
         -- suppression des espaces en début
         start := 1;
         while start <= len and then (buffer(start)=' ' or buffer(start)=ascii.ht) loop
            start := start + 1;
         end loop;
         -- si pas un commentaire...
         if start > len or else buffer(start) /= ';' then
            -- recherche de la fin
            fin := len;
            while fin >= start and then (buffer(start)=' ' or buffer(start)=ascii.ht) loop
               fin := fin-1;
            end loop;
            -- création du parametre
            if Last_pt = null then
               Param_list := new dbg_param_rec'( new string'(buffer(start..fin)), null );
               Last_pt := Param_list;
            else
               Last_pt.next := new dbg_param_rec'( new string'(buffer(start..fin)), null );
               Last_pt := Last_pt.next;
            end if;
         end if;
      end loop;
      Close( f );
   exception
      when others =>
         Log.Error( GNAT.Current_Exception.Exception_Information );
   end Init_debug;


   function Is_set( param : string ) return boolean is
      pt : dbg_param_pt;
   begin
      if Param_list = null then
         return false;
      end if;
      pt := Param_list;
      while pt /= null loop
         if pt.param.all = param then
            return true;
         end if;
         pt := pt.next;
      end loop;
      return false;
   end Is_set;

end Debug_pkg;

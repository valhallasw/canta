with Interfaces.C;		use Interfaces.C;
with Interfaces.C.Strings;
with Win32;

package body Conversions is

   type bi_word is record
      low : Win32.word;
      hi  : Win32.word;
   end record;

   type bi_short is record
      low : Win32.SHORT;
      hi  : Win32.SHORT;
   end record;

   function HI_WORD( L : Win32.LONG ) return Win32.SHORT is
      bi : bi_short;
      for bi use at L'address;
   begin
      return bi.hi;
   end HI_WORD;

   procedure Split_short( L : Win32.LONG; hi, low : out Win32.SHORT) is
      bi : bi_short;
      for bi use at L'address;
   begin
      hi  := bi.hi;
      low := bi.low;
   end Split_short;

   -- conversion en minuscule
   function To_lower( s : string ) return string is
      resu : string := s;
   begin
      for i in resu'range loop
         if resu(i) in 'A'..'Z' then
            resu(i) := character'val( character'pos(resu(i)) - character'pos('A') + character'pos('a') );
         end if;
      end loop;
      return resu;
   end To_lower;

   function To_Ada( c_string : string ) return string is
      len : natural := c_string'first;
   begin
      while len <= c_string'last and then c_string(len) /= ascii.nul loop
         len := len + 1;
      end loop;
      return c_string(c_string'first..len-1);
   end To_Ada;

end Conversions;

package body Stat_pkg is

   size : constant := 13;
   median : constant := 7;

   -- les valeurs dans l'ordre chronologique
   values : array(1..size) of short_integer := (1..size => 0);
   -- index de la dernière valeur entrée
   last_entry : integer := 0;
   -- les valeurs triées par ordre croissant
   sorted : array(1..size) of short_integer := (1..size => 0);


   -- *******************************************************************************************


   function add_median( value : short_integer ) return short_integer is
      index : integer;
      last_value : short_integer;
   begin
      --
      -- supression de la dernière valeur entrée
      --
      if last_entry /= 0 then
        -- lecture valeur la plus ancienne
        if last_entry = size then
           index := 1;
        else
           index := last_entry + 1;
        end if;
        last_value := values(index);
        -- recherche de cette valeur dans le tableau trié
        index := 1;
        while index < size and then sorted(index) /= last_value loop
           index := index + 1;
        end loop;
        -- suppression de la valeur
        for i in index..size-1 loop
           sorted(i) := sorted(i+1);
        end loop;
        sorted(size) := 0;
      end if;
      --
      -- insertion de la nouvelle valeur
      --
      -- stocke nouvele valeur dans buffer circulaire
      if last_entry = size then
         last_entry := 1;
      else
         last_entry := last_entry + 1;
      end if;
      values(last_entry) := value;
      -- recherche de sa position dans le tableau
      index := 1;
      while index < size and then value > sorted(index) loop
         index := index + 1;
      end loop;
      -- index indique l'endroit ou inserer la valeur
      -- on décale les autres
      for i in reverse index+1..size loop
         sorted(i) := sorted(i-1);
      end loop;
      -- on insert
      sorted(index) := value;
      -- retourne la médianne
      return sorted(median);
   end add_median;


   function Variance return float is
      mean, var, tmp : float;
   begin
      mean := 0.0;
      for i in 1..size loop
         mean := mean + Float(values(i));
      end loop;
      mean := mean / Float(size);
      --
      var := 0.0;
      for i in 1..size loop
         tmp := float(values(i));
         var := var + ( tmp - mean ) * ( tmp - mean );
      end loop;
      var := var / Float(size-1);
      return var;
   end Variance;



end Stat_pkg;
